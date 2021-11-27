{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Canonicalize.Canonicalize where

import           Run.Target
import qualified AST.Canonical                 as Can
import qualified AST.Source                    as Src
import           Canonicalize.JSExp
import           Canonicalize.CanonicalM
import qualified Canonicalize.Env              as E
import           Canonicalize.Typing
import qualified Data.Map                      as M
import qualified Data.List                     as L
import           Parse.Madlib.Grammar           ( mergeAreas )
import           AST.Canonical                  ( getArea )
import           Explain.Location
import           Control.Monad.Except
import           Error.Error
import           Error.Context
import           Infer.Type



class Canonicalizable a b where
  canonicalize :: E.Env -> Target -> a -> CanonicalM b


instance Canonicalizable Src.Exp Can.Exp where
  canonicalize env target (Src.Source area e) = case e of
    Src.LNum  x           -> return $ Can.Canonical area (Can.LNum x)

    Src.LFloat x          -> return $ Can.Canonical area (Can.LFloat x)

    Src.LStr  x           -> return $ Can.Canonical area (Can.LStr x)

    Src.LBool x           -> return $ Can.Canonical area (Can.LBool x)

    Src.LUnit             -> return $ Can.Canonical area Can.LUnit

    Src.TemplateString es -> do
      es' <- mapM (canonicalize env target) es
      return $ Can.Canonical area (Can.TemplateString es')

    Src.JSExp js -> do
      pushJS js
      return $ Can.Canonical area (Can.JSExp $ filterJSExp target js)

    Src.App fn args -> do
      buildApp env target area fn args

    Src.UnOp op arg -> do
      buildApp env target area op [arg]

    Src.BinOp argL op argR -> do
      buildApp env target area op [argL, argR]

    Src.Access rec field -> do
      rec'   <- canonicalize env target rec
      field' <- canonicalize env target field
      return $ Can.Canonical area (Can.Access rec' field')

    Src.AbsWithMultilineBody params body ->
      buildAbs env target area params body

    Src.Abs params body ->
      buildAbs env target area params body

    Src.Return exp ->
      canonicalize env target exp

    Src.Assignment name exp -> do
      exp' <- canonicalize env target exp
      return $ Can.Canonical area (Can.Assignment name exp')

    Src.Export exp -> do
      exp' <- canonicalize env target exp
      return $ Can.Canonical area (Can.Export exp')

    Src.NameExport name -> do
      pushNameAccess name
      return $ Can.Canonical area (Can.NameExport name)

    Src.TypeExport name -> do
      pushTypeAccess name
      case M.lookup name (E.envTypeDecls env) of
        Just found -> return $ Can.Canonical area (Can.TypeExport name)
        Nothing    -> throwError $ CompilationError (UnboundType name) (Context (E.envCurrentPath env) area [])

      return $ Can.Canonical area (Can.TypeExport name)

    Src.Var name -> do
      pushNameAccess name
      return $ Can.Canonical area (Can.Var name)

    Src.TypedExp exp typing -> do
      exp'    <- canonicalize env target exp
      scheme  <- typingToScheme env typing
      typing' <- canonicalizeTyping typing
      return $ Can.Canonical area (Can.TypedExp exp' typing' scheme)

    -- TODO: verify that the names match
    Src.NamedTypedExp _ exp typing -> do
      exp'    <- canonicalize env target exp
      scheme  <- typingToScheme env typing
      typing' <- canonicalizeTyping typing
      return $ Can.Canonical area (Can.TypedExp exp' typing' scheme)

    Src.ListConstructor items -> do
      items' <- mapM (canonicalize env target) items
      return $ Can.Canonical area (Can.ListConstructor items')

    Src.TupleConstructor exps -> do
      exps' <- mapM (canonicalize env target) exps
      return $ Can.Canonical area (Can.TupleConstructor exps')

    Src.Record fields -> do
      fields' <- mapM (canonicalize env target) fields
      return $ Can.Canonical area (Can.Record fields')

    Src.If cond truthy falsy -> do
      cond'   <- canonicalize env target cond
      truthy' <- canonicalize env target truthy
      falsy'  <- canonicalize env target falsy
      return $ Can.Canonical area (Can.If cond' truthy' falsy')

    Src.Ternary cond truthy falsy -> do
      cond'   <- canonicalize env target cond
      truthy' <- canonicalize env target truthy
      falsy'  <- canonicalize env target falsy
      return $ Can.Canonical area (Can.If cond' truthy' falsy')

    Src.Do exps -> do
      exps' <- canonicalizeDefineExps exps
      return $ Can.Canonical area (Can.Do exps')
      where
        canonicalizeDefineExps :: [Src.Exp] -> CanonicalM [Can.Exp]
        canonicalizeDefineExps []     = return []
        canonicalizeDefineExps (e:es) = case e of
          Src.Source _ (Src.DoAssignment name action) -> do
            exp' <- canonicalize env target action
            es'  <- canonicalizeDefineExps es
            let fn  = Can.Canonical area (Can.Var "chain")
            let abs = Can.Canonical area $ Can.Abs (Can.Canonical area name) es'
            let app = Can.Canonical area (Can.App (Can.Canonical area $ Can.App fn abs False) exp' True)
            return [app]

          _ -> do
            e'  <- canonicalize env target e
            es' <- canonicalizeDefineExps es
            return $ e':es'

    Src.Where exp iss -> do
      exp' <- canonicalize env target exp
      iss' <- mapM (canonicalize env target) iss
      return $ Can.Canonical area (Can.Where exp' iss')

    Src.WhereAbs iss -> do
      buildAbs
        env
        target
        area
        [Src.Source emptyArea "__x__"]
        [Src.Source area (Src.Where (Src.Source emptyArea (Src.Var "__x__")) iss)]

    Src.JsxTag name props children -> do
      canonicalizeJsxTag env target (Src.Source area e)

    Src.JsxAutoClosedTag name props -> do
      canonicalizeJsxTag env target (Src.Source area (Src.JsxTag name props []))

    Src.Parenthesized _ exp _ ->
      canonicalize env target exp

    Src.Pipe exps -> do
      let (Area (Loc x l c) _) = area
          varPLoc              = Area (Loc x l c) (Loc (x + 3) l (c + 3))
      app <- buildApplication (Can.Canonical varPLoc (Can.Var "_P_")) exps
      return $ Can.Canonical area (Can.Abs (Can.Canonical varPLoc "_P_") [app])
     where
      buildApplication :: Can.Exp -> [Src.Exp] -> CanonicalM Can.Exp
      buildApplication prev es = case es of
        [e] -> do
          e' <- canonicalize env target e
          return $ Can.Canonical (mergeAreas (getArea prev) (Can.getArea e')) (Can.App e' prev True)

        e : es -> do
          e' <- canonicalize env target e
          let app = Can.Canonical (mergeAreas (getArea prev) (Can.getArea e')) (Can.App e' prev True)
          buildApplication app es

    Src.Dictionary items -> do
      pushNameAccess "__dict_ctor__"
      items' <- mapM (canonicalize env target) items

      return $ Can.Canonical area (Can.App
        (Can.Canonical area (Can.Var "__dict_ctor__"))
        (Can.Canonical area (Can.ListConstructor items')) True)

    Src.Extern typing name originalName -> do
      scheme  <- typingToScheme env typing
      return $ Can.Canonical area (Can.Extern scheme name originalName)


buildAbs :: E.Env -> Target -> Area -> [Src.Source Src.Name] -> [Src.Exp] -> CanonicalM Can.Exp
buildAbs env target area [Src.Source area' param] body = do
  body' <- mapM (canonicalize env target) body
  return $ Can.Canonical area (Can.Abs (Can.Canonical area' param) body')
buildAbs env target area (Src.Source area' param:xs) body  = do
  next <- buildAbs env target area xs body
  return $ Can.Canonical area (Can.Abs (Can.Canonical area' param) [next])


buildApp :: E.Env -> Target -> Area -> Src.Exp -> [Src.Exp] -> CanonicalM Can.Exp
buildApp env target area f args =
  buildApp' env target (length args) (length args) area f args

buildApp' :: E.Env -> Target -> Int -> Int -> Area -> Src.Exp -> [Src.Exp] -> CanonicalM Can.Exp
buildApp' env target total nth area f@(Src.Source _ f') [arg] = do
  arg' <- canonicalize env target arg
  f'   <- canonicalize env target f
  return $ Can.Canonical area (Can.App f' arg' (total == nth))
buildApp' env target total nth area f@(Src.Source _ f') xs = do
  let arg@(Src.Source area' _) = last xs
  arg'   <- canonicalize env target arg
  subApp <- buildApp' env target total (nth - 1) area f (init xs)
  return $ Can.Canonical (mergeAreas area area') (Can.App subApp arg' (total == nth))

canonicalizeJsxTag :: E.Env -> Target -> Src.Exp -> CanonicalM Can.Exp
canonicalizeJsxTag env target exp = case exp of
  Src.Source area (Src.JsxTag name props children) -> do
    pushNameAccess name
    pushNameAccess "text" -- fix for now
    let Area (Loc _ l c) (Loc _ l' c') = area
    let tagFnArea = Area (Loc 0 l c) (Loc 0 l (c + length name + 2))
    let tagFnVar = Can.Canonical tagFnArea (Can.Var name)

    children' <- mapM canonicalizeJsxChild children
    propFns <- mapM
      (\(Src.Source a (Src.JsxProp name' exp)) -> do
        pushNameAccess name'
        let Area (Loc _ l c) (Loc _ l' c') = a
        arg <- canonicalize env target exp
        return $ Can.Canonical
          a
          (Can.ListItem $ Can.Canonical
            a
            (Can.App (Can.Canonical (Area (Loc 0 l c) (Loc 0 l (c + length name'))) (Can.Var name')) arg True)
          )
      )
      props
    let propArea     = composeArea (length name) area propFns
    let childrenArea = composeArea 0 area children'

    let props'       = Can.Canonical propArea (Can.ListConstructor propFns)
    let children''   = Can.Canonical childrenArea (Can.ListConstructor children')

    return $ Can.Canonical
      area
      (Can.App (Can.Canonical (mergeAreas tagFnArea propArea) (Can.App tagFnVar props' False)) children'' True)
    where
    composeArea :: Int -> Area -> [Can.Canonical a] -> Area
    composeArea offset (Area (Loc _ l c) _) cans = if not (null cans)
      then mergeAreas (getArea $ head cans) (getArea $ last cans)
      else Area (Loc 0 l (c + offset)) (Loc 0 l (c + offset))

    canonicalizeJsxChild :: Src.JsxChild -> CanonicalM Can.ListItem
    canonicalizeJsxChild child = case child of
      Src.JsxChild exp -> do
        e' <- canonicalize env target exp
        return $ Can.Canonical area (Can.ListItem e')

      Src.JsxExpChild exp -> do
        e' <- canonicalize env target exp
        return $ Can.Canonical area (Can.ListItem e')

      Src.JsxSpreadChild exp -> do
        e' <- canonicalize env target exp
        return $ Can.Canonical area (Can.ListSpread e')



instance Canonicalizable Src.DictItem Can.ListItem where
  canonicalize env target (Src.Source area exp) = case exp of
    (Src.DictItem key value) -> do
      key' <- canonicalize env target key
      value' <- canonicalize env target value
      return $ Can.Canonical area (Can.ListItem (Can.Canonical area (Can.TupleConstructor [key', value'])))


instance Canonicalizable Src.ListItem Can.ListItem where
  canonicalize env target (Src.Source area item) = case item of
    Src.ListItem exp -> do
      exp' <- canonicalize env target exp
      return $ Can.Canonical area $ Can.ListItem exp'

    Src.ListSpread exp -> do
      exp' <- canonicalize env target exp
      return $ Can.Canonical area $ Can.ListSpread exp'


instance Canonicalizable Src.Field Can.Field where
  canonicalize env target (Src.Source area item) = case item of
    Src.Field (name, exp) -> do
      exp' <- canonicalize env target exp
      return $ Can.Canonical area $ Can.Field (name, exp')

    Src.FieldShorthand name -> do
      pushNameAccess name
      return $ Can.Canonical area $ Can.Field (name, Can.Canonical area (Can.Var name))

    Src.FieldSpread exp -> do
      exp' <- canonicalize env target exp
      return $ Can.Canonical area $ Can.FieldSpread exp'


instance Canonicalizable Src.Is Can.Is where
  canonicalize env target (Src.Source area (Src.Is pat exp)) = do
    pat' <- canonicalize env target pat
    exp' <- canonicalize env target exp
    return $ Can.Canonical area (Can.Is pat' exp')


instance Canonicalizable Src.Pattern Can.Pattern where
  canonicalize env target (Src.Source area pat) = case pat of
    Src.PVar name       -> return $ Can.Canonical area (Can.PVar name)

    Src.PAny            -> return $ Can.Canonical area Can.PAny

    Src.PCon (Src.Source _ name) pats -> do
      let nameToPush = if "." `L.isInfixOf` name then takeWhile (/= '.') name else name
      pushNameAccess nameToPush
      pats' <- mapM (canonicalize env target) pats
      return $ Can.Canonical area (Can.PCon name pats')

    Src.PNullaryCon (Src.Source _ name) -> do
      let nameToPush = if "." `L.isInfixOf` name then takeWhile (/= '.') name else name
      pushNameAccess nameToPush
      return $ Can.Canonical area (Can.PCon name [])

    Src.PNum    num  -> return $ Can.Canonical area (Can.PNum num)

    Src.PStr    str  -> return $ Can.Canonical area (Can.PStr str)

    Src.PBool   boo  -> return $ Can.Canonical area (Can.PBool boo)

    Src.PRecord pats -> do
      pats' <- mapM (canonicalize env target) (extractPatternFields pats)
      return $ Can.Canonical area (Can.PRecord pats')

    Src.PList pats -> do
      pats' <- mapM (canonicalize env target) pats
      return $ Can.Canonical area (Can.PList pats')

    Src.PTuple pats -> do
      pats' <- mapM (canonicalize env target) pats
      return $ Can.Canonical area (Can.PTuple pats')

    Src.PSpread pat -> do
      pat' <- canonicalize env target pat
      return $ Can.Canonical area (Can.PSpread pat')


extractPatternFields :: [Src.PatternField] -> M.Map Src.Name Src.Pattern
extractPatternFields pats = case pats of
  (Src.PatternField (Src.Source _ fieldName) pat : ps) ->
    M.insert fieldName pat (extractPatternFields ps)

  (Src.PatternFieldShorthand (Src.Source area fieldName) : ps) ->
    M.insert fieldName (Src.Source area (Src.PVar fieldName)) (extractPatternFields ps)

  [] ->
    mempty


instance Canonicalizable Src.Import Can.Import where
  canonicalize env target (Src.Source area imp) = case imp of
    Src.NamedImport names relPath absPath ->
      return $ Can.Canonical area (Can.NamedImport (canonicalizeName <$> names) relPath absPath)

    Src.TypeImport names relPath absPath ->
      return $ Can.Canonical area (Can.TypeImport (canonicalizeName <$> names) relPath absPath)

    Src.DefaultImport namespace relPath absPath ->
      return $ Can.Canonical area (Can.DefaultImport (canonicalizeName namespace) relPath absPath)

    Src.ImportAll relPath absPath ->
      return $ Can.Canonical area (Can.ImportAll relPath absPath)

canonicalizeName :: Src.Source Src.Name -> Can.Canonical Can.Name
canonicalizeName (Src.Source area name) = Can.Canonical area name
