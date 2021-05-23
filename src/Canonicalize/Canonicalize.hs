{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Canonicalize.Canonicalize where

import           Target
import qualified AST.Canonical                 as Can
import qualified AST.Source                    as Src
import           Canonicalize.JSExp
import           Canonicalize.CanonicalM
import qualified Canonicalize.Env              as E
import           Canonicalize.Typing
import qualified Data.Map                      as M
import           Parse.Madlib.Grammar           ( mergeAreas )
import           AST.Canonical                  ( getArea )
import           Explain.Location
import           Control.Monad.Except
import           Error.Error
import           Error.Context



class Canonicalizable a b where
  canonicalize :: E.Env -> Target -> a -> CanonicalM b


instance Canonicalizable Src.Exp Can.Exp where
  canonicalize env target (Src.Source infos area e) = case e of
    Src.LNum  x           -> return $ Can.Canonical area (Can.LNum x)

    Src.LStr  x           -> return $ Can.Canonical area (Can.LStr x)

    Src.LBool x           -> return $ Can.Canonical area (Can.LBool x)

    Src.LUnit             -> return $ Can.Canonical area Can.LUnit

    Src.TemplateString es -> do
      es' <- mapM (canonicalize env target) es
      return $ Can.Canonical area (Can.TemplateString es')

    Src.JSExp js -> do
      pushJS js
      return $ Can.Canonical area (Can.JSExp $ filterJSExp target js)

    Src.App fn arg close -> do
      fn'  <- canonicalize env target fn
      arg' <- canonicalize env target arg
      return $ Can.Canonical area (Can.App fn' arg' close)

    Src.Access rec field -> do
      rec'   <- canonicalize env target rec
      field' <- canonicalize env target field
      return $ Can.Canonical area (Can.Access rec' field')

    Src.Abs (Src.Source _ paramArea param) body -> do
      body' <- mapM (canonicalize env target) body
      let param' = Can.Canonical paramArea param
      return $ Can.Canonical area (Can.Abs param' body')

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
      exp'   <- canonicalize env target exp
      scheme <- typingToScheme env typing
      return $ Can.Canonical area (Can.TypedExp exp' scheme)

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

    Src.Where exp iss -> do
      exp' <- canonicalize env target exp
      iss' <- mapM (canonicalize env target) iss
      return $ Can.Canonical area (Can.Where exp' iss')

    Src.JsxTag name props children -> do
      pushNameAccess name
      pushNameAccess "text" -- fix for now
      let Area (Loc _ l c) (Loc _ l' c') = area
      let tagFnArea = Area (Loc 0 l c) (Loc 0 l (c + length name + 2))
      let tagFnVar = Can.Canonical tagFnArea (Can.Var name)

      children' <-
        ((\e@(Can.Canonical a _) -> Can.Canonical a (Can.ListItem e)) <$>) <$> mapM canonicalizeJsxChild children
      propFns <- mapM
        (\(Src.Source _ a (Src.JsxProp name' exp)) -> do
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

      canonicalizeJsxChild :: Src.Exp -> CanonicalM Can.Exp
      canonicalizeJsxChild e@(Src.Source _ area exp) = case exp of
        Src.TemplateString _ -> do
          e' <- canonicalize env target e
          return $ Can.Canonical area $ Can.App (Can.Canonical area (Can.Var "text")) e' True
        Src.LStr _ -> do
          e' <- canonicalize env target e
          return $ Can.Canonical area $ Can.App (Can.Canonical area (Can.Var "text")) e' True
        _ -> do
          e' <- canonicalize env target e
          return $ Can.Canonical area $ Can.JSXExpChild e'

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


instance Canonicalizable Src.ListItem Can.ListItem where
  canonicalize env target (Src.Source _ area item) = case item of
    Src.ListItem exp -> do
      exp' <- canonicalize env target exp
      return $ Can.Canonical area $ Can.ListItem exp'

    Src.ListSpread exp -> do
      exp' <- canonicalize env target exp
      return $ Can.Canonical area $ Can.ListSpread exp'


instance Canonicalizable Src.Field Can.Field where
  canonicalize env target (Src.Source _ area item) = case item of
    Src.Field (name, exp) -> do
      exp' <- canonicalize env target exp
      return $ Can.Canonical area $ Can.Field (name, exp')

    Src.FieldSpread exp -> do
      exp' <- canonicalize env target exp
      return $ Can.Canonical area $ Can.FieldSpread exp'


instance Canonicalizable Src.Is Can.Is where
  canonicalize env target (Src.Source _ area (Src.Is pat exp)) = do
    pat' <- canonicalize env target pat
    exp' <- canonicalize env target exp
    return $ Can.Canonical area (Can.Is pat' exp')


instance Canonicalizable Src.Pattern Can.Pattern where
  canonicalize env target (Src.Source _ area pat) = case pat of
    Src.PVar name       -> return $ Can.Canonical area (Can.PVar name)

    Src.PAny            -> return $ Can.Canonical area Can.PAny

    Src.PCtor name pats -> do
      pushNameAccess name
      pats' <- mapM (canonicalize env target) pats
      return $ Can.Canonical area (Can.PCtor name pats')

    Src.PNum    num  -> return $ Can.Canonical area (Can.PNum num)

    Src.PStr    str  -> return $ Can.Canonical area (Can.PStr str)

    Src.PBool   boo  -> return $ Can.Canonical area (Can.PBool boo)

    Src.PCon    name -> return $ Can.Canonical area (Can.PCon name)

    Src.PRecord pats -> do
      pats' <- mapM (canonicalize env target) pats
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


instance Canonicalizable Src.Import Can.Import where
  canonicalize env target (Src.Source _ area imp) = case imp of
    Src.NamedImport names relPath absPath ->
      return $ Can.Canonical area (Can.NamedImport (canonicalizeName <$> names) relPath absPath)

    Src.TypeImport names relPath absPath ->
      return $ Can.Canonical area (Can.TypeImport (canonicalizeName <$> names) relPath absPath)

    Src.DefaultImport namespace relPath absPath ->
      return $ Can.Canonical area (Can.DefaultImport (canonicalizeName namespace) relPath absPath)

canonicalizeName :: Src.Source Src.Name -> Can.Canonical Can.Name
canonicalizeName (Src.Source _ area name) = Can.Canonical area name
