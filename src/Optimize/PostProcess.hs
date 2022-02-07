{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Optimize.PostProcess where

import qualified Control.Monad.State           as MonadState
import qualified Data.Map                      as M
import           Data.List
import qualified AST.Solved                    as Slv
import qualified AST.PostProcessed             as PP
import           Infer.Type
import Explain.Location


data State
  = State { typeCount  :: Int
          , classCount :: Int
          , typeMap    :: M.Map String String
          , classMap   :: M.Map String String
          }

initialOptimizationState :: State
initialOptimizationState = State { typeCount = 0, classCount = 0, typeMap = mempty, classMap = mempty }

type PostProcess a = forall m . MonadState.MonadState State m => m a

numbers :: [String]
numbers = show <$> [0 ..]

generateClassShortname :: String -> PostProcess String
generateClassShortname "Eq" = return "Eq"
generateClassShortname n = do
  s <- MonadState.get
  let shortName = 'Ι' : numbers !! (1 + classCount s)
  MonadState.put s { classCount = classCount s + 1, classMap = M.insert n shortName (classMap s) }
  return shortName

getClassShortname :: Bool -> String -> PostProcess String
getClassShortname enabled n
  | not enabled = return n
  | otherwise = do
    s <- MonadState.get
    case M.lookup n (classMap s) of
      Just x  -> return x
      Nothing -> generateClassShortname n

generateTypeShortname :: String -> PostProcess String
generateTypeShortname n = do
  s <- MonadState.get
  let shortName = 'τ' : numbers !! (1 + typeCount s)
  MonadState.put s { typeCount = typeCount s + 1, typeMap = M.insert n shortName (typeMap s) }
  return shortName

getTypeShortname :: Bool -> String -> PostProcess String
getTypeShortname enabled n
  | not enabled = return n
  | otherwise = do
    s <- MonadState.get
    case M.lookup n (typeMap s) of
      Just x  -> return x
      Nothing -> generateTypeShortname n


collectAppArgs :: Bool -> Slv.Exp -> (Slv.Exp, [Slv.Exp])
collectAppArgs isFirst app = case app of
  Slv.Typed _ _ (Slv.App next arg isFinal) | not isFinal || isFirst ->
    let (nextFn, nextArgs) = collectAppArgs False next
    in  (nextFn, nextArgs <> [arg])

  b ->
    (b, [])


collectAbsParams :: Slv.Exp -> ([String], [Slv.Exp])
collectAbsParams abs = case abs of
  Slv.Typed _ _ (Slv.Abs (Slv.Typed _ _ param) [body]) ->
    let (nextParams, nextBody) = collectAbsParams body
    in  (param : nextParams, nextBody)

  Slv.Typed _ _ (Slv.Abs (Slv.Typed _ _ param) body) ->
    ([param], body)

  b ->
    ([], [b])


buildAbs :: [(String, Qual Type)] -> [Slv.Exp] -> Slv.Exp
buildAbs [(param, ps :=> t)] body =
  let bodyType = Slv.getType (last body)
  in  Slv.Typed (ps :=> (t `fn` bodyType)) emptyArea (Slv.Abs (Slv.Typed (ps :=> t) emptyArea param) body)
buildAbs ((param, ps :=> t) : xs) body =
  let next     = buildAbs xs body
      nextType = Slv.getType next
  in  Slv.Typed ([] :=> (t `fn` nextType)) emptyArea (Slv.Abs (Slv.Typed (ps :=> t) emptyArea param) [next])


buildApp :: Slv.Exp -> [Slv.Exp] -> Slv.Exp
buildApp f args =
  buildApp' (length args) (length args) f args

buildApp' :: Int -> Int -> Slv.Exp -> [Slv.Exp] -> Slv.Exp
buildApp' total nth f@(Slv.Typed (ps :=> t) area f') [arg] =
  Slv.Typed (ps :=> dropFirstParamType t) area (Slv.App f arg (total == nth))
buildApp' total nth f@(Slv.Typed (ps :=> t) _ f') xs =
  let arg@(Slv.Typed _ area _) = last xs
      subApp                    = buildApp' total (nth - 1) f (init xs)
  in  Slv.Typed (ps :=> dropNFirstParamTypes nth t) area (Slv.App subApp arg (total == nth))


-- Checks for Var "$" placeholders
-- Returns all the args to be applied, as well as the name of params to wrap it
placeholderArgCheck :: Int -> [Slv.Exp] -> ([Slv.Exp], [(String, Qual Type)])
placeholderArgCheck phIndex args = case args of
  (arg : next) -> case arg of
    Slv.Typed t area (Slv.Var "$") ->
      let paramName          = "__$" ++ show phIndex ++ "__"
          (nextArgs, params) = placeholderArgCheck (phIndex + 1) next
      in  (Slv.Typed t area (Slv.Var paramName) : nextArgs, (paramName, t) : params)

    _ ->
      let (nextArgs, params) = placeholderArgCheck phIndex next
      in  (arg : nextArgs, params)

  [] ->
    ([], [])


class Processable a b where
  -- Bool is True when we need to contract names for JS output
  postProcess :: Bool -> a -> PostProcess b

instance Processable Slv.Exp PP.Exp where
  postProcess _ (Slv.Untyped area (Slv.TypeExport name)) = return $ PP.Untyped area (PP.TypeExport name)
  postProcess enabled fullExp@(Slv.Typed qt@(_ :=> t) area e) = case e of
    Slv.LNum  x           -> return $ PP.Typed t area (PP.Literal (PP.LNum x))

    Slv.LFloat x          -> return $ PP.Typed t area (PP.Literal (PP.LFloat x))

    Slv.LStr  x           -> return $ PP.Typed t area (PP.Literal (PP.LStr x))

    Slv.LBool x           -> return $ PP.Typed t area (PP.Literal (PP.LBool x))

    Slv.LUnit             -> return $ PP.Typed t area (PP.Literal PP.LUnit)

    Slv.TemplateString es -> do
      es' <- mapM (postProcess enabled) es
      return $ PP.Typed t area (PP.TemplateString es')

    Slv.JSExp js         -> return $ PP.Typed t area (PP.JSExp js)

    Slv.App fn arg close -> do
      let (fn', args)                       = collectAppArgs True fullExp
          (args', wrapperPlaceholderParams) = placeholderArgCheck 0 args
      if null wrapperPlaceholderParams then do
        fn''  <- postProcess enabled fn'
        args' <- mapM (postProcess enabled) args
        return $ PP.Typed t area (PP.Call fn'' args')
      else do
        -- if we found some Var "$" args we need to wrap it in an Abs
        -- params
        let appWithRenamedArgs = buildApp fn' args'
            wrapperAbs         = buildAbs wrapperPlaceholderParams [appWithRenamedArgs]
        postProcess enabled wrapperAbs

    Slv.Access rec field -> do
      rec'   <- postProcess enabled rec
      field' <- postProcess enabled field
      return $ PP.Typed t area (PP.Access rec' field')

    Slv.Abs (Slv.Typed _ _ param) body -> do
      let (params, body') = collectAbsParams fullExp
      body'' <- mapM (postProcess enabled) body'
      return $ PP.Typed t area (PP.Definition params body'')

    Slv.Assignment name exp -> do
      exp' <- postProcess enabled exp
      return $ PP.Typed t area (PP.Assignment name exp')

    Slv.Export exp -> do
      exp' <- postProcess enabled exp
      return $ PP.Typed t area (PP.Export exp')

    Slv.NameExport name     -> return $ PP.Typed t area (PP.NameExport name)

    Slv.Var        name     -> return $ PP.Typed t area (PP.Var name)

    Slv.TypedExp exp _ scheme -> do
      exp' <- postProcess enabled exp
      return $ PP.Typed t area (PP.TypedExp exp' scheme)

    Slv.ListConstructor items -> do
      items' <- mapM (postProcess enabled) items
      return $ PP.Typed t area (PP.ListConstructor items')

    Slv.TupleConstructor exps -> do
      exps' <- mapM (postProcess enabled) exps
      return $ PP.Typed t area (PP.TupleConstructor exps')

    Slv.Record fields -> do
      fields' <- mapM (postProcess enabled) fields
      return $ PP.Typed t area (PP.Record fields')

    Slv.If cond truthy falsy -> do
      cond'   <- postProcess enabled cond
      truthy' <- postProcess enabled truthy
      falsy'  <- postProcess enabled falsy
      return $ PP.Typed t area (PP.If cond' truthy' falsy')

    Slv.Do exps -> do
      exps' <- mapM (postProcess enabled) exps
      return $ PP.Typed t area (PP.Do exps')

    Slv.Where exp iss -> do
      exp' <- postProcess enabled exp
      iss' <- mapM (postProcess enabled) iss
      return $ PP.Typed t area (PP.Where exp' iss')

    Slv.Extern qt name foreignName ->
      return $ PP.Typed t area (PP.Extern qt name foreignName)

    Slv.Placeholder (Slv.ClassRef "Number" _ _ _, ts) exp ->
      postProcess enabled exp

    Slv.Placeholder (Slv.MethodRef "Number" _ _, ts) exp ->
      postProcess enabled exp

    Slv.Placeholder (Slv.ClassRef "Eq" _ _ _, ts) exp ->
      postProcess enabled exp

    Slv.Placeholder (Slv.MethodRef "Eq" _ _, ts) exp ->
      postProcess enabled exp

    Slv.Placeholder (placeholderRef, ts) exp -> do
      exp'            <- postProcess enabled exp
      placeholderRef' <- optimizePlaceholderRef placeholderRef
      let tsStr = buildTypeStrForPlaceholder ts
      ts' <- getTypeShortname enabled tsStr
      return $ PP.Typed t area (PP.Placeholder (placeholderRef', ts') exp')

     where
      optimizePlaceholderRef :: Slv.PlaceholderRef -> PostProcess PP.PlaceholderRef
      optimizePlaceholderRef phr = case phr of
        Slv.ClassRef cls ps call var -> do
          ps'  <- mapM optimizeClassRefPred ps
          cls' <- getClassShortname enabled cls
          return $ PP.ClassRef cls' ps' call var

        Slv.MethodRef cls mtd call -> do
          cls' <- getClassShortname enabled cls
          return $ PP.MethodRef cls' mtd call

      optimizeClassRefPred :: Slv.ClassRefPred -> PostProcess PP.ClassRefPred
      optimizeClassRefPred (Slv.CRPNode cls ts var ps) = do
        ps'  <- mapM optimizeClassRefPred ps
        cls' <- getClassShortname enabled cls
        let tsStr = buildTypeStrForPlaceholder ts
        ts' <- getTypeShortname enabled tsStr
        return $ PP.CRPNode cls' ts' var ps'



instance Processable Slv.Typing PP.Typing where
  postProcess enabled (Slv.Untyped area typing) = case typing of
    Slv.TRSingle name       -> return $ PP.Untyped area $ PP.TRSingle name

    Slv.TRComp name typings -> do
      typings' <- mapM (postProcess enabled) typings
      return $ PP.Untyped area $ PP.TRComp name typings'

    Slv.TRArr left right -> do
      left'  <- postProcess enabled left
      right' <- postProcess enabled right
      return $ PP.Untyped area $ PP.TRArr left' right'

    Slv.TRRecord fields base -> do
      fields' <- mapM (postProcess enabled) fields
      base'   <- mapM (postProcess enabled) base
      return $ PP.Untyped area $ PP.TRRecord fields' base'

    Slv.TRTuple typings -> do
      typings' <- mapM (postProcess enabled) typings
      return $ PP.Untyped area $ PP.TRTuple typings'

    Slv.TRConstrained constraints typing -> do
      constraints' <- mapM (postProcess enabled) constraints
      typing'      <- postProcess enabled typing
      return $ PP.Untyped area $ PP.TRConstrained constraints' typing'

instance Processable Slv.ListItem PP.ListItem where
  postProcess enabled (Slv.Typed qt@(_ :=> t) area item) = case item of
    Slv.ListItem exp -> do
      exp' <- postProcess enabled exp
      return $ PP.Typed t area $ PP.ListItem exp'

    Slv.ListSpread exp -> do
      exp' <- postProcess enabled exp
      return $ PP.Typed t area $ PP.ListSpread exp'

instance Processable Slv.Field PP.Field where
  postProcess enabled (Slv.Typed qt@(_ :=> t) area item) = case item of
    Slv.Field (name, exp) -> do
      exp' <- postProcess enabled exp
      return $ PP.Typed t area $ PP.Field (name, exp')

    Slv.FieldSpread exp -> do
      exp' <- postProcess enabled exp
      return $ PP.Typed t area $ PP.FieldSpread exp'

instance Processable Slv.Is PP.Is where
  postProcess enabled (Slv.Typed qt@(_ :=> t) area (Slv.Is pat exp)) = do
    pat' <- postProcess enabled pat
    exp' <- postProcess enabled exp
    return $ PP.Typed t area (PP.Is pat' exp')

instance Processable Slv.Pattern PP.Pattern where
  postProcess enabled (Slv.Typed qt@(_ :=> t) area pat) = case pat of
    Slv.PVar name       -> return $ PP.Typed t area $ PP.PVar name

    Slv.PAny            -> return $ PP.Typed t area PP.PAny

    Slv.PCon name pats -> do
      pats' <- mapM (postProcess enabled) pats
      return $ PP.Typed t area $ PP.PCon name pats'

    Slv.PNum    num  -> return $ PP.Typed t area $ PP.PNum num

    Slv.PStr    str  -> return $ PP.Typed t area $ PP.PStr str

    Slv.PBool   boo  -> return $ PP.Typed t area $ PP.PBool boo

    Slv.PRecord pats -> do
      pats' <- mapM (postProcess enabled) pats
      return $ PP.Typed t area $ PP.PRecord pats'

    Slv.PList pats -> do
      pats' <- mapM (postProcess enabled) pats
      return $ PP.Typed t area $ PP.PList pats'

    Slv.PTuple pats -> do
      pats' <- mapM (postProcess enabled) pats
      return $ PP.Typed t area $ PP.PTuple pats'

    Slv.PSpread pat -> do
      pat' <- postProcess enabled pat
      return $ PP.Typed t area $ PP.PSpread pat'

instance Processable Slv.TypeDecl PP.TypeDecl where
  postProcess enabled (Slv.Untyped area typeDecl) = case typeDecl of
    adt@Slv.ADT{} -> do
      ctors <- mapM optimizeConstructors $ Slv.adtconstructors adt
      return $ PP.Untyped area $ PP.ADT { PP.adtname         = Slv.adtname adt
                                          , PP.adtparams       = Slv.adtparams adt
                                          , PP.adtconstructors = ctors
                                          , PP.adtexported     = Slv.adtexported adt
                                          }

    alias@Slv.Alias{} -> do
      aliastype <- postProcess enabled $ Slv.aliastype alias
      return $ PP.Untyped area $ PP.Alias { PP.aliasname     = Slv.aliasname alias
                                            , PP.aliasparams   = Slv.aliasparams alias
                                            , PP.aliastype     = aliastype
                                            , PP.aliasexported = Slv.aliasexported alias
                                            }
   where
    optimizeConstructors :: Slv.Constructor -> PostProcess PP.Constructor
    optimizeConstructors (Slv.Untyped a (Slv.Constructor name typings _)) = do
      typings' <- mapM (postProcess enabled) typings
      return $ PP.Untyped area $ PP.Constructor name typings'


instance Processable Slv.Interface PP.Interface where
  postProcess enabled (Slv.Untyped area (Slv.Interface name constraints vars methods methodTypings)) = do
    name'          <- getClassShortname enabled name
    methodTypings' <- mapM (postProcess enabled) methodTypings
    return $ PP.Untyped area $ PP.Interface name' constraints ((\(TV n _) -> n) <$> vars) methods methodTypings'

instance Processable Slv.Instance PP.Instance where
  postProcess enabled (Slv.Untyped area (Slv.Instance interface constraints pred methods)) = do
    interface' <- getClassShortname enabled interface
    let typingStr = intercalate "_" (getTypeHeadName <$> predTypes pred)
    typings' <- getTypeShortname enabled typingStr
    methods' <- mapM (\(exp, scheme) -> (, scheme) <$> postProcess enabled exp) methods
    return $ PP.Untyped area $ PP.Instance interface' constraints typings' methods'

instance Processable Slv.Import PP.Import where
  postProcess _ (Slv.Untyped area imp) = case imp of
    Slv.NamedImport names relPath absPath ->
      return $ PP.Untyped area $ PP.NamedImport (optimizeImportName <$> names) relPath absPath

    Slv.DefaultImport namespace relPath absPath ->
      return $ PP.Untyped area $ PP.DefaultImport (optimizeImportName namespace) relPath absPath


optimizeImportName :: Slv.Typed Slv.Name -> PP.Optimized PP.Name
optimizeImportName (Slv.Untyped area name) = PP.Untyped area name

instance Processable Slv.AST PP.AST where
  postProcess enabled ast = do
    imports    <- mapM (postProcess enabled) $ Slv.aimports ast
    exps       <- mapM (postProcess enabled) $ Slv.aexps ast
    typeDecls  <- mapM (postProcess enabled) $ Slv.atypedecls ast
    interfaces <- mapM (postProcess enabled) $ Slv.ainterfaces ast
    instances  <- mapM (postProcess enabled) $ Slv.ainstances ast

    return $ PP.AST { PP.aimports    = imports
                     , PP.aexps       = exps
                     , PP.atypedecls  = typeDecls
                     , PP.ainterfaces = interfaces
                     , PP.ainstances  = instances
                     , PP.apath       = Slv.apath ast
                     }


typingToStr :: Slv.Typing -> String
typingToStr (Slv.Untyped _ t) = case t of
  Slv.TRSingle n -> n

  Slv.TRComp n _ -> if "." `isInfixOf` n then tail $ dropWhile (/= '.') n else n

  Slv.TRTuple ts -> "Tuple_" <> show (length ts)

buildTypeStrForPlaceholder :: [Type] -> String
buildTypeStrForPlaceholder ts = intercalate "_" $ getTypeHeadName <$> ts

getTypeHeadName :: Type -> String
getTypeHeadName t = case t of
  TVar (TV n _)   ->
    n

  TCon (TC n _) _ ->
    case n of
      "{}" ->
        "Unit"

      "(,)" ->
        "Tuple_2"

      "(,,)" ->
        "Tuple_3"

      "(,,,)" ->
        "Tuple_4"

      "(,,,,)" ->
        "Tuple_5"

      "(,,,,,)" ->
        "Tuple_6"

      "(,,,,,,)" ->
        "Tuple_7"

      "(,,,,,,,)" ->
        "Tuple_8"

      "(,,,,,,,,)" ->
        "Tuple_9"

      "(,,,,,,,,,)" ->
        "Tuple_10"

      _ ->
        n

  TApp (TApp (TCon (TC "(->)" _) _) tl) tr ->
    getTypeHeadName tl <> "_arr_" <> getTypeHeadName tr

  TApp l _ ->
    getTypeHeadName l

  TRecord fields _ ->
    let fields'   = M.map getTypeHeadName fields
        fieldsStr = intercalate "_" $ uncurry (++) <$> M.toList fields'
    in  "Record" <> "_" <> fieldsStr


-- I think at some point we might want to follow imports in the optimization
-- process in order to correctly reduce dictionaries in the right order and have
-- an env for optimization to keep track of what dictionaries have been removed.
postProcessTable :: Bool -> Slv.Table -> PP.Table
postProcessTable enabled table =
  let postProcessed = mapM (postProcess enabled) table
  in  MonadState.evalState postProcessed initialOptimizationState
