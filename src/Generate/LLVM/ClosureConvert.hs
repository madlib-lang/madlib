{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Generate.LLVM.ClosureConvert where

import           Control.Monad.State
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Data.List
import qualified AST.Solved                    as Slv
import qualified Generate.LLVM.Optimized       as Opt
import           Infer.Type
import Data.Maybe
import Debug.Trace
import Text.Show.Pretty


data OptimizationState
  = OptimizationState { count :: Int, topLevel :: [Opt.Exp] }

data Env
  = Env { freeVars :: [Opt.Name], stillTopLevel :: Bool }

initialOptimizationState :: OptimizationState
initialOptimizationState = OptimizationState { count = 0, topLevel = [] }

type Optimize a = forall m . MonadState OptimizationState m => m a


numbers :: [String]
numbers = show <$> [0 ..]

generateClosureName :: Optimize String
generateClosureName = do
  s@(OptimizationState count _) <- get
  let index = numbers !! count
  let name = "$closureFn$" ++ index
  put s { count = count + 1 }
  return name

resetTopLevelExps :: Optimize ()
resetTopLevelExps = do
  s@(OptimizationState _ topLevel) <- get
  put s { topLevel = [] }

addTopLevelExp :: Opt.Exp -> Optimize ()
addTopLevelExp exp = do
  s@(OptimizationState _ topLevel) <- get
  put s { topLevel = topLevel ++ [exp] }

getTopLevelExps :: Optimize [Opt.Exp]
getTopLevelExps = do
  OptimizationState _ topLevel <- get
  return topLevel

addGlobalFreeVar :: Opt.Name -> Env -> Env
addGlobalFreeVar fv env =
  env { freeVars = fv : freeVars env }




findFreeVars :: Env -> Slv.Exp -> Optimize [(String, Opt.Exp)]
findFreeVars env exp = do
  fvs <- case exp of
    Slv.Solved _ _ (Slv.Var "+") ->
      return []

    Slv.Solved _ _ (Slv.Var "-") ->
      return []

    Slv.Solved _ _ (Slv.Var "*") ->
      return []

    Slv.Solved _ _ (Slv.Var "/") ->
      return []

    Slv.Solved _ _ (Slv.Var "==") ->
      return []

    Slv.Solved _ _ (Slv.Var "!=") ->
      return []

    Slv.Solved _ _ (Slv.Var ">") ->
      return []

    Slv.Solved _ _ (Slv.Var "<") ->
      return []

    Slv.Solved _ _ (Slv.Var n) -> do
      var' <- optimize env exp
      return [(n, var')]

    Slv.Solved _ _ (Slv.Abs (Slv.Solved _ _ param) body) -> do
      vars <- concat <$> mapM (findFreeVars env) body
      return $ filter (\(varName, _) -> varName /= param) vars

    Slv.Solved _ _ (Slv.App f arg _) -> do
      fFreeVars   <- findFreeVars env f
      argFreeVars <- findFreeVars env arg
      return $ fFreeVars ++ argFreeVars

    Slv.Solved _ _ (Slv.If cond truthy falsy) -> do
      condFreeVars   <- findFreeVars env cond
      truthyFreeVars <- findFreeVars env truthy
      falsyFreeVars  <- findFreeVars env falsy
      return $ condFreeVars ++ truthyFreeVars ++ falsyFreeVars

    Slv.Solved _ _ (Slv.TupleConstructor exps) -> do
      vars <- mapM (findFreeVars env) exps
      return $ concat vars

    Slv.Solved _ _ (Slv.ListConstructor exps) -> do
      vars <- mapM (findFreeVars env . Slv.getListItemExp) exps
      return $ concat vars

    Slv.Solved _ _ (Slv.Where whereExp iss) -> do
      expVars <- findFreeVars env whereExp
      issFreeVars <- findFreeVarsInBranches env iss
      return $ expVars ++ issFreeVars

    Slv.Solved _ _ (Slv.Assignment n exp) -> do
      findFreeVars env exp

    _ ->
      return []

  let globalVars = freeVars env
  let fvs' = M.toList $ M.fromList fvs

  return $ filter (\(varName, _) -> varName `notElem` ("inner":globalVars)) fvs'

findFreeVarsInBranches :: Env -> [Slv.Is] -> Optimize [(String, Opt.Exp)]
findFreeVarsInBranches env iss = case iss of
  (is : next) -> do
    branchVars <- findFreeVarsInBranch env is
    nextVars   <- findFreeVarsInBranches env next
    return $ branchVars ++ nextVars

  [] ->
    return []

findFreeVarsInBranch :: Env -> Slv.Is -> Optimize [(String, Opt.Exp)]
findFreeVarsInBranch env is = case is of
  Slv.Solved _ _ (Slv.Is pat exp) -> do
    let patternVars = getPatternVars pat
    expVars <- findFreeVars env exp
    return $ filter (\(varName, _) -> varName `notElem` patternVars) expVars


getPatternVars :: Slv.Pattern -> [String]
getPatternVars (Slv.Solved _ _ pat) = case pat of
  Slv.PVar n ->
    [n]

  Slv.PCon _ pats ->
    concatMap getPatternVars pats

  Slv.PRecord fields ->
    concatMap getPatternVars $ M.elems fields

  Slv.PList pats ->
    concatMap getPatternVars pats

  Slv.PTuple pats ->
    concatMap getPatternVars pats

  Slv.PSpread pat' ->
    getPatternVars pat'

  _ ->
    []



class Optimizable a b where
  optimize :: Env -> a -> Optimize b





instance Optimizable Slv.Exp Opt.Exp where
  optimize _ (Slv.Untyped area (Slv.TypeExport name)) = return $ Opt.Untyped area (Opt.TypeExport name)
  optimize env fullExp@(Slv.Solved qt@(_ :=> t) area e) = case e of
    Slv.LNum  x           -> return $ Opt.Optimized t area (Opt.LNum x)

    Slv.LStr  x           -> return $ Opt.Optimized t area (Opt.LStr x)

    Slv.LBool x           -> return $ Opt.Optimized t area (Opt.LBool x)

    Slv.LUnit             -> return $ Opt.Optimized t area Opt.LUnit

    Slv.TemplateString es -> do
      es' <- mapM (optimize env { stillTopLevel = False }) es
      return $ Opt.Optimized t area (Opt.TemplateString es')

    Slv.JSExp js         -> return $ Opt.Optimized t area (Opt.JSExp js)

    Slv.App fn arg close -> do
      fn'  <- optimize env { stillTopLevel = False } fn
      arg' <- optimize env { stillTopLevel = False } arg
      return $ Opt.Optimized t area (Opt.App fn' arg' close)

    Slv.Access rec field -> do
      rec'   <- optimize env { stillTopLevel = False } rec
      field' <- optimize env { stillTopLevel = False } field
      return $ Opt.Optimized t area (Opt.Access rec' field')

    Slv.Abs (Slv.Solved _ _ param) body -> do
      let isTopLevel = stillTopLevel env
      if isTopLevel then do
        body' <- mapM (optimize (env { stillTopLevel = False })) body
        return $ Opt.Optimized t area (Opt.Abs param body')
      else do
        body'       <- mapM (optimize env) body
        fvs         <- findFreeVars env fullExp
        closureName <- generateClosureName
        let def = Opt.Optimized t area (Opt.ClosureDef closureName (snd <$> fvs) param body')
        addTopLevelExp def

        let vars = snd <$> fvs
        let closure = Opt.Optimized t area (Opt.Closure closureName vars)
        return closure

    Slv.Assignment name exp -> do
      exp' <- optimize env exp
      return $ Opt.Optimized t area (Opt.Assignment name exp')

    Slv.Export exp -> do
      exp' <- optimize env exp
      return $ Opt.Optimized t area (Opt.Export exp')

    Slv.NameExport name     -> return $ Opt.Optimized t area (Opt.NameExport name)

    Slv.Var        name     -> return $ Opt.Optimized t area (Opt.Var name)

    Slv.TypedExp exp _ scheme -> do
      exp' <- optimize env exp
      return $ Opt.Optimized t area (Opt.TypedExp exp' scheme)

    Slv.ListConstructor items -> do
      items' <- mapM (optimize env) items
      return $ Opt.Optimized t area (Opt.ListConstructor items')

    Slv.TupleConstructor exps -> do
      exps' <- mapM (optimize env) exps
      return $ Opt.Optimized t area (Opt.TupleConstructor exps')

    Slv.Record fields -> do
      fields' <- mapM (optimize env { stillTopLevel = False }) fields
      return $ Opt.Optimized t area (Opt.Record fields')

    Slv.If cond truthy falsy -> do
      cond'   <- optimize env { stillTopLevel = False } cond
      truthy' <- optimize env { stillTopLevel = False } truthy
      falsy'  <- optimize env { stillTopLevel = False } falsy
      return $ Opt.Optimized t area (Opt.If cond' truthy' falsy')

    Slv.Do exps -> do
      exps' <- mapM (optimize env { stillTopLevel = False }) exps
      return $ Opt.Optimized t area (Opt.Do exps')

    Slv.Where exp iss -> do
      exp' <- optimize env { stillTopLevel = False } exp
      iss' <- mapM (optimize env { stillTopLevel = False }) iss
      return $ Opt.Optimized t area (Opt.Where exp' iss')

    Slv.Extern qt name originalName -> do
      return $ Opt.Optimized t area (Opt.Extern qt name originalName)

    Slv.Placeholder (placeholderRef, ts) exp -> do
      exp'            <- optimize env exp
      placeholderRef' <- optimizePlaceholderRef placeholderRef
      let tsStr = buildTypeStrForPlaceholder ts
      return $ Opt.Optimized t area (Opt.Placeholder (placeholderRef', tsStr) exp')

     where
      optimizePlaceholderRef :: Slv.PlaceholderRef -> Optimize Opt.PlaceholderRef
      optimizePlaceholderRef phr = case phr of
        Slv.ClassRef cls ps call var -> do
          ps'  <- mapM optimizeClassRefPred ps
          return $ Opt.ClassRef cls ps' call var

        Slv.MethodRef cls mtd call -> do
          return $ Opt.MethodRef cls mtd call

      optimizeClassRefPred :: Slv.ClassRefPred -> Optimize Opt.ClassRefPred
      optimizeClassRefPred (Slv.CRPNode cls ts var ps) = do
        ps'  <- mapM optimizeClassRefPred ps
        let tsStr = buildTypeStrForPlaceholder ts
        return $ Opt.CRPNode cls tsStr var ps'



instance Optimizable Slv.Typing Opt.Typing where
  optimize env (Slv.Untyped area typing) = case typing of
    Slv.TRSingle name       -> return $ Opt.Untyped area $ Opt.TRSingle name

    Slv.TRComp name typings -> do
      typings' <- mapM (optimize env) typings
      return $ Opt.Untyped area $ Opt.TRComp name typings'

    Slv.TRArr left right -> do
      left'  <- optimize env left
      right' <- optimize env right
      return $ Opt.Untyped area $ Opt.TRArr left' right'

    Slv.TRRecord fields base -> do
      fields' <- mapM (optimize env) fields
      base'   <- mapM (optimize env) base
      return $ Opt.Untyped area $ Opt.TRRecord fields' base'

    Slv.TRTuple typings -> do
      typings' <- mapM (optimize env) typings
      return $ Opt.Untyped area $ Opt.TRTuple typings'

    Slv.TRConstrained constraints typing -> do
      constraints' <- mapM (optimize env) constraints
      typing'      <- optimize env typing
      return $ Opt.Untyped area $ Opt.TRConstrained constraints' typing'

instance Optimizable Slv.ListItem Opt.ListItem where
  optimize env (Slv.Solved qt@(_ :=> t) area item) = case item of
    Slv.ListItem exp -> do
      exp' <- optimize env exp
      return $ Opt.Optimized t area $ Opt.ListItem exp'

    Slv.ListSpread exp -> do
      exp' <- optimize env exp
      return $ Opt.Optimized t area $ Opt.ListSpread exp'

instance Optimizable Slv.Field Opt.Field where
  optimize env (Slv.Solved qt@(_ :=> t) area item) = case item of
    Slv.Field (name, exp) -> do
      exp' <- optimize env exp
      return $ Opt.Optimized t area $ Opt.Field (name, exp')

    Slv.FieldSpread exp -> do
      exp' <- optimize env exp
      return $ Opt.Optimized t area $ Opt.FieldSpread exp'

instance Optimizable Slv.Is Opt.Is where
  optimize env (Slv.Solved qt@(_ :=> t) area (Slv.Is pat exp)) = do
    pat' <- optimize env pat
    exp' <- optimize env exp
    return $ Opt.Optimized t area (Opt.Is pat' exp')

instance Optimizable Slv.Pattern Opt.Pattern where
  optimize env (Slv.Solved qt@(_ :=> t) area pat) = case pat of
    Slv.PVar name       -> return $ Opt.Optimized t area $ Opt.PVar name

    Slv.PAny            -> return $ Opt.Optimized t area Opt.PAny

    Slv.PCon name pats -> do
      pats' <- mapM (optimize env) pats
      return $ Opt.Optimized t area $ Opt.PCon name pats'

    Slv.PNum    num  -> return $ Opt.Optimized t area $ Opt.PNum num

    Slv.PStr    str  -> return $ Opt.Optimized t area $ Opt.PStr str

    Slv.PBool   boo  -> return $ Opt.Optimized t area $ Opt.PBool boo

    Slv.PRecord pats -> do
      pats' <- mapM (optimize env) pats
      return $ Opt.Optimized t area $ Opt.PRecord pats'

    Slv.PList pats -> do
      pats' <- mapM (optimize env) pats
      return $ Opt.Optimized t area $ Opt.PList pats'

    Slv.PTuple pats -> do
      pats' <- mapM (optimize env) pats
      return $ Opt.Optimized t area $ Opt.PTuple pats'

    Slv.PSpread pat -> do
      pat' <- optimize env pat
      return $ Opt.Optimized t area $ Opt.PSpread pat'

instance Optimizable Slv.TypeDecl Opt.TypeDecl where
  optimize env (Slv.Untyped area typeDecl) = case typeDecl of
    adt@Slv.ADT{} -> do
      ctors <- mapM optimizeConstructors $ Slv.adtconstructors adt
      return $ Opt.Untyped area $ Opt.ADT { Opt.adtname         = Slv.adtname adt
                                          , Opt.adtparams       = Slv.adtparams adt
                                          , Opt.adtconstructors = ctors
                                          , Opt.adtexported     = Slv.adtexported adt
                                          }

    alias@Slv.Alias{} -> do
      aliastype <- optimize env $ Slv.aliastype alias
      return $ Opt.Untyped area $ Opt.Alias { Opt.aliasname     = Slv.aliasname alias
                                            , Opt.aliasparams   = Slv.aliasparams alias
                                            , Opt.aliastype     = aliastype
                                            , Opt.aliasexported = Slv.aliasexported alias
                                            }
   where
    optimizeConstructors :: Slv.Constructor -> Optimize Opt.Constructor
    optimizeConstructors (Slv.Untyped a (Slv.Constructor name typings t)) = do
      typings' <- mapM (optimize env) typings
      return $ Opt.Untyped area $ Opt.Constructor name typings' t


instance Optimizable Slv.Interface Opt.Interface where
  optimize env (Slv.Untyped area (Slv.Interface name constraints vars methods methodTypings)) = do
    methodTypings' <- mapM (optimize env) methodTypings
    return $ Opt.Untyped area $ Opt.Interface name constraints ((\(TV n _) -> n) <$> vars) methods methodTypings'

instance Optimizable Slv.Instance Opt.Instance where
  optimize env (Slv.Untyped area (Slv.Instance interface constraints pred methods)) = do
    let typingStr = intercalate "_" (getTypeHeadName <$> predTypes pred)
    methods' <- mapM (\(exp, scheme) -> (, scheme) <$> optimize env exp) methods
    return $ Opt.Untyped area $ Opt.Instance interface constraints typingStr methods'

instance Optimizable Slv.Import Opt.Import where
  optimize _ (Slv.Untyped area imp) = case imp of
    Slv.NamedImport names relPath absPath ->
      return $ Opt.Untyped area $ Opt.NamedImport (optimizeImportName <$> names) relPath absPath

    Slv.DefaultImport namespace relPath absPath ->
      return $ Opt.Untyped area $ Opt.DefaultImport (optimizeImportName namespace) relPath absPath


optimizeImportName :: Slv.Solved Slv.Name -> Opt.Optimized Opt.Name
optimizeImportName (Slv.Untyped area name) = Opt.Untyped area name

instance Optimizable Slv.AST Opt.AST where
  optimize env ast = do
    let globalVars = mapMaybe Slv.getExpName (Slv.aexps ast)
        env' = env { freeVars = globalVars }

    imports    <- mapM (optimize env') $ Slv.aimports ast
    exps       <- mapM (optimize env') $ Slv.aexps ast
    typeDecls  <- mapM (optimize env') $ Slv.atypedecls ast
    interfaces <- mapM (optimize env') $ Slv.ainterfaces ast
    instances  <- mapM (optimize env') $ Slv.ainstances ast

    defs <- getTopLevelExps
    resetTopLevelExps

    return $ Opt.AST { Opt.aimports    = imports
                     , Opt.aexps       = defs ++ exps
                     , Opt.atypedecls  = typeDecls
                     , Opt.ainterfaces = interfaces
                     , Opt.ainstances  = instances
                     , Opt.apath       = Slv.apath ast
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
  TVar (TV n _)   -> n
  TCon (TC n _) _ -> case n of
    "()"          -> "Unit"
    "(,)"         -> "Tuple_2"
    "(,,)"        -> "Tuple_3"
    "(,,,)"       -> "Tuple_4"
    "(,,,,)"      -> "Tuple_5"
    "(,,,,,)"     -> "Tuple_6"
    "(,,,,,,)"    -> "Tuple_7"
    "(,,,,,,,)"   -> "Tuple_8"
    "(,,,,,,,,)"  -> "Tuple_9"
    "(,,,,,,,,,)" -> "Tuple_10"
    _             -> n
  TApp (TApp (TCon (TC "(->)" _) _) tl) tr -> getTypeHeadName tl <> "_arr_" <> getTypeHeadName tr
  TApp l _  -> getTypeHeadName l


-- I think at some point we might want to follow imports in the optimization
-- process in order to correctly reduce dictionaries in the right order and have
-- an env for optimization to keep track of what dictionaries have been removed.
optimizeTable :: Slv.Table -> Opt.Table
optimizeTable table =
  let env       = Env { freeVars = [], stillTopLevel = True }
      optimized = mapM (optimize env) table
  in  evalState optimized initialOptimizationState
