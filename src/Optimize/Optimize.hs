{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Optimize.Optimize where

import           Control.Monad.State
import qualified Data.Map                      as M
import           Data.List
import qualified AST.Solved                    as Slv
import qualified AST.Optimized                 as Opt
import           Infer.Type


data OptimizationState
  = OptimizationState { typeCount  :: Int
                      , classCount :: Int
                      , typeMap    :: M.Map String String
                      , classMap   :: M.Map String String
                      }

initialOptimizationState :: OptimizationState
initialOptimizationState = OptimizationState { typeCount = 0, classCount = 0, typeMap = mempty, classMap = mempty }

type Optimize a = forall m . MonadState OptimizationState m => m a

numbers :: [String]
numbers = show <$> [0 ..]

generateClassShortname :: String -> Optimize String
generateClassShortname "Eq" = return "Eq"
generateClassShortname n = do
  s <- get
  let shortName = 'Ι' : numbers !! (1 + classCount s)
  put s { classCount = classCount s + 1, classMap = M.insert n shortName (classMap s) }
  return shortName

getClassShortname :: Bool -> String -> Optimize String
getClassShortname enabled n
  | not enabled = return n
  | otherwise = do
    s <- get
    case M.lookup n (classMap s) of
      Just x  -> return x
      Nothing -> generateClassShortname n

generateTypeShortname :: String -> Optimize String
generateTypeShortname n = do
  s <- get
  let shortName = 'τ' : numbers !! (1 + typeCount s)
  put s { typeCount = typeCount s + 1, typeMap = M.insert n shortName (typeMap s) }
  return shortName

getTypeShortname :: Bool -> String -> Optimize String
getTypeShortname enabled n
  | not enabled = return n
  | otherwise = do
    s <- get
    case M.lookup n (typeMap s) of
      Just x  -> return x
      Nothing -> generateTypeShortname n


class Optimizable a b where
  -- Bool is True when --optimize is used
  optimize :: Bool -> a -> Optimize b

instance Optimizable Slv.Exp Opt.Exp where
  optimize _ (Slv.Untyped area (Slv.TypeExport name)) = return $ Opt.Untyped area (Opt.TypeExport name)
  optimize enabled (Slv.Solved qt@(_ :=> t) area e) = case e of
    Slv.LNum  x           -> return $ Opt.Optimized t area (Opt.LNum x)

    Slv.LFloat x          -> return $ Opt.Optimized t area (Opt.LNum x)

    Slv.LStr  x           -> return $ Opt.Optimized t area (Opt.LStr x)

    Slv.LBool x           -> return $ Opt.Optimized t area (Opt.LBool x)

    Slv.LUnit             -> return $ Opt.Optimized t area Opt.LUnit

    Slv.TemplateString es -> do
      es' <- mapM (optimize enabled) es
      return $ Opt.Optimized t area (Opt.TemplateString es')

    Slv.JSExp js         -> return $ Opt.Optimized t area (Opt.JSExp js)

    Slv.App fn arg close -> do
      fn'  <- optimize enabled fn
      arg' <- optimize enabled arg
      return $ Opt.Optimized t area (Opt.App fn' arg' close)

    Slv.Access rec field -> do
      rec'   <- optimize enabled rec
      field' <- optimize enabled field
      return $ Opt.Optimized t area (Opt.Access rec' field')

    Slv.Abs (Slv.Solved _ _ param) body -> do
      body' <- mapM (optimize enabled) body
      return $ Opt.Optimized t area (Opt.Abs param body')

    Slv.Assignment name exp -> do
      exp' <- optimize enabled exp
      return $ Opt.Optimized t area (Opt.Assignment name exp')

    Slv.Export exp -> do
      exp' <- optimize enabled exp
      return $ Opt.Optimized t area (Opt.Export exp')

    Slv.NameExport name     -> return $ Opt.Optimized t area (Opt.NameExport name)

    Slv.Var        name     -> return $ Opt.Optimized t area (Opt.Var name)

    Slv.TypedExp exp _ scheme -> do
      exp' <- optimize enabled exp
      return $ Opt.Optimized t area (Opt.TypedExp exp' scheme)

    Slv.ListConstructor items -> do
      items' <- mapM (optimize enabled) items
      return $ Opt.Optimized t area (Opt.ListConstructor items')

    Slv.TupleConstructor exps -> do
      exps' <- mapM (optimize enabled) exps
      return $ Opt.Optimized t area (Opt.TupleConstructor exps')

    Slv.Record fields -> do
      fields' <- mapM (optimize enabled) fields
      return $ Opt.Optimized t area (Opt.Record fields')

    Slv.If cond truthy falsy -> do
      cond'   <- optimize enabled cond
      truthy' <- optimize enabled truthy
      falsy'  <- optimize enabled falsy
      return $ Opt.Optimized t area (Opt.If cond' truthy' falsy')

    Slv.Do exps -> do
      exps' <- mapM (optimize enabled) exps
      return $ Opt.Optimized t area (Opt.Do exps')

    Slv.Where exp iss -> do
      exp' <- optimize enabled exp
      iss' <- mapM (optimize enabled) iss
      return $ Opt.Optimized t area (Opt.Where exp' iss')

    Slv.Placeholder (Slv.ClassRef "Number" _ _ _, ts) exp ->
      optimize enabled exp

    Slv.Placeholder (Slv.MethodRef "Number" _ _, ts) exp ->
      optimize enabled exp

    Slv.Placeholder (Slv.ClassRef "Eq" _ _ _, ts) exp ->
      optimize enabled exp

    Slv.Placeholder (Slv.MethodRef "Eq" _ _, ts) exp ->
      optimize enabled exp

    Slv.Placeholder (placeholderRef, ts) exp -> do
      exp'            <- optimize enabled exp
      placeholderRef' <- optimizePlaceholderRef placeholderRef
      let tsStr = buildTypeStrForPlaceholder ts
      ts' <- getTypeShortname enabled tsStr
      return $ Opt.Optimized t area (Opt.Placeholder (placeholderRef', ts') exp')

     where
      optimizePlaceholderRef :: Slv.PlaceholderRef -> Optimize Opt.PlaceholderRef
      optimizePlaceholderRef phr = case phr of
        Slv.ClassRef cls ps call var -> do
          ps'  <- mapM optimizeClassRefPred ps
          cls' <- getClassShortname enabled cls
          return $ Opt.ClassRef cls' ps' call var

        Slv.MethodRef cls mtd call -> do
          cls' <- getClassShortname enabled cls
          return $ Opt.MethodRef cls' mtd call

      optimizeClassRefPred :: Slv.ClassRefPred -> Optimize Opt.ClassRefPred
      optimizeClassRefPred (Slv.CRPNode cls ts var ps) = do
        ps'  <- mapM optimizeClassRefPred ps
        cls' <- getClassShortname enabled cls
        let tsStr = buildTypeStrForPlaceholder ts
        ts' <- getTypeShortname enabled tsStr
        return $ Opt.CRPNode cls' ts' var ps'



instance Optimizable Slv.Typing Opt.Typing where
  optimize enabled (Slv.Untyped area typing) = case typing of
    Slv.TRSingle name       -> return $ Opt.Untyped area $ Opt.TRSingle name

    Slv.TRComp name typings -> do
      typings' <- mapM (optimize enabled) typings
      return $ Opt.Untyped area $ Opt.TRComp name typings'

    Slv.TRArr left right -> do
      left'  <- optimize enabled left
      right' <- optimize enabled right
      return $ Opt.Untyped area $ Opt.TRArr left' right'

    Slv.TRRecord fields base -> do
      fields' <- mapM (optimize enabled) fields
      base'   <- mapM (optimize enabled) base
      return $ Opt.Untyped area $ Opt.TRRecord fields' base'

    Slv.TRTuple typings -> do
      typings' <- mapM (optimize enabled) typings
      return $ Opt.Untyped area $ Opt.TRTuple typings'

    Slv.TRConstrained constraints typing -> do
      constraints' <- mapM (optimize enabled) constraints
      typing'      <- optimize enabled typing
      return $ Opt.Untyped area $ Opt.TRConstrained constraints' typing'

instance Optimizable Slv.ListItem Opt.ListItem where
  optimize enabled (Slv.Solved qt@(_ :=> t) area item) = case item of
    Slv.ListItem exp -> do
      exp' <- optimize enabled exp
      return $ Opt.Optimized t area $ Opt.ListItem exp'

    Slv.ListSpread exp -> do
      exp' <- optimize enabled exp
      return $ Opt.Optimized t area $ Opt.ListSpread exp'

instance Optimizable Slv.Field Opt.Field where
  optimize enabled (Slv.Solved qt@(_ :=> t) area item) = case item of
    Slv.Field (name, exp) -> do
      exp' <- optimize enabled exp
      return $ Opt.Optimized t area $ Opt.Field (name, exp')

    Slv.FieldSpread exp -> do
      exp' <- optimize enabled exp
      return $ Opt.Optimized t area $ Opt.FieldSpread exp'

instance Optimizable Slv.Is Opt.Is where
  optimize enabled (Slv.Solved qt@(_ :=> t) area (Slv.Is pat exp)) = do
    pat' <- optimize enabled pat
    exp' <- optimize enabled exp
    return $ Opt.Optimized t area (Opt.Is pat' exp')

instance Optimizable Slv.Pattern Opt.Pattern where
  optimize enabled (Slv.Solved qt@(_ :=> t) area pat) = case pat of
    Slv.PVar name       -> return $ Opt.Optimized t area $ Opt.PVar name

    Slv.PAny            -> return $ Opt.Optimized t area Opt.PAny

    Slv.PCon name pats -> do
      pats' <- mapM (optimize enabled) pats
      return $ Opt.Optimized t area $ Opt.PCon name pats'

    Slv.PNum    num  -> return $ Opt.Optimized t area $ Opt.PNum num

    Slv.PStr    str  -> return $ Opt.Optimized t area $ Opt.PStr str

    Slv.PBool   boo  -> return $ Opt.Optimized t area $ Opt.PBool boo

    Slv.PRecord pats -> do
      pats' <- mapM (optimize enabled) pats
      return $ Opt.Optimized t area $ Opt.PRecord pats'

    Slv.PList pats -> do
      pats' <- mapM (optimize enabled) pats
      return $ Opt.Optimized t area $ Opt.PList pats'

    Slv.PTuple pats -> do
      pats' <- mapM (optimize enabled) pats
      return $ Opt.Optimized t area $ Opt.PTuple pats'

    Slv.PSpread pat -> do
      pat' <- optimize enabled pat
      return $ Opt.Optimized t area $ Opt.PSpread pat'

instance Optimizable Slv.TypeDecl Opt.TypeDecl where
  optimize enabled (Slv.Untyped area typeDecl) = case typeDecl of
    adt@Slv.ADT{} -> do
      ctors <- mapM optimizeConstructors $ Slv.adtconstructors adt
      return $ Opt.Untyped area $ Opt.ADT { Opt.adtname         = Slv.adtname adt
                                          , Opt.adtparams       = Slv.adtparams adt
                                          , Opt.adtconstructors = ctors
                                          , Opt.adtexported     = Slv.adtexported adt
                                          }

    alias@Slv.Alias{} -> do
      aliastype <- optimize enabled $ Slv.aliastype alias
      return $ Opt.Untyped area $ Opt.Alias { Opt.aliasname     = Slv.aliasname alias
                                            , Opt.aliasparams   = Slv.aliasparams alias
                                            , Opt.aliastype     = aliastype
                                            , Opt.aliasexported = Slv.aliasexported alias
                                            }
   where
    optimizeConstructors :: Slv.Constructor -> Optimize Opt.Constructor
    optimizeConstructors (Slv.Untyped a (Slv.Constructor name typings _)) = do
      typings' <- mapM (optimize enabled) typings
      return $ Opt.Untyped area $ Opt.Constructor name typings'


instance Optimizable Slv.Interface Opt.Interface where
  optimize enabled (Slv.Untyped area (Slv.Interface name constraints vars methods methodTypings)) = do
    name'          <- getClassShortname enabled name
    methodTypings' <- mapM (optimize enabled) methodTypings
    return $ Opt.Untyped area $ Opt.Interface name' constraints ((\(TV n _) -> n) <$> vars) methods methodTypings'

instance Optimizable Slv.Instance Opt.Instance where
  optimize enabled (Slv.Untyped area (Slv.Instance interface constraints pred methods)) = do
    interface' <- getClassShortname enabled interface
    let typingStr = intercalate "_" (getTypeHeadName <$> predTypes pred)
    typings' <- getTypeShortname enabled typingStr
    methods' <- mapM (\(exp, scheme) -> (, scheme) <$> optimize enabled exp) methods
    return $ Opt.Untyped area $ Opt.Instance interface' constraints typings' methods'

instance Optimizable Slv.Import Opt.Import where
  optimize _ (Slv.Untyped area imp) = case imp of
    Slv.NamedImport names relPath absPath ->
      return $ Opt.Untyped area $ Opt.NamedImport (optimizeImportName <$> names) relPath absPath

    Slv.DefaultImport namespace relPath absPath ->
      return $ Opt.Untyped area $ Opt.DefaultImport (optimizeImportName namespace) relPath absPath


optimizeImportName :: Slv.Solved Slv.Name -> Opt.Optimized Opt.Name
optimizeImportName (Slv.Untyped area name) = Opt.Untyped area name

instance Optimizable Slv.AST Opt.AST where
  optimize enabled ast = do
    imports    <- mapM (optimize enabled) $ Slv.aimports ast
    exps       <- mapM (optimize enabled) $ Slv.aexps ast
    typeDecls  <- mapM (optimize enabled) $ Slv.atypedecls ast
    interfaces <- mapM (optimize enabled) $ Slv.ainterfaces ast
    instances  <- mapM (optimize enabled) $ Slv.ainstances ast

    return $ Opt.AST { Opt.aimports    = imports
                     , Opt.aexps       = exps
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
    "{}"          -> "Unit"
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
optimizeTable :: Bool -> Slv.Table -> Opt.Table
optimizeTable enabled table =
  let optimized = mapM (optimize enabled) table in evalState optimized initialOptimizationState
