{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Resolver
  ( Env(..)
  , RError(..)
  , ASTTable
  , resolve
  , resolveASTTable
  )
where


import qualified Data.List                     as L
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import           Grammar
import           Data.Foldable                  ( foldl'
                                                , foldlM
                                                )
import           Control.Monad.Validate         ( ValidateT
                                                , runValidateT
                                                , tolerate
                                                , refute
                                                , MonadValidate(dispute)
                                                )
import           Path                           ( computeRootPath )
import           Control.Monad                  ( liftM2
                                                , liftM3
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , ask
                                                , local
                                                , runReader
                                                , Reader
                                                )
import           Debug.Trace                    ( trace )
import           Control.Monad                  ( join )



type ASTTable = M.Map FilePath AST

data RError = TypeError Type Type -- TypeError expected actual
            | ParameterCountError Int Int -- ParameterCount expected actual
            | PathNotFound
            | CorruptedAST AST -- TODO: When we have backtraces this should go as it will become obsolete
            | FunctionNotFound Name
            | FunctionCallError Exp -- TODO: When we have backtraces this should go as it will become obsolete
            deriving(Eq, Show)

data Env =
  Env
    { vtable   :: M.Map Name Type
    , ftable   :: M.Map Name FunctionDef
    , rootPath :: Maybe FilePath
    }
    deriving(Eq, Show)


resolveASTTable :: Env -> AST -> ASTTable -> Either [RError] ASTTable
resolveASTTable env ast@AST { apath = Nothing } _ = Left [CorruptedAST ast]
resolveASTTable env ast@AST { apath = Just apath } table =
  resolveASTTable' env { rootPath = Just $ computeRootPath apath } ast table


resolveASTTable' :: Env -> AST -> ASTTable -> Either [RError] ASTTable
resolveASTTable' env@Env { rootPath = Just rootPath } ast@AST { aimports } table
  = let paths        = pathFromModName rootPath . ipath <$> aimports
        updatedTable = foldlM resolveImport table paths
    in  updatedTable >>= resolveAndUpdateAST env ast
 where
  resolveImport :: ASTTable -> FilePath -> Either [RError] ASTTable
  resolveImport table path =
    -- TODO: Remove undefined and use findAST ?
    let ast'         = fromMaybe undefined (M.lookup path table)
        updatedTable = resolveASTTable' env ast' table
    in  updatedTable >>= resolveAndUpdateAST env ast'


resolveAndUpdateAST :: Env -> AST -> ASTTable -> Either [RError] ASTTable
resolveAndUpdateAST env ast table =
  let nextEnv     = updateEnvFromTable env table
      resolvedAST = resolveAST ast nextEnv
  in  resolvedAST >>= updateASTTable table


updateASTTable :: ASTTable -> AST -> Either [RError] ASTTable
updateASTTable table ast = case apath ast of
  Just path -> return $ M.insert path ast table
  Nothing   -> Left [PathNotFound]


resolveAST :: AST -> Env -> Either [RError] AST
resolveAST ast env = runReader (runValidateT (resolve ast)) env


updateEnvFromTable :: Env -> ASTTable -> Env
updateEnvFromTable env@Env { ftable } =
  updateTable
    . M.fromList
    . (fnsToTuples <$>)
    . concat
    . (afunctions <$>)
    . M.elems
 where
  updateTable f = env { ftable = M.union f ftable }
  fnsToTuples x = (fname x, x)


pathFromModName :: FilePath -> String -> FilePath
pathFromModName root = (root ++) <$> (++ ".mad")



type ResolveM a
  = forall m . (MonadReader Env m, MonadValidate [RError] m) => m a

class Resolvable a where
  resolve :: a -> ResolveM a


-------------------------------------------------------------------------------
--                                   AST                                    --
-------------------------------------------------------------------------------
instance Resolvable AST where
  resolve ast@AST { afunctions } = do
    env      <- pushFunctionsToEnv afunctions <$> ask
    resolved <- local (const env) (resolveFunctionDefs afunctions)
    pure ast { afunctions = resolved }

resolveFunctionDefs :: [FunctionDef] -> ResolveM [FunctionDef]
resolveFunctionDefs []      = return []
resolveFunctionDefs [fd   ] = toList <$> resolve fd
resolveFunctionDefs (h : t) = do
  resolved <- resolve h
  next     <- local (pushFunctionToEnv resolved) (resolveFunctionDefs t)
  pure $ next <> [resolved]

toList :: a -> [a]
toList a = [a]

pushFunctionsToEnv :: [FunctionDef] -> Env -> Env
pushFunctionsToEnv fd env = foldr pushFunctionToEnv env fd

pushFunctionToEnv :: FunctionDef -> Env -> Env
pushFunctionToEnv fd@FunctionDef { fname } env@Env { ftable } =
  env { ftable = M.insert fname fd ftable }


-------------------------------------------------------------------------------
--                                FunctionDef                                --
-------------------------------------------------------------------------------
instance Resolvable FunctionDef where
  resolve f@FunctionDef { fbody = Body exp, ftypeDef, fparams, fname } = do
    env@Env { ftable, vtable } <- ask
    -- TODO: Rewrite that fold !
    let nextVTable = foldl' (\a (n, t) -> M.insert n t a) vtable typedParams
        nextEnv = env { ftable = M.insert fname f ftable, vtable = nextVTable }

    resolvedExpM <- local (const nextEnv) (resolve exp)
    tolerateWith f $ updateBody resolvedExpM
   where
    typedParams = case ftypeDef of
      Just typing -> zip fparams $ (init . ttypes) typing
      Nothing     -> [] -- TODO: add params with * as type ?
    (expected, actual) = case ftypeDef of
      Just x  -> (length (ttypes x) - 1, length fparams)
      Nothing -> (0, 0)
    updateBody :: Exp -> ResolveM FunctionDef
    updateBody exp = if expected == actual
      then pure f { fbody = Body exp, ftype = etype exp }
      else refute [ParameterCountError expected actual]



-------------------------------------------------------------------------------
--                                    Exp                                    --
-------------------------------------------------------------------------------
instance Resolvable Exp where

  -----------------------------------------------------------------------------
  --                                Operation                                --
  -----------------------------------------------------------------------------
  resolve e@Operation { eleft, eoperator, eright } = do
    l        <- resolve eleft
    r        <- resolve eright

    -- TODO: Give real context to TypeError !
    resolved <- pure $ case (etype l, etype r) of
      (Just ltype, Just rtype) -> resolveOperation eoperator ltype rtype
      _                        -> dispute [TypeError "" ""] >> pure ""

    resolved >>= updateOperation l r
   where
    updateOperation :: Exp -> Exp -> Type -> ResolveM Exp
    updateOperation el er t =
      pure e { eleft = el, eright = er, etype = Just t }

    resolveOperation :: Operator -> Type -> Type -> ResolveM Type
    resolveOperation TripleEq "Bool" "Bool" = pure "Bool"
    resolveOperation TripleEq "Bool" "Num" =
      dispute [TypeError "Bool" "Num"] >> pure ""
    resolveOperation TripleEq "Num" "Bool" =
      dispute [TypeError "Bool" "Num"] >> pure ""
    resolveOperation Plus "Num" "Num" = pure "Num"
    resolveOperation _ _ actual =
      dispute [TypeError "Unknown" actual] >> pure ""

  -----------------------------------------------------------------------------
  --                                VarAccess                                --
  -----------------------------------------------------------------------------
  resolve e@VarAccess { ename } = do
    env@Env { vtable } <- ask
    let t = M.lookup ename vtable in return $ e { etype = t }

  -----------------------------------------------------------------------------
  --                                 IntLit                                  --
  -----------------------------------------------------------------------------
  resolve e@IntLit{}                           = return e

  -----------------------------------------------------------------------------
  --                                StringLit                                --
  -----------------------------------------------------------------------------
  resolve e@StringLit{}                        = return e

  -----------------------------------------------------------------------------
  --                               FunctionCall                              --
  -----------------------------------------------------------------------------
  -- TODO: Handle error cases with tests for them
  resolve f@FunctionCall { ename, eargs = ea } = do
    -- Retrieve FunctionDef and derive params/return types
    functionDef   <- findFunctionDef ename
    functionTypes <- getFunctionTypes functionDef
    paramsTypes   <- getParamTypes functionTypes
    returnType    <- getReturnType functionTypes

    -- Resolve call args and derive types
    argsResolved  <- resolveArgs ea
    argsTypes     <- pure $ etype <$> argsResolved

    -- Merge call types with function definition types and validate
    typeTuples    <- pure $ zip paramsTypes argsTypes

    validate typeTuples >>= \case
      True  -> updateFunctionCall argsResolved returnType f
      False -> pure f
   where
    getFunctionTypes :: Maybe FunctionDef -> ResolveM [Maybe Type]
    getFunctionTypes fDef = pure $ case fDef >>= ftypeDef of
      (Just Typing { ttypes }) -> Just <$> ttypes
      Nothing                  -> []

    getParamTypes :: [Maybe String] -> ResolveM [Maybe String]
    getParamTypes [onlyOne] = pure []
    getParamTypes []        = pure []
    getParamTypes params    = pure $ init params

    getReturnType :: [Maybe String] -> ResolveM (Maybe String)
    getReturnType []     = pure Nothing
    getReturnType params = pure $ last params

    resolveArgs :: [Exp] -> ResolveM [Exp]
    resolveArgs exps = mapM resolve exps

    validate :: [(Maybe String, Maybe String)] -> ResolveM Bool
    validate types = if all (uncurry (==)) types
      then pure True
      else dispute [FunctionCallError f] >> pure False

    updateFunctionCall :: [Exp] -> Maybe Type -> Exp -> ResolveM Exp
    updateFunctionCall args t fc = pure fc { eargs = args, etype = t }

    findFunctionDef :: String -> ResolveM (Maybe FunctionDef)
    findFunctionDef name = do
      env@Env { ftable } <- ask
      case M.lookup name ftable of
        Just x  -> pure $ Just x
        Nothing -> dispute [FunctionNotFound name] >> pure Nothing

tolerateWith :: a -> ResolveM a -> ResolveM a
tolerateWith with v = tolerate v >>= \case
  Just a  -> pure a
  Nothing -> pure with
