{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
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
import           Data.Foldable                  ( foldlM )
import           Control.Monad.Validate         ( tolerate
                                                , refute
                                                , runValidate
                                                , MonadValidate(dispute)
                                                , Validate
                                                )
import           Path                           ( computeRootPath )
import           Control.Monad                  ( liftM2
                                                , liftM3
                                                )



type ASTTable = M.Map FilePath AST

data RError = TypeError Type Type
            | ParameterCountError Int Int
            | PathNotFound
            | CorruptedAST AST
            | FunctionNotFound Name
            | FunctionCallError Exp
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
resolveAST ast env = runValidate $ resolve env ast


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


class Resolvable a where
  resolve :: Env -> a -> Validate [RError] a

instance Resolvable AST where
  resolve env ast@AST { afunctions } =
    let currEnv = foldl pushFunctionDef env afunctions
    in  (\fd -> ast { afunctions = fd })
          <$> resolveFunctionDefs currEnv afunctions

resolveFunctionDefs :: Env -> [FunctionDef] -> Validate [RError] [FunctionDef]
resolveFunctionDefs _   []   = return []
resolveFunctionDefs env [fd] = toList <$> resolve env fd
resolveFunctionDefs env (h : t) =
  let resolved = resolve env h
      newEnv   = pushFunctionDef env <$> resolved
      next     = case runValidate newEnv of
        Left  err  -> resolveFunctionDefs env t
        Right env' -> resolveFunctionDefs env' t
  in  liftM2 (<>) (toList <$> resolved) next

toList :: a -> [a]
toList a = [a]

pushFunctionDef :: Env -> FunctionDef -> Env
pushFunctionDef env@Env { ftable } fd@FunctionDef { fname } =
  env { ftable = M.insert fname fd ftable }

instance Resolvable FunctionDef where
  resolve env@Env { ftable, vtable } f@FunctionDef { fbody = Body exp, ftypeDef, fparams }
    = let
        nextEnv =
          env { ftable = M.insert (fname f) f ftable, vtable = updatedVTable }
        resolvedExpM = resolve nextEnv exp
      in
        tolerateWith f $ resolvedExpM >>= updateBody
   where
    (expected, actual) = case ftypeDef of
      Just x  -> (length (ttypes x) - 1, length fparams)
      Nothing -> (0, 0)
    updateBody :: Exp -> Validate [RError] FunctionDef
    updateBody exp = if expected == actual
      then pure f { fbody = Body exp, ftype = etype exp }
      else refute [ParameterCountError expected actual]
    typedParams = case ftypeDef of
      Just x  -> zip fparams . init $ ttypes x
      Nothing -> [] -- TODO: add params with * as type ?
    updatedVTable = foldl (\a (n, t) -> M.insert n t a) vtable typedParams

instance Resolvable Exp where
  -------------------------------------
  --            Operation            --
  -------------------------------------
  resolve env e@Operation { eleft, eoperator, eright } = do
    l <- resolve env eleft
    r <- resolve env eright

    -- TODO: Give real context to TypeError !
    let t = case (etype l, etype r) of
          (Just ltype, Just rtype) -> resolveOperation eoperator ltype rtype
          _ -> Nothing

        newE = if t == Nothing then refute [TypeError "" ""] else pure e
    tolerateWith e newE >> pure e { eleft = l, eright = r, etype = t }
    where
      resolveOperation :: Operator -> Type -> Type -> Maybe Type
      resolveOperation TripleEq "Bool" "Bool" = Just "Bool"
      resolveOperation op ltype rtype = if ltype == "Num"
                then Just "Num" -- TODO: This should test for operator as well
                else Nothing
      resolveOperation _ _ _ = Nothing

  -------------------------------------
  --            VarAccess            --
  -------------------------------------
  resolve env@Env { vtable } e@VarAccess { ename } =
    let t = M.lookup ename vtable in return $ e { etype = t }

  -------------------------------------
  --             IntLit              --
  -------------------------------------
  resolve env e@IntLit{}    = return e

  -------------------------------------
  --             StringLit              --
  -------------------------------------
  resolve env e@StringLit{} = return e

  -------------------------------------
  --           FunctionCall          --
  -------------------------------------
  -- TODO: Handle error cases with tests for them
  resolve env@Env { ftable } f@FunctionCall { ename, eargs = ea } =
    let
      -- Retrieve function and derive types
        functionDef   = findFunctionDef ename
        functionTypes = getFunctionTypes <$> functionDef
        paramsTypes   = getParamTypes <$> functionTypes
        returnType    = getReturnType <$> functionTypes

        -- Resolve call args and derive types
        argsResolved  = resolveArgs ea
        argsTypes     = (etype <$>) <$> argsResolved

        -- Merge call types with function definition types And validate
        typeTuples    = liftM2 zip argsTypes paramsTypes
        result        = typeTuples >>= validate >> liftM3 updateFunctionCall
                                                          argsResolved
                                                          returnType
                                                          (pure f)
    in  tolerateWith f result
   where
    getFunctionTypes :: Maybe FunctionDef -> Maybe [Type]
    getFunctionTypes fDef = case fDef >>= ftypeDef of
      (Just Typing { ttypes }) -> Just ttypes
      Nothing                  -> Nothing

    getParamTypes :: Maybe [String] -> [Maybe String]
    getParamTypes (Just params) = Just <$> init params
    getParamTypes Nothing       = []

    getReturnType :: Maybe [String] -> Maybe String
    getReturnType (Just params) = Just $ last params
    getReturnType Nothing       = Nothing

    resolveArgs :: [Exp] -> Validate [RError] [Exp]
    resolveArgs exps = mapM (resolve env) exps

    validate :: [(Maybe String, Maybe String)] -> Validate [RError] Bool
    validate types = if all (uncurry (==)) types
      then pure True
      else refute [FunctionCallError f]

    updateFunctionCall :: [Exp] -> Maybe Type -> Exp -> Exp
    updateFunctionCall args t fc = fc { eargs = args, etype = t }

    findFunctionDef :: String -> Validate [RError] (Maybe FunctionDef)
    findFunctionDef name = case M.lookup name ftable of
      Just x  -> pure $ Just x
      Nothing -> refute [FunctionNotFound name]

tolerateWith :: Semigroup e => a -> Validate e a -> Validate e a
tolerateWith with v = tolerate v >>= \case
  Just a  -> pure a
  Nothing -> pure with
