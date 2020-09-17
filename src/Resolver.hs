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

import           Control.Monad.Except
import qualified Data.List                     as L
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import           Grammar
import           Text.Show.Pretty               ( ppShow )
import           Control.Monad                  ( liftM2 )
import           Data.Foldable                  ( foldlM )
import           System.FilePath.Posix          ( splitFileName )



type ASTTable = M.Map FilePath AST

data RError = TypeError Type Type
             | SignatureError Int Int
             | PathNotFound
             deriving(Eq, Show)

data Env =
  Env
    { vtable   :: M.Map Name Type
    , ftable   :: M.Map Name FunctionDef
    , rootPath :: Maybe FilePath
    }
    deriving(Eq, Show)

computeRootPath :: FilePath -> FilePath
computeRootPath = fst . splitFileName

-- TODO: Write tests that cover that part of the code

-- TODO: Write case for Nothing for apath
resolveASTTable env ast@AST { apath = Just apath } =
  let rootPath = computeRootPath apath
  in  resolveASTTable' env { rootPath = Just rootPath } ast

resolveASTTable' :: Env -> AST -> ASTTable -> Either RError ASTTable
resolveASTTable' env@Env { rootPath = Just rootPath } ast@AST { aimports } table
  = let paths        = toRoot rootPath . ipath <$> aimports
        updatedTable = foldlM resolveImport table paths
    in  resolveAndUpdateAST env ast updatedTable
 where
  resolveImport :: ASTTable -> FilePath -> Either RError ASTTable
  resolveImport table path =
    let ast'         = fromMaybe undefined (M.lookup path table)
        updatedTable = resolveASTTable' env ast' table
    in  resolveAndUpdateAST env ast' updatedTable

resolveAndUpdateAST
  :: Env -> AST -> Either RError ASTTable -> Either RError ASTTable
resolveAndUpdateAST env ast table =
  let nextEnv     = updateEnvFromTable env <$> table
      resolvedAst = resolveAst nextEnv ast
  in  updateASTTable resolvedAst table

updateASTTable
  :: Either RError AST -> Either RError ASTTable -> Either RError ASTTable
updateASTTable (     Left  x  ) table = Left x
updateASTTable east@(Right ast) table = case apath ast of
  Just p  -> liftM3 M.insert (Right p) east table
  Nothing -> Left PathNotFound

resolveAst :: Resolvable a => Either RError Env -> a -> Either RError a
resolveAst (Right e) ast = runExcept $ resolve e ast
resolveAst (Left  r) _   = Left r

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

-- Mostly needs to be defined, but we need a strategy to figure where
-- things are imported from, so that we can ha
toRoot :: FilePath -> FilePath -> FilePath
toRoot root = (root ++) <$> (++ ".mad")

class Resolvable a where
  resolve :: Env -> a -> Except RError a

instance Resolvable AST where
  resolve env ast@AST { afunctions } =
    let currEnv = foldl pushFunctionDef env afunctions
    in  (\fd -> ast { afunctions = fd })
          <$> resolveFunctionDefs currEnv afunctions

resolveFunctionDefs :: Env -> [FunctionDef] -> Except RError [FunctionDef]
resolveFunctionDefs _   []   = return []
resolveFunctionDefs env [fd] = toList <$> resolve env fd
resolveFunctionDefs env (h : t) =
  let resolved = resolve env h
      newEnv   = pushFunctionDef env <$> resolved
      next     = newEnv >>= flip resolveFunctionDefs t
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
        resolvedExpM >>= updateBody
   where
    (expected, actual) = case ftypeDef of
      Just x  -> (length (ttypes x) - 1, length fparams)
      Nothing -> (0, 0)
    updateBody :: Exp -> Except RError FunctionDef
    updateBody exp = if expected == actual
      then return f { fbody = Body exp, ftype = etype exp }
      else throwError $ SignatureError expected actual
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

    let t = case (etype l, etype r) of
          (Just ltype, Just rtype) -> Just "Num" -- TODO: This should test for operator as well
          (_         , _         ) -> Nothing
    return e { eleft = l, eright = r, etype = t }

  -------------------------------------
  --            VarAccess            --
  -------------------------------------
  resolve env@Env { vtable } e@VarAccess { ename } =
    let t = M.lookup ename vtable in return $ e { etype = t }

  -------------------------------------
  --             IntLit              --
  -------------------------------------
  resolve env e@IntLit{} = return e

  -------------------------------------
  --           FunctionCall          --
  -------------------------------------
  resolve env@Env { ftable } f@FunctionCall { ename, eargs = ea } =
    let fDef  = M.lookup ename ftable
        fInfo = case fDef of
          Just FunctionDef { ftypeDef, ftype } -> Right (ftypeDef, ftype)
          _ -> throwError "No GOOD !"
        fArgsResolved = resolveArgs ea >>= (\a -> return f { eargs = a })
        argTypes      = (eargs <$> fArgsResolved) >>= argsToTypes
        argTuples     = case fInfo of
          Right (Just typing, t) ->
            argTypes >>= (return . zip (init $ ttypes typing))
          _ -> return []
        isValid = (all (uncurry (==)) <$> argTuples)
    in  isValid >> fArgsResolved >>= (\x -> return x { etype = fDef >>= ftype })
   where
    resolveArgs :: [Exp] -> Except RError [Exp]
    resolveArgs e = mapM (resolve env) e

    argsToTypes :: [Exp] -> Except RError [Type]
    argsToTypes e = case mapM etype e of
      Just x  -> return x
      Nothing -> throwError $ TypeError "" ""
