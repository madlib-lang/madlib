{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
module Resolver(Env(..), buildAST, resolve, resolveASTTable) where

import           Control.Monad.Except
import qualified Data.List            as L
import qualified Data.Map             as M
import           Grammar
import           Text.Show.Pretty     (ppShow)


-- TODO: Move from Either String a to Either MadlibError a

resolveASTTable :: Env -> AST -> M.Map FilePath AST -> M.Map FilePath AST
resolveASTTable env ast@AST{ imports } table =
  let
    paths   = toRoot <$> ipath <$> imports
    result  = foldl resolveImports table paths
    nextEnv = updateEnvWithTable env result

    resolvedAst = case resolve nextEnv ast of
      Right x -> x
      _       -> undefined
  in
    M.insert "fixtures/example.mad" resolvedAst result
  where
    resolveImports :: M.Map FilePath AST -> FilePath -> M.Map FilePath AST
    resolveImports table path =
      let
        ast' = case M.lookup path table of
          Just x  -> x
          Nothing -> undefined -- TODO: handle
        updatedTable = resolveASTTable env ast' table
        nextEnv     = updateEnvWithTable env updatedTable
        resolvedAst = resolve nextEnv ast'
      in
        case resolvedAst of
          Right r -> M.insert path r table
          _       -> undefined

updateEnvWithTable :: Env -> M.Map FilePath AST -> Env
updateEnvWithTable env@Env{ftable} =
    updateTable
  . M.fromList
  . (fnsToTuples <$>)
  . concat
  . (functions <$>)
  . M.elems
  where
    updateTable f = env{ftable = M.union f ftable}
    fnsToTuples x = (fname x, x)

-- Mostly needs to be defined, but we need a strategy to figure where
-- things are imported from, so that we can ha
toRoot :: FilePath -> FilePath
toRoot = (++".mad") <$> ("fixtures/"++)

data Env = Env { vtable :: M.Map Name Type
               , ftable :: M.Map Name FunctionDef }
  deriving(Eq, Show)

class Resolvable a where
  resolve :: Env -> a -> Either String a

instance Resolvable AST where
  resolve env ast@AST {functions} =
    let
      currEnv = foldl pushFunctionDef env functions
    in
      (\fd -> ast { functions = fd }) <$> resolveFunctionDefs currEnv functions

resolveFunctionDefs :: Env -> [FunctionDef] -> Either String [FunctionDef]
resolveFunctionDefs _ []      = Right []
resolveFunctionDefs env [fd]  = (resolve env fd) >>= (\a -> return [a])
resolveFunctionDefs env (h:t) =
  let
    resolved = resolve env h
    newEnv   = case resolved of
      Right x -> pushFunctionDef env x
      _       -> env
    next   = resolveFunctionDefs newEnv t
  in
    case (resolved, next) of
      (Right r, Right n) -> return $ r:n
      _                  -> return []

toList :: a -> [a]
toList a = [a]

pushFunctionDef :: Env -> FunctionDef -> Env
pushFunctionDef env@Env { ftable } fd@FunctionDef { fname } =
  env { ftable = M.insert fname fd ftable }

instance Resolvable FunctionDef where
  resolve env@Env{ftable, vtable} f@FunctionDef{fbody = (Body exp), ftypeDef, fparams} =
    let
      nextEnv      = env { ftable = M.insert (fname f) f ftable, vtable = updatedVTable }
      resolvedExpM = resolve nextEnv exp
    in
      resolvedExpM >>= updateBody
    where
      sameCount = case ftypeDef of
        Just x  -> length (ttypes x) -1 == length fparams
        Nothing -> True
      updateBody :: Exp -> Either String FunctionDef
      updateBody exp =
        if sameCount
        then return f { fbody = Body exp, ftype = etype exp }
        else throwError "Error: () - Parameter count and signature don't match !"
      typedParams = case ftypeDef of
        Just x  -> zip fparams . init $ ttypes x
        Nothing -> [] -- TODO: add params with * as type ?
      updatedVTable = foldl (\a (n, t) -> M.insert n t a) vtable typedParams

instance Resolvable Exp where
  -------------------------------------
  --            Operation            --
  -------------------------------------
  resolve env e@Operation{eleft, eoperator, eright} =
    do
      l <- resolve env eleft
      r <- resolve env eright

      let t = case (etype l, etype r) of
            (Just ltype, Just rtype) -> Just "Num" -- TODO: This should test for operator as well
            (_, _)                   -> Nothing
      return e {eleft = l, eright = r, etype = t}

  -------------------------------------
  --            VarAccess            --
  -------------------------------------
  resolve env@Env{vtable} e@VarAccess {ename} =
    let
      t = M.lookup ename vtable
    in
      return $ e {etype = t}

  -------------------------------------
  --             IntLit              --
  -------------------------------------
  resolve env e@IntLit {}    = Right e

  -------------------------------------
  --           FunctionCall          --
  -------------------------------------
  resolve env@Env{ ftable } f@FunctionCall { ename, eargs = ea } =
    let
      fDef  = M.lookup ename ftable
      fInfo = case fDef of
        Just FunctionDef{ftypeDef, ftype} -> Right (ftypeDef, ftype)
        _                                 -> throwError "No GOOD !"
      fArgsResolved = resolveArgs ea >>= (\a -> return f{eargs = a})
      argTypes      = (eargs <$> fArgsResolved) >>= argsToTypes
      argTuples = case fInfo of
        Right (Just typing, t) -> argTypes >>= (return . zip (init $ ttypes typing))
        _                      -> return []
      isValid = argTuples >>= return . (all (uncurry (==)))
    in
      isValid >> fArgsResolved >>= (\x -> return x { etype = fDef >>= ftype })
    where
      resolveArgs :: [Exp] -> Either String [Exp]
      resolveArgs e = mapM (resolve env) e

      argsToTypes :: [Exp] -> Either String [Type]
      argsToTypes e = case mapM etype e of
        Just x  -> Right x
        Nothing -> throwError "OH NO"

buildAST :: String -> Either String AST
buildAST = parse
