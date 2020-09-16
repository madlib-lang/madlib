{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
module Resolver
  ( Env(..)
  , TCError(..)
  , ASTTable
  , buildAST
  , resolve
  , resolveASTTable
  )
where

import           Control.Monad.Except
import qualified Data.List                     as L
import qualified Data.Map                      as M
import           Grammar
import           Text.Show.Pretty               ( ppShow )

type ASTTable = M.Map FilePath AST

-- TODO: Move from Either String a to Either MError a

-- TODO: Most likely make it return an Either MError (Map FilePath AST)
-- TODO: Write tests that cover that part of the code
resolveASTTable :: Env -> AST -> ASTTable -> ASTTable
resolveASTTable env ast@AST { aimports } table =
  let paths       = toRoot . ipath <$> aimports
      result      = foldl resolveImports table paths
      nextEnv     = updateEnvFromTable env result

      resolvedAst = case runExcept $ resolve nextEnv ast of
        Right x -> x
        _       -> undefined
  in  case apath resolvedAst of
        Just p  -> M.insert p resolvedAst result
        Nothing -> table -- TODO: Again, use good errors here
 where
  resolveImports :: ASTTable -> FilePath -> ASTTable
  resolveImports table path =
    let ast' = case M.lookup path table of
          Just x  -> x
          Nothing -> undefined -- TODO: handle
        updatedTable = resolveASTTable env ast' table
        nextEnv      = updateEnvFromTable env updatedTable
        resolvedAst  = runExcept $ resolve nextEnv ast'
    in  case resolvedAst of
          Right r -> M.insert path r table
          _       -> undefined

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
toRoot :: FilePath -> FilePath
toRoot = ("fixtures/" ++) <$> (++ ".mad")


data TCError = TypeError Type Type
             | SignatureError Int Int
             deriving(Eq, Show)

data Env =
  Env
    { vtable :: M.Map Name Type
    , ftable :: M.Map Name FunctionDef
    }
    deriving(Eq, Show)

class Resolvable a where
  resolve :: Env -> a -> Except TCError a

instance Resolvable AST where
  resolve env ast@AST { afunctions } =
    let currEnv = foldl pushFunctionDef env afunctions
    in  (\fd -> ast { afunctions = fd })
          <$> resolveFunctionDefs currEnv afunctions

resolveFunctionDefs :: Env -> [FunctionDef] -> Except TCError [FunctionDef]
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
    updateBody :: Exp -> Except TCError FunctionDef
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
    resolveArgs :: [Exp] -> Except TCError [Exp]
    resolveArgs e = mapM (resolve env) e

    argsToTypes :: [Exp] -> Except TCError [Type]
    argsToTypes e = case mapM etype e of
      Just x  -> return x
      Nothing -> throwError $ TypeError "" ""

buildAST :: Path -> String -> Either String AST
buildAST path code = parse code >>= (\a -> return a { apath = Just path })
