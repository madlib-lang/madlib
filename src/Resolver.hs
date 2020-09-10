{-# LANGUAGE NamedFieldPuns #-}
module Resolver(Env(..), buildAST, resolve, generateOutput) where

import qualified Data.Foldable    as F
import qualified Data.Map         as M
import           Debug.Trace
import           Grammar
import           Text.Show.Pretty (ppShow)

data Env = Env { vtable :: M.Map Name Type
               , ftable :: M.Map Name FunctionDef }
  deriving(Eq, Show)

class Resolvable a where
  resolve :: Env -> a -> Either String a

instance Resolvable Program where
  -- right : Either String [FunctionDef]
  -- left : [FunctionDef] -> Program
  -- result : Either String Program
  resolve env p@Program {functions} = Program <$> mapM (resolve env) functions

instance Resolvable FunctionDef where
  resolve env@Env{ftable, vtable} f@FunctionDef{fbody = (Body exp), ftypeDef, fparams} =
    let
      nextEnv      = env { ftable = M.insert (fname f) f ftable, vtable = updatedVTable }
      resolvedExpM = resolve nextEnv exp
    in
      updateBody =<< resolvedExpM
    where
      sameCount = case ftypeDef of
        (Just x) -> length (ttypes x) -1 == length fparams
        _        -> True
      updateBody :: Exp -> Either String FunctionDef
      updateBody exp    = if sameCount
                          then return f { fbody = Body exp }
                          else Left "Error: () - Parameter count and signature don't match !"
      typedParams       = case ftypeDef of
        (Just x) -> zip fparams . init $ ttypes x
        _        -> [] -- TODO: add params with * as type ?
      updatedVTable = F.foldl (\a (n, t) -> M.insert n t a) vtable typedParams

instance Resolvable Exp where
  resolve env e@Operation{eleft, eoperator, eright} = do
    l <- resolve env eleft
    r <- resolve env eright

    let t = case (etype l, etype r) of
         (Just ltype, Just rtype) -> Just "Num" -- This should test for operator as well
         (_, _)                   -> Nothing
    return e {eleft = l, eright = r, etype = t}

  -- Looks in env and tries to find a FunctionDef with this value or a variable
  resolve env@Env{vtable} e@VarAccess {ename} =
    let
      t = M.lookup ename vtable
    in
      return $ e {etype = t}

  resolve env e@IntLit {}    = Right e

buildAST :: String -> Either String Program
buildAST x =
  case parse x of
    Left a  -> Left ("\nERR:\n" ++ a ++ "\n")
    Right b -> trace ("\nAST:\n" ++ show b ++ "\n") Right b

generateOutput :: Either String Program -> String
generateOutput (Left a)  = "ERR: " ++ a
generateOutput (Right b) = ppShow b
