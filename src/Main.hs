{-# LANGUAGE NamedFieldPuns #-}
module Main where

import qualified Data.Map         as M
import           Debug.Trace
import           Grammar
import           Text.Show.Pretty (ppShow)

main :: IO ()
main = interact $ generateOutput . buildAST

data Env = Env { vtable :: M.Map Name Type
               , ftable :: M.Map Name FunctionDef }

class Resolvable a where
  resolve :: Env -> a -> Either String a

instance Resolvable Program where
  resolve env p@Program {functions} = Program <$> mapM (resolve env) functions

instance Resolvable FunctionDef where
  resolve env f = undefined

instance Resolvable Exp where
  resolve env e@Operation {} = undefined
  resolve env e@IntLit {}    = Right e

buildAST :: String -> Either String Program
buildAST x =
  case parse x of
    Left a  -> Left ("\nERR:\n" ++ a ++ "\n")
    Right b -> trace ("\nAST:\n" ++ show b ++ "\n") Right b

generateOutput :: Either String Program -> String
generateOutput (Left a)  = "ERR: " ++ a
generateOutput (Right b) = ppShow b
