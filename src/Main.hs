module Main where

import qualified Data.Map as M
import           Resolver

main :: IO ()
main = interact $ generateOutput . (>>= resolve initialEnv) . buildAST
  where initialEnv = Env M.empty M.empty
