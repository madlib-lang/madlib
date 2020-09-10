{-# LANGUAGE NamedFieldPuns #-}
module Main where

import qualified Data.Map as M
import           Resolver

main :: IO ()
main = interact (\x -> generateOutput $ buildAST x >>= resolve initialEnv)
  where initialEnv = Env M.empty M.empty
