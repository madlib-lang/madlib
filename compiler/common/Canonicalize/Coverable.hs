{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Canonicalize.Coverable where

import           Data.Hashable
import           GHC.Generics hiding(Constructor)


data Coverable
  = Function { cline :: Int, cname :: String, castpath :: FilePath }
  | Line { cline :: Int, castpath :: FilePath }
  | Branch { cline :: Int, cblocknumber :: Int, cbranchnumber :: Int, castpath :: FilePath }
  deriving (Ord, Eq, Show, Generic, Hashable)
-- BRDA:<line number>,<block number>,<branch number>,<taken>