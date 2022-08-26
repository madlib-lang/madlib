{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Canonicalize.Coverable where

import           Data.Hashable
import           GHC.Generics hiding(Constructor)


data Coverable
  = Function { cline :: Int, cname :: String, castpath :: FilePath }
  | Line { cline :: Int, castpath :: FilePath }
  | Branch { castpath :: FilePath }
  deriving (Ord, Eq, Show, Generic, Hashable)
