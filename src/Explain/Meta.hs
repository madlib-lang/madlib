module Explain.Meta where

import           Explain.Location


data Infos a
  = Infos
      { nthArg :: Maybe Int
      , origin :: Maybe a
      }
      deriving(Eq, Show)

emptyInfos :: Infos a
emptyInfos = Infos { nthArg = Nothing, origin = Nothing }

data Meta a
  = Meta (Infos a) Area a
  deriving(Eq, Show)
