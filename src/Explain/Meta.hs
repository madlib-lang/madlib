module Explain.Meta where

import           Explain.Location

-- TODO: Consider that approach
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


getArea :: Meta a -> Area
getArea (Meta _ a _) = a
