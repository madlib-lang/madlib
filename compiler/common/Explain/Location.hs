{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Explain.Location where

import           Data.Hashable
import           GHC.Generics hiding(Constructor)


-- Loc ------------------------------------------------------------------------
-- TODO: remove absolute position as it's never used
data Loc = Loc Int Int Int deriving(Eq, Show, Ord, Generic, Hashable)

getLine :: Loc -> Int
getLine (Loc _ l _) = l

getCol :: Loc -> Int
getCol (Loc _ _ c) = c


-- Area -----------------------------------------------------------------------
data Area = Area Loc Loc deriving(Show, Eq, Ord, Generic, Hashable)


emptyArea :: Area
emptyArea = Area (Loc 0 0 0) (Loc 0 0 0)


getLineFromStart :: Area -> Int
getLineFromStart (Area (Loc _ l _) _) = l


getColumnFromStart :: Area -> Int
getColumnFromStart (Area (Loc _ _ c) _) = c


getStartLoc :: Area -> Loc
getStartLoc area = case area of
  Area loc _ -> loc


increaseStartColumns :: Int -> Area -> Area
increaseStartColumns amount (Area (Loc a l c) (Loc a' l' c')) =
  (Area (Loc a (l + amount) c) (Loc a' l' c'))


getEndLoc :: Area -> Loc
getEndLoc area = case area of
  Area _ loc -> loc


isAfter :: Area -> Area -> Bool
(Area (Loc a _ _) _) `isAfter` (Area (Loc a' _ _) _) =
  a > a'


isSameLine :: Area -> Area -> Bool
isSameLine (Area (Loc _ l _) _) (Area (Loc _ l' _) _) =
  l == l'


mergeAreas :: Area -> Area -> Area
mergeAreas (Area l _) (Area _ r) = Area l r
