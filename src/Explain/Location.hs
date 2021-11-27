module Explain.Location where

import Debug.Trace
import Text.Show.Pretty

data Loc = Loc Int Int Int deriving(Eq, Show, Ord)

getLine :: Loc -> Int
getLine (Loc _ l _) = l

getCol :: Loc -> Int
getCol (Loc _ _ c) = c


data Area = Area Loc Loc deriving(Show, Eq, Ord)

emptyArea :: Area
emptyArea = Area (Loc 0 0 0) (Loc 0 0 0)

getLineFromStart :: Area -> Int
getLineFromStart (Area (Loc _ l _) _) = l

getStartLoc :: Area -> Loc
getStartLoc area = case area of
  Area loc _ -> loc

getEndLoc :: Area -> Loc
getEndLoc area = case area of
  Area _ loc -> loc

isAfter :: Area -> Area -> Bool
(Area (Loc a _ _) _) `isAfter` (Area (Loc a' _ _) _) =
  a > a'

isSameLine :: Area -> Area -> Bool
isSameLine (Area (Loc _ l _) _) (Area (Loc _ l' _) _) =
  l == l'
