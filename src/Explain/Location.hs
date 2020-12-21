module Explain.Location where

data Loc = Loc Int Int Int deriving(Eq, Show)

data Area = Area Loc Loc deriving(Show, Eq)

emptyArea = Area (Loc 0 0 0) (Loc 0 0 0)
