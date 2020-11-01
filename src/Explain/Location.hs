module Explain.Location where

data Loc = Loc Int Int Int deriving(Eq, Show)

data Area = Area Loc Loc deriving(Show, Eq)

data Located a = Located Area a deriving(Eq, Show)

getArea :: Located a -> Area
getArea (Located a _) = a
