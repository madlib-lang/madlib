module Explain.Location where

data Loc = Loc Int Int Int deriving(Eq, Show)

data Area = Area Loc Loc deriving(Show, Eq)
