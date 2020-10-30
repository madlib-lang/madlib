module Explain.Location where

data Loc = Loc Int Int Int deriving(Eq, Show)

data Located a = Located Loc a deriving(Eq, Show)

getLoc :: Located a -> Loc
getLoc (Located l _) = l
