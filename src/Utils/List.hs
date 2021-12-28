module Utils.List where

import qualified Data.Set              as Set


removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = removeDuplicates' Set.empty where
  removeDuplicates' _ [] = []
  removeDuplicates' a (b : c) = if Set.member b a
    then removeDuplicates' a c
    else b : removeDuplicates' (Set.insert b a) c
