module Utils.EditDistance
  ( editDistance
  , findSimilar
  ) where

import qualified Data.List as List


-- | Levenshtein edit distance between two strings.
-- Uses the standard row-update DP: O(m*n) time, O(n) space.
editDistance :: String -> String -> Int
editDistance [] ys = length ys
editDistance xs [] = length xs
editDistance xs ys = last $ List.foldl' step [0..length ys] xs
  where
    step prev@(d:row) x = List.scanl (update x) (d + 1) (zip3 ys row prev)
    step []           _ = []
    update x acc (y, left, diag)
      | x == y    = diag
      | otherwise = 1 + min acc (min left diag)


-- | Find names from a list that are similar to the query.
-- Returns names whose edit distance is at most (length query `div` 3 + 1),
-- sorted by distance ascending, capped at 3 suggestions.
findSimilar :: String -> [String] -> [String]
findSimilar query candidates =
  let threshold = length query `div` 3 + 1
      unique    = List.nub candidates
      scored    = [(editDistance query c, c) | c <- unique]
      close     = filter (\(d, _) -> d <= threshold) scored
      sorted    = List.sortOn fst close
  in  take 3 (map snd sorted)
