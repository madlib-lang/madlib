module Fuzz.Generator where

import System.Random (mkStdGen, randomR, StdGen)
import Data.List (intercalate)
import Control.Monad (foldM)
import Fuzz.Types


generateProgramModel :: Int -> StressProfile -> Int -> ProgramModel
generateProgramModel seed profile maxSize =
  let (consts, g1) = randomInts 6 (-30, 40) (mkStdGen seed)
      listLen = clamp 3 12 (max 3 (maxSize `div` 2))
      (listVals, g2) = randomInts listLen (-20, 50) g1
      (useClosure, g3) = weightedBool (weights profile "closure") g2
      (usePap3, g4) = weightedBool (weights profile "pap3") g3
      (usePap4, g5) = weightedBool (weights profile "pap4") g4
      (useRecursion, g6) = weightedBool (weights profile "recursion") g4
      (useMap, g7) = weightedBool (weights profile "map") g5
      (useFilter, _) = weightedBool (weights profile "filter") g6
  in ProgramModel
      { pmSeed = seed
      , pmProfile = profile
      , pmConsts = consts
      , pmListVals = listVals
      , pmUseClosure = useClosure
      , pmUsePap3 = usePap3
      , pmUsePap4 = usePap4
      , pmUseRecursion = useRecursion
      , pmUseMap = useMap
      , pmUseFilter = useFilter
      }


renderProgram :: ProgramModel -> String
renderProgram m =
  unlines $
    [ "import IO from \"IO\""
    , "import List from \"List\""
    , ""
    , "assertEqInt = (label, expected, actual) => {"
    , "  if (expected == actual) {"
    , "    IO.putLine(\"OK\")"
    , "  } else {"
    , "    IO.putLine(\"ASSERT_FAIL\")"
    , "  }"
    , "}"
    , ""
    , "assertTrue = (label, ok) => {"
    , "  if (ok) {"
    , "    IO.putLine(\"OK\")"
    , "  } else {"
    , "    IO.putLine(\"ASSERT_FAIL\")"
    , "  }"
    , "}"
    , ""
    , "main = () => {"
    ]
    ++ baseSection
    ++ optionalSections
    ++
    [ "  IO.putLine(\"DONE\")"
    , "}"
    ]
 where
  c0 : c1 : c2 : c3 : c4 : c5 : _ = pmConsts m ++ repeat 1
  listValues = pmListVals m
  listLiteral = "[" <> intercalate ", " (show <$> listValues) <> "]"
  listSum = sum listValues
  nRec = abs c5 `mod` 15 + 1
  mapShift = c3
  mapExpectedSum = sum ((+ mapShift) <$> listValues)

  baseSection =
    [ "  a = " <> show c0
    , "  b = " <> show c1
    , "  c = " <> show c2
    , "  k = " <> show c3
    , "  nums = " <> listLiteral
    , "  assertEqInt(\"list-length-positive\", 1, if (List.length(nums) > 0) { 1 } else { 0 })"
    , "  assertEqInt(\"list-sum-host\", " <> show listSum <> ", List.reduceLeft((acc, x) => acc + x, 0, nums))"
    ]

  closureSection =
    [ "  // FUZZ_BLOCK_BEGIN:closure"
    , "  capture = " <> show c4
    , "  makeAdder = (x) => (y) => x + y + capture"
    , "  addA = makeAdder(a)"
    , "  closureResult = addA(b)"
    , "  assertEqInt(\"closure-capture\", a + b + capture, closureResult)"
    , "  // FUZZ_BLOCK_END:closure"
    ]

  pap3Section =
    [ "  // FUZZ_BLOCK_BEGIN:pap3"
    , "  tri = (x, y, z) => x * y + z"
    , "  tri1 = tri(a)"
    , "  tri2 = tri1(b)"
    , "  triResult = tri2(c)"
    , "  assertEqInt(\"pap3\", a * b + c, triResult)"
    , "  // FUZZ_BLOCK_END:pap3"
    ]

  pap4Section =
    [ "  // FUZZ_BLOCK_BEGIN:pap4"
    , "  quad = (w, x, y, z) => w + x * y - z"
    , "  q1 = quad(a)"
    , "  q2 = q1(b)"
    , "  q3 = q2(c)"
    , "  qResult = q3(k)"
    , "  assertEqInt(\"pap4\", a + b * c - k, qResult)"
    , "  // FUZZ_BLOCK_END:pap4"
    ]

  recursionSection =
    [ "  // FUZZ_BLOCK_BEGIN:recursion"
    , "  sumTo = (n, acc) => {"
    , "    if (n <= 0) {"
    , "      acc"
    , "    } else {"
    , "      sumTo(n - 1, acc + n)"
    , "    }"
    , "  }"
    , "  recN = " <> show nRec
    , "  recResult = sumTo(recN, 0)"
    , "  assertEqInt(\"rec-sum\", recN * (recN + 1) / 2, recResult)"
    , "  // FUZZ_BLOCK_END:recursion"
    ]

  mapSection =
    [ "  // FUZZ_BLOCK_BEGIN:map"
    , "  mapped = map((x) => x + " <> show mapShift <> ", nums)"
    , "  assertEqInt(\"map-length\", List.length(nums), List.length(mapped))"
    , "  assertEqInt(\"map-sum\", " <> show mapExpectedSum <> ", List.reduceLeft((acc, x) => acc + x, 0, mapped))"
    , "  // FUZZ_BLOCK_END:map"
    ]

  filterSection =
    [ "  // FUZZ_BLOCK_BEGIN:filter"
    , "  filtered = List.filter((x) => x % 2 == 0, nums)"
    , "  assertTrue(\"filter-bound\", List.length(filtered) <= List.length(nums))"
    , "  // FUZZ_BLOCK_END:filter"
    ]

  optionalSections =
    concat
      [ if pmUseClosure m then closureSection else []
      , if pmUsePap3 m then pap3Section else []
      , if pmUsePap4 m then pap4Section else []
      , if pmUseRecursion m then recursionSection else []
      , if pmUseMap m then mapSection else []
      , if pmUseFilter m then filterSection else []
      ]


shrinkProgramModel :: ProgramModel -> [ProgramModel]
shrinkProgramModel m =
  featureShrinks ++ constShrinks ++ listShrinks
 where
  featureShrinks =
    [ m { pmUseClosure = False } | pmUseClosure m ]
    ++ [ m { pmUsePap3 = False } | pmUsePap3 m ]
    ++ [ m { pmUsePap4 = False } | pmUsePap4 m ]
    ++ [ m { pmUseRecursion = False } | pmUseRecursion m ]
    ++ [ m { pmUseMap = False } | pmUseMap m ]
    ++ [ m { pmUseFilter = False } | pmUseFilter m ]

  constShrinks =
    concatMap shrinkConstAt [0 .. length (pmConsts m) - 1]

  shrinkConstAt idx =
    let old = pmConsts m !! idx
        candidates = dedupe [0, 1, -1, old `div` 2]
    in
      [ m { pmConsts = replaceAt idx c (pmConsts m) }
      | c <- candidates
      , c /= old
      ]

  listShrinks =
    let vals = pmListVals m
    in
      [ m { pmListVals = take n vals }
      | n <- [length vals - 1, length vals - 2 .. 2]
      , n < length vals
      ]
      ++
      [ m { pmListVals = replaceAt idx 0 vals }
      | idx <- [0 .. length vals - 1]
      , vals !! idx /= 0
      ]


greedyShrink :: (ProgramModel -> IO Bool) -> ProgramModel -> IO (ProgramModel, [String])
greedyShrink predicate model0 = foldM step (model0, []) (zip [0 :: Int ..] (shrinkProgramModel model0))
 where
  step (current, history) (idx, candidate) = do
    failed <- predicate candidate
    if failed then
      return (candidate, history ++ ["accepted-candidate-" <> show idx])
    else
      return (current, history)


weights :: StressProfile -> String -> Int
weights profile feature =
  case (profile, feature) of
    (Balanced, "closure") -> 70
    (Balanced, "pap3") -> 70
    (Balanced, "pap4") -> 50
    (Balanced, "recursion") -> 50
    (Balanced, "map") -> 80
    (Balanced, "filter") -> 70
    (PAPHeavy, "closure") -> 50
    (PAPHeavy, "pap3") -> 95
    (PAPHeavy, "pap4") -> 90
    (PAPHeavy, "recursion") -> 30
    (PAPHeavy, "map") -> 40
    (PAPHeavy, "filter") -> 40
    (AllocationHeavy, "closure") -> 55
    (AllocationHeavy, "pap3") -> 65
    (AllocationHeavy, "pap4") -> 60
    (AllocationHeavy, "recursion") -> 60
    (AllocationHeavy, "map") -> 95
    (AllocationHeavy, "filter") -> 90
    (ClosureHeavy, "closure") -> 95
    (ClosureHeavy, "pap3") -> 70
    (ClosureHeavy, "pap4") -> 65
    (ClosureHeavy, "recursion") -> 65
    (ClosureHeavy, "map") -> 60
    (ClosureHeavy, "filter") -> 50
    _ -> 50


randomInts :: Int -> (Int, Int) -> StdGen -> ([Int], StdGen)
randomInts n range0 = go n []
 where
  go remaining acc gen
    | remaining <= 0 = (reverse acc, gen)
    | otherwise =
      let (v, g1) = randomR range0 gen
      in go (remaining - 1) (v : acc) g1


weightedBool :: Int -> StdGen -> (Bool, StdGen)
weightedBool weight gen =
  let (v, g1) = randomR (1 :: Int, 100) gen
  in (v <= clamp 0 100 weight, g1)


replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx val xs =
  let (left, right) = splitAt idx xs
  in case right of
      [] -> xs
      (_ : rs) -> left ++ (val : rs)


dedupe :: Eq a => [a] -> [a]
dedupe = foldr (\x acc -> if x `elem` acc then acc else x : acc) []


clamp :: Ord a => a -> a -> a -> a
clamp lo hi v = max lo (min hi v)
