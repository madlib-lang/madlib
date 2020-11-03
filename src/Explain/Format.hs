module Explain.Format where

import           Error.Error
import           Explain.Reason
import           Explain.Meta
import           Explain.Location
import qualified AST.Source                    as Src
import           Infer.Type
import           Data.List



getModuleContent :: (FilePath -> IO String) -> Reason -> IO String
getModuleContent rf (Reason _ modulePath _) = rf modulePath


format :: (FilePath -> IO String) -> InferError -> IO String
format rf (InferError err reason) = do
  moduleContent <- lines <$> getModuleContent rf reason
  case reason of
    Reason (WrongTypeApplied (Meta infos (Area (Loc a li c) _) e)) _ area -> do
      let l = moduleContent !! (li - 1)
      let (Area (Loc _ lineStart colStart) (Loc _ lineEnd colEnd)) = area
      let (UnificationError expected actual) = err

      let nthInfo = case nthArg infos of
            Just nth -> "The " <> show nth <> nthEnding nth <> " "
            Nothing  -> "The "
      let fn = case origin infos of
            Just origin -> case origin of
              Src.Var n -> " of \"" <> n <> "\" "
              _         -> " "
            Nothing -> " "


      let message =
            "\n"
              <> nthInfo
              <> "argument"
              <> fn
              <> "has type\n\t"
              <> typeToStr actual
              <> "\nBut it was expected to be\n\t"
              <> typeToStr expected

      let
        hint = unlines
          [ "Hint: if the function is polymorphic it is possible that the error comes from"
          , "the application of other arguments. Otherwise you might want to add a typing to"
          , "to the signature to improve documentation and make error messages more"
          , "precise !"
          ]

      return
        $  "Error in function call at line "
        <> show li
        <> ":\n\n"
        <> l
        <> "\n"
        <> concat [ " " | _ <- [1 .. (colStart - 1)] ]
        <> concat [ "^" | _ <- [colStart .. (colEnd - 1)] ]
        <> message
        <> "\n\n"
        <> hint

    Reason (VariableNotDeclared (Meta _ (Area (Loc a li c) _) exp)) _ area ->
      do
        let l           = moduleContent !! (li - 1)

        let (Src.Var n) = exp

        let
          hint = unlines
            [ "Hint: here are some possible solutions:"
            , "    * If it is defined in another module, make sure to import it"
            , "    * If you already import it, make sure that it is exported"
            ]

        return
          $  "Error at line "
          <> show li
          <> ":\n\n"
          <> l
          <> "\n"
          <> formatHighlightArea area
          <> "\n"
          <> "The variable \""
          <> n
          <> "\" is not defined.\n\n"
          <> hint

    Reason (IfElseBranchTypesDontMatch ifElse falsy) _ _ -> do
      let ifElseArea                         = getArea ifElse
      let falsyArea                          = getArea falsy
      let (Area (Loc _ falsyLine _) _)       = falsyArea
      let (showStart, showEnd) = computeLinesToShow ifElseArea falsyArea
      let linesToShow = slice showStart showEnd moduleContent
      let (UnificationError expected actual) = err

      let message =
            "\n"
              <> "The else branch has type\n\t"
              <> typeToStr actual
              <> "\nBut it was expected to be\n\t"
              <> typeToStr expected

      let
        hint
          = "Hint: the if and else branch of an if else expression should return the same type."

      return
        $  "Error in if else expression at line "
        <> show falsyLine
        <> ":\n\n"
        <> unlines linesToShow
        <> formatHighlightArea falsyArea
        <> message
        <> "\n\n"
        <> hint

    Reason (IfElseCondIsNotBool ifElse cond) _ _ -> do
      let ifElseArea                         = getArea ifElse
      let condArea                           = getArea cond
      let (Area (Loc _ falsyLine _) _)       = condArea
      let (showStart, showEnd) = computeLinesToShow ifElseArea condArea
      let linesToShow = slice showStart showEnd moduleContent
      let (UnificationError expected actual) = err

      let message =
            "\n"
              <> "The condition has type\n\t"
              <> typeToStr actual
              <> "\nBut it was expected to be\n\t"
              <> typeToStr expected

      let hint =
            "Hint: the condition of an if else expression should be a Bool."

      return
        $  "Error in if else expression at line "
        <> show falsyLine
        <> ":\n\n"
        <> unlines linesToShow
        <> formatHighlightArea condArea
        <> message
        <> "\n\n"
        <> hint


-- computeLinesToShow - returns the first line and the last line to show
computeLinesToShow :: Area -> Area -> (Int, Int)
computeLinesToShow (Area (Loc _ l _) _) (Area (Loc _ l' _) _) = (l - 1, l' - 1)


formatHighlightArea :: Area -> String
formatHighlightArea (Area (Loc _ _ c) (Loc _ _ c')) =
  concat [ " " | _ <- [1 .. (c - 1)] ] <> concat [ "^" | _ <- [c .. (c' - 1)] ]



nthEnding :: Int -> String
nthEnding n = case n of
  1 -> "st"
  2 -> "nd"
  3 -> "rd"
  _ -> "th"

typeToStr :: Type -> String
typeToStr t = case t of
  TCon CString -> "String"
  TCon CNum    -> "Num"
  TCon CBool   -> "Bool"


slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)
