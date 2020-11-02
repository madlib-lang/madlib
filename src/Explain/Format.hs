module Explain.Format where

import           Error.Error
import Explain.Reason
import Explain.Meta
import Explain.Location
import qualified AST.Source as Src
import Infer.Type



getModuleContent :: (FilePath -> IO String) -> Reason -> IO String
getModuleContent rf (Reason _ modulePath _) = rf modulePath


format :: (FilePath -> IO String) -> InferError -> IO String
format rf (InferError err reason) = do
  moduleContent <- getModuleContent rf reason
  case reason of
    Reason (WrongTypeApplied (Meta infos (Area (Loc a li c) _) e)) _ area -> do
      let l = lines moduleContent !! (li - 1)
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
        hint
            = unlines [ "Hint: If the function is polymorphic it is possible that the error comes from"
                    , "the application of other arguments. Otherwise you might want to add a typing"
                    , "to the signature to improve documentation and make error messages more"
                    , "precise !"
                    ]

      return
        $  "Error in function call at line "
        <> show li
        <> ":\n"
        <> l
        <> "\n"
        <> concat [ " " | _ <- [1 .. (colStart - 1)] ]
        <> concat [ "^" | _ <- [colStart .. (colEnd - 1)] ]
        <> message
        <> "\n\n"
        <> hint

    Reason (VariableNotDeclared (Meta _ (Area (Loc a li c) _) exp)) _ area -> do
      let l           = lines moduleContent !! (li - 1)

      let (Src.Var n) = exp

      let
        hint
            = unlines [ "Here are some possible solutions:"
                    , "    * If it is defined in another module, make sure to import it"
                    , "    * If you already import it, make sure that it is exported"
                    ]

      return
        $  "Error at line "
        <> show li
        <> ":\n"
        <> l
        <> "\n"
        <> formatHighlightArea area
        <> "\n"
        <> "The variable \""
        <> n
        <> "\" is not defined.\n\n"
        <> hint


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
