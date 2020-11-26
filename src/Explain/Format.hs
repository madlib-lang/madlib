module Explain.Format where

import           Error.Error
import           Explain.Reason
import           Explain.Meta
import           Explain.Location
import qualified AST.Source                    as Src
import           Infer.Type
import           Data.List                      ( intercalate )
import qualified Data.Map                      as M



getModuleContent :: (FilePath -> IO String) -> Reason -> IO String
getModuleContent rf (Reason _ modulePath _) = rf modulePath


format :: (FilePath -> IO String) -> InferError -> IO String
format rf (InferError err reason) = do
  moduleContent <- lines <$> getModuleContent rf reason
  case reason of
    Reason (WrongTypeApplied (Meta _ _ abs) (Meta infos (Area (Loc a li c) _) e)) _ area
      -> do
        let beginning = case abs of
              -- TODO: Extend to other operators
              Src.App (Meta _ _ (Src.Var "+")) _ ->
                "Error applying the operator +"
              Src.Var "+" -> "Error applying the operator +"
              _           -> "Error in function call"

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
          $  beginning
          <> " at line "
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

    Reason (PatternTypeError switch pat) _ _ -> do
      let switchArea                         = getArea switch
      let patternArea                        = getArea pat
      let (Area (Loc _ patternLine _) _)     = patternArea
      let (showStart, showEnd) = computeLinesToShow switchArea patternArea
      let linesToShow = slice showStart showEnd moduleContent
      let (UnificationError expected actual) = err

      let message =
            "\n"
              <> "The pattern has type\n\t"
              <> typeToStr actual
              <> "\nBut it was expected to be\n\t"
              <> typeToStr expected

      let
        hint
          = "Hint: the case patterns of a switch expression should match constructors of the type given to the switch. A common mistake is to mix up type constructor and type. For example, given:\ndata Maybe a = Just a | Nothing\nYou could have the following valid patterns when called with Just(True):\n\t* case Just False: ...\n\t* case Just _: ...\n\t* case Just a: a"

      return
        $  "Error in switch expression at line "
        <> show patternLine
        <> ":\n\n"
        <> unlines linesToShow
        <> formatHighlightArea patternArea
        <> message
        <> "\n\n"
        <> hint

    Reason (PatternConstructorDoesNotExist switch pat) _ _ -> do
      let switchArea                     = getArea switch
      let patternArea                    = getArea pat
      let (Area (Loc _ patternLine _) _) = patternArea
      let (showStart, showEnd) = computeLinesToShow switchArea patternArea
      let linesToShow = slice showStart showEnd moduleContent
      let (UnknownType unknown)          = err

      let message =
            "\n" <> "Constructor used in pattern does not exist\n\t" <> unknown

      let hint = "Hint: make sure that you imported this type."

      return
        $  "Error in switch expression at line "
        <> show patternLine
        <> ":\n\n"
        <> unlines linesToShow
        <> formatHighlightArea patternArea
        <> message
        <> "\n\n"
        <> hint

    Reason (WrongImport imp) _ _ -> do
      let importArea                    = getArea imp
      let highlightArea                 = getArea imp
      let (Area (Loc _ importLine _) _) = highlightArea
      let (showStart, showEnd) = computeLinesToShow importArea highlightArea
      let linesToShow                   = slice showStart showEnd moduleContent

      let message =
            "\n" <> "The module you want to import could not be found\n"

      let
        hint
          = "Hint: make sure that the module exists and that it is in the right folder"

      return
        $  "Import not found at line "
        <> show importLine
        <> ":\n\n"
        <> unlines linesToShow
        <> formatHighlightArea highlightArea
        <> message
        <> "\n\n"
        <> hint

    Reason (TypeAndTypingMismatch exp typing expectedType actualType) _ _ -> do
      let typingArea                 = getArea typing
      let expArea                    = getArea exp
      let (Area (Loc _ expLine _) _) = expArea
      let (typingStart, typingEnd) = computeLinesToShow typingArea typingArea
      let (expStart, expEnd)         = computeLinesToShow expArea expArea
      let typingContent = slice typingStart typingEnd moduleContent
      let expContent = case exp of
            (Meta _ _ (Src.Assignment _ _)) ->
              slice expStart expEnd moduleContent
            _ -> []

      let
        message =
          "\n"
            <> "The type of the expression does not match its type definition.\n\n"
            <> "The definition has type\n\t"
            <> typeToStr expectedType
            <> "\nBut the actual type is\n\t"
            <> typeToStr actualType

      return
        $  "Type error at line "
        <> show expLine
        <> ":\n\n"
        <> unlines typingContent
        <> unlines expContent
        <> formatHighlightArea expArea
        <> "\n"
        <> message


-- computeLinesToShow : returns the first line and the last line to show
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
  TCon CNum -> "Number"
  TCon CBool   -> "Boolean"
  TVar (TV a)  -> a
  TArr (TArr t1 t2) t2' ->
    "("
      <> typeToStr t1
      <> " -> "
      <> typeToStr t2
      <> ")"
      <> " -> "
      <> typeToStr t2'
  TArr t1 t2     -> typeToStr t1 <> " -> " <> typeToStr t2
  TComp _ n vars -> n <> " " <> unwords (typeToStr <$> vars)
  TRecord fields _ ->
    "{ "
      <> intercalate
           ", "
           ((\(n, t) -> n <> ": " <> typeToStr t) <$> M.toList fields)
      <> "}"



slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)
