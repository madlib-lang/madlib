module Explain.Format where

import           Error.Error
import           Explain.Reason
import           Explain.Meta
import           Explain.Location
import qualified AST.Source                    as Src
import qualified AST.Canonical                 as Can
import           Infer.Type
import           Data.List                      ( intercalate )
import qualified Data.Map                      as M
import           Text.Show.Pretty               ( ppShow )
import           Control.Monad                  ( replicateM )



getModuleContent :: (FilePath -> IO String) -> Reason -> IO String
getModuleContent rf (Reason _ modulePath _    ) = rf modulePath
getModuleContent rf (SimpleReason modulePath _) = rf modulePath
getModuleContent _  _                           = return ""


format :: (FilePath -> IO String) -> InferError -> IO String
format rf (InferError err reason) = do
  moduleContent <- lines <$> getModuleContent rf reason
  case reason of
    Reason (WrongTypeApplied (Can.Canonical _ abs) (Can.Canonical (Area (Loc a li c) _) e)) _ area
      -> do
        let beginning = case abs of
              -- TODO: Extend to other operators
              Can.App (Can.Canonical _ (Can.Var "+")) _ _ ->
                "Error applying the operator +"
              Can.Var "+" -> "Error applying the operator +"
              _           -> "Error in function call"

        let l = moduleContent !! (li - 1)
        let (Area (Loc _ lineStart colStart) (Loc _ lineEnd colEnd)) = area
        let (UnificationError expected actual) = err

        -- let nthInfo = case nthArg infos of
        --       Just nth -> "The " <> show nth <> nthEnding nth <> " "
        --       Nothing  -> "The "
        -- let fn = case origin infos of
        --       Just origin -> case origin of
        --         Src.Var n -> " of \"" <> n <> "\" "
        --         _         -> " "
        --       Nothing -> " "


        let message =
              "\n"
                -- <> nthInfo
                -- <> "argument"
                -- <> fn
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

    Reason (VariableNotDeclared (Can.Canonical (Area (Loc a li c) _) exp)) _ area
      -> do
        let l           = moduleContent !! (li - 1)

        let (Can.Var n) = exp

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
      let ifElseArea                         = Can.getArea ifElse
      let falsyArea                          = Can.getArea falsy
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
      let ifElseArea                         = Can.getArea ifElse
      let condArea                           = Can.getArea cond
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
      let switchArea                         = Can.getArea switch
      let patternArea                        = Can.getArea pat
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
      let switchArea                     = Can.getArea switch
      let patternArea                    = Can.getArea pat
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
      let importArea                    = Src.getArea imp
      let highlightArea                 = Src.getArea imp
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
      let typingArea                 = Can.getArea typing
      let expArea                    = Can.getArea exp
      let (Area (Loc _ expLine _) _) = expArea
      let (typingStart, typingEnd) = computeLinesToShow typingArea typingArea
      let (expStart, expEnd)         = computeLinesToShow expArea expArea
      let typingContent = slice typingStart typingEnd moduleContent
      let expContent = case exp of
            (Can.Canonical _ (Can.Assignment _ _)) ->
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

    SimpleReason fp area -> do
      let (start, end)              = computeLinesToShow area area
      let expContent                = slice start end moduleContent
      let (Area (Loc x line col) _) = area
      let highlightArea = Area (Loc x line col) (Loc x line (col + 1))

      return
        $  "An error occured in module '"
        <> fp
        <> "' at line "
        <> show line
        <> ":\n\n"
        <> unlines expContent
        <> formatHighlightArea highlightArea
        <> "\n\n"
        <> formatTypeError err
        -- <> "\n"
        -- <> "If I had more information I'd tell you more but right now I don't know"

    _ -> return $ formatTypeError err


-- TODO: Add Env and lookup stuff there like unbound names that are close to give suggestions
formatTypeError :: TypeError -> String
formatTypeError err = case err of
  InfiniteType (TV n _) t -> "Infinite type " <> n <> " -> " <> typeToStr t

  UnboundVariable n ->
    "The variable '" <> n <> "' has not been declared, you might have a typo !"

  SignatureTooGeneral scGiven scInferred ->
    "The signature given is too general\n"
      <> "Type signature given:\n"
      <> "    "
      <> schemeToStr scGiven
      <> "\n\n"
      <> "Type inferred:\n"
      <> "    "
      <> schemeToStr scInferred

  UnificationError t t' ->
    "Type error, you gave :\n"
      <> "    "
      <> typeToStr t
      <> "\n"
      <> "But this type was expected:\n"
      <> "    "
      <> typeToStr t'

  NoInstanceFound cls ts ->
    "I could not find any instance for '"
      <> predToStr (IsIn cls ts)
      <> "'. Verify that you imported the module\nwhere the "
      <> cls
      <> " instance for '"
      <> unwords (typeToStr <$> ts)
      <> "' is defined."
      <> "\n\nNB: remember that instance methods are automatically imported when the module\n"
      <> "is imported, directly, or indirectly."

  AmbiguousType (TV n _, [IsIn cls _]) ->
    "An ambiguity for the type variable '"
      <> n
      <> "' could not be resolved! I am\n"
      <> "looking for an instance of '"
      <> cls
      <> "' but could not resolve it. You\n"
      <> "might want to add a type annotation to make it resolvable."

  AmbiguousType (TV n _, []) ->
    "An ambiguity for the type variable '" <> n <> "' could not be resolved!"

  InterfaceNotExisting cls ->
    "The interface '"
      <> cls
      <> "' is not defined. Make sure you imported the module\n"
      <> "defining it, or a module that imports it."

  KindError (t, k) (t', k') ->
    "The kind of types don't match, '"
      <> typeToStr t
      <> "has kind "
      <> kindToStr k
      <> " and "
      <> typeToStr t'
      <> " has kind "
      <> kindToStr k'
      <> "."

  InstancePredicateError pInstance pWrong pCorrect ->
    "A constraint in the instance declaration '"
      <> predToStr pInstance
      <> " is not correct.\n"
      <> "You gave the constraint '"
      <> predToStr pWrong
      <> "' but a constraint of the form '"
      <> predToStr pCorrect
      <> "'\nwas expected."

  ImportCycle paths ->
    "I found an import cycle:\n\n"
      <> buildCycleOutput (length paths) 0 paths
      <> "\nHint: Import cycles are not allowed and usually show a design issue. Consider splitting things in more\n"
      <> "modules in order to have both modules import a common dependency instead of having them being co-dependent.\n"
      <> "Another solution would be to move things that depend on the other module from the cycle into the other in\n"
      <> "order to collocate things that depend on each other."

   where
    buildCycleOutput :: Int -> Int -> [FilePath] -> String
    buildCycleOutput total current paths =
      let amountOfSpaces = current * 2
          spaces         = concat $ replicate amountOfSpaces " "
          prefix         = spaces <> if current /= 0 then "-> " else ""
          next           = if current < (total - 1)
            then buildCycleOutput total (current + 1) paths
            else ""
      in  prefix <> paths !! current <> "\n" <> next

  _ -> ppShow err


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


letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']


kindToStr :: Kind -> String
kindToStr k = case k of
  Star     -> "*"
  Kfun l r -> kindToStr l <> " -> " <> kindToStr r

schemeToStr :: Scheme -> String
schemeToStr (Forall _ ([] :=> t)) = typeToStr t
schemeToStr (Forall _ (ps :=> t)) = predsToStr ps <> " => " <> typeToStr t

predsToStr :: [Pred] -> String
predsToStr [p] = predToStr p
predsToStr ps  = "(" <> intercalate ", " (predToStr <$> ps) <> ")"


predToStr :: Pred -> String
predToStr (IsIn cls ts) = cls <> " " <> unwords (typeToParenWrappedStr <$> ts)

typeToParenWrappedStr :: Type -> String
typeToParenWrappedStr t = case t of
  TApp _ _ -> "(" <> typeToStr t <> ")"
  _        -> typeToStr t

typeToStr :: Type -> String
typeToStr t = case t of
  TCon (TC a _) -> a
  TVar (TV a _) -> a
  TApp (TApp (TCon (TC "(->)" _)) t2) t2' ->
    typeToStr t2 <> " -> " <> typeToStr t2'
  TApp t1 t2 -> typeToStr t1 <> " " <> typeToStr t2
  TGen x     -> letters !! x
  TRecord fields _ ->
    "{ "
      <> intercalate
           ", "
           ((\(n, t) -> n <> ": " <> typeToStr t) <$> M.toList fields)
      <> "}"
  _ -> ppShow t



slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)
