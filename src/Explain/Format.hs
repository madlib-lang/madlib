module Explain.Format where

import           Error.Error
import           Explain.Meta
import           Explain.Location
import qualified AST.Source                    as Src
import qualified AST.Canonical                 as Can
import           Infer.Type
import           Data.List                      ( intercalate
                                                , foldl'
                                                )
import qualified Data.Map                      as M
import           Text.Show.Pretty               ( ppShow )
import           Control.Monad                  ( replicateM )
import           Utils.Tuple

data Color = Green | Red | Grey | WhiteOnRed

colorWhen :: Bool -> Color -> String -> String
colorWhen when c s | when      = color c s
                   | otherwise = s

color :: Color -> String -> String
color c s = case c of
  Green      -> "\x1b[32m" <> s <> "\x1b[0m"
  Red        -> "\x1b[31m" <> s <> "\x1b[0m"
  Grey       -> "\x1b[90m" <> s <> "\x1b[0m"
  WhiteOnRed -> "\x1b[41m" <> s <> "\x1b[0m"

underlineWhen :: Bool -> String -> String
underlineWhen when s | when      = "\x1b[4m" <> s <> "\x1b[0m"
                     | otherwise = s


getModuleContent :: (FilePath -> IO String) -> Context -> IO String
getModuleContent rf (Context modulePath _ _) = rf modulePath
getModuleContent _  _                        = return ""


format :: (FilePath -> IO String) -> Bool -> InferError -> IO String
format rf json (InferError err ctx) = do
  moduleContent <- lines <$> getModuleContent rf ctx
  let formattedError = case ctx of
        Context fp area bt ->
          let (Area (Loc _ line _) _) = area
          in  "in module '"
                <> fp
                <> "' at line "
                <> show line
                <> ":\n\n"
                <> analyzeBacktrace json err bt
                <> showAreaInSource json area area moduleContent
                <> "\n"
                <> formatTypeError json err

        _ -> formatTypeError json err

  return $ colorWhen (not json) WhiteOnRed "Error" <> " " <> formattedError


analyzeBacktrace :: Bool -> TypeError -> Backtrace -> String
analyzeBacktrace json err exps = case exps of
  ((BTExp e1) : BTExp (Can.Canonical _ (Can.If cond _ falsy)) : ex) -> if e1 == cond
    then "The " <> underlineWhen (not json) "condition" <> " of the following if/else expression is not correct:\n"
    else "\nThe error occured in the following if/else expression:\n"

  (BTExp (Can.Canonical _ (Can.TypedExp _ _)) : ex) -> case err of
    UnificationError _ _ ->
      "The " <> underlineWhen (not json) "type declaration" <> " does not match the inferred type:\n"
    _ -> ""

  (BTExp (Can.Canonical _ (Can.Assignment n _)) : BTInstance inst : ex) ->
    "The implementation of the following " <> underlineWhen (not json) "method" <> " is not correct:\n"

  (BTConstructor ctor) : _ -> "Error in the following type " <> underlineWhen (not json) "constructor" <> ":\n"

  _                        -> if length exps > 1 then analyzeBacktrace json err (tail exps) else ""


-- TODO: Add Env and lookup stuff there like unbound names that are close to give suggestions
formatTypeError :: Bool -> TypeError -> String
formatTypeError json err = case err of
  InfiniteType (TV n _) t -> "Infinite type " <> n <> " -> " <> prettyPrintType True t

  UnboundVariable n       -> "The variable '" <> n <> "' has not been declared, you might have a typo !"

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
    "Type error, expected:\n"
      <> "    "
      <> colorWhen (not json) Green (prettyPrintType True t')
      <> "\n"
      <> "But found:\n"
      <> "    "
      <> colorWhen (not json) Red (prettyPrintType True t)

  NoInstanceFound cls ts ->
    "I could not find any instance for '"
      <> predToStr True (IsIn cls ts)
      <> "'. Verify that you imported the module\nwhere the "
      <> cls
      <> " instance for '"
      <> unwords (prettyPrintType True <$> ts)
      <> "' is defined."
      <> "\n\nNB: remember that instance methods are automatically imported when the module\n"
      <> "is imported, directly, or indirectly."

  AmbiguousType (TV n _, IsIn cls _ : _) ->
    "An ambiguity could not be resolved! I am\n"
      <> "looking for an instance of '"
      <> cls
      <> "' but could not resolve it. You\n"
      <> "might want to add a type annotation to make it resolvable."

  AmbiguousType (TV n _, []) -> "An ambiguity for the type variable '" <> n <> "' could not be resolved!"

  InterfaceNotExisting cls ->
    "The interface '"
      <> cls
      <> "' is not defined. Make sure you imported the module\n"
      <> "defining it, or a module that imports it."

  KindError (t, k) (t', k') ->
    "The kind of types don't match, '"
      <> prettyPrintType True t
      <> "has kind "
      <> kindToStr k
      <> " and "
      <> prettyPrintType True t'
      <> " has kind "
      <> kindToStr k'
      <> "."

  InstancePredicateError pInstance pWrong pCorrect ->
    "A constraint in the instance declaration '"
      <> predToStr True pInstance
      <> " is not correct.\n"
      <> "You gave the constraint '"
      <> predToStr True pWrong
      <> "' but a constraint of the form '"
      <> predToStr True pCorrect
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
          next           = if current < (total - 1) then buildCycleOutput total (current + 1) paths else ""
      in  prefix <> paths !! current <> "\n" <> next

  GrammarError _ text -> text

  UnknownType t       -> "Type Error, the type '" <> t <> "' is not found.\n\nHint: Verify that you imported it!"

  NameAlreadyDefined name ->
    "Type Error, the variable '"
      <> name
      <> "' is already used."
      <> "\n\n"
      <> "Hint: Change the name of the variable.\n"
      <> "Also note that the variable might be defined further down. All top level assignments share the scope and using a local name\n"
      <> "that is defined in the global scope of a module is not allowed."

  NameAlreadyExported name ->
    "Export already defined. You are trying to export the name '"
      <> name
      <> "' but it\n"
      <> "appears that you have already exported it."

  NotExported name path ->
    "You are trying to import '"
      <> name
      <> "' from the module located here:\n"
      <> "'"
      <> path
      <> "'\n"
      <> "Unfortunately, that module does not export '"
      <> name
      <> "'!\n\n"
      <> "Hint: Verify that you spelled it correctly or add the export to the module if you can."

  _ -> ppShow err


-- computeLinesToShow : returns the first line and the last line to show
computeLinesToShow :: Area -> Area -> (Int, Int)
computeLinesToShow (Area (Loc _ l _) _) (Area (Loc _ l' _) _) = (l - 1, l' - 1)


formatHighlightArea :: Area -> String
formatHighlightArea (Area (Loc _ _ c) (Loc _ _ c')) =
  concat [ " " | _ <- [1 .. (c - 1)] ] <> concat [ "^" | _ <- [c .. (c' - 1)] ]


showAreaInSource :: Bool -> Area -> Area -> [String] -> String
showAreaInSource json start end code =
  let lines                    = [1 ..]
      (firstLine, lastLine)    = computeLinesToShow start end
      firstLineToShow          = max 0 (firstLine - 2)
      lastLineToShow           = lastLine + 3
      amountCharsForLineNumber = length $ show lastLineToShow
      prettyPrintedLineNumbers =
          (\n ->
              let asStr       = show n
                  spacesToAdd = amountCharsForLineNumber - length asStr
              in  replicate spacesToAdd ' ' <> asStr <> "|"
            )
            <$> lines
      before = (\(lNum, line) -> colorWhen (not json) Grey $ lNum <> line)
        <$> slice firstLineToShow (firstLine - 1) (zip prettyPrintedLineNumbers code)
      expContent = uncurry (<>) <$> slice firstLine lastLine (zip prettyPrintedLineNumbers code)
      after      = (\(lNum, line) -> colorWhen (not json) Grey $ lNum <> line)
        <$> slice (lastLine + 1) lastLineToShow (zip prettyPrintedLineNumbers code)
      (Area (Loc x line col) (Loc _ line' col')) = end
      endCol                = if line == line' then col' else col + 1
      highlightArea         = Area (Loc x line col) (Loc x line endCol)
      spacesBeforeHighlight = " " <> concat (" " <$ show lastLineToShow)
      formattedArea         = spacesBeforeHighlight <> formatHighlightArea highlightArea
  in  unlines $ before ++ expContent ++ [formattedArea] ++ after



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
schemeToStr (Forall _ ([] :=> t)) = prettyPrintType True t
schemeToStr (Forall _ (ps :=> t)) = predsToStr True ps <> " => " <> prettyPrintType True t

predsToStr :: Bool -> [Pred] -> String
predsToStr rewrite [p] = predToStr rewrite p
predsToStr rewrite ps  = "(" <> intercalate ", " (predToStr rewrite <$> ps) <> ")"


predToStr :: Bool -> Pred -> String
predToStr rewrite (IsIn cls ts) = let types = typeToParenWrappedStr rewrite <$> ts in cls <> " " <> unwords types

typeToParenWrappedStr :: Bool -> Type -> String
typeToParenWrappedStr rewrite t = case t of
  TApp _ _ -> "(" <> prettyPrintType rewrite t <> ")"
  _        -> prettyPrintType rewrite t



prettyPrintType :: Bool -> Type -> String
prettyPrintType rewrite = lst . prettyPrintType' rewrite (mempty, mempty)

hkLetters :: [Char]
hkLetters = ['m' ..]


prettyPrintType' :: Bool -> (M.Map String Int, M.Map String Int) -> Type -> (M.Map String Int, M.Map String Int, String)
prettyPrintType' rewrite (vars, hkVars) t = case t of
  TCon (TC n _) _ -> (vars, hkVars, n)

  TVar (TV n k)   -> if not rewrite
    then (vars, hkVars, n)
    else case k of
      Star -> case M.lookup n vars of
        Just x  -> (vars, hkVars, letters !! x)
        Nothing -> let newIndex = M.size vars in (M.insert n newIndex vars, hkVars, letters !! newIndex)

      Kfun _ _ -> case M.lookup n hkVars of
        Just x  -> (vars, hkVars, [hkLetters !! x])
        Nothing -> let newIndex = M.size hkVars in (vars, M.insert n newIndex hkVars, [hkLetters !! newIndex])

  TApp (TApp (TCon (TC "(,)" _) _) tl) tr ->
    let (varsLeft , hkVarsLeft , left ) = prettyPrintType' rewrite (vars, hkVars) tl
        (varsRight, hkVarsRight, right) = prettyPrintType' rewrite (varsLeft, hkVarsLeft) tr
    in  (varsRight, hkVarsRight, "<" <> left <> ", " <> right <> ">")

  TApp (TApp (TApp (TCon (TC "(,,)" _) _) tl) tr) trr ->
    let (varsLeft      , hkVarsLeft      , left      ) = prettyPrintType' rewrite (vars, hkVars) tl
        (varsRight     , hkVarsRight     , right     ) = prettyPrintType' rewrite (varsLeft, hkVarsLeft) tr
        (varsRightRight, hkVarsRightRight, rightRight) = prettyPrintType' rewrite (varsRight, hkVarsRight) trr
    in  (varsRightRight, hkVarsRightRight, "<" <> left <> ", " <> right <> ", " <> rightRight <> ">")

  TApp (TApp (TApp (TApp (TCon (TC "(,,,)" _) _) tl) tr) trr) trrr ->
    let (varsLeft      , hkVarsLeft      , left      ) = prettyPrintType' rewrite (vars, hkVars) tl
        (varsRight     , hkVarsRight     , right     ) = prettyPrintType' rewrite (varsLeft, hkVarsLeft) tr
        (varsRightRight, hkVarsRightRight, rightRight) = prettyPrintType' rewrite (varsRight, hkVarsRight) trr
        (varsRightRightRight, hkVarsRightRightRight, rightRightRight) =
            prettyPrintType' rewrite (varsRightRight, hkVarsRightRight) trrr
    in  ( varsRightRight
        , hkVarsRightRight
        , "<" <> left <> ", " <> right <> ", " <> rightRight <> ", " <> rightRightRight <> ">"
        )

  TApp (TApp (TCon (TC "(->)" _) _) tl) tr ->
    let (varsLeft, hkVarsLeft, left) = case tl of
          TApp (TApp (TCon (TC "(->)" _) _) tl') tr' ->
            let (varsLeft' , hkVarsLeft' , left' ) = prettyPrintType' rewrite (vars, hkVars) tl'
                (varsRight', hkVarsRight', right') = prettyPrintType' rewrite (varsLeft', varsLeft') tr'
                leftParenthesis                    = case tl' of
                  TApp (TApp (TCon (TC "(->)" _) _) _) _ -> True
                  _ -> False
                left'' = if leftParenthesis then "(" <> left' <> ")" else left'
            in  (varsRight', hkVarsRight', "(" <> left'' <> " -> " <> right' <> ")")

          _ -> prettyPrintType' rewrite (vars, hkVars) tl

        (varsRight, hkVarsRight, right) = prettyPrintType' rewrite (varsLeft, hkVarsLeft) tr
    in  (varsRight, hkVarsRight, left <> " -> " <> right)

  TApp tl tr ->
    let (varsLeft , hkVarsLeft , left ) = prettyPrintType' rewrite (vars, hkVars) tl
        (varsRight, hkVarsRight, right) = case tr of
          TApp _ _ ->
            let (varsRight', hkVarsRight', right') = prettyPrintType' rewrite (varsLeft, hkVarsLeft) tr
            in  if not (isTuple tr)
                  then (varsRight', hkVarsRight', "(" <> right' <> ")")
                  else (varsRight', hkVarsRight', right')
          _ -> prettyPrintType' rewrite (varsLeft, hkVarsLeft) tr
    in  (varsRight, hkVarsRight, left <> " " <> right)

  TRecord fields _ ->
    let (finalVars, finalHkVars, compiledFields) =
            foldl'
                (\(vars', hkVars', compiledFields') (fieldName, fieldType) ->
                  let (vars'', hkVars'', compiledField) = prettyPrintType' rewrite (vars', hkVars') fieldType
                  in  (vars'', hkVars'', compiledFields' ++ [(fieldName, compiledField)])
                )
                (vars, hkVars, [])
              $ M.toList fields
        compiledFields' = (\(fieldName, fieldType) -> fieldName <> " :: " <> fieldType) <$> compiledFields
        compiled        = "{ " <> intercalate ", " compiledFields' <> " }"
    in  (finalVars, finalHkVars, compiled)

  TGen n -> (vars, hkVars, "TGen" <> show n)

  _      -> (vars, hkVars, "")


isTuple :: Type -> Bool
isTuple t = case t of
  TApp (TApp (TCon (TC "(,)" _) _) _) _ -> True
  TApp (TApp (TApp (TCon (TC "(,,)" _) _) _) _) _ -> True
  TApp (TApp (TApp (TApp (TCon (TC "(,,,)" _) _) _) _) _) _ -> True
  TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,)" _) _) _) _) _) _) _ -> True
  _ -> False


prettyPrintKind :: Kind -> String
prettyPrintKind k = case k of
  Star       -> "*"
  Kfun k1 k2 -> prettyPrintKind k1 <> " -> " <> prettyPrintKind k2


slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)
