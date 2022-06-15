module Explain.Format where

import           Error.Error
import           Error.Warning
import           Error.Backtrace
import           Error.Context
import           Explain.Location
import qualified AST.Source                    as Src
import qualified AST.Canonical                 as Can
import qualified AST.Solved                    as Slv
import           Infer.Type
import           Data.List                      ( intercalate
                                                , foldl'
                                                , isInfixOf
                                                )
import qualified Data.Map                      as M
import           Text.Show.Pretty               ( ppShow )
import           Control.Monad                  ( replicateM )
import           Utils.Tuple
import Debug.Trace

data Color = Green | Yellow | Red | Grey | WhiteOnRed | WhiteOnYellow

colorWhen :: Bool -> Color -> String -> String
colorWhen when c s | when      = color c s
                   | otherwise = s

color :: Color -> String -> String
color c s = case c of
  Green         -> "\x1b[32m" <> s <> "\x1b[0m"
  Yellow        -> "\x1b[33m" <> s <> "\x1b[0m"
  Red           -> "\x1b[31m" <> s <> "\x1b[0m"
  Grey          -> "\x1b[90m" <> s <> "\x1b[0m"
  WhiteOnRed    -> "\x1b[41m" <> s <> "\x1b[0m"
  WhiteOnYellow -> "\x1b[43m" <> s <> "\x1b[0m"

underlineWhen :: Bool -> String -> String
underlineWhen when s | when      = "\x1b[4m" <> s <> "\x1b[0m"
                     | otherwise = s


getModuleContent :: (FilePath -> IO String) -> Context -> IO String
getModuleContent rf (Context "" _ _) =
  return ""
getModuleContent rf (Context modulePath _ _) =
  rf modulePath
getModuleContent _  _                        =
  return ""


formatWarning :: (FilePath -> IO String) -> Bool -> CompilationWarning -> IO String
formatWarning rf json (CompilationWarning warning ctx) = do
  moduleContent <- lines <$> getModuleContent rf ctx
  let formattedWarning = case ctx of
        Context fp area bt ->
          let (Area (Loc _ line _) _) = area
          in  "in module '"
                <> fp
                <> "' at line "
                <> show line
                <> ":\n"
                <> showAreaInSource json area area moduleContent
                <> "\n"
                <> formatWarningContent json warning

        _ -> formatWarningContent json warning

  return $ colorWhen (not json) WhiteOnYellow "Warning" <> " " <> formattedWarning

formatWarningContent :: Bool -> WarningKind -> String
formatWarningContent _ warning = case warning of
  UnusedImport name path ->
    "You imported '"
      <> name
      <> "' from the module located at '"
      <> path
      <> "'\n"
      <> "but it seems that you never use it."

  MadlibVersionMinorTooLow pkgName minVersion versionUsed ->
    let start = case pkgName of
          Just n  -> "The package '" <> n <> "'"
          Nothing -> "This package"
    in  start
          <> " requires the minimum version '"
          <> minVersion
          <> "' but you currently use madlib\n"
          <> "version '"
          <> versionUsed
          <> "'.\n\n"
          <> "Hint: Update your version of madlib."

  MadlibVersionMajorDiffer pkgName minVersion versionUsed ->
    let start = case pkgName of
          Just n  -> "The package '" <> n <> "'"
          Nothing -> "This package"
    in  start
          <> " requires the minimum version '"
          <> minVersion
          <> "' but you currently use madlib\n"
          <> "version '"
          <> versionUsed
          <> "'. Because major versions differ it means there is a breaking\n"
          <> "change and you may not be able to run the project.\n\n"
          <> "Hint: Update your version of madlib."

format :: (FilePath -> IO String) -> Bool -> CompilationError -> IO String
format rf json (CompilationError err ctx) = do
  moduleContent <- lines <$> getModuleContent rf ctx
  let formattedError = case ctx of
        Context fp area bt ->
          let (Area (Loc _ line _) _) = area
          in  "in module '"
                <> fp
                <> "' at line "
                <> show line
                <> ":\n"
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

  (BTExp (Can.Canonical _ Can.TypedExp{}) : ex) -> case err of
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
  InfiniteType tv t ->
    let (vars, hkVars, printedT) = prettyPrintType' True (mempty, mempty) t
        (_, _, printedN)         = prettyPrintType' True (vars, hkVars) (TVar tv)
    in  "Infinite type " <> printedN <> " -> " <> printedT

  IllegalSkipAccess ->
    "You accessed the skip symbol '_'. This is not permitted as it does not hold any value\n"
    <> "and only serves to indicate that you are not interested in whatever it may contain."

  UnboundVariable n ->
    "The variable '" <> n <> "' has not been declared, you might have a typo."

  UnboundVariableFromNamespace namespace name ->
    "The default import '" <> namespace <> "' does not export the function '" <> name <> "'.\n\n"
    <> "Hint: Verify that it is exported or that you spelled it correctly."

  CapitalizedADTTVar adtname param ->
    "The type parameter '" <> param <> "' in the type declaration '" <> adtname <> "' is capitalized.\n"
      <> "Type parameters can't be capitalized.\n\n"
      <> "Hint: Either remove it if you don't need the type variable, or make its first letter lowercase."

  UnboundType n ->
    "The type '" <> n <> "' has not been declared, you might have a typo!\n\n" <> "Hint: Maybe you forgot to import it?"

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
      <> lst (predToStr True (mempty, mempty) (IsIn cls ts Nothing))
      <> "'. Verify that you imported the module\nwhere the "
      <> cls
      <> " instance for '"
      <> unwords (prettyPrintType True <$> ts)
      <> "' is defined."
      <> "\n\nNB: remember that instance methods are automatically imported when the module\n"
      <> "is imported, directly, or indirectly."

  AmbiguousType (TV n _, IsIn cls _ _ : _) ->
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
      <> "' has kind "
      <> kindToStr k
      <> " and "
      <> prettyPrintType True t'
      <> " has kind "
      <> kindToStr k'
      <> "."

  InstancePredicateError pInstance pWrong pCorrect ->
    "A constraint in the instance declaration '"
      <> lst (predToStr True (mempty, mempty) pInstance)
      <> " is not correct.\n"
      <> "You gave the constraint '"
      <> lst (predToStr True (mempty, mempty) pWrong)
      <> "' but a constraint of the form '"
      <> lst (predToStr True (mempty, mempty) pCorrect)
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
    "Illegal shadowing, the variable '"
      <> name
      <> "' is already defined. Shadowing is not permitted in madlib."
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

  RecursiveVarAccess name ->
    "You are using a variable that is recursively accessing itself and is thus not yet initialized.\n"
      <> "This is not allowed and can only work if there exists a function in between, let me show you\n"
      <> "some examples that should make this clearer:\n"
      <> "parser = J.map(Title, J.field(\"title\", parser)) // this is not allowed because parser is directly refering to itself\n"
      <> "parser = J.map(Title, J.field(\"title\", J.lazy((_) => parser))) // this works because now the recursive accessed is wrapped in a function"

  NotInScope name (Loc _ line _) ->
    "This expression relies on an expression that accesses the variable '"
      <> name
      <> "' at line "
      <> ppShow line
      <> ".\nAll variables need to have been defined by the time they are accessed and this access is thus not allowed.\n\n"
      <> "Hint: Move that call further down in the module so that the name is defined when you access it."

  TypesHaveDifferentOrigin adtName origin1 origin2 ->
    "Types do not match. You try to use a type that seems similar but comes from two different locations.\n"
      <> "The type '"
      <> adtName
      <> "' is used from:\n"
      <> "  - '"
      <> origin1
      <> "'\n"
      <> "  - '"
      <> origin2
      <> "'\n\n"
      <> "Hint: Import it only from one place, or if you meant to use both, make sure to convert from one to the other\n"
      <> "correctly."

  ShouldBeTypedOrAbove name ->
    "You access the name '"
      <> name
      <> "' before it is defined. This is fine, but in that case you must\n"
      <> "give it a type annotation.\n\n"
      <> "Hint: Place that declaration above the place you use it, or give it a type annotation."

  NotCapitalizedADTName name ->
    "The name '" <> name <> "' of this type is not capitalized. This is incorrect and all types in madlib should start with\n"
      <> "an uppercased letter."

  NotCapitalizedAliasName name ->
    "The name '" <> name <> "' of this type alias is not capitalized. This is incorrect and all types in madlib should start with\n"
      <> "an uppercased letter."

  NotCapitalizedConstructorName name ->
    "The name '" <> name <> "' of this type constructor is not capitalized. This is incorrect and all types in madlib should start with\n"
      <> "an uppercased letter."

  TypingHasWrongKind t expectedKind actualKind ->
    "The type annotation '" <> prettyPrintType False t <> "' has a wrong kind.\n"
      <> "expected:\n"
      <> "    "
      <> colorWhen (not json) Green (kindToStr expectedKind)
      <> "\n"
      <> "But found:\n"
      <> "    "
      <> colorWhen (not json) Red (kindToStr actualKind)

  ContextTooWeak preds ->
    "The context of the type annotation is too weak. The type inferred for the implementation\n"
      <> "has the following constraints: " <> intercalate ", " (predClass <$> preds) <>".\n\n"
      <> "Hint: Add the missing interface constraints to the type annotation."

  WrongAliasArgCount aliasName expected actual ->
    "The alias '" <> aliasName <> "' was expected to have " <> show expected <> " argument" <> (if expected > 1 then "s" else "") <> ", but\n"
      <> show actual <> " "<> (if actual > 1 then "were" else "was") <>" given.\n\n"
      <> "Hint: "
      <> (
            if actual > expected then
              "remove " <> show (actual - expected) <> " argument(s)"
            else
              "add the missing '" <> show (expected - actual) <> "' argument(s)"
         )

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


letters :: [Char]
letters = ['a' ..]

hkLetters :: [Char]
hkLetters = ['m' ..]


kindToStr :: Kind -> String
kindToStr k = case k of
  Star     -> "*"
  Kfun l r -> kindToStr l <> " -> " <> kindToStr r


schemeToStr :: Scheme -> String
schemeToStr (Forall _ ([] :=> t)) = prettyPrintType True t
schemeToStr (Forall _ (ps :=> t)) =
  let (vars, hkVars, predStr) = predsToStr True (mempty, mempty) ps
      (_, _, typeStr)         = prettyPrintType' True (vars, hkVars) t
  in
    if length ps > 1 then
      "(" <> predStr <> ")" <> " => " <> typeStr
    else
      predStr <> " => " <> typeStr


predsToStr :: Bool -> (M.Map String Int, M.Map String Int) -> [Pred] -> (M.Map String Int, M.Map String Int, String)
predsToStr rewrite (vars, hkVars) [] = (vars, hkVars, "")
predsToStr rewrite (vars, hkVars) [p] = predToStr rewrite (vars, hkVars) p
predsToStr rewrite (vars, hkVars) (p:ps)  =
  let (vars', hkVars', predStr) = predToStr rewrite (vars, hkVars) p
  in
    if null ps then
     (vars', hkVars', predStr)
    else
      let (vars'', hkVars'', predStr'') = predsToStr rewrite (vars', hkVars') ps
      in  (vars'', hkVars'', predStr <> ", " <> predStr'')


predToStr :: Bool -> (M.Map String Int, M.Map String Int) -> Pred -> (M.Map String Int, M.Map String Int, String)
predToStr rewrite (vars, hkVars) p@(IsIn cls _ _) =
  let (vars', hkVars', predStr) = predToStr' rewrite (vars, hkVars) p
  in  (vars', hkVars', cls <> " " <> predStr)

predToStr' :: Bool -> (M.Map String Int, M.Map String Int) -> Pred -> (M.Map String Int, M.Map String Int, String)
predToStr' rewrite (vars, hkVars) (IsIn cls [] _) = (vars, hkVars, "")
predToStr' rewrite (vars, hkVars) (IsIn cls (t:ts) _) =
  let (vars', hkVars', typeStr) = typeToParenWrappedStr rewrite (vars, hkVars) t
  in
    if null ts then
      (vars', hkVars', typeStr)
    else
      let (vars'', hkVars'', typeStr'') = predToStr' rewrite (vars', hkVars') (IsIn cls ts Nothing)
      in  (vars'', hkVars'', typeStr <> " " <> typeStr'')

typeToParenWrappedStr :: Bool -> (M.Map String Int, M.Map String Int) -> Type -> (M.Map String Int, M.Map String Int, String)
typeToParenWrappedStr rewrite (vars, hkVars) t =
  let (vars', hkVars', typeStr) = prettyPrintType' rewrite (vars, hkVars) t
  in  case t of
    TApp _ _ -> (vars', hkVars', "(" <> typeStr <> ")")
    _        -> (vars', hkVars', typeStr)


prettyPrintQualType :: Bool -> Qual Type -> String
prettyPrintQualType rewrite qt = schemeToStr (Forall [] qt)


prettyPrintType :: Bool -> Type -> String
prettyPrintType rewrite = lst . prettyPrintType' rewrite (mempty, mempty)


prettyPrintType' :: Bool -> (M.Map String Int, M.Map String Int) -> Type -> (M.Map String Int, M.Map String Int, String)
prettyPrintType' rewrite (vars, hkVars) t = case t of
  TCon (TC n _) _ -> (vars, hkVars, n)

  TVar (TV n k)   -> if not rewrite
    then (vars, hkVars, n)
    else case k of
      Star -> case M.lookup n vars of
        Just x  -> (vars, hkVars, [letters !! x])
        Nothing -> let newIndex = M.size vars in (M.insert n newIndex vars, hkVars, [letters !! newIndex])

      Kfun _ _ -> case M.lookup n hkVars of
        Just x  -> (vars, hkVars, [hkLetters !! x])
        Nothing -> let newIndex = M.size hkVars in (vars, M.insert n newIndex hkVars, [hkLetters !! newIndex])

  TApp (TApp (TCon (TC "(,)" _) _) tl) tr ->
    let (varsLeft , hkVarsLeft , left ) = prettyPrintType' rewrite (vars, hkVars) tl
        (varsRight, hkVarsRight, right) = prettyPrintType' rewrite (varsLeft, hkVarsLeft) tr
    in  (varsRight, hkVarsRight, "#[" <> left <> ", " <> right <> "]")

  TApp (TApp (TApp (TCon (TC "(,,)" _) _) tl) tr) trr ->
    let (varsLeft      , hkVarsLeft      , left      ) = prettyPrintType' rewrite (vars, hkVars) tl
        (varsRight     , hkVarsRight     , right     ) = prettyPrintType' rewrite (varsLeft, hkVarsLeft) tr
        (varsRightRight, hkVarsRightRight, rightRight) = prettyPrintType' rewrite (varsRight, hkVarsRight) trr
    in  (varsRightRight, hkVarsRightRight, "#[" <> left <> ", " <> right <> ", " <> rightRight <> "]")

  TApp (TApp (TApp (TApp (TCon (TC "(,,,)" _) _) tl) tr) trr) trrr ->
    let (varsLeft      , hkVarsLeft      , left      ) = prettyPrintType' rewrite (vars, hkVars) tl
        (varsRight     , hkVarsRight     , right     ) = prettyPrintType' rewrite (varsLeft, hkVarsLeft) tr
        (varsRightRight, hkVarsRightRight, rightRight) = prettyPrintType' rewrite (varsRight, hkVarsRight) trr
        (varsRightRightRight, hkVarsRightRightRight, rightRightRight) =
            prettyPrintType' rewrite (varsRightRight, hkVarsRightRight) trrr
    in  ( varsRightRight
        , hkVarsRightRight
        , "#[" <> left <> ", " <> right <> ", " <> rightRight <> ", " <> rightRightRight <> "]"
        )

  TApp (TApp (TCon (TC "(->)" _) _) tl) tr ->
    let (varsLeft, hkVarsLeft, left) = case tl of
          TApp (TApp (TCon (TC "(->)" _) _) tl') tr' ->
            let (varsLeft' , hkVarsLeft' , left' ) = prettyPrintType' rewrite (vars, hkVars) tl'
                (varsRight', hkVarsRight', right') = prettyPrintType' rewrite (varsLeft', hkVarsLeft') tr'
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

  -- TODO: Add spreads display
  TRecord fields base ->
    let (finalVars, finalHkVars, compiledFields) =
            foldl'
                (\(vars', hkVars', compiledFields') (fieldName, fieldType) ->
                  let (vars'', hkVars'', compiledField) = prettyPrintType' rewrite (vars', hkVars') fieldType
                  in  (vars'', hkVars'', compiledFields' ++ [(fieldName, compiledField)])
                )
                (vars, hkVars, [])
              $ M.toList fields
        compiledFields' = (\(fieldName, fieldType) -> fieldName <> " :: " <> fieldType) <$> compiledFields
        formattedBase   = case base of
          Just b  -> "...base, "
          Nothing -> ""
        compiled = "{ " <> formattedBase <> intercalate ", " compiledFields' <> " }"
    in  (finalVars, finalHkVars, compiled)

  TGen n -> (vars, hkVars, "TGen" <> show n)

  _      -> (vars, hkVars, "")


removeNamespace :: String -> String
removeNamespace name =
  if "." `isInfixOf` name then
    reverse . takeWhile (/= '.') . reverse $ name
  else
    name



prettyPrintConstructorTyping :: Slv.Typing -> String
prettyPrintConstructorTyping t@(Slv.Untyped _ typing) = case typing of
  Slv.TRComp _ ts ->
    if not (null ts) then "(" <> prettyPrintConstructorTyping' False t <> ")" else prettyPrintConstructorTyping' False t
  Slv.TRArr _ _ -> "(" <> prettyPrintConstructorTyping' False t <> ")"
  _             -> prettyPrintConstructorTyping' True t

prettyPrintConstructorTyping' :: Bool -> Slv.Typing -> String
prettyPrintConstructorTyping' paren (Slv.Untyped _ typing) = case typing of
  Slv.TRSingle n -> removeNamespace n
  Slv.TRComp n typing' ->
    let space = if not (null typing') then " " else ""
    in  if paren then
      "("
      <> removeNamespace n
      <> space
      <> unwords ((\t -> prettyPrintConstructorTyping' (isTRArrOrTRCompWithArgs t) t) <$> typing')
      <> ")"
    else
      removeNamespace n
      <> space
      <> unwords ((\t -> prettyPrintConstructorTyping' (isTRArrOrTRCompWithArgs t) t) <$> typing')
  Slv.TRArr (Slv.Untyped _ (Slv.TRArr l r)) r' ->
    "("
      <> prettyPrintConstructorTyping' False l
      <> " -> "
      <> prettyPrintConstructorTyping' False r
      <> ") -> "
      <> prettyPrintConstructorTyping' False r'
  Slv.TRArr l r -> if paren
    then "(" <> prettyPrintConstructorTyping' False l <> " -> " <> prettyPrintConstructorTyping' False r <> ")"
    else prettyPrintConstructorTyping' False l <> " -> " <> prettyPrintConstructorTyping' False r
  Slv.TRTuple ts -> "#[" <> intercalate ", " (prettyPrintConstructorTyping' False <$> ts) <> "]"
  Slv.TRRecord ts _ ->
    let mapped  = M.mapWithKey (\k v -> k <> " :: " <> prettyPrintConstructorTyping' False v) (snd <$> ts)
        fields  = M.elems mapped
        fields' = intercalate ", " fields
    in  "{ " <> fields' <> " }"
  _ -> ""

isTRArrOrTRCompWithArgs :: Slv.Typing -> Bool
isTRArrOrTRCompWithArgs (Slv.Untyped _ typing) = case typing of
  Slv.TRArr  _ _  -> True
  Slv.TRComp _ ts -> not (null ts)
  _               -> False


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
