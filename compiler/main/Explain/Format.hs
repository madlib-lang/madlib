{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
module Explain.Format where

import           Error.Error
import           Error.Warning
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
import qualified Data.Maybe                    as Maybe
import           Infer.Exp (dedupePreds)
import           System.Environment (lookupEnv)
import qualified Error.Diagnose                as Diagnose
import qualified Prettyprinter.Render.Terminal as Terminal
import qualified Data.Text as Text
import qualified Prettyprinter as Pretty



data Color = Green | Yellow | Red | Grey | WhiteOnRed | WhiteOnYellow

colorWhen :: Bool -> Color -> String -> String
colorWhen when c s | when      = color c s
                   | otherwise = s

color :: Color -> String -> String
color c s = case c of
  Green ->
    "\x1b[92m" <> s <> "\x1b[0m"

  Yellow ->
    "\x1b[93m" <> s <> "\x1b[0m"

  Red ->
    "\x1b[91m" <> s <> "\x1b[0m"

  Grey ->
    "\x1b[90m" <> s <> "\x1b[0m"

  WhiteOnRed ->
    "\x1b[41m" <> s <> "\x1b[0m"

  WhiteOnYellow ->
    "\x1b[43m" <> s <> "\x1b[0m"


underlineWhen :: Bool -> String -> String
underlineWhen when s | when      = "\x1b[4m" <> s <> "\x1b[0m"
                     | otherwise = s


getModuleContent :: (FilePath -> IO String) -> Context -> IO String
getModuleContent _ (Context "" _) =
  return ""
getModuleContent rf (Context modulePath _) =
  rf modulePath
getModuleContent _  _                        =
  return ""


formatWarning :: (FilePath -> IO String) -> Bool -> CompilationWarning -> IO String
formatWarning rf json (CompilationWarning warning ctx) = do
  noColor       <- lookupEnv "NO_COLOR"
  moduleContent <- getModuleContent rf ctx
  let isColorEnabled =  not json && not (noColor /= Just "" && Maybe.isJust noColor)
      modulePath     = getCtxPath' ctx
      report         = createWarningDiagnostic isColorEnabled ctx warning
      diagnostic     = Diagnose.addFile Diagnose.def modulePath moduleContent
      diagnostic'    = Diagnose.addReport diagnostic report
      diagnosticDoc  =
        if isColorEnabled then
          Diagnose.defaultStyle $ Diagnose.prettyDiagnostic True 2 diagnostic'
        else
          Pretty.unAnnotate $ Diagnose.prettyDiagnostic True 2 diagnostic'
      layoutOptions = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }
      s = Terminal.renderStrict (Pretty.layoutPretty layoutOptions diagnosticDoc)

  return $ Text.unpack s


createWarningDiagnostic :: Bool -> Context -> WarningKind -> Diagnose.Report String
createWarningDiagnostic _ context warning = case warning of
  UnusedImport name path ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Warn
          Nothing
          "Unused import"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "You imported '" <> name <> "' from the module located at\n"
                <> "'" <> path <> "'\n"
                <> "but it seems that you never use it."
            )
          ]
          [Diagnose.Hint "Remove the import if you don't use it."]

      NoContext ->
        Diagnose.Warn
          Nothing
          "Unused import"
          []
          [Diagnose.Hint "Remove the import if you don't use it."]

  MissingMethods missingMethods ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Warn
          Nothing
          "Missing methods"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "The instance does not implement all methods. The missing\n"
                <> "methods are the following:\n"
                <> intercalate "\n" (map ("  - "<>) missingMethods)
            )
          ]
          [Diagnose.Hint "Implement the missing methods."]

      NoContext ->
        Diagnose.Warn
          Nothing
          "Missing methods"
          []
          [Diagnose.Hint "Implement the missing methods."]

  UnusedParameter name ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Warn
          Nothing
          "Unused parameter"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "You declared a parameter named '" <> name <> "' but it seems that\n"
                <> "you never use it"
            )
          ]
          [ Diagnose.Hint "Remove this parameter if you don't need it"
          , Diagnose.Hint "Use the skip parameter '_' to suppress this warning."
          ]

      NoContext ->
        Diagnose.Warn
          Nothing
          "Unused parameter"
          []
          [ Diagnose.Hint "Remove this parameter if you don't need it"
          , Diagnose.Hint "Use the skip parameter '_' to suppress this warning."
          ]

  UnusedDeclaration name ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Warn
          Nothing
          "Unused variable"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "You declared a variable named '" <> name <> "' but it seems that\n"
                <> "you never use it"
            )
          ]
          [Diagnose.Hint "Remove it if you don't need it."]

      NoContext ->
        Diagnose.Warn
          Nothing
          "Unused variable"
          []
          [Diagnose.Hint "Remove it if you don't need it."]

  UnusedTopLevelDeclaration name ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Warn
          Nothing
          "Unused top level binding"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "You declared a top level binding named '" <> name <> "' but it seems that\n"
                <> "you never use it"
            )
          ]
          [Diagnose.Hint "Remove it if you don't need it."]

      NoContext ->
        Diagnose.Warn
          Nothing
          "Unused top level binding"
          []
          [Diagnose.Hint "Remove it if you don't need it."]

  UnusedConstructor name ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Warn
          Nothing
          "Unused constructor"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "You declared a constructor named '" <> name <> "' but it seems that\n"
                <> "you never use it"
            )
          ]
          [Diagnose.Hint "Remove it if you don't need it."]

      NoContext ->
        Diagnose.Warn
          Nothing
          "Unused constructor"
          []
          [Diagnose.Hint "Remove it if you don't need it."]

  UnusedType name ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Warn
          Nothing
          "Unused type"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "You declared a type named '" <> name <> "' but it seems that\n"
                <> "you never use it"
            )
          ]
          [Diagnose.Hint "Remove it if you don't need it."]

      NoContext ->
        Diagnose.Warn
          Nothing
          "Unused type"
          []
          [Diagnose.Hint "Remove it if you don't need it."]

  MadlibVersionMinorTooLow pkgName minVersion versionUsed ->
    let start = case pkgName of
          Just n  -> "The package '" <> n <> "'"
          Nothing -> "This package"
    in case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Warn
          Nothing
          "Minor Madlib version is too low"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                start
                <> " requires the minimum version '"
                <> minVersion
                <> "' but you currently use madlib\n"
                <> "version '"
                <> versionUsed
                <> "'"
            )
          ]
          [Diagnose.Hint "Update your version of madlib."]

      NoContext ->
        Diagnose.Warn
          Nothing
          "Minor Madlib version is too low"
          []
          [Diagnose.Hint "Update your version of madlib."]

  MadlibVersionMajorDiffer pkgName minVersion versionUsed ->
    let start = case pkgName of
          Just n  -> "The package '" <> n <> "'"
          Nothing -> "This package"
    in  case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Warn
          Nothing
          "Minor Madlib version is too low"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                start
                <> " requires the minimum version '"
                <> minVersion
                <> "' but you currently use madlib\n"
                <> "version '"
                <> versionUsed
                <> "'. Because major versions differ it means there is a breaking\n"
                <> "change and you may not be able to run the project"
            )
          ]
          [Diagnose.Hint "Update your version of madlib."]

      NoContext ->
        Diagnose.Warn
          Nothing
          "Minor Madlib version is too low"
          []
          [Diagnose.Hint "Update your version of madlib."]


format :: (FilePath -> IO String) -> Bool -> CompilationError -> IO String
format rf json (CompilationError err ctx) = do
  noColor       <- lookupEnv "NO_COLOR"
  moduleContent <- getModuleContent rf ctx
  let isColorEnabled =  not json && not (noColor /= Just "" && Maybe.isJust noColor)
      modulePath     = getCtxPath' ctx
      report         = createErrorDiagnostic isColorEnabled ctx err
      diagnostic     = Diagnose.addFile Diagnose.def modulePath moduleContent
      diagnostic'    = Diagnose.addReport diagnostic report
      diagnosticDoc  =
        if isColorEnabled then
          Diagnose.defaultStyle $ Diagnose.prettyDiagnostic True 2 diagnostic'
        else
          Pretty.unAnnotate $ Diagnose.prettyDiagnostic True 2 diagnostic'
      layoutOptions = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }
      s = Terminal.renderStrict (Pretty.layoutPretty layoutOptions diagnosticDoc)

  return $ Text.unpack s

createErrorDiagnostic :: Bool -> Context -> TypeError -> Diagnose.Report String
createErrorDiagnostic color context typeError = case typeError of
  UnificationError t t' ->
    let (_, _, pretty, pretty') = prettyPrintTypesWithDiff color (mempty, mempty) (mempty, mempty) t t'
        expectedStr = if color then "\x1b[0mexpected:\n  " else "expected:\n  "
        foundStr = if color then "\n\x1b[0mbut found:\n  " else "\nbut found:\n  "
    in  case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Type error"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $ expectedStr <> pretty' <> foundStr <> pretty
            )
          ]
          []

      NoContext ->
        Diagnose.Err
          Nothing
          ("Type error\n\n" <> expectedStr <> pretty' <> foundStr <> pretty)
          []
          []

  TypingHasWrongKind t expectedKind actualKind ->
    let expectedStr = if color then "\x1b[0mexpected:\n  " else "expected:\n  "
        foundStr = if color then "\n\x1b[0mbut found:\n  " else "\nbut found:\n  "
    in case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Typing has wrong kind"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "The type annotation '" <> prettyPrintType False t <> "' has a wrong kind.\n"
                <> expectedStr
                <> colorWhen color Green (kindToStr expectedKind)
                <> foundStr
                <> colorWhen color Red (kindToStr actualKind)
            )
          ]
          []

      NoContext ->
        Diagnose.Err
          Nothing
          "Typing has wrong kind"
          []
          []

  NoMain ->
    Diagnose.Err
      Nothing
      "You forgot to define a 'main' function in your main module."
      []
      []

  NotADefinition ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Not a definition"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This "It is not a definition and is not allowed"
            )
          ]
          [ Diagnose.Note "Top level expressions are not allowed in Madlib."
          , Diagnose.Hint "You may want to assign it to a top level variable."
          ]

      NoContext ->
        Diagnose.Err
          Nothing
          "Not a definition"
          []
          [ Diagnose.Note "Top level expressions are not allowed in Madlib."
          , Diagnose.Hint "You may want to assign it to a top level variable."
          ]

  InfiniteType tv t ->
    let (vars, hkVars, printedT) = prettyPrintType' True (mempty, mempty) t
        (_, _, printedN)         = prettyPrintType' True (vars, hkVars) (TVar tv)
    in  case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Infinite type"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $ "Infinite type " <> printedN <> " -> " <> printedT
            )
          ]
          []

      NoContext ->
        Diagnose.Err
          Nothing
          ("Infinite type\n\n" <> "Infinite type " <> printedN <> " -> " <> printedT)
          []
          []

  IllegalSkipAccess ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Illegal skip access"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                 "You accessed the skip symbol '_'. This is not permitted as\n"
              <> "it does not hold any value and only serves to indicate that\n"
              <> "you are not interested in whatever it may contain."
            )
          ]
          [Diagnose.Hint "Give it a name if you intend to use it"]

      NoContext ->
        Diagnose.Err
          Nothing
          "Illegal skip access"
          []
          [Diagnose.Hint "Give it a name if you intend to use it"]

  UnboundVariable n ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Unbound variable"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $ "The variable '" <> n <> "' has not been declared"
            )
          ]
          [Diagnose.Hint "Verify that you don't have a typo"]

      NoContext ->
        Diagnose.Err
          Nothing
          "Unbound variable"
          []
          [Diagnose.Hint "Verify that you don't have a typo"]

  UnboundVariableFromNamespace namespace name ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Name not exported"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $ "Function '" <> name <> "' not found in\ndefault import '" <> namespace <> "'"
            )
          ]
          [Diagnose.Hint "Verify that it is exported or that you spelled it correctly."]

      NoContext ->
        Diagnose.Err
          Nothing
          "Name not exported"
          []
          [Diagnose.Hint "Verify that it is exported or that you spelled it correctly."]

  CapitalizedADTTVar adtname param ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Capitalized ADT variable"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                 "The type parameter '" <> param <> "' in the type declaration\n"
              <> "'" <> adtname <> "' is capitalized. Type parameters can't be capitalized."
            )
          ]
          [Diagnose.Hint "Either remove it if you don't need the type variable, or\nmake its first letter lowercase."]

      NoContext ->
        Diagnose.Err
          Nothing
          "Capitalized ADT variable"
          []
          [Diagnose.Hint "Either remove it if you don't need the type variable, or\nmake its first letter lowercase."]

  UnboundType n ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Unbound Type"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $ "The type '" <> n <> "' has not been declared"
            )
          ]
          [Diagnose.Hint "Maybe you forgot to import it?", Diagnose.Hint "Maybe you have a typo?"]

      NoContext ->
        Diagnose.Err
          Nothing
          "Unbound Type"
          []
          [Diagnose.Hint "Maybe you forgot to import it?", Diagnose.Hint "Maybe you have a typo?"]

  SignatureTooGeneral scGiven scInferred ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Signature too general"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $ "Type signature given:\n  " <> schemeToStr scGiven <> "\nType inferred:\n  " <> schemeToStr scInferred
            )
          ]
          []

      NoContext ->
        Diagnose.Err
          Nothing
          "Signature too general"
          []
          []

  NoInstanceFound cls ts ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Instance not found"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
              "No instance for '" <> lst (predToStr True (mempty, mempty) (IsIn cls ts Nothing)) <> "' was found.\n"
              <> ""
            )
          ]
          [ Diagnose.Hint $ "Verify that you imported the module where the " <> cls <> "\ninstance for '" <> unwords (prettyPrintType True <$> ts) <> "' is defined"
          , Diagnose.Note "Remember that instance methods are automatically imported when the module\nis imported, directly, or indirectly."
          ]

      NoContext ->
        Diagnose.Err
          Nothing
          "Instance not found"
          []
          [ Diagnose.Hint $ "Verify that you imported the module where the " <> cls <> "\ninstance for '" <> unwords (prettyPrintType True <$> ts) <> "' is defined"
          , Diagnose.Note "Remember that instance methods are automatically imported when the module\nis imported, directly, or indirectly."
          ]

  AmbiguousType (TV _ _, IsIn cls _ maybeArea : _) ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Ambiguous type"
          (
            ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $ "An instance of '" <> cls <> "' could not be found"
            ) : case maybeArea of
                  Just (Area (Loc _ startL' startC') (Loc _ endL' endC')) ->
                    [ ( Diagnose.Position (startL', startC') (endL', endC') modulePath
                      , Diagnose.Where "The constraint originates from here"
                      )
                    ]

                  Nothing ->
                    []
          )
          [Diagnose.Hint "You can add a type annotation to make it resolvable."]

      NoContext ->
        Diagnose.Err
          Nothing
          "Ambiguous type"
          []
          [Diagnose.Hint "You can add a type annotation to make it resolvable."]

  AmbiguousType (TV n _, []) ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Ambiguous type"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $ "An ambiguity for the type variable '" <> n <> "' could not be resolved"
            )
          ]
          [Diagnose.Hint "You can add a type annotation to make it resolvable."]

      NoContext ->
        Diagnose.Err
          Nothing
          "Ambiguous type"
          []
          [Diagnose.Hint "You can add a type annotation to make it resolvable."]

  InterfaceNotExisting cls ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Interface not found"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $ "The interface '" <> cls <> "' is not defined.\n"
            )
          ]
          [Diagnose.Hint "Make sure you imported the module defining it,\nor a module that imports it."]

      NoContext ->
        Diagnose.Err
          Nothing
          "Interface not found"
          []
          [Diagnose.Hint "Make sure you imported the module defining it,\nor a module that imports it."]

  KindError (t, k) (t', k') ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Kind error"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "The kind of types don't match, '"
                <> prettyPrintType True t
                <> "' has kind "
                <> kindToStr k
                <> " and "
                <> prettyPrintType True t'
                <> " has kind "
                <> kindToStr k'
                <> "."
            )
          ]
          []

      NoContext ->
        Diagnose.Err
          Nothing
          "Kind error"
          []
          []

  InstancePredicateError pInstance pWrong pCorrect ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Instance predicate error"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "A constraint in the instance declaration '"
                <> lst (predToStr True (mempty, mempty) pInstance)
                <> " is not correct.\n"
                <> "You gave the constraint '"
                <> lst (predToStr True (mempty, mempty) pWrong)
                <> "' but a constraint of the form '"
                <> lst (predToStr True (mempty, mempty) pCorrect)
                <> "'\nwas expected."
            )
          ]
          []

      NoContext ->
        Diagnose.Err
          Nothing
          "Instance predicate error"
          []
          []

  ImportCycle paths ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Import cycle"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "I found an import cycle:\n\n"
                <> buildCycleOutput (length paths) 0 paths
            )
          ]
          [Diagnose.Hint $
            "Import cycles are not allowed and usually show a design issue.\n"
            <> "Consider splitting things in more modules in order to have\n"
            <> "both modules import a common dependency instead of having\n"
            <> "them being co-dependent. Another solution would be to move\n"
            <> "things that depend on the other module from the cycle into\n"
            <> "the other in order to collocate things that depend on each\nother."
          ]

      NoContext ->
        Diagnose.Err
          Nothing
          "Import cycle"
          []
          [Diagnose.Hint $
            "Import cycles are not allowed and usually show a design issue.\n"
            <> "Consider splitting things in more modules in order to have\n"
            <> "both modules import a common dependency instead of having\n"
            <> "them being co-dependent. Another solution would be to move\n"
            <> "things that depend on the other module from the cycle into\n"
            <> "the other in order to collocate things that depend on each\nother."
          ]
   where
    buildCycleOutput :: Int -> Int -> [FilePath] -> String
    buildCycleOutput total current paths =
      let amountOfSpaces = current * 2
          spaces         = concat $ replicate amountOfSpaces " "
          prefix         = spaces <> if current /= 0 then "-> " else ""
          next           = if current < (total - 1) then buildCycleOutput total (current + 1) paths else ""
      in  prefix <> paths !! current <> "\n" <> next

  GrammarError _ _ ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Grammar error"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This "Unexpected character"
            )
          ]
          []

      NoContext ->
        Diagnose.Err
          Nothing
          "Grammar error"
          []
          []

  UnknownType t ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Grammar error"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $ "The type '" <> t <> "' was not found"
            )
          ]
          [Diagnose.Hint "Verify that you imported it"]

      NoContext ->
        Diagnose.Err
          Nothing
          "Grammar error"
          []
          [Diagnose.Hint "Verify that you imported it"]

  NameAlreadyDefined name ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Illegal shadowing"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $ "The variable '" <> name <> "' is already defined"
            )
          ]
          [ Diagnose.Hint "Change the name of the variable"
          , Diagnose.Note $
              "The variable might be defined further down. All top level\n"
              <> "assignments share the scope and using a local name that is\n"
              <> "defined in the global scope of a module is not allowed."
          ]

      NoContext ->
        Diagnose.Err
          Nothing
          "Illegal shadowing"
          []
          [ Diagnose.Hint "Change the name of the variable"
          , Diagnose.Note $
              "The variable might be defined further down. All top level\n"
              <> "assignments share the scope and using a local name that is\n"
              <> "defined in the global scope of a module is not allowed."
          ]

  NameAlreadyExported name ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Already exported"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "Export already defined. You are trying to export the\n"
                <> "name '" <> name <> "' but it\n"
                <> "appears that you have already exported it."
            )
          ]
          []

      NoContext ->
        Diagnose.Err
          Nothing
          "Already exported"
          []
          []

  NotExported name path ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Not exported"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "You are trying to import '" <> name <> "' from\n"
                <> "the module located here:\n"
                <> "'" <> path <> "'\n"
                <> "Unfortunately, that module does not export '" <> name <> "'"
            )
          ]
          [Diagnose.Hint "Verify that you spelled it correctly or add the export to the module if you can."]

      NoContext ->
        Diagnose.Err
          Nothing
          "Not exported"
          []
          [Diagnose.Hint "Verify that you spelled it correctly or add the export to the module if you can."]

  RecursiveVarAccess _ ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Recursive variable access"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "You are using a variable that is recursively accessing itself\n"
                <> "and is thus not yet initialized."
            )
          ]
          [ Diagnose.Note $
              "This is not allowed and can only work if there exists a\n"
              <> "function in between, let me show you some examples that should\n"
              <> "make this clearer:\n"
              <> "// this is not allowed because parser is directly refering to itself\n"
              <> "parser = J.map(Title, J.field(\"title\", parser))\n"
              <> "// this works because now the recursive accessed is wrapped in a function\n"
              <> "parser = J.map(Title, J.field(\"title\", J.lazy((_) => parser)))"
          ]

      NoContext ->
        Diagnose.Err
          Nothing
          "Recursive variable access"
          []
          [ Diagnose.Note $
              "This is not allowed and can only work if there exists a\n"
              <> "function in between, let me show you some examples that should\n"
              <> "make this clearer:\n"
              <> "// this is not allowed because parser is directly refering to itself\n"
              <> "parser = J.map(Title, J.field(\"title\", parser))\n"
              <> "// this works because now the recursive accessed is wrapped in a function\n"
              <> "parser = J.map(Title, J.field(\"title\", J.lazy((_) => parser)))"
          ]

  NotInScope name (Loc _ line _) ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Not in scope"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "This expression relies on an expression that accesses the\n"
                <> "variable '" <> name <> "' at line " <> ppShow line
            )
          ]
          [ Diagnose.Note $
              "All variables need to have been defined by the time they are\n"
              <> "accessed and this access is thus not allowed."
          , Diagnose.Hint $
              "Move that call further down in the module so that the name\n"
              <> "is defined when you access it."
          ]

      NoContext ->
        Diagnose.Err
          Nothing
          "Not in scope"
          []
          [ Diagnose.Note $
              "All variables need to have been defined by the time they are\n"
              <> "accessed and this access is thus not allowed."
          , Diagnose.Hint $
              "Move that call further down in the module so that the name\n"
              <> "is defined when you access it."
          ]

  TypesHaveDifferentOrigin adtName origin1 origin2 ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Types have different origins"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "Types do not match. You try to use a type that seems similar\n"
                <> "but comes from two different locations. The type '" <> adtName <> "'\n"
                <> "is used from:\n"
                <> "  - '" <> origin1 <> "'\n"
                <> "  - '" <> origin2 <> "'"
            )
          ]
          [ Diagnose.Hint $
              "Import it only from one place, or if you meant to use both,\n"
              <> "make sure to convert from one to the other correctly."
          ]

      NoContext ->
        Diagnose.Err
          Nothing
          "Types have different origins"
          []
          [ Diagnose.Hint $
              "Import it only from one place, or if you meant to use both,\n"
              <> "make sure to convert from one to the other correctly."
          ]

  ShouldBeTypedOrAbove name ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Must be typed or above"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "You access the name '" <> name <> "' before it\n"
                <> "is defined"
            )
          ]
          [ Diagnose.Note $
              "This is fine, but in that case you must give it a type\n"
              <> "annotation."
          , Diagnose.Hint $
              "Place that declaration above the place you use it, or give\n"
              <> "it a type annotation."
          ]

      NoContext ->
        Diagnose.Err
          Nothing
          "Must be typed or above"
          []
          [ Diagnose.Note $
              "This is fine, but in that case you must give it a type\n"
              <> "annotation."
          , Diagnose.Hint $
              "Place that declaration above the place you use it, or give\n"
              <> "it a type annotation."
          ]

  NotCapitalizedADTName name ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "ADT name not capitalized"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "The name '" <> name <> "' of this type is not capitalized"
            )
          ]
          [ Diagnose.Note $
              "This is incorrect and all types in madlib should start with\n"
              <> "an uppercased letter."
          ]

      NoContext ->
        Diagnose.Err
          Nothing
          "ADT name not capitalized"
          []
          [ Diagnose.Note $
              "This is incorrect and all types in madlib should start with\n"
              <> "an uppercased letter."
          ]

  NotCapitalizedAliasName name ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Alias name not capitalized"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "The name '" <> name <> "' of this type alias is not capitalized"
            )
          ]
          [ Diagnose.Note $
              "This is incorrect and all types in madlib should start with\n"
              <> "an uppercased letter."
          ]

      NoContext ->
        Diagnose.Err
          Nothing
          "Alias name not capitalized"
          []
          [ Diagnose.Note $
              "This is incorrect and all types in madlib should start with\n"
              <> "an uppercased letter."
          ]

  NotCapitalizedConstructorName name ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Constructor name not capitalized"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "The name '" <> name <> "' of this type constructor is not capitalized"
            )
          ]
          [ Diagnose.Note $
              "This is incorrect and all types in madlib should start with\n"
              <> "an uppercased letter."
          ]

      NoContext ->
        Diagnose.Err
          Nothing
          "Constructor name not capitalized"
          []
          [ Diagnose.Note $
              "This is incorrect and all types in madlib should start with\n"
              <> "an uppercased letter."
          ]

  ContextTooWeak preds ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Context too weak"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "The context of the type annotation is too weak. The type\n"
                <> "inferred for the implementation has the following\n"
                <> "constraints: " <> intercalate ", " (predClass <$> preds)
            )
          ]
          [Diagnose.Hint  "Add the missing interface constraints to the type annotation."]

      NoContext ->
        Diagnose.Err
          Nothing
          "Context too weak"
          []
          [Diagnose.Hint  "Add the missing interface constraints to the type annotation."]

  WrongAliasArgCount aliasName expected actual ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Wrong alias argument count"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "The alias '" <> aliasName <> "' was expected to have " <> show expected <> " argument" <> (if expected > 1 then "s" else "") <> ",\nbut "
                <> show actual <> " "<> (if actual > 1 then "were" else "was") <>" given"
            )
          ]
          [Diagnose.Hint $
            if actual > expected then
              "Remove " <> show (actual - expected) <> " argument(s)"
            else
              "Add the missing '" <> show (expected - actual) <> "' argument(s)"
          ]

      NoContext ->
        Diagnose.Err
          Nothing
          "Wrong alias argument count"
          []
          [Diagnose.Hint $
            if actual > expected then
              "Remove " <> show (actual - expected) <> " argument(s)"
            else
              "Add the missing '" <> show (expected - actual) <> "' argument(s)"
          ]

  ImportNotFound importName ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Import not found"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "You tried to import the module '" <> importName <> "',\n"
                <> "but it could not be found"
            )
          ]
          [Diagnose.Hint "Verify that you don't have a typo."]

      NoContext ->
        Diagnose.Err
          Nothing
          "Import not found"
          []
          [Diagnose.Hint "Verify that you don't have a typo."]


-- computeLinesToShow : returns the first line and the last line to show
computeLinesToShow :: Area -> Area -> (Int, Int)
computeLinesToShow (Area (Loc _ l _) _) (Area (Loc _ l' _) _) = (l - 1, l' - 1)


formatHighlightArea :: Area -> String
formatHighlightArea (Area (Loc _ _ c) (Loc _ _ c')) =
  concat [ " " | _ <- [1 .. (c - 1)] ] <> concat [ "^" | _ <- [c .. (c' - 1)] ]


showAreaInSource :: Bool -> Area -> Area -> [String] -> String
showAreaInSource = showAreaInSource' 2 3

showAreaInSource' :: Int -> Int -> Bool -> Area -> Area -> [String] -> String
showAreaInSource' before after json start end code =
  let lines                    = [1 ..]
      (firstLine, lastLine)    = computeLinesToShow start end
      firstLineToShow          = max 0 (firstLine - before)
      lastLineToShow           = lastLine + after
      amountCharsForLineNumber = length $ show lastLineToShow
      prettyPrintedLineNumbers =
          (\n ->
              let asStr       = show n
                  spacesToAdd = amountCharsForLineNumber - length asStr
              in  replicate spacesToAdd ' ' <> asStr <> "|"
            )
            <$> lines
      before' = (\(lNum, line) -> colorWhen (not json) Grey $ lNum <> line)
        <$> slice firstLineToShow (firstLine - 1) (zip prettyPrintedLineNumbers code)
      expContent = uncurry (<>) <$> slice firstLine lastLine (zip prettyPrintedLineNumbers code)
      after'      = (\(lNum, line) -> colorWhen (not json) Grey $ lNum <> line)
        <$> slice (lastLine + 1) lastLineToShow (zip prettyPrintedLineNumbers code)
      (Area (Loc x line col) (Loc _ line' col')) = end
      endCol                = if line == line' then col' else col + 1
      highlightArea         = Area (Loc x line col) (Loc x line endCol)
      spacesBeforeHighlight = " " <> concat (" " <$ show lastLineToShow)
      formattedArea         = spacesBeforeHighlight <> formatHighlightArea highlightArea
  in  unlines $ before' ++ expContent ++ [formattedArea] ++ after'


letters :: [Char]
letters = ['a' ..]


hkLetters :: [Char]
hkLetters = ['m' ..]


kindToStr :: Kind -> String
kindToStr k = case k of
  Star     ->
    "*"

  Kfun l r ->
    kindToStr l <> " -> " <> kindToStr r


schemeToStr :: Scheme -> String
schemeToStr (Forall _ ([] :=> t)) = prettyPrintType True t
schemeToStr (Forall _ (ps :=> t)) =
  let (vars, hkVars, predStr) = predsToStr True (mempty, mempty) (dedupePreds ps)
      (_, _, typeStr)         = prettyPrintType' True (vars, hkVars) t
  in
    if length ps > 1 then
      "(" <> predStr <> ")" <> " => " <> typeStr
    else
      predStr <> " => " <> typeStr


predsToStr :: Bool -> (M.Map String Int, M.Map String Int) -> [Pred] -> (M.Map String Int, M.Map String Int, String)
predsToStr _ (vars, hkVars) [] = (vars, hkVars, "")
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
predToStr' _ (vars, hkVars) (IsIn _ [] _) = (vars, hkVars, "")
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


prettyPrintQualType :: Qual Type -> String
prettyPrintQualType qt =
  schemeToStr (Forall [] qt)


prettyPrintTypesWithDiff :: Bool -> (M.Map String Int, M.Map String Int) -> (M.Map String Int, M.Map String Int) -> Type -> Type -> ((M.Map String Int, M.Map String Int), (M.Map String Int, M.Map String Int), String, String)
prettyPrintTypesWithDiff colored vars1 vars2 t1 t2 = case (t1, t2) of
  (TApp (TApp (TCon (TC "(->)" _) _) tl1) tr1, TApp (TApp (TCon (TC "(->)" _) _) tl2) tr2) ->
    let (vars1', vars2', tl1', tl2')   = prettyPrintTypesWithDiff colored vars1 vars2 tl1 tl2
        (vars1'', vars2'', tr1', tr2') = prettyPrintTypesWithDiff colored vars1' vars2' tr1 tr2
        pretty1 = case tl1 of
          TApp (TApp (TCon (TC "(->)" _) _) _) _ ->
            colorWhen colored Grey "(" <> colorWhen colored Grey tl1' <> colorWhen colored Grey ")" <> colorWhen colored Grey " -> " <> colorWhen colored Grey tr1'

          _ ->
            colorWhen colored Grey tl1' <> colorWhen colored Grey " -> " <> colorWhen colored Grey tr1'

        pretty2 = case tl2 of
          TApp (TApp (TCon (TC "(->)" _) _) _) _ ->
            colorWhen colored Grey "(" <> colorWhen colored Grey tl2' <> colorWhen colored Grey ")" <> colorWhen colored Grey " -> " <> colorWhen colored Grey tr2'

          _ ->
            colorWhen colored Grey tl2' <> colorWhen colored Grey " -> " <> colorWhen colored Grey tr2'
    in  (vars1'', vars2'', pretty1, pretty2)

  (TApp (TApp (TCon (TC "(,)" _) _) tl1) tr1, TApp (TApp (TCon (TC "(,)" _) _) tl2) tr2) ->
    let (vars1', vars2', tl1', tl2')   = prettyPrintTypesWithDiff colored vars1 vars2 tl1 tl2
        (vars1'', vars2'', tr1', tr2') = prettyPrintTypesWithDiff colored vars1' vars2' tr1 tr2
        openTuple  = colorWhen colored Grey "#["
        closeTuple = colorWhen colored Grey "]"
        separator  = colorWhen colored Grey ", "
    in  ( vars1''
        , vars2''
        , openTuple <> tl1' <> separator <> tr1' <> closeTuple
        , openTuple <> tl2' <> separator <> tr2' <> closeTuple
        )

  (TApp (TApp (TApp (TCon (TC "(,,)" _) _) t11) t12) t13, TApp (TApp (TApp (TCon (TC "(,,)" _) _) t21) t22) t23) ->
    let (vars1', vars2', t11', t21')     = prettyPrintTypesWithDiff colored vars1 vars2 t11 t21
        (vars1'', vars2'', t12', t22')   = prettyPrintTypesWithDiff colored vars1' vars2' t12 t22
        (vars1''', vars2''', t13', t23') = prettyPrintTypesWithDiff colored vars1'' vars2'' t13 t23
        openTuple  = colorWhen colored Grey "#["
        closeTuple = colorWhen colored Grey "]"
        separator  = colorWhen colored Grey ", "
    in  ( vars1'''
        , vars2'''
        , openTuple <> t11' <> separator <> t12' <> separator <> t13' <> closeTuple
        , openTuple <> t21' <> separator <> t22' <> separator <> t23' <> closeTuple
        )

  (TApp (TApp (TApp (TApp (TCon (TC "(,,,)" _) _) t11) t12) t13) t14, TApp(TApp (TApp (TApp (TCon (TC "(,,,)" _) _) t21) t22) t23) t24) ->
    let (vars1', vars2', t11', t21')       = prettyPrintTypesWithDiff colored vars1 vars2 t11 t21
        (vars1'', vars2'', t12', t22')     = prettyPrintTypesWithDiff colored vars1' vars2' t12 t22
        (vars1''', vars2''', t13', t23')   = prettyPrintTypesWithDiff colored vars1'' vars2'' t13 t23
        (vars1'''', vars2'''', t14', t24') = prettyPrintTypesWithDiff colored vars1''' vars2''' t14 t24
        openTuple  = colorWhen colored Grey "#["
        closeTuple = colorWhen colored Grey "]"
        separator  = colorWhen colored Grey ", "
    in  ( vars1''''
        , vars2''''
        , openTuple <> t11' <> separator <> t12' <> separator <> t13' <> separator <> t14' <> closeTuple
        , openTuple <> t21' <> separator <> t22' <> separator <> t23' <> separator <> t24' <> closeTuple
        )

  (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,)" _) _) t11) t12) t13) t14) t15, TApp (TApp(TApp (TApp (TApp (TCon (TC "(,,,,)" _) _) t21) t22) t23) t24) t25) ->
    let (vars1', vars2', t11', t21')         = prettyPrintTypesWithDiff colored vars1 vars2 t11 t21
        (vars1'', vars2'', t12', t22')       = prettyPrintTypesWithDiff colored vars1' vars2' t12 t22
        (vars1''', vars2''', t13', t23')     = prettyPrintTypesWithDiff colored vars1'' vars2'' t13 t23
        (vars1'''', vars2'''', t14', t24')   = prettyPrintTypesWithDiff colored vars1''' vars2''' t14 t24
        (vars1''''', vars2''''', t15', t25') = prettyPrintTypesWithDiff colored vars1'''' vars2'''' t15 t25
        openTuple  = colorWhen colored Grey "#["
        closeTuple = colorWhen colored Grey "]"
        separator  = colorWhen colored Grey ", "
    in  ( vars1'''''
        , vars2'''''
        , openTuple <> t11' <> separator <> t12' <> separator <> t13' <> separator <> t14' <> separator <> t15' <> closeTuple
        , openTuple <> t21' <> separator <> t22' <> separator <> t23' <> separator <> t24' <> separator <> t25' <> closeTuple
        )

  (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,)" _) _) t11) t12) t13) t14) t15) t16, TApp (TApp (TApp(TApp (TApp (TApp (TCon (TC "(,,,,,)" _) _) t21) t22) t23) t24) t25) t26) ->
    let (vars1', vars2', t11', t21')           = prettyPrintTypesWithDiff colored vars1 vars2 t11 t21
        (vars1'', vars2'', t12', t22')         = prettyPrintTypesWithDiff colored vars1' vars2' t12 t22
        (vars1''', vars2''', t13', t23')       = prettyPrintTypesWithDiff colored vars1'' vars2'' t13 t23
        (vars1'''', vars2'''', t14', t24')     = prettyPrintTypesWithDiff colored vars1''' vars2''' t14 t24
        (vars1''''', vars2''''', t15', t25')   = prettyPrintTypesWithDiff colored vars1'''' vars2'''' t15 t25
        (vars1'''''', vars2'''''', t16', t26') = prettyPrintTypesWithDiff colored vars1''''' vars2''''' t16 t26
        openTuple  = colorWhen colored Grey "#["
        closeTuple = colorWhen colored Grey "]"
        separator  = colorWhen colored Grey ", "
    in  ( vars1''''''
        , vars2''''''
        , openTuple <> t11' <> separator <> t12' <> separator <> t13' <> separator <> t14' <> separator <> t15' <> separator <> t16' <> closeTuple
        , openTuple <> t21' <> separator <> t22' <> separator <> t23' <> separator <> t24' <> separator <> t25' <> separator <> t26' <> closeTuple
        )

  (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,)" _) _) t11) t12) t13) t14) t15) t16) t17, TApp (TApp (TApp (TApp(TApp (TApp (TApp (TCon (TC "(,,,,,,)" _) _) t21) t22) t23) t24) t25) t26) t27) ->
    let (vars1', vars2', t11', t21')             = prettyPrintTypesWithDiff colored vars1 vars2 t11 t21
        (vars1'', vars2'', t12', t22')           = prettyPrintTypesWithDiff colored vars1' vars2' t12 t22
        (vars1''', vars2''', t13', t23')         = prettyPrintTypesWithDiff colored vars1'' vars2'' t13 t23
        (vars1'''', vars2'''', t14', t24')       = prettyPrintTypesWithDiff colored vars1''' vars2''' t14 t24
        (vars1''''', vars2''''', t15', t25')     = prettyPrintTypesWithDiff colored vars1'''' vars2'''' t15 t25
        (vars1'''''', vars2'''''', t16', t26')   = prettyPrintTypesWithDiff colored vars1''''' vars2''''' t16 t26
        (vars1''''''', vars2''''''', t17', t27') = prettyPrintTypesWithDiff colored vars1'''''' vars2'''''' t17 t27
        openTuple  = colorWhen colored Grey "#["
        closeTuple = colorWhen colored Grey "]"
        separator  = colorWhen colored Grey ", "
    in  ( vars1'''''''
        , vars2'''''''
        , openTuple <> t11' <> separator <> t12' <> separator <> t13' <> separator <> t14' <> separator <> t15' <> separator <> t16' <> separator <> t17' <> closeTuple
        , openTuple <> t21' <> separator <> t22' <> separator <> t23' <> separator <> t24' <> separator <> t25' <> separator <> t26' <> separator <> t27' <> closeTuple
        )

  (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,,)" _) _) t11) t12) t13) t14) t15) t16) t17) t18, TApp (TApp (TApp (TApp (TApp(TApp (TApp (TApp (TCon (TC "(,,,,,,,)" _) _) t21) t22) t23) t24) t25) t26) t27) t28) ->
    let (vars1', vars2', t11', t21')               = prettyPrintTypesWithDiff colored vars1 vars2 t11 t21
        (vars1'', vars2'', t12', t22')             = prettyPrintTypesWithDiff colored vars1' vars2' t12 t22
        (vars1''', vars2''', t13', t23')           = prettyPrintTypesWithDiff colored vars1'' vars2'' t13 t23
        (vars1'''', vars2'''', t14', t24')         = prettyPrintTypesWithDiff colored vars1''' vars2''' t14 t24
        (vars1''''', vars2''''', t15', t25')       = prettyPrintTypesWithDiff colored vars1'''' vars2'''' t15 t25
        (vars1'''''', vars2'''''', t16', t26')     = prettyPrintTypesWithDiff colored vars1''''' vars2''''' t16 t26
        (vars1''''''', vars2''''''', t17', t27')   = prettyPrintTypesWithDiff colored vars1'''''' vars2'''''' t17 t27
        (vars1'''''''', vars2'''''''', t18', t28') = prettyPrintTypesWithDiff colored vars1''''''' vars2''''''' t18 t28
        openTuple  = colorWhen colored Grey "#["
        closeTuple = colorWhen colored Grey "]"
        separator  = colorWhen colored Grey ", "
    in  ( vars1''''''''
        , vars2''''''''
        , openTuple <> t11' <> separator <> t12' <> separator <> t13' <> separator <> t14' <> separator <> t15' <> separator <> t16' <> separator <> t17' <> separator <> t18' <> closeTuple
        , openTuple <> t21' <> separator <> t22' <> separator <> t23' <> separator <> t24' <> separator <> t25' <> separator <> t26' <> separator <> t27' <> separator <> t28' <> closeTuple
        )

  (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,,,)" _) _) t11) t12) t13) t14) t15) t16) t17) t18) t19, TApp (TApp (TApp (TApp (TApp (TApp(TApp (TApp (TApp (TCon (TC "(,,,,,,,,)" _) _) t21) t22) t23) t24) t25) t26) t27) t28) t29) ->
    let (vars1', vars2', t11', t21')                 = prettyPrintTypesWithDiff colored vars1 vars2 t11 t21
        (vars1'', vars2'', t12', t22')               = prettyPrintTypesWithDiff colored vars1' vars2' t12 t22
        (vars1''', vars2''', t13', t23')             = prettyPrintTypesWithDiff colored vars1'' vars2'' t13 t23
        (vars1'''', vars2'''', t14', t24')           = prettyPrintTypesWithDiff colored vars1''' vars2''' t14 t24
        (vars1''''', vars2''''', t15', t25')         = prettyPrintTypesWithDiff colored vars1'''' vars2'''' t15 t25
        (vars1'''''', vars2'''''', t16', t26')       = prettyPrintTypesWithDiff colored vars1''''' vars2''''' t16 t26
        (vars1''''''', vars2''''''', t17', t27')     = prettyPrintTypesWithDiff colored vars1'''''' vars2'''''' t17 t27
        (vars1'''''''', vars2'''''''', t18', t28')   = prettyPrintTypesWithDiff colored vars1''''''' vars2''''''' t18 t28
        (vars1''''''''', vars2''''''''', t19', t29') = prettyPrintTypesWithDiff colored vars1'''''''' vars2'''''''' t19 t29
        openTuple  = colorWhen colored Grey "#["
        closeTuple = colorWhen colored Grey "]"
        separator  = colorWhen colored Grey ", "
    in  ( vars1'''''''''
        , vars2'''''''''
        , openTuple <> t11' <> separator <> t12' <> separator <> t13' <> separator <> t14' <> separator <> t15' <> separator <> t16' <> separator <> t17' <> separator <> t18' <> separator <> t19' <> closeTuple
        , openTuple <> t21' <> separator <> t22' <> separator <> t23' <> separator <> t24' <> separator <> t25' <> separator <> t26' <> separator <> t27' <> separator <> t28' <> separator <> t29' <> closeTuple
        )

  (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,,,,)" _) _) t11) t12) t13) t14) t15) t16) t17) t18) t19) t110, TApp (TApp (TApp (TApp (TApp (TApp (TApp(TApp (TApp (TApp (TCon (TC "(,,,,,,,,,)" _) _) t21) t22) t23) t24) t25) t26) t27) t28) t29) t210) ->
    let (vars1', vars2', t11', t21')                     = prettyPrintTypesWithDiff colored vars1 vars2 t11 t21
        (vars1'', vars2'', t12', t22')                   = prettyPrintTypesWithDiff colored vars1' vars2' t12 t22
        (vars1''', vars2''', t13', t23')                 = prettyPrintTypesWithDiff colored vars1'' vars2'' t13 t23
        (vars1'''', vars2'''', t14', t24')               = prettyPrintTypesWithDiff colored vars1''' vars2''' t14 t24
        (vars1''''', vars2''''', t15', t25')             = prettyPrintTypesWithDiff colored vars1'''' vars2'''' t15 t25
        (vars1'''''', vars2'''''', t16', t26')           = prettyPrintTypesWithDiff colored vars1''''' vars2''''' t16 t26
        (vars1''''''', vars2''''''', t17', t27')         = prettyPrintTypesWithDiff colored vars1'''''' vars2'''''' t17 t27
        (vars1'''''''', vars2'''''''', t18', t28')       = prettyPrintTypesWithDiff colored vars1''''''' vars2''''''' t18 t28
        (vars1''''''''', vars2''''''''', t19', t29')     = prettyPrintTypesWithDiff colored vars1'''''''' vars2'''''''' t19 t29
        (vars1'''''''''', vars2'''''''''', t110', t210') = prettyPrintTypesWithDiff colored vars1''''''''' vars2''''''''' t110 t210
        openTuple  = colorWhen colored Grey "#["
        closeTuple = colorWhen colored Grey "]"
        separator  = colorWhen colored Grey ", "
    in  ( vars1''''''''''
        , vars2''''''''''
        , openTuple <> t11' <> separator <> t12' <> separator <> t13' <> separator <> t14' <> separator <> t15' <> separator <> t16' <> separator <> t17' <> separator <> t18' <> separator <> t19' <> separator <> t110' <> closeTuple
        , openTuple <> t21' <> separator <> t22' <> separator <> t23' <> separator <> t24' <> separator <> t25' <> separator <> t26' <> separator <> t27' <> separator <> t28' <> separator <> t29' <> separator <> t210' <> closeTuple
        )

  (TApp l1 r1, TApp l2 r2) ->
    let (vars1', vars2', l1', l2')   = prettyPrintTypesWithDiff colored vars1 vars2 l1 l2
        (vars1'', vars2'', r1', r2') = prettyPrintTypesWithDiff colored vars1' vars2' r1 r2
        pretty1 = case r1 of
          TApp _ _ | not (isTuple r1) ->
            l1' <> colorWhen colored Grey " (" <> r1' <> colorWhen colored Grey ")"

          _ ->
            l1' <> " " <> r1'
        pretty2 = case r2 of
          TApp _ _ | not (isTuple r2) ->
            l2' <> colorWhen colored Grey " (" <> r2' <> colorWhen colored Grey ")"

          _ ->
            l2' <> " " <> r2'
    in  (vars1'', vars2'', pretty1, pretty2)

  (TRecord fields1 base1, TRecord fields2 base2) ->
    let allFields1 = case base1 of
          Just (TRecord fields _) ->
            fields <> fields1
          _ ->
            fields1
        allFields2 = case base2 of
          Just (TRecord fields _) ->
            fields <> fields2
          _ ->
            fields2
        ((finalVars1, finalHkVars1), (finalVars2, finalHkVars2), compiledFields1, compiledFields2) =
            foldl'
                (\(allVars1, allVars2, compiledFields1', compiledFields2') (fieldName, fieldType1) ->
                    case M.lookup fieldName allFields2 of
                      Just fieldType2 ->
                        let (allVars1', allVars2', pretty1, pretty2) = prettyPrintTypesWithDiff colored allVars1 allVars2 fieldType1 fieldType2
                        in  (allVars1', allVars2', compiledFields1' ++ [colorWhen colored Grey $ fieldName <> " :: " <> pretty1], compiledFields2' ++ [colorWhen colored Grey $ fieldName <> " :: " <> pretty2])

                      Nothing ->
                        let (vars1', hkVars1', pretty1) = prettyPrintType' True allVars1 fieldType1
                        in  ((vars1', hkVars1'), allVars2, compiledFields1' ++ [colorWhen (Maybe.isJust base2 && colored) Grey $ colorWhen (Maybe.isNothing base2 && colored) Red $ fieldName <> " :: " <> pretty1], compiledFields2')
                )
                (vars1, vars2, [], [])
              $ M.toList allFields1

        missingFields = allFields2 M.\\ allFields1
        ((finalVars2', finalHkVars2'), compiledMissingFields') =
            foldl'
                (\(allVars2, compiledFields2') (fieldName, fieldType2) ->
                    let (vars2', hkVars2', pretty2) = prettyPrintType' True allVars2 fieldType2
                    in  ((vars2', hkVars2'), compiledFields2' ++ [colorWhen colored Green $ fieldName <> " :: " <> pretty2])
                )
                ((finalVars2, finalHkVars2), [])
              $ M.toList missingFields
        (finalVars1', formattedBase1)   = case base1 of
          Just t1  ->
            let (vars, hkVars, pretty) = prettyPrintType' True (finalVars1, finalHkVars1) t1
            in ((vars, hkVars), colorWhen colored Grey "..." <> colorWhen colored Grey pretty <> colorWhen colored Grey ", ")

          Nothing ->
            ((finalVars1, finalHkVars1), "")

        (finalVars2'', formattedBase2)   = case base2 of
          Just t2  ->
            let (vars, hkVars, pretty) = prettyPrintType' True (finalVars2', finalHkVars2') t2
            in ((vars, hkVars), colorWhen colored Grey "..." <> colorWhen colored Grey pretty <> colorWhen colored Grey ", ")

          Nothing ->
            ((finalVars2', finalHkVars2'), "")

        compiled1 = colorWhen colored Grey "{ " <> formattedBase1 <> intercalate (colorWhen colored Grey ", ") compiledFields1 <> colorWhen colored Grey " }"
        compiled2 = colorWhen colored Grey "{ " <> formattedBase2 <> intercalate (colorWhen colored Grey ", ") (compiledFields2 ++ compiledMissingFields') <> colorWhen colored Grey " }"
    in  (finalVars1', finalVars2'', compiled1, compiled2)

  (t1, t2) ->
    let (vars1', hkVars1', pretty1) = prettyPrintType' True vars1 t1
        (vars2', hkVars2', pretty2) = prettyPrintType' True vars2 t2
    in if t1 /= t2 then
      ((vars1', hkVars1'), (vars2', hkVars2'), colorWhen colored Red pretty1, colorWhen colored Green pretty2)
    else
      ((vars1', hkVars1'), (vars2', hkVars2'), colorWhen colored Grey pretty1, colorWhen colored Grey pretty2)



prettyPrintType :: Bool -> Type -> String
prettyPrintType rewrite =
  lst . prettyPrintType' rewrite (mempty, mempty)


prettyPrintType' :: Bool -> (M.Map String Int, M.Map String Int) -> Type -> (M.Map String Int, M.Map String Int, String)
prettyPrintType' rewrite (vars, hkVars) t = case t of
  TCon (TC n _) _ ->
    (vars, hkVars, n)

  TVar (TV n k)   ->
    if not rewrite then
      (vars, hkVars, n)
    else
      case k of
        Star -> case M.lookup n vars of
          Just x ->
            (vars, hkVars, [letters !! x])

          Nothing ->
            let newIndex = M.size vars
            in  (M.insert n newIndex vars, hkVars, [letters !! newIndex])

        Kfun _ _ -> case M.lookup n hkVars of
          Just x ->
            (vars, hkVars, [hkLetters !! x])

          Nothing ->
            let newIndex = M.size hkVars
            in  (vars, M.insert n newIndex hkVars, [hkLetters !! newIndex])

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
        (_, _, rightRightRight) =
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
                  TApp (TApp (TCon (TC "(->)" _) _) _) _ ->
                    True

                  _ ->
                    False
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
          Just _  -> "...base, "
          Nothing -> ""
        compiled = "{ " <> formattedBase <> intercalate ", " compiledFields' <> " }"
    in  (finalVars, finalHkVars, compiled)

  TGen n ->
    if not rewrite then
      (vars, hkVars, "T" <> show n)
    else
      case M.lookup ("T" <> show n) vars of
        Just x  ->
          (vars, hkVars, [letters !! x])

        Nothing ->
          let newIndex = M.size vars
          in  (M.insert ("T" <> show n) newIndex vars, hkVars, [letters !! newIndex])

  _ ->
    (vars, hkVars, "")


removeNamespace :: String -> String
removeNamespace name =
  if "." `isInfixOf` name then
    reverse . takeWhile (/= '.') . reverse $ name
  else
    name


prettyPrintTyping :: Slv.Typing -> String
prettyPrintTyping t@(Slv.Untyped _ typing) = case typing of
  Slv.TRComp _ ts ->
    if not (null ts) then
      "(" <> prettyPrintTyping' False t <> ")"
    else
      prettyPrintTyping' False t

  Slv.TRArr _ _ ->
    "(" <> prettyPrintTyping' False t <> ")"

  _ -> prettyPrintTyping' True t


prettyPrintTyping' :: Bool -> Slv.Typing -> String
prettyPrintTyping' paren (Slv.Untyped _ typing) = case typing of
  Slv.TRSingle n ->
    removeNamespace n

  Slv.TRComp n typing' ->
    let space = if not (null typing') then " " else ""
    in  if paren then
      "("
      <> removeNamespace n
      <> space
      <> unwords ((\t -> prettyPrintTyping' (isTRArrOrTRCompWithArgs t) t) <$> typing')
      <> ")"
    else
      removeNamespace n
      <> space
      <> unwords ((\t -> prettyPrintTyping' (isTRArrOrTRCompWithArgs t) t) <$> typing')

  Slv.TRArr (Slv.Untyped _ (Slv.TRArr l r)) r' ->
    "("
      <> prettyPrintTyping' False l
      <> " -> "
      <> prettyPrintTyping' False r
      <> ") -> "
      <> prettyPrintTyping' False r'

  Slv.TRArr l r ->
    if paren then
      "(" <> prettyPrintTyping' False l <> " -> " <> prettyPrintTyping' False r <> ")"
    else
      prettyPrintTyping' False l <> " -> " <> prettyPrintTyping' False r

  Slv.TRTuple ts ->
    "#[" <> intercalate ", " (prettyPrintTyping' False <$> ts) <> "]"

  Slv.TRRecord ts _ ->
    let mapped  = M.mapWithKey (\k v -> k <> " :: " <> prettyPrintTyping' False v) (snd <$> ts)
        fields  = M.elems mapped
        fields' = intercalate ", " fields
    in  "{ " <> fields' <> " }"


isTRArrOrTRCompWithArgs :: Slv.Typing -> Bool
isTRArrOrTRCompWithArgs (Slv.Untyped _ typing) = case typing of
  Slv.TRArr  _ _  ->
    True

  Slv.TRComp _ ts ->
    not (null ts)

  _               ->
    False


isTuple :: Type -> Bool
isTuple t = case t of
  TApp (TApp (TCon (TC "(,)" _) _) _) _ ->
    True

  TApp (TApp (TApp (TCon (TC "(,,)" _) _) _) _) _ ->
    True

  TApp (TApp (TApp (TApp (TCon (TC "(,,,)" _) _) _) _) _) _ ->
    True

  TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,)" _) _) _) _) _) _) _ ->
    True

  TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,)" _) _) _) _) _) _) _) _ ->
    True

  TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,)" _) _) _) _) _) _) _) _) _ ->
    True

  TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,,)" _) _) _) _) _) _) _) _) _) _ ->
    True

  TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,,,)" _) _) _) _) _) _) _) _) _) _) _ ->
    True

  TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,,,,)" _) _) _) _) _) _) _) _) _) _) _) _ ->
    True

  _ -> False


slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)
