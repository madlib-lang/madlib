{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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
import qualified Data.List as List
import qualified Prettyprinter.Internal.Type as Pretty
import Debug.Trace


indentationSize :: Int
indentationSize = 2

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


simpleFormatWarning :: Bool -> CompilationWarning -> IO String
simpleFormatWarning json (CompilationWarning warning ctx) = do
  noColor       <- lookupEnv "NO_COLOR"
  let isColorEnabled =  not json && not (noColor /= Just "" && Maybe.isJust noColor)
  return $ createSimpleWarningDiagnostic isColorEnabled ctx warning


createSimpleWarningDiagnostic :: Bool -> Context -> WarningKind -> String
createSimpleWarningDiagnostic _ _ warning = case warning of
  UnusedImport name path ->
    "Unused import\n\n"
    <> "You imported '" <> name <> "' from the module located at\n"
    <> "'" <> path <> "'\n"
    <> "but it seems that you never use it.\n\n"
    <> "Hint: Remove the import if you don't use it."

  MissingMethods missingMethods ->
    "Missing methods\n\n"
    <> "The instance does not implement all methods. The missing\n"
    <> "methods are the following:\n"
    <> intercalate "\n" (map ("  - "<>) missingMethods)
    <> "\n\n"
    <> "Hint: Implement the missing methods."

  UnusedParameter name ->
    "Unused parameter\n\n"
    <> "You declared a parameter named '" <> name <> "' but it seems that\n"
    <> "you never use it\n\n"
    <> "Hint: Remove this parameter if you don't need it\n"
    <> "Hint: Use the skip parameter '_' to suppress this warning."

  UnusedDeclaration name ->
    "Unused variable\n\n"
    <> "You declared a variable named '" <> name <> "' but it seems that\n"
    <> "you never use it\n\n"
    <> "Hint: Remove it if you don't need it."

  UnusedTopLevelDeclaration name ->
    "Unused top level binding\n\n"
    <> "You declared a top level binding named '" <> name <> "' but it seems that\n"
    <> "you never use it\n\n"
    <> "Hint: Remove it if you don't need it."

  UnusedConstructor name ->
    "Unused constructor\n\n"
    <> "You declared a constructor named '" <> name <> "' but it seems that\n"
    <> "you never use it\n\n"
    <> "Hint: Remove it if you don't need it."

  UnusedType name ->
    "Unused type\n\n"
    <> "You declared a type named '" <> name <> "' but it seems that\n"
    <> "you never use it\n\n"
    <> "Hint: Remove it if you don't need it."

  MadlibVersionMinorTooLow pkgName minVersion versionUsed ->
    let start = case pkgName of
          Just n  -> "The package '" <> n <> "'"
          Nothing -> "This package"
    in  "Minor Madlib version is too low\n\n"
        <> start
        <> " requires the minimum version '"
        <> minVersion
        <> "' but you currently use madlib\n"
        <> "version '"
        <> versionUsed
        <> "'\n\n"
        <> "Hint: Update your version of madlib."

  MadlibVersionMajorDiffer pkgName minVersion versionUsed ->
    let start = case pkgName of
          Just n  -> "The package '" <> n <> "'"
          Nothing -> "This package"
    in  "Minor Madlib version is too low\n\n"
        <> start
        <> " requires the minimum version '"
        <> minVersion
        <> "' but you currently use madlib\n"
        <> "version '"
        <> versionUsed
        <> "'. Because major versions differ it means there is a breaking\n"
        <> "change and you may not be able to run the project\n\n"
        <> "Hint: Update your version of madlib."

  IncompletePattern missingPatterns ->
    "Incomplete pattern\n\n"
    <> "Examples of missing patterns:\n"
    <> intercalate "\n" (map ("  - "++) missingPatterns) <> "\n\n"
    <> "Note: If the input of where is not handled by a branch, it will most likely crash at\nruntime.\n"
    <> "Hint: Pattern match the missing constructors or add a catch all branch with '_ => ...'."

  RedundantPattern ->
    "Redundant pattern\n\n"
    <> "Note: This pattern will never be reached.\n"
    <> "Hint: Remove it or move it higher up so that it might be useful."

  TypedHoleFound t ->
    let (renderedType, _) = renderSchemesWithDiff False (Forall [] ([] :=> t)) (Forall [] ([] :=> t))
        indentedType = unlines $ ("  "<>) <$> lines renderedType
    in  "Typed hole\n\n"
        <> "I found a typed hole with type:\n" <> indentedType
        <> "\n\n"
        <> "Note: This will crash at runtime if reached.\n"
        <> "Hint: Replace it with a valid expression of that type."


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

  IncompletePattern missingPatterns ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Warn
          Nothing
          "Incomplete pattern"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "The branches do not cover all cases\n"
                <> "Examples of missing patterns:\n"
                <> intercalate "\n" (map ("  - "++) missingPatterns)
            )
          ]
          [ Diagnose.Note "If the input of where is not handled by a branch, it will most likely crash at\nruntime."
          , Diagnose.Hint "Pattern match the missing constructors or add a catch all branch with '_ => ...'."
          ]

      NoContext ->
        Diagnose.Warn
          Nothing
          "Incomplete pattern"
          []
          [ Diagnose.Note "If the input of where is not handled by a branch, it will most likely crash at\nruntime."
          , Diagnose.Hint "Pattern match the missing constructors or add a catch all branch with '_ => ...'."
          ]

  RedundantPattern ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Warn
          Nothing
          "Redundant pattern"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This "Unreachable pattern"
            )
          ]
          [ Diagnose.Note "This pattern will never be reached."
          , Diagnose.Hint "Remove it or move it higher up so that it might be useful."
          ]

      NoContext ->
        Diagnose.Warn
          Nothing
          "Redundant pattern"
          []
          [ Diagnose.Note "This pattern will never be reached."
          , Diagnose.Hint "Remove it or move it higher up so that it might be useful."
          ]

  TypedHoleFound t ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        let (renderedType, _) = renderSchemesWithDiff False (Forall [] ([] :=> t)) (Forall [] ([] :=> t))
            indentedType = unlines $ ("  "<>) <$> lines renderedType
        in  Diagnose.Warn
              Nothing
              "Typed hole"
              [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
                , Diagnose.This $ "I found a typed hole with type:\n  " <> indentedType
                )
              ]
              [ Diagnose.Note "This will crash at runtime if reached."
              , Diagnose.Hint "Replace it with a valid expression of that type."
              ]

      NoContext ->
        Diagnose.Warn
          Nothing
          "Typed hole"
          []
          [ Diagnose.Note "This will crash at runtime if reached."
          , Diagnose.Hint "Replace it with a valid expression of that type."
          ]


formatError :: (FilePath -> IO String) -> Bool -> CompilationError -> IO String
formatError rf json (CompilationError err ctx) = do
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


simpleFormatError :: Bool -> CompilationError -> IO String
simpleFormatError json (CompilationError err ctx) = do
  noColor <- lookupEnv "NO_COLOR"
  let isColorEnabled = not json && not (noColor /= Just "" && Maybe.isJust noColor)
  return $ createSimpleErrorDiagnostic isColorEnabled ctx err


renderTypesWithDiff :: Bool -> Type -> Type -> (String, String)
renderTypesWithDiff color t1 t2 =
  let (_, _, docT1, docT2) = typesToDocWithDiff (mempty, mempty) (mempty, mempty) t1 t2
      (docT1', docT2')  =
        if color then
          (docT1, docT2)
        else
          (Pretty.unAnnotate docT1, Pretty.unAnnotate docT2)
      layoutOptions = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.AvailablePerLine 50 1.0 }
      s1 = Terminal.renderStrict (Pretty.layoutPretty layoutOptions docT1')
      s2 = Terminal.renderStrict (Pretty.layoutPretty layoutOptions docT2')
  in  ( if color then
          "\x1b[0m" <> Text.unpack s1
        else
          Text.unpack s1
      , if color then
          "\x1b[0m" <> Text.unpack s2
        else
          Text.unpack s2
      )

renderSchemesWithDiff :: Bool -> Scheme -> Scheme -> (String, String)
renderSchemesWithDiff color sc1 sc2 =
  let (_, _, docT1, docT2) = schemesToDocWithDiff (mempty, mempty) (mempty, mempty) sc1 sc2
      (docT1', docT2')  =
        if color then
          (docT1, docT2)
        else
          (Pretty.unAnnotate docT1, Pretty.unAnnotate docT2)
      layoutOptions = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.AvailablePerLine 50 1.0 }
      s1            = Terminal.renderStrict (Pretty.layoutPretty layoutOptions docT1')
      s2            = Terminal.renderStrict (Pretty.layoutPretty layoutOptions docT2')
  in  ( if color then
          "\x1b[0m" <> Text.unpack s1
        else
          Text.unpack s1
      , if color then
          "\x1b[0m" <> Text.unpack s2
        else
          Text.unpack s2
      )


renderType :: Type -> String
renderType t =
  let (_, _, docT)  = typeToDoc (mempty, mempty) t
      docT'         = Pretty.unAnnotate docT
      layoutOptions = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.AvailablePerLine 50 1.0 }
      s             = Terminal.renderStrict (Pretty.layoutPretty layoutOptions docT')
  in  Text.unpack s


renderScheme :: Scheme -> String
renderScheme sc =
  let (_, _, docT)  = schemeToDoc (mempty, mempty) sc
      docT'         = Pretty.unAnnotate docT
      layoutOptions = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.AvailablePerLine 50 1.0 }
      s             = Terminal.renderStrict (Pretty.layoutPretty layoutOptions docT')
  in  Text.unpack s


createSimpleErrorDiagnostic :: Bool -> Context -> TypeError -> String
createSimpleErrorDiagnostic color _ typeError = case typeError of
  UnificationError t1 t2 ->
    let (pretty1', pretty2') = renderTypesWithDiff color t1 t2
        pretty1'' = unlines $ ("  "<>) <$> lines pretty1'
        pretty2'' = unlines $ ("  "<>) <$> lines pretty2'
        expectedStr = if color then "\x1b[0mexpected:\n" else "expected:\n"
        foundStr = if color then "\n\x1b[0mbut found:\n" else "\nbut found:\n"
    in  "Type error\n\n" <> expectedStr <> pretty2'' <> foundStr <> pretty1''

  TypingHasWrongKind t expectedKind actualKind ->
    let expectedStr = if color then "\x1b[0mexpected:\n  " else "expected:\n  "
        foundStr = if color then "\n\x1b[0mbut found:\n  " else "\nbut found:\n  "
    in  "Typing has wrong kind\n\n"
        <> "The type annotation '" <> prettyPrintType False t <> "' has a wrong kind.\n"
        <> expectedStr
        <> colorWhen color Green (kindToStr expectedKind)
        <> foundStr
        <> colorWhen color Red (kindToStr actualKind)

  NoMain ->
    "You forgot to define a 'main' function in your main module\n\n"
    <> "Hint: Add a main method"

  MainInvalidTyping ->
    "The main function has a wrong type signature\n\n"
    <> "Hint: The typing of the main function should be 'List String -> {}'"

  MutationRestriction ->
    "This value depends on a closure doing mutation and can't be generic\n\n"
    <> "Hint: Add a type definition with concrete types"

  MutatingFunction n ->
    "You are trying to mutate the function '" <> n <> "'. It is currently not allowed\n\n"
    <> "Hint: Wrap it in a record\n"
    <> "Hint: Use a closure that returns a setter and getter to mutate it"

  MethodNameAlreadyDefined ->
    "You are trying to redefine a method name\n\n"
    <> "Hint: Use a different name for the method"

  NotADefinition ->
    "Not a definition\n\n"
    <> "It is not a definition and is not allowed\n\n"
    <> "Note: Top level expressions are not allowed in Madlib.\n"
    <> "Hint: You may want to assign it to a top level variable.\n"

  InfiniteType tv t ->
    let (vars, hkVars, printedT) = prettyPrintType' True (mempty, mempty) t
        (_, _, printedN)         = prettyPrintType' True (vars, hkVars) (TVar tv)
    in  "Infinite type\n\n"
        <> "Infinite type " <> printedN <> " -> " <> printedT

  IllegalSkipAccess ->
    "Illegal skip access\n\n"
    <> "You accessed the skip symbol '_'. This is not permitted as\n"
    <> "it does not hold any value and only serves to indicate that\n"
    <> "you are not interested in whatever it may contain.\n\n"
    <> "Hint: Give it a name if you intend to use it"

  UnboundVariable n ->
    "Unbound variable\n\n"
    <> "The variable '" <> n <> "' has not been declared\n\n"
    <> "Hint: Verify that you don't have a typo."

  UnboundVariableFromNamespace namespace name ->
    "Name not exported\n\n"
    <> "Function '" <> name <> "' not found in\ndefault import '" <> namespace <> "'\n\n"
    <> "Hint: Verify that it is exported or that you spelled it correctly."

  CapitalizedADTTVar adtname param ->
    "Capitalized ADT variable\n\n"
    <> "The type parameter '" <> param <> "' in the type declaration\n"
    <> "'" <> adtname <> "' is capitalized. Type parameters can't be capitalized."
    <> "Hint: Either remove it if you don't need the type variable, or\nmake its first letter lowercase."

  UnboundType n ->
    "Unbound Type\n\n"
    <> "The type '" <> n <> "' has not been declared\n\n"
    <> "Hint: Maybe you forgot to import it?\n"
    <> "Hint: Maybe you have a typo?"

  DerivingAliasNotAllowed n ->
    "Deriving Alias Not Allowed\n\n"
    <> "The type '" <> n <> "' is an alias.\n\n"
    <> "Hint: Aliases can't be derived, use the aliased type isntead\n"

  InvalidInterfaceDerived n ->
    "Invalid Interface Derived\n\n"
    <> "The interface '" <> n <> "' can't be derived.\n\n"
    <> "Note: Eq and Show are automatically derived\n"
    <> "Hint: Currently only Comparable can be derived\n"

  SignatureTooGeneral scGiven scInferred ->
    let (scInferred', scGiven') = renderSchemesWithDiff color scInferred scGiven
        scGiven''    = unlines $ ("  "<>) <$> lines scGiven'
        scInferred'' = unlines $ ("  "<>) <$> lines scInferred'
        givenStr     = if color then "\x1b[0mType signature given:\n" else "Type signature given:\n  "
        inferredStr  = if color then "\n\x1b[0mType inferred:\n" else "\nType inferred:\n  "
    in  "Signature too general\n\n"
        <> givenStr <> scGiven'' <> inferredStr <> scInferred''

  NoInstanceFound cls ts ->
    "Instance not found\n\n"
    <> "No instance for '" <> lst (predToStr True (mempty, mempty) (IsIn cls ts Nothing)) <> "' was found.\n\n"
    <> "Hint: Verify that you imported the module where the " <> cls <> "\ninstance for '" <> unwords (prettyPrintType True <$> ts) <> "' is defined"
    <> "Note: Remember that instance methods are automatically imported when the module\nis imported, directly, or indirectly."

  AmbiguousType (TV _ _, IsIn cls _ _ : _) ->
    "Ambiguous type\n\n"
    <> "An instance of '" <> cls <> "' could not be found\n\n"
    <> "Hint: You can add a type annotation to make it resolvable."

  AmbiguousType (TV n _, []) ->
    "Ambiguous type\n\n"
    <> "An ambiguity for the type variable '" <> n <> "' could not be resolved\n\n"
    <> "Hint: You can add a type annotation to make it resolvable."

  InterfaceNotExisting cls ->
    "Interface not found\n\n"
    <> "The interface '" <> cls <> "' is not defined.\n\n"
    <> "Hint: Make sure you imported the module defining it,\nor a module that imports it."

  KindError (t, k) (t', k') ->
    "Kind error\n\n"
    <> "The kind of types don't match, '"
    <> prettyPrintType True t
    <> "' has kind "
    <> kindToStr k
    <> " and "
    <> prettyPrintType True t'
    <> " has kind "
    <> kindToStr k'
    <> "."

  InstancePredicateError pInstance pWrong pCorrect ->
    "Instance predicate error\n\n"
    <> "A constraint in the instance declaration '"
    <> lst (predToStr True (mempty, mempty) pInstance)
    <> " is not correct.\n"
    <> "You gave the constraint '"
    <> lst (predToStr True (mempty, mempty) pWrong)
    <> "' but a constraint of the form '"
    <> lst (predToStr True (mempty, mempty) pCorrect)
    <> "'\nwas expected."

  ImportCycle paths ->
    "Import cycle\n\n"
    <> "I found an import cycle:\n"
    <> buildCycleOutput (length paths) 0 paths
    <> "\n\n"
    <> "Note: Import cycles are not allowed and usually show a design issue.\n"
    <> "Hint: Consider splitting things in more modules in order to have\n"
    <> "both modules import a common dependency instead of having\n"
    <> "them being co-dependent.\n"
    <> "Hint: Another solution would be to move\n"
    <> "things that depend on the other module from the cycle into\n"
    <> "the other in order to collocate things that depend on each\nother."
    where
      buildCycleOutput :: Int -> Int -> [FilePath] -> String
      buildCycleOutput total current paths =
        let amountOfSpaces = current * 2
            spaces         = concat $ replicate amountOfSpaces " "
            prefix         = spaces <> if current /= 0 then "-> " else ""
            next           = if current < (total - 1) then buildCycleOutput total (current + 1) paths else ""
        in  prefix <> paths !! current <> "\n" <> next

  TypeAnnotationNameMismatch typingName expName ->
    "The name of type annotation ( " <> typingName <> " ) does not match the name of definition ( " <> expName <> " )"

  GrammarError _ _ ->
    "Grammar error\n\n"
    <> "Unexpected character"

  UnknownType t ->
    "Unknown type\n\n"
    <> "The type '" <> t <> "' was not found\n\n"
    <> "Hint: Verify that you imported it"

  NameAlreadyDefined name ->
    "Illegal shadowing\n\n"
    <> "The variable '" <> name <> "' is already defined\n\n"
    <> "Hint: Change the name of the variable.\n"
    <> "Note: The variable might be defined further down. All top level\n"
    <> "assignments share the scope and using a local name that is\n"
    <> "defined in the global scope of a module is not allowed."

  NameAlreadyExported name ->
    "Already exported\n\n"
    <> "Export already defined. You are trying to export the\n"
    <> "name '" <> name <> "' but it\n"
    <> "appears that you have already exported it."

  NotExported name path ->
    "Not exported\n\n"
    <> "You are trying to import '" <> name <> "' from\n"
    <> "the module located here:\n"
    <> "'" <> path <> "'\n"
    <> "Unfortunately, that module does not export '" <> name <> "'\n\n"
    <> "Hint: Verify that you spelled it correctly or add the export to the module if you can."

  RecursiveVarAccess _ ->
    "Recursive variable access\n\n"
    <> "You are using a variable that is recursively accessing itself\n"
    <> "and is thus not yet initialized\n\n"
    <> "Note: This is not allowed and can only work if there exists a\n"
    <> "function in between, let me show you some examples that should\n"
    <> "make this clearer:\n"
    <> "// this is not allowed because parser is directly refering to itself\n"
    <> "parser = J.map(Title, J.field(\"title\", parser))\n"
    <> "// this works because now the recursive accessed is wrapped in a function\n"
    <> "parser = J.map(Title, J.field(\"title\", J.lazy((_) => parser)))"

  NotInScope name (Loc _ line _) ->
    "Not in scope\n\n"
    <> "This expression relies on an expression that accesses the\n"
    <> "variable '" <> name <> "' at line " <> ppShow line <> "\n\n"
    <> "Note: All variables need to have been defined by the time they are\n"
    <> "accessed and this access is thus not allowed.\n"
    <> "Hint: Move that call further down in the module so that the name\n"
    <> "is defined when you access it."

  TypesHaveDifferentOrigin adtName origin1 origin2 ->
    "Types have different origins\n\n"
    <> "Types do not match. You try to use a type that seems similar\n"
    <> "but comes from two different locations. The type '" <> adtName <> "'\n"
    <> "is used from:\n"
    <> "  - '" <> origin1 <> "'\n"
    <> "  - '" <> origin2 <> "'\n\n"
    <> "Hint: Import it only from one place, or if you meant to use both,\n"
    <> "make sure to convert from one to the other correctly."

  ShouldBeTypedOrAbove name ->
    "Must be typed or above\n\n"
    <> "You access the name '" <> name <> "' before it\n"
    <> "is defined\n\n"
    <> "Note: This is fine, but in that case you must give it a type\n"
    <> "annotation.\n"
    <> "Hint: Place that declaration above the place you use it, or give\n"
    <> "it a type annotation."

  NotCapitalizedADTName name ->
    "ADT name not capitalized\n\n"
    <> "The name '" <> name <> "' of this type is not capitalized\n\n"
    <> "Note: This is incorrect and all types in madlib should start with\n"
    <> "an uppercased letter."

  NotCapitalizedAliasName name ->
    "Alias name not capitalized\n\n"
    <> "The name '" <> name <> "' of this type alias is not capitalized\n\n"
    <> "Note: This is incorrect and all types in madlib should start with\n"
    <> "an uppercased letter."

  NotCapitalizedConstructorName name ->
    "Constructor name not capitalized\n\n"
    <> "The name '" <> name <> "' of this type constructor is not capitalized\n\n"
    <> "Note: This is incorrect and all types in madlib should start with\n"
              <> "an uppercased letter."

  ContextTooWeak preds ->
    "Context too weak\n\n"
    <> "The context of the type annotation is too weak. The type\n"
    <> "inferred for the implementation has the following\n"
    <> "constraints: " <> intercalate ", " (predClass <$> preds)
    <> "\n\n"
    <> "Hint: Add the missing interface constraints to the type annotation."

  OverloadedMutation n _ ->
    "Mutation in overloaded context\n\n"
    <> "You are mutating the variable '" <> n <> "' in a function that has constraints"
    <> "\n\n"
    <> "Note: This will not work as it'll always generate a new reference for the closure\n"
    <> "leading to the value not being changed.\n"
    <> "Hint: Add or change type annotations to suppress the constraints."

  WrongAliasArgCount aliasName expected actual ->
    "Wrong alias argument count\n\n"
    <> "The alias '" <> aliasName <> "' was expected to have " <> show expected <> " argument" <> (if expected > 1 then "s" else "") <> ",\nbut "
    <> show actual <> " "<> (if actual > 1 then "were" else "was") <>" given\n\n"
    <>  if actual > expected then
          "Hint: Remove " <> show (actual - expected) <> " argument(s)"
        else
          "Hint: Add the missing '" <> show (expected - actual) <> "' argument(s)"

  ImportNotFound importName ->
    "Import not found\n\n"
    <> "You tried to import the module '" <> importName <> "',\n"
    <> "but it could not be found\n\n"
    <> "Hint: Verify that you don't have a typo."

  InterfaceAlreadyDefined interfaceName ->
    "Interface already defined\n\n"
    <> "You defined the interface '" <> interfaceName <> "',\n"
    <> "but it already exists\n\n"
    <> "Hint: Verify that you don't have a typo."

  ADTAlreadyDefined adtType ->
    let adtName = renderType adtType
    in  "Type already defined\n\n"
        <> "You defined the type '" <> adtName <> "',\n"
        <> "but it already exists\n\n"
        <> "Hint: Verify that you don't have a typo."

  WrongSpreadType t ->
    "Type error\n\n" <> t <> "\n\n"
    <> "Hint: Verify that you don't have a typo."

  FatalError ->
    "Fatal error"

  Error ->
    "Unknown error"

  ASTHasNoPath ->
    "A module could not be found or loaded"

  ConstructorAccessBadIndex typeName constructorName arity index ->
    "You want to access the parameter at index '" <> show index <> "' for the constructor '" <> constructorName <> "'\n"
    <> "from type '" <> typeName <> "' but it has only " <> show arity <> " parameters."

  ConstructorAccessNoConstructorFound typeName ->
    "No constructor found for the type '" <> typeName <> "'."


  ConstructorAccessTooManyConstructors typeName _ ->
    "You can't access a value from the constructor of the type '" <> typeName <> "' because it has more than one constructor."




createErrorDiagnostic :: Bool -> Context -> TypeError -> Diagnose.Report String
createErrorDiagnostic color context typeError = case typeError of
  UnificationError t1 t2 ->
    let (pretty1', pretty2') = renderTypesWithDiff color t1 t2
        pretty1'' = unlines $ ("  "<>) <$> lines pretty1'
        pretty2'' = unlines $ ("  "<>) <$> lines pretty2'
        expectedStr = if color then "\x1b[0mexpected:\n" else "expected:\n  "
        foundStr = if color then "\n\x1b[0mbut found:\n" else "\nbut found:\n  "
    in  case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Type error"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $ expectedStr <> pretty2'' <> foundStr <> pretty1''
            )
          ]
          []

      NoContext ->
        Diagnose.Err
          Nothing
          ("Type error\n\n" <> expectedStr <> pretty2'' <> foundStr <> pretty1'')
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
      [Diagnose.Hint "Add a main method"]

  MainInvalidTyping ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Main invalid typing"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $ "The main function has a wrong type signature"
            )
          ]
          [ Diagnose.Hint "The typing of the main function should be 'List String -> {}'"
          , Diagnose.Note "You can omit the typing for the main function"
          ]

  MutationRestriction ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Mutation restriction"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $ "This value depends on a closure doing mutation and can't be generic"
            )
          ]
          [Diagnose.Hint "Add a type definition with concrete types"]

  MutatingFunction n ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Mutation function"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $ "You are trying to mutate the function '" <> n <> "'. It is currently not allowed."
            )
          ]
          [ Diagnose.Hint "Wrap it in a record"
          , Diagnose.Hint "Use a closure that returns a setter and getter to mutate it"
          ]

  MethodNameAlreadyDefined ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Method name already defined"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $ "You are trying to redefine a method name"
            )
          ]
          [ Diagnose.Hint "Use a different name for the method"
          ]

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

  ConstructorAccessBadIndex typeName constructorName arity index ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Constructor access - bad index"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "You want to access the parameter at index '" <> show index <> "' for the constructor '" <> constructorName <> "'\n"
                <> "from type '" <> typeName <> "' but it has only " <> show arity <> " parameters."
            )
          ]
          [ Diagnose.Hint $ "Verify the arity of '" <> constructorName <> "'" ]

  ConstructorAccessNoConstructorFound typeName ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Constructor not found"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "No constructor found for the type '" <> typeName <> "'."
            )
          ]
          []

  ConstructorAccessTooManyConstructors typeName _ ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Constructor access - too many constructors"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "You can't access a value from the constructor of the type '"
                <> typeName <> "'\nbecause it has more than one constructor."
            )
          ]
          []

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

  DerivingAliasNotAllowed n ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Deriving Alias Not Allowed"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $ "The type '" <> n <> "' is an alias."
            )
          ]
          [Diagnose.Hint "Aliases can't be derived, use the aliased type isntead"]

      NoContext ->
        Diagnose.Err
          Nothing
          "Deriving Alias Not Allowed"
          []
          [Diagnose.Hint "Aliases can't be derived, use the aliased type isntead"]

  InvalidInterfaceDerived n ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Invalid Interface Derived"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $ "The interface '" <> n <> "' can't be derived."
            )
          ]
          [Diagnose.Note "Eq and Show are automatically derived", Diagnose.Hint "Currently only Comparable can be derived"]

      NoContext ->
        Diagnose.Err
          Nothing
          "Invalid Interface Derived"
          []
          [Diagnose.Note "Eq and Show are automatically derived", Diagnose.Hint "Currently only Comparable can be derived"]

  SignatureTooGeneral scGiven scInferred ->
    let (scInferred', scGiven') = renderSchemesWithDiff color scInferred scGiven
        scGiven''    = unlines $ ("  "<>) <$> lines scGiven'
        scInferred'' = unlines $ ("  "<>) <$> lines scInferred'
        givenStr     = if color then "\x1b[0mType signature given:\n" else "Type signature given:\n  "
        inferredStr  = if color then "\n\x1b[0mType inferred:\n" else "\nType inferred:\n  "
    in  case context of
          Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
            Diagnose.Err
              Nothing
              "Signature too general"
              [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
                , Diagnose.This $ givenStr <> scGiven'' <> inferredStr <> scInferred''
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
          [ Diagnose.Note "Import cycles are not allowed and usually show a design issue."
          , Diagnose.Hint $
              "Consider splitting things in more modules in order to have\n"
              <> "both modules import a common dependency instead of having\n"
              <> "them being co-dependent."
          , Diagnose.Hint $
              "Another solution would be to move\n"
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

  TypeAnnotationNameMismatch typingName expName ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Type annotation error"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $ "The name of type annotation ( " <> typingName <> " ) does not match the name of definition ( " <> expName <> " )"
            )
          ]
          []

      NoContext ->
        Diagnose.Err
          Nothing
          "Type annotation error"
          []
          []

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
          "Unknown type"
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
        let positionInfos =
              Maybe.mapMaybe
                (\p@(IsIn _ _ maybeArea) ->
                  case maybeArea of
                    Just (Area (Loc _ startL startC) (Loc _ endL endC)) ->
                      let (_, _, rendererPred) = predToStr True (mempty, mempty) p
                      in  Just
                            ( Diagnose.Position (startL, startC) (endL, endC) modulePath
                            , Diagnose.This $ "The constraint '" <> rendererPred <> "' originates from here"
                            )

                    Nothing ->
                      Nothing
                )
                preds
        in  Diagnose.Err
              Nothing
              "Context too weak"
              (
                [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
                  , Diagnose.This $
                      "The context of the type annotation is too weak. The type\n"
                      <> "inferred for the implementation has constraints\n"
                      <> "for the following instances: " <> intercalate ", " (predClass <$> preds)
                  )
                ] ++ positionInfos
              )
              [Diagnose.Hint  "Add the missing interface constraints to the type annotation."]

      NoContext ->
        Diagnose.Err
          Nothing
          "Context too weak"
          []
          [Diagnose.Hint  "Add the missing interface constraints to the type annotation."]

  OverloadedMutation n _ ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Mutation in overloaded context"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "You are mutating the variable '" <> n <> "' in a function that has constraints"
            )
          ]
          [ Diagnose.Note $ "This will not work as it'll always generate a new reference for the closure\n"
              <> "leading to the value not being changed."
          , Diagnose.Hint "Add or change type annotations to suppress the constraints."
          ]

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

  InterfaceAlreadyDefined interfaceName ->
    case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Interface already defined"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "You defined the interface '" <> interfaceName <> "',\n"
                <> "but it already exists"
            )
          ]
          [Diagnose.Hint "Verify that you don't have a typo."]

      NoContext ->
        Diagnose.Err
          Nothing
          "Interface already defined"
          []
          [Diagnose.Hint "Verify that you don't have a typo."]

  ADTAlreadyDefined adtType ->
    let adtName = renderType adtType
    in  case context of
      Context modulePath (Area (Loc _ startL startC) (Loc _ endL endC)) ->
        Diagnose.Err
          Nothing
          "Type already defined"
          [ ( Diagnose.Position (startL, startC) (endL, endC) modulePath
            , Diagnose.This $
                "You defined the type '" <> adtName <> "',\n"
                <> "but it already exists"
            )
          ]
          [Diagnose.Hint "Verify that you don't have a typo."]

      NoContext ->
        Diagnose.Err
          Nothing
          "Type already defined"
          []
          [Diagnose.Hint "Verify that you don't have a typo."]

  WrongSpreadType t ->
    Diagnose.Err
      Nothing
      ("Type error\n\n" <> t)
      []
      [Diagnose.Hint "Verify that you don't have a typo."]

  FatalError ->
    Diagnose.Err
      Nothing
      "Fatal error"
      []
      []

  Error ->
    Diagnose.Err
      Nothing
      "Unknown error"
      []
      []

  ASTHasNoPath ->
    Diagnose.Err
      Nothing
      "A module could not be found or loaded"
      []
      []


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


pushAnnotation :: ann -> Pretty.Doc ann -> Pretty.Doc ann
pushAnnotation ann doc = case doc of
  Pretty.Fail     -> Pretty.Annotated ann Pretty.Fail
  Pretty.Empty    -> Pretty.Annotated ann Pretty.Empty
  Pretty.Char c   -> Pretty.Annotated ann $ Pretty.Char c
  Pretty.Text l t -> Pretty.Annotated ann $ Pretty.Text l t
  Pretty.Line     -> Pretty.Annotated ann Pretty.Line

  Pretty.FlatAlt x y     -> Pretty.Annotated ann $ Pretty.FlatAlt (pushAnnotation ann x) (pushAnnotation ann y)
  Pretty.Cat x y         -> Pretty.Annotated ann $ Pretty.Cat (pushAnnotation ann x) (pushAnnotation ann y)
  Pretty.Nest i x        -> Pretty.Annotated ann $ Pretty.Nest i (pushAnnotation ann x)
  Pretty.Union x y       -> Pretty.Annotated ann $ Pretty.Union (pushAnnotation ann x) (pushAnnotation ann y)
  Pretty.Column f        -> Pretty.Annotated ann $ Pretty.Column (pushAnnotation ann . f)
  Pretty.WithPageWidth f -> Pretty.Annotated ann $ Pretty.WithPageWidth (pushAnnotation ann . f)
  Pretty.Nesting f       -> Pretty.Annotated ann $ Pretty.Nesting (pushAnnotation ann . f)
  Pretty.Annotated _ x   -> Pretty.Annotated ann (pushAnnotation ann x)


gatherAllFnArgsForDiff :: Type -> Type -> [(Type, Type)]
gatherAllFnArgsForDiff t1 t2 = case (t1, t2) of
  (TApp (TApp (TCon (TC "(->)" _) _) tl1) tr1, TApp (TApp (TCon (TC "(->)" _) _) tl2) tr2) ->
    (tl1, tl2) : gatherAllFnArgsForDiff tr1 tr2

  _ ->
    [(t1, t2)]


constructorAndFunctionArgsToDocsWithDiff :: Bool -> (M.Map String Int, M.Map String Int) -> (M.Map String Int, M.Map String Int) -> [(Type, Type)] -> ((M.Map String Int, M.Map String Int), (M.Map String Int, M.Map String Int), [Pretty.Doc Terminal.AnsiStyle], [Pretty.Doc Terminal.AnsiStyle])
constructorAndFunctionArgsToDocsWithDiff isFunctionArg vars1 vars2 ts = case ts of
  ((t1@(TApp _ _), t2@(TApp _ _)) : next) | not (isTuple t1 || isTuple t2) ->
    let (vars1', vars2', t1', t2')       = typesToDocWithDiff vars1 vars2 t1 t2
        (vars1'', vars2'', next1, next2) = constructorAndFunctionArgsToDocsWithDiff isFunctionArg vars1' vars2' next
    in  ( vars1''
        , vars2''
        , if isFunctionArg && not (isFunctionType t1) then
            t1' : next1
          else
            Pretty.group (Pretty.lparen <> Pretty.nest indentationSize (Pretty.line' <> t1') <> Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) Pretty.rparen) : next1
        , if isFunctionArg&& not (isFunctionType t2) then
            t2' : next2
          else
            Pretty.group (Pretty.lparen <> Pretty.nest indentationSize (Pretty.line' <> t2') <> Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) Pretty.rparen) : next2
        )

  ((t1@(TApp _ _), t2) : next) | not (isTuple t1) ->
    let (vars1', vars2', t1', t2')       = typesToDocWithDiff vars1 vars2 t1 t2
        (vars1'', vars2'', next1, next2) = constructorAndFunctionArgsToDocsWithDiff isFunctionArg vars1' vars2' next
    in  ( vars1''
        , vars2''
        , if isFunctionArg && not (isFunctionType t1) then
            t1' : next1
          else
            Pretty.group (Pretty.lparen <> Pretty.nest indentationSize (Pretty.line' <> t1') <> Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) Pretty.rparen) : next1
        , t2' : next2
        )

  ((t1, t2@(TApp _ _)) : next) | not (isTuple t2) ->
    let (vars1', vars2', t1', t2')       = typesToDocWithDiff vars1 vars2 t1 t2
        (vars1'', vars2'', next1, next2) = constructorAndFunctionArgsToDocsWithDiff isFunctionArg vars1' vars2' next
    in  ( vars1''
        , vars2''
        , t1' : next1
        , if isFunctionArg && not (isFunctionType t2) then
            t2' : next2
          else
            Pretty.group (Pretty.lparen <> Pretty.nest indentationSize (Pretty.line' <> t2') <> Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) Pretty.rparen) : next2
        )

  ((t1, t2) : next) ->
    let (vars1', vars2', t1', t2')       = typesToDocWithDiff vars1 vars2 t1 t2
        (vars1'', vars2'', next1, next2) = constructorAndFunctionArgsToDocsWithDiff isFunctionArg vars1' vars2' next
    in  (vars1'', vars2'', t1' : next1, t2' : next2)

  [] ->
    (vars1, vars2, [], [])


gatherAllConstructorArgsForDiff :: Type -> Type -> [(Type, Type)]
gatherAllConstructorArgsForDiff t1 t2 = case (t1, t2) of
  (TApp (TApp (TCon (TC "(->)" _) _) _) _, TApp (TApp (TCon (TC "(->)" _) _) _) _) ->
    [(t1, t2)]

  (TApp (TApp (TCon (TC "(->)" _) _) _) _, _) ->
    [(t1, t2)]

  (_, TApp (TApp (TCon (TC "(->)" _) _) _) _) ->
    [(t1, t2)]

  (TApp l1 r1, TApp l2 r2) ->
    gatherAllConstructorArgsForDiff l1 l2 ++ [(r1, r2)]

  _ ->
    [(t1, t2)]


predsToDocsWithDiff :: (M.Map String Int, M.Map String Int)
  -> (M.Map String Int, M.Map String Int)
  -> [Pred]
  -> [Pred]
  -> ((M.Map String Int, M.Map String Int), (M.Map String Int, M.Map String Int), [Pretty.Doc Terminal.AnsiStyle], [Pretty.Doc Terminal.AnsiStyle])
predsToDocsWithDiff (vars1, hkVars1) (vars2, hkVars2) ps1 ps2 = case (ps1, ps2) of
  (IsIn cls1 ts1 _ : more1, IsIn cls2 ts2 _ : more2) ->
    let (vars1', hkVars1', ts1')             = constructorAndFunctionArgsToDocs False (vars1, hkVars1) ts1
        (vars2', hkVars2', ts2')             = constructorAndFunctionArgsToDocs False (vars2, hkVars2) ts2
        (allVars1, allVars2, more1', more2') = predsToDocsWithDiff (vars1', hkVars1') (vars2', hkVars2') more1 more2
        areEqual = cls1 == cls2 && ts1 == ts2
    in  ( allVars1
        , allVars2
        , (
            Pretty.group (
              Pretty.nest indentationSize (
                Pretty.annotate (if areEqual then Terminal.color Terminal.Black else Terminal.color Terminal.Red) (Pretty.pretty cls1)
                <> Pretty.hcat ((Pretty.line <>) . (Pretty.annotate (if areEqual then Terminal.color Terminal.Black else Terminal.color Terminal.Red)) <$> ts1')
              )
              <> Pretty.line'
            )
          ) : more1'
        , (
            Pretty.group (
              Pretty.nest indentationSize (
                Pretty.annotate (if areEqual then Terminal.color Terminal.Black else Terminal.color Terminal.Green) (Pretty.pretty cls1)
                <> Pretty.hcat ((Pretty.line <>) . (Pretty.annotate (if areEqual then Terminal.color Terminal.Black else Terminal.color Terminal.Green)) <$> ts2')
              )
              <> Pretty.line'
            )
          ) : more2'
        )

  (IsIn cls1 ts1 _ : more1, []) ->
    let (vars1', hkVars1', ts1')             = constructorAndFunctionArgsToDocs False (vars1, hkVars1) ts1
        (allVars1, allVars2, more1', _) = predsToDocsWithDiff (vars1', hkVars1') (vars2, hkVars2) more1 []
    in  ( allVars1
        , allVars2
        , (
            Pretty.group (
              Pretty.nest indentationSize (
                Pretty.annotate (Terminal.color Terminal.Red) (Pretty.pretty cls1)
                <> Pretty.hcat ((Pretty.line <>) . (Pretty.annotate (Terminal.color Terminal.Red)) <$> ts1')
              )
              <> Pretty.line'
            )
          ) : more1'
        , []
        )

  ([], IsIn cls2 ts2 _ : more2) ->
    let (vars2', hkVars2', ts2')        = constructorAndFunctionArgsToDocs False (vars1, hkVars1) ts2
        (allVars1, allVars2, _, more2') = predsToDocsWithDiff (vars1, hkVars1) (vars2', hkVars2') more2 []
    in  ( allVars1
        , allVars2
        , []
        , (
            Pretty.group (
              Pretty.nest indentationSize (
                Pretty.annotate (Terminal.color Terminal.Green) (Pretty.pretty cls2)
                <> Pretty.hcat ((Pretty.line <>) . (Pretty.annotate (Terminal.color Terminal.Green)) <$> ts2')
              )
              <> Pretty.line'
            )
          ) : more2'
        )

  ([], []) ->
    ((vars1, hkVars1), (vars2, hkVars2), [], [])


schemesToDocWithDiff :: (M.Map String Int, M.Map String Int)
  -> (M.Map String Int, M.Map String Int)
  -> Scheme
  -> Scheme
  -> ((M.Map String Int, M.Map String Int), (M.Map String Int, M.Map String Int), Pretty.Doc Terminal.AnsiStyle, Pretty.Doc Terminal.AnsiStyle)
schemesToDocWithDiff (vars1, hkVars1) (vars2, hkVars2) sc1 sc2 = case (sc1, sc2) of
  (Forall _ ([] :=> t1), Forall _ ([] :=> t2)) ->
    typesToDocWithDiff (vars1, hkVars1) (vars2, hkVars2) t1 t2

  (Forall _ (ps1 :=> t1), Forall _ (ps2 :=> t2)) ->
    let (vars1', vars2', ps1', ps2')  = predsToDocsWithDiff (vars1, hkVars1) (vars2, hkVars2) (dedupePreds ps1) (dedupePreds ps2)
        (vars1'', vars2'', t1', t2') = typesToDocWithDiff vars1' vars2' t1 t2
    in
        ( vars1''
        , vars2''
        , if length ps1 > 1 then
            Pretty.group (
              Pretty.annotate (Terminal.color Terminal.Black) Pretty.lparen
              <> Pretty.hcat (List.intersperse (Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.space) ps1')
              <> Pretty.annotate (Terminal.color Terminal.Black) Pretty.rparen
              <> Pretty.nest indentationSize (
                    Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty " =>" <> Pretty.line)
                    <> t1'
                )
            )
          else
            Pretty.group (
              Pretty.hcat (List.intersperse (Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.space) ps1')
              <> Pretty.nest indentationSize (
                    Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty " =>" <> Pretty.line)
                    <> t1'
                )
            )
        , if length ps2 > 1 then
            Pretty.group (
              Pretty.annotate (Terminal.color Terminal.Black) Pretty.lparen
              <> Pretty.hcat (List.intersperse (Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.space) ps2')
              <> Pretty.annotate (Terminal.color Terminal.Black) Pretty.rparen
              <> Pretty.nest indentationSize (
                    Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty " =>" <> Pretty.line)
                    <> t2'
                )
            )
          else
            Pretty.group (
              Pretty.hcat (List.intersperse (Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.space) ps2')
              <> Pretty.nest indentationSize (
                    Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty " =>" <> Pretty.line)
                    <> t2'
                )
            )
        )


typesToDocWithDiff :: (M.Map String Int, M.Map String Int)
  -> (M.Map String Int, M.Map String Int)
  -> Type
  -> Type
  -> ((M.Map String Int, M.Map String Int), (M.Map String Int, M.Map String Int), Pretty.Doc Terminal.AnsiStyle, Pretty.Doc Terminal.AnsiStyle)
typesToDocWithDiff vars1 vars2 t1 t2 = case (t1, t2) of
  (TApp (TApp (TCon (TC "(->)" _) _) _) _, TApp (TApp (TCon (TC "(->)" _) _) _) _) ->
    let allArgs = gatherAllFnArgsForDiff t1 t2
        (vars1', vars2', ts1, ts2) = constructorAndFunctionArgsToDocsWithDiff True vars1 vars2 allArgs
    in  ( vars1'
        , vars2'
        , Pretty.group $ Pretty.hcat $ List.intersperse (Pretty.line <> Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty "-> ")) (Pretty.annotate (Terminal.color Terminal.Black) <$> ts1)
        , Pretty.group $ Pretty.hcat $ List.intersperse (Pretty.line <> Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty "-> ")) (Pretty.annotate (Terminal.color Terminal.Black) <$> ts2)
        )

  (TApp (TApp (TCon (TC "(->)" _) _) _) _, _) ->
    let (vars1', hkVars1', pretty1) = typeToDoc vars1 t1
        (vars2', hkVars2', pretty2) = typeToDoc vars2 t2
    in  ((vars1', hkVars1'), (vars2', hkVars2'), pushAnnotation (Terminal.color Terminal.Red <> Terminal.bold) pretty1, pushAnnotation (Terminal.color Terminal.Green <> Terminal.bold) pretty2)

  (_, TApp (TApp (TCon (TC "(->)" _) _) _) _) ->
    let (vars1', hkVars1', pretty1) = typeToDoc vars1 t1
        (vars2', hkVars2', pretty2) = typeToDoc vars2 t2
    in  ((vars1', hkVars1'), (vars2', hkVars2'), pushAnnotation (Terminal.color Terminal.Red <> Terminal.bold) pretty1, pushAnnotation (Terminal.color Terminal.Green <> Terminal.bold) pretty2)

  (TApp (TApp (TCon (TC "(,)" _) _) tl1) tr1, TApp (TApp (TCon (TC "(,)" _) _) tl2) tr2) ->
    let (vars1', vars2', tl1', tl2')   = typesToDocWithDiff vars1 vars2 tl1 tl2
        (vars1'', vars2'', tr1', tr2') = typesToDocWithDiff vars1' vars2' tr1 tr2
        openTuple  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.pretty "#[" <> Pretty.line'
        closeTuple = Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty "]")
        separator  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.line
    in  ( vars1''
        , vars2''
        , Pretty.group (
            Pretty.nest indentationSize (openTuple <> Pretty.annotate (Terminal.color Terminal.Black) tl1' <> separator <> Pretty.annotate (Terminal.color Terminal.Black) tr1')
            <> closeTuple
          )
        ,  Pretty.group (
            Pretty.nest indentationSize (openTuple <> Pretty.annotate (Terminal.color Terminal.Black) tl2' <> separator <> Pretty.annotate (Terminal.color Terminal.Black) tr2')
            <> closeTuple
          )
        )

  (TApp (TApp (TApp (TCon (TC "(,,)" _) _) t11) t12) t13, TApp (TApp (TApp (TCon (TC "(,,)" _) _) t21) t22) t23) ->
    let (vars1', vars2', t11', t21')     = typesToDocWithDiff vars1 vars2 t11 t21
        (vars1'', vars2'', t12', t22')   = typesToDocWithDiff vars1' vars2' t12 t22
        (vars1''', vars2''', t13', t23') = typesToDocWithDiff vars1'' vars2'' t13 t23
        openTuple  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.pretty "#[" <> Pretty.line'
        closeTuple = Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty "]")
        separator  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.line
    in  ( vars1'''
        , vars2'''
        , Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t11' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t12' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t13'
            )
            <> closeTuple
          )
        ,  Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t21' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t22' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t23'
            )
            <> closeTuple
          )
        )

  (TApp (TApp (TApp (TApp (TCon (TC "(,,,)" _) _) t11) t12) t13) t14, TApp (TApp (TApp (TApp (TCon (TC "(,,,)" _) _) t21) t22) t23) t24) ->
    let (vars1', vars2', t11', t21')       = typesToDocWithDiff vars1 vars2 t11 t21
        (vars1'', vars2'', t12', t22')     = typesToDocWithDiff vars1' vars2' t12 t22
        (vars1''', vars2''', t13', t23')   = typesToDocWithDiff vars1'' vars2'' t13 t23
        (vars1'''', vars2'''', t14', t24') = typesToDocWithDiff vars1''' vars2''' t14 t24
        openTuple  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.pretty "#[" <> Pretty.line'
        closeTuple = Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty "]")
        separator  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.line
    in  ( vars1''''
        , vars2''''
        , Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t11' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t12' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t13' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t14'
            )
            <> closeTuple
          )
        ,  Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t21' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t22' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t23' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t24'
            )
            <> closeTuple
          )
        )

  (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,)" _) _) t11) t12) t13) t14) t15, TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,)" _) _) t21) t22) t23) t24) t25) ->
    let (vars1', vars2', t11', t21')         = typesToDocWithDiff vars1 vars2 t11 t21
        (vars1'', vars2'', t12', t22')       = typesToDocWithDiff vars1' vars2' t12 t22
        (vars1''', vars2''', t13', t23')     = typesToDocWithDiff vars1'' vars2'' t13 t23
        (vars1'''', vars2'''', t14', t24')   = typesToDocWithDiff vars1''' vars2''' t14 t24
        (vars1''''', vars2''''', t15', t25') = typesToDocWithDiff vars1'''' vars2'''' t15 t25
        openTuple  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.pretty "#[" <> Pretty.line'
        closeTuple = Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty "]")
        separator  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.line
    in  ( vars1'''''
        , vars2'''''
        , Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t11' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t12' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t13' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t14' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t15'
            )
            <> closeTuple
          )
        ,  Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t21' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t22' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t23' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t24' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t25'
            )
            <> closeTuple
          )
        )

  (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,)" _) _) t11) t12) t13) t14) t15) t16, TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,)" _) _) t21) t22) t23) t24) t25) t26) ->
    let (vars1', vars2', t11', t21')           = typesToDocWithDiff vars1 vars2 t11 t21
        (vars1'', vars2'', t12', t22')         = typesToDocWithDiff vars1' vars2' t12 t22
        (vars1''', vars2''', t13', t23')       = typesToDocWithDiff vars1'' vars2'' t13 t23
        (vars1'''', vars2'''', t14', t24')     = typesToDocWithDiff vars1''' vars2''' t14 t24
        (vars1''''', vars2''''', t15', t25')   = typesToDocWithDiff vars1'''' vars2'''' t15 t25
        (vars1'''''', vars2'''''', t16', t26') = typesToDocWithDiff vars1''''' vars2''''' t16 t26
        openTuple  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.pretty "#[" <> Pretty.line'
        closeTuple = Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty "]")
        separator  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.line
    in  ( vars1''''''
        , vars2''''''
        , Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t11' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t12' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t13' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t14' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t15' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t16'
            )
            <> closeTuple
          )
        ,  Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t21' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t22' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t23' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t24' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t25' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t26'
            )
            <> closeTuple
          )
        )

  (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,)" _) _) t11) t12) t13) t14) t15) t16) t17, TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,)" _) _) t21) t22) t23) t24) t25) t26) t27) ->
    let (vars1', vars2', t11', t21')             = typesToDocWithDiff vars1 vars2 t11 t21
        (vars1'', vars2'', t12', t22')           = typesToDocWithDiff vars1' vars2' t12 t22
        (vars1''', vars2''', t13', t23')         = typesToDocWithDiff vars1'' vars2'' t13 t23
        (vars1'''', vars2'''', t14', t24')       = typesToDocWithDiff vars1''' vars2''' t14 t24
        (vars1''''', vars2''''', t15', t25')     = typesToDocWithDiff vars1'''' vars2'''' t15 t25
        (vars1'''''', vars2'''''', t16', t26')   = typesToDocWithDiff vars1''''' vars2''''' t16 t26
        (vars1''''''', vars2''''''', t17', t27') = typesToDocWithDiff vars1'''''' vars2'''''' t17 t27
        openTuple  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.pretty "#[" <> Pretty.line'
        closeTuple = Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty "]")
        separator  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.line
    in  ( vars1'''''''
        , vars2'''''''
        , Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t11' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t12' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t13' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t14' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t15' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t16' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t17'
            )
            <> closeTuple
          )
        ,  Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t21' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t22' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t23' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t24' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t25' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t26' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t27'
            )
            <> closeTuple
          )
        )

  (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,,)" _) _) t11) t12) t13) t14) t15) t16) t17) t18, TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,,)" _) _) t21) t22) t23) t24) t25) t26) t27) t28) ->
    let (vars1', vars2', t11', t21')               = typesToDocWithDiff vars1 vars2 t11 t21
        (vars1'', vars2'', t12', t22')             = typesToDocWithDiff vars1' vars2' t12 t22
        (vars1''', vars2''', t13', t23')           = typesToDocWithDiff vars1'' vars2'' t13 t23
        (vars1'''', vars2'''', t14', t24')         = typesToDocWithDiff vars1''' vars2''' t14 t24
        (vars1''''', vars2''''', t15', t25')       = typesToDocWithDiff vars1'''' vars2'''' t15 t25
        (vars1'''''', vars2'''''', t16', t26')     = typesToDocWithDiff vars1''''' vars2''''' t16 t26
        (vars1''''''', vars2''''''', t17', t27')   = typesToDocWithDiff vars1'''''' vars2'''''' t17 t27
        (vars1'''''''', vars2'''''''', t18', t28') = typesToDocWithDiff vars1''''''' vars2''''''' t18 t28
        openTuple  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.pretty "#[" <> Pretty.line'
        closeTuple = Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty "]")
        separator  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.line
    in  ( vars1''''''''
        , vars2''''''''
        , Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t11' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t12' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t13' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t14' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t15' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t16' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t17' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t18'
            )
            <> closeTuple
          )
        ,  Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t21' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t22' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t23' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t24' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t25' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t26' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t27' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t28'
            )
            <> closeTuple
          )
        )

  (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,,,)" _) _) t11) t12) t13) t14) t15) t16) t17) t18) t19, TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,,)" _) _) t21) t22) t23) t24) t25) t26) t27) t28) t29) ->
    let (vars1', vars2', t11', t21')                 = typesToDocWithDiff vars1 vars2 t11 t21
        (vars1'', vars2'', t12', t22')               = typesToDocWithDiff vars1' vars2' t12 t22
        (vars1''', vars2''', t13', t23')             = typesToDocWithDiff vars1'' vars2'' t13 t23
        (vars1'''', vars2'''', t14', t24')           = typesToDocWithDiff vars1''' vars2''' t14 t24
        (vars1''''', vars2''''', t15', t25')         = typesToDocWithDiff vars1'''' vars2'''' t15 t25
        (vars1'''''', vars2'''''', t16', t26')       = typesToDocWithDiff vars1''''' vars2''''' t16 t26
        (vars1''''''', vars2''''''', t17', t27')     = typesToDocWithDiff vars1'''''' vars2'''''' t17 t27
        (vars1'''''''', vars2'''''''', t18', t28')   = typesToDocWithDiff vars1''''''' vars2''''''' t18 t28
        (vars1''''''''', vars2''''''''', t19', t29') = typesToDocWithDiff vars1'''''''' vars2'''''''' t19 t29
        openTuple  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.pretty "#[" <> Pretty.line'
        closeTuple = Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty "]")
        separator  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.line
    in  ( vars1'''''''''
        , vars2'''''''''
        , Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t11' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t12' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t13' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t14' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t15' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t16' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t17' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t18' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t19'
            )
            <> closeTuple
          )
        ,  Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t21' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t22' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t23' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t24' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t25' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t26' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t27' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t28' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t29'
            )
            <> closeTuple
          )
        )

  (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,,,,)" _) _) t11) t12) t13) t14) t15) t16) t17) t18) t19) t110, TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,,,)" _) _) t21) t22) t23) t24) t25) t26) t27) t28) t29) t210) ->
    let (vars1', vars2', t11', t21')                     = typesToDocWithDiff vars1 vars2 t11 t21
        (vars1'', vars2'', t12', t22')                   = typesToDocWithDiff vars1' vars2' t12 t22
        (vars1''', vars2''', t13', t23')                 = typesToDocWithDiff vars1'' vars2'' t13 t23
        (vars1'''', vars2'''', t14', t24')               = typesToDocWithDiff vars1''' vars2''' t14 t24
        (vars1''''', vars2''''', t15', t25')             = typesToDocWithDiff vars1'''' vars2'''' t15 t25
        (vars1'''''', vars2'''''', t16', t26')           = typesToDocWithDiff vars1''''' vars2''''' t16 t26
        (vars1''''''', vars2''''''', t17', t27')         = typesToDocWithDiff vars1'''''' vars2'''''' t17 t27
        (vars1'''''''', vars2'''''''', t18', t28')       = typesToDocWithDiff vars1''''''' vars2''''''' t18 t28
        (vars1''''''''', vars2''''''''', t19', t29')     = typesToDocWithDiff vars1'''''''' vars2'''''''' t19 t29
        (vars1'''''''''', vars2'''''''''', t110', t210') = typesToDocWithDiff vars1''''''''' vars2''''''''' t110 t210
        openTuple  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.pretty "#[" <> Pretty.line'
        closeTuple = Pretty.line' <> Pretty.annotate (Terminal.color Terminal.Black) (Pretty.pretty "]")
        separator  = Pretty.annotate (Terminal.color Terminal.Black) $ Pretty.comma <> Pretty.line
    in  ( vars1''''''''''
        , vars2''''''''''
        , Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t11' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t12' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t13' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t14' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t15' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t16' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t17' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t18' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t19' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t110' 
            )
            <> closeTuple
          )
        ,  Pretty.group (
            Pretty.nest indentationSize (
              openTuple
              <> Pretty.annotate (Terminal.color Terminal.Black) t21' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t22' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t23' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t24' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t25' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t26' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t27' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t28' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t29' <> separator
              <> Pretty.annotate (Terminal.color Terminal.Black) t210' 
            )
            <> closeTuple
          )
        )

  (TApp _ _, TApp _ _) ->
    let allArgs = gatherAllConstructorArgsForDiff t1 t2
        (vars1', vars2', ctor1 : args1, ctor2 : args2) = constructorAndFunctionArgsToDocsWithDiff False vars1 vars2 allArgs
    in  ( vars1'
        , vars2'
        , Pretty.group (
            Pretty.annotate (Terminal.color Terminal.Black) ctor1
            <> Pretty.nest indentationSize
                (
                  Pretty.line <> Pretty.hcat (List.intersperse Pretty.line (Pretty.annotate (Terminal.color Terminal.Black) <$> args1))
                )
          )
        , Pretty.group (
            Pretty.annotate (Terminal.color Terminal.Black) ctor2
            <> Pretty.nest indentationSize
                (
                  Pretty.line <> Pretty.hcat (List.intersperse Pretty.line (Pretty.annotate (Terminal.color Terminal.Black) <$> args2))
                )
          )
        )

  (TRecord fields1 base1 _, TRecord fields2 base2 _) ->
    let allFields1 = case base1 of
          Just (TRecord fields _ _) ->
            fields <> fields1
          _ ->
            fields1
        allFields2 = case base2 of
          Just (TRecord fields _ _) ->
            fields <> fields2
          _ ->
            fields2
        ((finalVars1, finalHkVars1), (finalVars2, finalHkVars2), compiledFields1, compiledFields2) =
            foldl'
                (\(allVars1, allVars2, compiledFields1', compiledFields2') (fieldName, fieldType1) ->
                    case M.lookup fieldName allFields2 of
                      Just fieldType2 ->
                        let (allVars1', allVars2', pretty1, pretty2) = typesToDocWithDiff allVars1 allVars2 fieldType1 fieldType2
                        in  (allVars1', allVars2', compiledFields1' ++ [Pretty.pretty fieldName <> Pretty.pretty " :: " <> pretty1], compiledFields2' ++ [Pretty.pretty fieldName <> Pretty.pretty " :: " <> pretty2])

                      Nothing ->
                        let (vars1', hkVars1', pretty1) = typeToDoc allVars1 fieldType1
                        in  ((vars1', hkVars1'), allVars2, compiledFields1' ++ [Pretty.annotate (Terminal.color Terminal.Red) $ Pretty.pretty fieldName <> Pretty.pretty " :: " <> pretty1], compiledFields2')
                )
                (vars1, vars2, [], [])
              $ M.toList allFields1

        missingFields = allFields2 M.\\ allFields1
        ((finalVars2', finalHkVars2'), compiledMissingFields') =
            foldl'
                (\(allVars2, compiledFields2') (fieldName, fieldType2) ->
                    let (vars2', hkVars2', pretty2) = typeToDoc allVars2 fieldType2
                    in  ((vars2', hkVars2'), compiledFields2' ++ [pushAnnotation (Terminal.color Terminal.Green) $ Pretty.pretty fieldName <> Pretty.pretty " :: " <> pretty2])
                )
                ((finalVars2, finalHkVars2), [])
              $ M.toList missingFields
        (finalVars1', formattedBase1)   = case base1 of
          Just t1  ->
            let (vars, hkVars, pretty) = typeToDoc (finalVars1, finalHkVars1) t1
            in ((vars, hkVars), pushAnnotation (Terminal.color Terminal.Red) $ Pretty.pretty "..." <> pretty <> Pretty.comma <> Pretty.line)

          Nothing ->
            ((finalVars1, finalHkVars1), Pretty.emptyDoc)

        (finalVars2'', formattedBase2)   = case base2 of
          Just t2  ->
            let (vars, hkVars, pretty) = typeToDoc (finalVars2', finalHkVars2') t2
            in ((vars, hkVars), pushAnnotation (Terminal.color Terminal.Green) $ Pretty.pretty "..." <> pretty <> Pretty.comma <> Pretty.line)

          Nothing ->
            ((finalVars2', finalHkVars2'), Pretty.emptyDoc)

        compiled1 = Pretty.annotate (Terminal.color Terminal.Black) $
            Pretty.group (
              Pretty.lbrace <> Pretty.nest indentationSize (
                Pretty.line
                <> formattedBase1
                <> Pretty.hcat (List.intersperse (Pretty.comma <> Pretty.line) (Pretty.annotate (Terminal.color Terminal.Black) <$> compiledFields1))
              )
              <> (if null compiledFields1 then Pretty.emptyDoc else Pretty.line)
              <> Pretty.annotate (Terminal.color Terminal.Black) Pretty.rbrace
            )
        compiled2 = Pretty.annotate (Terminal.color Terminal.Black) $
            Pretty.group (
              Pretty.lbrace <> Pretty.nest indentationSize (
                Pretty.line
                <> formattedBase2
                <> Pretty.hcat (List.intersperse (Pretty.comma <> Pretty.line) (Pretty.annotate (Terminal.color Terminal.Black) <$> (compiledFields2 ++ compiledMissingFields')))
              )
              <> (if null compiledFields2 then Pretty.emptyDoc else Pretty.line)
              <> Pretty.annotate (Terminal.color Terminal.Black) Pretty.rbrace
            )
    in  (finalVars1', finalVars2'', compiled1, compiled2)

  (t1, t2) ->
    let (vars1', hkVars1', pretty1) = typeToDoc vars1 t1
        (vars2', hkVars2', pretty2) = typeToDoc vars2 t2
    in if t1 /= t2 then
      ((vars1', hkVars1'), (vars2', hkVars2'), pushAnnotation (Terminal.color Terminal.Red <> Terminal.bold) pretty1, pushAnnotation (Terminal.color Terminal.Green <> Terminal.bold) pretty2)
    else
      ((vars1', hkVars1'), (vars2', hkVars2'), pretty1, pretty2)



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
  TRecord fields base _ ->
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


gatherAllFnArgs :: Type -> [Type]
gatherAllFnArgs t = case t of
  TApp (TApp (TCon (TC "(->)" _) _) tl) tr ->
    tl : gatherAllFnArgs tr

  _ ->
    [t]


gatherAllConstructorArgs :: Type -> [Type]
gatherAllConstructorArgs t = case t of
  TApp (TApp (TCon (TC "(->)" _) _) _) _ ->
    [t]

  TApp l r ->
    gatherAllConstructorArgs l ++ [r]

  _ ->
    [t]


constructorAndFunctionArgsToDocs :: Bool -> (M.Map String Int, M.Map String Int) -> [Type] -> (M.Map String Int, M.Map String Int, [Pretty.Doc ann])
constructorAndFunctionArgsToDocs isFunctionArg (vars, hkVars) ts = case ts of
  (t@(TApp _ _) : next) | not (isTuple t) ->
    let (vars', hkVvars', t')     = typeToDoc (vars, hkVars) t
        (vars'', hkVars'', next') = constructorAndFunctionArgsToDocs isFunctionArg (vars', hkVvars') next
    in  ( vars'', hkVars''
        , if isFunctionArg && not (isFunctionType t) then
            t' : next'
          else
            Pretty.group (Pretty.lparen <> Pretty.nest indentationSize (Pretty.line' <> t') <> Pretty.line' <> Pretty.rparen) : next'
        )

  (t : next) ->
    let (vars', hkVars', t')      = typeToDoc (vars, hkVars) t
        (vars'', hkVars'', next') = constructorAndFunctionArgsToDocs isFunctionArg (vars', hkVars') next
    in  (vars'', hkVars'', t' : next')

  [] ->
    (vars, hkVars, [])



predsToDocs :: (M.Map String Int, M.Map String Int) -> [Pred] -> (M.Map String Int, M.Map String Int, [Pretty.Doc ann])
predsToDocs (vars, hkVars) ps = case ps of
  (IsIn cls ts _ : more) ->
    let (vars', hkVars', ts')     = constructorAndFunctionArgsToDocs False (vars, hkVars) ts
        (vars'', hkVars'', more') = predsToDocs (vars', hkVars') more
    in  ( vars''
        , hkVars''
        , (
            Pretty.group (
              Pretty.nest indentationSize (
                Pretty.pretty cls
                <> Pretty.hcat ((Pretty.line <>) <$> ts')
              )
              <> Pretty.line'
            )
          ) : more'
        )

  [] ->
    (vars, hkVars, [])


schemeToDoc :: (M.Map String Int, M.Map String Int) -> Scheme -> (M.Map String Int, M.Map String Int, Pretty.Doc ann)
schemeToDoc (vars, hkVars) sc = case sc of
  Forall _ ([] :=> t) ->
    typeToDoc (vars, hkVars) t

  Forall _ (ps :=> t) ->
    let (vars', hkVars', ps')  = predsToDocs (vars, hkVars) (dedupePreds ps)
        (vars'', hkVars'', t') = typeToDoc (vars', hkVars') t
    in
      if length ps > 1 then
        ( vars''
        , hkVars''
        , Pretty.lparen <> Pretty.hcat ps' <> Pretty.rparen <> Pretty.pretty " => " <> t'
        )
      else
        ( vars''
        , hkVars''
        , Pretty.hcat (List.intersperse (Pretty.comma <> Pretty.space) ps') <> Pretty.pretty " => " <> t'
        )


typeToDoc :: (M.Map String Int, M.Map String Int) -> Type -> (M.Map String Int, M.Map String Int, Pretty.Doc ann)
typeToDoc (vars, hkVars) t = case t of
  TCon (TC n _) _ ->
    (vars, hkVars, Pretty.pretty n)

  TVar (TV n k)   ->
    case k of
      Star -> case M.lookup n vars of
        Just x ->
          (vars, hkVars, Pretty.pretty [letters !! x])

        Nothing ->
          let newIndex = M.size vars
          in  (M.insert n newIndex vars, hkVars, Pretty.pretty [letters !! newIndex])

      Kfun _ _ -> case M.lookup n hkVars of
        Just x ->
          (vars, hkVars, Pretty.pretty [hkLetters !! x])

        Nothing ->
          let newIndex = M.size hkVars
          in  (vars, M.insert n newIndex hkVars, Pretty.pretty [hkLetters !! newIndex])

  TApp (TApp (TCon (TC "(,)" _) _) tl) tr ->
    let (varsLeft , hkVarsLeft , left ) = typeToDoc (vars, hkVars) tl
        (varsRight, hkVarsRight, right) = typeToDoc (varsLeft, hkVarsLeft) tr
    in  ( varsRight
        , hkVarsRight
        , Pretty.group (
            Pretty.nest indentationSize (
              Pretty.pretty "#[" <> Pretty.line' <> left <> Pretty.comma <> Pretty.line <> right
            )
            <> Pretty.line' <> Pretty.pretty "]"
          )
        )

  TApp (TApp (TApp (TCon (TC "(,,)" _) _) t1) t2) t3 ->
    let (vars' , hkVars' , t1')   = typeToDoc (vars, hkVars) t1
        (vars'', hkVars'', t2')   = typeToDoc (vars', hkVars') t2
        (vars''', hkVars''', t3') = typeToDoc (vars'', hkVars'') t3
    in  ( vars'''
        , hkVars'''
        , Pretty.group (
            Pretty.nest indentationSize (
              Pretty.pretty "#[" <> Pretty.line' <> t1' <> Pretty.comma <> Pretty.line <> t2' <> Pretty.comma <> Pretty.line <> t3'
            )
            <> Pretty.line' <> Pretty.pretty "]"
          )
        )

  TApp (TApp (TApp (TApp (TCon (TC "(,,,)" _) _) t1) t2) t3) t4 ->
    let (vars' , hkVars' , t1')   = typeToDoc (vars, hkVars) t1
        (vars'', hkVars'', t2')   = typeToDoc (vars', hkVars') t2
        (vars''', hkVars''', t3') = typeToDoc (vars'', hkVars'') t3
        (vars'''', hkVars'''', t4') = typeToDoc (vars''', hkVars''') t4
    in  ( vars''''
        , hkVars''''
        , Pretty.group (
            Pretty.nest indentationSize (
              Pretty.pretty "#[" <> Pretty.line' <> t1' <> Pretty.comma <> Pretty.line <> t2' <> Pretty.comma <> Pretty.line <> t3' <> Pretty.comma <> Pretty.line <> t4'
            )
            <> Pretty.line' <> Pretty.pretty "]"
          )
        )

  TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,)" _) _) t1) t2) t3) t4) t5 ->
    let (vars' , hkVars' , t1')   = typeToDoc (vars, hkVars) t1
        (vars'', hkVars'', t2')   = typeToDoc (vars', hkVars') t2
        (vars''', hkVars''', t3') = typeToDoc (vars'', hkVars'') t3
        (vars'''', hkVars'''', t4') = typeToDoc (vars''', hkVars''') t4
        (vars''''', hkVars''''', t5') = typeToDoc (vars'''', hkVars'''') t5
    in  ( vars'''''
        , hkVars'''''
        , Pretty.group (
            Pretty.nest indentationSize (
              Pretty.pretty "#["
              <> Pretty.line' <> t1' <> Pretty.comma
              <> Pretty.line <> t2' <> Pretty.comma
              <> Pretty.line <> t3' <> Pretty.comma
              <> Pretty.line <> t4' <> Pretty.comma
              <> Pretty.line <> t5'
            )
            <> Pretty.line' <> Pretty.pretty "]"
          )
        )

  TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,)" _) _) t1) t2) t3) t4) t5) t6 ->
    let (vars' , hkVars' , t1')         = typeToDoc (vars, hkVars) t1
        (vars'', hkVars'', t2')         = typeToDoc (vars', hkVars') t2
        (vars''', hkVars''', t3')       = typeToDoc (vars'', hkVars'') t3
        (vars'''', hkVars'''', t4')     = typeToDoc (vars''', hkVars''') t4
        (vars''''', hkVars''''', t5')   = typeToDoc (vars'''', hkVars'''') t5
        (vars'''''', hkVars'''''', t6') = typeToDoc (vars''''', hkVars''''') t6
    in  ( vars''''''
        , hkVars''''''
        , Pretty.group (
            Pretty.nest indentationSize (
              Pretty.pretty "#["
              <> Pretty.line' <> t1' <> Pretty.comma
              <> Pretty.line <> t2' <> Pretty.comma
              <> Pretty.line <> t3' <> Pretty.comma
              <> Pretty.line <> t4' <> Pretty.comma
              <> Pretty.line <> t5' <> Pretty.comma
              <> Pretty.line <> t6'
            )
            <> Pretty.line' <> Pretty.pretty "]"
          )
        )

  TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,)" _) _) t1) t2) t3) t4) t5) t6) t7 ->
    let (vars' , hkVars' , t1')           = typeToDoc (vars, hkVars) t1
        (vars'', hkVars'', t2')           = typeToDoc (vars', hkVars') t2
        (vars''', hkVars''', t3')         = typeToDoc (vars'', hkVars'') t3
        (vars'''', hkVars'''', t4')       = typeToDoc (vars''', hkVars''') t4
        (vars''''', hkVars''''', t5')     = typeToDoc (vars'''', hkVars'''') t5
        (vars'''''', hkVars'''''', t6')   = typeToDoc (vars''''', hkVars''''') t6
        (vars''''''', hkVars''''''', t7') = typeToDoc (vars'''''', hkVars'''''') t7
    in  ( vars'''''''
        , hkVars'''''''
        , Pretty.group (
            Pretty.nest indentationSize (
              Pretty.pretty "#["
              <> Pretty.line' <> t1' <> Pretty.comma
              <> Pretty.line <> t2' <> Pretty.comma
              <> Pretty.line <> t3' <> Pretty.comma
              <> Pretty.line <> t4' <> Pretty.comma
              <> Pretty.line <> t5' <> Pretty.comma
              <> Pretty.line <> t6' <> Pretty.comma
              <> Pretty.line <> t7'
            )
            <> Pretty.line' <> Pretty.pretty "]"
          )
        )

  TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,)" _) _) t1) t2) t3) t4) t5) t6) t7) t8 ->
    let (vars' , hkVars' , t1')             = typeToDoc (vars, hkVars) t1
        (vars'', hkVars'', t2')             = typeToDoc (vars', hkVars') t2
        (vars''', hkVars''', t3')           = typeToDoc (vars'', hkVars'') t3
        (vars'''', hkVars'''', t4')         = typeToDoc (vars''', hkVars''') t4
        (vars''''', hkVars''''', t5')       = typeToDoc (vars'''', hkVars'''') t5
        (vars'''''', hkVars'''''', t6')     = typeToDoc (vars''''', hkVars''''') t6
        (vars''''''', hkVars''''''', t7')   = typeToDoc (vars'''''', hkVars'''''') t7
        (vars'''''''', hkVars'''''''', t8') = typeToDoc (vars''''''', hkVars''''''') t8
    in  ( vars''''''''
        , hkVars''''''''
        , Pretty.group (
            Pretty.nest indentationSize (
              Pretty.pretty "#["
              <> Pretty.line' <> t1' <> Pretty.comma
              <> Pretty.line <> t2' <> Pretty.comma
              <> Pretty.line <> t3' <> Pretty.comma
              <> Pretty.line <> t4' <> Pretty.comma
              <> Pretty.line <> t5' <> Pretty.comma
              <> Pretty.line <> t6' <> Pretty.comma
              <> Pretty.line <> t7' <> Pretty.comma
              <> Pretty.line <> t8'
            )
            <> Pretty.line' <> Pretty.pretty "]"
          )
        )

  TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,)" _) _) t1) t2) t3) t4) t5) t6) t7) t8) t9 ->
    let (vars' , hkVars' , t1')               = typeToDoc (vars, hkVars) t1
        (vars'', hkVars'', t2')               = typeToDoc (vars', hkVars') t2
        (vars''', hkVars''', t3')             = typeToDoc (vars'', hkVars'') t3
        (vars'''', hkVars'''', t4')           = typeToDoc (vars''', hkVars''') t4
        (vars''''', hkVars''''', t5')         = typeToDoc (vars'''', hkVars'''') t5
        (vars'''''', hkVars'''''', t6')       = typeToDoc (vars''''', hkVars''''') t6
        (vars''''''', hkVars''''''', t7')     = typeToDoc (vars'''''', hkVars'''''') t7
        (vars'''''''', hkVars'''''''', t8')   = typeToDoc (vars''''''', hkVars''''''') t8
        (vars''''''''', hkVars''''''''', t9') = typeToDoc (vars'''''''', hkVars'''''''') t9
    in  ( vars'''''''''
        , hkVars'''''''''
        , Pretty.group (
            Pretty.nest indentationSize (
              Pretty.pretty "#["
              <> Pretty.line' <> t1' <> Pretty.comma
              <> Pretty.line <> t2' <> Pretty.comma
              <> Pretty.line <> t3' <> Pretty.comma
              <> Pretty.line <> t4' <> Pretty.comma
              <> Pretty.line <> t5' <> Pretty.comma
              <> Pretty.line <> t6' <> Pretty.comma
              <> Pretty.line <> t7' <> Pretty.comma
              <> Pretty.line <> t8' <> Pretty.comma
              <> Pretty.line <> t9'
            )
            <> Pretty.line' <> Pretty.pretty "]"
          )
        )

  TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TApp (TCon (TC "(,,,,,,)" _) _) t1) t2) t3) t4) t5) t6) t7) t8) t9) t10 ->
    let (vars' , hkVars' , t1')                  = typeToDoc (vars, hkVars) t1
        (vars'', hkVars'', t2')                  = typeToDoc (vars', hkVars') t2
        (vars''', hkVars''', t3')                = typeToDoc (vars'', hkVars'') t3
        (vars'''', hkVars'''', t4')              = typeToDoc (vars''', hkVars''') t4
        (vars''''', hkVars''''', t5')            = typeToDoc (vars'''', hkVars'''') t5
        (vars'''''', hkVars'''''', t6')          = typeToDoc (vars''''', hkVars''''') t6
        (vars''''''', hkVars''''''', t7')        = typeToDoc (vars'''''', hkVars'''''') t7
        (vars'''''''', hkVars'''''''', t8')      = typeToDoc (vars''''''', hkVars''''''') t8
        (vars''''''''', hkVars''''''''', t9')    = typeToDoc (vars'''''''', hkVars'''''''') t9
        (vars'''''''''', hkVars'''''''''', t10') = typeToDoc (vars''''''''', hkVars''''''''') t10
    in  ( vars''''''''''
        , hkVars''''''''''
        , Pretty.group (
            Pretty.nest indentationSize (
              Pretty.pretty "#["
              <> Pretty.line' <> t1' <> Pretty.comma
              <> Pretty.line <> t2' <> Pretty.comma
              <> Pretty.line <> t3' <> Pretty.comma
              <> Pretty.line <> t4' <> Pretty.comma
              <> Pretty.line <> t5' <> Pretty.comma
              <> Pretty.line <> t6' <> Pretty.comma
              <> Pretty.line <> t7' <> Pretty.comma
              <> Pretty.line <> t8' <> Pretty.comma
              <> Pretty.line <> t9' <> Pretty.comma
              <> Pretty.line <> t10'
            )
            <> Pretty.line' <> Pretty.pretty "]"
          )
        )

  (TApp (TApp (TCon (TC "(->)" _) _) _) _) ->
    let allArgs = gatherAllFnArgs t
        (vars', hkVars', args) = constructorAndFunctionArgsToDocs True (vars, hkVars) allArgs
    in  ( vars'
        , hkVars'
        , Pretty.hcat $ List.intersperse (Pretty.softline <> Pretty.pretty "-> ") args
        )

  (TApp _ _) ->
    let allArgs = gatherAllConstructorArgs t
        (vars', hkVars', ctor : args) = constructorAndFunctionArgsToDocs False (vars, hkVars) allArgs
    in  ( vars'
        , hkVars'
        , Pretty.group (
            ctor
            <> Pretty.nest indentationSize
                (
                  Pretty.line <> Pretty.hcat (List.intersperse Pretty.line args)
                )
          )
        )

  TRecord fields base _ ->
    let (finalVars, finalHkVars, compiledFields) =
            foldl'
                (\(vars', hkVars', compiledFields') (fieldName, fieldType) ->
                  let (vars'', hkVars'', compiledField) = typeToDoc (vars', hkVars') fieldType
                  in  (vars'', hkVars'', compiledFields' ++ [(fieldName, compiledField)])
                )
                (vars, hkVars, [])
              $ M.toList fields
        compiledFields' = (\(fieldName, fieldType) -> Pretty.pretty fieldName <> Pretty.pretty " :: " <> fieldType) <$> compiledFields
        formattedBase   = case base of
          Just _  -> Pretty.pretty "...base," <> Pretty.line
          Nothing -> Pretty.emptyDoc
        compiled = Pretty.group (
            Pretty.lbrace <> Pretty.nest indentationSize (
              Pretty.line
              <> formattedBase
              <> Pretty.hcat (List.intersperse (Pretty.comma <> Pretty.line) compiledFields')
            )
            <> Pretty.line
            <> Pretty.rbrace
          )
    in  (finalVars, finalHkVars, compiled)

  TGen n ->
    case M.lookup ("T" <> show n) vars of
      Just x  ->
        (vars, hkVars, Pretty.pretty [letters !! x])

      Nothing ->
        let newIndex = M.size vars
        in  (M.insert ("T" <> show n) newIndex vars, hkVars, Pretty.pretty [letters !! newIndex])

  _ ->
    (vars, hkVars, Pretty.emptyDoc)


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
prettyPrintTyping' _ Slv.Typed{} = undefined
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
isTRArrOrTRCompWithArgs (Slv.Typed _ _ _) = undefined
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
