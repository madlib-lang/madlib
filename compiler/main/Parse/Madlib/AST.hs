{-# LANGUAGE ConstrainedClassMethods   #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Parse.Madlib.AST where

import qualified Data.Map                         as M
import           Control.Exception                ( IOException
                                                  , try, SomeException (SomeException)
                                                  )
import qualified Data.ByteString                  as BS
import           Parse.Megaparsec.Madlib          ( parseWithStructuredError, parseWithStructuredErrorBS, parseWithRecoveryBS, ParseError(..) )
import           Parse.Megaparsec.Common          ( ParseRecoveryError(..) )
import           AST.Source 
import           Utils.Path                       ( resolveAbsoluteSrcPath )
import           Utils.PathUtils
import           Error.Error
import           Error.Context
import           Control.Monad.Except
import           System.FilePath                  ( dropFileName, takeExtension, normalise, takeBaseName )
import qualified Prelude                          as P
import           Prelude                          hiding ( readFile )
import qualified System.Environment.Executable    as E
import           Explain.Location
import           Data.List
import           Parse.Madlib.TargetMacro
import           Run.Options
import           Text.Show.Pretty (ppShow)
import           Data.Either (isLeft)



isJsonImport :: Import -> Bool
isJsonImport imp = case imp of
  (Source _ _ (DefaultImport _ _ absPath)) ->
    takeExtension absPath == ".json"

  _ ->
    False


processJsonImports :: Options -> AST -> IO AST
processJsonImports options ast@AST{ aimports } = do
  let (jsonImports, madImports) = partition isJsonImport aimports
  jsonAssignments <- generateJsonAssignments (optPathUtils options) jsonImports
  return ast { aimports = madImports, aexps = jsonAssignments ++ aexps ast }


escapeJSONString :: String -> String
escapeJSONString s = case s of
  '\n':r -> "\\n" ++ escapeJSONString r
  '\t':r -> "\\t" ++ escapeJSONString r
  '\\':r -> "\\\\" ++ escapeJSONString r
  '\"':r -> "\\\"" ++ escapeJSONString r
  '\'':r -> "\\\'" ++ escapeJSONString r
  x:r    -> x : escapeJSONString r
  _      -> ""


generateJsonAssignments :: PathUtils -> [Import] -> IO [Exp]
generateJsonAssignments _ [] = return []
generateJsonAssignments pathUtils ((Source area sourceTarget (DefaultImport (Source _ _ name) _ absPath)):imps) = do
  next <- generateJsonAssignments pathUtils imps
  jsonContent <- readFile pathUtils absPath
  let var = Source area sourceTarget (LStr $ escapeJSONString jsonContent)
  let assignment = Source area sourceTarget (Assignment name var)

  return $ assignment : next


-- | Check if an AST uses ?? (MaybeDefault), ?. (OptionalAccess), or JSX tags
-- (which may need Nothing for missing Maybe-typed props)
usesMaybeOperators :: AST -> Bool
usesMaybeOperators ast = any expUsesMaybe (aexps ast)
  where
    expUsesMaybe :: Source Exp_ -> Bool
    expUsesMaybe (Source _ _ e) = expContentUsesMaybe e

    expContentUsesMaybe :: Exp_ -> Bool
    expContentUsesMaybe e = case e of
      MaybeDefault _ _    -> True
      OptionalAccess _ _  -> True
      JsxTag _ _ _        -> True
      JsxAutoClosedTag _ _ -> True
      App fn args         -> expUsesMaybe fn || any expUsesMaybe args
      BinOp l op r        -> expUsesMaybe l || expUsesMaybe op || expUsesMaybe r
      UnOp op arg         -> expUsesMaybe op || expUsesMaybe arg
      Abs _ body          -> any expUsesMaybe body
      AbsWithMultilineBody _ body -> any expUsesMaybe body
      Access rec _        -> expUsesMaybe rec
      ArrayAccess arr idx -> expUsesMaybe arr || expUsesMaybe idx
      Assignment _ e'     -> expUsesMaybe e'
      Mutate _ e'         -> expUsesMaybe e'
      If c t f            -> expUsesMaybe c || expUsesMaybe t || expUsesMaybe f
      Ternary c t f       -> expUsesMaybe c || expUsesMaybe t || expUsesMaybe f
      Where e' iss        -> expUsesMaybe e' || any isUsesMaybe iss
      Do exps             -> any expUsesMaybe exps
      Export e'           -> expUsesMaybe e'
      Return e'           -> expUsesMaybe e'
      ListConstructor lis -> any liUsesMaybe lis
      TupleConstructor es -> any expUsesMaybe es
      Record fields       -> any fieldUsesMaybe fields
      TemplateString es   -> any expUsesMaybe es
      TypedExp e' _       -> expUsesMaybe e'
      _                   -> False

    isUsesMaybe (Source _ _ (Is _ e)) = expUsesMaybe e
    isUsesMaybe _ = False

    liUsesMaybe (Source _ _ (ListItem e)) = expUsesMaybe e
    liUsesMaybe (Source _ _ (ListSpread e)) = expUsesMaybe e
    liUsesMaybe _ = False

    fieldUsesMaybe (Source _ _ (Field (_, e))) = expUsesMaybe e
    fieldUsesMaybe (Source _ _ (FieldSpread e)) = expUsesMaybe e
    fieldUsesMaybe _ = False


validatePreludePrivateModules :: Options -> AST -> IO (Either CompilationError AST)
validatePreludePrivateModules options ast@AST{ aimports, apath = Just path } = do
  if "prelude/__internal__" `isInfixOf` path || "prelude\\__internal__" `isInfixOf` path then
    return $ Right ast
  else do
    updatedImports <- computeAbsoluteImportPaths (optPathUtils options) (not $ optParseOnly options) path (optRootPath options) aimports

    case updatedImports of
      Right updatedImports' -> do
        let processed = foldl
              (\res imp ->
                if isLeft res then
                  res
                else
                  let importPath = getImportAbsolutePath imp
                  in  if not ("prelude/__internal__" `isInfixOf` path) && not ("prelude\\__internal__" `isInfixOf` path) then
                        let fileName = takeBaseName importPath
                        in  if "__" `isPrefixOf` fileName && "__" `isSuffixOf` fileName then
                          Left $ CompilationError (ImportNotFound importPath) (Context path (getArea imp))
                        else
                          Right ()
                      else
                        Right ()
              )
              (Right ())
              updatedImports'

        return $ ast <$ processed

      Left err ->
        return $Left err


buildAST :: Options -> FilePath -> String -> IO (Either CompilationError AST)
buildAST options path code = case parseWithStructuredError code of
  Right ast -> do
    let astWithPath = setPath ast path
    validatedImports <- validatePreludePrivateModules options astWithPath
    case validatedImports of
      Left err ->
        return $ Left err

      Right _ -> do
        let astWithProcessedMacros = resolveMacros (optTarget options) astWithPath
        let builtinsImport = Source emptyArea TargetAll $ DefaultImport (Source emptyArea TargetAll "__BUILTINS__") "__BUILTINS__" "__BUILTINS__"
        let builtinsDictTypeImport = Source emptyArea TargetAll $ TypeImport [Source emptyArea TargetAll "Dictionary"] "__BUILTINS__" "__BUILTINS__"
        let maybeTypeImport = Source emptyArea TargetAll $ TypeImport [Source emptyArea TargetAll "Maybe"] "Maybe" "Maybe"
        let maybeNameImport = Source emptyArea TargetAll $ NamedImport [Source emptyArea TargetAll "Just", Source emptyArea TargetAll "Nothing"] "Maybe" "Maybe"
        let isBuiltinsFile = "__BUILTINS__.mad" `isSuffixOf` path
        let isPreludeFile = "/__internal__/" `isInfixOf` path
        let hasBuiltinsImport = any ((== "__BUILTINS__") . snd . getImportPath) (aimports astWithProcessedMacros)
        -- Check if Just/Nothing are already imported (not just the Maybe type)
        let hasJustImport = any (importsJustConstructor . getSourceContent) (aimports astWithProcessedMacros)
            importsJustConstructor (NamedImport names _ _) = any ((== "Just") . getSourceContent) names
            importsJustConstructor (DefaultImport _ _ alias) = alias == "Maybe"
            importsJustConstructor _ = False
        let withBuiltins =
              if isBuiltinsFile || hasBuiltinsImport then
                astWithProcessedMacros
              else
                astWithProcessedMacros { aimports = builtinsDictTypeImport : builtinsImport : aimports astWithProcessedMacros }
        -- Auto-import Maybe type only when ?? or ?. operators are used
        let needsMaybeImport = not isPreludeFile && not hasJustImport && usesMaybeOperators withBuiltins
        let astWithAllImports =
              if needsMaybeImport then
                withBuiltins { aimports = maybeNameImport : aimports withBuiltins }
              else
                withBuiltins
        astWithAbsoluteImportPaths <- computeAbsoluteImportPathsForAST (optPathUtils options) (not $ optParseOnly options) (optRootPath options) astWithAllImports
        case astWithAbsoluteImportPaths of
          Right astWithAbsoluteImportPaths' -> do
            astWithJsonAssignments     <- processJsonImports options astWithAbsoluteImportPaths'
            return $ Right astWithJsonAssignments

          Left _ ->
            return astWithAbsoluteImportPaths

  Left parseErr -> return $ Left (parseErrorToCompilationError path parseErr)


-- | Build AST directly from a ByteString (avoids String intermediary)
buildASTFromBS :: Options -> FilePath -> BS.ByteString -> IO (Either CompilationError AST)
buildASTFromBS options path bs = case parseWithStructuredErrorBS bs of
  Right ast -> do
    let astWithPath = setPath ast path
    validatedImports <- validatePreludePrivateModules options astWithPath
    case validatedImports of
      Left err ->
        return $ Left err

      Right _ -> do
        let astWithProcessedMacros = resolveMacros (optTarget options) astWithPath
        let builtinsImport = Source emptyArea TargetAll $ DefaultImport (Source emptyArea TargetAll "__BUILTINS__") "__BUILTINS__" "__BUILTINS__"
        let builtinsDictTypeImport = Source emptyArea TargetAll $ TypeImport [Source emptyArea TargetAll "Dictionary"] "__BUILTINS__" "__BUILTINS__"
        let maybeTypeImport = Source emptyArea TargetAll $ TypeImport [Source emptyArea TargetAll "Maybe"] "Maybe" "Maybe"
        let maybeNameImport = Source emptyArea TargetAll $ NamedImport [Source emptyArea TargetAll "Just", Source emptyArea TargetAll "Nothing"] "Maybe" "Maybe"
        let isBuiltinsFile = "__BUILTINS__.mad" `isSuffixOf` path
        let isPreludeFile = "/__internal__/" `isInfixOf` path
        let hasBuiltinsImport = any ((== "__BUILTINS__") . snd . getImportPath) (aimports astWithProcessedMacros)
        -- Check if Just/Nothing are already imported (not just the Maybe type)
        let hasJustImport = any (importsJustConstructor . getSourceContent) (aimports astWithProcessedMacros)
            importsJustConstructor (NamedImport names _ _) = any ((== "Just") . getSourceContent) names
            importsJustConstructor (DefaultImport _ _ alias) = alias == "Maybe"
            importsJustConstructor _ = False
        let withBuiltins =
              if isBuiltinsFile || hasBuiltinsImport then
                astWithProcessedMacros
              else
                astWithProcessedMacros { aimports = builtinsDictTypeImport : builtinsImport : aimports astWithProcessedMacros }
        -- Auto-import Maybe type only when ?? or ?. operators are used
        let needsMaybeImport = not isPreludeFile && not hasJustImport && usesMaybeOperators withBuiltins
        let astWithAllImports =
              if needsMaybeImport then
                withBuiltins { aimports = maybeNameImport : aimports withBuiltins }
              else
                withBuiltins
        astWithAbsoluteImportPaths <- computeAbsoluteImportPathsForAST (optPathUtils options) (not $ optParseOnly options) (optRootPath options) astWithAllImports
        case astWithAbsoluteImportPaths of
          Right astWithAbsoluteImportPaths' -> do
            astWithJsonAssignments     <- processJsonImports options astWithAbsoluteImportPaths'
            return $ Right astWithJsonAssignments

          Left _ ->
            return astWithAbsoluteImportPaths

  Left parseErr -> return $ Left (parseErrorToCompilationError path parseErr)


-- | Build AST from ByteString with error recovery for LSP mode.
-- Returns a partial AST (with valid declarations) plus recovery errors as CompilationErrors.
buildASTFromBSWithRecovery :: Options -> FilePath -> BS.ByteString -> IO (Either CompilationError AST, [CompilationError])
buildASTFromBSWithRecovery options path bs =
  let (parseResult, recoveryErrors) = parseWithRecoveryBS bs
      recoveryCompErrors = map (\(ParseRecoveryError l c l' c' msg) ->
        CompilationError (GrammarError path msg)
          (Context path (Area (Loc 0 l c) (Loc 0 l' c'))))
        recoveryErrors
  in  case parseResult of
        Right ast -> do
          result <- postProcessParsedAST options path ast
          return (result, recoveryCompErrors)
        Left parseErr ->
          return (Left $ parseErrorToCompilationError path parseErr, recoveryCompErrors)


-- | Shared post-parse AST processing (macros, builtins, import paths, JSON)
postProcessParsedAST :: Options -> FilePath -> AST -> IO (Either CompilationError AST)
postProcessParsedAST options path ast = do
  let astWithPath = setPath ast path
  validatedImports <- validatePreludePrivateModules options astWithPath
  case validatedImports of
    Left err ->
      return $ Left err

    Right _ -> do
      let astWithProcessedMacros = resolveMacros (optTarget options) astWithPath
      let builtinsImport = Source emptyArea TargetAll $ DefaultImport (Source emptyArea TargetAll "__BUILTINS__") "__BUILTINS__" "__BUILTINS__"
      let builtinsDictTypeImport = Source emptyArea TargetAll $ TypeImport [Source emptyArea TargetAll "Dictionary"] "__BUILTINS__" "__BUILTINS__"
      let astWithBuiltinsImport =
            if "__BUILTINS__.mad" `isSuffixOf` path || any ((== "__BUILTINS__") . snd . getImportPath) (aimports astWithProcessedMacros) then
              astWithProcessedMacros
            else
              astWithProcessedMacros { aimports = builtinsDictTypeImport : builtinsImport : aimports astWithProcessedMacros }
      astWithAbsoluteImportPaths <- computeAbsoluteImportPathsForAST (optPathUtils options) (not $ optParseOnly options) (optRootPath options) astWithBuiltinsImport
      case astWithAbsoluteImportPaths of
        Right astWithAbsoluteImportPaths' -> do
          astWithJsonAssignments <- processJsonImports options astWithAbsoluteImportPaths'
          return $ Right astWithJsonAssignments

        Left _ ->
          return astWithAbsoluteImportPaths


parseErrorToCompilationError :: FilePath -> ParseError -> CompilationError
parseErrorToCompilationError path parseErr = case parseErr of
  ParseBadEscape area ->
    CompilationError BadEscapeSequence (Context path area)
  ParseEmptyChar area ->
    CompilationError EmptyChar (Context path area)
  ParseSyntaxError line col msg ->
    CompilationError (GrammarError path msg) (Context path (Area (Loc 0 line col) (Loc 0 line (col + 1))))
  ParseLexError msg ->
    CompilationError (GrammarError path msg) (Context path (Area (Loc 0 1 1) (Loc 0 1 2)))


setPath :: AST -> FilePath -> AST
setPath ast path = ast { apath = Just path }


computeAbsoluteImportPath :: PathUtils -> FilePath -> Import -> IO (Maybe Import)
computeAbsoluteImportPath pathUtils rootPath (Source area target imp) = case imp of
  NamedImport names rel _ -> do
    abs <- resolveAbsoluteSrcPath pathUtils rootPath rel
    return $ Source area target . NamedImport names rel <$> abs

  TypeImport names rel _ -> do
    abs <- resolveAbsoluteSrcPath pathUtils rootPath rel
    return $ Source area target . TypeImport names rel <$> abs

  DefaultImport namespace rel _ -> do
    abs <- resolveAbsoluteSrcPath pathUtils rootPath rel
    return $ Source area target . DefaultImport namespace rel <$> abs


computeAbsoluteImportPaths :: PathUtils -> Bool -> FilePath -> FilePath -> [Import] -> IO (Either CompilationError [Import])
computeAbsoluteImportPaths pathUtils mustExist astPath rootPath imps = case imps of
  imp : next -> do
    imp' <- computeAbsoluteImportPath pathUtils (dropFileName astPath) imp
    case imp' of
      Nothing ->
        if mustExist then
          return
            $ Left
            $ CompilationError
                (ImportNotFound $ snd $ getImportPath imp)
                (Context astPath (getArea imp))
        else do
          next' <- computeAbsoluteImportPaths pathUtils mustExist astPath rootPath next
          return $ (imp :) <$> next'

      Just good -> do
        next' <- computeAbsoluteImportPaths pathUtils mustExist astPath rootPath next
        return $ (good :) <$> next'

  [] ->
    return $ Right []


computeAbsoluteImportPathsForAST :: PathUtils -> Bool -> FilePath -> AST -> IO (Either CompilationError AST)
computeAbsoluteImportPathsForAST pathUtils mustExist rootPath ast@AST{ aimports, apath = Just path } = do
  updatedImports <- computeAbsoluteImportPaths pathUtils mustExist path rootPath aimports
  return $ (\updated -> ast { aimports = updated }) <$> updatedImports
computeAbsoluteImportPathsForAST _ _ _ ast = error $ "ast has no path\n\n" <> ppShow ast
