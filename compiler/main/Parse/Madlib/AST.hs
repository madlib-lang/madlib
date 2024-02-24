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
import           Parse.Madlib.Grammar             ( parse )
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
import           Text.Read (readMaybe)
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
  let var = Source area sourceTarget (LStr $ "\"" <> escapeJSONString jsonContent <> "\"")
  let assignment = Source area sourceTarget (Assignment name var)

  return $ assignment : next


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
buildAST options path code = case parse code of
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
        let astWithBuiltinsImport =
              if "__BUILTINS__.mad" `isSuffixOf` path || any ((== "__BUILTINS__") . snd . getImportPath) (aimports astWithProcessedMacros) then
                astWithProcessedMacros
              else
                astWithProcessedMacros { aimports = builtinsDictTypeImport : builtinsImport : aimports astWithProcessedMacros }
        astWithAbsoluteImportPaths <- computeAbsoluteImportPathsForAST (optPathUtils options) (not $ optParseOnly options) (optRootPath options) astWithBuiltinsImport
        case astWithAbsoluteImportPaths of
          Right astWithAbsoluteImportPaths' -> do
            astWithJsonAssignments     <- processJsonImports options astWithAbsoluteImportPaths'
            return $ Right astWithJsonAssignments

          Left _ ->
            return astWithAbsoluteImportPaths

  Left e -> do
    let split = lines e
    if head split == "BadEscape" then do
      let Just l1 = (readMaybe $ split !! 1) :: Maybe Int
          Just c1 = (readMaybe $ split !! 2) :: Maybe Int
          Just l2 = (readMaybe $ split !! 3) :: Maybe Int
          Just c2 = (readMaybe $ split !! 4) :: Maybe Int

      return $ Left $ CompilationError BadEscapeSequence (Context path (Area (Loc 0 l1 c1) (Loc 0 l2 c2)))
    else if head split == "EmptyChar" then do
      let Just l1 = (readMaybe $ split !! 1) :: Maybe Int
          Just c1 = (readMaybe $ split !! 2) :: Maybe Int
          Just l2 = (readMaybe $ split !! 3) :: Maybe Int
          Just c2 = (readMaybe $ split !! 4) :: Maybe Int

      return $ Left $ CompilationError EmptyChar (Context path (Area (Loc 0 l1 c1) (Loc 0 l2 c2)))
    else do
      let line = (readMaybe $ head split) :: Maybe Int
          col = (readMaybe $ split !! 1) :: Maybe Int
          text = if length split < 2 then
                "Syntax error"
              else
                unlines (tail . tail $ split)

      case (line, col) of
        (Just line', Just col') ->
          return $ Left $ CompilationError (GrammarError path text) (Context path (Area (Loc 0 line' col') (Loc 0 line' (col' + 1))))

        (Just line', Nothing) ->
          return $ Left $ CompilationError (GrammarError path text) (Context path (Area (Loc 0 line' 0) (Loc 0 (line' + 1) 0)))

        _ -> do
          -- then we try to parse this: "lexical error at line 2, column 11"
          let split' = words e
          let line = (readMaybe $ init $ split' !! 4) :: Maybe Int
              col  = (readMaybe $ split' !! 6) :: Maybe Int
              text = "Syntax error"

          case (line, col) of
            (Just line', Just col') ->
              return $ Left $ CompilationError (GrammarError path text) (Context path (Area (Loc 0 line' col') (Loc 0 line' (col' + 1))))

            (Just line', Nothing) ->
              return $ Left $ CompilationError (GrammarError path text) (Context path (Area (Loc 0 line' 0) (Loc 0 (line' + 1) 0)))

            _ ->
              return $ Left $ CompilationError (GrammarError path text) (Context path (Area (Loc 0 1 1) (Loc 1 1 1)))


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
