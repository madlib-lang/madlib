{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Run.Utils where

import           System.FilePath                ( takeDirectory
                                                , takeBaseName
                                                , takeExtension
                                                , takeFileName
                                                , dropExtension
                                                , joinPath
                                                , splitDirectories
                                                , makeRelative
                                                , hasTrailingPathSeparator
                                                )
import           System.Directory               ( createDirectoryIfMissing
                                                , getDirectoryContents
                                                , doesDirectoryExist
                                                , getCurrentDirectory
                                                )
import           System.Environment             ( getEnv )
import           Control.Exception              ( try )
import           Control.Monad                  ( filterM )
import           Data.List                      ( isSuffixOf )
import qualified Distribution.System            as DistributionSystem

import           Run.CommandLine
import Run.Target

isCoverageEnabled :: IO Bool
isCoverageEnabled = do
  coverageEnv <- try $ getEnv "COVERAGE_MODE"
  case coverageEnv :: Either IOError String of
    Right "on" -> return True
    _          -> return False

getDefaultExecutableName :: FilePath
getDefaultExecutableName = case DistributionSystem.buildOS of
  DistributionSystem.Windows ->
    "a.exe"

  _ ->
    "a.out"


sanitizeOutputPath :: Command -> Either String FilePath
sanitizeOutputPath Compile { compileOutput, compileTarget = TLLVM } =
  if hasTrailingPathSeparator compileOutput then
    Right $ joinPath [compileOutput, getDefaultExecutableName]
  else
    Right compileOutput

sanitizeOutputPath Compile { compileOutput, compileBundle } =
  let ext = takeExtension compileOutput
  in
    if compileBundle then
      case ext of
        ".js" ->
          Right compileOutput

        _ ->
          Left $ "Wrong output. With bundle option the output must have '.js' extension, but '" <> ext <> "' was given!"
    else
      if hasTrailingPathSeparator compileOutput then
        Right compileOutput
      else
        Right $ compileOutput <> "/"

compilationBlackList :: [FilePath]
compilationBlackList = ["madlib_modules", "node_modules"]

getFilesToCompile :: Bool -> FilePath -> IO [FilePath]
getFilesToCompile testsOnly entrypoint = case takeExtension entrypoint of
  ".mad"     ->
    return [entrypoint]

  '.' : _ ->
    return []

  _          -> do
    paths <- getDirectoryContents entrypoint
    let fullPaths =
          (\file -> joinPath [entrypoint, file])
            <$> filter (\p -> p /= "." && p /= ".." && not (any (`isSuffixOf` p) compilationBlackList)) paths
    let filtered = if not testsOnly
          then filter ((== ".mad") . takeExtension) fullPaths
          else filter (isSuffixOf ".spec.mad") fullPaths

    subFolders <- filterM doesDirectoryExist fullPaths
    next       <- mapM (getFilesToCompile testsOnly) subFolders
    return $ filtered ++ concat next
