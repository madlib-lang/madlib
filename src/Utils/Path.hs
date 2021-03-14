module Utils.Path
  ( computeRootPath
  , resolveAbsoluteSrcPath
  , computeTargetPath
  , makeRelativeEx
  , cleanRelativePath
  )
where

import           System.FilePath                ( dropFileName
                                                , replaceExtension
                                                , dropTrailingPathSeparator
                                                , takeDirectory
                                                , splitPath
                                                , joinPath
                                                , splitFileName
                                                , normalise, takeExtension
                                                )
import qualified MadlibDotJSON
import           Data.List                      ( isInfixOf
                                                , isPrefixOf
                                                , (\\)
                                                )
import           Utils.PathUtils
import           Data.Maybe
import qualified Data.Map                      as M
import qualified Utils.URL                     as URL



data ModulePath
  = PackagePath
  | FileSystemPath


madlibDotJsonFile :: FilePath
madlibDotJsonFile = "madlib.json"

madlibModulesFolder :: FilePath
madlibModulesFolder = "madlib_modules"


computeRootPath :: FilePath -> FilePath
computeRootPath = fst . splitFileName


cleanRelativePath :: FilePath -> FilePath
cleanRelativePath path = case normalise path of
  '/' : xs -> '/' : xs
  or       -> "./" <> or


resolveAbsoluteSrcPath :: PathUtils -> FilePath -> FilePath -> IO (Maybe FilePath)
resolveAbsoluteSrcPath pathUtils rootPath path = case getPathType path of
  FileSystemPath -> do
    let ext = if takeExtension path == ".json" then ".json" else ".mad"
    Just <$> canonicalizePath pathUtils (replaceExtension (joinPath [dropFileName rootPath, path]) ext)
  PackagePath -> makePathForPackage pathUtils rootPath path


-- TODO: Maybe also verify that the .mad file exists first ?
-- Also we could read the madlib.json file and see if that is defined as
-- a dependency ?
getPathType :: FilePath -> ModulePath
getPathType ('.' : _)              = FileSystemPath
getPathType ('/' : _)              = FileSystemPath
getPathType path | '/' `elem` path = FileSystemPath
getPathType _                      = PackagePath


makePathForPackage :: PathUtils -> FilePath -> FilePath -> IO (Maybe FilePath)
makePathForPackage pathUtils rootPath pkgName = do
  preludeModulePath <- findPreludeModulePath pathUtils pkgName
  case preludeModulePath of
    Just path -> return $ Just path
    Nothing   -> do
      madlibDotJsonFile <- retrieveMadlibDotJson pathUtils rootPath
      case madlibDotJsonFile of
        Just json -> findMadlibPackage pathUtils pkgName rootPath json
        _         -> return Nothing



retrieveMadlibDotJson :: PathUtils -> FilePath -> IO (Maybe MadlibDotJSON.MadlibDotJSON)
retrieveMadlibDotJson pathUtils dir = do
  let path = joinPath [dir, madlibDotJsonFile]
  found <- doesFileExist pathUtils path
  if found
    then do
      json <- MadlibDotJSON.load pathUtils path
      case json of
        Right json' -> return $ Just json'
        Left  _     -> return Nothing
    else if dir == "/" then return Nothing else retrieveMadlibDotJson pathUtils $ getParentFolder dir

getParentFolder :: FilePath -> FilePath
getParentFolder = joinPath . init . splitPath


findMadlibPackage :: PathUtils -> FilePath -> FilePath -> MadlibDotJSON.MadlibDotJSON -> IO (Maybe FilePath)
findMadlibPackage pathUtils pkgName dir madlibDotJson = do
  let dependencies = MadlibDotJSON.dependencies madlibDotJson
  let sanitizedUrl = URL.sanitize <$> (dependencies >>= M.lookup pkgName)
  let path = (\url -> joinPath [dir, madlibModulesFolder, url, madlibDotJsonFile]) <$> sanitizedUrl
  case path of
    Nothing -> return Nothing
    Just p  -> do
      found <- doesFileExist pathUtils p
      if found
        then findMadlibPackageMainPath pathUtils p
        else (findMadlibPackage pathUtils pkgName . joinPath . init . splitPath) dir madlibDotJson


findMadlibPackageMainPath :: PathUtils -> FilePath -> IO (Maybe FilePath)
findMadlibPackageMainPath pathUtils file = do
  json <- MadlibDotJSON.load pathUtils file
  let folder = takeDirectory file
  case json of
    Left  _     -> return Nothing
    Right json' -> Just <$> canonicalizePath pathUtils (joinPath [folder, MadlibDotJSON.main json'])


findPreludeModulePath :: PathUtils -> FilePath -> IO (Maybe FilePath)
findPreludeModulePath pathUtils moduleName = do
  exePath <- getExecutablePath pathUtils
  findPreludeModulePath' pathUtils moduleName (dropFileName exePath)

findPreludeModulePath' :: PathUtils -> FilePath -> FilePath -> IO (Maybe FilePath)
findPreludeModulePath' _         _          ""      = return Nothing
findPreludeModulePath' pathUtils moduleName currDir = do
  let fullPath = replaceExtension (joinPath [currDir, "prelude/__internal__", moduleName]) ".mad"
  found <- doesFileExist pathUtils fullPath
  if found
    then return $ Just fullPath
    else findPreludeModulePath' pathUtils moduleName ((joinPath . init . splitPath) currDir)


computeTargetPath :: FilePath -> FilePath -> FilePath -> FilePath
computeTargetPath outputPath rootPath path = case isPackage path of
  FileSystemPath ->
    let cleanOutputPath = dropTrailingPathSeparator $ normalise outputPath
        rootParts       = dropTrailingPathSeparator <$> splitPath rootPath
        pathParts       = dropTrailingPathSeparator <$> splitPath path
        withoutRoot     = if rootParts `isPrefixOf` pathParts
          then pathParts \\ rootParts -- remove the root path components
          else pathParts
    in  cleanRelativePath . joinPath $ [cleanOutputPath, replaceExtension (joinPath withoutRoot) ".mjs"]

  PackagePath -> if isPreludePath path
    then
      let cleanOutputPath = dropTrailingPathSeparator $ normalise outputPath
          split           = dropTrailingPathSeparator <$> splitPath path
          fromInternal    = tail $ dropWhile (/= "__internal__") split
          complete        = [cleanOutputPath, ".prelude"] <> fromInternal
      in  cleanRelativePath $ replaceExtension (joinPath complete) ".mjs"
    else
      let cleanOutputPath       = dropTrailingPathSeparator $ normalise outputPath
          split                 = dropTrailingPathSeparator <$> splitPath path
          fromMadlibModules     = tail $ dropWhile (/= "madlib_modules") split
          complete              = [cleanOutputPath, ".deps"] <> fromMadlibModules
          madlibModulesReplaced = (\p -> if p == "madlib_modules" then ".deps" else p) <$> complete
      in  cleanRelativePath $ replaceExtension (joinPath madlibModulesReplaced) ".mjs"


isPreludePath :: FilePath -> Bool
isPreludePath = isInfixOf "prelude/__internal__"


makeRelativeEx :: FilePath -> FilePath -> FilePath
makeRelativeEx base target =
  let splitBase   = dropTrailingPathSeparator <$> splitPath base
      splitTarget = dropTrailingPathSeparator <$> splitPath target
  in  case (splitBase, splitTarget) of
        (first : ps, first' : ps') -> if first == first'
          then makeRelativeEx (joinPath ps) (joinPath ps')
          else cleanRelativePath . joinPath $ (".." <$ ps) <> [".."] <> [first'] <> ps'
        ([], ps) -> cleanRelativePath $ joinPath ps


isPackage :: FilePath -> ModulePath
isPackage path =
  let split = dropTrailingPathSeparator <$> splitPath path
  in  if elem "madlib_modules" split || isPreludePath path then PackagePath else FileSystemPath
