module PackageGenerator where

import           GHC.IO.Handle
import           GHC.IO.Handle.FD
import           System.Directory
import           System.FilePath


defaultVersion :: String
defaultVersion = "0.0.1"


runPackageGenerator :: FilePath -> IO ()
runPackageGenerator path = do
  pkgName    <- askName path
  pkgVersion <- askVersion

  buildPackage path pkgName pkgVersion

  putStrLn $ "Your project is ready, do 'cd " <> path <> "'"


askName :: FilePath -> IO String
askName path = do
  putStr $ "package name [" <> path <> "]: "
  hFlush stdout
  pkgNameInput <- getLine
  case pkgNameInput of
    "" -> return path
    _  -> return pkgNameInput

askVersion :: IO String
askVersion = do
  putStr $ "package version [" <> defaultVersion <> "]: "
  hFlush stdout
  pkgVersionInput <- getLine
  case pkgVersionInput of
    "" -> return defaultVersion
    _  -> return pkgVersionInput

buildPackage :: FilePath -> String -> String -> IO ()
buildPackage path name version = do
  let srcFolder            = joinPath [path, "src"]
      madlibDotJsonPath    = joinPath [path, "madlib.json"]
      madlibDotJsonContent = makeMadlibDotJson name version
      mainDotMadPath       = joinPath [srcFolder, "Main.mad"]
      readmeContent        = readme path
      readmePath           = joinPath [path, "README.md"]

  createDirectoryIfMissing False path
  createDirectoryIfMissing False srcFolder
  writeFile madlibDotJsonPath madlibDotJsonContent
  writeFile mainDotMadPath    mainDotMadContent
  writeFile readmePath        readmeContent

makeMadlibDotJson :: String -> String -> String
makeMadlibDotJson name version = unlines
  ["{", "  \"name\": \"" <> name <> "\",", "  \"version\": \"" <> version <> "\",", "  \"main\": \"src/Main.mad\"", "}"]

mainDotMadContent :: String
mainDotMadContent = unlines ["import IO from \"IO\"", "", "IO.log(\"Hello World!\")"]

readme :: String -> String
readme projectName = unlines
  [ "# " <> projectName
  , "## Getting started"
  , "You just created a new madlib project, if it's your first project you should read the following."
  , "### Notes on Madlib"
  , "Madlib is a general purpose programming language that compiles to Javascript. It means that you need to have [Nodejs](https://nodejs.org/) installed and configured in order to make it work. Madlib can target nodejs or browser, by default it will compile for nodejs."
  , "### How to run it"
  , "First, you need to compile the program:"
  , "```shell"
  , "madlib compile -i src/Main.mad"
  , "```"
  , "Then, you can run it like this:"
  , "```shell"
  , "node build/Main.mjs"
  , "```"
  ]
