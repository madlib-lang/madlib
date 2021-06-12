{-# LANGUAGE NamedFieldPuns #-}
module Run.Package where

import           System.Directory              ( canonicalizePath, listDirectory, doesFileExist, doesDirectoryExist, getCurrentDirectory )
import           System.FilePath               ( takeDirectory, joinPath )
import           Text.Show.Pretty
import qualified Data.Map                      as Map
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BLChar8
import qualified Data.List                     as List
import           Crypto.Hash.MD5               ( hashlazy )
import           Data.ByteString.Builder


import           MadlibDotJson.MadlibDotJson
import           Error.Error
import           Error.Warning
import qualified AST.Canonical                      as Can
import qualified Canonicalize.AST                   as Can
import qualified Canonicalize.Env                   as Can
import qualified AST.Solved                         as Slv
import qualified Infer.AST                          as Slv
import qualified AST.Source                         as Src
import qualified Parse.Madlib.AST                   as Src
import           Run.Target
import           Run.PackageHash

parse :: FilePath -> IO (Either CompilationError Src.Table)
parse = Src.buildASTTable mempty

canonicalize :: Src.Table -> FilePath -> (Either CompilationError Can.Table, [CompilationWarning])
canonicalize srcTable main =
  let (result, warnings) = Can.runCanonicalization mempty TAny Can.initialEnv srcTable main
  in  (fst <$> result, warnings)

typeCheck :: Can.Table -> FilePath -> (Either [CompilationError] Slv.Table, [CompilationWarning])
typeCheck canTable path = Slv.solveManyASTs' canTable [path]


typeCheckMain :: FilePath -> IO (Either [CompilationError] Slv.Table, [CompilationWarning])
typeCheckMain main = do
  parsed        <- parse main

  case parsed of
    Right srcTable -> case canonicalize srcTable main of
      (Left err, warnings)       -> return (Left [err], warnings)

      (Right canTable, warnings) -> return $ typeCheck canTable main

    Left err    -> return (Left [err], [])


runBuildPackage :: IO ()
runBuildPackage = do
  madlibDotJson <- loadCurrentMadlibDotJson

  case madlibDotJson of
    Left _ -> putStrLn "The package must have a madlib.json file"

    Right MadlibDotJson { main } -> do
      canonicalMain <- canonicalizePath main
      currentDirectoryPath <- getCurrentDirectory

      hash <- generatePackageHash currentDirectoryPath

      putStrLn hash

      (typeChecked, warnings) <- typeCheckMain canonicalMain
      return ()
      -- putStrLn $ ppShow $ Map.lookup canonicalMain <$> typeChecked
