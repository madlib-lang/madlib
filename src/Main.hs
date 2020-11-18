{-# LANGUAGE FlexibleContexts   #-}
module Main where

import qualified Data.Map                      as M
import           GHC.IO                         ( )
import           Text.Show.Pretty               ( ppShow )
import           Control.Monad.Except           ( runExcept )
import           Control.Monad.State            ( StateT(runStateT) )
import           System.FilePath                ( takeDirectory )
import           System.Directory               ( canonicalizePath
                                                , createDirectoryIfMissing
                                                )
import           Utils.Path              hiding ( PathUtils(..) )
import           Parse.AST

import           Infer.Solve
import           Infer.Infer
import           Options.Applicative
import           Tools.CommandLineFlags
import           Tools.CommandLine
import           Compile
import qualified AST.Solved                    as Slv
import qualified Explain.Format                as Explain
-- import           Paths_madlib                  as Meta


main :: IO ()
main = run =<< execParser opts
 where
  opts = info
    (parseTransform <**> helper)
    (  fullDesc
    <> progDesc "madlib@0.0.3" -- TODO: make this use Meta.version instead
    <> header hashBar
    )

run :: TransformFlags -> IO ()
run (TransformFlags i o c) = do
  let (FileInput entrypoint)  = i
  let (FileOutput outputPath) = o
  putStrLn $ "OUTPUT: " ++ outputPath
  putStrLn $ "ENTRYPOINT: " ++ entrypoint
  putStrLn $ computeRootPath entrypoint
  astTable <- buildASTTable entrypoint
  putStrLn $ ppShow astTable
  let rootPath = computeRootPath entrypoint
  putStrLn $ "ROOT PATH: " ++ rootPath

  canonicalEntrypoint <- canonicalizePath entrypoint
  astTable            <- buildASTTable canonicalEntrypoint
  putStrLn $ ppShow astTable

  rootPath <- canonicalizePath $ computeRootPath entrypoint

  let entryAST         = astTable >>= flip findAST canonicalEntrypoint
      resolvedASTTable = case (entryAST, astTable) of
        (Right ast, Right table) ->
          runExcept (runStateT (solveTable table ast) Unique { count = 0 })
        (_     , Left e) -> Left e
        (Left e, _     ) -> Left e

  putStrLn $ "RESOLVED:\n" ++ ppShow resolvedASTTable

  case resolvedASTTable of
    Left  err        -> Explain.format readFile err >>= putStrLn
    Right (table, _) -> do
      generate rootPath outputPath table
      putStrLn "compiled JS:"
      putStrLn $ concat $ compile rootPath outputPath <$> M.elems table


generate :: FilePath -> FilePath -> Slv.Table -> IO ()
generate rootPath outputPath table =
  (head <$>) <$> mapM (generateAST rootPath outputPath) $ M.elems table


generateAST :: FilePath -> FilePath -> Slv.AST -> IO ()
generateAST rootPath outputPath ast@Slv.AST { Slv.apath = Just path } = do
  let resolvedOutputPath = computeTargetPath outputPath rootPath path

  createDirectoryIfMissing True $ takeDirectory resolvedOutputPath
  writeFile resolvedOutputPath $ compile rootPath resolvedOutputPath ast




