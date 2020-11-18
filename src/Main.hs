{-# LANGUAGE FlexibleContexts   #-}
module Main where

import qualified Data.Map                      as M
import           GHC.IO                         ( )
import           System.Environment             ( getArgs )
import           Text.Show.Pretty               ( ppShow )
import           Control.Monad.Except           ( runExcept )
import           Control.Monad.State            ( StateT(runStateT) )
import           System.FilePath                ( takeDirectory
                                                , replaceExtension
                                                )
import           System.FilePath.Posix          ( splitFileName )
import           System.Directory               ( createDirectoryIfMissing )
import           Path
import           AST

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
    <> progDesc "madlib@0.0.2" -- TODO: make this use Meta.version instead
    <> header hashBar
    )

run :: TransformFlags -> IO ()
run (TransformFlags i o c) = do
  let (FileInput entrypoint) = i
  putStrLn $ "ENTRYPOINT: " ++ entrypoint
  putStrLn $ show (computeRootPath entrypoint)
  astTable <- buildASTTable entrypoint
  putStrLn $ ppShow astTable
  let rootPath = computeRootPath entrypoint
  putStrLn $ "ROOT PATH: " ++ rootPath

  let entryAST         = astTable >>= flip findAST entrypoint
      resolvedASTTable = case (entryAST, astTable) of
        (Right ast, Right table) -> runExcept
          (runStateT (inferAST rootPath table ast) Unique { count = 0 })
        (_, Left e) -> Left e

  putStrLn $ "RESOLVED:\n" ++ ppShow resolvedASTTable

  case resolvedASTTable of
    Left  err        -> Explain.format readFile err >>= putStrLn
    Right (table, _) -> do
      generate table
      putStrLn "compiled JS:"
      putStrLn $ concat $ compile <$> M.elems table


generate :: Slv.Table -> IO ()
generate table = (head <$>) <$> mapM generateAST $ M.elems table


generateAST :: Slv.AST -> IO ()
generateAST ast@Slv.AST { Slv.apath = Just path } = do
  let outputPath = makeOutputPath path

  createDirectoryIfMissing True $ takeDirectory outputPath
  writeFile outputPath $ compile ast


makeOutputPath :: FilePath -> FilePath
makeOutputPath path = "./build/" <> replaceExtension path "mjs"

