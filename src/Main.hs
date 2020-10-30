{-# LANGUAGE FlexibleContexts   #-}
module Main where

import           Prelude                 hiding ( readFile )
import qualified Data.Map                      as M
import           GHC.IO                         ( )
import           System.Environment             ( getArgs )
import           Text.Show.Pretty               ( ppShow )
import           Control.Monad.Except           ( runExcept )
import           Control.Monad.State            ( StateT(runStateT) )
import           System.FilePath                ( takeDirectory
                                                , replaceExtension
                                                )
import           System.Directory               ( createDirectoryIfMissing )

import           Path
import           AST

import           Infer.Type
import           Infer.Solve
import           Compile
import qualified AST.Source as Src
import qualified AST.Solved as Slv

main :: IO ()
main = do
  entrypoint <- head <$> getArgs
  astTable   <- buildASTTable entrypoint
  putStrLn $ ppShow astTable

  let rootPath = computeRootPath entrypoint

  let entryAST         = astTable >>= flip findAST entrypoint
      resolvedASTTable = case (entryAST, astTable) of
        (Right ast, Right table) ->
          runExcept (runStateT (inferAST rootPath table ast) Unique { count = 0 })
        (_, _) -> Left $ UnboundVariable ""

  putStrLn $ "RESOLVED:\n" ++ ppShow resolvedASTTable

  case resolvedASTTable of
    Left  _          -> putStrLn "Err"
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
