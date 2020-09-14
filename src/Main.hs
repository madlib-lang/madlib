module Main where

import qualified Data.Map           as M
import           Debug.Trace        (trace)
import           GHC.IO             ()
import           Grammar
import           Resolver
import           System.Environment (getArgs)
import           Text.Show.Pretty   (ppShow)


main :: IO ()
main = do
  args         <- getArgs
  entrypoint   <- return $ head args
  content      <- readFile entrypoint
  ast          <- return $ buildAST content
  astTable     <- case ast of
    Right a -> buildASTTable a (M.fromList [(entrypoint, a)])
  -- putStrLn $ ppShow astTable

  resolvedTable <- case (ast, astTable) of
    (Right a, Right table) -> return $ resolveASTTable (Env M.empty M.empty) a table

  putStrLn $ "RESOLVED:\n"++ppShow resolvedTable

  -- resolved <- case ast of
  --   (Right a) -> return $ resolve (Env M.empty M.empty) a
  --   x         -> return x
  -- putStrLn (generateOutput resolved)
  return ()

buildASTTable :: AST
              -> M.Map FilePath AST
              -> IO (Either String (M.Map FilePath AST))
buildASTTable ast table = do
  contents <- mapM readFile importPaths
  let
    built = mapM buildAST contents
    mapped = case built of
      Right x -> Right $ M.union table (M.fromList $ zip importPaths x)
      Left l  -> Left l
  return mapped
  where
    -- TODO: See toRoot function in Resolver.hs
    importPaths = ("fixtures/"++) <$> (++".mad") <$> ipath <$> imports ast
