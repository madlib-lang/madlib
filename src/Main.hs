{-# LANGUAGE NamedFieldPuns   #-}
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
  let entrypoint = head args
  content      <- readFile entrypoint
  let ast = buildAST entrypoint content
  astTable     <- case ast of
    Right a  -> buildASTTable a (M.fromList [(entrypoint, a)])
    Left err -> return $ Left err

  resolvedTable <- case (ast, astTable) of
    (Right a, Right table) -> return $ resolveASTTable (Env M.empty M.empty) a table
    (Left a, Left b)       -> return $ trace (show (a, b)) M.empty

  putStrLn $ "RESOLVED:\n"++ppShow resolvedTable
  return ()

buildASTTable :: AST
              -> M.Map FilePath AST
              -> IO (Either String (M.Map FilePath AST))
buildASTTable AST { aimports } table = do
  contents <- mapM readFile importPaths
  let
    withPaths = zip importPaths contents
    built = mapM (uncurry buildAST) withPaths
    mapped = case built of
      Right x -> Right $ M.union table (M.fromList $ zip importPaths x)
      Left l  -> Left l
  return mapped
  where
    -- TODO: See toRoot function in Resolver.hs
    importPaths = ("fixtures/"++) . (++".mad") . ipath <$> aimports
