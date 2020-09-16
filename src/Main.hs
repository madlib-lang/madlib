{-# LANGUAGE NamedFieldPuns   #-}
module Main where

import           Prelude                 hiding ( readFile )
import qualified Prelude                       as P
import qualified Data.Map                      as M
import           Debug.Trace                    ( trace )
import           GHC.IO                         ( )
import           Grammar
import           Resolver
import           System.Environment             ( getArgs )
import           Text.Show.Pretty               ( ppShow )
import           AST                            ( buildASTTable )
import           Control.Monad                  ( liftM2 )

main :: IO ()
main = do
  entrypoint <- head <$> getArgs
  astTable   <- buildASTTable entrypoint
  let entryAST = astTable >>= getEntrypoint entrypoint
      resolvedTable =
        liftM2 (resolveASTTable (Env M.empty M.empty)) entryAST astTable

  putStrLn $ "RESOLVED:\n" ++ ppShow resolvedTable
  return ()
 where
  getEntrypoint :: FilePath -> ASTTable -> Either String AST
  getEntrypoint table path = case M.lookup table path of
    Just x  -> return x
    Nothing -> Left "Entrypoint not found !"

maybeToRight :: e -> Maybe a -> Either e a
maybeToRight fallback (Just a) = Right a
maybeToRight fallback Nothing  = Left fallback

type SourceTable = M.Map FilePath String
