{-# LANGUAGE FlexibleContexts   #-}
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
import           AST                            ( ASTError(..)
                                                , buildASTTable
                                                , getEntrypoint
                                                )

main :: IO ()
main = do
  entrypoint <- head <$> getArgs
  astTable   <- buildASTTable entrypoint
  let entryAST      = astTable >>= getEntrypoint entrypoint
      resolvedTable = case (entryAST, astTable) of
        (Left _, Left _) -> Left [PathNotFound]
        (Right ast, Right table) ->
          resolveASTTable (Env M.empty M.empty Nothing) ast table

  putStrLn $ "RESOLVED:\n" ++ ppShow resolvedTable
  return ()

-- TODO: Implement function to build it
type SourceTable = M.Map FilePath String
