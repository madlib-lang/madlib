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
                                                , findAST
                                                )

main :: IO ()
main = do
  entrypoint <- head <$> getArgs
  astTable   <- buildASTTable entrypoint
  putStrLn $ ppShow astTable
  -- let entryAST      = astTable >>= findAST entrypoint
  --     resolvedTable = case (entryAST, astTable) of
  --       (Left _, Left _) -> Left [RError PathNotFound (Backtrace [])]
  --       (Right ast, Right table) ->
  --         resolveASTTable (Env M.empty M.empty Nothing (Backtrace [])) ast table

  -- putStrLn $ "RESOLVED:\n" ++ ppShow resolvedTable
  -- return ()

-- TODO: Implement function to build it
type SourceTable = M.Map FilePath String
