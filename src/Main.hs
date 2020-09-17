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
import           AST                            ( ASTBuildError(..)
                                                , buildASTTable
                                                , getEntrypoint
                                                )
import           Control.Monad                  ( liftM2 )
import           Control.Monad.Catch

main :: IO ()
main = do
  entrypoint <- head <$> getArgs
  astTable   <- buildASTTable entrypoint
  let entryAST = astTable >>= getEntrypoint entrypoint
      resolvedTable =
        liftM2 (resolveASTTable (Env M.empty M.empty Nothing)) entryAST astTable

  putStrLn $ "RESOLVED:\n" ++ ppShow resolvedTable
  return ()

-- TODO: Implement function to build it
type SourceTable = M.Map FilePath String
