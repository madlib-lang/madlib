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
import Infer
import Control.Monad.State (runStateT, runState)
import Control.Monad.Except (runExcept)

main :: IO ()
main = do
  entrypoint <- head <$> getArgs
  astTable   <- buildASTTable entrypoint
  putStrLn $ ppShow astTable

  -- putStrLn $ case astTable of
  --   Left e -> ppShow e
  --   Right o -> ppShow $ infer M.empty o >>= findAST entrypoint
  let entryAST      = astTable >>= findAST entrypoint
      resolvedTable = case (entryAST, astTable) of
        (Left _, Left _) -> Left $ UnboundVariable ""
        (Right ast, Right table) ->
          -- Move all of this to runInfer :: ASTTable -> Either InferError (...)
          -- runExcept $ runStateT (infer M.empty $ head $ aexps ast) Unique { count = 0 }
          runInfer ast

  putStrLn $ "RESOLVED:\n" ++ ppShow resolvedTable
  -- return ()

-- TODO: Implement function to build it
type SourceTable = M.Map FilePath String
