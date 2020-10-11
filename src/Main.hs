{-# LANGUAGE FlexibleContexts   #-}
module Main where

import           Prelude                 hiding ( readFile )
import qualified Data.Map                      as M
import           GHC.IO                         ( )
import           System.Environment             ( getArgs )
import           Text.Show.Pretty               ( ppShow )
import           AST                            ( ASTError(..)
                                                , buildASTTable
                                                , findAST
                                                )
import           Infer

main :: IO ()
main = do
  entrypoint <- head <$> getArgs
  astTable   <- buildASTTable entrypoint
  putStrLn $ ppShow astTable

  -- putStrLn $ case astTable of
  --   Left e -> ppShow e
  --   Right o -> ppShow $ infer M.empty o >>= findAST entrypoint
  let entryAST      = astTable >>= findAST entrypoint
      initialEnv    = buildInitialEnv <$> entryAST
      resolvedTable = case (entryAST, astTable, initialEnv) of
        (Left _, Left _, Left _) -> Left $ UnboundVariable ""
        (Right ast, Right _, Right env) ->
          -- Move all of this to runInfer :: ASTTable -> Either InferError (...)
          -- runExcept $ runStateT (infer M.empty $ head $ aexps ast) Unique { count = 0 }
          runInfer env ast

  putStrLn $ "RESOLVED:\n" ++ ppShow resolvedTable
  -- return ()

-- TODO: Implement function to build it
type SourceTable = M.Map FilePath String
