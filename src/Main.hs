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
import           Control.Monad.Except           ( runExcept )
import           Control.Monad.State            ( StateT(runStateT) )
import           Compile
import           Debug.Trace                    ( trace )

main :: IO ()
main = do
  entrypoint <- head <$> getArgs
  astTable   <- buildASTTable entrypoint
  putStrLn $ ppShow astTable

  -- putStrLn $ case astTable of
  --   Left e -> ppShow e
  --   Right o -> ppShow $ infer M.empty o >>= findAST entrypoint
  let entryAST    = astTable >>= findAST entrypoint
      resolvedAST = case (entryAST, astTable) of
        (Left _, Left _) -> Left $ UnboundVariable ""
        (Right ast, Right _) ->
          trace (ppShow $ runEnv ast) (runEnv ast) >>= (`runInfer` ast)
         where
          runEnv x = fst
            <$> runExcept (runStateT (buildInitialEnv x) Unique { count = 0 })

  putStrLn $ "RESOLVED:\n" ++ ppShow resolvedAST

  case resolvedAST of
    Left  _   -> putStrLn "Err"
    Right ast -> do
      putStrLn "compiled JS:"
      putStrLn $ compile ast
  -- return ()

-- TODO: Implement function to build it
type SourceTable = M.Map FilePath String
