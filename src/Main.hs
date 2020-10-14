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
        (Left  _  , Left _ ) -> Left $ UnboundVariable ""
        (Right ast, Right _) -> runEnv ast >>= (`runInfer` ast)
         where
          runEnv x = fst
            <$> runExcept (runStateT (buildInitialEnv x) Unique { count = 0 })

  putStrLn $ "RESOLVED:\n" ++ ppShow resolvedTable
  -- return ()

-- TODO: Implement function to build it
type SourceTable = M.Map FilePath String
