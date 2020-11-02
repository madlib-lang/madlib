{-# LANGUAGE FlexibleContexts   #-}
module Main where

import qualified Data.Map                      as M
import           GHC.IO                         ( )
import           System.Environment             ( getArgs )
import           Text.Show.Pretty               ( ppShow )
import           Control.Monad.Except           ( runExcept )
import           Control.Monad.State            ( StateT(runStateT) )
import           System.FilePath                ( takeDirectory
                                                , replaceExtension
                                                )
import           System.Directory               ( createDirectoryIfMissing )

import           Path
import           AST

import           Infer.Solve
import           Infer.Infer
import           Explain.Reason
import           Error.Error
import           Compile
import qualified AST.Solved as Slv
import qualified AST.Source as Src
import Explain.Location
import Infer.Type
import Explain.Meta

main :: IO ()
main = do
  entrypoint <- head <$> getArgs
  astTable   <- buildASTTable entrypoint
  putStrLn $ ppShow astTable

  let rootPath = computeRootPath entrypoint

  let entryAST         = astTable >>= flip findAST entrypoint
      resolvedASTTable = case (entryAST, astTable) of
        (Right ast, Right table) ->
          runExcept (runStateT (inferAST rootPath table ast) Unique { count = 0 })
        (_, _) -> Left $ InferError (UnboundVariable "") NoReason

  putStrLn $ "RESOLVED:\n" ++ ppShow resolvedASTTable

  case resolvedASTTable of
    Left  err        -> do
      formatted <- formatError err
      putStrLn formatted
    Right (table, _) -> do
      generate table
      putStrLn "compiled JS:"
      putStrLn $ concat $ compile <$> M.elems table


generate :: Slv.Table -> IO ()
generate table = (head <$>) <$> mapM generateAST $ M.elems table


generateAST :: Slv.AST -> IO ()
generateAST ast@Slv.AST { Slv.apath = Just path } = do
  let outputPath = makeOutputPath path

  createDirectoryIfMissing True $ takeDirectory outputPath
  writeFile outputPath $ compile ast


makeOutputPath :: FilePath -> FilePath
makeOutputPath path = "./build/" <> replaceExtension path "mjs"


formatError :: InferError -> IO String
formatError (InferError err reason) = case reason of
  Reason (WrongTypeApplied (Meta infos (Area (Loc a li c) _) e)) area -> do
    fc <- readFile "fixtures/wrong.mad"
    let l = lines fc !! (li - 1)
    let (Area (Loc _ lineStart colStart) (Loc _ lineEnd colEnd)) = area
    let (UnificationError expected actual) = err

    let nthInfo = case getNthArgFromInfos infos of
          Just nth -> "The "<>show nth<>nthEnding nth<>" "
          Nothing  -> "The "


    let message = "\n"<> nthInfo <> "argument of [TBD] has type\n\t"<>typeToStr actual<>"\nBut it was expected to be\n\t"<>typeToStr expected

    return $ "Error in function call at line "<>show li<>":\n"
      <> l <> "\n" <> concat [" " | _ <- [1..(colStart - 1)]] <> concat ["^" | _ <- [colStart..(colEnd - 1)]] <> message


nthEnding :: Int -> String
nthEnding n = case n of
  1 -> "st"
  2 -> "nd"
  3 -> "rd"
  _ -> "th"

typeToStr :: Type -> String
typeToStr t = case t of
  TCon CString -> "String"
  TCon CNum    -> "Num"