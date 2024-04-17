{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns   #-}
module Main where

import           GHC.IO.Encoding
import           Run.Run
import           Run.GenerateDocumentation
import           Run.Utils
import           Run.GeneratePackage
import           Run.Compile
import           Run.CommandLine
import           Run.PackageInstaller
import           Run.TestRunner
import           Run.Package
import           Run.Format
import           Run.LanguageServer
import           Run.Config
import qualified Run.Repl as Repl


main :: IO ()
main = do
  setLocaleEncoding utf8
  runCommandParser >>= run

run :: Command -> IO ()
run cmd = do
  case cmd of
    Compile{} ->
      let sanitizedOutput = sanitizeOutputPath cmd
      in  case sanitizedOutput of
            Right s ->
              runCompilation cmd { compileOutput = s }

            Left  e ->
              putStrLn e

    Test entrypoint target debug watch coverage optLevel ->
      runTests entrypoint target debug watch coverage optLevel

    Install ->
      runPackageInstaller

    Package{ packageSubCommand, rebuild } ->
      runPackage packageSubCommand rebuild

    New path ->
      runPackageGenerator path

    Doc path ->
      runDocumentationGenerator path

    Repl target ->
      Repl.start target

    Run target path args watchMode ->
      runRun target path args watchMode

    Format path code fix width ->
      runFormatter width fix path code

    LanguageServer -> do
      runLanguageServer

    Config configCommand ->
      runConfigurationCommand configCommand
