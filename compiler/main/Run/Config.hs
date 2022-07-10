module Run.Config where

import           Run.CommandLine
import           System.Environment.Executable  ( getExecutablePath )
import           System.FilePath (joinPath, dropFileName)


runConfigurationCommand :: ConfigCommand -> IO ()
runConfigurationCommand configCommand = case configCommand of
  RuntimeHeadersPath -> do
    compilerPath <- getExecutablePath
    let runtimeHeadersPath = joinPath [dropFileName compilerPath, "runtime/src"]
    putStrLn runtimeHeadersPath

  RuntimeLibHeadersPath -> do
    compilerPath <- getExecutablePath
    let runtimeLibHeadersPath = joinPath [dropFileName compilerPath, "runtime/include"]
    putStrLn runtimeLibHeadersPath

  InstallDir -> do
    compilerPath <- getExecutablePath
    putStrLn $ dropFileName compilerPath
