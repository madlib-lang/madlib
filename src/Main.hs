{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts   #-}
module Main where

-- import           Options.Applicative
import           Run.CommandLine
import           GHC.IO.Encoding
import           Run.Run

main :: IO ()
main = do
  setLocaleEncoding utf8
  runCommandParser >>= run
