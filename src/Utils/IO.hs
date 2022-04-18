module Utils.IO where

import System.IO

putStrLnAndFlush :: String -> IO ()
putStrLnAndFlush str = do
  putStrLn str
  hFlush stdout

