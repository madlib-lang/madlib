module Path
  ( computeRootPath
  )
where

import           System.FilePath.Posix          ( splitFileName )

computeRootPath :: FilePath -> FilePath
computeRootPath = fst . splitFileName
