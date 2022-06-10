module Error.Context where

import           Explain.Location
import           Error.Backtrace

data Context
  = NoContext
  | Context { ctxAstPath :: FilePath, ctxArea :: Area, ctxBacktrace :: Backtrace }
  deriving(Eq, Show)

getCtxArea :: Context -> Maybe Area
getCtxArea ctx = case ctx of
  NoContext        -> Nothing
  Context _ area _ -> Just area

getCtxPath :: Context -> Maybe FilePath
getCtxPath ctx = case ctx of
  NoContext        -> Nothing
  Context path _ _ -> Just path
