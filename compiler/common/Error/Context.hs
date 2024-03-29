module Error.Context where

import           Explain.Location


data Context
  = NoContext
  | Context { ctxAstPath :: FilePath, ctxArea :: Area }
  deriving(Eq, Ord, Show)


getCtxArea :: Context -> Maybe Area
getCtxArea ctx = case ctx of
  NoContext ->
    Nothing

  Context _ area ->
    Just area


getCtxPath :: Context -> Maybe FilePath
getCtxPath ctx = case ctx of
  NoContext ->
    Nothing

  Context path _ ->
    Just path


getCtxPath' :: Context -> FilePath
getCtxPath' ctx = case ctx of
  NoContext ->
    ""

  Context path _ ->
    path
