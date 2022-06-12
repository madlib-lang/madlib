module Utils.Version where

import Data.Version

major :: Version -> Int
major version = case version of
  Version [major, _, _] _ -> major

  _                       -> 0

minor :: Version -> Int
minor version = case version of
  Version [_, minor, _] _ -> minor

  _                       -> 0

patch :: Version -> Int
patch version = case version of
  Version [_, _, patch] _ -> patch

  _                       -> 0

