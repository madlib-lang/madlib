module Compile.Utils where

import           Target


getGlobalForTarget :: Target -> String
getGlobalForTarget target = case target of
  TNode    -> "global"
  TBrowser -> "window"
