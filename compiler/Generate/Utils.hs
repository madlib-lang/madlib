module Generate.Utils where

import           Run.Target
import           Data.Aeson.Text                ( encodeToLazyText )
import           Data.Text.Lazy                 ( unpack )


getGlobalForTarget :: Target -> String
getGlobalForTarget target = case target of
  TNode    -> "global"
  TBrowser -> "window"

escapeString :: String -> String
escapeString = unpack . encodeToLazyText
