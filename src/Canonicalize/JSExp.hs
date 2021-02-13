module Canonicalize.JSExp where

import           Target
import           Text.Regex.TDFA

toRegex :: String -> Regex
toRegex = makeRegexOpts defaultCompOpt { multiline = False } defaultExecOpt

filterJSExp :: Target -> String -> String
filterJSExp target = removeSelectors target . removeOtherTargets target

removeOtherTargets :: Target -> String -> String
removeOtherTargets target code =
  let (startRegex, endRegex) = case target of
        TNode    -> ("{Browser}", "{/Browser}")
        TBrowser -> ("{Node}", "{/Node}")
      (before, matched, after) =
          match (toRegex startRegex) code :: (String, String, String)
      withoutStart = before <> after
      (_, matched', after') =
          match (toRegex endRegex) after :: (String, String, String)
      oneLess = before <> after'
      found   = not (null $ matched <> matched')
  in  if found then removeOtherTargets target oneLess else before

removeSelectors :: Target -> String -> String
removeSelectors target code =
  let regexCleanup = case target of
        TNode    -> "({Node}[\n]*|{/Node}[\n]*)"
        TBrowser -> "({Browser}[\n]*|{/Browser}[\n]*)"
      (before, matched, after) =
          match (toRegex regexCleanup) code :: (String, String, String)
  in  if not (null matched)
        then removeSelectors target $ before <> after
        else before <> after
