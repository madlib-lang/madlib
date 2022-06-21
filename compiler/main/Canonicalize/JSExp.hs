{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Canonicalize.JSExp where

import           Run.Target
import           Text.Regex.TDFA

toRegex :: String -> Regex
toRegex = makeRegexOpts defaultCompOpt { multiline = False } defaultExecOpt

browserStartRegex :: Regex
browserStartRegex = toRegex "{Browser}"

browserEndRegex :: Regex
browserEndRegex = toRegex "{/Browser}"

nodeStartRegex :: Regex
nodeStartRegex = toRegex "{Node}"

nodeEndRegex :: Regex
nodeEndRegex = toRegex "{/Node}"

filterJSExp :: Target -> String -> String
filterJSExp target = removeSelectors target . removeOtherTargets target

removeOtherTargets :: Target -> String -> String
removeOtherTargets TAny code   = code
removeOtherTargets TLLVM code  = code
removeOtherTargets target code =
  let (startRegex, endRegex) =
        case target of
          TNode ->
            (browserStartRegex, browserEndRegex)

          TBrowser ->
            (nodeStartRegex, nodeEndRegex)

      (before, matched, after) = match startRegex code :: (String, String, String)
      (_, matched', after')    = match endRegex after :: (String, String, String)
      oneLess                  = before <> after'
      found                    = not (null $ matched <> matched')
  in  if found then removeOtherTargets target oneLess else before

removeSelectors :: Target -> String -> String
removeSelectors TAny code   = code
removeSelectors TLLVM code  = code
removeSelectors target code =
  let regexCleanup = case target of
        TNode    -> "({Node}[\n]*|{/Node}[\n]*)"
        TBrowser -> "({Browser}[\n]*|{/Browser}[\n]*)"
      (before, matched, after) = match (toRegex regexCleanup) code :: (String, String, String)
  in  if not (null matched) then removeSelectors target $ before <> after else before <> after
