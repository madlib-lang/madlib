module Parse.Macro where

import Run.Target
import Debug.Trace
import Text.Show.Pretty


processLines :: Bool -> Target -> [String] -> [String]
processLines skip target lines = case lines of
  (line : nextLines) ->
    if line == "#iftarget llvm" || line == "#elseif llvm" then
      if target == TLLVM then
        "" : processLines False target nextLines
      else
        "" : processLines True target nextLines
    else if line == "#iftarget js" || line == "#elseif js" then
      if target == TBrowser || target == TNode then
        "" : processLines False target nextLines
      else
        "" : processLines True target nextLines
    else if line == "#endif" then
      "" : processLines False target nextLines
    else if skip then
      "" : processLines skip target nextLines
    else
      line : processLines skip target nextLines

  [] ->
    []


parseTargetMacros :: Target -> String -> String
parseTargetMacros target source =
  let sourceLines    = lines source
      processedLines = processLines False target sourceLines
  in  unlines processedLines
