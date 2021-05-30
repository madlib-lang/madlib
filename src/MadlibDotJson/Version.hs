module MadlibDotJson.Version where

import Data.Version                               ( Version(Version), showVersion, parseVersion )
import           Error.Warning
import           Error.Context
import           Text.ParserCombinators.ReadP


parse :: String -> Maybe Version
parse v = 
  let r = readP_to_S parseVersion v
  in
    if not (null r) then
      case last r of
        (a, "") -> Just a
        _       -> Nothing
    else
      Nothing


checkVersion :: Maybe String -> String -> Version -> Maybe CompilationWarning
checkVersion pkgName versionFromPkg versionFromCompiler =
  let versionFromPkg' = parse versionFromPkg
  in  case (versionFromPkg', versionFromCompiler) of
    (Just (Version (major:minor:patch:_) _), Version (major':minor':patch':_) _) ->
      if major /= major' then
        return $
          CompilationWarning
          (MadlibVersionMajorDiffer pkgName versionFromPkg (showVersion versionFromCompiler))
          NoContext
      else if minor' < minor then
        return $
          CompilationWarning
          (MadlibVersionMinorTooLow pkgName versionFromPkg (showVersion versionFromCompiler))
          NoContext
      else
        Nothing

    _ -> Nothing

