module Compile.JSInternals where

import           Target
import           Compile.Utils

generateInternalsModuleContent :: Target -> Bool -> Bool -> String
generateInternalsModuleContent target optimized coverage =
  curryFn target optimized
    <> "\n"
    <> eqFn target optimized
    <> "\n"
    <> applyDictsFn target optimized
    <> if coverage then "\n" <> hpFnWrap <> "\n" <> hpLineWrap else ""


curryFnName :: Bool -> String
curryFnName optimized = if optimized then "λ1" else "__curry__"

curryFn :: Target -> Bool -> String
curryFn target optimized =
  let fnName = curryFnName optimized
  in
    unlines
      [ "const toString = (fn, args = []) => () => ("
      , "  `curry(${fn.toString()})${args.length > 0 ? `(${args.join(`,`)})` : ``}`"
      , ")"
      , getGlobalForTarget target <> "." <> fnName <> " = (fn) => {"
      , "  function curried(...args) {"
      , "    const length = args.length"
      , "    function saucy(...args2) {"
      , "      return curried.apply(this, args.concat(args2))"
      , "    }"
      , "    saucy.toString = toString(fn, args)"
      , "    return ("
      , "      length >= fn.length ?"
      , "      fn.apply(this, args) :"
      , "      saucy"
      , "    )"
      , "  }"
      , "  curried.toString = toString(fn)"
      , "  return curried"
      , "};"
      , ""
      ]


hpFnWrap :: String
hpFnWrap = unlines
  [ "global.__hpFnWrap = (astPath, line, name) => (fn) => {"
  , "  function wrapped(...args) {"
  , "    __hp(astPath, 'function', line, name)"
  , "    __hp(astPath, 'line', line, line)"
  , "    return fn.apply(this, args)"
  , "  }"
  , "  return wrapped"
  , "}"
  ]

hpLineWrap :: String
hpLineWrap = unlines
  [ "global.__hpLineWrap = (astPath, line, x) => {"
  , "  __hp(astPath, 'line', line, line)"
  , "  return x"
  , "}"
  ]

eqFnName :: Bool -> String
eqFnName optimized = if optimized then "λ2" else "__eq__"

eqFn :: Target -> Bool -> String
eqFn target optimized =
  let fnName = eqFnName optimized
  in
    unlines
      [ getGlobalForTarget target <> "." <> fnName <> " = (l, r) => {"
      , "  if (l === r) {"
      , "    return true;"
      , "  }"
      , "  if (typeof l !== typeof r) {"
      , "    return false;"
      , "  }"
      , "  if (typeof l === `object`) {"
      , "    if (Array.isArray(l)) {"
      , "      return l.reduce((res, _, i) => res && "
      <> fnName
      <> "(l[i], r[i]), true);"
      , "    }"
      , "    const keysL = Object.keys(l);"
      , "    const keysR = Object.keys(r);"
      , "    return keysL.length === keysR.length && keysL.reduce((res, k) => res && "
      <> fnName
      <> "(l[k], r[k]), true);"
      , "  }"
      , "  return l === r;"
      , "}"
      ]

applyDictsFnName :: Bool -> String
applyDictsFnName optimized = if optimized then "λ3" else "__apMtdDicts__"

applyDictsFn :: Target -> Bool -> String
applyDictsFn target optimized =
  let fnName = applyDictsFnName optimized
  in
    unlines
      [ "const __applyMany__ = (f, params) => params.reduce((_f, param) => _f(param), f);"
      , getGlobalForTarget target <> "." <> fnName <> " = (dict, dicts) =>"
      , "  Object.keys(dict).reduce((o, k) => ({ ...o, [k]: __applyMany__(dict[k], dicts) }), {});"
      ]
