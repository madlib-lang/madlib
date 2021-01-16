module Compile.JSInternals where


generateInternalsModuleContent :: Bool -> Bool -> String
generateInternalsModuleContent optimized coverage =
  curryFn optimized
    <> "\n"
    <> eqFn optimized
    <> "\n"
    <> applyDictsFn optimized
    <> if coverage then "\n" <> hpFnWrap <> "\n" <> hpLineWrap else ""


curryFnName :: Bool -> String
curryFnName optimized = if optimized then "λ1" else "__curry__"

curryFn :: Bool -> String
curryFn optimized =
  let fnName = curryFnName optimized
  in
    unlines
      [ "const toString = (fn, args = []) => () => ("
      , "  `curry(${fn.toString()})${args.length > 0 ? `(${args.join(`,`)})` : ``}`"
      , ")"
      , "global." <> fnName <> " = (fn) => {"
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

eqFn :: Bool -> String
eqFn optimized =
  let fnName = eqFnName optimized
  in
    unlines
      [ "global." <> fnName <> " = (l, r) => {"
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

applyDictsFn :: Bool -> String
applyDictsFn optimized =
  let fnName = applyDictsFnName optimized
  in
    unlines
      [ "const __applyMany__ = (f, params) => params.reduce((_f, param) => _f(param), f);"
      , "global." <> fnName <> " = (dict, dicts) =>"
      , "  Object.keys(dict).reduce((o, k) => ({ ...o, [k]: __applyMany__(dict[k], dicts) }), {});"
      ]
