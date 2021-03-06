module Generate.JSInternals where

import           Run.Target
import           Generate.Utils

generateInternalsModuleContent :: Target -> Bool -> Bool -> String
generateInternalsModuleContent target optimized coverage =
  curryFn target optimized
    <> "\n"
    <> eqFn target optimized
    <> "\n"
    <> applyDictsFn target optimized
    <> "\n"
    <> onceFn target optimized
    <> if coverage then "\n" <> hpFnWrap <> "\n" <> hpLineWrap else ""


curryFnName :: Bool -> String
curryFnName optimized = if optimized then "λ1" else "__curry__"

curryFn :: Target -> Bool -> String
curryFn target optimized =
  let fnName = curryFnName optimized
  in  unlines
        [ getGlobalForTarget target <> ".$ = '__$__'"
        , "const PLACEHOLDER = '__$__'"
        , getGlobalForTarget target <> "." <> fnName <> " = fn => {"
        , "  const test = x => x === PLACEHOLDER;"
        , "  return function curried() {"
        , "    const argLength = arguments.length;"
        , "    let args = new Array(argLength);"
        , ""
        , "    for (let i = 0; i < argLength; ++i) {"
        , "      args[i] = arguments[i];"
        , "    }"
        , "    const countNonPlaceholders = toCount => {"
        , "      let count = toCount.length;"
        , "      while (!test(toCount[count])) {"
        , "        count--;"
        , "      }"
        , "      return count;"
        , "    };"
        , "    const length = as => (as.some(test) ? countNonPlaceholders(as) : as.length);"
        , "    function saucy() {"
        , "      const arg2Length = arguments.length;"
        , "      const args2 = new Array(arg2Length);"
        , "      for (let j = 0; j < arg2Length; ++j) {"
        , "        args2[j] = arguments[j];"
        , "      }"
        , ""
        , "      return curried.apply("
        , "        this,"
        , "        args"
        , "          .map(y =>"
        , "            test(y) && args2[0]"
        , "              ? args2.shift()"
        , "              : y"
        , "          )"
        , "          .concat(args2)"
        , "      );"
        , "    }"
        , ""
        , "    if (length(args) >= fn.length) {"
        , "      const currentArgs = args.slice(0, fn.length);"
        , "      const result = fn.apply(this, currentArgs);"
        , "      const nextArgs = args.slice(fn.length);"
        , ""
        , "      if (typeof result === \"function\" && length(nextArgs) > 0) {"
        , "        return result.apply(this, nextArgs);"
        , "      } else {"
        , "        return result;"
        , "      }"
        , "    } else {"
        , "      return saucy;"
        , "    }"
        , "  };"
        , "};"
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
hpLineWrap =
  unlines ["global.__hpLineWrap = (astPath, line, x) => {", "  __hp(astPath, 'line', line, line)", "  return x", "}"]

eqFnName :: Bool -> String
eqFnName optimized = if optimized then "λ2" else "__eq__"

eqFn :: Target -> Bool -> String
eqFn target optimized =
  let fnName = eqFnName optimized
  in  unlines
        [ getGlobalForTarget target <> "." <> fnName <> " = (l, r) => {"
        , "  if (l === r) {"
        , "    return true;"
        , "  }"
        , "  if (typeof l !== typeof r) {"
        , "    return false;"
        , "  }"
        , "  if (typeof l === `object`) {"
        , "    if (Array.isArray(l)) {"
        , "      return l.length === r.length && l.reduce((res, _, i) => res && " <> fnName <> "(l[i], r[i]), true);"
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
  in  unlines
        [ "const __applyMany__ = (f, params) => params.reduce((_f, param) => _f(param), f);"
        , getGlobalForTarget target <> "." <> fnName <> " = (dict, dicts) =>"
        , "  Object.keys(dict).reduce((o, k) => ({ ...o, [k]: () => __applyMany__(dict[k](), dicts) }), {});"
        ]

onceFnName :: Bool -> String
onceFnName optimized = if optimized then "λ4" else "__once__"

onceFn :: Target -> Bool -> String
onceFn target optimized =
  let fnName = onceFnName optimized
  in  unlines
        [ getGlobalForTarget target <> "." <> fnName <> " = (fn, context) => {\n"
        , "    var result;\n"
        , "    return function() {\n"
        , "        if (fn) {\n"
        , "            result = fn.apply(context || this, arguments);\n"
        , "            fn = null;\n"
        , "        }\n"
        , "        return result;\n"
        , "    };\n"
        , "}\n"
        ]
