module Generate.JSInternals where

import           Run.Target
import           Generate.Utils

generateInternalsModuleContent :: Target -> Bool -> Bool -> String
generateInternalsModuleContent target optimized coverage =
  eqFn target optimized
    <> "\n"
    <> dictCtorFn target optimized
    <> "\n"
    <> applyDictsFn target optimized
    <> "\n"
    <> onceFn target optimized
    <> if coverage then "\n" <> hpFnWrap <> "\n" <> hpLineWrap else ""


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

dictCtorFn :: Target -> Bool -> String
dictCtorFn target optimized =
  let fnName = "__dict_ctor__"
      eqFn   = eqFnName optimized
  in  unlines
        [ getGlobalForTarget target <> "." <> fnName <> " = (items) => {"
        , "  let addedItems = [];"
        , "  for(const item of items) {"
        , "    if (addedItems.find(([key, _]) => " ++ eqFn ++ "(key, item[0])) === undefined) {"
        , "      addedItems.push(item);"
        , "    }"
        , "  }"
        , "  return { __constructor: \"Dictionary\", __args: [addedItems] };"
        , "}"
        ]

-- fromList :: List (<k, v>) -> Dictionary k v
-- export fromList = pipe(
--   L.uniqueBy((a, b) => T.fst(a) == T.fst(b)),
--   Dictionary
-- )

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
