module Generate.JSInternals where

import           Run.Target
import           Generate.Utils
import qualified Utils.Hash                    as Hash
import qualified Data.ByteString.Lazy.Char8    as BLChar8
import           Utils.Hash (generateHashFromPath)


preludeHash :: String
preludeHash = generateHashFromPath "prelude"


generateInternalsModuleContent :: Target -> Bool -> Bool -> String
generateInternalsModuleContent target optimized coverage =
  eqFn target optimized
    <> "\n"
    <> applyDictsFn target optimized
    <> "\n"
    <> onceFn target optimized
    <> "\n"
    <> listToJSArray target
    <> "\n"
    <> jsArrayToList target
    <> "\n"
    <> listConstructorSpreadFn target
    <> "\n"
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


eqFnName :: Bool -> String
eqFnName optimized = if optimized then "λ2" else "__eq__"

eqFn :: Target -> Bool -> String
eqFn target optimized =
  let fnName = eqFnName optimized
  in  unlines
        [ "const __setToArray__ = (s) => {"
        , "  let nextNodes = [s]"
        , "  let result = []"
        , ""
        , "  while (nextNodes.length > 0) {"
        , "    const elem = nextNodes.shift();"
        , ""
        , "    if (elem.__constructor === `SetRBNode`) {"
        , "      result = result.concat(__setToArray__(elem.__args[2]))"
        , "      result.push(elem.__args[1])"
        , "      nextNodes.push(elem.__args[3])"
        , "    }"
        , "  }"
        , ""
        , "  return result;"
        , "}"
        , ""
        , "const __dictToArray__ = (s) => {"
        , "  let nextNodes = [s]"
        , "  let result = []"
        , ""
        , "  while (nextNodes.length > 0) {"
        , "    const elem = nextNodes.shift();"
        , ""
        , "    if (elem.__constructor === `DictRBNode`) {"
        , "      result = result.concat(__dictToArray__(elem.__args[3]))"
        , "      result.push(elem.__args[1])"
        , "      result.push(elem.__args[2])"
        , "      nextNodes.push(elem.__args[4])"
        , "    }"
        , "  }"
        , ""
        , "  return result;"
        , "}"
        , ""
        , "const __eq__Set = (l, r) => {"
        , "  const arrL = __setToArray__(l)"
        , "  const arrR = __setToArray__(r)"
        , ""
        , "  let result = true"
        , "  let index = 0"
        , ""
        , "  while (result && index < arrL.length) {"
        , "    if (!__eq__(arrL[index], arrR[index])) {"
        , "      result = false"
        , "    }"
        , ""
        , "    index = index + 1"
        , "  }"
        , ""
        , "  return result"
        , "}"
        , ""
        , "const __eq__Dict = (l, r) => {"
        , "  const arrL = __dictToArray__(l)"
        , "  const arrR = __dictToArray__(r)"
        , ""
        , "  let result = true"
        , "  let index = 0"
        , ""
        , "  while (result && index < arrL.length) {"
        , "    if (!__eq__(arrL[index], arrR[index])) {"
        , "      result = false"
        , "    }"
        , ""
        , "    index = index + 1"
        , "  }"
        , ""
        , "  return result"
        , "}"
        , ""
        , getGlobalForTarget target <> "." <> fnName <> " = (l, r) => {"
        , "  if (l === r) {"
        , "    return true;"
        , "  }"
        , "  if (typeof l !== typeof r) {"
        , "    return false;"
        , "  }"
        , "if (l === null && r !== null || l !== null && r === null) {"
        , "  return false;"
        , "}"
        , "if (l === null && r === null) {"
        , "  return true;"
        , "}"
        , "  if (typeof l === `object`) {"
        , "if (l.n && l.v) {"
        , "  let result = true;"
        , "  while (l !== null && result) {"
        , "    if (r === null) {"
        , "      result = false"
        , "      break"
        , "    }"
        , "    result = " <> fnName <> "(l.v, r.v)"
        , "    l = l.n"
        , "    r = r.n"
        , "  }"
        , "  return result && r === null;"
        , "}"
        , ""
        , "  if (l.__constructor === `SetRBNode` || l.__constructor === `SetRBEmpty`) {"
        , "    return __eq__Set(l, r)"
        , "  }"
        , "  if (l.__constructor === `DictRBNode` || l.__constructor === `DictRBEmpty`) {"
        , "    return __eq__Dict(l, r)"
        , "  }"
        , ""
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

listToJSArray :: Target -> String
listToJSArray target =
  unlines
    [ getGlobalForTarget target <> "." <> "__listToJSArray__ = (list) => {"
    , "  let res = []"
    , ""
    , "  while (list) {"
    , "    res.push(list.v)"
    , "    list = list.n"
    , "  }"
    , ""
    , "  return res"
    , "}"
    ]

jsArrayToList :: Target -> String
jsArrayToList target =
  unlines
    [ getGlobalForTarget target <> "." <> "__jsArrayToList__ = (arr) => {"
    , "  let res = null"
    , ""
    , "  for (let i = arr.length - 1; i >= 0; i--) {"
    , "    let head = { v: arr[i], n: res }"
    , "    res = head"
    , "  }"
    , ""
    , "  return res"
    , "}"
    ]

listConstructorSpreadFn :: Target -> String
listConstructorSpreadFn target =
  unlines
    [ getGlobalForTarget target <> ".__listCtorSpread__ = (_spread, _next) => {"
    , "  if (_spread === null) {"
    , "    return _next"
    , "  }"
    , ""
    , "  let head = { ..._spread }"
    , "  let result = head"
    , "  while (head.n !== null) {"
    , "    head = head.n"
    , "  }"
    , "  head.n = _next"
    , "  return result"
    , "}"
    ]
