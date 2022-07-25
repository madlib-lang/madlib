module Generate.JSInternals where

import           Run.Target
import           Generate.Utils
import qualified Utils.Hash                    as Hash
import qualified Data.ByteString.Lazy.Char8    as BLChar8



generateHashFromPath :: FilePath -> String
generateHashFromPath =
  Hash.hash . BLChar8.pack


preludeHash :: String
preludeHash = generateHashFromPath "prelude"


generateInternalsModuleContent :: Target -> Bool -> Bool -> String
generateInternalsModuleContent target optimized coverage =
  aliasStringGlobal target
    <> "\n"
    <> eqFn target optimized
    <> "\n"
    <> applyDictsFn target optimized
    <> "\n"
    <> onceFn target optimized
    <> "\n"
    <> inspectStaticInstances target optimized
    <> "\n"
    <> listToJSArray target
    <> "\n"
    <> listConstructorSpreadFn target
    <> "\n"
    <> if coverage then "\n" <> hpFnWrap <> "\n" <> hpLineWrap else ""


aliasStringGlobal :: Target -> String
aliasStringGlobal target =
  let global = getGlobalForTarget target
  in global <> ".__String = "<> global <> ".String;\n\n"

-- Do we need this?
aliasMathGlobal :: Target -> String
aliasMathGlobal target =
  let global = getGlobalForTarget target
  in global <> ".__Math = "<> global <> ".Math;\n\n"


inspectStaticInstances :: Target -> Bool -> String
inspectStaticInstances target _ = unlines
  [ getGlobalForTarget target <> ".Inspect = {};"
  , ""
  , "Inspect['Integer_"<>preludeHash<>"'] = {};"
  , "Inspect['Integer_"<>preludeHash<>"']['inspect'] = () => x => '' + x;"
  , ""
  , "Inspect['Byte_"<>preludeHash<>"'] = {};"
  , "Inspect['Byte_"<>preludeHash<>"']['inspect'] = () => x => { x = x % 256; return ('0' + (x < 0 ? 256 + x : x).toString(16)).slice(-2).toUpperCase(); };"
  , ""
  , "Inspect['Float_"<>preludeHash<>"'] = {};"
  , "Inspect['Float_"<>preludeHash<>"']['inspect'] = () => x => x;"
  , ""
  , "Inspect['String_"<>preludeHash<>"'] = {};"
  -- , "Inspect['String']['inspect'] = () => x => `\"${x.split('').map(Inspect.Char.inspect()).join('')}\"`;"
  , "Inspect['String_"<>preludeHash<>"']['inspect'] = () => x => {"
  , "  const escapeChar = (c) => {"
  , "    if (c === '\\n') {"
  , "      return `\\\\n`;"
  , "    } else if (c === '\\t') {"
  , "      return `\\\\t`;"
  , "    } else if (c === '\\r') {"
  , "      return `\\\\t`;"
  , "    } else {"
  , "      return c;"
  , "    }"
  , "  }"
  , "  return `\"${x.split('').map(escapeChar).join('')}\"`"
  , "}"
  , ""
  , "Inspect['Char_"<>preludeHash<>"'] = {};"
  , "Inspect['Char_"<>preludeHash<>"']['inspect'] = () => x => {"
  , "  if (x === '\\n') {"
  , "    return `'\\\\n'`;"
  , "  } else if (x === '\\t') {"
  , "    return `'\\\\t'`;"
  , "  } else if (x === '\\r') {"
  , "    return `'\\\\t'`;"
  , "  } else if (x === '\\c') {"
  , "    return `'\\\\c'`;"
  , "  } else {"
  , "    return `'${x}'`;"
  , "  }"
  , "};"
  , ""
  , "Inspect['Unit_"<>preludeHash<>"'] = {};"
  , "Inspect['Unit_"<>preludeHash<>"']['inspect'] = () => () => `{}`;"
  , ""
  , "Inspect['Boolean_"<>preludeHash<>"'] = {};"
  , "Inspect['Boolean_"<>preludeHash<>"']['inspect'] = () => x => x ? `true` : `false`;"
  , ""
  , "Inspect['a_arr_b'] = {};"
  , "Inspect['a_arr_b']['inspect'] = () => x => `[Function]`;"
  , ""
  , "Inspect['ByteArray_"<>preludeHash<>"'] = {};"
  , "Inspect['ByteArray_"<>preludeHash<>"']['inspect'] = () => bytearray => {"
  , "  let s = '', h = '0123456789ABCDEF'"
  , "  bytearray.forEach((v, index) => {"
  , "    if ((index + 1) % 8 === 0) { s += ' ' }"
  , "    s += h[v >> 4] + h[v & 15]"
  , "  })"
  , "  return `ByteArray(${s})`;"
  , "};"
  , ""
  , "Inspect['List_"<>preludeHash<>"'] = {};"
  , "Inspect['List_"<>preludeHash<>"']['inspect'] = () => Inspect_a => list => {\n"
  , "  let items = [];\n"
  , "  while (list !== null) {"
  , "    items.push(Inspect_a.inspect()(list.v));"
  , "    list = list.n;"
  , "  }"
  , "  return \"[\" + items.join(\", \") + \"]\""
  , "};"
  , ""
  , "Inspect['Array_"<>preludeHash<>"'] = {};"
  , "Inspect['Array_"<>preludeHash<>"']['inspect'] = () => Inspect_a => array => `Array([${array.map(Inspect_a.inspect()).join(\", \")}])`;"
  , ""
  , "Inspect['Dictionary_"<>preludeHash<>"'] = {};"
  , "Inspect['Dictionary_"<>preludeHash<>"']['inspect'] = () => Inspect_value => Inspect_key => dict => {"
  , "  let list = dict.__args[0]"
  , "  let items = [];\n"
  , "  while (list !== null) {"
  , "    items.push(list.v);"
  , "    list = list.n;"
  , "  }"
  , "  const renderedItems = items"
  , "    .map(([key, value]) => `${Inspect_key.inspect()(key)}: ${Inspect_value.inspect()(value)}`)"
  , "    .join(', ')"
  , "  return items.length === 0 ? '{{}}' : `{{ ${renderedItems} }}`"
  , "};"
  , ""
  , "Inspect['Tuple_2'] = {};"
  , "Inspect['Tuple_2']['inspect'] = () => Inspect_tuple_2 => Inspect_tuple_1 => ([value1, value2]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}]`;"
  , ""
  , "Inspect['Tuple_3'] = {};"
  , "Inspect['Tuple_3']['inspect'] = () => Inspect_tuple_3 => Inspect_tuple_2 => Inspect_tuple_1 => ([value1, value2, value3]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}, ${Inspect_tuple_3.inspect()(value3)}]`;"
  , ""
  , "Inspect['Tuple_4'] = {};"
  , "Inspect['Tuple_4']['inspect'] = () => Inspect_tuple_4 => Inspect_tuple_3 => Inspect_tuple_2 => Inspect_tuple_1 => ([value1, value2, value3, value4]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}, ${Inspect_tuple_3.inspect()(value3)}, ${Inspect_tuple_4.inspect()(value4)}]`;"
  , ""
  , "Inspect['Tuple_5'] = {};"
  , "Inspect['Tuple_5']['inspect'] = () => Inspect_tuple_5 => Inspect_tuple_4 => Inspect_tuple_3 => Inspect_tuple_2 => Inspect_tuple_1 => ([value1, value2, value3, value4, value5]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}, ${Inspect_tuple_3.inspect()(value3)}, ${Inspect_tuple_4.inspect()(value4)}, ${Inspect_tuple_5.inspect()(value5)}]`;"
  , ""
  , "Inspect['Tuple_6'] = {};"
  , "Inspect['Tuple_6']['inspect'] = () => Inspect_tuple_6 => Inspect_tuple_5 => Inspect_tuple_4 => Inspect_tuple_3 => Inspect_tuple_2 => Inspect_tuple_1 => ([value1, value2, value3, value4, value5, value6]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}, ${Inspect_tuple_3.inspect()(value3)}, ${Inspect_tuple_4.inspect()(value4)}, ${Inspect_tuple_5.inspect()(value5)}, ${Inspect_tuple_6.inspect()(value6)}]`;"
  , ""
  , "Inspect['Tuple_7'] = {};"
  , "Inspect['Tuple_7']['inspect'] = () => Inspect_tuple_7 => Inspect_tuple_6 => Inspect_tuple_5 => Inspect_tuple_4 => Inspect_tuple_3 => Inspect_tuple_2 => Inspect_tuple_1 => ([value1, value2, value3, value4, value5, value6, value7]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}, ${Inspect_tuple_3.inspect()(value3)}, ${Inspect_tuple_4.inspect()(value4)}, ${Inspect_tuple_5.inspect()(value5)}, ${Inspect_tuple_6.inspect()(value6)}, ${Inspect_tuple_7.inspect()(value7)}]`;"
  , ""
  , "Inspect['Tuple_8'] = {};"
  , "Inspect['Tuple_8']['inspect'] = () => Inspect_tuple_8 => Inspect_tuple_7 => Inspect_tuple_6 => Inspect_tuple_5 => Inspect_tuple_4 => Inspect_tuple_3 => Inspect_tuple_2 => Inspect_tuple_1 => ([value1, value2, value3, value4, value5, value6, value7, value8]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}, ${Inspect_tuple_3.inspect()(value3)}, ${Inspect_tuple_4.inspect()(value4)}, ${Inspect_tuple_5.inspect()(value5)}, ${Inspect_tuple_6.inspect()(value6)}, ${Inspect_tuple_7.inspect()(value7)}, ${Inspect_tuple_8.inspect()(value8)}]`;"
  , ""
  , "Inspect['Tuple_9'] = {};"
  , "Inspect['Tuple_9']['inspect'] = () => Inspect_tuple_9 => Inspect_tuple_8 => Inspect_tuple_7 => Inspect_tuple_6 => Inspect_tuple_5 => Inspect_tuple_4 => Inspect_tuple_3 => Inspect_tuple_2 => Inspect_tuple_1 => ([value1, value2, value3, value4, value5, value6, value7, value8, value9]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}, ${Inspect_tuple_3.inspect()(value3)}, ${Inspect_tuple_4.inspect()(value4)}, ${Inspect_tuple_5.inspect()(value5)}, ${Inspect_tuple_6.inspect()(value6)}, ${Inspect_tuple_7.inspect()(value7)}, ${Inspect_tuple_8.inspect()(value8)}, ${Inspect_tuple_9.inspect()(value9)}]`;"
  , ""
  , "Inspect['Tuple_10'] = {};"
  , "Inspect['Tuple_10']['inspect'] = () => Inspect_tuple_10 => Inspect_tuple_9 => Inspect_tuple_8 => Inspect_tuple_7 => Inspect_tuple_6 => Inspect_tuple_5 => Inspect_tuple_4 => Inspect_tuple_3 => Inspect_tuple_2 => Inspect_tuple_1 => ([value1, value2, value3, value4, value5, value6, value7, value8, value9, value10]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}, ${Inspect_tuple_3.inspect()(value3)}, ${Inspect_tuple_4.inspect()(value4)}, ${Inspect_tuple_5.inspect()(value5)}, ${Inspect_tuple_6.inspect()(value6)}, ${Inspect_tuple_7.inspect()(value7)}, ${Inspect_tuple_8.inspect()(value8)}, ${Inspect_tuple_9.inspect()(value9)}, ${Inspect_tuple_10.inspect()(value10)}]`;"
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
        , "if (l === null && r !== null || l !== null && r === null) {"
        , "  return false;"
        , "}"
        , "if (l === null && r === null) {"
        , "  return true;"
        , "}"
        , "  if (typeof l === `object`) {"
        , "    if (l.n && l.v) {" -- List
        , "      let result = true;"
        , "      while (l !== null && result) {"
        , "        result = " <> fnName <> "(l.v, r.v);"
        , "        l = l.n;"
        , "        r = r.n;"
        , "      }"
        , "      return result && r === null;"
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

listConstructorSpreadFn :: Target -> String
listConstructorSpreadFn target =
  unlines
    [ getGlobalForTarget target <> ".__listCtorSpread__ = (_spread, _next) => {"
    , "  if (_spread === null) {"
    , "    return _next;"
    , "  }"
    , ""
    , "  let head = _spread;"
    , "  let result = _spread;"
    , "  while (head.n !== null) {"
    , "    head = head.n;"
    , "  }"
    , "  head.n = _next;"
    , "  return result;"
    , "}"
    ]
