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
    <> "\n"
    <> inspectStaticInstances target optimized
    <> "\n"
    <> if coverage then "\n" <> hpFnWrap <> "\n" <> hpLineWrap else ""

inspectStaticInstances :: Target -> Bool -> String
inspectStaticInstances target optimized = unlines
  [ "global.Inspect = {}"
  , ""
  , "Inspect['Integer'] = {}"
  , "Inspect['Integer']['inspect'] = () => x => x"
  , "Inspect['Byte'] = {}"
  , "Inspect['Byte']['inspect'] = () => x => x"
  , "Inspect['Float'] = {}"
  , "Inspect['Float']['inspect'] = () => x => x"
  , "Inspect['String'] = {}"
  , "Inspect['String']['inspect'] = () => x => `\"${x}\"`"
  , "Inspect['Unit'] = {}"
  , "Inspect['Unit']['inspect'] = () => x => `{}`"
  , "Inspect['Boolean'] = {}"
  , "Inspect['Boolean']['inspect'] = () => x => x ? `true` : `false`"
  , "Inspect['a_arr_b'] = {}"
  , "Inspect['a_arr_b']['inspect'] = () => x => `[Function]`"
  , "Inspect['ByteArray'] = {}"
  , "Inspect['ByteArray']['inspect'] = () => bytearray => {"
  , "  let s = '', h = '0123456789ABCDEF'"
  , "  bytearray.forEach((v, index) => {"
  , "    if ((index + 1) % 8 === 0) { s += ' ' }"
  , "    s += h[v >> 4] + h[v & 15]"
  , "  })"
  , "  return `ByteArray(${s})`;"
  , "}"
  , "Inspect['List'] = {}"
  , "let __InspectListinspect = __once__(() => list => `[${list.map(Inspect_a.inspect()).join(\", \")}]`)"
  , "Inspect['List']['inspect'] = () => Inspect_a => {"
  , "  global.Inspect_a = Inspect_a"
  , "  return __InspectListinspect()"
  , "}"
  , "Inspect['Array'] = {}"
  , "let __InspectArrayinspect = __once__(() => array => `Array([${array.map(Inspect_a.inspect()).join(\", \")}])`)"
  , "Inspect['Array']['inspect'] = () => Inspect_a => {"
  , "  global.Inspect_a = Inspect_a"
  , "  return __InspectArrayinspect()"
  , "}"
  , "Inspect['Dictionary'] = {}"
  , "let __InspectDictionaryinspect = __once__(() => dict => {"
  , "  const items = dict.__args[0]"
  , "    .map(([key, value]) => `${Inspect_key.inspect()(key)}: ${Inspect_value.inspect()(value)}`)"
  , "    .join(', ')"
  , "  return `{{ ${items} }}`"
  , "})"
  , "Inspect['Dictionary']['inspect'] = () => Inspect_value => Inspect_key => {"
  , "  global.Inspect_value = Inspect_value"
  , "  global.Inspect_key = Inspect_key"
  , "  return __InspectDictionaryinspect()"
  , "}"
  , "Inspect['Tuple_2'] = {}"
  , "let __InspectTuple_2inspect = __once__(() => ([value1, value2]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}]`)"
  , "Inspect['Tuple_2']['inspect'] = () => Inspect_tuple_2 => Inspect_tuple_1 => {"
  , "  global.Inspect_tuple_1 = Inspect_tuple_1"
  , "  global.Inspect_tuple_2 = Inspect_tuple_2"
  , "  return __InspectTuple_2inspect()"
  , "}"
  , "Inspect['Tuple_3'] = {}"
  , "let __InspectTuple_3inspect = __once__(() => ([value1, value2, value3]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}, ${Inspect_tuple_3.inspect()(value3)}]`)"
  , "Inspect['Tuple_3']['inspect'] = () => Inspect_tuple_3 => Inspect_tuple_2 => Inspect_tuple_1 => {"
  , "  global.Inspect_tuple_1 = Inspect_tuple_1"
  , "  global.Inspect_tuple_2 = Inspect_tuple_2"
  , "  global.Inspect_tuple_3 = Inspect_tuple_3"
  , "  return __InspectTuple_3inspect()"
  , "}"
  , "Inspect['Tuple_4'] = {}"
  , "let __InspectTuple_4inspect = __once__(() => ([value1, value2, value3, value4]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}, ${Inspect_tuple_3.inspect()(value3)}, ${Inspect_tuple_4.inspect()(value4)}]`)"
  , "Inspect['Tuple_4']['inspect'] = () => Inspect_tuple_4 => Inspect_tuple_3 => Inspect_tuple_2 => Inspect_tuple_1 => {"
  , "  global.Inspect_tuple_1 = Inspect_tuple_1"
  , "  global.Inspect_tuple_2 = Inspect_tuple_2"
  , "  global.Inspect_tuple_3 = Inspect_tuple_3"
  , "  global.Inspect_tuple_4 = Inspect_tuple_4"
  , "  return __InspectTuple_4inspect()"
  , "}"
  , "Inspect['Tuple_5'] = {}"
  , "let __InspectTuple_5inspect = __once__(() => ([value1, value2, value3, value4, value5]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}, ${Inspect_tuple_3.inspect()(value3)}, ${Inspect_tuple_4.inspect()(value4)}, ${Inspect_tuple_5.inspect()(value5)}]`)"
  , "Inspect['Tuple_5']['inspect'] = () => Inspect_tuple_5 => Inspect_tuple_4 => Inspect_tuple_3 => Inspect_tuple_2 => Inspect_tuple_1 => {"
  , "  global.Inspect_tuple_1 = Inspect_tuple_1"
  , "  global.Inspect_tuple_2 = Inspect_tuple_2"
  , "  global.Inspect_tuple_3 = Inspect_tuple_3"
  , "  global.Inspect_tuple_4 = Inspect_tuple_4"
  , "  global.Inspect_tuple_5 = Inspect_tuple_5"
  , "  return __InspectTuple_5inspect()"
  , "}"
  , "Inspect['Tuple_6'] = {}"
  , "let __InspectTuple_6inspect = __once__(() => ([value1, value2, value3, value4, value5, value6]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}, ${Inspect_tuple_3.inspect()(value3)}, ${Inspect_tuple_4.inspect()(value4)}, ${Inspect_tuple_5.inspect()(value5)}, ${Inspect_tuple_6.inspect()(value6)}]`)"
  , "Inspect['Tuple_6']['inspect'] = () => Inspect_tuple_6 => Inspect_tuple_5 => Inspect_tuple_4 => Inspect_tuple_3 => Inspect_tuple_2 => Inspect_tuple_1 => {"
  , "  global.Inspect_tuple_1 = Inspect_tuple_1"
  , "  global.Inspect_tuple_2 = Inspect_tuple_2"
  , "  global.Inspect_tuple_3 = Inspect_tuple_3"
  , "  global.Inspect_tuple_4 = Inspect_tuple_4"
  , "  global.Inspect_tuple_5 = Inspect_tuple_5"
  , "  global.Inspect_tuple_6 = Inspect_tuple_6"
  , "  return __InspectTuple_6inspect()"
  , "}"
  , "Inspect['Tuple_7'] = {}"
  , "let __InspectTuple_7inspect = __once__(() => ([value1, value2, value3, value4, value5, value6, value7]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}, ${Inspect_tuple_3.inspect()(value3)}, ${Inspect_tuple_4.inspect()(value4)}, ${Inspect_tuple_5.inspect()(value5)}, ${Inspect_tuple_6.inspect()(value6)}, ${Inspect_tuple_7.inspect()(value7)}]`)"
  , "Inspect['Tuple_7']['inspect'] = () => Inspect_tuple_7 => Inspect_tuple_6 => Inspect_tuple_5 => Inspect_tuple_4 => Inspect_tuple_3 => Inspect_tuple_2 => Inspect_tuple_1 => {"
  , "  global.Inspect_tuple_1 = Inspect_tuple_1"
  , "  global.Inspect_tuple_2 = Inspect_tuple_2"
  , "  global.Inspect_tuple_3 = Inspect_tuple_3"
  , "  global.Inspect_tuple_4 = Inspect_tuple_4"
  , "  global.Inspect_tuple_5 = Inspect_tuple_5"
  , "  global.Inspect_tuple_6 = Inspect_tuple_6"
  , "  global.Inspect_tuple_7 = Inspect_tuple_7"
  , "  return __InspectTuple_7inspect()"
  , "}"
  , "Inspect['Tuple_8'] = {}"
  , "let __InspectTuple_8inspect = __once__(() => ([value1, value2, value3, value4, value5, value6, value7, value8]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}, ${Inspect_tuple_3.inspect()(value3)}, ${Inspect_tuple_4.inspect()(value4)}, ${Inspect_tuple_5.inspect()(value5)}, ${Inspect_tuple_6.inspect()(value6)}, ${Inspect_tuple_7.inspect()(value7)}, ${Inspect_tuple_8.inspect()(value8)}]`)"
  , "Inspect['Tuple_8']['inspect'] = () => Inspect_tuple_8 => Inspect_tuple_7 => Inspect_tuple_6 => Inspect_tuple_5 => Inspect_tuple_4 => Inspect_tuple_3 => Inspect_tuple_2 => Inspect_tuple_1 => {"
  , "  global.Inspect_tuple_1 = Inspect_tuple_1"
  , "  global.Inspect_tuple_2 = Inspect_tuple_2"
  , "  global.Inspect_tuple_3 = Inspect_tuple_3"
  , "  global.Inspect_tuple_4 = Inspect_tuple_4"
  , "  global.Inspect_tuple_5 = Inspect_tuple_5"
  , "  global.Inspect_tuple_6 = Inspect_tuple_6"
  , "  global.Inspect_tuple_7 = Inspect_tuple_7"
  , "  global.Inspect_tuple_8 = Inspect_tuple_8"
  , "  return __InspectTuple_8inspect()"
  , "}"
  , "Inspect['Tuple_9'] = {}"
  , "let __InspectTuple_9inspect = __once__(() => ([value1, value2, value3, value4, value5, value6, value7, value8, value9]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}, ${Inspect_tuple_3.inspect()(value3)}, ${Inspect_tuple_4.inspect()(value4)}, ${Inspect_tuple_5.inspect()(value5)}, ${Inspect_tuple_6.inspect()(value6)}, ${Inspect_tuple_7.inspect()(value7)}, ${Inspect_tuple_8.inspect()(value8)}, ${Inspect_tuple_9.inspect()(value9)}]`)"
  , "Inspect['Tuple_9']['inspect'] = () => Inspect_tuple_9 => Inspect_tuple_8 => Inspect_tuple_7 => Inspect_tuple_6 => Inspect_tuple_5 => Inspect_tuple_4 => Inspect_tuple_3 => Inspect_tuple_2 => Inspect_tuple_1 => {"
  , "  global.Inspect_tuple_1 = Inspect_tuple_1"
  , "  global.Inspect_tuple_2 = Inspect_tuple_2"
  , "  global.Inspect_tuple_3 = Inspect_tuple_3"
  , "  global.Inspect_tuple_4 = Inspect_tuple_4"
  , "  global.Inspect_tuple_5 = Inspect_tuple_5"
  , "  global.Inspect_tuple_6 = Inspect_tuple_6"
  , "  global.Inspect_tuple_7 = Inspect_tuple_7"
  , "  global.Inspect_tuple_8 = Inspect_tuple_8"
  , "  global.Inspect_tuple_9 = Inspect_tuple_9"
  , "  return __InspectTuple_9inspect()"
  , "}"
  , "Inspect['Tuple_10'] = {}"
  , "let __InspectTuple_10inspect = __once__(() => ([value1, value2, value3, value4, value5, value6, value7, value8, value9, value10]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}, ${Inspect_tuple_3.inspect()(value3)}, ${Inspect_tuple_4.inspect()(value4)}, ${Inspect_tuple_5.inspect()(value5)}, ${Inspect_tuple_6.inspect()(value6)}, ${Inspect_tuple_7.inspect()(value7)}, ${Inspect_tuple_8.inspect()(value8)}, ${Inspect_tuple_9.inspect()(value9)}, ${Inspect_tuple_10.inspect()(value10)}]`)"
  , "Inspect['Tuple_10']['inspect'] = () => Inspect_tuple_10 => Inspect_tuple_9 => Inspect_tuple_8 => Inspect_tuple_7 => Inspect_tuple_6 => Inspect_tuple_5 => Inspect_tuple_4 => Inspect_tuple_3 => Inspect_tuple_2 => Inspect_tuple_1 => {"
  , "  global.Inspect_tuple_1 = Inspect_tuple_1"
  , "  global.Inspect_tuple_2 = Inspect_tuple_2"
  , "  global.Inspect_tuple_3 = Inspect_tuple_3"
  , "  global.Inspect_tuple_4 = Inspect_tuple_4"
  , "  global.Inspect_tuple_5 = Inspect_tuple_5"
  , "  global.Inspect_tuple_6 = Inspect_tuple_6"
  , "  global.Inspect_tuple_7 = Inspect_tuple_7"
  , "  global.Inspect_tuple_8 = Inspect_tuple_8"
  , "  global.Inspect_tuple_9 = Inspect_tuple_9"
  , "  global.Inspect_tuple_10 = Inspect_tuple_10"
  , "  return __InspectTuple_10inspect()"
  , "}"
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
