window.__String = window.String;


window.__eq__ = (l, r) => {
  if (l === r) {
    return true;
  }
  if (typeof l !== typeof r) {
    return false;
  }
if (l === null && r !== null || l !== null && r === null) {
  return false;
}
if (l === null && r === null) {
  return true;
}
  if (typeof l === `object`) {
    if (l.n && l.v) {
      let result = true;
      while (l !== null && result) {
        result = __eq__(l.v, r.v);
        l = l.n;
        r = r.n;
      }
      return result && r === null;
    }
    const keysL = Object.keys(l);
    const keysR = Object.keys(r);
    return keysL.length === keysR.length && keysL.reduce((res, k) => res && __eq__(l[k], r[k]), true);
  }
  return l === r;
}

const __applyMany__ = (f, params) => params.reduce((_f, param) => _f(param), f);
window.__apMtdDicts__ = (dict, dicts) =>
  Object.keys(dict).reduce((o, k) => ({ ...o, [k]: () => __applyMany__(dict[k](), dicts) }), {});

window.__once__ = (fn, context) => {

    var result;

    return function() {

        if (fn) {

            result = fn.apply(context || this, arguments);

            fn = null;

        }

        return result;

    };

}


window.Inspect = {};

Inspect['Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Inspect['Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d']['inspect'] = () => x => '' + x;

Inspect['Byte_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Inspect['Byte_5b7ebeeaa5acfe1eeea5a9e9845b152d']['inspect'] = () => x => { x = x % 256; return ('0' + (x < 0 ? 256 + x : x).toString(16)).slice(-2).toUpperCase(); };

Inspect['Float_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Inspect['Float_5b7ebeeaa5acfe1eeea5a9e9845b152d']['inspect'] = () => x => '' + x;

Inspect['String_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Inspect['String_5b7ebeeaa5acfe1eeea5a9e9845b152d']['inspect'] = () => x => {
  const escapeChar = (c) => {
    if (c === '\n') {
      return `\\n`;
    } else if (c === '\t') {
      return `\\t`;
    } else if (c === '\r') {
      return `\\t`;
    } else {
      return c;
    }
  }
  return `"${x.split('').map(escapeChar).join('')}"`
}

Inspect['Char_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Inspect['Char_5b7ebeeaa5acfe1eeea5a9e9845b152d']['inspect'] = () => x => {
  if (x === '\n') {
    return `'\\n'`;
  } else if (x === '\t') {
    return `'\\t'`;
  } else if (x === '\r') {
    return `'\\t'`;
  } else if (x === '\c') {
    return `'\\c'`;
  } else {
    return `'${x}'`;
  }
};

Inspect['Unit_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Inspect['Unit_5b7ebeeaa5acfe1eeea5a9e9845b152d']['inspect'] = () => () => `{}`;

Inspect['Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Inspect['Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d']['inspect'] = () => x => x ? `true` : `false`;

Inspect['a_arr_b'] = {};
Inspect['a_arr_b']['inspect'] = () => x => `[Function]`;

Inspect['ByteArray_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Inspect['ByteArray_5b7ebeeaa5acfe1eeea5a9e9845b152d']['inspect'] = () => bytearray => {
  let s = '', h = '0123456789ABCDEF'
  bytearray.forEach((v, index) => {
    if ((index + 1) % 8 === 0) { s += ' ' }
    s += h[v >> 4] + h[v & 15]
  })
  return `ByteArray(${s})`;
};

Inspect['List_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Inspect['List_5b7ebeeaa5acfe1eeea5a9e9845b152d']['inspect'] = () => Inspect_a => list => {

  let items = [];

  while (list !== null) {
    items.push(Inspect_a.inspect()(list.v));
    list = list.n;
  }
  return "[" + items.join(", ") + "]"
};

Inspect['Array_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Inspect['Array_5b7ebeeaa5acfe1eeea5a9e9845b152d']['inspect'] = () => Inspect_a => array => `Array([${array.map(Inspect_a.inspect()).join(", ")}])`;

Inspect['Dictionary_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Inspect['Dictionary_5b7ebeeaa5acfe1eeea5a9e9845b152d']['inspect'] = () => Inspect_value => Inspect_key => dict => {
  let list = dict.__args[0]
  let items = [];

  while (list !== null) {
    items.push(list.v);
    list = list.n;
  }
  const renderedItems = items
    .map(([key, value]) => `${Inspect_key.inspect()(key)}: ${Inspect_value.inspect()(value)}`)
    .join(', ')
  return items.length === 0 ? '{{}}' : `{{ ${renderedItems} }}`
};

Inspect['Tuple_2'] = {};
Inspect['Tuple_2']['inspect'] = () => Inspect_tuple_2 => Inspect_tuple_1 => ([value1, value2]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}]`;

Inspect['Tuple_3'] = {};
Inspect['Tuple_3']['inspect'] = () => Inspect_tuple_3 => Inspect_tuple_2 => Inspect_tuple_1 => ([value1, value2, value3]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}, ${Inspect_tuple_3.inspect()(value3)}]`;

Inspect['Tuple_4'] = {};
Inspect['Tuple_4']['inspect'] = () => Inspect_tuple_4 => Inspect_tuple_3 => Inspect_tuple_2 => Inspect_tuple_1 => ([value1, value2, value3, value4]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}, ${Inspect_tuple_3.inspect()(value3)}, ${Inspect_tuple_4.inspect()(value4)}]`;

Inspect['Tuple_5'] = {};
Inspect['Tuple_5']['inspect'] = () => Inspect_tuple_5 => Inspect_tuple_4 => Inspect_tuple_3 => Inspect_tuple_2 => Inspect_tuple_1 => ([value1, value2, value3, value4, value5]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}, ${Inspect_tuple_3.inspect()(value3)}, ${Inspect_tuple_4.inspect()(value4)}, ${Inspect_tuple_5.inspect()(value5)}]`;

Inspect['Tuple_6'] = {};
Inspect['Tuple_6']['inspect'] = () => Inspect_tuple_6 => Inspect_tuple_5 => Inspect_tuple_4 => Inspect_tuple_3 => Inspect_tuple_2 => Inspect_tuple_1 => ([value1, value2, value3, value4, value5, value6]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}, ${Inspect_tuple_3.inspect()(value3)}, ${Inspect_tuple_4.inspect()(value4)}, ${Inspect_tuple_5.inspect()(value5)}, ${Inspect_tuple_6.inspect()(value6)}]`;

Inspect['Tuple_7'] = {};
Inspect['Tuple_7']['inspect'] = () => Inspect_tuple_7 => Inspect_tuple_6 => Inspect_tuple_5 => Inspect_tuple_4 => Inspect_tuple_3 => Inspect_tuple_2 => Inspect_tuple_1 => ([value1, value2, value3, value4, value5, value6, value7]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}, ${Inspect_tuple_3.inspect()(value3)}, ${Inspect_tuple_4.inspect()(value4)}, ${Inspect_tuple_5.inspect()(value5)}, ${Inspect_tuple_6.inspect()(value6)}, ${Inspect_tuple_7.inspect()(value7)}]`;

Inspect['Tuple_8'] = {};
Inspect['Tuple_8']['inspect'] = () => Inspect_tuple_8 => Inspect_tuple_7 => Inspect_tuple_6 => Inspect_tuple_5 => Inspect_tuple_4 => Inspect_tuple_3 => Inspect_tuple_2 => Inspect_tuple_1 => ([value1, value2, value3, value4, value5, value6, value7, value8]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}, ${Inspect_tuple_3.inspect()(value3)}, ${Inspect_tuple_4.inspect()(value4)}, ${Inspect_tuple_5.inspect()(value5)}, ${Inspect_tuple_6.inspect()(value6)}, ${Inspect_tuple_7.inspect()(value7)}, ${Inspect_tuple_8.inspect()(value8)}]`;

Inspect['Tuple_9'] = {};
Inspect['Tuple_9']['inspect'] = () => Inspect_tuple_9 => Inspect_tuple_8 => Inspect_tuple_7 => Inspect_tuple_6 => Inspect_tuple_5 => Inspect_tuple_4 => Inspect_tuple_3 => Inspect_tuple_2 => Inspect_tuple_1 => ([value1, value2, value3, value4, value5, value6, value7, value8, value9]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}, ${Inspect_tuple_3.inspect()(value3)}, ${Inspect_tuple_4.inspect()(value4)}, ${Inspect_tuple_5.inspect()(value5)}, ${Inspect_tuple_6.inspect()(value6)}, ${Inspect_tuple_7.inspect()(value7)}, ${Inspect_tuple_8.inspect()(value8)}, ${Inspect_tuple_9.inspect()(value9)}]`;

Inspect['Tuple_10'] = {};
Inspect['Tuple_10']['inspect'] = () => Inspect_tuple_10 => Inspect_tuple_9 => Inspect_tuple_8 => Inspect_tuple_7 => Inspect_tuple_6 => Inspect_tuple_5 => Inspect_tuple_4 => Inspect_tuple_3 => Inspect_tuple_2 => Inspect_tuple_1 => ([value1, value2, value3, value4, value5, value6, value7, value8, value9, value10]) => `#[${Inspect_tuple_1.inspect()(value1)}, ${Inspect_tuple_2.inspect()(value2)}, ${Inspect_tuple_3.inspect()(value3)}, ${Inspect_tuple_4.inspect()(value4)}, ${Inspect_tuple_5.inspect()(value5)}, ${Inspect_tuple_6.inspect()(value6)}, ${Inspect_tuple_7.inspect()(value7)}, ${Inspect_tuple_8.inspect()(value8)}, ${Inspect_tuple_9.inspect()(value9)}, ${Inspect_tuple_10.inspect()(value10)}]`;

window.__listToJSArray__ = (list) => {
  let res = []

  while (list) {
    res.push(list.v)
    list = list.n
  }

  return res
}

window.__jsArrayToList__ = (arr) => {
  let res = null

  for (let i = arr.length - 1; i >= 0; i--) {
    let head = { v: arr[i], n: res }
    res = head
  }

  return res
}

window.__listCtorSpread__ = (_spread, _next) => {
  if (_spread === null) {
    return _next;
  }

  let head = _spread;
  let result = _spread;
  while (head.n !== null) {
    head = head.n;
  }
  head.n = _next;
  return result;
}

