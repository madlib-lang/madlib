(function (global, factory) {
  typeof exports === 'object' && typeof module !== 'undefined' ? module.exports = factory() :
  typeof define === 'function' && define.amd ? define(factory) :
  (global = typeof globalThis !== 'undefined' ? globalThis : global || self, global.exe = factory());
})(this, (function () { 'use strict';

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
  };

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

  };


  window.Inspect = {};

  Inspect['Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
  Inspect['Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d']['inspect'] = () => x => '' + x;

  Inspect['Byte_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
  Inspect['Byte_5b7ebeeaa5acfe1eeea5a9e9845b152d']['inspect'] = () => x => { x = x % 256; return ('0' + (x < 0 ? 256 + x : x).toString(16)).slice(-2).toUpperCase(); };

  Inspect['Float_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
  Inspect['Float_5b7ebeeaa5acfe1eeea5a9e9845b152d']['inspect'] = () => x => x;

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
    };
    return `"${x.split('').map(escapeChar).join('')}"`
  };

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
    let s = '', h = '0123456789ABCDEF';
    bytearray.forEach((v, index) => {
      if ((index + 1) % 8 === 0) { s += ' '; }
      s += h[v >> 4] + h[v & 15];
    });
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
    let list = dict.__args[0];
    let items = [];

    while (list !== null) {
      items.push(list.v);
      list = list.n;
    }
    const renderedItems = items
      .map(([key, value]) => `${Inspect_key.inspect()(key)}: ${Inspect_value.inspect()(value)}`)
      .join(', ');
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
    let res = [];

    while (list) {
      res.push(list.v);
      list = list.n;
    }

    return res
  };

  window.__jsArrayToList__ = (arr) => {
    let res = null;

    for (let i = arr.length - 1; i >= 0; i--) {
      let head = { v: arr[i], n: res };
      res = head;
    }

    return res
  };

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
  };

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Function.mad
  let complement = (fn => x => !(fn(x)));
  let always = (a => b => a);
  let identity = (a => a);
  let equals = (val => a => __eq__(val, a));
  let notEquals = (val => a => !__eq__(val, a));
  let ifElse = (predicate => truthy => falsy => value => (predicate(value) ? truthy(value) : falsy(value)));
  let when = (predicate => truthy => value => ifElse(predicate)(truthy)(always(value))(value));
  let not = (b => !(b));
  let noop = (_ => ({ __constructor: "Unit", __args: [] }));
  let flip = (f => b => a => f(a)(b));
  let any = (predicate => list => {
      let $_result_;
      let $_continue_ = true;
      let $$predicate = predicate;
      let $$list = list;

      while($_continue_) {
        let $predicate = $$predicate;
        let $list = $$list;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ === null) {
      ($_result_ = false);
    }
    else if (__x__ !== null && true && true) {
      let { v: x, n: xs } = __x__;
      ($predicate(x) ? ($_result_ = true) : ($$predicate = $predicate, $$list = xs, $_continue_ = true));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($list);
      }
      return $_result_;
  });
  let all = (predicate => list => {
      let $_result_;
      let $_continue_ = true;
      let $$predicate = predicate;
      let $$list = list;

      while($_continue_) {
        let $predicate = $$predicate;
        let $list = $$list;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ === null) {
      ($_result_ = true);
    }
    else if (__x__ !== null && true && true) {
      let { v: x, n: xs } = __x__;
      (!($predicate(x)) ? ($_result_ = false) : ($$predicate = $predicate, $$list = xs, $_continue_ = true));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($list);
      }
      return $_result_;
  });
  let either = (predA => predB => x => predA(x) || predB(x));
  let both = (predA => predB => x => predA(x) && predB(x));

  const nativeMemoize = (fn) => {
    let cache = {};
    return (a) => {
      const key = JSON.stringify(a);
      if (!cache[key]) {
        cache[key] = fn.apply(undefined, [a]);
      }
      return cache[key]
    }
  }
  ;
  let memoize = (fn =>  nativeMemoize(fn) );
  var Fun = { complement, always, identity, equals, notEquals, ifElse, when, not, noop, flip, any, all, either, both, memoize };

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Functor.mad

  window.Functor = {};
  let mapL = (Functor_l11) => (_P_ => Functor_l11.map()(always(_P_)));

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Applicative.mad

  window.Applicative = {};
  let apL = (Functor_b27) => (Applicative_b27) => (a => b => Applicative_b27.ap()(Functor_b27.map()(always)(a))(b));

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Monad.mad

  window.Monad = {};
  let andDo = (Functor_q42) => (Applicative_q42) => (Monad_q42) => (b => a => Monad_q42.chain()((_ => b))(a));

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Alternative.mad

  window.Alternative = {};

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Show.mad
  window.Show = {};

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Maybe.mad

  let Just = (a => ({ __constructor: "Just", __args: [ a ] }));
  let Nothing = ({ __constructor: "Nothing", __args: [  ] });
  Inspect['Maybe_17998c1898470349f86806803a2b29f2'] = {};
  Inspect['Maybe_17998c1898470349f86806803a2b29f2']['inspect'] = () => (Inspect_t45) => (__$a__ => ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      let a0 = __x__.__args[0];
      return `Just(` + Inspect_t45.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "Nothing") {
      return `Nothing`;
    }
    else {
      return `Unknown`;
    }
  })(__$a__));
  Functor['Maybe_17998c1898470349f86806803a2b29f2'] = {};
  Functor['Maybe_17998c1898470349f86806803a2b29f2']['map'] = () => (f => __x__ => ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      let x = __x__.__args[0];
      return Just(f(x));
    }
    else if (__x__.__constructor === "Nothing") {
      return Nothing;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  Applicative['Maybe_17998c1898470349f86806803a2b29f2'] = {};
  Applicative['Maybe_17998c1898470349f86806803a2b29f2']['ap'] = () => (mf => mx => ((__x__) => {
    if (__x__.length === 2 && __x__[0].__constructor === "Just" && true && __x__[1].__constructor === "Just" && true) {
      let [{ __args: [f]},{ __args: [x]}] = __x__;
      return Applicative.Maybe_17998c1898470349f86806803a2b29f2.pure()(f(x));
    }
    else {
      return Nothing;
    }
  })(([mf, mx])));
  Applicative['Maybe_17998c1898470349f86806803a2b29f2']['pure'] = () => Just;
  Monad['Maybe_17998c1898470349f86806803a2b29f2'] = {};
  Monad['Maybe_17998c1898470349f86806803a2b29f2']['chain'] = () => (f => m => ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      let x = __x__.__args[0];
      return f(x);
    }
    else if (__x__.__constructor === "Nothing") {
      return Nothing;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(m));
  Monad['Maybe_17998c1898470349f86806803a2b29f2']['of'] = () => Applicative.Maybe_17998c1898470349f86806803a2b29f2.pure();
  Show['Maybe_17998c1898470349f86806803a2b29f2'] = {};
  Show['Maybe_17998c1898470349f86806803a2b29f2']['show'] = () => (Show_p119) => (__x__ => ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      let a = __x__.__args[0];
      return `Just ` + Show_p119.show()(a);
    }
    else if (__x__.__constructor === "Nothing") {
      return `Nothing`;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  let fromMaybe = (or => __x__ => ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      let a = __x__.__args[0];
      return a;
    }
    else if (__x__.__constructor === "Nothing") {
      return or;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  let isJust = (__x__ => ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      return true;
    }
    else if (__x__.__constructor === "Nothing") {
      return false;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  var Maybe = { fromMaybe, isJust, Just, Nothing };

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Compare.mad
  window.Comparable = {};
  let MORE = 1;
  let LESS = -1;
  let EQUAL = 0;
  let eq = (Comparable_w22) => (a => b => __eq__(Comparable_w22.compare()(a)(b), EQUAL));
  let gt = (Comparable_q42) => (a => b => __eq__(Comparable_q42.compare()(a)(b), MORE));

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Scan.mad

  window.Scan = {};

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Number.mad

  Comparable['Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
  Comparable['Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d']['compare'] = () => (a => b => (a > b ? MORE : (__eq__(a, b) ? EQUAL : LESS)));
  Comparable['Byte_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
  Comparable['Byte_5b7ebeeaa5acfe1eeea5a9e9845b152d']['compare'] = () => (a => b => (a > b ? MORE : (__eq__(a, b) ? EQUAL : LESS)));
  Comparable['Float_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
  Comparable['Float_5b7ebeeaa5acfe1eeea5a9e9845b152d']['compare'] = () => (a => b => (a > b ? MORE : (__eq__(a, b) ? EQUAL : LESS)));
  Show['Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
  Show['Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d']['show'] = () => Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect();
  Show['Byte_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
  Show['Byte_5b7ebeeaa5acfe1eeea5a9e9845b152d']['show'] = () => Inspect.Byte_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect();
  Show['Float_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
  Show['Float_5b7ebeeaa5acfe1eeea5a9e9845b152d']['show'] = () => Inspect.Float_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect();
  Scan['Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
  Scan['Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d']['scan'] = () => scanInteger;
  Scan['Float_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
  Scan['Float_5b7ebeeaa5acfe1eeea5a9e9845b152d']['scan'] = () => scanFloat;
  Scan['Byte_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
  Scan['Byte_5b7ebeeaa5acfe1eeea5a9e9845b152d']['scan'] = () => scanByte;
  let scanInteger = (str =>  {
    const n = parseInt(str);
    return isNaN(n) ? Nothing : Just(n)
  } );
  let scanFloat = (str =>  {
    const n = parseFloat(str);
    return isNaN(n) ? Nothing : Just(n)
  } );
  let scanByte = (str =>  {
    const n = parseInt(str);
    return isNaN(n) ? Nothing : Just(n)
  } );

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Semigroup.mad
  window.Semigroup = {};

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Monoid.mad

  window.Monoid = {};

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/List.mad

  Functor['List_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
  Functor['List_5b7ebeeaa5acfe1eeea5a9e9845b152d']['map'] = () => (f => list => {
      let $_result_;
      let $_continue_ = true;
      let $_start_ = {};
      let $_end_ = $_start_;
      let $$f = f;
      let $$list = list;

      while($_continue_) {
        let $f = $$f;
        let $list = $$list;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ !== null && true && true) {
      let { v: a, n: xs } = __x__;
      ($_end_ = $_end_.n = { v: $f(a) }, $$f = $f, $$list = xs, $_continue_ = true);
    }
    else if (__x__ === null) {
      ($_end_.n = (null), $_result_ = $_start_.n);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($list);
      }
      return $_result_;
  });
  Applicative['List_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
  Applicative['List_5b7ebeeaa5acfe1eeea5a9e9845b152d']['ap'] = () => (mf => ma => (_P_ => flatten(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((f => Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(f)(ma)))(_P_)))(mf));
  Applicative['List_5b7ebeeaa5acfe1eeea5a9e9845b152d']['pure'] = () => (x => ({ v: x, n: null }));
  Monad['List_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
  Monad['List_5b7ebeeaa5acfe1eeea5a9e9845b152d']['chain'] = () => (f => xs => (_P_ => flatten(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(f)(_P_)))(xs));
  Monad['List_5b7ebeeaa5acfe1eeea5a9e9845b152d']['of'] = () => Applicative.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.pure();
  Semigroup['List_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
  Semigroup['List_5b7ebeeaa5acfe1eeea5a9e9845b152d']['assoc'] = () => (list1 => list2 => {
      let $_result_;
      let $_continue_ = true;
      let $_start_ = {};
      let $_end_ = $_start_;
      let $$list1 = list1;
      let $$list2 = list2;

      while($_continue_) {
        let $list1 = $$list1;
        let $list2 = $$list2;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ !== null && true && true) {
      let { v: item, n: more } = __x__;
      ($_end_ = $_end_.n = { v: item }, $$list1 = more, $$list2 = $list2, $_continue_ = true);
    }
    else if (__x__ === null) {
      ($_end_.n = $list2, $_result_ = $_start_.n);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($list1);
      }
      return $_result_;
  });
  Monoid['List_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
  Monoid['List_5b7ebeeaa5acfe1eeea5a9e9845b152d']['mconcat'] = () => Semigroup.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.assoc();
  Monoid['List_5b7ebeeaa5acfe1eeea5a9e9845b152d']['mempty'] = () => (null);
  let mapMaybe = (f => list => {
      let $_result_;
      let $_continue_ = true;
      let $_start_ = {};
      let $_end_ = $_start_;
      let $$f = f;
      let $$list = list;

      while($_continue_) {
        let $f = $$f;
        let $list = $$list;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ !== null && true && true) {
      let { v: a, n: xs } = __x__;
      ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      let mapped = __x__.__args[0];
      ($_end_ = $_end_.n = { v: mapped }, $$f = $f, $$list = xs, $_continue_ = true);
    }
    else if (__x__.__constructor === "Nothing") {
      ($$f = $f, $$list = xs, $_continue_ = true);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($f(a));
    }
    else if (__x__ === null) {
      ($_end_.n = (null), $_result_ = $_start_.n);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($list);
      }
      return $_result_;
  });
  let repeat$1 = (a => count => {
      let $_result_;
      let $_continue_ = true;
      let $_start_ = {};
      let $_end_ = $_start_;
      let $$a = a;
      let $$count = count;

      while($_continue_) {
        let $a = $$a;
        let $count = $$count;

          $_continue_ = false;
          ($count <= 0 ? ($_end_.n = (null), $_result_ = $_start_.n) : ($_end_ = $_end_.n = { v: $a }, $$a = $a, $$count = $count - 1, $_continue_ = true));
      }
      return $_result_;
  });
  let repeatWith = (f => count => {
      let helper = (index => {
      let $_result_;
      let $_continue_ = true;
      let $_start_ = {};
      let $_end_ = $_start_;
      let $$index = index;

      while($_continue_) {
        let $index = $$index;

          $_continue_ = false;
          ($index >= count ? ($_end_.n = (null), $_result_ = $_start_.n) : ($_end_ = $_end_.n = { v: f($index) }, $$index = $index + 1, $_continue_ = true));
      }
      return $_result_;
  });
      return helper(0);
  });
  let range = (start => end => repeatWith((i => i + start))(end - start));
  let mapM$1 = (Functor_p171) => (Applicative_p171) => (f => list => {
      let helper = (x => result => (_P_ => (__$PH2__ => Applicative_p171.ap()(__$PH2__)(result))((__$PH1__ => Functor_p171.map()(__$PH1__)(f(x)))(_P_)))((x_ => result_ => ({ v: x_, n: result_ }))));
      return reduceRight(helper)(Applicative_p171.pure()((null)))(list);
  });
  let singleton = Applicative.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.pure();
  let intercalate = (sep => list => {
      let helper = (acc => l => {
      let $_result_;
      let $_continue_ = true;
      let $$acc = acc;
      let $$l = l;

      while($_continue_) {
        let $acc = $$acc;
        let $l = $$l;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ !== null && true && __x__.n === null) {
      let { v: a } = __x__;
      ($_result_ = append(a)($acc));
    }
    else if (__x__ === null) {
      ($_result_ = $acc);
    }
    else if (__x__ !== null && true && true) {
      let { v: a, n: xs } = __x__;
      ($$acc = concat($acc)(({ v: a, n: sep })), $$l = xs, $_continue_ = true);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($l);
      }
      return $_result_;
  });
      return helper((null))(list);
  });
  let intersperse = (a => xs => ((__x__) => {
    if (__x__ === null) {
      return (null);
    }
    else if (__x__ !== null && true && __x__.n === null) {
      let { v: one } = __x__;
      return ({ v: one, n: null });
    }
    else if (__x__ !== null && true && __x__.n !== null && true && __x__.n.n === null) {
      let { v: one, n: { v: two } } = __x__;
      return ({ v: one, n: { v: a, n: { v: two, n: null } } });
    }
    else if (__x__ !== null && true && true) {
      let { v: one, n: rest } = __x__;
      return ({ v: one, n: { v: a, n: intersperse(a)(rest) } });
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(xs));
  let _intercalateWithIndex = (i => f => xs => ((__x__) => {
    if (__x__ === null) {
      return (null);
    }
    else if (__x__ !== null && true && __x__.n === null) {
      let { v: one } = __x__;
      return ({ v: one, n: null });
    }
    else if (__x__ !== null && true && __x__.n !== null && true && __x__.n.n === null) {
      let { v: one, n: { v: two } } = __x__;
      return ({ v: one, n: { v: f(i), n: { v: two, n: null } } });
    }
    else if (__x__ !== null && true && true) {
      let { v: one, n: rest } = __x__;
      return ({ v: one, n: { v: f(i), n: _intercalateWithIndex(i + 1)(f)(rest) } });
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(xs));
  let intercalateWithIndex = _intercalateWithIndex(0);
  let mapWithIndex = (f => list => {
      let helper = (list_ => index => {
      let $_result_;
      let $_continue_ = true;
      let $_start_ = {};
      let $_end_ = $_start_;
      let $$list_ = list_;
      let $$index = index;

      while($_continue_) {
        let $list_ = $$list_;
        let $index = $$index;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ === null) {
      ($_end_.n = (null), $_result_ = $_start_.n);
    }
    else if (__x__ !== null && true && true) {
      let { v: a, n: xs } = __x__;
      ($_end_ = $_end_.n = { v: f(a)($index) }, $$list_ = xs, $$index = $index + 1, $_continue_ = true);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($list_);
      }
      return $_result_;
  });
      return helper(list)(0);
  });
  let concat = Semigroup.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.assoc();
  let append = (item => list => {
      let $_result_;
      let $_continue_ = true;
      let $_start_ = {};
      let $_end_ = $_start_;
      let $$item = item;
      let $$list = list;

      while($_continue_) {
        let $item = $$item;
        let $list = $$list;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ === null) {
      ($_end_.n = ({ v: $item, n: null }), $_result_ = $_start_.n);
    }
    else if (__x__ !== null && true && true) {
      let { v: a, n: xs } = __x__;
      ($_end_ = $_end_.n = { v: a }, $$item = $item, $$list = xs, $_continue_ = true);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($list);
      }
      return $_result_;
  });
  let last = (list => {
      let $_result_;
      let $_continue_ = true;
      let $$list = list;

      while($_continue_) {
        let $list = $$list;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ !== null && true && __x__.n === null) {
      let { v: item } = __x__;
      ($_result_ = Just(item));
    }
    else if (__x__ === null) {
      ($_result_ = Nothing);
    }
    else if (__x__ !== null && true && __x__.n !== null && true && true) {
      let { n: { v: a, n: xs } } = __x__;
      ($$list = ({ v: a, n: xs }), $_continue_ = true);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($list);
      }
      return $_result_;
  });
  let first = (list => ((__x__) => {
    if (__x__ === null) {
      return Nothing;
    }
    else if (__x__ !== null && true && true) {
      let { v: a,  } = __x__;
      return Just(a);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(list));
  let init$1 = (list => {
      let $_result_;
      let $_continue_ = true;
      let $_start_ = {};
      let $_end_ = $_start_;
      let $$list = list;

      while($_continue_) {
        let $list = $$list;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ !== null && true && __x__.n === null) {
      ($_end_.n = (null), $_result_ = $_start_.n);
    }
    else if (__x__ === null) {
      ($_end_.n = (null), $_result_ = $_start_.n);
    }
    else if (__x__ !== null && true && true) {
      let { v: a, n: xs } = __x__;
      ($_end_ = $_end_.n = { v: a }, $$list = xs, $_continue_ = true);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($list);
      }
      return $_result_;
  });
  let tail = (list => ((__x__) => {
    if (__x__ === null) {
      return (null);
    }
    else if (__x__ !== null && true && true) {
      let { v: a, n: xs } = __x__;
      return xs;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(list));
  let nth = (i => list => {
      let $_result_;
      let $_continue_ = true;
      let $$i = i;
      let $$list = list;

      while($_continue_) {
        let $i = $$i;
        let $list = $$list;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ === null) {
      ($_result_ = Nothing);
    }
    else if (__x__ !== null && true && true) {
      let { v: a, n: xs } = __x__;
      (__eq__($i, 0) ? ($_result_ = Just(a)) : ($$i = $i - 1, $$list = xs, $_continue_ = true));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($list);
      }
      return $_result_;
  });
  let reduceRight = (f => acc => list => (_P_ => reduceLeft((a => b => f(b)(a)))(acc)(reverse$1(_P_)))(list));
  let reduceLeft = (f => acc => list => {
      let $_result_;
      let $_continue_ = true;
      let $$f = f;
      let $$acc = acc;
      let $$list = list;

      while($_continue_) {
        let $f = $$f;
        let $acc = $$acc;
        let $list = $$list;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ === null) {
      ($_result_ = $acc);
    }
    else if (__x__ !== null && true && true) {
      let { v: a, n: xs } = __x__;
      ($$f = $f, $$acc = $f($acc)(a), $$list = xs, $_continue_ = true);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($list);
      }
      return $_result_;
  });
  let reduce = reduceLeft;
  let reduceM = (Functor_c444) => (Applicative_c444) => (Monad_c444) => (f => acc => list => ((__x__) => {
    if (__x__ === null) {
      return Monad_c444.of()(acc);
    }
    else if (__x__ !== null && true && true) {
      let { v: a, n: xs } = __x__;
      return Monad_c444.chain()((v => reduceM()(Applicative_c444)(Monad_c444)(f)(v)(xs)))(f(acc)(a));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(list));
  let filter = (predicate => list => {
      let $_result_;
      let $_continue_ = true;
      let $_start_ = {};
      let $_end_ = $_start_;
      let $$predicate = predicate;
      let $$list = list;

      while($_continue_) {
        let $predicate = $$predicate;
        let $list = $$list;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ === null) {
      ($_end_.n = (null), $_result_ = $_start_.n);
    }
    else if (__x__ !== null && true && true) {
      let { v: a, n: xs } = __x__;
      ($predicate(a) ? ($_end_ = $_end_.n = { v: a }, $$predicate = $predicate, $$list = xs, $_continue_ = true) : ($$predicate = $predicate, $$list = xs, $_continue_ = true));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($list);
      }
      return $_result_;
  });
  let reject = (predicate => list => filter(Fun.complement(predicate))(list));
  let find = (predicate => list => {
      let $_result_;
      let $_continue_ = true;
      let $$predicate = predicate;
      let $$list = list;

      while($_continue_) {
        let $predicate = $$predicate;
        let $list = $$list;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ === null) {
      ($_result_ = Nothing);
    }
    else if (__x__ !== null && true && true) {
      let { v: a, n: xs } = __x__;
      ($predicate(a) ? ($_result_ = Just(a)) : ($$predicate = $predicate, $$list = xs, $_continue_ = true));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($list);
      }
      return $_result_;
  });
  let _$_length_$_$2 = (list => {
      let helper = (list_ => count => {
      let $_result_;
      let $_continue_ = true;
      let $$list_ = list_;
      let $$count = count;

      while($_continue_) {
        let $list_ = $$list_;
        let $count = $$count;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ === null) {
      ($_result_ = $count);
    }
    else if (__x__ !== null && true && true) {
      let { v: a, n: xs } = __x__;
      ($$list_ = xs, $$count = $count + 1, $_continue_ = true);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($list_);
      }
      return $_result_;
  });
      return helper(list)(0);
  });
  let slice$1 = (start => end => list => {
      let helper = (start_ => end_ => list_ => {
      let $_result_;
      let $_continue_ = true;
      let $_start_ = {};
      let $_end_ = $_start_;
      let $$start_ = start_;
      let $$end_ = end_;
      let $$list_ = list_;

      while($_continue_) {
        let $start_ = $$start_;
        let $end_ = $$end_;
        let $list_ = $$list_;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ === null) {
      ($_end_.n = (null), $_result_ = $_start_.n);
    }
    else if (__x__ !== null && true && true) {
      let { v: a, n: xs } = __x__;
      (__eq__($start_, 0) && $end_ > 0 ? ($_end_ = $_end_.n = { v: a }, $$start_ = 0, $$end_ = $end_ - 1, $$list_ = xs, $_continue_ = true) : ($start_ > 0 ? ($$start_ = $start_ - 1, $$end_ = $end_ - 1, $$list_ = xs, $_continue_ = true) : ($_end_.n = (null), $_result_ = $_start_.n)));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($list_);
      }
      return $_result_;
  });
      let realStart = (start < 0 ? start + _$_length_$_$2(list) : start);
      let realEnd = (__eq__(end, 0) ? _$_length_$_$2(list) : (end < 0 ? end + _$_length_$_$2(list) : end));
      return helper(realStart)(realEnd)(list);
  });
  let isEmpty$1 = (xs => __eq__(xs, (null)));
  let uniqueBy = (f => reduce((result => elem => ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      return result;
    }
    else if (__x__.__constructor === "Nothing") {
      return append(elem)(result);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(find(f(elem))(result))))((null)));
  let descending = (compareFn => a => as => xs => {
      let $_result_;
      let $_continue_ = true;
      let $$compareFn = compareFn;
      let $$a = a;
      let $$as = as;
      let $$xs = xs;

      while($_continue_) {
        let $compareFn = $$compareFn;
        let $a = $$a;
        let $as = $$as;
        let $xs = $$xs;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ !== null && true && true) {
      let { v: b, n: bs } = __x__;
      (__eq__($compareFn($a)(b), MORE) ? ($$compareFn = $compareFn, $$a = b, $$as = ({ v: $a, n: $as }), $$xs = bs, $_continue_ = true) : ($_result_ = ({ v: ({ v: $a, n: $as }), n: sequences($compareFn)($xs) })));
    }
    else {
      ($_result_ = ({ v: ({ v: $a, n: $as }), n: sequences($compareFn)($xs) }));
    }
  })($xs);
      }
      return $_result_;
  });
  let ascending = (compareFn => a => as => xs => {
      let $_result_;
      let $_continue_ = true;
      let $$compareFn = compareFn;
      let $$a = a;
      let $$as = as;
      let $$xs = xs;

      while($_continue_) {
        let $compareFn = $$compareFn;
        let $a = $$a;
        let $as = $$as;
        let $xs = $$xs;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ !== null && true && true) {
      let { v: b, n: bs } = __x__;
      (!__eq__($compareFn($a)(b), MORE) ? ($$compareFn = $compareFn, $$a = b, $$as = (ys => $as(({ v: $a, n: ys }))), $$xs = bs, $_continue_ = true) : ($_result_ = ({ v: $as(({ v: $a, n: null })), n: sequences($compareFn)($xs) })));
    }
    else {
      ($_result_ = ({ v: $as(({ v: $a, n: null })), n: sequences($compareFn)($xs) }));
    }
  })($xs);
      }
      return $_result_;
  });
  let sequences = (compareFn => list => ((__x__) => {
    if (__x__ !== null && true && __x__.n !== null && true && true) {
      let { v: a, n: { v: b, n: xs } } = __x__;
      return (__eq__(compareFn(a)(b), MORE) ? descending(compareFn)(b)(({ v: a, n: null }))(xs) : ascending(compareFn)(b)((l => ({ v: a, n: l })))(xs));
    }
    else {
      let xs = __x__;
      return ({ v: xs, n: null });
    }
  })(list));
  let sortBy = (compareFn => list => {
      let merge = (listA => listB => {
      let $_result_;
      let $_continue_ = true;
      let $_start_ = {};
      let $_end_ = $_start_;
      let $$listA = listA;
      let $$listB = listB;

      while($_continue_) {
        let $listA = $$listA;
        let $listB = $$listB;

          $_continue_ = false;
          ((__x__) => {
    if (__x__.length === 2 && __x__[0] !== null && true && true && __x__[1] !== null && true && true) {
      let [{ v: a, n: as },{ v: b, n: bs }] = __x__;
      (__eq__(compareFn(a)(b), MORE) ? ($_end_ = $_end_.n = { v: b }, $$listA = $listA, $$listB = bs, $_continue_ = true) : ($_end_ = $_end_.n = { v: a }, $$listA = as, $$listB = $listB, $_continue_ = true));
    }
    else if (__x__.length === 2 && __x__[0] === null && true) {
      let [,bs] = __x__;
      ($_end_.n = bs, $_result_ = $_start_.n);
    }
    else if (__x__.length === 2 && true && __x__[1] === null) {
      let [as,] = __x__;
      ($_end_.n = as, $_result_ = $_start_.n);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(([$listA, $listB]));
      }
      return $_result_;
  });
      let mergePairs = (l => {
      let $_result_;
      let $_continue_ = true;
      let $_start_ = {};
      let $_end_ = $_start_;
      let $$l = l;

      while($_continue_) {
        let $l = $$l;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ !== null && true && __x__.n !== null && true && true) {
      let { v: a, n: { v: b, n: xs } } = __x__;
      ($_end_ = $_end_.n = { v: merge(a)(b) }, $$l = xs, $_continue_ = true);
    }
    else {
      let xs = __x__;
      ($_end_.n = xs, $_result_ = $_start_.n);
    }
  })($l);
      }
      return $_result_;
  });
      let mergeAll = (l => {
      let $_result_;
      let $_continue_ = true;
      let $$l = l;

      while($_continue_) {
        let $l = $$l;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ !== null && true && __x__.n === null) {
      let { v: x } = __x__;
      ($_result_ = x);
    }
    else {
      let xs = __x__;
      ($$l = mergePairs(xs), $_continue_ = true);
    }
  })($l);
      }
      return $_result_;
  });
      return (_P_ => mergeAll(sequences(compareFn)(_P_)))(list);
  });
  let sort = (Comparable_i788) => sortBy(Comparable_i788.compare());
  let sortAsc = (Comparable_l791) => sort(Comparable_l791);
  let sortDesc = (Comparable_r797) => sortBy((a => b => Comparable_r797.compare()(a)(b) * -1));
  let flatten = reduceLeft(concat)((null));
  let zip = (as => bs => {
      let $_result_;
      let $_continue_ = true;
      let $_start_ = {};
      let $_end_ = $_start_;
      let $$as = as;
      let $$bs = bs;

      while($_continue_) {
        let $as = $$as;
        let $bs = $$bs;

          $_continue_ = false;
          ((__x__) => {
    if (__x__.length === 2 && __x__[0] !== null && true && true && __x__[1] !== null && true && true) {
      let [{ v: a, n: aa },{ v: b, n: bb }] = __x__;
      ($_end_ = $_end_.n = { v: ([a, b]) }, $$as = aa, $$bs = bb, $_continue_ = true);
    }
    else if (__x__.length === 2 && __x__[0] !== null && true && __x__[0].n === null && __x__[1] !== null && true && __x__[1].n === null) {
      let [{ v: a },{ v: b }] = __x__;
      ($_end_.n = ({ v: ([a, b]), n: null }), $_result_ = $_start_.n);
    }
    else if (__x__.length === 2 && __x__[0] === null && __x__[1] === null) {
      ($_end_.n = (null), $_result_ = $_start_.n);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(([$as, $bs]));
      }
      return $_result_;
  });
  let includes = (x => list => {
      let $_result_;
      let $_continue_ = true;
      let $$x = x;
      let $$list = list;

      while($_continue_) {
        let $x = $$x;
        let $list = $$list;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ === null) {
      ($_result_ = false);
    }
    else if (__x__ !== null && true && true) {
      let { v: a, n: xs } = __x__;
      (__eq__(a, $x) ? ($_result_ = true) : ($$x = $x, $$list = xs, $_continue_ = true));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($list);
      }
      return $_result_;
  });
  let drop$1 = (n => list => slice$1(n)(0)(list));
  let dropLast$1 = (n => list => slice$1(0)(-n)(list));
  let take$1 = (n => list => slice$1(0)(n)(list));
  let takeLast$1 = (n => list => slice$1(-n)(0)(list));
  let dropWhile$1 = (predicate => list => {
      let $_result_;
      let $_continue_ = true;
      let $$predicate = predicate;
      let $$list = list;

      while($_continue_) {
        let $predicate = $$predicate;
        let $list = $$list;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ === null) {
      ($_result_ = (null));
    }
    else if (__x__ !== null && true && true) {
      let { v: a, n: xs } = __x__;
      ($predicate(a) ? ($$predicate = $predicate, $$list = xs, $_continue_ = true) : ($_result_ = ({ v: a, n: xs })));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($list);
      }
      return $_result_;
  });
  let takeWhile$2 = (predicate => list => {
      let $_result_;
      let $_continue_ = true;
      let $_start_ = {};
      let $_end_ = $_start_;
      let $$predicate = predicate;
      let $$list = list;

      while($_continue_) {
        let $predicate = $$predicate;
        let $list = $$list;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ === null) {
      ($_end_.n = (null), $_result_ = $_start_.n);
    }
    else if (__x__ !== null && true && true) {
      let { v: a, n: xs } = __x__;
      ($predicate(a) ? ($_end_ = $_end_.n = { v: a }, $$predicate = $predicate, $$list = xs, $_continue_ = true) : ($_end_.n = (null), $_result_ = $_start_.n));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($list);
      }
      return $_result_;
  });
  let reverse$1 = (list => {
      let helper = (acc => l => {
      let $_result_;
      let $_continue_ = true;
      let $$acc = acc;
      let $$l = l;

      while($_continue_) {
        let $acc = $$acc;
        let $l = $$l;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ !== null && true && true) {
      let { v: h, n: xs } = __x__;
      ($$acc = ({ v: h, n: $acc }), $$l = xs, $_continue_ = true);
    }
    else if (__x__ === null) {
      ($_result_ = $acc);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($l);
      }
      return $_result_;
  });
      return helper((null))(list);
  });
  var List = { mapMaybe, repeat: repeat$1, repeatWith, range, mapM: mapM$1, singleton, intercalate, intersperse, intercalateWithIndex, mapWithIndex, concat, append, last, first, init: init$1, tail, nth, reduceRight, reduceLeft, reduce, reduceM, filter, reject, find, _$_length_$_: _$_length_$_$2, slice: slice$1, isEmpty: isEmpty$1, uniqueBy, sortBy, sort, sortAsc, sortDesc, flatten, zip, includes, drop: drop$1, dropLast: dropLast$1, take: take$1, takeLast: takeLast$1, dropWhile: dropWhile$1, takeWhile: takeWhile$2, reverse: reverse$1 };

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/String.mad

  Semigroup['String_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
  Semigroup['String_5b7ebeeaa5acfe1eeea5a9e9845b152d']['assoc'] = () => (a => b => a + b);
  Monoid['String_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
  Monoid['String_5b7ebeeaa5acfe1eeea5a9e9845b152d']['mconcat'] = () => (a => b => a + b);
  Monoid['String_5b7ebeeaa5acfe1eeea5a9e9845b152d']['mempty'] = () => ``;
  Comparable['String_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
  Comparable['String_5b7ebeeaa5acfe1eeea5a9e9845b152d']['compare'] = () => (a => b =>  a > b ? MORE : a === b ? EQUAL : LESS );
  let toLower$1 = (s =>  s.toLowerCase() );
  let toUpper$1 = (s =>  s.toUpperCase() );
  let split = (separator => str =>  {
    const items = str.split(separator);

    if (items.length === 0) {
      return null
    }

    let current = {};
    let output = current;
    items.forEach((item) => {
      current = current.n = {};
      current.v = item;
    });
    current.n = null;

    return output.n
  } );
  let join = (a => xs => (_P_ => List.reduce(Monoid.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.mconcat())(``)(List.intersperse(a)(_P_)))(xs));
  let lines = split(`\n`);
  let unlines = join(`\n`);
  let words = split(` `);
  let unwords = join(` `);
  let toList$1 = (str =>  {
    let result = { v: null, n: null };
    let current = result;
    str.split('').forEach(c => {
      current = current.n = { v: c, n: null };
    });
    return result.n
  } );
  let fromList$2 = (list =>  {
    let chars = [];
    while (list !== null) {
      chars.push(list.v);
      list = list.n;
    }
    return chars.join('')
  } );
  let mapChars = (f => s =>  s.split("").map(f).join("") );
  let filterChars = (predicate => s => (_P_ => fromList$2(List.filter(predicate)(toList$1(_P_))))(s));
  let reduceChars = (f => initial => s => (_P_ => List.reduce(f)(initial)(toList$1(_P_)))(s));
  let slice = (start => end => s =>  s.slice(start, end === 0 ? s.length : end) );
  let isEmpty = (s => __eq__(s, ``));
  let drop = (n => s => slice(n)(0)(s));
  let dropLast = (n => s => slice(0)(-n)(s));
  let dropWhile = (predicate => s => (_P_ => fromList$2(List.dropWhile(predicate)(toList$1(_P_))))(s));
  let take = (n => s => slice(0)(n)(s));
  let takeLast = (n => s => slice(-n)(0)(s));
  let takeWhile$1 = (predicate => s => (_P_ => fromList$2(List.takeWhile(predicate)(toList$1(_P_))))(s));
  let charAt = (n => s => {
    const c = s[n];
    return !!c ? Just(c) : Nothing
  });
  let firstChar = charAt(0);
  let lastChar = (s => charAt(_$_length_$_$1(s) - 1)(s));
  let trim = (s =>  s.trim() );
  let trimStart = (s =>  s.trimStart() );
  let trimEnd = (s =>  s.trimEnd() );
  let _$_length_$_$1 = (s =>  s.length );
  let repeat = (c => n => (_P_ => fromList$2(List.repeat(c)(_P_)))(n));
  let match = (regex => input =>  input.match(regex) !== null );
  let replace = (regex => replacing => input => 
    input.replace(new RegExp(regex, "g"), replacing)
  );
  let pushChar = (c => s =>  c + s );
  let appendChar = (c => s =>  s + c );
  let reverse = (s => (_P_ => fromList$2(List.reverse(toList$1(_P_))))(s));
  var String$1 = { toLower: toLower$1, toUpper: toUpper$1, split, join, lines, unlines, words, unwords, toList: toList$1, fromList: fromList$2, mapChars, filterChars, reduceChars, slice, isEmpty, drop, dropLast, dropWhile, take, takeLast, takeWhile: takeWhile$1, charAt, firstChar, lastChar, trim, trimStart, trimEnd, _$_length_$_: _$_length_$_$1, repeat, match, replace, pushChar, appendChar, reverse };

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Char.mad

  Comparable['Char_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
  Comparable['Char_5b7ebeeaa5acfe1eeea5a9e9845b152d']['compare'] = () => (a => b =>  a > b ? MORE : a === b ? EQUAL : LESS );
  Show['Char_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
  Show['Char_5b7ebeeaa5acfe1eeea5a9e9845b152d']['show'] = () => (x =>  x );
  let isDigit = (s => __eq__(s, __String.fromCharCode(48)) || __eq__(s, __String.fromCharCode(49)) || __eq__(s, __String.fromCharCode(50)) || __eq__(s, __String.fromCharCode(51)) || __eq__(s, __String.fromCharCode(52)) || __eq__(s, __String.fromCharCode(53)) || __eq__(s, __String.fromCharCode(54)) || __eq__(s, __String.fromCharCode(55)) || __eq__(s, __String.fromCharCode(56)) || __eq__(s, __String.fromCharCode(57)));
  let isLetter = (c => (_P_ => String$1.match(`[a-zA-Z]+`)(Show.Char_5b7ebeeaa5acfe1eeea5a9e9845b152d.show()(_P_)))(c));
  let toLower = (c =>  c.toLowerCase() );
  let toUpper = (c =>  c.toUpperCase() );
  var Char = { isDigit, isLetter, toLower, toUpper };

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Bifunctor.mad
  window.Bifunctor = {};

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Either.mad

  let Left = (a => ({ __constructor: "Left", __args: [ a ] }));
  let Right = (a => ({ __constructor: "Right", __args: [ a ] }));
  Inspect['Either_8b29150c5dbf7739dbfa340875ffa50e'] = {};
  Inspect['Either_8b29150c5dbf7739dbfa340875ffa50e']['inspect'] = () => (Inspect_l115) => (Inspect_o118) => (__$a__ => ((__x__) => {
    if (__x__.__constructor === "Left" && true) {
      let a0 = __x__.__args[0];
      return `Left(` + Inspect_l115.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "Right" && true) {
      let a0 = __x__.__args[0];
      return `Right(` + Inspect_o118.inspect()(a0) + `)`;
    }
    else {
      return `Unknown`;
    }
  })(__$a__));
  Functor['Either_8b29150c5dbf7739dbfa340875ffa50e'] = {};
  Functor['Either_8b29150c5dbf7739dbfa340875ffa50e']['map'] = () => (f => __x__ => ((__x__) => {
    if (__x__.__constructor === "Right" && true) {
      let a = __x__.__args[0];
      return Right(f(a));
    }
    else if (__x__.__constructor === "Left" && true) {
      let e = __x__.__args[0];
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  Applicative['Either_8b29150c5dbf7739dbfa340875ffa50e'] = {};
  Applicative['Either_8b29150c5dbf7739dbfa340875ffa50e']['ap'] = () => (mf => m => ((__x__) => {
    if (__x__.__constructor === "Left" && true) {
      let e = __x__.__args[0];
      return Left(e);
    }
    else if (__x__.__constructor === "Right" && true) {
      let f = __x__.__args[0];
      return Functor.Either_8b29150c5dbf7739dbfa340875ffa50e.map()(f)(m);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(mf));
  Applicative['Either_8b29150c5dbf7739dbfa340875ffa50e']['pure'] = () => Right;
  Monad['Either_8b29150c5dbf7739dbfa340875ffa50e'] = {};
  Monad['Either_8b29150c5dbf7739dbfa340875ffa50e']['chain'] = () => (f => __x__ => ((__x__) => {
    if (__x__.__constructor === "Right" && true) {
      let a = __x__.__args[0];
      return f(a);
    }
    else if (__x__.__constructor === "Left" && true) {
      let e = __x__.__args[0];
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  Monad['Either_8b29150c5dbf7739dbfa340875ffa50e']['of'] = () => Applicative.Either_8b29150c5dbf7739dbfa340875ffa50e.pure();
  Bifunctor['Either_8b29150c5dbf7739dbfa340875ffa50e'] = {};
  Bifunctor['Either_8b29150c5dbf7739dbfa340875ffa50e']['bimap'] = () => (leftF => rightF => __x__ => ((__x__) => {
    if (__x__.__constructor === "Right" && true) {
      let a = __x__.__args[0];
      return Right(rightF(a));
    }
    else if (__x__.__constructor === "Left" && true) {
      let e = __x__.__args[0];
      return Left(leftF(e));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  Bifunctor['Either_8b29150c5dbf7739dbfa340875ffa50e']['mapFirst'] = () => mapLeft;
  Bifunctor['Either_8b29150c5dbf7739dbfa340875ffa50e']['mapSecond'] = () => Functor.Either_8b29150c5dbf7739dbfa340875ffa50e.map();
  Show['Either_8b29150c5dbf7739dbfa340875ffa50e'] = {};
  Show['Either_8b29150c5dbf7739dbfa340875ffa50e']['show'] = () => (Show_m272) => (Show_p275) => (__x__ => ((__x__) => {
    if (__x__.__constructor === "Right" && true) {
      let a = __x__.__args[0];
      return `Right ` + Show_m272.show()(a);
    }
    else if (__x__.__constructor === "Left" && true) {
      let e = __x__.__args[0];
      return `Left ` + Show_p275.show()(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  Functor.Either_8b29150c5dbf7739dbfa340875ffa50e.map();
  let mapLeft = (f => m => ((__x__) => {
    if (__x__.__constructor === "Right" && true) {
      let a = __x__.__args[0];
      return Right(a);
    }
    else if (__x__.__constructor === "Left" && true) {
      let e = __x__.__args[0];
      return Left(f(e));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(m));

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Control.mad

  let _$_while_$_ = (f => {
      let $_result_;
      let $_continue_ = true;
      let $$f = f;

      while($_continue_) {
        let $f = $$f;

          $_continue_ = false;
          ($f(({ __constructor: "Unit", __args: [] })) ? ($$f = $f, $_continue_ = true) : ($_result_ = ({ __constructor: "Unit", __args: [] })));
      }
      return $_result_;
  });
  let maybeLoop = (start => evaluate => {
      let $_result_;
      let $_continue_ = true;
      let $$start = start;
      let $$evaluate = evaluate;

      while($_continue_) {
        let $start = $$start;
        let $evaluate = $$evaluate;

          $_continue_ = false;
          ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      let x = __x__.__args[0];
      ($$start = x, $$evaluate = $evaluate, $_continue_ = true);
    }
    else if (__x__.__constructor === "Nothing") {
      ($_result_ = $start);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($evaluate($start));
      }
      return $_result_;
  });

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Parse.mad

  let Loc = (a => b => c => ({ __constructor: "Loc", __args: [ a, b, c ] }));
  let Parser$1 = (a => ({ __constructor: "Parser", __args: [ a ] }));
  let Error$1 = (a => ({ __constructor: "Error", __args: [ a ] }));
  Inspect['Location_a91f6bfcb96b14c62290a677a2570798'] = {};
  Inspect['Location_a91f6bfcb96b14c62290a677a2570798']['inspect'] = () => (__$a__ => ((__x__) => {
    if (__x__.__constructor === "Loc" && true && true && true) {
      let a0 = __x__.__args[0];
      let a1 = __x__.__args[1];
      let a2 = __x__.__args[2];
      return `Loc(` + Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `, ` + Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a1) + `, ` + Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a2) + `)`;
    }
    else {
      return `Unknown`;
    }
  })(__$a__));
  Inspect['Parser_a91f6bfcb96b14c62290a677a2570798'] = {};
  Inspect['Parser_a91f6bfcb96b14c62290a677a2570798']['inspect'] = () => (Inspect_d549) => (__$a__ => ((__x__) => {
    if (__x__.__constructor === "Parser" && true) {
      let a0 = __x__.__args[0];
      return `Parser(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else {
      return `Unknown`;
    }
  })(__$a__));
  Inspect['Error_a91f6bfcb96b14c62290a677a2570798'] = {};
  Inspect['Error_a91f6bfcb96b14c62290a677a2570798']['inspect'] = () => (__$a__ => ((__x__) => {
    if (__x__.__constructor === "Error" && true) {
      let a0 = __x__.__args[0];
      return `Error(` + Inspect.Location_a91f6bfcb96b14c62290a677a2570798.inspect()(a0) + `)`;
    }
    else {
      return `Unknown`;
    }
  })(__$a__));
  Functor['Parser_a91f6bfcb96b14c62290a677a2570798'] = {};
  Functor['Parser_a91f6bfcb96b14c62290a677a2570798']['map'] = () => (f => m => Parser$1((s => l => ((__x__) => {
    if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && true && true && __x__[0].n === null && true) {
      let [{ v: [a, b] },loc] = __x__;
      return ([({ v: ([f(a), b]), n: null }), loc]);
    }
    else if (__x__.length === 2 && __x__[0] === null && true) {
      let [,loc] = __x__;
      return ([(null), loc]);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(parse$1(m)(s)(l)))));
  Applicative['Parser_a91f6bfcb96b14c62290a677a2570798'] = {};
  Applicative['Parser_a91f6bfcb96b14c62290a677a2570798']['ap'] = () => (parserA => parserB => Parser$1((s => l => ((__x__) => {
    if (__x__.length === 2 && __x__[0] === null && true) {
      let [,l1] = __x__;
      return ([(null), l1]);
    }
    else if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && true && true && __x__[0].n === null && true) {
      let [{ v: [f, s1] },l1] = __x__;
      return ((__x__) => {
    if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && true && true && __x__[0].n === null && true) {
      let [{ v: [a, s2] },l2] = __x__;
      return ([({ v: ([f(a), s2]), n: null }), l2]);
    }
    else if (__x__.length === 2 && __x__[0] === null && true) {
      let [,l2] = __x__;
      return ([(null), l2]);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(parse$1(parserB)(s1)(l1));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(parse$1(parserA)(s)(l)))));
  Applicative['Parser_a91f6bfcb96b14c62290a677a2570798']['pure'] = () => (a => Parser$1((s => l => ([({ v: ([a, s]), n: null }), l]))));
  Monad['Parser_a91f6bfcb96b14c62290a677a2570798'] = {};
  Monad['Parser_a91f6bfcb96b14c62290a677a2570798']['chain'] = () => (f => m => Parser$1((s => l => ((__x__) => {
    if (__x__.length === 2 && __x__[0] === null && true) {
      let [,ll] = __x__;
      return ([(null), ll]);
    }
    else if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && true && true && __x__[0].n === null && true) {
      let [{ v: [a, s1] },l1] = __x__;
      return parse$1(f(a))(s1)(l1);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(parse$1(m)(s)(l)))));
  Monad['Parser_a91f6bfcb96b14c62290a677a2570798']['of'] = () => Applicative.Parser_a91f6bfcb96b14c62290a677a2570798.pure();
  Alternative['Parser_a91f6bfcb96b14c62290a677a2570798'] = {};
  Alternative['Parser_a91f6bfcb96b14c62290a677a2570798']['aempty'] = () => Parser$1((_ => l => ([(null), l])));
  Alternative['Parser_a91f6bfcb96b14c62290a677a2570798']['alt'] = () => (ma => mb => Parser$1((s => l => ((__x__) => {
    if (__x__.length === 2 && __x__[0] === null && true) {
      return parse$1(mb)(s)(l);
    }
    else {
      let res = __x__;
      return res;
    }
  })(parse$1(ma)(s)(l)))));
  let incLoc = (c => __x__ => ((__x__) => {
    if (__x__.__constructor === "Loc" && true && true && true) {
      let abs = __x__.__args[0];
      let line = __x__.__args[1];
      let col = __x__.__args[2];
      return (__eq__(c, __String.fromCharCode(10)) ? Loc(abs + 1)(line + 1)(0) : Loc(abs + 1)(line)(col + 1));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  let parse$1 = (parser => input => loc => ((__x__) => {
    if (__x__.__constructor === "Parser" && true) {
      let fn = __x__.__args[0];
      return fn(input)(loc);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(parser));
  let runParser = (m => s => ((__x__) => {
    if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && true && __x__[0].v[1] === "" && __x__[0].n === null && true) {
      let [{ v: [res, ] },] = __x__;
      return Right(res);
    }
    else if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && true && true && __x__[0].n === null && true) {
      let [{ v: [, rest] },l] = __x__;
      return Left(Error$1(l));
    }
    else if (__x__.length === 2 && true && true) {
      let [,l] = __x__;
      return Left(Error$1(l));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(parse$1(m)(s)(Loc(0)(0)(0))));
  let fail$1 = Alternative.Parser_a91f6bfcb96b14c62290a677a2570798.aempty();
  let anyChar = Parser$1((s => l => ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      let c = __x__.__args[0];
      return ([({ v: ([c, String$1.drop(1)(s)]), n: null }), incLoc(c)(l)]);
    }
    else if (__x__.__constructor === "Nothing") {
      return ([(null), l]);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(String$1.charAt(0)(s))));
  let location = Parser$1((s => l => ([({ v: ([l, s]), n: null }), l])));
  let oneOf = (cs => satisfy((__$PH1__ => List.includes(__$PH1__)(cs))));
  let notOneOf = (cs => satisfy(complement((__$PH2__ => List.includes(__$PH2__)(cs)))));
  let choice = (ps => List.reduce(Alternative.Parser_a91f6bfcb96b14c62290a677a2570798.alt())(fail$1)(ps));
  let many = (p => Parser$1((s => l => {
      let rest = s;
      let loc = l;
      let acc = (null);
      _$_while_$_((_ => ((__x__) => {
    if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && true && true && __x__[0].n === null && true) {
      let [{ v: [parsed, r] },loc_] = __x__;
      return (() => {
    rest = r;
    loc = loc_;
    acc = ({ v: parsed, n: acc });
    return true
  })();
    }
    else {
      return false;
    }
  })(parse$1(p)(rest)(loc))));
      return ([({ v: ([List.reverse(acc), rest]), n: null }), loc]);
  })));
  let some = (p => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((first => Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()((rest => ({ v: first, n: rest })))(many(p))))(p));
  let manyTill = (p => end => Parser$1((s => l => {
      let result = maybeLoop(([s, l, (null)]))((state => ((__x__) => {
    if (__x__.length === 3 && true && true && true) {
      let [ss,ll,acc] = __x__;
      return ((__x__) => {
    if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && true && true && __x__[0].n === null && true) {
      return Nothing;
    }
    else {
      return ((__x__) => {
    if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && true && true && __x__[0].n === null && true) {
      let [{ v: [parsed, rest] },loc] = __x__;
      return Just(([rest, loc, ({ v: parsed, n: acc })]));
    }
    else {
      return Nothing;
    }
  })(parse$1(p)(ss)(ll));
    }
  })(parse$1(end)(ss)(ll));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(state)));
      return ((__x__) => {
    if (__x__.length === 3 && true && true && true) {
      let [rest,loc,parseResult] = __x__;
      return ([({ v: ([List.reverse(parseResult), rest]), n: null }), loc]);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(result);
  })));
  let someTill = (p => end => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((first => Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()((rest => Monoid.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.mconcat()(({ v: first, n: null }))(rest)))(manyTill(p)(end))))(p));
  let lookAhead = (p => Parser$1((s => l => ((__x__) => {
    if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && true && true && __x__[0].n === null && true) {
      let [{ v: [a, ] },] = __x__;
      return ([({ v: ([a, s]), n: null }), l]);
    }
    else {
      return ([(null), l]);
    }
  })(parse$1(p)(s)(l)))));
  let takeWhile = (pred => Parser$1((s => l => {
      let result = maybeLoop(([s, l, (null)]))((state => ((__x__) => {
    if (__x__.length === 3 && true && true && true) {
      let [ss,ll,acc] = __x__;
      return ((__x__) => {
    if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && true && true && __x__[0].n === null && true) {
      let [{ v: [parsed, rest] },loc] = __x__;
      return (pred(parsed) ? Just(([rest, loc, ({ v: parsed, n: acc })])) : Nothing);
    }
    else {
      return Nothing;
    }
  })(parse$1(anyChar)(ss)(ll));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(state)));
      return ((__x__) => {
    if (__x__.length === 3 && true && true && true) {
      let [rest,loc,parseResult] = __x__;
      return ([({ v: ([List.reverse(parseResult), rest]), n: null }), loc]);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(result);
  })));
  let sepBy = (parser => separator => Alternative.Parser_a91f6bfcb96b14c62290a677a2570798.alt()((() => {
    
    return Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((first => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((rest => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.of()(({ v: first, n: rest }))))(many(andDo(Functor.Parser_a91f6bfcb96b14c62290a677a2570798)(Applicative.Parser_a91f6bfcb96b14c62290a677a2570798)(Monad.Parser_a91f6bfcb96b14c62290a677a2570798)(parser)(separator)))))(parser)
  })())(fail$1));
  let satisfy = (pred => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()(ifElse(pred)(Monad.Parser_a91f6bfcb96b14c62290a677a2570798.of())(always(fail$1)))(anyChar));
  let char = (_P_ => satisfy(equals(_P_)));
  let notChar = (_P_ => satisfy(notEquals(_P_)));
  let eof = Parser$1((s => l => ((__x__) => {
    if (__x__.length === 2 && __x__[0] === null && true) {
      return ([({ v: ([({ __constructor: "Unit", __args: [] }), ``]), n: null }), l]);
    }
    else {
      return ([(null), l]);
    }
  })(parse$1(anyChar)(s)(l))));
  let string$1 = (s => ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      let c = __x__.__args[0];
      return (_P_ => (__$PH3__ => Applicative.Parser_a91f6bfcb96b14c62290a677a2570798.ap()(__$PH3__)(string$1(String$1.drop(1)(s))))(Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()((a => b => String$1.pushChar(a)(b)))(_P_)))(char(c));
    }
    else if (__x__.__constructor === "Nothing") {
      return Applicative.Parser_a91f6bfcb96b14c62290a677a2570798.pure()(``);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(String$1.firstChar(s)));
  let spaces = (_P_ => some(oneOf(_P_)))(({ v: __String.fromCharCode(32), n: { v: __String.fromCharCode(10), n: { v: __String.fromCharCode(13), n: { v: __String.fromCharCode(9), n: null } } } }));
  let token = (__$PH4__ => apL(Functor.Parser_a91f6bfcb96b14c62290a677a2570798)(Applicative.Parser_a91f6bfcb96b14c62290a677a2570798)(__$PH4__)(Alternative.Parser_a91f6bfcb96b14c62290a677a2570798.alt()(spaces)(Applicative.Parser_a91f6bfcb96b14c62290a677a2570798.pure()((null)))));
  let symbol = (_P_ => token(string$1(_P_)));
  let digit = satisfy(Char.isDigit);
  let letter = satisfy(Char.isLetter);
  let letters = many(letter);
  var P = { runParser, fail: fail$1, anyChar, location, oneOf, notOneOf, choice, many, some, manyTill, someTill, lookAhead, takeWhile, sepBy, satisfy, char, notChar, eof, string: string$1, spaces, token, symbol, digit, letter, letters, Loc, Parser: Parser$1, Error: Error$1 };

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Tuple.mad

  Show['Tuple_2'] = {};
  Show['Tuple_2']['show'] = () => (Show_y24) => (Show_x23) => (__x__ => ((__x__) => {
    if (__x__.length === 2 && true && true) {
      let [a,b] = __x__;
      return `#[` + Show_x23.show()(a) + `, ` + Show_y24.show()(b) + `]`;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  Show['Tuple_3'] = {};
  Show['Tuple_3']['show'] = () => (Show_s44) => (Show_r43) => (Show_q42) => (__x__ => ((__x__) => {
    if (__x__.length === 3 && true && true && true) {
      let [a,b,c] = __x__;
      return `#[` + Show_q42.show()(a) + `, ` + Show_r43.show()(b) + `, ` + Show_s44.show()(c) + `]`;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  Show['Tuple_4'] = {};
  Show['Tuple_4']['show'] = () => (Show_u72) => (Show_t71) => (Show_s70) => (Show_r69) => (__x__ => ((__x__) => {
    if (__x__.length === 4 && true && true && true && true) {
      let [a,b,c,d] = __x__;
      return `#[` + Show_r69.show()(a) + `, ` + Show_s70.show()(b) + `, ` + Show_t71.show()(c) + `, ` + Show_u72.show()(d) + `]`;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  let fst = (tuple => ((__x__) => {
    if (__x__.length === 2 && true && true) {
      let [a,] = __x__;
      return a;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(tuple));
  let snd = (tuple => ((__x__) => {
    if (__x__.length === 2 && true && true) {
      let [,b] = __x__;
      return b;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(tuple));
  var Tuple = { fst, snd };

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Dictionary.mad

  Functor['Dictionary_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
  Functor['Dictionary_5b7ebeeaa5acfe1eeea5a9e9845b152d']['map'] = () => (fn => __x__ => ((__x__) => {
    if (__x__.__constructor === "Dictionary" && true) {
      let items = __x__.__args[0];
      return (_P_ => Dictionary(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((i => ([Tuple.fst(i), fn(Tuple.snd(i))])))(_P_)))(items);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  let Dictionary = (items =>  ({
    __constructor: "Dictionary",
    __args: [items],
  }) );
  let removeConsecutiveDoubles = (list => {
      let $_result_;
      let $_continue_ = true;
      let $_start_ = {};
      let $_end_ = $_start_;
      let $$list = list;

      while($_continue_) {
        let $list = $$list;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ !== null && __x__.v.length === 2 && true && true && __x__.n !== null && __x__.n.v.length === 2 && true && true && true) {
      let { v: [k1, v1], n: { v: [k2, v2], n: xs } } = __x__;
      (__eq__(k1, k2) ? ($_end_ = $_end_.n = { v: ([k2, v2]) }, $$list = xs, $_continue_ = true) : ($_end_ = $_end_.n = { v: ([k1, v1]) }, $$list = ({ v: ([k2, v2]), n: xs }), $_continue_ = true));
    }
    else if (__x__ !== null && true && __x__.n === null) {
      let { v: last } = __x__;
      ($_end_.n = ({ v: last, n: null }), $_result_ = $_start_.n);
    }
    else if (__x__ === null) {
      ($_end_.n = (null), $_result_ = $_start_.n);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($list);
      }
      return $_result_;
  });
  let fromList$1 = (Comparable_k88) => (_P_ => Dictionary(removeConsecutiveDoubles(List.sortBy((a => b => Comparable_k88.compare()(Tuple.fst(a))(Tuple.fst(b))))(_P_))));
  let empty$1 = Dictionary((null));
  let insert = (Comparable_a130) => (key => value => dict => {
      let helper = (list => {
      let $_result_;
      let $_continue_ = true;
      let $_start_ = {};
      let $_end_ = $_start_;
      let $$list = list;

      while($_continue_) {
        let $list = $$list;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ === null) {
      ($_end_.n = ({ v: ([key, value]), n: null }), $_result_ = $_start_.n);
    }
    else if (__x__ !== null && __x__.v.length === 2 && true && true && true) {
      let { v: [k, v], n: xs } = __x__;
      (eq(Comparable_a130)(k)(key) ? ($_end_.n = ({ v: ([key, value]), n: xs }), $_result_ = $_start_.n) : (gt(Comparable_a130)(k)(key) ? ($_end_.n = ({ v: ([key, value]), n: { v: ([k, v]), n: xs } }), $_result_ = $_start_.n) : ($_end_ = $_end_.n = { v: ([k, v]) }, $$list = xs, $_continue_ = true)));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($list);
      }
      return $_result_;
  });
      return ((__x__) => {
    if (__x__.__constructor === "Dictionary" && true) {
      let items = __x__.__args[0];
      return Dictionary(helper(items));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(dict);
  });
  let get = (Comparable_p145) => (k => __x__ => ((__x__) => {
    if (__x__.__constructor === "Dictionary" && true) {
      let items = __x__.__args[0];
      return (_P_ => Functor.Maybe_17998c1898470349f86806803a2b29f2.map()(Tuple.snd)(List.find((item => ((__x__) => {
    if (__x__.length === 2 && true && true) {
      let [kk,] = __x__;
      return __eq__(k, kk);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(item)))(_P_)))(items);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  let merge = (Comparable_z181) => (a => b => ((__x__) => {
    if (__x__.length === 2 && __x__[0].__constructor === "Dictionary" && true && __x__[1].__constructor === "Dictionary" && true) {
      let [{ __args: [itemsA]},{ __args: [itemsB]}] = __x__;
      return fromList$1(Comparable_z181)(List.concat(itemsA)(itemsB));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(([a, b])));
  let _$_length_$_ = (dictionary => ((__x__) => {
    if (__x__.__constructor === "Dictionary" && true) {
      let items = __x__.__args[0];
      return List._$_length_$_(items);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(dictionary));
  let toList = (dict => ((__x__) => {
    if (__x__.__constructor === "Dictionary" && true) {
      let items = __x__.__args[0];
      return items;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(dict));
  let mapM = (Functor_m246) => (Applicative_m246) => (f => dict => ((__x__) => {
    if (__x__.__constructor === "Dictionary" && true) {
      let items = __x__.__args[0];
      return (_P_ => Functor_m246.map()(Dictionary)(List.mapM(Functor_m246)(Applicative_m246)((__x__ => ((__x__) => {
    if (__x__.length === 2 && true && true) {
      let [k,v] = __x__;
      return Functor_m246.map()((mapped => ([k, mapped])))(f(v));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__)))(_P_)))(items);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(dict));
  let mapWithKey = (fn => __x__ => ((__x__) => {
    if (__x__.__constructor === "Dictionary" && true) {
      let items = __x__.__args[0];
      return (_P_ => Dictionary(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((i => ([Tuple.fst(i), fn(Tuple.fst(i))(Tuple.snd(i))])))(_P_)))(items);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  let keys = (m => ((__x__) => {
    if (__x__.__constructor === "Dictionary" && true) {
      let items = __x__.__args[0];
      return Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(Tuple.fst)(items);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(m));
  let values = (m => ((__x__) => {
    if (__x__.__constructor === "Dictionary" && true) {
      let items = __x__.__args[0];
      return Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(Tuple.snd)(items);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(m));
  var Dictionary$1 = { fromList: fromList$1, empty: empty$1, insert, get, merge, _$_length_$_, toList, mapM, mapWithKey, keys, values };

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/JsonValue.mad
  let JsonString = (a => ({ __constructor: "JsonString", __args: [ a ] }));
  let JsonInteger = (a => ({ __constructor: "JsonInteger", __args: [ a ] }));
  let JsonFloat = (a => ({ __constructor: "JsonFloat", __args: [ a ] }));
  let JsonBoolean = (a => ({ __constructor: "JsonBoolean", __args: [ a ] }));
  let JsonNull = ({ __constructor: "JsonNull", __args: [  ] });
  let JsonObject = (a => ({ __constructor: "JsonObject", __args: [ a ] }));
  let JsonArray = (a => ({ __constructor: "JsonArray", __args: [ a ] }));
  Inspect['JsonValue_17c1986fd39732128bc8ae9ee9fcf909'] = {};
  Inspect['JsonValue_17c1986fd39732128bc8ae9ee9fcf909']['inspect'] = () => (__$a__ => ((__x__) => {
    if (__x__.__constructor === "JsonString" && true) {
      let a0 = __x__.__args[0];
      return `JsonString(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "JsonInteger" && true) {
      let a0 = __x__.__args[0];
      return `JsonInteger(` + Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "JsonFloat" && true) {
      let a0 = __x__.__args[0];
      return `JsonFloat(` + Inspect.Float_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "JsonBoolean" && true) {
      let a0 = __x__.__args[0];
      return `JsonBoolean(` + Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "JsonNull") {
      return `JsonNull`;
    }
    else if (__x__.__constructor === "JsonObject" && true) {
      let a0 = __x__.__args[0];
      return `JsonObject(` + Inspect.Dictionary_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(Inspect.JsonValue_17c1986fd39732128bc8ae9ee9fcf909)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(a0) + `)`;
    }
    else if (__x__.__constructor === "JsonArray" && true) {
      let a0 = __x__.__args[0];
      return `JsonArray(` + Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(Inspect.JsonValue_17c1986fd39732128bc8ae9ee9fcf909)(a0) + `)`;
    }
    else {
      return `Unknown`;
    }
  })(__$a__));

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/JsonParser.mad

  let Parser = (a => ({ __constructor: "Parser", __args: [ a ] }));
  Inspect['Parser_fd0ade162d43822d649eddf766ec9ce6'] = {};
  Inspect['Parser_fd0ade162d43822d649eddf766ec9ce6']['inspect'] = () => (Inspect_f2553) => (__$a__ => ((__x__) => {
    if (__x__.__constructor === "Parser" && true) {
      let a0 = __x__.__args[0];
      return `Parser(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else {
      return `Unknown`;
    }
  })(__$a__));
  Functor['Parser_fd0ade162d43822d649eddf766ec9ce6'] = {};
  Functor['Parser_fd0ade162d43822d649eddf766ec9ce6']['map'] = () => (f => parser => Parser((input => ((__x__) => {
    if (__x__.__constructor === "Parser" && true) {
      let parserFn = __x__.__args[0];
      return Functor.Either_8b29150c5dbf7739dbfa340875ffa50e.map()(f)(parserFn(input));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(parser))));
  Applicative['Parser_fd0ade162d43822d649eddf766ec9ce6'] = {};
  Applicative['Parser_fd0ade162d43822d649eddf766ec9ce6']['ap'] = () => (mf => parser => Parser((input => ((__x__) => {
    if (__x__.__constructor === "Parser" && true) {
      let f = __x__.__args[0];
      return ((__x__) => {
    if (__x__.__constructor === "Parser" && true) {
      let parserFn = __x__.__args[0];
      return Applicative.Either_8b29150c5dbf7739dbfa340875ffa50e.ap()(f(input))(parserFn(input));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(parser);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(mf))));
  Applicative['Parser_fd0ade162d43822d649eddf766ec9ce6']['pure'] = () => succeed;
  Monad['Parser_fd0ade162d43822d649eddf766ec9ce6'] = {};
  Monad['Parser_fd0ade162d43822d649eddf766ec9ce6']['chain'] = () => (fn => parser => Parser((input => ((__x__) => {
    if (__x__.__constructor === "Parser" && true) {
      let parserFn = __x__.__args[0];
      return ((__x__) => {
    if (__x__.__constructor === "Right" && true) {
      let a = __x__.__args[0];
      return ((__x__) => {
    if (__x__.__constructor === "Parser" && true) {
      let parserFn_ = __x__.__args[0];
      return parserFn_(input);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(fn(a));
    }
    else if (__x__.__constructor === "Left" && true) {
      let e = __x__.__args[0];
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(parserFn(input));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(parser))));
  Monad['Parser_fd0ade162d43822d649eddf766ec9ce6']['of'] = () => succeed;
  let stringCharacter = choice(({ v: Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(always(__String.fromCharCode(34)))(P.string(`\\\"`)), n: { v: Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(always(__String.fromCharCode(10)))(P.string(`\\n`)), n: { v: Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(always(__String.fromCharCode(9)))(P.string(`\\t`)), n: { v: notChar(__String.fromCharCode(34)), n: null } } } }));
  let jsonString = (() => {
    
    return Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((_ => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((cs => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((_ => (_P_ => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.of()(JsonString(String$1.fromList(_P_))))(cs)))(symbol(`"`))))(many(stringCharacter))))(char(__String.fromCharCode(34)))
  })();
  let jsonInteger = (_P_ => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((_P_ => (__x__ => ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      let i = __x__.__args[0];
      return Monad.Parser_a91f6bfcb96b14c62290a677a2570798.of()(JsonInteger(i));
    }
    else if (__x__.__constructor === "Nothing") {
      return P.fail;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__))(Scan.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.scan()(String$1.fromList(_P_)))))(some(_P_)))(digit);
  let jsonFloat = (() => {
    
    return Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((beforeDot => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((dot => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((afterDot => (_P_ => (__x__ => ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      let f = __x__.__args[0];
      return Monad.Parser_a91f6bfcb96b14c62290a677a2570798.of()(JsonFloat(f));
    }
    else if (__x__.__constructor === "Nothing") {
      return P.fail;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__))(Scan.Float_5b7ebeeaa5acfe1eeea5a9e9845b152d.scan()(String$1.fromList((__$PH1__ => List.concat(__$PH1__)(afterDot))(_P_)))))(List.append(dot)(beforeDot))))(some(digit))))(char(__String.fromCharCode(46)))))(some(digit))
  })();
  let jsonNull = (_P_ => Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()((_ => JsonNull))(symbol(_P_)))(`null`);
  let jsonBoolean = (_P_ => Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()((b => (__eq__(b, `true`) ? JsonBoolean(true) : JsonBoolean(false))))(choice(_P_)))(({ v: symbol(`true`), n: { v: symbol(`false`), n: null } }));
  let jsonArray = (() => {
    
    return Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((_ => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((items => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((_ => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((_ => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.of()(JsonArray(items))))(symbol(`]`))))(Alternative.Parser_a91f6bfcb96b14c62290a677a2570798.alt()(spaces)(Applicative.Parser_a91f6bfcb96b14c62290a677a2570798.pure()((null))))))(Alternative.Parser_a91f6bfcb96b14c62290a677a2570798.alt()(sepBy(jsonValue)(symbol(`,`)))(Monad.Parser_a91f6bfcb96b14c62290a677a2570798.of()((null))))))(symbol(`[`))
  })();
  let objectField = (() => {
    
    return Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((_ => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((fieldName => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((_ => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((_ => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((fieldValue => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.of()(([String$1.fromList(fieldName), fieldValue]))))(jsonValue)))(symbol(`:`))))(char(__String.fromCharCode(34)))))(many(notChar(__String.fromCharCode(34))))))(char(__String.fromCharCode(34)))
  })();
  let jsonObject = (() => {
    
    return Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((_ => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((fields => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((_ => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((_ => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.of()(JsonObject(Dictionary$1.fromList(Comparable.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(fields)))))(symbol(`}`))))(Alternative.Parser_a91f6bfcb96b14c62290a677a2570798.alt()(spaces)(Applicative.Parser_a91f6bfcb96b14c62290a677a2570798.pure()((null))))))(Alternative.Parser_a91f6bfcb96b14c62290a677a2570798.alt()(sepBy(objectField)(symbol(`,`)))(Monad.Parser_a91f6bfcb96b14c62290a677a2570798.of()((null))))))(symbol(`{`))
  })();
  let jsonValue = (() => {
    
    return Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((_ => choice(({ v: jsonFloat, n: { v: jsonInteger, n: { v: jsonNull, n: { v: jsonBoolean, n: { v: jsonString, n: { v: jsonArray, n: { v: jsonObject, n: null } } } } } } }))))(Alternative.Parser_a91f6bfcb96b14c62290a677a2570798.alt()(spaces)(Applicative.Parser_a91f6bfcb96b14c62290a677a2570798.pure()((null))))
  })();
  let getParserFn = (parser => ((__x__) => {
    if (__x__.__constructor === "Parser" && true) {
      let fn = __x__.__args[0];
      return fn;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(parser));
  let parse = (parser => input => (_P_ => (__x__ => ((__x__) => {
    if (__x__.__constructor === "Left" && true) {
      let e = __x__.__args[0];
      return Left(`Invalid json: ` + Inspect.Error_a91f6bfcb96b14c62290a677a2570798.inspect()(e));
    }
    else if (__x__.__constructor === "Right" && true) {
      let parsed = __x__.__args[0];
      return ((__x__) => {
    if (__x__.__constructor === "Parser" && true) {
      let parserFn = __x__.__args[0];
      return parserFn(parsed);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(parser);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__))(runParser(jsonValue)(_P_)))(input));
  let succeed = (a => Parser((_ => Right(a))));
  let fail = (err => Parser((_ => Left(err))));
  let string = Parser((input => ((__x__) => {
    if (__x__.__constructor === "JsonString" && true) {
      let s = __x__.__args[0];
      return Right(s);
    }
    else {
      return Left(`Error parsing string`);
    }
  })(input)));
  let integer = Parser((input => ((__x__) => {
    if (__x__.__constructor === "JsonInteger" && true) {
      let i = __x__.__args[0];
      return Right(i);
    }
    else {
      return Left(`Error parsing integer`);
    }
  })(input)));
  let float = Parser((input => ((__x__) => {
    if (__x__.__constructor === "JsonFloat" && true) {
      let f = __x__.__args[0];
      return Right(f);
    }
    else {
      return Left(`Error parsing float`);
    }
  })(input)));
  let boolean = Parser((input => ((__x__) => {
    if (__x__.__constructor === "JsonBoolean" && true) {
      let b = __x__.__args[0];
      return Right(b);
    }
    else {
      return Left(`Error parsing boolean`);
    }
  })(input)));
  let list = (parser => Parser((input => ((__x__) => {
    if (__x__.__constructor === "JsonArray" && true) {
      let arr = __x__.__args[0];
      return ((__x__) => {
    if (__x__.__constructor === "Parser" && true) {
      let parserFn = __x__.__args[0];
      return List.mapM(Functor.Either_8b29150c5dbf7739dbfa340875ffa50e)(Applicative.Either_8b29150c5dbf7739dbfa340875ffa50e)(parserFn)(arr);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(parser);
    }
    else {
      return Left(`Error parsing list`);
    }
  })(input))));
  let dict = (parser => Parser((input => ((__x__) => {
    if (__x__.length === 2 && __x__[0].__constructor === "JsonObject" && true && __x__[1].__constructor === "Parser" && true) {
      let [{ __args: [d]},{ __args: [parserFn]}] = __x__;
      return Dictionary$1.mapM(Functor.Either_8b29150c5dbf7739dbfa340875ffa50e)(Applicative.Either_8b29150c5dbf7739dbfa340875ffa50e)(parserFn)(d);
    }
    else {
      return Left(`Error parsing dict`);
    }
  })(([input, parser])))));
  let maybe = (parser => Parser((input => ((__x__) => {
    if (__x__.__constructor === "Right" && true) {
      let a = __x__.__args[0];
      return Right(Just(a));
    }
    else if (__x__.__constructor === "Left" && true) {
      return Right(Nothing);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(getParserFn(parser)(input)))));
  let lazy = (wrapped => Parser((input => ((__x__) => {
    if (__x__.__constructor === "Parser" && true) {
      let parserFn = __x__.__args[0];
      return parserFn(input);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(wrapped(({ __constructor: "Unit", __args: [] }))))));
  let field = (fieldName => parser => Parser((input => ((__x__) => {
    if (__x__.__constructor === "JsonObject" && true) {
      let d = __x__.__args[0];
      return (_P_ => (__x__ => ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      let value = __x__.__args[0];
      return getParserFn(parser)(value);
    }
    else if (__x__.__constructor === "Nothing") {
      return Left(`Error parsing fieldname '` + fieldName + `'`);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__))(Dictionary$1.get(Comparable.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(fieldName)(_P_)))(d);
    }
    else {
      return Left(`Error parsing fieldname '` + fieldName + `'`);
    }
  })(input))));
  let path = (pathParts => parser => Parser((input => (_P_ => Monad.Either_8b29150c5dbf7739dbfa340875ffa50e.chain()(getParserFn(parser))(List.reduce((val => fieldName => ((__x__) => {
    if (__x__.__constructor === "Right" && __x__.__args[0].__constructor === "JsonObject" && true) {
      let d = __x__.__args[0].__args[0];
      return (_P_ => (__x__ => ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      let value = __x__.__args[0];
      return Right(value);
    }
    else if (__x__.__constructor === "Nothing") {
      return Left(`Error parsing fieldname '` + fieldName + `'`);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__))(Dictionary$1.get(Comparable.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(fieldName)(_P_)))(d);
    }
    else {
      return Left(`Error parsing path: '` + Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(pathParts) + `' - value: '` + Inspect.Either_8b29150c5dbf7739dbfa340875ffa50e.inspect()(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.JsonValue_17c1986fd39732128bc8ae9ee9fcf909)(val) + `'`);
    }
  })(val)))(Right(input))(_P_)))(pathParts))));
  let chain1 = Monad.Parser_fd0ade162d43822d649eddf766ec9ce6.chain();
  let chain2 = (fn => parserA => parserB => Parser((input => ((__x__) => {
    if (__x__.length === 2 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true) {
      let [{ __args: [a]},{ __args: [b]}] = __x__;
      return getParserFn(fn(a)(b))(input);
    }
    else if (__x__.length === 2 && __x__[0].__constructor === "Left" && true && true) {
      let [{ __args: [e]},] = __x__;
      return Left(e);
    }
    else if (__x__.length === 2 && true && __x__[1].__constructor === "Left" && true) {
      let [,{ __args: [e]}] = __x__;
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(([getParserFn(parserA)(input), getParserFn(parserB)(input)])))));
  let chain3 = (fn => parserA => parserB => parserC => Parser((input => ((__x__) => {
    if (__x__.length === 3 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true) {
      let [{ __args: [a]},{ __args: [b]},{ __args: [c]}] = __x__;
      return getParserFn(fn(a)(b)(c))(input);
    }
    else if (__x__.length === 3 && __x__[0].__constructor === "Left" && true && true && true) {
      let [{ __args: [e]},,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 3 && true && __x__[1].__constructor === "Left" && true && true) {
      let [,{ __args: [e]},] = __x__;
      return Left(e);
    }
    else if (__x__.length === 3 && true && true && __x__[2].__constructor === "Left" && true) {
      let [,,{ __args: [e]}] = __x__;
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(([getParserFn(parserA)(input), getParserFn(parserB)(input), getParserFn(parserC)(input)])))));
  let chain4 = (fn => parserA => parserB => parserC => parserD => Parser((input => ((__x__) => {
    if (__x__.length === 4 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true && __x__[3].__constructor === "Right" && true) {
      let [{ __args: [a]},{ __args: [b]},{ __args: [c]},{ __args: [d]}] = __x__;
      return getParserFn(fn(a)(b)(c)(d))(input);
    }
    else if (__x__.length === 4 && __x__[0].__constructor === "Left" && true && true && true && true) {
      let [{ __args: [e]},,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 4 && true && __x__[1].__constructor === "Left" && true && true && true) {
      let [,{ __args: [e]},,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 4 && true && true && __x__[2].__constructor === "Left" && true && true) {
      let [,,{ __args: [e]},] = __x__;
      return Left(e);
    }
    else if (__x__.length === 4 && true && true && true && __x__[3].__constructor === "Left" && true) {
      let [,,,{ __args: [e]}] = __x__;
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(([getParserFn(parserA)(input), getParserFn(parserB)(input), getParserFn(parserC)(input), getParserFn(parserD)(input)])))));
  let chain5 = (fn => parserA => parserB => parserC => parserD => parserE => Parser((input => ((__x__) => {
    if (__x__.length === 5 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true && __x__[3].__constructor === "Right" && true && __x__[4].__constructor === "Right" && true) {
      let [{ __args: [a]},{ __args: [b]},{ __args: [c]},{ __args: [d]},{ __args: [e]}] = __x__;
      return getParserFn(fn(a)(b)(c)(d)(e))(input);
    }
    else if (__x__.length === 5 && __x__[0].__constructor === "Left" && true && true && true && true && true) {
      let [{ __args: [e]},,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 5 && true && __x__[1].__constructor === "Left" && true && true && true && true) {
      let [,{ __args: [e]},,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 5 && true && true && __x__[2].__constructor === "Left" && true && true && true) {
      let [,,{ __args: [e]},,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 5 && true && true && true && __x__[3].__constructor === "Left" && true && true) {
      let [,,,{ __args: [e]},] = __x__;
      return Left(e);
    }
    else if (__x__.length === 5 && true && true && true && true && __x__[4].__constructor === "Left" && true) {
      let [,,,,{ __args: [e]}] = __x__;
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(([getParserFn(parserA)(input), getParserFn(parserB)(input), getParserFn(parserC)(input), getParserFn(parserD)(input), getParserFn(parserE)(input)])))));
  let chain6 = (fn => parserA => parserB => parserC => parserD => parserE => parserF => Parser((input => ((__x__) => {
    if (__x__.length === 6 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true && __x__[3].__constructor === "Right" && true && __x__[4].__constructor === "Right" && true && __x__[5].__constructor === "Right" && true) {
      let [{ __args: [a]},{ __args: [b]},{ __args: [c]},{ __args: [d]},{ __args: [e]},{ __args: [f]}] = __x__;
      return getParserFn(fn(a)(b)(c)(d)(e)(f))(input);
    }
    else if (__x__.length === 6 && __x__[0].__constructor === "Left" && true && true && true && true && true && true) {
      let [{ __args: [e]},,,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 6 && true && __x__[1].__constructor === "Left" && true && true && true && true && true) {
      let [,{ __args: [e]},,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 6 && true && true && __x__[2].__constructor === "Left" && true && true && true && true) {
      let [,,{ __args: [e]},,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 6 && true && true && true && __x__[3].__constructor === "Left" && true && true && true) {
      let [,,,{ __args: [e]},,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 6 && true && true && true && true && __x__[4].__constructor === "Left" && true && true) {
      let [,,,,{ __args: [e]},] = __x__;
      return Left(e);
    }
    else if (__x__.length === 6 && true && true && true && true && true && __x__[5].__constructor === "Left" && true) {
      let [,,,,,{ __args: [e]}] = __x__;
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(([getParserFn(parserA)(input), getParserFn(parserB)(input), getParserFn(parserC)(input), getParserFn(parserD)(input), getParserFn(parserE)(input), getParserFn(parserF)(input)])))));
  let chain7 = (fn => parserA => parserB => parserC => parserD => parserE => parserF => parserG => Parser((input => ((__x__) => {
    if (__x__.length === 7 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true && __x__[3].__constructor === "Right" && true && __x__[4].__constructor === "Right" && true && __x__[5].__constructor === "Right" && true && __x__[6].__constructor === "Right" && true) {
      let [{ __args: [a]},{ __args: [b]},{ __args: [c]},{ __args: [d]},{ __args: [e]},{ __args: [f]},{ __args: [g]}] = __x__;
      return getParserFn(fn(a)(b)(c)(d)(e)(f)(g))(input);
    }
    else if (__x__.length === 7 && __x__[0].__constructor === "Left" && true && true && true && true && true && true && true) {
      let [{ __args: [e]},,,,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 7 && true && __x__[1].__constructor === "Left" && true && true && true && true && true && true) {
      let [,{ __args: [e]},,,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 7 && true && true && __x__[2].__constructor === "Left" && true && true && true && true && true) {
      let [,,{ __args: [e]},,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 7 && true && true && true && __x__[3].__constructor === "Left" && true && true && true && true) {
      let [,,,{ __args: [e]},,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 7 && true && true && true && true && __x__[4].__constructor === "Left" && true && true && true) {
      let [,,,,{ __args: [e]},,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 7 && true && true && true && true && true && __x__[5].__constructor === "Left" && true && true) {
      let [,,,,,{ __args: [e]},] = __x__;
      return Left(e);
    }
    else if (__x__.length === 7 && true && true && true && true && true && true && __x__[6].__constructor === "Left" && true) {
      let [,,,,,,{ __args: [e]}] = __x__;
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(([getParserFn(parserA)(input), getParserFn(parserB)(input), getParserFn(parserC)(input), getParserFn(parserD)(input), getParserFn(parserE)(input), getParserFn(parserF)(input), getParserFn(parserG)(input)])))));
  let chain8 = (fn => parserA => parserB => parserC => parserD => parserE => parserF => parserG => parserH => Parser((input => ((__x__) => {
    if (__x__.length === 8 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true && __x__[3].__constructor === "Right" && true && __x__[4].__constructor === "Right" && true && __x__[5].__constructor === "Right" && true && __x__[6].__constructor === "Right" && true && __x__[7].__constructor === "Right" && true) {
      let [{ __args: [a]},{ __args: [b]},{ __args: [c]},{ __args: [d]},{ __args: [e]},{ __args: [f]},{ __args: [g]},{ __args: [h]}] = __x__;
      return getParserFn(fn(a)(b)(c)(d)(e)(f)(g)(h))(input);
    }
    else if (__x__.length === 8 && __x__[0].__constructor === "Left" && true && true && true && true && true && true && true && true) {
      let [{ __args: [e]},,,,,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 8 && true && __x__[1].__constructor === "Left" && true && true && true && true && true && true && true) {
      let [,{ __args: [e]},,,,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 8 && true && true && __x__[2].__constructor === "Left" && true && true && true && true && true && true) {
      let [,,{ __args: [e]},,,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 8 && true && true && true && __x__[3].__constructor === "Left" && true && true && true && true && true) {
      let [,,,{ __args: [e]},,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 8 && true && true && true && true && __x__[4].__constructor === "Left" && true && true && true && true) {
      let [,,,,{ __args: [e]},,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 8 && true && true && true && true && true && __x__[5].__constructor === "Left" && true && true && true) {
      let [,,,,,{ __args: [e]},,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 8 && true && true && true && true && true && true && __x__[6].__constructor === "Left" && true && true) {
      let [,,,,,,{ __args: [e]},] = __x__;
      return Left(e);
    }
    else if (__x__.length === 8 && true && true && true && true && true && true && true && __x__[7].__constructor === "Left" && true) {
      let [,,,,,,,{ __args: [e]}] = __x__;
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(([getParserFn(parserA)(input), getParserFn(parserB)(input), getParserFn(parserC)(input), getParserFn(parserD)(input), getParserFn(parserE)(input), getParserFn(parserF)(input), getParserFn(parserG)(input), getParserFn(parserH)(input)])))));
  let map1 = Functor.Parser_fd0ade162d43822d649eddf766ec9ce6.map();
  let map2 = (fn => parserA => parserB => Parser((input => ((__x__) => {
    if (__x__.length === 2 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true) {
      let [{ __args: [a]},{ __args: [b]}] = __x__;
      return Right(fn(a)(b));
    }
    else if (__x__.length === 2 && __x__[0].__constructor === "Left" && true && true) {
      let [{ __args: [e]},] = __x__;
      return Left(e);
    }
    else if (__x__.length === 2 && true && __x__[1].__constructor === "Left" && true) {
      let [,{ __args: [e]}] = __x__;
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(([getParserFn(parserA)(input), getParserFn(parserB)(input)])))));
  let map3 = (fn => parserA => parserB => parserC => Parser((input => ((__x__) => {
    if (__x__.length === 3 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true) {
      let [{ __args: [a]},{ __args: [b]},{ __args: [c]}] = __x__;
      return Right(fn(a)(b)(c));
    }
    else if (__x__.length === 3 && __x__[0].__constructor === "Left" && true && true && true) {
      let [{ __args: [e]},,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 3 && true && __x__[1].__constructor === "Left" && true && true) {
      let [,{ __args: [e]},] = __x__;
      return Left(e);
    }
    else if (__x__.length === 3 && true && true && __x__[2].__constructor === "Left" && true) {
      let [,,{ __args: [e]}] = __x__;
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(([getParserFn(parserA)(input), getParserFn(parserB)(input), getParserFn(parserC)(input)])))));
  let map4 = (fn => parserA => parserB => parserC => parserD => Parser((input => ((__x__) => {
    if (__x__.length === 4 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true && __x__[3].__constructor === "Right" && true) {
      let [{ __args: [a]},{ __args: [b]},{ __args: [c]},{ __args: [d]}] = __x__;
      return Right(fn(a)(b)(c)(d));
    }
    else if (__x__.length === 4 && __x__[0].__constructor === "Left" && true && true && true && true) {
      let [{ __args: [e]},,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 4 && true && __x__[1].__constructor === "Left" && true && true && true) {
      let [,{ __args: [e]},,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 4 && true && true && __x__[2].__constructor === "Left" && true && true) {
      let [,,{ __args: [e]},] = __x__;
      return Left(e);
    }
    else if (__x__.length === 4 && true && true && true && __x__[3].__constructor === "Left" && true) {
      let [,,,{ __args: [e]}] = __x__;
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(([getParserFn(parserA)(input), getParserFn(parserB)(input), getParserFn(parserC)(input), getParserFn(parserD)(input)])))));
  let map5 = (fn => parserA => parserB => parserC => parserD => parserE => Parser((input => ((__x__) => {
    if (__x__.length === 5 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true && __x__[3].__constructor === "Right" && true && __x__[4].__constructor === "Right" && true) {
      let [{ __args: [a]},{ __args: [b]},{ __args: [c]},{ __args: [d]},{ __args: [e]}] = __x__;
      return Right(fn(a)(b)(c)(d)(e));
    }
    else if (__x__.length === 5 && __x__[0].__constructor === "Left" && true && true && true && true && true) {
      let [{ __args: [e]},,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 5 && true && __x__[1].__constructor === "Left" && true && true && true && true) {
      let [,{ __args: [e]},,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 5 && true && true && __x__[2].__constructor === "Left" && true && true && true) {
      let [,,{ __args: [e]},,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 5 && true && true && true && __x__[3].__constructor === "Left" && true && true) {
      let [,,,{ __args: [e]},] = __x__;
      return Left(e);
    }
    else if (__x__.length === 5 && true && true && true && true && __x__[4].__constructor === "Left" && true) {
      let [,,,,{ __args: [e]}] = __x__;
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(([getParserFn(parserA)(input), getParserFn(parserB)(input), getParserFn(parserC)(input), getParserFn(parserD)(input), getParserFn(parserE)(input)])))));
  let map6 = (fn => parserA => parserB => parserC => parserD => parserE => parserF => Parser((input => ((__x__) => {
    if (__x__.length === 6 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true && __x__[3].__constructor === "Right" && true && __x__[4].__constructor === "Right" && true && __x__[5].__constructor === "Right" && true) {
      let [{ __args: [a]},{ __args: [b]},{ __args: [c]},{ __args: [d]},{ __args: [e]},{ __args: [f]}] = __x__;
      return Right(fn(a)(b)(c)(d)(e)(f));
    }
    else if (__x__.length === 6 && __x__[0].__constructor === "Left" && true && true && true && true && true && true) {
      let [{ __args: [e]},,,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 6 && true && __x__[1].__constructor === "Left" && true && true && true && true && true) {
      let [,{ __args: [e]},,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 6 && true && true && __x__[2].__constructor === "Left" && true && true && true && true) {
      let [,,{ __args: [e]},,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 6 && true && true && true && __x__[3].__constructor === "Left" && true && true && true) {
      let [,,,{ __args: [e]},,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 6 && true && true && true && true && __x__[4].__constructor === "Left" && true && true) {
      let [,,,,{ __args: [e]},] = __x__;
      return Left(e);
    }
    else if (__x__.length === 6 && true && true && true && true && true && __x__[5].__constructor === "Left" && true) {
      let [,,,,,{ __args: [e]}] = __x__;
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(([getParserFn(parserA)(input), getParserFn(parserB)(input), getParserFn(parserC)(input), getParserFn(parserD)(input), getParserFn(parserE)(input), getParserFn(parserF)(input)])))));
  let map7 = (fn => parserA => parserB => parserC => parserD => parserE => parserF => parserG => Parser((input => ((__x__) => {
    if (__x__.length === 7 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true && __x__[3].__constructor === "Right" && true && __x__[4].__constructor === "Right" && true && __x__[5].__constructor === "Right" && true && __x__[6].__constructor === "Right" && true) {
      let [{ __args: [a]},{ __args: [b]},{ __args: [c]},{ __args: [d]},{ __args: [e]},{ __args: [f]},{ __args: [g]}] = __x__;
      return Right(fn(a)(b)(c)(d)(e)(f)(g));
    }
    else if (__x__.length === 7 && __x__[0].__constructor === "Left" && true && true && true && true && true && true && true) {
      let [{ __args: [e]},,,,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 7 && true && __x__[1].__constructor === "Left" && true && true && true && true && true && true) {
      let [,{ __args: [e]},,,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 7 && true && true && __x__[2].__constructor === "Left" && true && true && true && true && true) {
      let [,,{ __args: [e]},,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 7 && true && true && true && __x__[3].__constructor === "Left" && true && true && true && true) {
      let [,,,{ __args: [e]},,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 7 && true && true && true && true && __x__[4].__constructor === "Left" && true && true && true) {
      let [,,,,{ __args: [e]},,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 7 && true && true && true && true && true && __x__[5].__constructor === "Left" && true && true) {
      let [,,,,,{ __args: [e]},] = __x__;
      return Left(e);
    }
    else if (__x__.length === 7 && true && true && true && true && true && true && __x__[6].__constructor === "Left" && true) {
      let [,,,,,,{ __args: [e]}] = __x__;
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(([getParserFn(parserA)(input), getParserFn(parserB)(input), getParserFn(parserC)(input), getParserFn(parserD)(input), getParserFn(parserE)(input), getParserFn(parserF)(input), getParserFn(parserG)(input)])))));
  let map8 = (fn => parserA => parserB => parserC => parserD => parserE => parserF => parserG => parserH => Parser((input => ((__x__) => {
    if (__x__.length === 8 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true && __x__[3].__constructor === "Right" && true && __x__[4].__constructor === "Right" && true && __x__[5].__constructor === "Right" && true && __x__[6].__constructor === "Right" && true && __x__[7].__constructor === "Right" && true) {
      let [{ __args: [a]},{ __args: [b]},{ __args: [c]},{ __args: [d]},{ __args: [e]},{ __args: [f]},{ __args: [g]},{ __args: [h]}] = __x__;
      return Right(fn(a)(b)(c)(d)(e)(f)(g)(h));
    }
    else if (__x__.length === 8 && __x__[0].__constructor === "Left" && true && true && true && true && true && true && true && true) {
      let [{ __args: [e]},,,,,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 8 && true && __x__[1].__constructor === "Left" && true && true && true && true && true && true && true) {
      let [,{ __args: [e]},,,,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 8 && true && true && __x__[2].__constructor === "Left" && true && true && true && true && true && true) {
      let [,,{ __args: [e]},,,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 8 && true && true && true && __x__[3].__constructor === "Left" && true && true && true && true && true) {
      let [,,,{ __args: [e]},,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 8 && true && true && true && true && __x__[4].__constructor === "Left" && true && true && true && true) {
      let [,,,,{ __args: [e]},,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 8 && true && true && true && true && true && __x__[5].__constructor === "Left" && true && true && true) {
      let [,,,,,{ __args: [e]},,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 8 && true && true && true && true && true && true && __x__[6].__constructor === "Left" && true && true) {
      let [,,,,,,{ __args: [e]},] = __x__;
      return Left(e);
    }
    else if (__x__.length === 8 && true && true && true && true && true && true && true && __x__[7].__constructor === "Left" && true) {
      let [,,,,,,,{ __args: [e]}] = __x__;
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(([getParserFn(parserA)(input), getParserFn(parserB)(input), getParserFn(parserC)(input), getParserFn(parserD)(input), getParserFn(parserE)(input), getParserFn(parserF)(input), getParserFn(parserG)(input), getParserFn(parserH)(input)])))));
  var Json = { parse, succeed, fail, string, integer, float, boolean, list, dict, maybe, lazy, field, path, chain1, chain2, chain3, chain4, chain5, chain6, chain7, chain8, map1, map2, map3, map4, map5, map6, map7, map8, Parser };

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Parser/Documentation.mad

  let BothTargets = (a => b => ({ __constructor: "BothTargets", __args: [ a, b ] }));
  let JSTarget = (a => ({ __constructor: "JSTarget", __args: [ a ] }));
  let LLVMTarget = (a => ({ __constructor: "LLVMTarget", __args: [ a ] }));
  Inspect['Record_constraintsf_0059980c9d93cf2de3fe268b3d9d26775_declarationf_1059980c9d93cf2de3fe268b3d9d26775_descriptionf_2059980c9d93cf2de3fe268b3d9d26775_examplef_3059980c9d93cf2de3fe268b3d9d26775_sincef_4059980c9d93cf2de3fe268b3d9d26775'] = {};
  Inspect['Record_constraintsf_0059980c9d93cf2de3fe268b3d9d26775_declarationf_1059980c9d93cf2de3fe268b3d9d26775_descriptionf_2059980c9d93cf2de3fe268b3d9d26775_examplef_3059980c9d93cf2de3fe268b3d9d26775_sincef_4059980c9d93cf2de3fe268b3d9d26775']['inspect'] = () => (Inspect_e914) => (Inspect_x907) => (Inspect_q900) => (Inspect_j893) => (Inspect_c886) => (__$a__ => `{ ` + `constraints: ` + Inspect_c886.inspect()(__$a__.constraints) + `, ` + `declaration: ` + Inspect_j893.inspect()(__$a__.declaration) + `, ` + `description: ` + Inspect_q900.inspect()(__$a__.description) + `, ` + `example: ` + Inspect_x907.inspect()(__$a__.example) + `, ` + `since: ` + Inspect_e914.inspect()(__$a__.since) + ` }`);
  Inspect['Record_constraintsf_0059980c9d93cf2de3fe268b3d9d26775_descriptionf_1059980c9d93cf2de3fe268b3d9d26775_examplef_2059980c9d93cf2de3fe268b3d9d26775_methodsf_3059980c9d93cf2de3fe268b3d9d26775_namef_4059980c9d93cf2de3fe268b3d9d26775_sincef_5059980c9d93cf2de3fe268b3d9d26775_varsf_6059980c9d93cf2de3fe268b3d9d26775'] = {};
  Inspect['Record_constraintsf_0059980c9d93cf2de3fe268b3d9d26775_descriptionf_1059980c9d93cf2de3fe268b3d9d26775_examplef_2059980c9d93cf2de3fe268b3d9d26775_methodsf_3059980c9d93cf2de3fe268b3d9d26775_namef_4059980c9d93cf2de3fe268b3d9d26775_sincef_5059980c9d93cf2de3fe268b3d9d26775_varsf_6059980c9d93cf2de3fe268b3d9d26775']['inspect'] = () => (Inspect_i970) => (Inspect_b963) => (Inspect_u956) => (Inspect_n949) => (Inspect_g942) => (Inspect_z935) => (Inspect_s928) => (__$a__ => `{ ` + `constraints: ` + Inspect_s928.inspect()(__$a__.constraints) + `, ` + `description: ` + Inspect_z935.inspect()(__$a__.description) + `, ` + `example: ` + Inspect_g942.inspect()(__$a__.example) + `, ` + `methods: ` + Inspect_n949.inspect()(__$a__.methods) + `, ` + `name: ` + Inspect_u956.inspect()(__$a__.name) + `, ` + `since: ` + Inspect_b963.inspect()(__$a__.since) + `, ` + `vars: ` + Inspect_i970.inspect()(__$a__.vars) + ` }`);
  Inspect['Record_aliasedTypef_0059980c9d93cf2de3fe268b3d9d26775_descriptionf_1059980c9d93cf2de3fe268b3d9d26775_examplef_2059980c9d93cf2de3fe268b3d9d26775_namef_3059980c9d93cf2de3fe268b3d9d26775_paramsf_4059980c9d93cf2de3fe268b3d9d26775_sincef_5059980c9d93cf2de3fe268b3d9d26775'] = {};
  Inspect['Record_aliasedTypef_0059980c9d93cf2de3fe268b3d9d26775_descriptionf_1059980c9d93cf2de3fe268b3d9d26775_examplef_2059980c9d93cf2de3fe268b3d9d26775_namef_3059980c9d93cf2de3fe268b3d9d26775_paramsf_4059980c9d93cf2de3fe268b3d9d26775_sincef_5059980c9d93cf2de3fe268b3d9d26775']['inspect'] = () => (Inspect_h1021) => (Inspect_a1014) => (Inspect_t1007) => (Inspect_m1000) => (Inspect_f993) => (Inspect_y986) => (__$a__ => `{ ` + `aliasedType: ` + Inspect_y986.inspect()(__$a__.aliasedType) + `, ` + `description: ` + Inspect_f993.inspect()(__$a__.description) + `, ` + `example: ` + Inspect_m1000.inspect()(__$a__.example) + `, ` + `name: ` + Inspect_t1007.inspect()(__$a__.name) + `, ` + `params: ` + Inspect_a1014.inspect()(__$a__.params) + `, ` + `since: ` + Inspect_h1021.inspect()(__$a__.since) + ` }`);
  Inspect['Record_constructorsf_0059980c9d93cf2de3fe268b3d9d26775_descriptionf_1059980c9d93cf2de3fe268b3d9d26775_examplef_2059980c9d93cf2de3fe268b3d9d26775_namef_3059980c9d93cf2de3fe268b3d9d26775_paramsf_4059980c9d93cf2de3fe268b3d9d26775_sincef_5059980c9d93cf2de3fe268b3d9d26775'] = {};
  Inspect['Record_constructorsf_0059980c9d93cf2de3fe268b3d9d26775_descriptionf_1059980c9d93cf2de3fe268b3d9d26775_examplef_2059980c9d93cf2de3fe268b3d9d26775_namef_3059980c9d93cf2de3fe268b3d9d26775_paramsf_4059980c9d93cf2de3fe268b3d9d26775_sincef_5059980c9d93cf2de3fe268b3d9d26775']['inspect'] = () => (Inspect_f1071) => (Inspect_y1064) => (Inspect_r1057) => (Inspect_k1050) => (Inspect_d1043) => (Inspect_w1036) => (__$a__ => `{ ` + `constructors: ` + Inspect_w1036.inspect()(__$a__.constructors) + `, ` + `description: ` + Inspect_d1043.inspect()(__$a__.description) + `, ` + `example: ` + Inspect_k1050.inspect()(__$a__.example) + `, ` + `name: ` + Inspect_r1057.inspect()(__$a__.name) + `, ` + `params: ` + Inspect_y1064.inspect()(__$a__.params) + `, ` + `since: ` + Inspect_f1071.inspect()(__$a__.since) + ` }`);
  Inspect['Record_descriptionf_0059980c9d93cf2de3fe268b3d9d26775_examplef_1059980c9d93cf2de3fe268b3d9d26775_namef_2059980c9d93cf2de3fe268b3d9d26775_sincef_3059980c9d93cf2de3fe268b3d9d26775_typingf_4059980c9d93cf2de3fe268b3d9d26775'] = {};
  Inspect['Record_descriptionf_0059980c9d93cf2de3fe268b3d9d26775_examplef_1059980c9d93cf2de3fe268b3d9d26775_namef_2059980c9d93cf2de3fe268b3d9d26775_sincef_3059980c9d93cf2de3fe268b3d9d26775_typingf_4059980c9d93cf2de3fe268b3d9d26775']['inspect'] = () => (Inspect_w1114) => (Inspect_p1107) => (Inspect_i1100) => (Inspect_b1093) => (Inspect_u1086) => (__$a__ => `{ ` + `description: ` + Inspect_u1086.inspect()(__$a__.description) + `, ` + `example: ` + Inspect_b1093.inspect()(__$a__.example) + `, ` + `name: ` + Inspect_i1100.inspect()(__$a__.name) + `, ` + `since: ` + Inspect_p1107.inspect()(__$a__.since) + `, ` + `typing: ` + Inspect_w1114.inspect()(__$a__.typing) + ` }`);
  Inspect['Targeted_059980c9d93cf2de3fe268b3d9d26775'] = {};
  Inspect['Targeted_059980c9d93cf2de3fe268b3d9d26775']['inspect'] = () => (Inspect_i1126) => (__$a__ => ((__x__) => {
    if (__x__.__constructor === "BothTargets" && true && true) {
      let a0 = __x__.__args[0];
      let a1 = __x__.__args[1];
      return `BothTargets(` + Inspect_i1126.inspect()(a0) + `, ` + Inspect_i1126.inspect()(a1) + `)`;
    }
    else if (__x__.__constructor === "JSTarget" && true) {
      let a0 = __x__.__args[0];
      return `JSTarget(` + Inspect_i1126.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "LLVMTarget" && true) {
      let a0 = __x__.__args[0];
      return `LLVMTarget(` + Inspect_i1126.inspect()(a0) + `)`;
    }
    else {
      return `Unknown`;
    }
  })(__$a__));
  Inspect['Record_aliasesf_0059980c9d93cf2de3fe268b3d9d26775_descriptionf_1059980c9d93cf2de3fe268b3d9d26775_expressionsf_2059980c9d93cf2de3fe268b3d9d26775_instancesf_3059980c9d93cf2de3fe268b3d9d26775_interfacesf_4059980c9d93cf2de3fe268b3d9d26775_namef_5059980c9d93cf2de3fe268b3d9d26775_pathf_6059980c9d93cf2de3fe268b3d9d26775_typeDeclarationsf_7059980c9d93cf2de3fe268b3d9d26775'] = {};
  Inspect['Record_aliasesf_0059980c9d93cf2de3fe268b3d9d26775_descriptionf_1059980c9d93cf2de3fe268b3d9d26775_expressionsf_2059980c9d93cf2de3fe268b3d9d26775_instancesf_3059980c9d93cf2de3fe268b3d9d26775_interfacesf_4059980c9d93cf2de3fe268b3d9d26775_namef_5059980c9d93cf2de3fe268b3d9d26775_pathf_6059980c9d93cf2de3fe268b3d9d26775_typeDeclarationsf_7059980c9d93cf2de3fe268b3d9d26775']['inspect'] = () => (Inspect_e1200) => (Inspect_x1193) => (Inspect_q1186) => (Inspect_j1179) => (Inspect_c1172) => (Inspect_v1165) => (Inspect_o1158) => (Inspect_h1151) => (__$a__ => `{ ` + `aliases: ` + Inspect_h1151.inspect()(__$a__.aliases) + `, ` + `description: ` + Inspect_o1158.inspect()(__$a__.description) + `, ` + `expressions: ` + Inspect_v1165.inspect()(__$a__.expressions) + `, ` + `instances: ` + Inspect_c1172.inspect()(__$a__.instances) + `, ` + `interfaces: ` + Inspect_j1179.inspect()(__$a__.interfaces) + `, ` + `name: ` + Inspect_q1186.inspect()(__$a__.name) + `, ` + `path: ` + Inspect_x1193.inspect()(__$a__.path) + `, ` + `typeDeclarations: ` + Inspect_e1200.inspect()(__$a__.typeDeclarations) + ` }`);
  let getName$1 = (targeted => ((__x__) => {
    if (__x__.__constructor === "BothTargets" && true && true) {
      let a = __x__.__args[0];
      return a.name;
    }
    else if (__x__.__constructor === "JSTarget" && true) {
      let a = __x__.__args[0];
      return a.name;
    }
    else if (__x__.__constructor === "LLVMTarget" && true) {
      let a = __x__.__args[0];
      return a.name;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(targeted));
  let makeInstance = (declaration => constraints => description => since => example => ({ declaration: declaration, constraints: constraints, description: description, since: since, example: example }));
  let makeInterface = (name => vars => constraints => methods => description => since => example => ({ name: name, vars: vars, constraints: constraints, methods: methods, description: description, since: since, example: example }));
  let makeAlias = (name => params => aliasedType => description => since => example => ({ name: name, params: params, aliasedType: aliasedType, description: description, since: since, example: example }));
  let makeType = (name => params => constructors => description => since => example => ({ name: name, params: params, constructors: constructors, description: description, since: since, example: example }));
  let makeExpression = (name => description => typing => since => example => ({ name: name, description: description, typing: typing, since: since, example: example }));
  let makeTargeted = (maybeJS => maybeLLVM => ((__x__) => {
    if (__x__.length === 2 && __x__[0].__constructor === "Just" && true && __x__[1].__constructor === "Just" && true) {
      let [{ __args: [js]},{ __args: [llvm]}] = __x__;
      return BothTargets(js)(llvm);
    }
    else if (__x__.length === 2 && __x__[0].__constructor === "Just" && true && __x__[1].__constructor === "Nothing") {
      let [{ __args: [js]},{ __args: []}] = __x__;
      return JSTarget(js);
    }
    else if (__x__.length === 2 && __x__[0].__constructor === "Nothing" && __x__[1].__constructor === "Just" && true) {
      let [{ __args: []},{ __args: [llvm]}] = __x__;
      return LLVMTarget(llvm);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(([maybeJS, maybeLLVM])));
  let makeModule = (path => name => description => expressions => typeDeclarations => aliases => interfaces => instances => ({ path: path, name: name, description: description, expressions: expressions, typeDeclarations: typeDeclarations, aliases: aliases, interfaces: interfaces, instances: instances }));
  let expressionParser = Json.map5(makeExpression)(Json.field(`name`)(Json.string))(Json.field(`description`)(Json.string))(Json.field(`type`)(Json.string))(Json.field(`since`)(Json.string))(Json.field(`example`)(Json.string));
  let parser = Json.field(`modules`)(Json.list(Json.map8(makeModule)(Json.field(`path`)(Json.string))(Json.field(`moduleName`)(Json.string))(Json.field(`description`)(Json.string))(Json.field(`expressions`)(Json.list(Json.map2(makeTargeted)(Json.maybe(Json.field(`js`)(expressionParser)))(Json.maybe(Json.field(`llvm`)(expressionParser))))))(Json.field(`typeDeclarations`)(Json.list(Json.map6(makeType)(Json.path(({ v: `js`, n: { v: `name`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `params`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `constructors`, n: null } }))(Json.list(Json.string)))(Json.path(({ v: `js`, n: { v: `description`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `since`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `example`, n: null } }))(Json.string)))))(Json.field(`aliases`)(Json.list(Json.map6(makeAlias)(Json.path(({ v: `js`, n: { v: `name`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `params`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `aliasedType`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `description`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `since`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `example`, n: null } }))(Json.string)))))(Json.field(`interfaces`)(Json.list(Json.map7(makeInterface)(Json.path(({ v: `js`, n: { v: `name`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `vars`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `constraints`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `methods`, n: null } }))(Json.list(Json.string)))(Json.path(({ v: `js`, n: { v: `description`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `since`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `example`, n: null } }))(Json.string)))))(Json.field(`instances`)(Json.list(Json.map5(makeInstance)(Json.path(({ v: `js`, n: { v: `declaration`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `constraints`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `description`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `since`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `example`, n: null } }))(Json.string)))))));

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/State.mad

  let LLVM = ({ __constructor: "LLVM", __args: [  ] });
  let JS = ({ __constructor: "JS", __args: [  ] });
  Inspect['Target_80f4a43bf97979d3e4a8a496cd64fec2'] = {};
  Inspect['Target_80f4a43bf97979d3e4a8a496cd64fec2']['inspect'] = () => (__$a__ => ((__x__) => {
    if (__x__.__constructor === "LLVM") {
      return `LLVM`;
    }
    else if (__x__.__constructor === "JS") {
      return `JS`;
    }
    else {
      return `Unknown`;
    }
  })(__$a__));
  Inspect['Record_modulesf_080f4a43bf97979d3e4a8a496cd64fec2_pathf_180f4a43bf97979d3e4a8a496cd64fec2_searchf_280f4a43bf97979d3e4a8a496cd64fec2_targetf_380f4a43bf97979d3e4a8a496cd64fec2'] = {};
  Inspect['Record_modulesf_080f4a43bf97979d3e4a8a496cd64fec2_pathf_180f4a43bf97979d3e4a8a496cd64fec2_searchf_280f4a43bf97979d3e4a8a496cd64fec2_targetf_380f4a43bf97979d3e4a8a496cd64fec2']['inspect'] = () => (Inspect_p93) => (Inspect_i86) => (Inspect_b79) => (Inspect_u72) => (__$a__ => `{ ` + `modules: ` + Inspect_u72.inspect()(__$a__.modules) + `, ` + `path: ` + Inspect_b79.inspect()(__$a__.path) + `, ` + `search: ` + Inspect_i86.inspect()(__$a__.search) + `, ` + `target: ` + Inspect_p93.inspect()(__$a__.target) + ` }`);

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Wish.mad

  let Wish = (a => ({ __constructor: "Wish", __args: [ a ] }));
  Inspect['Wish_48091bbb4c188d584814a4a3f8207f71'] = {};
  Inspect['Wish_48091bbb4c188d584814a4a3f8207f71']['inspect'] = () => (Inspect_m350) => (Inspect_o352) => (__$a__ => ((__x__) => {
    if (__x__.__constructor === "Wish" && true) {
      let a0 = __x__.__args[0];
      return `Wish(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else {
      return `Unknown`;
    }
  })(__$a__));
  Functor['Wish_48091bbb4c188d584814a4a3f8207f71'] = {};
  Functor['Wish_48091bbb4c188d584814a4a3f8207f71']['map'] = () => (f => m => Wish((badCB => goodCB => ((__x__) => {
    if (__x__.__constructor === "Wish" && true) {
      let run = __x__.__args[0];
      return run(badCB)((x => goodCB(f(x))));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(m))));
  Applicative['Wish_48091bbb4c188d584814a4a3f8207f71'] = {};
  Applicative['Wish_48091bbb4c188d584814a4a3f8207f71']['ap'] = () => (mf => m => Wish((badCB => goodCB => ((__x__) => {
    if (__x__.length === 2 && __x__[0].__constructor === "Wish" && true && __x__[1].__constructor === "Wish" && true) {
      let [{ __args: [runMF]},{ __args: [runM]}] = __x__;
      return runM(badCB)((x => runMF(badCB)((f => goodCB(f(x))))));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(([mf, m])))));
  Applicative['Wish_48091bbb4c188d584814a4a3f8207f71']['pure'] = () => (a => Wish((_ => goodCB => goodCB(a))));
  Monad['Wish_48091bbb4c188d584814a4a3f8207f71'] = {};
  Monad['Wish_48091bbb4c188d584814a4a3f8207f71']['chain'] = () => (f => m => Wish((badCB => goodCB => ((__x__) => {
    if (__x__.__constructor === "Wish" && true) {
      let run = __x__.__args[0];
      return run(badCB)((x => ((__x__) => {
    if (__x__.__constructor === "Wish" && true) {
      let r = __x__.__args[0];
      return r(badCB)(goodCB);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(f(x))));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(m))));
  Monad['Wish_48091bbb4c188d584814a4a3f8207f71']['of'] = () => Applicative.Wish_48091bbb4c188d584814a4a3f8207f71.pure();
  Bifunctor['Wish_48091bbb4c188d584814a4a3f8207f71'] = {};
  Bifunctor['Wish_48091bbb4c188d584814a4a3f8207f71']['bimap'] = () => (leftF => rightF => m => Wish((badCB => goodCB => ((__x__) => {
    if (__x__.__constructor === "Wish" && true) {
      let run = __x__.__args[0];
      return run((_P_ => badCB(leftF(_P_))))((_P_ => goodCB(rightF(_P_))));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(m))));
  Bifunctor['Wish_48091bbb4c188d584814a4a3f8207f71']['mapFirst'] = () => mapRej;
  Bifunctor['Wish_48091bbb4c188d584814a4a3f8207f71']['mapSecond'] = () => Functor.Wish_48091bbb4c188d584814a4a3f8207f71.map();
  let mapRej = (f => m => Wish((badCB => goodCB => ((__x__) => {
    if (__x__.__constructor === "Wish" && true) {
      let run = __x__.__args[0];
      return run((x => badCB(f(x))))(goodCB);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(m))));
  let fulfill = (badCB => goodCB => m => {
      ((__x__) => {
    if (__x__.__constructor === "Wish" && true) {
      let run = __x__.__args[0];
      return  setTimeout(() => run(badCB)(goodCB), 0);  }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(m);
      return ({ __constructor: "Unit", __args: [] });
  });

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadUI/src/Key.mad

  let KEY_ANY = ({ __constructor: "KEY_ANY", __args: [  ] });
  let KEY_BREAK = ({ __constructor: "KEY_BREAK", __args: [  ] });
  let KEY_BACKSPACE = ({ __constructor: "KEY_BACKSPACE", __args: [  ] });
  let KEY_TAB = ({ __constructor: "KEY_TAB", __args: [  ] });
  let KEY_CLEAR = ({ __constructor: "KEY_CLEAR", __args: [  ] });
  let KEY_ENTER = ({ __constructor: "KEY_ENTER", __args: [  ] });
  let KEY_SHIFT = ({ __constructor: "KEY_SHIFT", __args: [  ] });
  let KEY_CTRL = ({ __constructor: "KEY_CTRL", __args: [  ] });
  let KEY_ALT = ({ __constructor: "KEY_ALT", __args: [  ] });
  let KEY_PAUSE = ({ __constructor: "KEY_PAUSE", __args: [  ] });
  let KEY_CAPS_LOCK = ({ __constructor: "KEY_CAPS_LOCK", __args: [  ] });
  let KEY_HANGUL = ({ __constructor: "KEY_HANGUL", __args: [  ] });
  let KEY_HANJA = ({ __constructor: "KEY_HANJA", __args: [  ] });
  let KEY_ESCAPE = ({ __constructor: "KEY_ESCAPE", __args: [  ] });
  let KEY_CONVERSION = ({ __constructor: "KEY_CONVERSION", __args: [  ] });
  let KEY_NON_CONVERSION = ({ __constructor: "KEY_NON_CONVERSION", __args: [  ] });
  let KEY_SPACE = ({ __constructor: "KEY_SPACE", __args: [  ] });
  let KEY_PAGE_UP = ({ __constructor: "KEY_PAGE_UP", __args: [  ] });
  let KEY_PAGE_DOWN = ({ __constructor: "KEY_PAGE_DOWN", __args: [  ] });
  let KEY_END = ({ __constructor: "KEY_END", __args: [  ] });
  let KEY_HOME = ({ __constructor: "KEY_HOME", __args: [  ] });
  let KEY_LEFT_ARROW = ({ __constructor: "KEY_LEFT_ARROW", __args: [  ] });
  let KEY_UP_ARROW = ({ __constructor: "KEY_UP_ARROW", __args: [  ] });
  let KEY_RIGHT_ARROW = ({ __constructor: "KEY_RIGHT_ARROW", __args: [  ] });
  let KEY_DOWN_ARROW = ({ __constructor: "KEY_DOWN_ARROW", __args: [  ] });
  let KEY_SELECT = ({ __constructor: "KEY_SELECT", __args: [  ] });
  let KEY_PRINT = ({ __constructor: "KEY_PRINT", __args: [  ] });
  let KEY_EXECUTE = ({ __constructor: "KEY_EXECUTE", __args: [  ] });
  let KEY_PRINT_SCREEN = ({ __constructor: "KEY_PRINT_SCREEN", __args: [  ] });
  let KEY_INSERT = ({ __constructor: "KEY_INSERT", __args: [  ] });
  let KEY_DELETE = ({ __constructor: "KEY_DELETE", __args: [  ] });
  let KEY_HELP = ({ __constructor: "KEY_HELP", __args: [  ] });
  let KEY_0 = ({ __constructor: "KEY_0", __args: [  ] });
  let KEY_1 = ({ __constructor: "KEY_1", __args: [  ] });
  let KEY_2 = ({ __constructor: "KEY_2", __args: [  ] });
  let KEY_3 = ({ __constructor: "KEY_3", __args: [  ] });
  let KEY_4 = ({ __constructor: "KEY_4", __args: [  ] });
  let KEY_5 = ({ __constructor: "KEY_5", __args: [  ] });
  let KEY_6 = ({ __constructor: "KEY_6", __args: [  ] });
  let KEY_7 = ({ __constructor: "KEY_7", __args: [  ] });
  let KEY_8 = ({ __constructor: "KEY_8", __args: [  ] });
  let KEY_9 = ({ __constructor: "KEY_9", __args: [  ] });
  let KEY_COLON = ({ __constructor: "KEY_COLON", __args: [  ] });
  let KEY_LEFT_CHEVRON = ({ __constructor: "KEY_LEFT_CHEVRON", __args: [  ] });
  let KEY_EQUAL = ({ __constructor: "KEY_EQUAL", __args: [  ] });
  let KEY_ESZETT = ({ __constructor: "KEY_ESZETT", __args: [  ] });
  let KEY_AT = ({ __constructor: "KEY_AT", __args: [  ] });
  let KEY_A = ({ __constructor: "KEY_A", __args: [  ] });
  let KEY_B = ({ __constructor: "KEY_B", __args: [  ] });
  let KEY_C = ({ __constructor: "KEY_C", __args: [  ] });
  let KEY_D = ({ __constructor: "KEY_D", __args: [  ] });
  let KEY_E = ({ __constructor: "KEY_E", __args: [  ] });
  let KEY_F = ({ __constructor: "KEY_F", __args: [  ] });
  let KEY_G = ({ __constructor: "KEY_G", __args: [  ] });
  let KEY_H = ({ __constructor: "KEY_H", __args: [  ] });
  let KEY_I = ({ __constructor: "KEY_I", __args: [  ] });
  let KEY_J = ({ __constructor: "KEY_J", __args: [  ] });
  let KEY_K = ({ __constructor: "KEY_K", __args: [  ] });
  let KEY_L = ({ __constructor: "KEY_L", __args: [  ] });
  let KEY_M = ({ __constructor: "KEY_M", __args: [  ] });
  let KEY_N = ({ __constructor: "KEY_N", __args: [  ] });
  let KEY_O = ({ __constructor: "KEY_O", __args: [  ] });
  let KEY_P = ({ __constructor: "KEY_P", __args: [  ] });
  let KEY_Q = ({ __constructor: "KEY_Q", __args: [  ] });
  let KEY_R = ({ __constructor: "KEY_R", __args: [  ] });
  let KEY_S = ({ __constructor: "KEY_S", __args: [  ] });
  let KEY_T = ({ __constructor: "KEY_T", __args: [  ] });
  let KEY_U = ({ __constructor: "KEY_U", __args: [  ] });
  let KEY_V = ({ __constructor: "KEY_V", __args: [  ] });
  let KEY_W = ({ __constructor: "KEY_W", __args: [  ] });
  let KEY_X = ({ __constructor: "KEY_X", __args: [  ] });
  let KEY_Y = ({ __constructor: "KEY_Y", __args: [  ] });
  let KEY_Z = ({ __constructor: "KEY_Z", __args: [  ] });
  let KEY_CMD_LEFT = ({ __constructor: "KEY_CMD_LEFT", __args: [  ] });
  let KEY_CMD_RIGHT = ({ __constructor: "KEY_CMD_RIGHT", __args: [  ] });
  let KEY_SLEEP = ({ __constructor: "KEY_SLEEP", __args: [  ] });
  let KEY_NUMPAD_0 = ({ __constructor: "KEY_NUMPAD_0", __args: [  ] });
  let KEY_NUMPAD_1 = ({ __constructor: "KEY_NUMPAD_1", __args: [  ] });
  let KEY_NUMPAD_2 = ({ __constructor: "KEY_NUMPAD_2", __args: [  ] });
  let KEY_NUMPAD_3 = ({ __constructor: "KEY_NUMPAD_3", __args: [  ] });
  let KEY_NUMPAD_4 = ({ __constructor: "KEY_NUMPAD_4", __args: [  ] });
  let KEY_NUMPAD_5 = ({ __constructor: "KEY_NUMPAD_5", __args: [  ] });
  let KEY_NUMPAD_6 = ({ __constructor: "KEY_NUMPAD_6", __args: [  ] });
  let KEY_NUMPAD_7 = ({ __constructor: "KEY_NUMPAD_7", __args: [  ] });
  let KEY_NUMPAD_8 = ({ __constructor: "KEY_NUMPAD_8", __args: [  ] });
  let KEY_NUMPAD_9 = ({ __constructor: "KEY_NUMPAD_9", __args: [  ] });
  let KEY_MULTIPLY = ({ __constructor: "KEY_MULTIPLY", __args: [  ] });
  let KEY_ADD = ({ __constructor: "KEY_ADD", __args: [  ] });
  let KEY_NUMPAD_PERIOD = ({ __constructor: "KEY_NUMPAD_PERIOD", __args: [  ] });
  let KEY_SUBSTRACT = ({ __constructor: "KEY_SUBSTRACT", __args: [  ] });
  let KEY_DECIMAL_POINT = ({ __constructor: "KEY_DECIMAL_POINT", __args: [  ] });
  let KEY_DIVIDE = ({ __constructor: "KEY_DIVIDE", __args: [  ] });
  let KEY_F1 = ({ __constructor: "KEY_F1", __args: [  ] });
  let KEY_F2 = ({ __constructor: "KEY_F2", __args: [  ] });
  let KEY_F3 = ({ __constructor: "KEY_F3", __args: [  ] });
  let KEY_F4 = ({ __constructor: "KEY_F4", __args: [  ] });
  let KEY_F5 = ({ __constructor: "KEY_F5", __args: [  ] });
  let KEY_F6 = ({ __constructor: "KEY_F6", __args: [  ] });
  let KEY_F7 = ({ __constructor: "KEY_F7", __args: [  ] });
  let KEY_F8 = ({ __constructor: "KEY_F8", __args: [  ] });
  let KEY_F9 = ({ __constructor: "KEY_F9", __args: [  ] });
  let KEY_F10 = ({ __constructor: "KEY_F10", __args: [  ] });
  let KEY_F11 = ({ __constructor: "KEY_F11", __args: [  ] });
  let KEY_F12 = ({ __constructor: "KEY_F12", __args: [  ] });
  let KEY_F13 = ({ __constructor: "KEY_F13", __args: [  ] });
  let KEY_F14 = ({ __constructor: "KEY_F14", __args: [  ] });
  let KEY_F15 = ({ __constructor: "KEY_F15", __args: [  ] });
  let KEY_F16 = ({ __constructor: "KEY_F16", __args: [  ] });
  let KEY_F17 = ({ __constructor: "KEY_F17", __args: [  ] });
  let KEY_F18 = ({ __constructor: "KEY_F18", __args: [  ] });
  let KEY_F19 = ({ __constructor: "KEY_F19", __args: [  ] });
  let KEY_F20 = ({ __constructor: "KEY_F20", __args: [  ] });
  let KEY_F21 = ({ __constructor: "KEY_F21", __args: [  ] });
  let KEY_F22 = ({ __constructor: "KEY_F22", __args: [  ] });
  let KEY_F23 = ({ __constructor: "KEY_F23", __args: [  ] });
  let KEY_F24 = ({ __constructor: "KEY_F24", __args: [  ] });
  let KEY_F25 = ({ __constructor: "KEY_F25", __args: [  ] });
  let KEY_F26 = ({ __constructor: "KEY_F26", __args: [  ] });
  let KEY_F27 = ({ __constructor: "KEY_F27", __args: [  ] });
  let KEY_F28 = ({ __constructor: "KEY_F28", __args: [  ] });
  let KEY_F29 = ({ __constructor: "KEY_F29", __args: [  ] });
  let KEY_F30 = ({ __constructor: "KEY_F30", __args: [  ] });
  let KEY_F31 = ({ __constructor: "KEY_F31", __args: [  ] });
  let KEY_F32 = ({ __constructor: "KEY_F32", __args: [  ] });
  let KEY_NUM_LOCK = ({ __constructor: "KEY_NUM_LOCK", __args: [  ] });
  let KEY_SCROLL_LOCK = ({ __constructor: "KEY_SCROLL_LOCK", __args: [  ] });
  let KEY_AIRPLANE_MODE = ({ __constructor: "KEY_AIRPLANE_MODE", __args: [  ] });
  let KEY_CIRCONFLEX = ({ __constructor: "KEY_CIRCONFLEX", __args: [  ] });
  let KEY_EXCLAMATION_MARK = ({ __constructor: "KEY_EXCLAMATION_MARK", __args: [  ] });
  let KEY_ARABIC_SEMI_COLON = ({ __constructor: "KEY_ARABIC_SEMI_COLON", __args: [  ] });
  let KEY_NUMBER_SIGN = ({ __constructor: "KEY_NUMBER_SIGN", __args: [  ] });
  let KEY_DOLLAR = ({ __constructor: "KEY_DOLLAR", __args: [  ] });
  let KEY_U_GRAVE_ACCENT = ({ __constructor: "KEY_U_GRAVE_ACCENT", __args: [  ] });
  let KEY_PAGE_BACKWARD = ({ __constructor: "KEY_PAGE_BACKWARD", __args: [  ] });
  let KEY_PAGE_FORWARD = ({ __constructor: "KEY_PAGE_FORWARD", __args: [  ] });
  let KEY_REFRESH = ({ __constructor: "KEY_REFRESH", __args: [  ] });
  let KEY_RIGHT_PAREN = ({ __constructor: "KEY_RIGHT_PAREN", __args: [  ] });
  let KEY_ASTERISK = ({ __constructor: "KEY_ASTERISK", __args: [  ] });
  let KEY_TILDE = ({ __constructor: "KEY_TILDE", __args: [  ] });
  let KEY_MUTE = ({ __constructor: "KEY_MUTE", __args: [  ] });
  let KEY_NEXT = ({ __constructor: "KEY_NEXT", __args: [  ] });
  let KEY_PREVIOUS = ({ __constructor: "KEY_PREVIOUS", __args: [  ] });
  let KEY_STOP = ({ __constructor: "KEY_STOP", __args: [  ] });
  let KEY_PLAY_PAUSE = ({ __constructor: "KEY_PLAY_PAUSE", __args: [  ] });
  let KEY_EMAIL = ({ __constructor: "KEY_EMAIL", __args: [  ] });
  let KEY_MUTE_UNMUTE = ({ __constructor: "KEY_MUTE_UNMUTE", __args: [  ] });
  let KEY_DECREASE_VOLUME = ({ __constructor: "KEY_DECREASE_VOLUME", __args: [  ] });
  let KEY_INCREASE_VOLUME = ({ __constructor: "KEY_INCREASE_VOLUME", __args: [  ] });
  let KEY_SEMI_COLON = ({ __constructor: "KEY_SEMI_COLON", __args: [  ] });
  let KEY_COMMA = ({ __constructor: "KEY_COMMA", __args: [  ] });
  let KEY_DASH = ({ __constructor: "KEY_DASH", __args: [  ] });
  let KEY_PERIOD = ({ __constructor: "KEY_PERIOD", __args: [  ] });
  let KEY_FORWARD_SLASH = ({ __constructor: "KEY_FORWARD_SLASH", __args: [  ] });
  let KEY_GRAVE_ACCENT = ({ __constructor: "KEY_GRAVE_ACCENT", __args: [  ] });
  let KEY_QUESTION_MARK = ({ __constructor: "KEY_QUESTION_MARK", __args: [  ] });
  let KEY_BRACKET_LEFT = ({ __constructor: "KEY_BRACKET_LEFT", __args: [  ] });
  let KEY_BACK_SLASH = ({ __constructor: "KEY_BACK_SLASH", __args: [  ] });
  let KEY_BRACKET_RIGHT = ({ __constructor: "KEY_BRACKET_RIGHT", __args: [  ] });
  let KEY_SINGLE_QUOTE = ({ __constructor: "KEY_SINGLE_QUOTE", __args: [  ] });
  let KEY_BACK_TICK = ({ __constructor: "KEY_BACK_TICK", __args: [  ] });
  let KEY_CMD = ({ __constructor: "KEY_CMD", __args: [  ] });
  let KEY_ALTGR = ({ __constructor: "KEY_ALTGR", __args: [  ] });
  let KEY_LEFT_BACK_SLASH = ({ __constructor: "KEY_LEFT_BACK_SLASH", __args: [  ] });
  let KEY_GNOME_COMPOSE = ({ __constructor: "KEY_GNOME_COMPOSE", __args: [  ] });
  let KEY_C_CEDILLA = ({ __constructor: "KEY_C_CEDILLA", __args: [  ] });
  let KEY_XF86_FORWARD = ({ __constructor: "KEY_XF86_FORWARD", __args: [  ] });
  let KEY_XF86_BACKWARD = ({ __constructor: "KEY_XF86_BACKWARD", __args: [  ] });
  let KEY_ALPHA_NUMERIC = ({ __constructor: "KEY_ALPHA_NUMERIC", __args: [  ] });
  let KEY_HIRAGANA_KATAKANA = ({ __constructor: "KEY_HIRAGANA_KATAKANA", __args: [  ] });
  let KEY_HALF_WIDTH_FULL_WIDTH = ({ __constructor: "KEY_HALF_WIDTH_FULL_WIDTH", __args: [  ] });
  let KEY_KANJI = ({ __constructor: "KEY_KANJI", __args: [  ] });
  let KEY_UNLOCK_TRACK_PAD = ({ __constructor: "KEY_UNLOCK_TRACK_PAD", __args: [  ] });
  let KEY_TOGGLE_TOUCH_PAD = ({ __constructor: "KEY_TOGGLE_TOUCH_PAD", __args: [  ] });
  Inspect['Key_29c16bbb90cf3a28d74dcca5983dc9c2'] = {};
  Inspect['Key_29c16bbb90cf3a28d74dcca5983dc9c2']['inspect'] = () => (__$a__ => ((__x__) => {
    if (__x__.__constructor === "KEY_ANY") {
      return `KEY_ANY`;
    }
    else if (__x__.__constructor === "KEY_BREAK") {
      return `KEY_BREAK`;
    }
    else if (__x__.__constructor === "KEY_BACKSPACE") {
      return `KEY_BACKSPACE`;
    }
    else if (__x__.__constructor === "KEY_TAB") {
      return `KEY_TAB`;
    }
    else if (__x__.__constructor === "KEY_CLEAR") {
      return `KEY_CLEAR`;
    }
    else if (__x__.__constructor === "KEY_ENTER") {
      return `KEY_ENTER`;
    }
    else if (__x__.__constructor === "KEY_SHIFT") {
      return `KEY_SHIFT`;
    }
    else if (__x__.__constructor === "KEY_CTRL") {
      return `KEY_CTRL`;
    }
    else if (__x__.__constructor === "KEY_ALT") {
      return `KEY_ALT`;
    }
    else if (__x__.__constructor === "KEY_PAUSE") {
      return `KEY_PAUSE`;
    }
    else if (__x__.__constructor === "KEY_CAPS_LOCK") {
      return `KEY_CAPS_LOCK`;
    }
    else if (__x__.__constructor === "KEY_HANGUL") {
      return `KEY_HANGUL`;
    }
    else if (__x__.__constructor === "KEY_HANJA") {
      return `KEY_HANJA`;
    }
    else if (__x__.__constructor === "KEY_ESCAPE") {
      return `KEY_ESCAPE`;
    }
    else if (__x__.__constructor === "KEY_CONVERSION") {
      return `KEY_CONVERSION`;
    }
    else if (__x__.__constructor === "KEY_NON_CONVERSION") {
      return `KEY_NON_CONVERSION`;
    }
    else if (__x__.__constructor === "KEY_SPACE") {
      return `KEY_SPACE`;
    }
    else if (__x__.__constructor === "KEY_PAGE_UP") {
      return `KEY_PAGE_UP`;
    }
    else if (__x__.__constructor === "KEY_PAGE_DOWN") {
      return `KEY_PAGE_DOWN`;
    }
    else if (__x__.__constructor === "KEY_END") {
      return `KEY_END`;
    }
    else if (__x__.__constructor === "KEY_HOME") {
      return `KEY_HOME`;
    }
    else if (__x__.__constructor === "KEY_LEFT_ARROW") {
      return `KEY_LEFT_ARROW`;
    }
    else if (__x__.__constructor === "KEY_UP_ARROW") {
      return `KEY_UP_ARROW`;
    }
    else if (__x__.__constructor === "KEY_RIGHT_ARROW") {
      return `KEY_RIGHT_ARROW`;
    }
    else if (__x__.__constructor === "KEY_DOWN_ARROW") {
      return `KEY_DOWN_ARROW`;
    }
    else if (__x__.__constructor === "KEY_SELECT") {
      return `KEY_SELECT`;
    }
    else if (__x__.__constructor === "KEY_PRINT") {
      return `KEY_PRINT`;
    }
    else if (__x__.__constructor === "KEY_EXECUTE") {
      return `KEY_EXECUTE`;
    }
    else if (__x__.__constructor === "KEY_PRINT_SCREEN") {
      return `KEY_PRINT_SCREEN`;
    }
    else if (__x__.__constructor === "KEY_INSERT") {
      return `KEY_INSERT`;
    }
    else if (__x__.__constructor === "KEY_DELETE") {
      return `KEY_DELETE`;
    }
    else if (__x__.__constructor === "KEY_HELP") {
      return `KEY_HELP`;
    }
    else if (__x__.__constructor === "KEY_0") {
      return `KEY_0`;
    }
    else if (__x__.__constructor === "KEY_1") {
      return `KEY_1`;
    }
    else if (__x__.__constructor === "KEY_2") {
      return `KEY_2`;
    }
    else if (__x__.__constructor === "KEY_3") {
      return `KEY_3`;
    }
    else if (__x__.__constructor === "KEY_4") {
      return `KEY_4`;
    }
    else if (__x__.__constructor === "KEY_5") {
      return `KEY_5`;
    }
    else if (__x__.__constructor === "KEY_6") {
      return `KEY_6`;
    }
    else if (__x__.__constructor === "KEY_7") {
      return `KEY_7`;
    }
    else if (__x__.__constructor === "KEY_8") {
      return `KEY_8`;
    }
    else if (__x__.__constructor === "KEY_9") {
      return `KEY_9`;
    }
    else if (__x__.__constructor === "KEY_COLON") {
      return `KEY_COLON`;
    }
    else if (__x__.__constructor === "KEY_LEFT_CHEVRON") {
      return `KEY_LEFT_CHEVRON`;
    }
    else if (__x__.__constructor === "KEY_EQUAL") {
      return `KEY_EQUAL`;
    }
    else if (__x__.__constructor === "KEY_ESZETT") {
      return `KEY_ESZETT`;
    }
    else if (__x__.__constructor === "KEY_AT") {
      return `KEY_AT`;
    }
    else if (__x__.__constructor === "KEY_A") {
      return `KEY_A`;
    }
    else if (__x__.__constructor === "KEY_B") {
      return `KEY_B`;
    }
    else if (__x__.__constructor === "KEY_C") {
      return `KEY_C`;
    }
    else if (__x__.__constructor === "KEY_D") {
      return `KEY_D`;
    }
    else if (__x__.__constructor === "KEY_E") {
      return `KEY_E`;
    }
    else if (__x__.__constructor === "KEY_F") {
      return `KEY_F`;
    }
    else if (__x__.__constructor === "KEY_G") {
      return `KEY_G`;
    }
    else if (__x__.__constructor === "KEY_H") {
      return `KEY_H`;
    }
    else if (__x__.__constructor === "KEY_I") {
      return `KEY_I`;
    }
    else if (__x__.__constructor === "KEY_J") {
      return `KEY_J`;
    }
    else if (__x__.__constructor === "KEY_K") {
      return `KEY_K`;
    }
    else if (__x__.__constructor === "KEY_L") {
      return `KEY_L`;
    }
    else if (__x__.__constructor === "KEY_M") {
      return `KEY_M`;
    }
    else if (__x__.__constructor === "KEY_N") {
      return `KEY_N`;
    }
    else if (__x__.__constructor === "KEY_O") {
      return `KEY_O`;
    }
    else if (__x__.__constructor === "KEY_P") {
      return `KEY_P`;
    }
    else if (__x__.__constructor === "KEY_Q") {
      return `KEY_Q`;
    }
    else if (__x__.__constructor === "KEY_R") {
      return `KEY_R`;
    }
    else if (__x__.__constructor === "KEY_S") {
      return `KEY_S`;
    }
    else if (__x__.__constructor === "KEY_T") {
      return `KEY_T`;
    }
    else if (__x__.__constructor === "KEY_U") {
      return `KEY_U`;
    }
    else if (__x__.__constructor === "KEY_V") {
      return `KEY_V`;
    }
    else if (__x__.__constructor === "KEY_W") {
      return `KEY_W`;
    }
    else if (__x__.__constructor === "KEY_X") {
      return `KEY_X`;
    }
    else if (__x__.__constructor === "KEY_Y") {
      return `KEY_Y`;
    }
    else if (__x__.__constructor === "KEY_Z") {
      return `KEY_Z`;
    }
    else if (__x__.__constructor === "KEY_CMD_LEFT") {
      return `KEY_CMD_LEFT`;
    }
    else if (__x__.__constructor === "KEY_CMD_RIGHT") {
      return `KEY_CMD_RIGHT`;
    }
    else if (__x__.__constructor === "KEY_SLEEP") {
      return `KEY_SLEEP`;
    }
    else if (__x__.__constructor === "KEY_NUMPAD_0") {
      return `KEY_NUMPAD_0`;
    }
    else if (__x__.__constructor === "KEY_NUMPAD_1") {
      return `KEY_NUMPAD_1`;
    }
    else if (__x__.__constructor === "KEY_NUMPAD_2") {
      return `KEY_NUMPAD_2`;
    }
    else if (__x__.__constructor === "KEY_NUMPAD_3") {
      return `KEY_NUMPAD_3`;
    }
    else if (__x__.__constructor === "KEY_NUMPAD_4") {
      return `KEY_NUMPAD_4`;
    }
    else if (__x__.__constructor === "KEY_NUMPAD_5") {
      return `KEY_NUMPAD_5`;
    }
    else if (__x__.__constructor === "KEY_NUMPAD_6") {
      return `KEY_NUMPAD_6`;
    }
    else if (__x__.__constructor === "KEY_NUMPAD_7") {
      return `KEY_NUMPAD_7`;
    }
    else if (__x__.__constructor === "KEY_NUMPAD_8") {
      return `KEY_NUMPAD_8`;
    }
    else if (__x__.__constructor === "KEY_NUMPAD_9") {
      return `KEY_NUMPAD_9`;
    }
    else if (__x__.__constructor === "KEY_MULTIPLY") {
      return `KEY_MULTIPLY`;
    }
    else if (__x__.__constructor === "KEY_ADD") {
      return `KEY_ADD`;
    }
    else if (__x__.__constructor === "KEY_NUMPAD_PERIOD") {
      return `KEY_NUMPAD_PERIOD`;
    }
    else if (__x__.__constructor === "KEY_SUBSTRACT") {
      return `KEY_SUBSTRACT`;
    }
    else if (__x__.__constructor === "KEY_DECIMAL_POINT") {
      return `KEY_DECIMAL_POINT`;
    }
    else if (__x__.__constructor === "KEY_DIVIDE") {
      return `KEY_DIVIDE`;
    }
    else if (__x__.__constructor === "KEY_F1") {
      return `KEY_F1`;
    }
    else if (__x__.__constructor === "KEY_F2") {
      return `KEY_F2`;
    }
    else if (__x__.__constructor === "KEY_F3") {
      return `KEY_F3`;
    }
    else if (__x__.__constructor === "KEY_F4") {
      return `KEY_F4`;
    }
    else if (__x__.__constructor === "KEY_F5") {
      return `KEY_F5`;
    }
    else if (__x__.__constructor === "KEY_F6") {
      return `KEY_F6`;
    }
    else if (__x__.__constructor === "KEY_F7") {
      return `KEY_F7`;
    }
    else if (__x__.__constructor === "KEY_F8") {
      return `KEY_F8`;
    }
    else if (__x__.__constructor === "KEY_F9") {
      return `KEY_F9`;
    }
    else if (__x__.__constructor === "KEY_F10") {
      return `KEY_F10`;
    }
    else if (__x__.__constructor === "KEY_F11") {
      return `KEY_F11`;
    }
    else if (__x__.__constructor === "KEY_F12") {
      return `KEY_F12`;
    }
    else if (__x__.__constructor === "KEY_F13") {
      return `KEY_F13`;
    }
    else if (__x__.__constructor === "KEY_F14") {
      return `KEY_F14`;
    }
    else if (__x__.__constructor === "KEY_F15") {
      return `KEY_F15`;
    }
    else if (__x__.__constructor === "KEY_F16") {
      return `KEY_F16`;
    }
    else if (__x__.__constructor === "KEY_F17") {
      return `KEY_F17`;
    }
    else if (__x__.__constructor === "KEY_F18") {
      return `KEY_F18`;
    }
    else if (__x__.__constructor === "KEY_F19") {
      return `KEY_F19`;
    }
    else if (__x__.__constructor === "KEY_F20") {
      return `KEY_F20`;
    }
    else if (__x__.__constructor === "KEY_F21") {
      return `KEY_F21`;
    }
    else if (__x__.__constructor === "KEY_F22") {
      return `KEY_F22`;
    }
    else if (__x__.__constructor === "KEY_F23") {
      return `KEY_F23`;
    }
    else if (__x__.__constructor === "KEY_F24") {
      return `KEY_F24`;
    }
    else if (__x__.__constructor === "KEY_F25") {
      return `KEY_F25`;
    }
    else if (__x__.__constructor === "KEY_F26") {
      return `KEY_F26`;
    }
    else if (__x__.__constructor === "KEY_F27") {
      return `KEY_F27`;
    }
    else if (__x__.__constructor === "KEY_F28") {
      return `KEY_F28`;
    }
    else if (__x__.__constructor === "KEY_F29") {
      return `KEY_F29`;
    }
    else if (__x__.__constructor === "KEY_F30") {
      return `KEY_F30`;
    }
    else if (__x__.__constructor === "KEY_F31") {
      return `KEY_F31`;
    }
    else if (__x__.__constructor === "KEY_F32") {
      return `KEY_F32`;
    }
    else if (__x__.__constructor === "KEY_NUM_LOCK") {
      return `KEY_NUM_LOCK`;
    }
    else if (__x__.__constructor === "KEY_SCROLL_LOCK") {
      return `KEY_SCROLL_LOCK`;
    }
    else if (__x__.__constructor === "KEY_AIRPLANE_MODE") {
      return `KEY_AIRPLANE_MODE`;
    }
    else if (__x__.__constructor === "KEY_CIRCONFLEX") {
      return `KEY_CIRCONFLEX`;
    }
    else if (__x__.__constructor === "KEY_EXCLAMATION_MARK") {
      return `KEY_EXCLAMATION_MARK`;
    }
    else if (__x__.__constructor === "KEY_ARABIC_SEMI_COLON") {
      return `KEY_ARABIC_SEMI_COLON`;
    }
    else if (__x__.__constructor === "KEY_NUMBER_SIGN") {
      return `KEY_NUMBER_SIGN`;
    }
    else if (__x__.__constructor === "KEY_DOLLAR") {
      return `KEY_DOLLAR`;
    }
    else if (__x__.__constructor === "KEY_U_GRAVE_ACCENT") {
      return `KEY_U_GRAVE_ACCENT`;
    }
    else if (__x__.__constructor === "KEY_PAGE_BACKWARD") {
      return `KEY_PAGE_BACKWARD`;
    }
    else if (__x__.__constructor === "KEY_PAGE_FORWARD") {
      return `KEY_PAGE_FORWARD`;
    }
    else if (__x__.__constructor === "KEY_REFRESH") {
      return `KEY_REFRESH`;
    }
    else if (__x__.__constructor === "KEY_RIGHT_PAREN") {
      return `KEY_RIGHT_PAREN`;
    }
    else if (__x__.__constructor === "KEY_ASTERISK") {
      return `KEY_ASTERISK`;
    }
    else if (__x__.__constructor === "KEY_TILDE") {
      return `KEY_TILDE`;
    }
    else if (__x__.__constructor === "KEY_MUTE") {
      return `KEY_MUTE`;
    }
    else if (__x__.__constructor === "KEY_NEXT") {
      return `KEY_NEXT`;
    }
    else if (__x__.__constructor === "KEY_PREVIOUS") {
      return `KEY_PREVIOUS`;
    }
    else if (__x__.__constructor === "KEY_STOP") {
      return `KEY_STOP`;
    }
    else if (__x__.__constructor === "KEY_PLAY_PAUSE") {
      return `KEY_PLAY_PAUSE`;
    }
    else if (__x__.__constructor === "KEY_EMAIL") {
      return `KEY_EMAIL`;
    }
    else if (__x__.__constructor === "KEY_MUTE_UNMUTE") {
      return `KEY_MUTE_UNMUTE`;
    }
    else if (__x__.__constructor === "KEY_DECREASE_VOLUME") {
      return `KEY_DECREASE_VOLUME`;
    }
    else if (__x__.__constructor === "KEY_INCREASE_VOLUME") {
      return `KEY_INCREASE_VOLUME`;
    }
    else if (__x__.__constructor === "KEY_SEMI_COLON") {
      return `KEY_SEMI_COLON`;
    }
    else if (__x__.__constructor === "KEY_COMMA") {
      return `KEY_COMMA`;
    }
    else if (__x__.__constructor === "KEY_DASH") {
      return `KEY_DASH`;
    }
    else if (__x__.__constructor === "KEY_PERIOD") {
      return `KEY_PERIOD`;
    }
    else if (__x__.__constructor === "KEY_FORWARD_SLASH") {
      return `KEY_FORWARD_SLASH`;
    }
    else if (__x__.__constructor === "KEY_GRAVE_ACCENT") {
      return `KEY_GRAVE_ACCENT`;
    }
    else if (__x__.__constructor === "KEY_QUESTION_MARK") {
      return `KEY_QUESTION_MARK`;
    }
    else if (__x__.__constructor === "KEY_BRACKET_LEFT") {
      return `KEY_BRACKET_LEFT`;
    }
    else if (__x__.__constructor === "KEY_BACK_SLASH") {
      return `KEY_BACK_SLASH`;
    }
    else if (__x__.__constructor === "KEY_BRACKET_RIGHT") {
      return `KEY_BRACKET_RIGHT`;
    }
    else if (__x__.__constructor === "KEY_SINGLE_QUOTE") {
      return `KEY_SINGLE_QUOTE`;
    }
    else if (__x__.__constructor === "KEY_BACK_TICK") {
      return `KEY_BACK_TICK`;
    }
    else if (__x__.__constructor === "KEY_CMD") {
      return `KEY_CMD`;
    }
    else if (__x__.__constructor === "KEY_ALTGR") {
      return `KEY_ALTGR`;
    }
    else if (__x__.__constructor === "KEY_LEFT_BACK_SLASH") {
      return `KEY_LEFT_BACK_SLASH`;
    }
    else if (__x__.__constructor === "KEY_GNOME_COMPOSE") {
      return `KEY_GNOME_COMPOSE`;
    }
    else if (__x__.__constructor === "KEY_C_CEDILLA") {
      return `KEY_C_CEDILLA`;
    }
    else if (__x__.__constructor === "KEY_XF86_FORWARD") {
      return `KEY_XF86_FORWARD`;
    }
    else if (__x__.__constructor === "KEY_XF86_BACKWARD") {
      return `KEY_XF86_BACKWARD`;
    }
    else if (__x__.__constructor === "KEY_ALPHA_NUMERIC") {
      return `KEY_ALPHA_NUMERIC`;
    }
    else if (__x__.__constructor === "KEY_HIRAGANA_KATAKANA") {
      return `KEY_HIRAGANA_KATAKANA`;
    }
    else if (__x__.__constructor === "KEY_HALF_WIDTH_FULL_WIDTH") {
      return `KEY_HALF_WIDTH_FULL_WIDTH`;
    }
    else if (__x__.__constructor === "KEY_KANJI") {
      return `KEY_KANJI`;
    }
    else if (__x__.__constructor === "KEY_UNLOCK_TRACK_PAD") {
      return `KEY_UNLOCK_TRACK_PAD`;
    }
    else if (__x__.__constructor === "KEY_TOGGLE_TOUCH_PAD") {
      return `KEY_TOGGLE_TOUCH_PAD`;
    }
    else {
      return `Unknown`;
    }
  })(__$a__));
  let KEY_CODE_MAPPINGS = fromList$1(Comparable.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d)(({ v: ([3, KEY_BREAK]), n: { v: ([8, KEY_BACKSPACE]), n: { v: ([9, KEY_TAB]), n: { v: ([12, KEY_CLEAR]), n: { v: ([13, KEY_ENTER]), n: { v: ([16, KEY_SHIFT]), n: { v: ([17, KEY_CTRL]), n: { v: ([18, KEY_ALT]), n: { v: ([19, KEY_PAUSE]), n: { v: ([20, KEY_CAPS_LOCK]), n: { v: ([21, KEY_HANGUL]), n: { v: ([25, KEY_HANJA]), n: { v: ([27, KEY_ESCAPE]), n: { v: ([28, KEY_CONVERSION]), n: { v: ([29, KEY_NON_CONVERSION]), n: { v: ([32, KEY_SPACE]), n: { v: ([33, KEY_PAGE_UP]), n: { v: ([34, KEY_PAGE_DOWN]), n: { v: ([35, KEY_END]), n: { v: ([36, KEY_HOME]), n: { v: ([37, KEY_LEFT_ARROW]), n: { v: ([38, KEY_UP_ARROW]), n: { v: ([39, KEY_RIGHT_ARROW]), n: { v: ([40, KEY_DOWN_ARROW]), n: { v: ([41, KEY_SELECT]), n: { v: ([42, KEY_PRINT]), n: { v: ([43, KEY_EXECUTE]), n: { v: ([44, KEY_PRINT_SCREEN]), n: { v: ([45, KEY_INSERT]), n: { v: ([46, KEY_DELETE]), n: { v: ([47, KEY_HELP]), n: { v: ([48, KEY_0]), n: { v: ([49, KEY_1]), n: { v: ([50, KEY_2]), n: { v: ([51, KEY_3]), n: { v: ([52, KEY_4]), n: { v: ([53, KEY_5]), n: { v: ([54, KEY_6]), n: { v: ([55, KEY_7]), n: { v: ([56, KEY_8]), n: { v: ([57, KEY_9]), n: { v: ([58, KEY_COLON]), n: { v: ([59, KEY_EQUAL]), n: { v: ([60, KEY_LEFT_CHEVRON]), n: { v: ([61, KEY_EQUAL]), n: { v: ([63, KEY_ESZETT]), n: { v: ([64, KEY_AT]), n: { v: ([65, KEY_A]), n: { v: ([66, KEY_B]), n: { v: ([67, KEY_C]), n: { v: ([68, KEY_D]), n: { v: ([69, KEY_E]), n: { v: ([70, KEY_F]), n: { v: ([71, KEY_G]), n: { v: ([72, KEY_H]), n: { v: ([73, KEY_I]), n: { v: ([74, KEY_J]), n: { v: ([75, KEY_K]), n: { v: ([76, KEY_L]), n: { v: ([77, KEY_M]), n: { v: ([78, KEY_N]), n: { v: ([79, KEY_O]), n: { v: ([80, KEY_P]), n: { v: ([81, KEY_Q]), n: { v: ([82, KEY_R]), n: { v: ([83, KEY_S]), n: { v: ([84, KEY_T]), n: { v: ([85, KEY_U]), n: { v: ([86, KEY_V]), n: { v: ([87, KEY_W]), n: { v: ([88, KEY_X]), n: { v: ([89, KEY_Y]), n: { v: ([90, KEY_Z]), n: { v: ([91, KEY_CMD_LEFT]), n: { v: ([92, KEY_CMD_RIGHT]), n: { v: ([93, KEY_CMD_RIGHT]), n: { v: ([95, KEY_SLEEP]), n: { v: ([96, KEY_NUMPAD_0]), n: { v: ([97, KEY_NUMPAD_1]), n: { v: ([98, KEY_NUMPAD_2]), n: { v: ([99, KEY_NUMPAD_3]), n: { v: ([100, KEY_NUMPAD_4]), n: { v: ([101, KEY_NUMPAD_5]), n: { v: ([102, KEY_NUMPAD_6]), n: { v: ([103, KEY_NUMPAD_7]), n: { v: ([104, KEY_NUMPAD_8]), n: { v: ([105, KEY_NUMPAD_9]), n: { v: ([106, KEY_MULTIPLY]), n: { v: ([107, KEY_ADD]), n: { v: ([108, KEY_NUMPAD_PERIOD]), n: { v: ([109, KEY_SUBSTRACT]), n: { v: ([110, KEY_DECIMAL_POINT]), n: { v: ([111, KEY_DIVIDE]), n: { v: ([112, KEY_F1]), n: { v: ([113, KEY_F2]), n: { v: ([114, KEY_F3]), n: { v: ([115, KEY_F4]), n: { v: ([116, KEY_F5]), n: { v: ([117, KEY_F6]), n: { v: ([118, KEY_F7]), n: { v: ([119, KEY_F8]), n: { v: ([120, KEY_F9]), n: { v: ([121, KEY_F10]), n: { v: ([122, KEY_F11]), n: { v: ([123, KEY_F12]), n: { v: ([124, KEY_F13]), n: { v: ([125, KEY_F14]), n: { v: ([126, KEY_F15]), n: { v: ([127, KEY_F16]), n: { v: ([128, KEY_F17]), n: { v: ([129, KEY_F18]), n: { v: ([130, KEY_F19]), n: { v: ([131, KEY_F20]), n: { v: ([132, KEY_F21]), n: { v: ([133, KEY_F22]), n: { v: ([134, KEY_F23]), n: { v: ([135, KEY_F24]), n: { v: ([136, KEY_F25]), n: { v: ([137, KEY_F26]), n: { v: ([138, KEY_F27]), n: { v: ([139, KEY_F28]), n: { v: ([140, KEY_F29]), n: { v: ([141, KEY_F30]), n: { v: ([142, KEY_F31]), n: { v: ([143, KEY_F32]), n: { v: ([144, KEY_NUM_LOCK]), n: { v: ([145, KEY_SCROLL_LOCK]), n: { v: ([151, KEY_AIRPLANE_MODE]), n: { v: ([160, KEY_CIRCONFLEX]), n: { v: ([161, KEY_EXCLAMATION_MARK]), n: { v: ([162, KEY_ARABIC_SEMI_COLON]), n: { v: ([163, KEY_NUMBER_SIGN]), n: { v: ([164, KEY_DOLLAR]), n: { v: ([165, KEY_U_GRAVE_ACCENT]), n: { v: ([166, KEY_PAGE_BACKWARD]), n: { v: ([167, KEY_PAGE_FORWARD]), n: { v: ([168, KEY_REFRESH]), n: { v: ([169, KEY_RIGHT_PAREN]), n: { v: ([170, KEY_ASTERISK]), n: { v: ([171, KEY_TILDE]), n: { v: ([172, KEY_HOME]), n: { v: ([173, KEY_MUTE]), n: { v: ([174, KEY_DECREASE_VOLUME]), n: { v: ([175, KEY_INCREASE_VOLUME]), n: { v: ([176, KEY_NEXT]), n: { v: ([177, KEY_PREVIOUS]), n: { v: ([178, KEY_STOP]), n: { v: ([179, KEY_PLAY_PAUSE]), n: { v: ([180, KEY_EMAIL]), n: { v: ([181, KEY_MUTE_UNMUTE]), n: { v: ([182, KEY_DECREASE_VOLUME]), n: { v: ([183, KEY_INCREASE_VOLUME]), n: { v: ([186, KEY_SEMI_COLON]), n: { v: ([187, KEY_EQUAL]), n: { v: ([188, KEY_COMMA]), n: { v: ([189, KEY_DASH]), n: { v: ([190, KEY_PERIOD]), n: { v: ([191, KEY_FORWARD_SLASH]), n: { v: ([192, KEY_GRAVE_ACCENT]), n: { v: ([193, KEY_QUESTION_MARK]), n: { v: ([194, KEY_NUMPAD_PERIOD]), n: { v: ([219, KEY_BRACKET_LEFT]), n: { v: ([220, KEY_BACK_SLASH]), n: { v: ([221, KEY_BRACKET_RIGHT]), n: { v: ([222, KEY_SINGLE_QUOTE]), n: { v: ([223, KEY_BACK_TICK]), n: { v: ([224, KEY_CMD]), n: { v: ([225, KEY_ALTGR]), n: { v: ([226, KEY_LEFT_BACK_SLASH]), n: { v: ([230, KEY_GNOME_COMPOSE]), n: { v: ([231, KEY_C_CEDILLA]), n: { v: ([233, KEY_XF86_FORWARD]), n: { v: ([234, KEY_XF86_BACKWARD]), n: { v: ([235, KEY_NON_CONVERSION]), n: { v: ([240, KEY_ALPHA_NUMERIC]), n: { v: ([242, KEY_HIRAGANA_KATAKANA]), n: { v: ([243, KEY_HALF_WIDTH_FULL_WIDTH]), n: { v: ([244, KEY_KANJI]), n: { v: ([251, KEY_UNLOCK_TRACK_PAD]), n: { v: ([255, KEY_TOGGLE_TOUCH_PAD]), n: null } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } }));
  let getKeyFromCode = (keyCode => fromMaybe(KEY_ANY)(get(Comparable.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d)(keyCode)(KEY_CODE_MAPPINGS)));

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadUI/src/Event.mad

  let AbstractEvent = (a => ({ __constructor: "AbstractEvent", __args: [ a ] }));
  let MouseEvent = (a => ({ __constructor: "MouseEvent", __args: [ a ] }));
  let InputEvent = (a => ({ __constructor: "InputEvent", __args: [ a ] }));
  let KeyboardEvent = (a => ({ __constructor: "KeyboardEvent", __args: [ a ] }));
  let PopStateEvent = (a => ({ __constructor: "PopStateEvent", __args: [ a ] }));
  Inspect['Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_defaultPreventedf_150730a644794cadc5c69c04e30ae3c86_eventTypef_250730a644794cadc5c69c04e30ae3c86_preventDefaultf_350730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_450730a644794cadc5c69c04e30ae3c86_stopPropagationf_550730a644794cadc5c69c04e30ae3c86_timeStampf_650730a644794cadc5c69c04e30ae3c86'] = {};
  Inspect['Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_defaultPreventedf_150730a644794cadc5c69c04e30ae3c86_eventTypef_250730a644794cadc5c69c04e30ae3c86_preventDefaultf_350730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_450730a644794cadc5c69c04e30ae3c86_stopPropagationf_550730a644794cadc5c69c04e30ae3c86_timeStampf_650730a644794cadc5c69c04e30ae3c86']['inspect'] = () => (Inspect_l869) => (Inspect_e862) => (Inspect_x855) => (Inspect_q848) => (Inspect_j841) => (Inspect_c834) => (Inspect_v827) => (__$a__ => `{ ` + `bubbles: ` + Inspect_v827.inspect()(__$a__.bubbles) + `, ` + `defaultPrevented: ` + Inspect_c834.inspect()(__$a__.defaultPrevented) + `, ` + `eventType: ` + Inspect_j841.inspect()(__$a__.eventType) + `, ` + `preventDefault: ` + Inspect_q848.inspect()(__$a__.preventDefault) + `, ` + `stopImmediatePropagation: ` + Inspect_x855.inspect()(__$a__.stopImmediatePropagation) + `, ` + `stopPropagation: ` + Inspect_e862.inspect()(__$a__.stopPropagation) + `, ` + `timeStamp: ` + Inspect_l869.inspect()(__$a__.timeStamp) + ` }`);
  Inspect['Record_valuef_050730a644794cadc5c69c04e30ae3c86'] = {};
  Inspect['Record_valuef_050730a644794cadc5c69c04e30ae3c86']['inspect'] = () => (Inspect_b885) => (__$a__ => `{ ` + `value: ` + Inspect_b885.inspect()(__$a__.value) + ` }`);
  Inspect['Event_50730a644794cadc5c69c04e30ae3c86'] = {};
  Inspect['Event_50730a644794cadc5c69c04e30ae3c86']['inspect'] = () => (__$a__ => ((__x__) => {
    if (__x__.__constructor === "AbstractEvent" && true) {
      let a0 = __x__.__args[0];
      return `AbstractEvent(` + Inspect.Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_defaultPreventedf_150730a644794cadc5c69c04e30ae3c86_eventTypef_250730a644794cadc5c69c04e30ae3c86_preventDefaultf_350730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_450730a644794cadc5c69c04e30ae3c86_stopPropagationf_550730a644794cadc5c69c04e30ae3c86_timeStampf_650730a644794cadc5c69c04e30ae3c86.inspect()(Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(a0) + `)`;
    }
    else if (__x__.__constructor === "MouseEvent" && true) {
      let a0 = __x__.__args[0];
      return `MouseEvent(` + Inspect.Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_defaultPreventedf_150730a644794cadc5c69c04e30ae3c86_eventTypef_250730a644794cadc5c69c04e30ae3c86_preventDefaultf_350730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_450730a644794cadc5c69c04e30ae3c86_stopPropagationf_550730a644794cadc5c69c04e30ae3c86_timeStampf_650730a644794cadc5c69c04e30ae3c86.inspect()(Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(a0) + `)`;
    }
    else if (__x__.__constructor === "InputEvent" && true) {
      let a0 = __x__.__args[0];
      return `InputEvent(` + Inspect.Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_dataf_150730a644794cadc5c69c04e30ae3c86_defaultPreventedf_250730a644794cadc5c69c04e30ae3c86_eventTypef_350730a644794cadc5c69c04e30ae3c86_inputTypef_450730a644794cadc5c69c04e30ae3c86_preventDefaultf_550730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_650730a644794cadc5c69c04e30ae3c86_stopPropagationf_750730a644794cadc5c69c04e30ae3c86_targetf_850730a644794cadc5c69c04e30ae3c86_timeStampf_950730a644794cadc5c69c04e30ae3c86.inspect()(Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d)(__apMtdDicts__(Inspect.Record_valuef_050730a644794cadc5c69c04e30ae3c86, [Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d]))(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(a0) + `)`;
    }
    else if (__x__.__constructor === "KeyboardEvent" && true) {
      let a0 = __x__.__args[0];
      return `KeyboardEvent(` + Inspect.Record_altKeyf_050730a644794cadc5c69c04e30ae3c86_bubblesf_150730a644794cadc5c69c04e30ae3c86_ctrlKeyf_250730a644794cadc5c69c04e30ae3c86_defaultPreventedf_350730a644794cadc5c69c04e30ae3c86_eventTypef_450730a644794cadc5c69c04e30ae3c86_keyf_550730a644794cadc5c69c04e30ae3c86_preventDefaultf_650730a644794cadc5c69c04e30ae3c86_shiftKeyf_750730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_850730a644794cadc5c69c04e30ae3c86_stopPropagationf_950730a644794cadc5c69c04e30ae3c86_timeStampf_1050730a644794cadc5c69c04e30ae3c86.inspect()(Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.a_arr_b)(Inspect.Key_29c16bbb90cf3a28d74dcca5983dc9c2)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(a0) + `)`;
    }
    else if (__x__.__constructor === "PopStateEvent" && true) {
      let a0 = __x__.__args[0];
      return `PopStateEvent(` + Inspect.Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_defaultPreventedf_150730a644794cadc5c69c04e30ae3c86_eventTypef_250730a644794cadc5c69c04e30ae3c86_preventDefaultf_350730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_450730a644794cadc5c69c04e30ae3c86_stopPropagationf_550730a644794cadc5c69c04e30ae3c86_timeStampf_650730a644794cadc5c69c04e30ae3c86_urlf_750730a644794cadc5c69c04e30ae3c86.inspect()(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(a0) + `)`;
    }
    else {
      return `Unknown`;
    }
  })(__$a__));
  Inspect['Record_altKeyf_050730a644794cadc5c69c04e30ae3c86_bubblesf_150730a644794cadc5c69c04e30ae3c86_ctrlKeyf_250730a644794cadc5c69c04e30ae3c86_defaultPreventedf_350730a644794cadc5c69c04e30ae3c86_eventTypef_450730a644794cadc5c69c04e30ae3c86_keyf_550730a644794cadc5c69c04e30ae3c86_preventDefaultf_650730a644794cadc5c69c04e30ae3c86_shiftKeyf_750730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_850730a644794cadc5c69c04e30ae3c86_stopPropagationf_950730a644794cadc5c69c04e30ae3c86_timeStampf_1050730a644794cadc5c69c04e30ae3c86'] = {};
  Inspect['Record_altKeyf_050730a644794cadc5c69c04e30ae3c86_bubblesf_150730a644794cadc5c69c04e30ae3c86_ctrlKeyf_250730a644794cadc5c69c04e30ae3c86_defaultPreventedf_350730a644794cadc5c69c04e30ae3c86_eventTypef_450730a644794cadc5c69c04e30ae3c86_keyf_550730a644794cadc5c69c04e30ae3c86_preventDefaultf_650730a644794cadc5c69c04e30ae3c86_shiftKeyf_750730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_850730a644794cadc5c69c04e30ae3c86_stopPropagationf_950730a644794cadc5c69c04e30ae3c86_timeStampf_1050730a644794cadc5c69c04e30ae3c86']['inspect'] = () => (Inspect_p1003) => (Inspect_i996) => (Inspect_b989) => (Inspect_u982) => (Inspect_n975) => (Inspect_g968) => (Inspect_z961) => (Inspect_s954) => (Inspect_l947) => (Inspect_e940) => (Inspect_x933) => (__$a__ => `{ ` + `altKey: ` + Inspect_x933.inspect()(__$a__.altKey) + `, ` + `bubbles: ` + Inspect_e940.inspect()(__$a__.bubbles) + `, ` + `ctrlKey: ` + Inspect_l947.inspect()(__$a__.ctrlKey) + `, ` + `defaultPrevented: ` + Inspect_s954.inspect()(__$a__.defaultPrevented) + `, ` + `eventType: ` + Inspect_z961.inspect()(__$a__.eventType) + `, ` + `key: ` + Inspect_g968.inspect()(__$a__.key) + `, ` + `preventDefault: ` + Inspect_n975.inspect()(__$a__.preventDefault) + `, ` + `shiftKey: ` + Inspect_u982.inspect()(__$a__.shiftKey) + `, ` + `stopImmediatePropagation: ` + Inspect_b989.inspect()(__$a__.stopImmediatePropagation) + `, ` + `stopPropagation: ` + Inspect_i996.inspect()(__$a__.stopPropagation) + `, ` + `timeStamp: ` + Inspect_p1003.inspect()(__$a__.timeStamp) + ` }`);
  Inspect['Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_dataf_150730a644794cadc5c69c04e30ae3c86_defaultPreventedf_250730a644794cadc5c69c04e30ae3c86_eventTypef_350730a644794cadc5c69c04e30ae3c86_inputTypef_450730a644794cadc5c69c04e30ae3c86_preventDefaultf_550730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_650730a644794cadc5c69c04e30ae3c86_stopPropagationf_750730a644794cadc5c69c04e30ae3c86_targetf_850730a644794cadc5c69c04e30ae3c86_timeStampf_950730a644794cadc5c69c04e30ae3c86'] = {};
  Inspect['Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_dataf_150730a644794cadc5c69c04e30ae3c86_defaultPreventedf_250730a644794cadc5c69c04e30ae3c86_eventTypef_350730a644794cadc5c69c04e30ae3c86_inputTypef_450730a644794cadc5c69c04e30ae3c86_preventDefaultf_550730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_650730a644794cadc5c69c04e30ae3c86_stopPropagationf_750730a644794cadc5c69c04e30ae3c86_targetf_850730a644794cadc5c69c04e30ae3c86_timeStampf_950730a644794cadc5c69c04e30ae3c86']['inspect'] = () => (Inspect_u1086) => (Inspect_n1079) => (Inspect_g1072) => (Inspect_z1065) => (Inspect_s1058) => (Inspect_l1051) => (Inspect_e1044) => (Inspect_x1037) => (Inspect_q1030) => (Inspect_j1023) => (__$a__ => `{ ` + `bubbles: ` + Inspect_j1023.inspect()(__$a__.bubbles) + `, ` + `data: ` + Inspect_q1030.inspect()(__$a__.data) + `, ` + `defaultPrevented: ` + Inspect_x1037.inspect()(__$a__.defaultPrevented) + `, ` + `eventType: ` + Inspect_e1044.inspect()(__$a__.eventType) + `, ` + `inputType: ` + Inspect_l1051.inspect()(__$a__.inputType) + `, ` + `preventDefault: ` + Inspect_s1058.inspect()(__$a__.preventDefault) + `, ` + `stopImmediatePropagation: ` + Inspect_z1065.inspect()(__$a__.stopImmediatePropagation) + `, ` + `stopPropagation: ` + Inspect_g1072.inspect()(__$a__.stopPropagation) + `, ` + `target: ` + Inspect_n1079.inspect()(__$a__.target) + `, ` + `timeStamp: ` + Inspect_u1086.inspect()(__$a__.timeStamp) + ` }`);
  Inspect['Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_defaultPreventedf_150730a644794cadc5c69c04e30ae3c86_eventTypef_250730a644794cadc5c69c04e30ae3c86_preventDefaultf_350730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_450730a644794cadc5c69c04e30ae3c86_stopPropagationf_550730a644794cadc5c69c04e30ae3c86_timeStampf_650730a644794cadc5c69c04e30ae3c86_urlf_750730a644794cadc5c69c04e30ae3c86'] = {};
  Inspect['Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_defaultPreventedf_150730a644794cadc5c69c04e30ae3c86_eventTypef_250730a644794cadc5c69c04e30ae3c86_preventDefaultf_350730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_450730a644794cadc5c69c04e30ae3c86_stopPropagationf_550730a644794cadc5c69c04e30ae3c86_timeStampf_650730a644794cadc5c69c04e30ae3c86_urlf_750730a644794cadc5c69c04e30ae3c86']['inspect'] = () => (Inspect_k1154) => (Inspect_d1147) => (Inspect_w1140) => (Inspect_p1133) => (Inspect_i1126) => (Inspect_b1119) => (Inspect_u1112) => (Inspect_n1105) => (__$a__ => `{ ` + `bubbles: ` + Inspect_n1105.inspect()(__$a__.bubbles) + `, ` + `defaultPrevented: ` + Inspect_u1112.inspect()(__$a__.defaultPrevented) + `, ` + `eventType: ` + Inspect_b1119.inspect()(__$a__.eventType) + `, ` + `preventDefault: ` + Inspect_i1126.inspect()(__$a__.preventDefault) + `, ` + `stopImmediatePropagation: ` + Inspect_p1133.inspect()(__$a__.stopImmediatePropagation) + `, ` + `stopPropagation: ` + Inspect_w1140.inspect()(__$a__.stopPropagation) + `, ` + `timeStamp: ` + Inspect_d1147.inspect()(__$a__.timeStamp) + `, ` + `url: ` + Inspect_k1154.inspect()(__$a__.url) + ` }`);
  let buildKeyboardEvent = (e => {
      let k = getKeyFromCode(e.keyCode);
      return KeyboardEvent(({ bubbles: e.bubbles, defaultPrevented: e.defaultPrevented, preventDefault: e.preventDefault, stopImmediatePropagation: e.stopImmediatePropagation, stopPropagation: e.stopPropagation, timeStamp: e.timeStamp, eventType: e.eventType, key: k, altKey: e.altKey, ctrlKey: e.ctrlKey, shiftKey: e.shiftKey }));
  });
  let buildInputEvent = (e => InputEvent(({ bubbles: e.bubbles, defaultPrevented: e.defaultPrevented, preventDefault: e.preventDefault, stopImmediatePropagation: e.stopImmediatePropagation, stopPropagation: e.stopPropagation, timeStamp: e.timeStamp, eventType: e.eventType, target: e.target, data: e.data, inputType: e.inputType })));
  let buildMouseEvent = (e => MouseEvent(({ bubbles: e.bubbles, defaultPrevented: e.defaultPrevented, preventDefault: e.preventDefault, stopImmediatePropagation: e.stopImmediatePropagation, stopPropagation: e.stopPropagation, timeStamp: e.timeStamp, eventType: e.eventType })));
  let buildAbstractEvent = (e => AbstractEvent(({ bubbles: e.bubbles, defaultPrevented: e.defaultPrevented, preventDefault: e.preventDefault, stopImmediatePropagation: e.stopImmediatePropagation, stopPropagation: e.stopPropagation, timeStamp: e.timeStamp, eventType: e.eventType })));
  let buildPopStateEvent = (e => PopStateEvent(({ url:  document.location.hash.substr(1) || "/" , bubbles: e.bubbles, defaultPrevented: e.defaultPrevented, preventDefault: e.preventDefault, stopImmediatePropagation: e.stopImmediatePropagation, stopPropagation: e.stopPropagation, timeStamp: e.timeStamp, eventType: e.eventType })));
  let EventConstructors =  Object.freeze({
    abort: buildAbstractEvent,
    afterprint: buildAbstractEvent,
    beforeprint: buildAbstractEvent,
    beforeunload: buildAbstractEvent,
    blur: buildAbstractEvent,
    canplay: buildAbstractEvent,
    canplaythrough: buildAbstractEvent,
    change: buildAbstractEvent,
    click: buildMouseEvent,
    contextmenu: buildAbstractEvent,
    copy: buildAbstractEvent,
    cuechange: buildAbstractEvent,
    cut: buildAbstractEvent,
    dblclick: buildMouseEvent,
    drag: buildAbstractEvent,
    dragend: buildAbstractEvent,
    dragenter: buildAbstractEvent,
    dragleave: buildAbstractEvent,
    dragover: buildAbstractEvent,
    dragstart: buildAbstractEvent,
    drop: buildAbstractEvent,
    durationchange: buildAbstractEvent,
    emptied: buildAbstractEvent,
    ended: buildAbstractEvent,
    error: buildAbstractEvent,
    focus: buildAbstractEvent,
    input: buildInputEvent,
    invalid: buildAbstractEvent,
    keydown: buildKeyboardEvent,
    keypress: buildKeyboardEvent,
    keyup: buildKeyboardEvent,
    load: buildAbstractEvent,
    loadeddata: buildAbstractEvent,
    loadedmetadata: buildAbstractEvent,
    loadstart: buildAbstractEvent,
    mousedown: buildMouseEvent,
    mouseenter: buildMouseEvent,
    mouseleave: buildMouseEvent,
    mousemove: buildMouseEvent,
    mouseout: buildMouseEvent,
    mouseover: buildMouseEvent,
    mouseup: buildMouseEvent,
    mousewheel: buildMouseEvent,
    offline: buildAbstractEvent,
    online: buildAbstractEvent,
    pagehide: buildAbstractEvent,
    pageshow: buildAbstractEvent,
    paste: buildAbstractEvent,
    pause: buildAbstractEvent,
    play: buildAbstractEvent,
    playing: buildAbstractEvent,
    popstate: buildPopStateEvent,
    progress: buildAbstractEvent,
    ratechange: buildAbstractEvent,
    reset: buildAbstractEvent,
    resize: buildAbstractEvent,
    scroll: buildAbstractEvent,
    search: buildAbstractEvent,
    seeked: buildAbstractEvent,
    seeking: buildAbstractEvent,
    select: buildAbstractEvent,
    stalled: buildAbstractEvent,
    storage: buildAbstractEvent,
    submit: buildAbstractEvent,
    suspend: buildAbstractEvent,
    timeupdate: buildAbstractEvent,
    toggle: buildAbstractEvent,
    transitioncancel: buildAbstractEvent,
    transitionend: buildAbstractEvent,
    transitionrun: buildAbstractEvent,
    transitionstart: buildAbstractEvent,
    unload: buildAbstractEvent,
    volumechange: buildAbstractEvent,
    waiting: buildAbstractEvent,
    wheel: buildAbstractEvent,
  })
  ;

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadUI/src/Action.mad

  let GlobalAction = (a => b => ({ __constructor: "GlobalAction", __args: [ a, b ] }));
  Inspect['GlobalAction_8fa092d0d5414d48db6f0f118b65d141'] = {};
  Inspect['GlobalAction_8fa092d0d5414d48db6f0f118b65d141']['inspect'] = () => (Inspect_g110) => (__$a__ => ((__x__) => {
    if (__x__.__constructor === "GlobalAction" && true && true) {
      let a0 = __x__.__args[0];
      let a1 = __x__.__args[1];
      return `GlobalAction(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `, ` + Inspect.a_arr_b.inspect()(a1) + `)`;
    }
    else {
      return `Unknown`;
    }
  })(__$a__));
  let syncAction = (stateUpdate => _ => event => ({ v: Monad.Wish_48091bbb4c188d584814a4a3f8207f71.of()((state => stateUpdate(state)(event))), n: null }));
  let onUrlChanged = GlobalAction(`popstate`);

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadUI/src/Config.mad

  Inspect['Record_globalEventHandlersf_0e264b6be2a515e01e171e67e9c05fdef_subscriptionsf_1e264b6be2a515e01e171e67e9c05fdef'] = {};
  Inspect['Record_globalEventHandlersf_0e264b6be2a515e01e171e67e9c05fdef_subscriptionsf_1e264b6be2a515e01e171e67e9c05fdef']['inspect'] = () => (Inspect_p67) => (Inspect_i60) => (__$a__ => `{ ` + `globalEventHandlers: ` + Inspect_i60.inspect()(__$a__.globalEventHandlers) + `, ` + `subscriptions: ` + Inspect_p67.inspect()(__$a__.subscriptions) + ` }`);
  let DEFAULT_CONFIG = ({ subscriptions: (null), globalEventHandlers: (null) });
  let addGlobalEventHandler = (action => config => ({ ...config, globalEventHandlers: append(action)(config.globalEventHandlers) }));

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/ByteArray.mad
  let fromList = (bytes =>  {
    let bytesArray = [];
    while (bytes !== null) {
      bytesArray.push(bytes.v);
      bytes = bytes.n;
    }
    return Uint8Array.from(bytesArray)
  } );
  fromList((null));

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Http.mad
  Inspect['TransferEncoding_796b81c0e00da04bf7bbfe5795b16551'] = {};
  Inspect['TransferEncoding_796b81c0e00da04bf7bbfe5795b16551']['inspect'] = () => (__$a__ => ((__x__) => {
    if (__x__.__constructor === "Chunked") {
      return `Chunked`;
    }
    else if (__x__.__constructor === "Compress") {
      return `Compress`;
    }
    else if (__x__.__constructor === "Deflate") {
      return `Deflate`;
    }
    else if (__x__.__constructor === "Gzip") {
      return `Gzip`;
    }
    else {
      return `Unknown`;
    }
  })(__$a__));
  Inspect['Header_796b81c0e00da04bf7bbfe5795b16551'] = {};
  Inspect['Header_796b81c0e00da04bf7bbfe5795b16551']['inspect'] = () => (__$a__ => ((__x__) => {
    if (__x__.__constructor === "Header" && true && true) {
      let a0 = __x__.__args[0];
      let a1 = __x__.__args[1];
      return `Header(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `, ` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a1) + `)`;
    }
    else {
      return `Unknown`;
    }
  })(__$a__));
  Inspect['Method_796b81c0e00da04bf7bbfe5795b16551'] = {};
  Inspect['Method_796b81c0e00da04bf7bbfe5795b16551']['inspect'] = () => (__$a__ => ((__x__) => {
    if (__x__.__constructor === "CONNECT") {
      return `CONNECT`;
    }
    else if (__x__.__constructor === "DELETE") {
      return `DELETE`;
    }
    else if (__x__.__constructor === "GET") {
      return `GET`;
    }
    else if (__x__.__constructor === "HEAD") {
      return `HEAD`;
    }
    else if (__x__.__constructor === "OPTIONS") {
      return `OPTIONS`;
    }
    else if (__x__.__constructor === "PATCH") {
      return `PATCH`;
    }
    else if (__x__.__constructor === "POST") {
      return `POST`;
    }
    else if (__x__.__constructor === "PUT") {
      return `PUT`;
    }
    else if (__x__.__constructor === "TRACE") {
      return `TRACE`;
    }
    else {
      return `Unknown`;
    }
  })(__$a__));
  Inspect['Record_bodyf_0796b81c0e00da04bf7bbfe5795b16551_headersf_1796b81c0e00da04bf7bbfe5795b16551_statusf_2796b81c0e00da04bf7bbfe5795b16551'] = {};
  Inspect['Record_bodyf_0796b81c0e00da04bf7bbfe5795b16551_headersf_1796b81c0e00da04bf7bbfe5795b16551_statusf_2796b81c0e00da04bf7bbfe5795b16551']['inspect'] = () => (Inspect_d445) => (Inspect_w438) => (Inspect_p431) => (__$a__ => `{ ` + `body: ` + Inspect_p431.inspect()(__$a__.body) + `, ` + `headers: ` + Inspect_w438.inspect()(__$a__.headers) + `, ` + `status: ` + Inspect_d445.inspect()(__$a__.status) + ` }`);
  Inspect['ClientError_796b81c0e00da04bf7bbfe5795b16551'] = {};
  Inspect['ClientError_796b81c0e00da04bf7bbfe5795b16551']['inspect'] = () => (__$a__ => ((__x__) => {
    if (__x__.__constructor === "AccessDenied") {
      return `AccessDenied`;
    }
    else if (__x__.__constructor === "AddressNotFound") {
      return `AddressNotFound`;
    }
    else if (__x__.__constructor === "BadTransferEncoding") {
      return `BadTransferEncoding`;
    }
    else if (__x__.__constructor === "BadUrl" && true) {
      let a0 = __x__.__args[0];
      return `BadUrl(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "ConnectionFailed") {
      return `ConnectionFailed`;
    }
    else if (__x__.__constructor === "Http2FramingError") {
      return `Http2FramingError`;
    }
    else if (__x__.__constructor === "IncompleteResponse") {
      return `IncompleteResponse`;
    }
    else if (__x__.__constructor === "InternalError") {
      return `InternalError`;
    }
    else if (__x__.__constructor === "InvalidSSLCertificate") {
      return `InvalidSSLCertificate`;
    }
    else if (__x__.__constructor === "MalformedResponse") {
      return `MalformedResponse`;
    }
    else if (__x__.__constructor === "NotSupported") {
      return `NotSupported`;
    }
    else if (__x__.__constructor === "SSLConnectionFailed") {
      return `SSLConnectionFailed`;
    }
    else if (__x__.__constructor === "SSLEngineNotFound") {
      return `SSLEngineNotFound`;
    }
    else if (__x__.__constructor === "SSLInitializationFailed") {
      return `SSLInitializationFailed`;
    }
    else if (__x__.__constructor === "Timeout") {
      return `Timeout`;
    }
    else if (__x__.__constructor === "TooManyRedirects") {
      return `TooManyRedirects`;
    }
    else if (__x__.__constructor === "UnresolvedProxy") {
      return `UnresolvedProxy`;
    }
    else if (__x__.__constructor === "UnsupportedProtocol") {
      return `UnsupportedProtocol`;
    }
    else {
      return `Unknown`;
    }
  })(__$a__));
  Inspect['Error_796b81c0e00da04bf7bbfe5795b16551'] = {};
  Inspect['Error_796b81c0e00da04bf7bbfe5795b16551']['inspect'] = () => (Inspect_n481) => (__$a__ => ((__x__) => {
    if (__x__.__constructor === "BadResponse" && true) {
      let a0 = __x__.__args[0];
      return `BadResponse(` + Inspect.Record_bodyf_0796b81c0e00da04bf7bbfe5795b16551_headersf_1796b81c0e00da04bf7bbfe5795b16551_statusf_2796b81c0e00da04bf7bbfe5795b16551.inspect()(Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d)(__apMtdDicts__(Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d, [Inspect.Header_796b81c0e00da04bf7bbfe5795b16551]))(Inspect_n481)(a0) + `)`;
    }
    else if (__x__.__constructor === "ClientError" && true) {
      let a0 = __x__.__args[0];
      return `ClientError(` + Inspect.ClientError_796b81c0e00da04bf7bbfe5795b16551.inspect()(a0) + `)`;
    }
    else {
      return `Unknown`;
    }
  })(__$a__));
  Inspect['Record_bodyf_0796b81c0e00da04bf7bbfe5795b16551_headersf_1796b81c0e00da04bf7bbfe5795b16551_methodf_2796b81c0e00da04bf7bbfe5795b16551_urlf_3796b81c0e00da04bf7bbfe5795b16551'] = {};
  Inspect['Record_bodyf_0796b81c0e00da04bf7bbfe5795b16551_headersf_1796b81c0e00da04bf7bbfe5795b16551_methodf_2796b81c0e00da04bf7bbfe5795b16551_urlf_3796b81c0e00da04bf7bbfe5795b16551']['inspect'] = () => (Inspect_b521) => (Inspect_u514) => (Inspect_n507) => (Inspect_g500) => (__$a__ => `{ ` + `body: ` + Inspect_g500.inspect()(__$a__.body) + `, ` + `headers: ` + Inspect_n507.inspect()(__$a__.headers) + `, ` + `method: ` + Inspect_u514.inspect()(__$a__.method) + `, ` + `url: ` + Inspect_b521.inspect()(__$a__.url) + ` }`);

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadUI/src/Attribute.mad
  let AttributeAlt = (a => ({ __constructor: "AttributeAlt", __args: [ a ] }));
  let AttributeClass = (a => ({ __constructor: "AttributeClass", __args: [ a ] }));
  let AttributeHref = (a => ({ __constructor: "AttributeHref", __args: [ a ] }));
  let AttributeKey = (a => ({ __constructor: "AttributeKey", __args: [ a ] }));
  let AttributeOnClick = (a => ({ __constructor: "AttributeOnClick", __args: [ a ] }));
  let AttributeOnInput = (a => ({ __constructor: "AttributeOnInput", __args: [ a ] }));
  let AttributePlaceholder = (a => ({ __constructor: "AttributePlaceholder", __args: [ a ] }));
  let AttributeSrc = (a => ({ __constructor: "AttributeSrc", __args: [ a ] }));
  let AttributeTo = (a => ({ __constructor: "AttributeTo", __args: [ a ] }));
  let AttributeType = (a => ({ __constructor: "AttributeType", __args: [ a ] }));
  Inspect['Attribute_e244123bdf371983d613158f20719eee'] = {};
  Inspect['Attribute_e244123bdf371983d613158f20719eee']['inspect'] = () => (Inspect_z2391) => (__$a__ => ((__x__) => {
    if (__x__.__constructor === "StringAttribute" && true) {
      let a0 = __x__.__args[0];
      return `StringAttribute(` + Inspect.Tuple_2.inspect()(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeAccept" && true) {
      let a0 = __x__.__args[0];
      return `AttributeAccept(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeAcceptCharset" && true) {
      let a0 = __x__.__args[0];
      return `AttributeAcceptCharset(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeAccessKey" && true) {
      let a0 = __x__.__args[0];
      return `AttributeAccessKey(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeAction" && true) {
      let a0 = __x__.__args[0];
      return `AttributeAction(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeAlt" && true) {
      let a0 = __x__.__args[0];
      return `AttributeAlt(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeAsync" && true) {
      let a0 = __x__.__args[0];
      return `AttributeAsync(` + Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeAutoComplete" && true) {
      let a0 = __x__.__args[0];
      return `AttributeAutoComplete(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeAutoFocus" && true) {
      let a0 = __x__.__args[0];
      return `AttributeAutoFocus(` + Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeAutoPlay" && true) {
      let a0 = __x__.__args[0];
      return `AttributeAutoPlay(` + Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeChecked" && true) {
      let a0 = __x__.__args[0];
      return `AttributeChecked(` + Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeCite" && true) {
      let a0 = __x__.__args[0];
      return `AttributeCite(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeClass" && true) {
      let a0 = __x__.__args[0];
      return `AttributeClass(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeCols" && true) {
      let a0 = __x__.__args[0];
      return `AttributeCols(` + Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeColSpan" && true) {
      let a0 = __x__.__args[0];
      return `AttributeColSpan(` + Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeContentEditable" && true) {
      let a0 = __x__.__args[0];
      return `AttributeContentEditable(` + Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeControls" && true) {
      let a0 = __x__.__args[0];
      return `AttributeControls(` + Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeCoords" && true) {
      let a0 = __x__.__args[0];
      return `AttributeCoords(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeData" && true) {
      let a0 = __x__.__args[0];
      return `AttributeData(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeDateTime" && true) {
      let a0 = __x__.__args[0];
      return `AttributeDateTime(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeDefault" && true) {
      let a0 = __x__.__args[0];
      return `AttributeDefault(` + Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeDefer" && true) {
      let a0 = __x__.__args[0];
      return `AttributeDefer(` + Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeDir" && true) {
      let a0 = __x__.__args[0];
      return `AttributeDir(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeDirName" && true) {
      let a0 = __x__.__args[0];
      return `AttributeDirName(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeDisabled" && true) {
      let a0 = __x__.__args[0];
      return `AttributeDisabled(` + Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeDownload" && true) {
      let a0 = __x__.__args[0];
      return `AttributeDownload(` + Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeDraggable" && true) {
      let a0 = __x__.__args[0];
      return `AttributeDraggable(` + Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeEncType" && true) {
      let a0 = __x__.__args[0];
      return `AttributeEncType(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeFor" && true) {
      let a0 = __x__.__args[0];
      return `AttributeFor(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeForm" && true) {
      let a0 = __x__.__args[0];
      return `AttributeForm(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeFormAction" && true) {
      let a0 = __x__.__args[0];
      return `AttributeFormAction(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeHeaders" && true) {
      let a0 = __x__.__args[0];
      return `AttributeHeaders(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeHeight" && true) {
      let a0 = __x__.__args[0];
      return `AttributeHeight(` + Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeHidden" && true) {
      let a0 = __x__.__args[0];
      return `AttributeHidden(` + Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeHigh" && true) {
      let a0 = __x__.__args[0];
      return `AttributeHigh(` + Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeHref" && true) {
      let a0 = __x__.__args[0];
      return `AttributeHref(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeHrefLang" && true) {
      let a0 = __x__.__args[0];
      return `AttributeHrefLang(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeId" && true) {
      let a0 = __x__.__args[0];
      return `AttributeId(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeIsMap" && true) {
      let a0 = __x__.__args[0];
      return `AttributeIsMap(` + Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeKey" && true) {
      let a0 = __x__.__args[0];
      return `AttributeKey(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeKind" && true) {
      let a0 = __x__.__args[0];
      return `AttributeKind(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeLang" && true) {
      let a0 = __x__.__args[0];
      return `AttributeLang(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeLabel" && true) {
      let a0 = __x__.__args[0];
      return `AttributeLabel(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeList" && true) {
      let a0 = __x__.__args[0];
      return `AttributeList(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeLoop" && true) {
      let a0 = __x__.__args[0];
      return `AttributeLoop(` + Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeLow" && true) {
      let a0 = __x__.__args[0];
      return `AttributeLow(` + Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeMax" && true) {
      let a0 = __x__.__args[0];
      return `AttributeMax(` + Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeMaxLength" && true) {
      let a0 = __x__.__args[0];
      return `AttributeMaxLength(` + Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeMedia" && true) {
      let a0 = __x__.__args[0];
      return `AttributeMedia(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeMethod" && true) {
      let a0 = __x__.__args[0];
      return `AttributeMethod(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeMin" && true) {
      let a0 = __x__.__args[0];
      return `AttributeMin(` + Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeMultiple" && true) {
      let a0 = __x__.__args[0];
      return `AttributeMultiple(` + Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeMuted" && true) {
      let a0 = __x__.__args[0];
      return `AttributeMuted(` + Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeName" && true) {
      let a0 = __x__.__args[0];
      return `AttributeName(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeNoValidate" && true) {
      let a0 = __x__.__args[0];
      return `AttributeNoValidate(` + Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnAbort" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnAbort(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnBlur" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnBlur(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnCanPlay" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnCanPlay(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnCanPlayThrough" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnCanPlayThrough(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnChange" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnChange(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnClick" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnClick(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnContextMenu" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnContextMenu(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnCopy" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnCopy(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnCueChange" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnCueChange(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnCut" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnCut(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnDblClick" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnDblClick(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnDrag" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnDrag(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnDragEnd" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnDragEnd(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnDragEnter" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnDragEnter(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnDragLeave" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnDragLeave(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnDragOver" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnDragOver(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnDragStart" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnDragStart(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnDrop" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnDrop(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnDurationChange" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnDurationChange(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnEmptied" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnEmptied(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnEnded" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnEnded(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnError" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnError(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnFocus" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnFocus(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnInput" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnInput(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnInvalid" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnInvalid(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnKeyPress" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnKeyPress(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnKeyDown" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnKeyDown(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnKeyUp" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnKeyUp(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnLoad" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnLoad(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnLoadedData" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnLoadedData(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnLoadedMetaData" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnLoadedMetaData(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnLoadStart" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnLoadStart(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnMouseDown" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnMouseDown(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnMouseEnter" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnMouseEnter(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnMouseLeave" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnMouseLeave(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnMouseMove" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnMouseMove(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnMouseUp" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnMouseUp(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnMouseWheel" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnMouseWheel(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnMouseOver" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnMouseOver(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnMouseOut" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnMouseOut(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnPaste" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnPaste(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnPause" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnPause(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnPlay" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnPlay(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnPlaying" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnPlaying(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnProgress" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnProgress(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnRateChange" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnRateChange(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnReset" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnReset(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnScroll" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnScroll(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnSearch" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnSearch(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnSeeked" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnSeeked(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnSeeking" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnSeeking(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnSelect" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnSelect(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnStalled" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnStalled(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnSubmit" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnSubmit(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnSuspend" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnSuspend(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnTimeUpdate" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnTimeUpdate(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnToggle" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnToggle(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnTransitionCancel" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnTransitionCancel(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnTransitionEnd" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnTransitionEnd(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnTransitionRun" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnTransitionRun(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnTransitionStart" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnTransitionStart(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnVolumeChange" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnVolumeChange(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnWaiting" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnWaiting(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOnWheel" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOnWheel(` + Inspect.a_arr_b.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOpen" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOpen(` + Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeOptimum" && true) {
      let a0 = __x__.__args[0];
      return `AttributeOptimum(` + Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributePattern" && true) {
      let a0 = __x__.__args[0];
      return `AttributePattern(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributePlaceholder" && true) {
      let a0 = __x__.__args[0];
      return `AttributePlaceholder(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributePoster" && true) {
      let a0 = __x__.__args[0];
      return `AttributePoster(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributePreload" && true) {
      let a0 = __x__.__args[0];
      return `AttributePreload(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeReadOnly" && true) {
      let a0 = __x__.__args[0];
      return `AttributeReadOnly(` + Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeRel" && true) {
      let a0 = __x__.__args[0];
      return `AttributeRel(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeRequired" && true) {
      let a0 = __x__.__args[0];
      return `AttributeRequired(` + Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeReversed" && true) {
      let a0 = __x__.__args[0];
      return `AttributeReversed(` + Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeRows" && true) {
      let a0 = __x__.__args[0];
      return `AttributeRows(` + Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeRowSpan" && true) {
      let a0 = __x__.__args[0];
      return `AttributeRowSpan(` + Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeSandBox" && true) {
      let a0 = __x__.__args[0];
      return `AttributeSandBox(` + Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeScope" && true) {
      let a0 = __x__.__args[0];
      return `AttributeScope(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeSelected" && true) {
      let a0 = __x__.__args[0];
      return `AttributeSelected(` + Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeShape" && true) {
      let a0 = __x__.__args[0];
      return `AttributeShape(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeSize" && true) {
      let a0 = __x__.__args[0];
      return `AttributeSize(` + Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeSizes" && true) {
      let a0 = __x__.__args[0];
      return `AttributeSizes(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeSpan" && true) {
      let a0 = __x__.__args[0];
      return `AttributeSpan(` + Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeSpellCheck" && true) {
      let a0 = __x__.__args[0];
      return `AttributeSpellCheck(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeSrc" && true) {
      let a0 = __x__.__args[0];
      return `AttributeSrc(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeSrcDoc" && true) {
      let a0 = __x__.__args[0];
      return `AttributeSrcDoc(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeSrcLang" && true) {
      let a0 = __x__.__args[0];
      return `AttributeSrcLang(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeSrcSet" && true) {
      let a0 = __x__.__args[0];
      return `AttributeSrcSet(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeStart" && true) {
      let a0 = __x__.__args[0];
      return `AttributeStart(` + Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeStep" && true) {
      let a0 = __x__.__args[0];
      return `AttributeStep(` + Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeStyle" && true) {
      let a0 = __x__.__args[0];
      return `AttributeStyle(` + Inspect.Dictionary_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeTabIndex" && true) {
      let a0 = __x__.__args[0];
      return `AttributeTabIndex(` + Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeTarget" && true) {
      let a0 = __x__.__args[0];
      return `AttributeTarget(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeTitle" && true) {
      let a0 = __x__.__args[0];
      return `AttributeTitle(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeTo" && true) {
      let a0 = __x__.__args[0];
      return `AttributeTo(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeTranslate" && true) {
      let a0 = __x__.__args[0];
      return `AttributeTranslate(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeType" && true) {
      let a0 = __x__.__args[0];
      return `AttributeType(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeUseMap" && true) {
      let a0 = __x__.__args[0];
      return `AttributeUseMap(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeValue" && true) {
      let a0 = __x__.__args[0];
      return `AttributeValue(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeWidth" && true) {
      let a0 = __x__.__args[0];
      return `AttributeWidth(` + Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "AttributeWrap" && true) {
      let a0 = __x__.__args[0];
      return `AttributeWrap(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else {
      return `Unknown`;
    }
  })(__$a__));
  let alt = AttributeAlt;
  let className = AttributeClass;
  let href = AttributeHref;
  let inputType = AttributeType;
  let key = AttributeKey;
  let onClick = AttributeOnClick;
  let onInput = AttributeOnInput;
  let placeholder = AttributePlaceholder;
  let src = AttributeSrc;
  let to = AttributeTo;

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadUI/src/CoreUtils.mad

  let runAction =  env => updater => {
    env.currentState = updater(env.currentState);
    window.env = env;
    const newElement = env.rootView(env.currentState);
    env.patch(env.currentElement, newElement);
    env.currentElement = newElement;
  } ;
  let wrapEventHandler =  (env, ctor, handler) => {
    return event => {
      event.eventType = event.type;
      // Calling an event handler gives us a list of wishes
      let wishes = handler(env.currentState)(ctor(event));

      while (wishes !== null) {
        fulfill(runAction(env))(runAction(env))(wishes.v);
        wishes = wishes.n;
      }
    }
  }
  ;

  function createElement(tagName, options) {
      return document.createElement(tagName, options);
  }
  function createElementNS(namespaceURI, qualifiedName, options) {
      return document.createElementNS(namespaceURI, qualifiedName, options);
  }
  function createDocumentFragment() {
      return parseFragment(document.createDocumentFragment());
  }
  function createTextNode(text) {
      return document.createTextNode(text);
  }
  function createComment(text) {
      return document.createComment(text);
  }
  function insertBefore(parentNode, newNode, referenceNode) {
      if (isDocumentFragment$1(parentNode)) {
          let node = parentNode;
          while (node && isDocumentFragment$1(node)) {
              const fragment = parseFragment(node);
              node = fragment.parent;
          }
          parentNode = node !== null && node !== void 0 ? node : parentNode;
      }
      if (isDocumentFragment$1(newNode)) {
          newNode = parseFragment(newNode, parentNode);
      }
      if (referenceNode && isDocumentFragment$1(referenceNode)) {
          referenceNode = parseFragment(referenceNode).firstChildNode;
      }
      parentNode.insertBefore(newNode, referenceNode);
  }
  function removeChild(node, child) {
      node.removeChild(child);
  }
  function appendChild(node, child) {
      if (isDocumentFragment$1(child)) {
          child = parseFragment(child, node);
      }
      node.appendChild(child);
  }
  function parentNode(node) {
      if (isDocumentFragment$1(node)) {
          while (node && isDocumentFragment$1(node)) {
              const fragment = parseFragment(node);
              node = fragment.parent;
          }
          return node !== null && node !== void 0 ? node : null;
      }
      return node.parentNode;
  }
  function nextSibling(node) {
      var _a;
      if (isDocumentFragment$1(node)) {
          const fragment = parseFragment(node);
          const parent = parentNode(fragment);
          if (parent && fragment.lastChildNode) {
              const children = Array.from(parent.childNodes);
              const index = children.indexOf(fragment.lastChildNode);
              return (_a = children[index + 1]) !== null && _a !== void 0 ? _a : null;
          }
          return null;
      }
      return node.nextSibling;
  }
  function tagName(elm) {
      return elm.tagName;
  }
  function setTextContent(node, text) {
      node.textContent = text;
  }
  function getTextContent(node) {
      return node.textContent;
  }
  function isElement$1(node) {
      return node.nodeType === 1;
  }
  function isText(node) {
      return node.nodeType === 3;
  }
  function isComment(node) {
      return node.nodeType === 8;
  }
  function isDocumentFragment$1(node) {
      return node.nodeType === 11;
  }
  function parseFragment(fragmentNode, parentNode) {
      var _a, _b, _c;
      const fragment = fragmentNode;
      (_a = fragment.parent) !== null && _a !== void 0 ? _a : (fragment.parent = parentNode !== null && parentNode !== void 0 ? parentNode : null);
      (_b = fragment.firstChildNode) !== null && _b !== void 0 ? _b : (fragment.firstChildNode = fragmentNode.firstChild);
      (_c = fragment.lastChildNode) !== null && _c !== void 0 ? _c : (fragment.lastChildNode = fragmentNode.lastChild);
      return fragment;
  }
  const htmlDomApi = {
      createElement,
      createElementNS,
      createTextNode,
      createDocumentFragment,
      createComment,
      insertBefore,
      removeChild,
      appendChild,
      parentNode,
      nextSibling,
      tagName,
      setTextContent,
      getTextContent,
      isElement: isElement$1,
      isText,
      isComment,
      isDocumentFragment: isDocumentFragment$1,
  };

  function vnode(sel, data, children, text, elm) {
      const key = data === undefined ? undefined : data.key;
      return { sel, data, children, text, elm, key };
  }

  const array = Array.isArray;
  function primitive(s) {
      return (typeof s === "string" ||
          typeof s === "number" ||
          s instanceof String ||
          s instanceof Number);
  }

  function isUndef(s) {
      return s === undefined;
  }
  function isDef(s) {
      return s !== undefined;
  }
  const emptyNode = vnode("", {}, [], undefined, undefined);
  function sameVnode(vnode1, vnode2) {
      var _a, _b;
      const isSameKey = vnode1.key === vnode2.key;
      const isSameIs = ((_a = vnode1.data) === null || _a === void 0 ? void 0 : _a.is) === ((_b = vnode2.data) === null || _b === void 0 ? void 0 : _b.is);
      const isSameSel = vnode1.sel === vnode2.sel;
      const isSameTextOrFragment = !vnode1.sel && vnode1.sel === vnode2.sel
          ? typeof vnode1.text === typeof vnode2.text
          : true;
      return isSameSel && isSameKey && isSameIs && isSameTextOrFragment;
  }
  /**
   * @todo Remove this function when the document fragment is considered stable.
   */
  function documentFragmentIsNotSupported() {
      throw new Error("The document fragment is not supported on this platform.");
  }
  function isElement(api, vnode) {
      return api.isElement(vnode);
  }
  function isDocumentFragment(api, vnode) {
      return api.isDocumentFragment(vnode);
  }
  function createKeyToOldIdx(children, beginIdx, endIdx) {
      var _a;
      const map = {};
      for (let i = beginIdx; i <= endIdx; ++i) {
          const key = (_a = children[i]) === null || _a === void 0 ? void 0 : _a.key;
          if (key !== undefined) {
              map[key] = i;
          }
      }
      return map;
  }
  const hooks = [
      "create",
      "update",
      "remove",
      "destroy",
      "pre",
      "post",
  ];
  function init(modules, domApi, options) {
      const cbs = {
          create: [],
          update: [],
          remove: [],
          destroy: [],
          pre: [],
          post: [],
      };
      const api = domApi !== undefined ? domApi : htmlDomApi;
      for (const hook of hooks) {
          for (const module of modules) {
              const currentHook = module[hook];
              if (currentHook !== undefined) {
                  cbs[hook].push(currentHook);
              }
          }
      }
      function emptyNodeAt(elm) {
          const id = elm.id ? "#" + elm.id : "";
          // elm.className doesn't return a string when elm is an SVG element inside a shadowRoot.
          // https://stackoverflow.com/questions/29454340/detecting-classname-of-svganimatedstring
          const classes = elm.getAttribute("class");
          const c = classes ? "." + classes.split(" ").join(".") : "";
          return vnode(api.tagName(elm).toLowerCase() + id + c, {}, [], undefined, elm);
      }
      function emptyDocumentFragmentAt(frag) {
          return vnode(undefined, {}, [], undefined, frag);
      }
      function createRmCb(childElm, listeners) {
          return function rmCb() {
              if (--listeners === 0) {
                  const parent = api.parentNode(childElm);
                  api.removeChild(parent, childElm);
              }
          };
      }
      function createElm(vnode, insertedVnodeQueue) {
          var _a, _b, _c, _d;
          let i;
          let data = vnode.data;
          if (data !== undefined) {
              const init = (_a = data.hook) === null || _a === void 0 ? void 0 : _a.init;
              if (isDef(init)) {
                  init(vnode);
                  data = vnode.data;
              }
          }
          const children = vnode.children;
          const sel = vnode.sel;
          if (sel === "!") {
              if (isUndef(vnode.text)) {
                  vnode.text = "";
              }
              vnode.elm = api.createComment(vnode.text);
          }
          else if (sel !== undefined) {
              // Parse selector
              const hashIdx = sel.indexOf("#");
              const dotIdx = sel.indexOf(".", hashIdx);
              const hash = hashIdx > 0 ? hashIdx : sel.length;
              const dot = dotIdx > 0 ? dotIdx : sel.length;
              const tag = hashIdx !== -1 || dotIdx !== -1
                  ? sel.slice(0, Math.min(hash, dot))
                  : sel;
              const elm = (vnode.elm =
                  isDef(data) && isDef((i = data.ns))
                      ? api.createElementNS(i, tag, data)
                      : api.createElement(tag, data));
              if (hash < dot)
                  elm.setAttribute("id", sel.slice(hash + 1, dot));
              if (dotIdx > 0)
                  elm.setAttribute("class", sel.slice(dot + 1).replace(/\./g, " "));
              for (i = 0; i < cbs.create.length; ++i)
                  cbs.create[i](emptyNode, vnode);
              if (array(children)) {
                  for (i = 0; i < children.length; ++i) {
                      const ch = children[i];
                      if (ch != null) {
                          api.appendChild(elm, createElm(ch, insertedVnodeQueue));
                      }
                  }
              }
              else if (primitive(vnode.text)) {
                  api.appendChild(elm, api.createTextNode(vnode.text));
              }
              const hook = vnode.data.hook;
              if (isDef(hook)) {
                  (_b = hook.create) === null || _b === void 0 ? void 0 : _b.call(hook, emptyNode, vnode);
                  if (hook.insert) {
                      insertedVnodeQueue.push(vnode);
                  }
              }
          }
          else if (((_c = options === null || options === void 0 ? void 0 : options.experimental) === null || _c === void 0 ? void 0 : _c.fragments) && vnode.children) {
              vnode.elm = ((_d = api.createDocumentFragment) !== null && _d !== void 0 ? _d : documentFragmentIsNotSupported)();
              for (i = 0; i < cbs.create.length; ++i)
                  cbs.create[i](emptyNode, vnode);
              for (i = 0; i < vnode.children.length; ++i) {
                  const ch = vnode.children[i];
                  if (ch != null) {
                      api.appendChild(vnode.elm, createElm(ch, insertedVnodeQueue));
                  }
              }
          }
          else {
              vnode.elm = api.createTextNode(vnode.text);
          }
          return vnode.elm;
      }
      function addVnodes(parentElm, before, vnodes, startIdx, endIdx, insertedVnodeQueue) {
          for (; startIdx <= endIdx; ++startIdx) {
              const ch = vnodes[startIdx];
              if (ch != null) {
                  api.insertBefore(parentElm, createElm(ch, insertedVnodeQueue), before);
              }
          }
      }
      function invokeDestroyHook(vnode) {
          var _a, _b;
          const data = vnode.data;
          if (data !== undefined) {
              (_b = (_a = data === null || data === void 0 ? void 0 : data.hook) === null || _a === void 0 ? void 0 : _a.destroy) === null || _b === void 0 ? void 0 : _b.call(_a, vnode);
              for (let i = 0; i < cbs.destroy.length; ++i)
                  cbs.destroy[i](vnode);
              if (vnode.children !== undefined) {
                  for (let j = 0; j < vnode.children.length; ++j) {
                      const child = vnode.children[j];
                      if (child != null && typeof child !== "string") {
                          invokeDestroyHook(child);
                      }
                  }
              }
          }
      }
      function removeVnodes(parentElm, vnodes, startIdx, endIdx) {
          var _a, _b;
          for (; startIdx <= endIdx; ++startIdx) {
              let listeners;
              let rm;
              const ch = vnodes[startIdx];
              if (ch != null) {
                  if (isDef(ch.sel)) {
                      invokeDestroyHook(ch);
                      listeners = cbs.remove.length + 1;
                      rm = createRmCb(ch.elm, listeners);
                      for (let i = 0; i < cbs.remove.length; ++i)
                          cbs.remove[i](ch, rm);
                      const removeHook = (_b = (_a = ch === null || ch === void 0 ? void 0 : ch.data) === null || _a === void 0 ? void 0 : _a.hook) === null || _b === void 0 ? void 0 : _b.remove;
                      if (isDef(removeHook)) {
                          removeHook(ch, rm);
                      }
                      else {
                          rm();
                      }
                  }
                  else if (ch.children) {
                      // Fragment node
                      invokeDestroyHook(ch);
                      removeVnodes(parentElm, ch.children, 0, ch.children.length - 1);
                  }
                  else {
                      // Text node
                      api.removeChild(parentElm, ch.elm);
                  }
              }
          }
      }
      function updateChildren(parentElm, oldCh, newCh, insertedVnodeQueue) {
          let oldStartIdx = 0;
          let newStartIdx = 0;
          let oldEndIdx = oldCh.length - 1;
          let oldStartVnode = oldCh[0];
          let oldEndVnode = oldCh[oldEndIdx];
          let newEndIdx = newCh.length - 1;
          let newStartVnode = newCh[0];
          let newEndVnode = newCh[newEndIdx];
          let oldKeyToIdx;
          let idxInOld;
          let elmToMove;
          let before;
          while (oldStartIdx <= oldEndIdx && newStartIdx <= newEndIdx) {
              if (oldStartVnode == null) {
                  oldStartVnode = oldCh[++oldStartIdx]; // Vnode might have been moved left
              }
              else if (oldEndVnode == null) {
                  oldEndVnode = oldCh[--oldEndIdx];
              }
              else if (newStartVnode == null) {
                  newStartVnode = newCh[++newStartIdx];
              }
              else if (newEndVnode == null) {
                  newEndVnode = newCh[--newEndIdx];
              }
              else if (sameVnode(oldStartVnode, newStartVnode)) {
                  patchVnode(oldStartVnode, newStartVnode, insertedVnodeQueue);
                  oldStartVnode = oldCh[++oldStartIdx];
                  newStartVnode = newCh[++newStartIdx];
              }
              else if (sameVnode(oldEndVnode, newEndVnode)) {
                  patchVnode(oldEndVnode, newEndVnode, insertedVnodeQueue);
                  oldEndVnode = oldCh[--oldEndIdx];
                  newEndVnode = newCh[--newEndIdx];
              }
              else if (sameVnode(oldStartVnode, newEndVnode)) {
                  // Vnode moved right
                  patchVnode(oldStartVnode, newEndVnode, insertedVnodeQueue);
                  api.insertBefore(parentElm, oldStartVnode.elm, api.nextSibling(oldEndVnode.elm));
                  oldStartVnode = oldCh[++oldStartIdx];
                  newEndVnode = newCh[--newEndIdx];
              }
              else if (sameVnode(oldEndVnode, newStartVnode)) {
                  // Vnode moved left
                  patchVnode(oldEndVnode, newStartVnode, insertedVnodeQueue);
                  api.insertBefore(parentElm, oldEndVnode.elm, oldStartVnode.elm);
                  oldEndVnode = oldCh[--oldEndIdx];
                  newStartVnode = newCh[++newStartIdx];
              }
              else {
                  if (oldKeyToIdx === undefined) {
                      oldKeyToIdx = createKeyToOldIdx(oldCh, oldStartIdx, oldEndIdx);
                  }
                  idxInOld = oldKeyToIdx[newStartVnode.key];
                  if (isUndef(idxInOld)) {
                      // New element
                      api.insertBefore(parentElm, createElm(newStartVnode, insertedVnodeQueue), oldStartVnode.elm);
                  }
                  else {
                      elmToMove = oldCh[idxInOld];
                      if (elmToMove.sel !== newStartVnode.sel) {
                          api.insertBefore(parentElm, createElm(newStartVnode, insertedVnodeQueue), oldStartVnode.elm);
                      }
                      else {
                          patchVnode(elmToMove, newStartVnode, insertedVnodeQueue);
                          oldCh[idxInOld] = undefined;
                          api.insertBefore(parentElm, elmToMove.elm, oldStartVnode.elm);
                      }
                  }
                  newStartVnode = newCh[++newStartIdx];
              }
          }
          if (newStartIdx <= newEndIdx) {
              before = newCh[newEndIdx + 1] == null ? null : newCh[newEndIdx + 1].elm;
              addVnodes(parentElm, before, newCh, newStartIdx, newEndIdx, insertedVnodeQueue);
          }
          if (oldStartIdx <= oldEndIdx) {
              removeVnodes(parentElm, oldCh, oldStartIdx, oldEndIdx);
          }
      }
      function patchVnode(oldVnode, vnode, insertedVnodeQueue) {
          var _a, _b, _c, _d, _e, _f, _g, _h;
          const hook = (_a = vnode.data) === null || _a === void 0 ? void 0 : _a.hook;
          (_b = hook === null || hook === void 0 ? void 0 : hook.prepatch) === null || _b === void 0 ? void 0 : _b.call(hook, oldVnode, vnode);
          const elm = (vnode.elm = oldVnode.elm);
          if (oldVnode === vnode)
              return;
          if (vnode.data !== undefined ||
              (isDef(vnode.text) && vnode.text !== oldVnode.text)) {
              (_c = vnode.data) !== null && _c !== void 0 ? _c : (vnode.data = {});
              (_d = oldVnode.data) !== null && _d !== void 0 ? _d : (oldVnode.data = {});
              for (let i = 0; i < cbs.update.length; ++i)
                  cbs.update[i](oldVnode, vnode);
              (_g = (_f = (_e = vnode.data) === null || _e === void 0 ? void 0 : _e.hook) === null || _f === void 0 ? void 0 : _f.update) === null || _g === void 0 ? void 0 : _g.call(_f, oldVnode, vnode);
          }
          const oldCh = oldVnode.children;
          const ch = vnode.children;
          if (isUndef(vnode.text)) {
              if (isDef(oldCh) && isDef(ch)) {
                  if (oldCh !== ch)
                      updateChildren(elm, oldCh, ch, insertedVnodeQueue);
              }
              else if (isDef(ch)) {
                  if (isDef(oldVnode.text))
                      api.setTextContent(elm, "");
                  addVnodes(elm, null, ch, 0, ch.length - 1, insertedVnodeQueue);
              }
              else if (isDef(oldCh)) {
                  removeVnodes(elm, oldCh, 0, oldCh.length - 1);
              }
              else if (isDef(oldVnode.text)) {
                  api.setTextContent(elm, "");
              }
          }
          else if (oldVnode.text !== vnode.text) {
              if (isDef(oldCh)) {
                  removeVnodes(elm, oldCh, 0, oldCh.length - 1);
              }
              api.setTextContent(elm, vnode.text);
          }
          (_h = hook === null || hook === void 0 ? void 0 : hook.postpatch) === null || _h === void 0 ? void 0 : _h.call(hook, oldVnode, vnode);
      }
      return function patch(oldVnode, vnode) {
          let i, elm, parent;
          const insertedVnodeQueue = [];
          for (i = 0; i < cbs.pre.length; ++i)
              cbs.pre[i]();
          if (isElement(api, oldVnode)) {
              oldVnode = emptyNodeAt(oldVnode);
          }
          else if (isDocumentFragment(api, oldVnode)) {
              oldVnode = emptyDocumentFragmentAt(oldVnode);
          }
          if (sameVnode(oldVnode, vnode)) {
              patchVnode(oldVnode, vnode, insertedVnodeQueue);
          }
          else {
              elm = oldVnode.elm;
              parent = api.parentNode(elm);
              createElm(vnode, insertedVnodeQueue);
              if (parent !== null) {
                  api.insertBefore(parent, vnode.elm, api.nextSibling(elm));
                  removeVnodes(parent, [oldVnode], 0, 0);
              }
          }
          for (i = 0; i < insertedVnodeQueue.length; ++i) {
              insertedVnodeQueue[i].data.hook.insert(insertedVnodeQueue[i]);
          }
          for (i = 0; i < cbs.post.length; ++i)
              cbs.post[i]();
          return vnode;
      };
  }

  function addNS(data, children, sel) {
      data.ns = "http://www.w3.org/2000/svg";
      if (sel !== "foreignObject" && children !== undefined) {
          for (let i = 0; i < children.length; ++i) {
              const child = children[i];
              if (typeof child === "string")
                  continue;
              const childData = child.data;
              if (childData !== undefined) {
                  addNS(childData, child.children, child.sel);
              }
          }
      }
  }
  function h(sel, b, c) {
      let data = {};
      let children;
      let text;
      let i;
      if (c !== undefined) {
          if (b !== null) {
              data = b;
          }
          if (array(c)) {
              children = c;
          }
          else if (primitive(c)) {
              text = c.toString();
          }
          else if (c && c.sel) {
              children = [c];
          }
      }
      else if (b !== undefined && b !== null) {
          if (array(b)) {
              children = b;
          }
          else if (primitive(b)) {
              text = b.toString();
          }
          else if (b && b.sel) {
              children = [b];
          }
          else {
              data = b;
          }
      }
      if (children !== undefined) {
          for (i = 0; i < children.length; ++i) {
              if (primitive(children[i]))
                  children[i] = vnode(undefined, undefined, undefined, children[i], undefined);
          }
      }
      if (sel[0] === "s" &&
          sel[1] === "v" &&
          sel[2] === "g" &&
          (sel.length === 3 || sel[3] === "." || sel[3] === "#")) {
          addNS(data, children, sel);
      }
      return vnode(sel, data, children, text, undefined);
  }

  const xlinkNS = "http://www.w3.org/1999/xlink";
  const xmlNS = "http://www.w3.org/XML/1998/namespace";
  const colonChar = 58;
  const xChar = 120;
  function updateAttrs(oldVnode, vnode) {
      let key;
      const elm = vnode.elm;
      let oldAttrs = oldVnode.data.attrs;
      let attrs = vnode.data.attrs;
      if (!oldAttrs && !attrs)
          return;
      if (oldAttrs === attrs)
          return;
      oldAttrs = oldAttrs || {};
      attrs = attrs || {};
      // update modified attributes, add new attributes
      for (key in attrs) {
          const cur = attrs[key];
          const old = oldAttrs[key];
          if (old !== cur) {
              if (cur === true) {
                  elm.setAttribute(key, "");
              }
              else if (cur === false) {
                  elm.removeAttribute(key);
              }
              else {
                  if (key.charCodeAt(0) !== xChar) {
                      elm.setAttribute(key, cur);
                  }
                  else if (key.charCodeAt(3) === colonChar) {
                      // Assume xml namespace
                      elm.setAttributeNS(xmlNS, key, cur);
                  }
                  else if (key.charCodeAt(5) === colonChar) {
                      // Assume xlink namespace
                      elm.setAttributeNS(xlinkNS, key, cur);
                  }
                  else {
                      elm.setAttribute(key, cur);
                  }
              }
          }
      }
      // remove removed attributes
      // use `in` operator since the previous `for` iteration uses it (.i.e. add even attributes with undefined value)
      // the other option is to remove all attributes with value == undefined
      for (key in oldAttrs) {
          if (!(key in attrs)) {
              elm.removeAttribute(key);
          }
      }
  }
  const attributesModule = {
      create: updateAttrs,
      update: updateAttrs,
  };

  function invokeHandler(handler, vnode, event) {
      if (typeof handler === "function") {
          // call function handler
          handler.call(vnode, event, vnode);
      }
      else if (typeof handler === "object") {
          // call multiple handlers
          for (let i = 0; i < handler.length; i++) {
              invokeHandler(handler[i], vnode, event);
          }
      }
  }
  function handleEvent(event, vnode) {
      const name = event.type;
      const on = vnode.data.on;
      // call event handler(s) if exists
      if (on && on[name]) {
          invokeHandler(on[name], vnode, event);
      }
  }
  function createListener() {
      return function handler(event) {
          handleEvent(event, handler.vnode);
      };
  }
  function updateEventListeners(oldVnode, vnode) {
      const oldOn = oldVnode.data.on;
      const oldListener = oldVnode.listener;
      const oldElm = oldVnode.elm;
      const on = vnode && vnode.data.on;
      const elm = (vnode && vnode.elm);
      let name;
      // optimization for reused immutable handlers
      if (oldOn === on) {
          return;
      }
      // remove existing listeners which no longer used
      if (oldOn && oldListener) {
          // if element changed or deleted we remove all existing listeners unconditionally
          if (!on) {
              for (name in oldOn) {
                  // remove listener if element was changed or existing listeners removed
                  oldElm.removeEventListener(name, oldListener, false);
              }
          }
          else {
              for (name in oldOn) {
                  // remove listener if existing listener removed
                  if (!on[name]) {
                      oldElm.removeEventListener(name, oldListener, false);
                  }
              }
          }
      }
      // add new listeners which has not already attached
      if (on) {
          // reuse existing listener or create new
          const listener = (vnode.listener =
              oldVnode.listener || createListener());
          // update vnode for listener
          listener.vnode = vnode;
          // if element changed or added we add all needed listeners unconditionally
          if (!oldOn) {
              for (name in on) {
                  // add listener if element was changed or new listeners added
                  elm.addEventListener(name, listener, false);
              }
          }
          else {
              for (name in on) {
                  // add listener if new listener added
                  if (!oldOn[name]) {
                      elm.addEventListener(name, listener, false);
                  }
              }
          }
      }
  }
  const eventListenersModule = {
      create: updateEventListeners,
      update: updateEventListeners,
      destroy: updateEventListeners,
  };

  function updateProps(oldVnode, vnode) {
      let key;
      let cur;
      let old;
      const elm = vnode.elm;
      let oldProps = oldVnode.data.props;
      let props = vnode.data.props;
      if (!oldProps && !props)
          return;
      if (oldProps === props)
          return;
      oldProps = oldProps || {};
      props = props || {};
      for (key in props) {
          cur = props[key];
          old = oldProps[key];
          if (old !== cur && (key !== "value" || elm[key] !== cur)) {
              elm[key] = cur;
          }
      }
  }
  const propsModule = { create: updateProps, update: updateProps };

  // Bindig `requestAnimationFrame` like this fixes a bug in IE/Edge. See #360 and #409.
  const raf = (typeof window !== "undefined" &&
      window.requestAnimationFrame.bind(window)) ||
      setTimeout;
  const nextFrame = function (fn) {
      raf(function () {
          raf(fn);
      });
  };
  let reflowForced = false;
  function setNextFrame(obj, prop, val) {
      nextFrame(function () {
          obj[prop] = val;
      });
  }
  function updateStyle(oldVnode, vnode) {
      let cur;
      let name;
      const elm = vnode.elm;
      let oldStyle = oldVnode.data.style;
      let style = vnode.data.style;
      if (!oldStyle && !style)
          return;
      if (oldStyle === style)
          return;
      oldStyle = oldStyle || {};
      style = style || {};
      const oldHasDel = "delayed" in oldStyle;
      for (name in oldStyle) {
          if (!style[name]) {
              if (name[0] === "-" && name[1] === "-") {
                  elm.style.removeProperty(name);
              }
              else {
                  elm.style[name] = "";
              }
          }
      }
      for (name in style) {
          cur = style[name];
          if (name === "delayed" && style.delayed) {
              for (const name2 in style.delayed) {
                  cur = style.delayed[name2];
                  if (!oldHasDel || cur !== oldStyle.delayed[name2]) {
                      setNextFrame(elm.style, name2, cur);
                  }
              }
          }
          else if (name !== "remove" && cur !== oldStyle[name]) {
              if (name[0] === "-" && name[1] === "-") {
                  elm.style.setProperty(name, cur);
              }
              else {
                  elm.style[name] = cur;
              }
          }
      }
  }
  function applyDestroyStyle(vnode) {
      let style;
      let name;
      const elm = vnode.elm;
      const s = vnode.data.style;
      if (!s || !(style = s.destroy))
          return;
      for (name in style) {
          elm.style[name] = style[name];
      }
  }
  function applyRemoveStyle(vnode, rm) {
      const s = vnode.data.style;
      if (!s || !s.remove) {
          rm();
          return;
      }
      if (!reflowForced) {
          // eslint-disable-next-line @typescript-eslint/no-unused-expressions
          vnode.elm.offsetLeft;
          reflowForced = true;
      }
      let name;
      const elm = vnode.elm;
      let i = 0;
      const style = s.remove;
      let amount = 0;
      const applied = [];
      for (name in style) {
          applied.push(name);
          elm.style[name] = style[name];
      }
      const compStyle = getComputedStyle(elm);
      const props = compStyle["transition-property"].split(", ");
      for (; i < props.length; ++i) {
          if (applied.indexOf(props[i]) !== -1)
              amount++;
      }
      elm.addEventListener("transitionend", function (ev) {
          if (ev.target === elm)
              --amount;
          if (amount === 0)
              rm();
      });
  }
  function forceReflow() {
      reflowForced = false;
  }
  const styleModule = {
      pre: forceReflow,
      create: updateStyle,
      update: updateStyle,
      destroy: applyDestroyStyle,
      remove: applyRemoveStyle,
  };

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadUI/src/Element.mad
  Inspect['Element_e57bb41ee76a8c1acf300f8ac2295260'] = {};
  Inspect['Element_e57bb41ee76a8c1acf300f8ac2295260']['inspect'] = () => (Inspect_r823) => (__$a__ => ((__x__) => {
    if (__x__.__constructor === "Element") {
      return `Element`;
    }
    else {
      return `Unknown`;
    }
  })(__$a__));

  const getAttributeTuple = attr =>
    [attr.__constructor.substr(9).toLowerCase(), attr.__args[0]];


  const PROP_NAMES = [
    "value"
  ];


  const objectifyAttrs = (env, attrs) => reduce(obj => attr => {
    const [attrName, attrValue] = getAttributeTuple(attr);

    if (attr.__constructor == "AttributeStyle") {
      const items = attr.__args[0].__args[0];
      const styleObj = reduce(obj => ([key, value]) => ({
        ...obj,
        [key]: value,
      }))({})(items);
      return { ...obj, style: styleObj }
    } else if (attr.__constructor == "StringAttribute") {
      return { ...obj, attrs: { ...obj.attrs, [attr.__args[0][0]]: attr.__args[0][1] }}
    } else if (attrName === "key") {
      return { ...obj, key: attrValue }
    } else if (PROP_NAMES.includes(attrName)) {
      return { ...obj, props: { ...obj.props, [attrName]: attrValue }}
    } else if (attrName.substr(0, 2) === "on") {
      const eventName = attrName.substr(2);
      const ctor = EventConstructors[eventName];
      return { ...obj, on: { ...obj.on, [eventName]: wrapEventHandler(env, ctor, attrValue) }}
    } else {
      return { ...obj, attrs: { ...obj.attrs, [attrName]: attrValue }}
    }
  })({})(attrs);


  const arrayifyChildren = (madlistChildren) => {
    let jsChildren = [];
    while (madlistChildren !== null) {
      jsChildren.push(madlistChildren.v);
      madlistChildren = madlistChildren.n;
    }

    return jsChildren
  }
  ;
  let tag = (tagName => attrs => children =>  {
    return h(tagName, objectifyAttrs(window.env, attrs), arrayifyChildren(children))
  } );
  let a = tag(`a`);
  let blockquote$1 = tag(`blockquote`);
  let br = tag(`br`);
  let button = tag(`button`);
  let code$1 = tag(`code`);
  let div = tag(`div`);
  let h1 = tag(`h1`);
  let h2 = tag(`h2`);
  let h3 = tag(`h3`);
  let h4 = tag(`h4`);
  let h5 = tag(`h5`);
  let h6 = tag(`h6`);
  let header = tag(`header`);
  let i = tag(`i`);
  let img = tag(`img`);
  let input = tag(`input`);
  let li = tag(`li`);
  let main = tag(`main`);
  let p = tag(`p`);
  let span = tag(`span`);
  let strong = tag(`strong`);
  let ul = tag(`ul`);
  let empty = (attrs => children =>  null );
  let link$1 = (attrs => children => {
    const objAttrs = objectifyAttrs(window.env, attrs);
    if (objAttrs.attrs.to) {
      if (!objAttrs.attrs) {
        objAttrs.attrs = {};
      }
      objAttrs.attrs.href = `\#${objAttrs.attrs.to}`;
      delete objAttrs.attrs.to;
    }

    return h("a", { ...objAttrs }, arrayifyChildren(children));
  });

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadUI/src/Core.mad
  let getUrl = (_ =>  document.location.hash.substr(1) || "/" );
  let render = (view => initialState => containerId => {
      
    window.env = {
      patch: null,
      currentElement: null,
      currentState: null,
      rootView: null,
    }
    ;
      let initialElement = view(initialState);
      
    const patch = init([attributesModule, propsModule, eventListenersModule, styleModule]);
    patch(document.getElementById(containerId), initialElement);

    window.env.patch = patch;
    window.env.currentElement = initialElement;
    window.env.rootView = view;
    window.env.currentState = initialState
    ;
      return ({ __constructor: "Unit", __args: [] });
  });

  const startGlobalEventHandlers = (env, globalActions) => {
    const keysForWindowEvents = [];
    while (globalActions !== null) {
      keysForWindowEvents.push(({
        eventName: globalActions.v.__args[0],
        eventHandler: globalActions.v.__args[1],
      }));
      globalActions = globalActions.n;
    }

    keysForWindowEvents.forEach(ga => {
      const handler = wrapEventHandler(env, EventConstructors[ga.eventName], ga.eventHandler);
      window.addEventListener(ga.eventName, handler);

      if (ga.eventName === "popstate") {
        handler({});
      }
    });
  }
  ;
  let renderWithConfig = (config => view => initialState => containerId => {
      render(view)(initialState)(containerId);
      
    const env = window.env;
    startGlobalEventHandlers(env, config.globalEventHandlers);

    let subs = config.subscriptions;
    while (subs !== null) {
      subs.v(runAction(env));
      subs = subs.n;
    }
      return ({ __constructor: "Unit", __args: [] });
  });

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Url.mad

  let encode = (url =>  {
    try {
      return Just(encodeURIComponent(url))
    } catch(e) {
      return Nothing
    }
  } );
  let decode = (url =>  {
      try {
        return Just(decodeURIComponent(url))
      } catch(e) {
        return Nothing
      }
    } );
  var Url = { encode, decode };

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/TargetInfo.mad
  Inspect['Record_hasJSf_0fb2405d57728f715cc9132d779e7fbdd_hasLLVMf_1fb2405d57728f715cc9132d779e7fbdd_isAvailablef_2fb2405d57728f715cc9132d779e7fbdd'] = {};
  Inspect['Record_hasJSf_0fb2405d57728f715cc9132d779e7fbdd_hasLLVMf_1fb2405d57728f715cc9132d779e7fbdd_isAvailablef_2fb2405d57728f715cc9132d779e7fbdd']['inspect'] = () => (Inspect_h59) => (Inspect_a52) => (Inspect_t45) => (__$a__ => `{ ` + `hasJS: ` + Inspect_t45.inspect()(__$a__.hasJS) + `, ` + `hasLLVM: ` + Inspect_a52.inspect()(__$a__.hasLLVM) + `, ` + `isAvailable: ` + Inspect_h59.inspect()(__$a__.isAvailable) + ` }`);

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Etiquette.mad

  let Etiquette = (content => div(({ v: className(`definition__etiquette`), n: null }))(({ v: content, n: null })));

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Title.mad

  let makeTagClassName = (isWarning => (isWarning ? `definition__target-tag definition__target-tag--warning` : `definition__target-tag`));
  let TargetTags = (targetInfo => ({ v: (targetInfo.hasJS ? span(({ v: className(makeTagClassName(!(targetInfo.isAvailable))), n: null }))(({ v: `JS`, n: null })) : empty()((null))), n: { v: (targetInfo.hasLLVM ? span(({ v: className(makeTagClassName(!(targetInfo.isAvailable))), n: null }))(({ v: `LLVM`, n: null })) : empty()((null))), n: null } }));
  let Title = (title => targetInfo => moduleName => h2(({ v: className(`definition__title`), n: null }))(({ v: span(({ v: className(`definition__title-span`), n: null }))(({ v: title, n: null })), n: __listCtorSpread__(TargetTags(targetInfo), { v: span(({ v: className(`definition__module`), n: null }))(({ v: moduleName, n: null })), n: null }) })));

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Since.mad

  let Since = (_P_ => ifElse(isEmpty)(always(empty()((null))))((since => p(({ v: className(`definition__since`), n: null }))(({ v: `since v`, n: { v: since, n: null } }))))((__R__ => __R__.since)(_P_)));

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadMarkdownParser/src/Main.mad

  let Text = (a => ({ __constructor: "Text", __args: [ a ] }));
  let Bold = (a => ({ __constructor: "Bold", __args: [ a ] }));
  let Italic = (a => ({ __constructor: "Italic", __args: [ a ] }));
  let InlineCode = (a => ({ __constructor: "InlineCode", __args: [ a ] }));
  let Link = (a => b => ({ __constructor: "Link", __args: [ a, b ] }));
  let Image = (a => b => ({ __constructor: "Image", __args: [ a, b ] }));
  let LineReturn = ({ __constructor: "LineReturn", __args: [  ] });
  let H1 = (a => ({ __constructor: "H1", __args: [ a ] }));
  let H2 = (a => ({ __constructor: "H2", __args: [ a ] }));
  let H3 = (a => ({ __constructor: "H3", __args: [ a ] }));
  let H4 = (a => ({ __constructor: "H4", __args: [ a ] }));
  let H5 = (a => ({ __constructor: "H5", __args: [ a ] }));
  let H6 = (a => ({ __constructor: "H6", __args: [ a ] }));
  let Paragraph = (a => ({ __constructor: "Paragraph", __args: [ a ] }));
  let Blockquote = (a => ({ __constructor: "Blockquote", __args: [ a ] }));
  let Code = (a => b => ({ __constructor: "Code", __args: [ a, b ] }));
  let UnorderedList = (a => ({ __constructor: "UnorderedList", __args: [ a ] }));
  Inspect['ContentPart_d248b95ce983f5117125258d627bcc0b'] = {};
  Inspect['ContentPart_d248b95ce983f5117125258d627bcc0b']['inspect'] = () => (__$a__ => ((__x__) => {
    if (__x__.__constructor === "Text" && true) {
      let a0 = __x__.__args[0];
      return `Text(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "Bold" && true) {
      let a0 = __x__.__args[0];
      return `Bold(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "Italic" && true) {
      let a0 = __x__.__args[0];
      return `Italic(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "InlineCode" && true) {
      let a0 = __x__.__args[0];
      return `InlineCode(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
    }
    else if (__x__.__constructor === "Link" && true && true) {
      let a0 = __x__.__args[0];
      let a1 = __x__.__args[1];
      return `Link(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `, ` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a1) + `)`;
    }
    else if (__x__.__constructor === "Image" && true && true) {
      let a0 = __x__.__args[0];
      let a1 = __x__.__args[1];
      return `Image(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `, ` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a1) + `)`;
    }
    else if (__x__.__constructor === "LineReturn") {
      return `LineReturn`;
    }
    else {
      return `Unknown`;
    }
  })(__$a__));
  Inspect['Block_d248b95ce983f5117125258d627bcc0b'] = {};
  Inspect['Block_d248b95ce983f5117125258d627bcc0b']['inspect'] = () => (__$a__ => ((__x__) => {
    if (__x__.__constructor === "H1" && true) {
      let a0 = __x__.__args[0];
      return `H1(` + Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(Inspect.ContentPart_d248b95ce983f5117125258d627bcc0b)(a0) + `)`;
    }
    else if (__x__.__constructor === "H2" && true) {
      let a0 = __x__.__args[0];
      return `H2(` + Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(Inspect.ContentPart_d248b95ce983f5117125258d627bcc0b)(a0) + `)`;
    }
    else if (__x__.__constructor === "H3" && true) {
      let a0 = __x__.__args[0];
      return `H3(` + Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(Inspect.ContentPart_d248b95ce983f5117125258d627bcc0b)(a0) + `)`;
    }
    else if (__x__.__constructor === "H4" && true) {
      let a0 = __x__.__args[0];
      return `H4(` + Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(Inspect.ContentPart_d248b95ce983f5117125258d627bcc0b)(a0) + `)`;
    }
    else if (__x__.__constructor === "H5" && true) {
      let a0 = __x__.__args[0];
      return `H5(` + Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(Inspect.ContentPart_d248b95ce983f5117125258d627bcc0b)(a0) + `)`;
    }
    else if (__x__.__constructor === "H6" && true) {
      let a0 = __x__.__args[0];
      return `H6(` + Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(Inspect.ContentPart_d248b95ce983f5117125258d627bcc0b)(a0) + `)`;
    }
    else if (__x__.__constructor === "Paragraph" && true) {
      let a0 = __x__.__args[0];
      return `Paragraph(` + Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(Inspect.ContentPart_d248b95ce983f5117125258d627bcc0b)(a0) + `)`;
    }
    else if (__x__.__constructor === "Blockquote" && true) {
      let a0 = __x__.__args[0];
      return `Blockquote(` + Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(Inspect.ContentPart_d248b95ce983f5117125258d627bcc0b)(a0) + `)`;
    }
    else if (__x__.__constructor === "Code" && true && true) {
      let a0 = __x__.__args[0];
      let a1 = __x__.__args[1];
      return `Code(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `, ` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a1) + `)`;
    }
    else if (__x__.__constructor === "UnorderedList" && true) {
      let a0 = __x__.__args[0];
      return `UnorderedList(` + Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(__apMtdDicts__(Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d, [Inspect.ContentPart_d248b95ce983f5117125258d627bcc0b]))(a0) + `)`;
    }
    else {
      return `Unknown`;
    }
  })(__$a__));
  let between = (start => mid => end => (_P_ => (__$PH2__ => apL(Functor.Parser_a91f6bfcb96b14c62290a677a2570798)(Applicative.Parser_a91f6bfcb96b14c62290a677a2570798)(__$PH2__)(end))((__$PH1__ => Applicative.Parser_a91f6bfcb96b14c62290a677a2570798.ap()(__$PH1__)(mid))(mapL(Functor.Parser_a91f6bfcb96b14c62290a677a2570798)(identity)(_P_))))(start));
  P.choice(({ v: P.letter, n: { v: P.digit, n: { v: P.char(__String.fromCharCode(33)), n: { v: P.char(__String.fromCharCode(63)), n: { v: P.char(__String.fromCharCode(32)), n: null } } } } }));
  let linkCharacter = P.choice(({ v: P.letter, n: { v: P.digit, n: { v: P.char(__String.fromCharCode(33)), n: { v: P.char(__String.fromCharCode(35)), n: { v: P.char(__String.fromCharCode(36)), n: { v: P.char(__String.fromCharCode(37)), n: { v: P.char(__String.fromCharCode(38)), n: { v: P.char(__String.fromCharCode(39)), n: { v: P.char(__String.fromCharCode(42)), n: { v: P.char(__String.fromCharCode(43)), n: { v: P.char(__String.fromCharCode(44)), n: { v: P.char(__String.fromCharCode(45)), n: { v: P.char(__String.fromCharCode(46)), n: { v: P.char(__String.fromCharCode(47)), n: { v: P.char(__String.fromCharCode(58)), n: { v: P.char(__String.fromCharCode(59)), n: { v: P.char(__String.fromCharCode(61)), n: { v: P.char(__String.fromCharCode(63)), n: { v: P.char(__String.fromCharCode(64)), n: { v: P.char(__String.fromCharCode(95)), n: { v: P.char(__String.fromCharCode(126)), n: null } } } } } } } } } } } } } } } } } } } } }));
  let bold = (_P_ => (__$PH4__ => apL(Functor.Parser_a91f6bfcb96b14c62290a677a2570798)(Applicative.Parser_a91f6bfcb96b14c62290a677a2570798)(__$PH4__)(P.string(`**`)))((__$PH3__ => Applicative.Parser_a91f6bfcb96b14c62290a677a2570798.ap()(__$PH3__)((_P_ => Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(String$1.fromList)((a => P.someTill(a)(P.lookAhead(P.string(`**`))))(_P_)))(P.notChar(__String.fromCharCode(10)))))(mapL(Functor.Parser_a91f6bfcb96b14c62290a677a2570798)(Bold)(_P_))))(P.string(`**`));
  let italic = (_P_ => (__$PH7__ => apL(Functor.Parser_a91f6bfcb96b14c62290a677a2570798)(Applicative.Parser_a91f6bfcb96b14c62290a677a2570798)(__$PH7__)(P.char(__String.fromCharCode(42))))((__$PH6__ => Applicative.Parser_a91f6bfcb96b14c62290a677a2570798.ap()(__$PH6__)((_P_ => Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(String$1.fromList)(P.many(_P_)))(P.notOneOf(({ v: __String.fromCharCode(42), n: { v: __String.fromCharCode(10), n: null } })))))((__$PH5__ => Applicative.Parser_a91f6bfcb96b14c62290a677a2570798.ap()(__$PH5__)(P.notChar(__String.fromCharCode(32))))(mapL(Functor.Parser_a91f6bfcb96b14c62290a677a2570798)((a => b => Italic(String$1.pushChar(a)(b))))(_P_)))))(P.char(__String.fromCharCode(42)));
  let inlineCode = (_P_ => (__$PH9__ => apL(Functor.Parser_a91f6bfcb96b14c62290a677a2570798)(Applicative.Parser_a91f6bfcb96b14c62290a677a2570798)(__$PH9__)(P.char(__String.fromCharCode(96))))((__$PH8__ => Applicative.Parser_a91f6bfcb96b14c62290a677a2570798.ap()(__$PH8__)((_P_ => Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(String$1.fromList)(P.many(_P_)))(P.notOneOf(({ v: __String.fromCharCode(96), n: { v: __String.fromCharCode(10), n: null } })))))(mapL(Functor.Parser_a91f6bfcb96b14c62290a677a2570798)(InlineCode)(_P_))))(P.char(__String.fromCharCode(96)));
  let link = (_P_ => (__$PH10__ => Applicative.Parser_a91f6bfcb96b14c62290a677a2570798.ap()(__$PH10__)(between(P.char(__String.fromCharCode(40)))((_P_ => Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(String$1.fromList)(P.many(_P_)))(linkCharacter))(P.char(__String.fromCharCode(41)))))(Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(Link)(_P_)))(between(P.char(__String.fromCharCode(91)))((_P_ => Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(String$1.fromList)(P.many(_P_)))(P.notOneOf(({ v: __String.fromCharCode(93), n: { v: __String.fromCharCode(10), n: null } }))))(P.char(__String.fromCharCode(93))));
  let image = (_P_ => (__$PH12__ => Applicative.Parser_a91f6bfcb96b14c62290a677a2570798.ap()(__$PH12__)(between(P.char(__String.fromCharCode(40)))((_P_ => Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(String$1.fromList)(P.many(_P_)))(linkCharacter))(P.char(__String.fromCharCode(41)))))((__$PH11__ => Applicative.Parser_a91f6bfcb96b14c62290a677a2570798.ap()(__$PH11__)(between(P.char(__String.fromCharCode(91)))((_P_ => Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(String$1.fromList)(P.many(_P_)))(P.notOneOf(({ v: __String.fromCharCode(93), n: { v: __String.fromCharCode(10), n: null } }))))(P.char(__String.fromCharCode(93)))))(mapL(Functor.Parser_a91f6bfcb96b14c62290a677a2570798)(Image)(_P_))))(P.char(__String.fromCharCode(33)));
  let textTerminals = P.choice(({ v: Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(always(``))(bold), n: { v: Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(always(``))(italic), n: { v: Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(always(``))(inlineCode), n: { v: Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(always(``))(image), n: { v: Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(always(``))(link), n: { v: Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(always(``))(P.eof), n: { v: P.string(`\n`), n: null } } } } } } }));
  let text = (_P_ => Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()((_P_ => Text(String$1.fromList(_P_))))((__$PH13__ => P.someTill(__$PH13__)(P.lookAhead(textTerminals)))(_P_)))(P.notChar(__String.fromCharCode(10)));
  let lineReturn = Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(always(LineReturn))(P.char(__String.fromCharCode(10)));
  let content = (_P_ => P.many(P.choice(_P_)))(({ v: bold, n: { v: italic, n: { v: inlineCode, n: { v: image, n: { v: link, n: { v: text, n: null } } } } } }));
  let lineReturnExceptBefore = (before => (_P_ => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()((__x__ => ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      return Alternative.Parser_a91f6bfcb96b14c62290a677a2570798.aempty();
    }
    else if (__x__.__constructor === "Nothing") {
      return lineReturn;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__)))(P.lookAhead((__$PH14__ => Applicative.Parser_a91f6bfcb96b14c62290a677a2570798.ap()(__$PH14__)(Alternative.Parser_a91f6bfcb96b14c62290a677a2570798.alt()(Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(always(Just(({ __constructor: "Unit", __args: [] }))))(before))(Applicative.Parser_a91f6bfcb96b14c62290a677a2570798.pure()(Nothing))))(mapL(Functor.Parser_a91f6bfcb96b14c62290a677a2570798)(identity)(_P_)))))(lineReturn));
  let contentWithLineReturn = (delimiter => (_P_ => Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(dropWhile$1(equals(LineReturn)))(P.some(P.choice(_P_))))(({ v: bold, n: { v: italic, n: { v: inlineCode, n: { v: image, n: { v: link, n: { v: text, n: { v: lineReturnExceptBefore(delimiter), n: null } } } } } } })));
  let heading = (constructor => _P_ => (__$PH16__ => apL(Functor.Parser_a91f6bfcb96b14c62290a677a2570798)(Applicative.Parser_a91f6bfcb96b14c62290a677a2570798)(__$PH16__)(singleReturnTerminal))((__$PH15__ => Applicative.Parser_a91f6bfcb96b14c62290a677a2570798.ap()(__$PH15__)(content))(mapL(Functor.Parser_a91f6bfcb96b14c62290a677a2570798)(constructor)(P.symbol(_P_)))));
  let singleReturnTerminal = Alternative.Parser_a91f6bfcb96b14c62290a677a2570798.alt()(P.string(`\n`))(Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(always(``))(P.eof));
  let doubleReturnTerminal = P.choice(({ v: P.string(`\n\n`), n: { v: Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(always(``))(P.eof), n: { v: (_P_ => (__$PH17__ => Applicative.Parser_a91f6bfcb96b14c62290a677a2570798.ap()(__$PH17__)(P.eof))(Applicative.Parser_a91f6bfcb96b14c62290a677a2570798.ap()(Applicative.Parser_a91f6bfcb96b14c62290a677a2570798.pure()((_ => _ => ``)))(_P_)))(P.char(__String.fromCharCode(10))), n: null } } }));
  let code = (_P_ => (__$PH21__ => apL(Functor.Parser_a91f6bfcb96b14c62290a677a2570798)(Applicative.Parser_a91f6bfcb96b14c62290a677a2570798)(__$PH21__)(P.choice(({ v: Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()((_ => ``))(apL(Functor.Parser_a91f6bfcb96b14c62290a677a2570798)(Applicative.Parser_a91f6bfcb96b14c62290a677a2570798)(P.string(`\n\`\`\``))(P.eof)), n: { v: P.string(`\n\`\`\`\n`), n: null } }))))((__$PH20__ => Applicative.Parser_a91f6bfcb96b14c62290a677a2570798.ap()(__$PH20__)(Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(String$1.fromList)(P.manyTill(P.anyChar)(P.lookAhead(P.string(`\n\`\`\``))))))((__$PH19__ => apL(Functor.Parser_a91f6bfcb96b14c62290a677a2570798)(Applicative.Parser_a91f6bfcb96b14c62290a677a2570798)(__$PH19__)(P.char(__String.fromCharCode(10))))((__$PH18__ => Applicative.Parser_a91f6bfcb96b14c62290a677a2570798.ap()(__$PH18__)(Alternative.Parser_a91f6bfcb96b14c62290a677a2570798.alt()(Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(String$1.fromList)(P.letters))(Applicative.Parser_a91f6bfcb96b14c62290a677a2570798.pure()(``))))(mapL(Functor.Parser_a91f6bfcb96b14c62290a677a2570798)((lang => c => Code(lang)(c)))(_P_))))))(P.string(`\`\`\``));
  let blockquote = (_P_ => (__$PH23__ => apL(Functor.Parser_a91f6bfcb96b14c62290a677a2570798)(Applicative.Parser_a91f6bfcb96b14c62290a677a2570798)(__$PH23__)(P.choice(({ v: doubleReturnTerminal, n: { v: P.lookAhead(P.string(`\n\`\`\``)), n: { v: P.lookAhead(P.string(`\n>`)), n: null } } }))))((__$PH22__ => Applicative.Parser_a91f6bfcb96b14c62290a677a2570798.ap()(__$PH22__)(contentWithLineReturn(P.choice(({ v: P.string(`\n`), n: { v: P.string(`\`\`\``), n: { v: P.string(`>`), n: null } } })))))(mapL(Functor.Parser_a91f6bfcb96b14c62290a677a2570798)(Blockquote)(_P_))))(Alternative.Parser_a91f6bfcb96b14c62290a677a2570798.alt()(P.symbol(`>`))(P.string(`>`)));
  let listItemStart = Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(always(``))(apL(Functor.Parser_a91f6bfcb96b14c62290a677a2570798)(Applicative.Parser_a91f6bfcb96b14c62290a677a2570798)(P.many(P.char(__String.fromCharCode(32))))(apL(Functor.Parser_a91f6bfcb96b14c62290a677a2570798)(Applicative.Parser_a91f6bfcb96b14c62290a677a2570798)(P.oneOf(({ v: __String.fromCharCode(42), n: { v: __String.fromCharCode(45), n: { v: __String.fromCharCode(43), n: null } } })))(P.some(P.char(__String.fromCharCode(32))))));
  let unorderedListItem = (_P_ => Monad.Parser_a91f6bfcb96b14c62290a677a2570798.chain()(always(apL(Functor.Parser_a91f6bfcb96b14c62290a677a2570798)(Applicative.Parser_a91f6bfcb96b14c62290a677a2570798)(content)(singleReturnTerminal)))(_P_))(listItemStart);
  let unorderedList = (_P_ => Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(UnorderedList)(P.some(_P_)))(unorderedListItem);
  let paragraph = (_P_ => (__$PH24__ => apL(Functor.Parser_a91f6bfcb96b14c62290a677a2570798)(Applicative.Parser_a91f6bfcb96b14c62290a677a2570798)(__$PH24__)(P.choice(({ v: doubleReturnTerminal, n: { v: P.lookAhead(P.string(`\n\`\`\``)), n: { v: P.lookAhead(P.string(`\n>`)), n: { v: P.lookAhead(apL(Functor.Parser_a91f6bfcb96b14c62290a677a2570798)(Applicative.Parser_a91f6bfcb96b14c62290a677a2570798)(P.string(`\n`))(listItemStart)), n: null } } } }))))(Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(Paragraph)(_P_)))(contentWithLineReturn(P.choice(({ v: listItemStart, n: { v: P.string(`\n`), n: { v: P.string(`\`\`\``), n: { v: P.string(`>`), n: null } } } }))));
  let block = P.choice(({ v: heading(H6)(`######`), n: { v: heading(H5)(`#####`), n: { v: heading(H4)(`####`), n: { v: heading(H3)(`###`), n: { v: heading(H2)(`##`), n: { v: heading(H1)(`#`), n: { v: unorderedList, n: { v: blockquote, n: { v: code, n: { v: paragraph, n: null } } } } } } } } } }));
  let markdownParser = (_P_ => Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()((_P_ => Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((__x__ => ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      let x = __x__.__args[0];
      return x;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__)))(filter((__x__ => ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      __x__.__args[0];
      return true;
    }
    else if (__x__.__constructor === "Nothing") {
      return false;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__)))(_P_))))(P.many(P.choice(_P_))))(({ v: Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(always(Nothing))(P.spaces), n: { v: Functor.Parser_a91f6bfcb96b14c62290a677a2570798.map()(Just)(block), n: null } }));
  let parseMarkdown = (_P_ => mapLeft(always(`Malformed markdown input`))(P.runParser(markdownParser)(_P_)));

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadMarkdownRenderer/src/Config.mad

  Inspect['Record_linkViewf_0ce723a3860371783fb1c5e99ad00afb9'] = {};
  Inspect['Record_linkViewf_0ce723a3860371783fb1c5e99ad00afb9']['inspect'] = () => (Inspect_o40) => (__$a__ => `{ ` + `linkView: ` + Inspect_o40.inspect()(__$a__.linkView) + ` }`);
  let defaultConfig = ({ linkView: (name => url => a(({ v: href(url), n: null }))(({ v: name, n: null }))) });
  let setLinkView = (linkView => config => ({ ...config, linkView: linkView }));

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadMarkdownRenderer/src/Main.mad

  let doRender = (config => markdown => div(({ v: className(`markdown`), n: null }))((Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(renderBlock(config))(markdown))));
  let renderBlock = (config => __x__ => ((__x__) => {
    if (__x__.__constructor === "H1" && true) {
      let content = __x__.__args[0];
      return h1((null))((renderContent(config)(content)));
    }
    else if (__x__.__constructor === "H2" && true) {
      let content = __x__.__args[0];
      return h2((null))((renderContent(config)(content)));
    }
    else if (__x__.__constructor === "H3" && true) {
      let content = __x__.__args[0];
      return h3((null))((renderContent(config)(content)));
    }
    else if (__x__.__constructor === "H4" && true) {
      let content = __x__.__args[0];
      return h4((null))((renderContent(config)(content)));
    }
    else if (__x__.__constructor === "H5" && true) {
      let content = __x__.__args[0];
      return h5((null))((renderContent(config)(content)));
    }
    else if (__x__.__constructor === "H6" && true) {
      let content = __x__.__args[0];
      return h6((null))((renderContent(config)(content)));
    }
    else if (__x__.__constructor === "Paragraph" && true) {
      let content = __x__.__args[0];
      return p((null))((renderContent(config)(content)));
    }
    else if (__x__.__constructor === "Blockquote" && true) {
      let content = __x__.__args[0];
      return blockquote$1((null))((renderContent(config)(content)));
    }
    else if (__x__.__constructor === "Code" && true && true) {
      let content = __x__.__args[1];
      return code$1((null))(({ v: content, n: null }));
    }
    else if (__x__.__constructor === "UnorderedList" && true) {
      let items = __x__.__args[0];
      return ul((null))((Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((item => li((null))((renderContent(config)(item)))))(items)));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  let renderContentPart = (config => __x__ => ((__x__) => {
    if (__x__.__constructor === "Text" && true) {
      let t = __x__.__args[0];
      return span(({ v: className(`markdown__text`), n: null }))(({ v: t, n: null }));
    }
    else if (__x__.__constructor === "Bold" && true) {
      let t = __x__.__args[0];
      return strong(({ v: className(`markdown__bold`), n: null }))(({ v: t, n: null }));
    }
    else if (__x__.__constructor === "Italic" && true) {
      let t = __x__.__args[0];
      return i(({ v: className(`markdown__italic`), n: null }))(({ v: t, n: null }));
    }
    else if (__x__.__constructor === "InlineCode" && true) {
      let t = __x__.__args[0];
      return span(({ v: className(`markdown__inline-code`), n: null }))(({ v: t, n: null }));
    }
    else if (__x__.__constructor === "Link" && true && true) {
      let t = __x__.__args[0];
      let l = __x__.__args[1];
      return config.linkView(t)(l);
    }
    else if (__x__.__constructor === "Image" && true && true) {
      let alt_ = __x__.__args[0];
      let s = __x__.__args[1];
      return img(({ v: className(`markdown__image`), n: { v: src(s), n: { v: alt(alt_), n: null } } }))((null));
    }
    else if (__x__.__constructor === "LineReturn") {
      return br((null))((null));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  let renderContent = (config => Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(renderContentPart(config)));
  let renderMarkdownWithConfig = (config => _P_ => (__x__ => ((__x__) => {
    if (__x__.__constructor === "Right" && true) {
      let ast = __x__.__args[0];
      return doRender(config)(ast);
    }
    else if (__x__.__constructor === "Left" && true) {
      return p((null))(({ v: `Error processing the given markdown`, n: null }));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__))(parseMarkdown(_P_)));

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Markdown.mad

  let mdConfig = (_P_ => setLinkView((txt => url => link$1(({ v: className(`markdown__link`), n: { v: to(url), n: null } }))(({ v: txt, n: null }))))(_P_))(defaultConfig);
  let renderMarkdown = renderMarkdownWithConfig(mdConfig);

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Description.mad

  let Description = (_P_ => (content => div(({ v: className(`definition__description`), n: null }))(({ v: content, n: null })))(renderMarkdown((__R__ => __R__.description)(_P_))));

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Example.mad

  let Example = (_P_ => ifElse(isEmpty)(always(empty()((null))))((example => p(({ v: className(`definition__example`), n: null }))(({ v: example, n: null }))))((__R__ => __R__.example)(_P_)));

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Typing.mad

  let Typing = (_P_ => (typing => p((null))(({ v: span(({ v: className(`definition__type`), n: null }))(({ v: typing, n: null })), n: null })))((__R__ => __R__.typing)(_P_)));

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/TargetedItem.mad

  Inspect['Record_hasJSf_059c0648b0200e5d96a19498b14e81296_hasLLVMf_159c0648b0200e5d96a19498b14e81296_isAvailablef_259c0648b0200e5d96a19498b14e81296'] = {};
  Inspect['Record_hasJSf_059c0648b0200e5d96a19498b14e81296_hasLLVMf_159c0648b0200e5d96a19498b14e81296_isAvailablef_259c0648b0200e5d96a19498b14e81296']['inspect'] = () => (Inspect_t97) => (Inspect_m90) => (Inspect_f83) => (__$a__ => `{ ` + `hasJS: ` + Inspect_f83.inspect()(__$a__.hasJS) + `, ` + `hasLLVM: ` + Inspect_m90.inspect()(__$a__.hasLLVM) + `, ` + `isAvailable: ` + Inspect_t97.inspect()(__$a__.isAvailable) + ` }`);
  let TargetedItem = (target => targeted => cardView => ((__x__) => {
    if (__x__.__constructor === "JSTarget" && true) {
      let js = __x__.__args[0];
      return (__eq__(target, JS) ? cardView(({ hasJS: true, hasLLVM: false, isAvailable: true }))(js) : cardView(({ hasJS: true, hasLLVM: false, isAvailable: false }))(js));
    }
    else if (__x__.__constructor === "LLVMTarget" && true) {
      let llvm = __x__.__args[0];
      return (__eq__(target, LLVM) ? cardView(({ hasJS: false, hasLLVM: true, isAvailable: true }))(llvm) : cardView(({ hasJS: false, hasLLVM: true, isAvailable: false }))(llvm));
    }
    else if (__x__.__constructor === "BothTargets" && true && true) {
      let js = __x__.__args[0];
      let llvm = __x__.__args[1];
      return (__eq__(target, JS) ? cardView(({ hasJS: true, hasLLVM: true, isAvailable: true }))(js) : cardView(({ hasJS: true, hasLLVM: true, isAvailable: true }))(llvm));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(targeted));

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Expression.mad
  let ExpressionView = (moduleName => targetInfo => definition => li(({ v: className(`definition` + (targetInfo.isAvailable ? `` : ` definition--greyed-out`)), n: { v: key(moduleName + `-` + definition.name), n: null } }))(({ v: Etiquette(`Function`), n: { v: Title(definition.name)(targetInfo)(moduleName), n: { v: Typing(definition), n: { v: Since(definition), n: { v: Description(definition), n: { v: Example(definition), n: null } } } } } })));
  let Expression = (target => moduleName => definition => TargetedItem(target)(definition)(ExpressionView(moduleName)));

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/SideMenu.mad

  let ModuleLink = (module => li(({ v: className(`side-menu__link-item`), n: null }))(({ v: link$1(({ v: className(`side-menu__link`), n: { v: to(`/` + module.name), n: null } }))(({ v: span(({ v: className(`side-menu__link-name`), n: null }))(({ v: module.name, n: null })), n: null })), n: null })));
  let MenuLink = (__x__ => ((__x__) => {
    if (__x__.length === 2 && true && true) {
      let [name,moduleName] = __x__;
      return li(({ v: className(`side-menu__link-item`), n: null }))(({ v: link$1(({ v: className(`side-menu__link`), n: { v: to(`/` + moduleName + `/` + name), n: null } }))(({ v: span(({ v: className(`side-menu__link-name`), n: null }))(({ v: name, n: null })), n: { v: span(({ v: className(`side-menu__link-extra`), n: null }))(({ v: moduleName, n: null })), n: null } })), n: null }));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  let LinksForType = (search => getItems => retrieveName => _P_ => itemsToLinks(Monad.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.chain()((module => (_P_ => Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((a => ([retrieveName(a), module.name])))(List.filter((_P_ => String$1.match(search)(String$1.toLower(retrieveName(_P_)))))(getItems(_P_))))(module)))(_P_)));
  let itemsToLinks = (_P_ => Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(MenuLink)(List.sortBy((a => b => Comparable.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.compare()(Tuple.fst(a))(Tuple.fst(b))))(_P_)));
  let sortAndFilterModules = (search => _P_ => List.sortBy((a => b => Comparable.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.compare()(a.name)(b.name)))(List.filter((_P_ => String$1.match(search)(String$1.toLower((__R__ => __R__.name)(_P_)))))(_P_)));
  let MenuSection = (title => items => (List.isEmpty(items) ? (null) : ({ v: h3(({ v: className(`side-menu__title`), n: null }))(({ v: title, n: null })), n: { v: ul(({ v: className(`side-menu__link-list`), n: null }))((items)), n: null } })));
  let SideMenu = (search => modules => {
      let moduleLinks = Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(ModuleLink)(sortAndFilterModules(search)(modules));
      let functionLinks = LinksForType(search)((__R__ => __R__.expressions))(getName$1)(modules);
      let typeLinks = LinksForType(search)((__R__ => __R__.typeDeclarations))((__R__ => __R__.name))(modules);
      let aliasLinks = LinksForType(search)((__R__ => __R__.aliases))((__R__ => __R__.name))(modules);
      let interfaceLinks = LinksForType(search)((__R__ => __R__.interfaces))((__R__ => __R__.name))(modules);
      let instanceLinks = LinksForType(search)((__R__ => __R__.instances))((__R__ => __R__.declaration))(modules);
      let notFound = all(List.isEmpty)(({ v: moduleLinks, n: { v: functionLinks, n: { v: typeLinks, n: { v: aliasLinks, n: { v: interfaceLinks, n: { v: instanceLinks, n: null } } } } } }));
      return (notFound ? div(({ v: className(`side-menu`), n: null }))(({ v: p(({ v: className(`side-menu__no-result`), n: null }))(({ v: `No result was found for `, n: { v: span(({ v: className(`side-menu__no-result-search`), n: null }))(({ v: search, n: null })), n: null } })), n: null })) : div(({ v: className(`side-menu`), n: null }))(({ v: div(({ v: className(`side-menu__scrollbar-container`), n: null }))((__listCtorSpread__(MenuSection(`MODULES`)(moduleLinks), __listCtorSpread__(MenuSection(`FUNCTIONS`)(functionLinks), __listCtorSpread__(MenuSection(`TYPES`)(typeLinks), __listCtorSpread__(MenuSection(`ALIASES`)(aliasLinks), __listCtorSpread__(MenuSection(`INTERFACES`)(interfaceLinks), MenuSection(`INSTANCES`)(instanceLinks)))))))), n: null })));
  });

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Header.mad

  let handleInput = (state => event => ((__x__) => {
    if (__x__.__constructor === "InputEvent" && true) {
      let e = __x__.__args[0];
      return ({ v: Monad.Wish_48091bbb4c188d584814a4a3f8207f71.of()(always(({ ...state, search: String$1.toLower(e.target.value) }))), n: null });
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(event));
  let handleTargetChange = (target => syncAction((state => _ => ({ ...state, target: target }))));
  let Header = (target => header(({ v: className(`header`), n: null }))(({ v: h1(({ v: className(`header__title`), n: null }))(({ v: `MadDoc`, n: null })), n: { v: input(({ v: inputType(`text`), n: { v: placeholder(`What are you looking for?`), n: { v: className(`search-field`), n: { v: onInput(handleInput), n: null } } } }))((null)), n: { v: div(({ v: className(`target-selector`), n: null }))(({ v: button(({ v: className(`target-selector__button` + (__eq__(target, JS) ? ` target-selector__button--selected` : ``)), n: { v: onClick(handleTargetChange(JS)), n: null } }))(({ v: `Javascript`, n: null })), n: { v: button(({ v: className(`target-selector__button` + (__eq__(target, LLVM) ? ` target-selector__button--selected` : ``)), n: { v: onClick(handleTargetChange(LLVM)), n: null } }))(({ v: `LLVM`, n: null })), n: null } })), n: null } } })));

  // file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/FilePath/Posix.mad

  let dropTrailingPathSeparator = ifElse((path => !__eq__(path, `/`) && __eq__(String$1.lastChar(path), Just(__String.fromCharCode(47)))))(String$1.dropLast(1))(identity);
  let performSplitPath = (buffer => foundSlash => path => {
      let $_result_;
      let $_continue_ = true;
      let $$buffer = buffer;
      let $$foundSlash = foundSlash;
      let $$path = path;

      while($_continue_) {
        let $buffer = $$buffer;
        let $foundSlash = $$foundSlash;
        let $path = $$path;

          $_continue_ = false;
          ((__x__) => {
    if (__x__.__constructor === "Nothing") {
      ($_result_ = ({ v: $buffer, n: null }));
    }
    else if (__x__.__constructor === "Just" && __x__.__args[0] === __String.fromCharCode(47)) {
      ($$buffer = $buffer + `/`, $$foundSlash = true, $$path = String$1.drop(1)($path), $_continue_ = true);
    }
    else if (__x__.__constructor === "Just" && true) {
      let char = __x__.__args[0];
      ($foundSlash ? ($_result_ = Monoid.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.mconcat()(({ v: $buffer, n: null }))(performSplitPath(Show.Char_5b7ebeeaa5acfe1eeea5a9e9845b152d.show()(char))(false)(String$1.drop(1)($path)))) : ($$buffer = $buffer + Show.Char_5b7ebeeaa5acfe1eeea5a9e9845b152d.show()(char), $$foundSlash = false, $$path = String$1.drop(1)($path), $_continue_ = true));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(String$1.firstChar($path));
      }
      return $_result_;
  });
  let splitPath = performSplitPath(``)(false);
  let joinPath = (_P_ => ifElse((_P_ => equals(Just(`/`))(first(_P_))))((_P_ => Monoid.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.mconcat()(`/`)(String$1.join(`/`)(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(dropTrailingPathSeparator)(drop$1(1)(_P_))))))((_P_ => String$1.join(`/`)(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(dropTrailingPathSeparator)(_P_))))(filter(complement(String$1.isEmpty))(_P_)));
  let canonicalizePath = (_P_ => joinPath(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((_P_ => ifElse((_P_ => equals(Just(__String.fromCharCode(47)))(String$1.lastChar(_P_))))(String$1.replace(`([^/]+)/*`)(`$1/`))(identity)(ifElse((_P_ => equals(`./`)(String$1.take(2)(_P_))))(String$1.drop(2))(identity)(_P_))))(splitPath(_P_))));
  let dropPathSegments = (howMany => _P_ => joinPath(drop$1(howMany)(splitPath(_P_))));
  let isRootPathOf = (root => path => {
      let rootParts = splitPath(root);
      let pathParts = splitPath(path);
      let rootStart = dropTrailingPathSeparator(fromMaybe(``)(first(rootParts)));
      let pathStart = dropTrailingPathSeparator(fromMaybe(``)(first(pathParts)));
      return (__eq__(rootStart, pathStart) || __eq__(rootStart, ``) ? (__eq__(rootStart, ``) ? true : isRootPathOf(dropPathSegments(1)(root))(dropPathSegments(1)(path))) : false);
  });

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Breadcrumbs.mad

  let Breadcrumb = (a => b => ({ __constructor: "Breadcrumb", __args: [ a, b ] }));
  Inspect['Breadcrumb_4abd1cdd7b1c927d13bb10af893c40c1'] = {};
  Inspect['Breadcrumb_4abd1cdd7b1c927d13bb10af893c40c1']['inspect'] = () => (__$a__ => ((__x__) => {
    if (__x__.__constructor === "Breadcrumb" && true && true) {
      let a0 = __x__.__args[0];
      let a1 = __x__.__args[1];
      return `Breadcrumb(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `, ` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a1) + `)`;
    }
    else {
      return `Unknown`;
    }
  })(__$a__));
  let getLink = (__x__ => ((__x__) => {
    if (__x__.__constructor === "Breadcrumb" && true && true) {
      let l = __x__.__args[1];
      return l;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  let getName = (__x__ => ((__x__) => {
    if (__x__.__constructor === "Breadcrumb" && true && true) {
      let l = __x__.__args[0];
      return l;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  let generateBreadcrumbName = (_P_ => (pathSegment => (__eq__(pathSegment, `/`) || __eq__(pathSegment, ``) ? `home` : pathSegment))(canonicalizePath(_P_)));
  let computeBreadcrumbs = (_P_ => snd(reduce((acc => pathSegment => ((__x__) => {
    if (__x__.length === 2 && true && true) {
      let [prevPath,breadcrumbs] = __x__;
      return (_P_ => (path => ([path, append(Breadcrumb(generateBreadcrumbName(pathSegment))(path))(breadcrumbs)]))(joinPath(append(pathSegment)(_P_))))(({ v: prevPath, n: null }));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(acc)))(([``, (null)]))(splitPath((__R__ => __R__.path)(_P_)))));
  let BreadcrumbItem = (breadcrumb => li(({ v: className(`breadcrumbs__item`), n: { v: key(getLink(breadcrumb)), n: null } }))(({ v: link$1(({ v: to(getLink(breadcrumb)), n: null }))(({ v: getName(breadcrumb), n: null })), n: null })));
  let Breadcrumbs = (_P_ => (breadcrumbs => ul(({ v: className(`breadcrumbs`), n: null }))((breadcrumbs)))(intercalateWithIndex((i => li(({ v: className(`breadcrumbs__separator`), n: { v: key(`sep-` + Show.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.show()(i)), n: null } }))(({ v: `/`, n: null }))))(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(BreadcrumbItem)(computeBreadcrumbs(_P_)))));

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Type.mad

  Inspect['Record_hasJSf_08676e862654b892c9c43f99085b0b684_hasLLVMf_18676e862654b892c9c43f99085b0b684_isAvailablef_28676e862654b892c9c43f99085b0b684'] = {};
  Inspect['Record_hasJSf_08676e862654b892c9c43f99085b0b684_hasLLVMf_18676e862654b892c9c43f99085b0b684_isAvailablef_28676e862654b892c9c43f99085b0b684']['inspect'] = () => (Inspect_m220) => (Inspect_f213) => (Inspect_y206) => (__$a__ => `{ ` + `hasJS: ` + Inspect_y206.inspect()(__$a__.hasJS) + `, ` + `hasLLVM: ` + Inspect_f213.inspect()(__$a__.hasLLVM) + `, ` + `isAvailable: ` + Inspect_m220.inspect()(__$a__.isAvailable) + ` }`);
  let Type = (moduleName => typeDefinition => {
      let constructors = typeDefinition.constructors;
      let manyCtors = _$_length_$_$2(constructors) > 1;
      let renderedConstructors = (manyCtors ? ConstructorsView(`=`)(constructors) : ({ v: span(({ v: className(`definition__constructor`), n: null }))(({ v: span(({ v: className(`highlight`), n: null }))(({ v: ` = `, n: null })), n: { v: span((null))(({ v: fromMaybe(`???`)(first(constructors)), n: null })), n: null } })), n: null }));
      return li(({ v: className(`definition`), n: null }))((__listCtorSpread__(({ v: Etiquette(`Type`), n: { v: Title(typeDefinition.name)(({ hasJS: false, hasLLVM: false, isAvailable: false }))(moduleName), n: null } }), { v: div(({ v: className(`definition__adt`), n: null }))(({ v: span(({ v: className(`highlight`), n: null }))(({ v: `type`, n: null })), n: { v: span((null))(({ v: ` `, n: { v: typeDefinition.name, n: { v: ` `, n: { v: typeDefinition.params, n: null } } } })), n: { v: span(({ v: className(`definition__constructors`), n: null }))((renderedConstructors)), n: null } } })), n: ({ v: Since(typeDefinition), n: { v: Description(typeDefinition), n: { v: Example(typeDefinition), n: null } } }) })));
  });
  let ConstructorsView = (separator => items => {
      let $_result_;
      let $_continue_ = true;
      let $_start_ = {};
      let $_end_ = $_start_;
      let $$separator = separator;
      let $$items = items;

      while($_continue_) {
        let $separator = $$separator;
        let $items = $$items;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ !== null && true && true) {
      let { v: ctor, n: more } = __x__;
      ($_end_ = $_end_.n = { v: ConstructorView($separator)(ctor) }, $$separator = `|`, $$items = more, $_continue_ = true);
    }
    else if (__x__ !== null && true && __x__.n === null) {
      let { v: ctor } = __x__;
      ($_end_.n = ({ v: ConstructorView($separator)(ctor), n: null }), $_result_ = $_start_.n);
    }
    else if (__x__ === null) {
      ($_end_.n = (null), $_result_ = $_start_.n);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($items);
      }
      return $_result_;
  });
  let ConstructorView = (separator => constructor => div(({ v: className(`definition__constructor`), n: null }))(({ v: span(({ v: className(`highlight`), n: null }))(({ v: `  `, n: { v: separator, n: null } })), n: { v: span((null))(({ v: ` `, n: { v: constructor, n: null } })), n: null } })));

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Alias.mad

  Inspect['Record_hasJSf_0282beae8e01da97cf064d349e642ce84_hasLLVMf_1282beae8e01da97cf064d349e642ce84_isAvailablef_2282beae8e01da97cf064d349e642ce84'] = {};
  Inspect['Record_hasJSf_0282beae8e01da97cf064d349e642ce84_hasLLVMf_1282beae8e01da97cf064d349e642ce84_isAvailablef_2282beae8e01da97cf064d349e642ce84']['inspect'] = () => (Inspect_a156) => (Inspect_t149) => (Inspect_m142) => (__$a__ => `{ ` + `hasJS: ` + Inspect_m142.inspect()(__$a__.hasJS) + `, ` + `hasLLVM: ` + Inspect_t149.inspect()(__$a__.hasLLVM) + `, ` + `isAvailable: ` + Inspect_a156.inspect()(__$a__.isAvailable) + ` }`);
  let Alias = (moduleName => aliasDef => {
      let aliasedType = aliasDef.aliasedType;
      let params = (String$1.isEmpty(aliasDef.params) ? `` : ` ` + aliasDef.params);
      return li(({ v: className(`definition`), n: null }))((__listCtorSpread__(({ v: Etiquette(`Alias`), n: { v: Title(aliasDef.name)(({ hasJS: false, hasLLVM: false, isAvailable: false }))(moduleName), n: null } }), { v: div(({ v: className(`definition__adt`), n: null }))(({ v: span(({ v: className(`highlight`), n: null }))(({ v: `alias`, n: null })), n: { v: span((null))(({ v: ` `, n: { v: aliasDef.name, n: { v: params, n: null } } })), n: { v: span(({ v: className(`definition__constructors`), n: null }))(({ v: span(({ v: className(`definition__constructor`), n: null }))(({ v: span(({ v: className(`highlight`), n: null }))(({ v: ` = `, n: null })), n: { v: span((null))(({ v: aliasedType, n: null })), n: null } })), n: null })), n: null } } })), n: ({ v: Since(aliasDef), n: { v: Description(aliasDef), n: { v: Example(aliasDef), n: null } } }) })));
  });

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Interface.mad

  Inspect['Record_hasJSf_08e01963e32f303d010c30bf965841bdf_hasLLVMf_18e01963e32f303d010c30bf965841bdf_isAvailablef_28e01963e32f303d010c30bf965841bdf'] = {};
  Inspect['Record_hasJSf_08e01963e32f303d010c30bf965841bdf_hasLLVMf_18e01963e32f303d010c30bf965841bdf_isAvailablef_28e01963e32f303d010c30bf965841bdf']['inspect'] = () => (Inspect_h189) => (Inspect_a182) => (Inspect_t175) => (__$a__ => `{ ` + `hasJS: ` + Inspect_t175.inspect()(__$a__.hasJS) + `, ` + `hasLLVM: ` + Inspect_a182.inspect()(__$a__.hasLLVM) + `, ` + `isAvailable: ` + Inspect_h189.inspect()(__$a__.isAvailable) + ` }`);
  let Interface = (moduleName => interfaceDef => {
      let methods = interfaceDef.methods;
      let constraints = interfaceDef.constraints;
      let constraintElements = (!__eq__(constraints, ``) ? ({ v: span((null))(({ v: constraints, n: null })), n: { v: span(({ v: className(`highlight`), n: null }))(({ v: ` => `, n: null })), n: null } }) : (null));
      return li(({ v: className(`definition`), n: null }))((__listCtorSpread__(({ v: Etiquette(`Interface`), n: { v: Title(interfaceDef.name)(({ hasJS: false, hasLLVM: false, isAvailable: false }))(moduleName), n: null } }), { v: div(({ v: className(`definition__interface`), n: null }))(({ v: span(({ v: className(`highlight`), n: null }))(({ v: `interface `, n: null })), n: { v: span((null))((constraintElements)), n: { v: span((null))(({ v: interfaceDef.name, n: { v: ` `, n: { v: interfaceDef.vars, n: null } } })), n: { v: span(({ v: className(`highlight`), n: null }))(({ v: ` {`, n: null })), n: { v: div((null))((Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((method => div((null))(({ v: `  `, n: { v: method, n: null } }))))(methods))), n: { v: span(({ v: className(`highlight`), n: null }))(({ v: `}`, n: null })), n: null } } } } } })), n: ({ v: Since(interfaceDef), n: { v: Description(interfaceDef), n: { v: Example(interfaceDef), n: null } } }) })));
  });

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Instance.mad

  Inspect['Record_hasJSf_0e18b622886cb39510dabf7debf8c29eb_hasLLVMf_1e18b622886cb39510dabf7debf8c29eb_isAvailablef_2e18b622886cb39510dabf7debf8c29eb'] = {};
  Inspect['Record_hasJSf_0e18b622886cb39510dabf7debf8c29eb_hasLLVMf_1e18b622886cb39510dabf7debf8c29eb_isAvailablef_2e18b622886cb39510dabf7debf8c29eb']['inspect'] = () => (Inspect_n143) => (Inspect_g136) => (Inspect_z129) => (__$a__ => `{ ` + `hasJS: ` + Inspect_z129.inspect()(__$a__.hasJS) + `, ` + `hasLLVM: ` + Inspect_g136.inspect()(__$a__.hasLLVM) + `, ` + `isAvailable: ` + Inspect_n143.inspect()(__$a__.isAvailable) + ` }`);
  let Instance = (moduleName => instanceDef => {
      let constraints = instanceDef.constraints;
      let constraintElements = (!__eq__(constraints, ``) ? ({ v: span((null))(({ v: constraints, n: null })), n: { v: span(({ v: className(`highlight`), n: null }))(({ v: ` => `, n: null })), n: null } }) : (null));
      return li(({ v: className(`definition`), n: null }))((__listCtorSpread__(({ v: Etiquette(`Instance`), n: { v: Title(instanceDef.declaration)(({ hasJS: false, hasLLVM: false, isAvailable: false }))(moduleName), n: null } }), { v: div(({ v: className(`definition__interface`), n: null }))(({ v: span(({ v: className(`highlight`), n: null }))(({ v: `instance `, n: null })), n: { v: span((null))((constraintElements)), n: { v: span((null))(({ v: instanceDef.declaration, n: null })), n: null } } })), n: ({ v: Since(instanceDef), n: { v: Description(instanceDef), n: { v: Example(instanceDef), n: null } } }) })));
  });

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/PathResolver.mad

  let ModuleResult = (a => ({ __constructor: "ModuleResult", __args: [ a ] }));
  let ExpressionResult = (a => b => ({ __constructor: "ExpressionResult", __args: [ a, b ] }));
  let TypeResult = (a => b => ({ __constructor: "TypeResult", __args: [ a, b ] }));
  let AliasResult = (a => b => ({ __constructor: "AliasResult", __args: [ a, b ] }));
  let InterfaceResult = (a => b => ({ __constructor: "InterfaceResult", __args: [ a, b ] }));
  let InstanceResult = (a => b => ({ __constructor: "InstanceResult", __args: [ a, b ] }));
  let NotFound = ({ __constructor: "NotFound", __args: [  ] });
  Inspect['PathResult_b34400a9a78f179ffe0f3c51f401cc20'] = {};
  Inspect['PathResult_b34400a9a78f179ffe0f3c51f401cc20']['inspect'] = () => (__$a__ => ((__x__) => {
    if (__x__.__constructor === "ModuleResult" && true) {
      let a0 = __x__.__args[0];
      return `ModuleResult(` + Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(__apMtdDicts__(Inspect.Record_aliasesf_0059980c9d93cf2de3fe268b3d9d26775_descriptionf_1059980c9d93cf2de3fe268b3d9d26775_expressionsf_2059980c9d93cf2de3fe268b3d9d26775_instancesf_3059980c9d93cf2de3fe268b3d9d26775_interfacesf_4059980c9d93cf2de3fe268b3d9d26775_namef_5059980c9d93cf2de3fe268b3d9d26775_pathf_6059980c9d93cf2de3fe268b3d9d26775_typeDeclarationsf_7059980c9d93cf2de3fe268b3d9d26775, [__apMtdDicts__(Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d, [__apMtdDicts__(Inspect.Record_constructorsf_0059980c9d93cf2de3fe268b3d9d26775_descriptionf_1059980c9d93cf2de3fe268b3d9d26775_examplef_2059980c9d93cf2de3fe268b3d9d26775_namef_3059980c9d93cf2de3fe268b3d9d26775_paramsf_4059980c9d93cf2de3fe268b3d9d26775_sincef_5059980c9d93cf2de3fe268b3d9d26775, [Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, __apMtdDicts__(Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d, [Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d])])]), Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, __apMtdDicts__(Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d, [__apMtdDicts__(Inspect.Record_constraintsf_0059980c9d93cf2de3fe268b3d9d26775_descriptionf_1059980c9d93cf2de3fe268b3d9d26775_examplef_2059980c9d93cf2de3fe268b3d9d26775_methodsf_3059980c9d93cf2de3fe268b3d9d26775_namef_4059980c9d93cf2de3fe268b3d9d26775_sincef_5059980c9d93cf2de3fe268b3d9d26775_varsf_6059980c9d93cf2de3fe268b3d9d26775, [Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, __apMtdDicts__(Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d, [Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d]), Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d])]), __apMtdDicts__(Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d, [__apMtdDicts__(Inspect.Record_constraintsf_0059980c9d93cf2de3fe268b3d9d26775_declarationf_1059980c9d93cf2de3fe268b3d9d26775_descriptionf_2059980c9d93cf2de3fe268b3d9d26775_examplef_3059980c9d93cf2de3fe268b3d9d26775_sincef_4059980c9d93cf2de3fe268b3d9d26775, [Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d])]), __apMtdDicts__(Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d, [__apMtdDicts__(Inspect.Targeted_059980c9d93cf2de3fe268b3d9d26775, [__apMtdDicts__(Inspect.Record_descriptionf_0059980c9d93cf2de3fe268b3d9d26775_examplef_1059980c9d93cf2de3fe268b3d9d26775_namef_2059980c9d93cf2de3fe268b3d9d26775_sincef_3059980c9d93cf2de3fe268b3d9d26775_typingf_4059980c9d93cf2de3fe268b3d9d26775, [Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d])])]), Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, __apMtdDicts__(Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d, [__apMtdDicts__(Inspect.Record_aliasedTypef_0059980c9d93cf2de3fe268b3d9d26775_descriptionf_1059980c9d93cf2de3fe268b3d9d26775_examplef_2059980c9d93cf2de3fe268b3d9d26775_namef_3059980c9d93cf2de3fe268b3d9d26775_paramsf_4059980c9d93cf2de3fe268b3d9d26775_sincef_5059980c9d93cf2de3fe268b3d9d26775, [Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d])])]))(a0) + `)`;
    }
    else if (__x__.__constructor === "ExpressionResult" && true && true) {
      let a0 = __x__.__args[0];
      let a1 = __x__.__args[1];
      return `ExpressionResult(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `, ` + Inspect.Targeted_059980c9d93cf2de3fe268b3d9d26775.inspect()(__apMtdDicts__(Inspect.Record_descriptionf_0059980c9d93cf2de3fe268b3d9d26775_examplef_1059980c9d93cf2de3fe268b3d9d26775_namef_2059980c9d93cf2de3fe268b3d9d26775_sincef_3059980c9d93cf2de3fe268b3d9d26775_typingf_4059980c9d93cf2de3fe268b3d9d26775, [Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d]))(a1) + `)`;
    }
    else if (__x__.__constructor === "TypeResult" && true && true) {
      let a0 = __x__.__args[0];
      let a1 = __x__.__args[1];
      return `TypeResult(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `, ` + Inspect.Record_constructorsf_0059980c9d93cf2de3fe268b3d9d26775_descriptionf_1059980c9d93cf2de3fe268b3d9d26775_examplef_2059980c9d93cf2de3fe268b3d9d26775_namef_3059980c9d93cf2de3fe268b3d9d26775_paramsf_4059980c9d93cf2de3fe268b3d9d26775_sincef_5059980c9d93cf2de3fe268b3d9d26775.inspect()(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(__apMtdDicts__(Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d, [Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d]))(a1) + `)`;
    }
    else if (__x__.__constructor === "AliasResult" && true && true) {
      let a0 = __x__.__args[0];
      let a1 = __x__.__args[1];
      return `AliasResult(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `, ` + Inspect.Record_aliasedTypef_0059980c9d93cf2de3fe268b3d9d26775_descriptionf_1059980c9d93cf2de3fe268b3d9d26775_examplef_2059980c9d93cf2de3fe268b3d9d26775_namef_3059980c9d93cf2de3fe268b3d9d26775_paramsf_4059980c9d93cf2de3fe268b3d9d26775_sincef_5059980c9d93cf2de3fe268b3d9d26775.inspect()(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(a1) + `)`;
    }
    else if (__x__.__constructor === "InterfaceResult" && true && true) {
      let a0 = __x__.__args[0];
      let a1 = __x__.__args[1];
      return `InterfaceResult(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `, ` + Inspect.Record_constraintsf_0059980c9d93cf2de3fe268b3d9d26775_descriptionf_1059980c9d93cf2de3fe268b3d9d26775_examplef_2059980c9d93cf2de3fe268b3d9d26775_methodsf_3059980c9d93cf2de3fe268b3d9d26775_namef_4059980c9d93cf2de3fe268b3d9d26775_sincef_5059980c9d93cf2de3fe268b3d9d26775_varsf_6059980c9d93cf2de3fe268b3d9d26775.inspect()(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(__apMtdDicts__(Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d, [Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d]))(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(a1) + `)`;
    }
    else if (__x__.__constructor === "InstanceResult" && true && true) {
      let a0 = __x__.__args[0];
      let a1 = __x__.__args[1];
      return `InstanceResult(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `, ` + Inspect.Record_constraintsf_0059980c9d93cf2de3fe268b3d9d26775_declarationf_1059980c9d93cf2de3fe268b3d9d26775_descriptionf_2059980c9d93cf2de3fe268b3d9d26775_examplef_3059980c9d93cf2de3fe268b3d9d26775_sincef_4059980c9d93cf2de3fe268b3d9d26775.inspect()(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(a1) + `)`;
    }
    else if (__x__.__constructor === "NotFound") {
      return `NotFound`;
    }
    else {
      return `Unknown`;
    }
  })(__$a__));
  let filterByPath = (path => {
      let canPath = canonicalizePath(path);
      return List.filter((module => (_P_ => ifElse(isRootPathOf(drop(1)(toLower$1(canPath))))(always(!(List.isEmpty(module.expressions)) || !(List.isEmpty(module.typeDeclarations)) || !(List.isEmpty(module.aliases)) || !(List.isEmpty(module.interfaces)) || !(List.isEmpty(module.instances))))((_P_ => any(isRootPathOf(drop(1)(toLower$1(canPath))))(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((_P_ => toLower$1(Monoid.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.mconcat()(module.name + `/`)(_P_))))(always((__listCtorSpread__(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(getName$1)(module.expressions), __listCtorSpread__(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((__R__ => __R__.name))(module.typeDeclarations), __listCtorSpread__(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((__R__ => __R__.name))(module.aliases), __listCtorSpread__(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((__R__ => __R__.name))(module.interfaces), Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((__R__ => __R__.declaration))(module.instances)))))))(_P_)))))(toLower$1((__R__ => __R__.name)(_P_))))(module)));
  });
  let getModulesToShow = (state => (_P_ => filterByPath(state.path)((__R__ => __R__.modules)(_P_)))(state));
  let isItemView = (path => ifElse((_P_ => equals(1)(List._$_length_$_(_P_))))((_P_ => (__x__ => ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      let m = __x__.__args[0];
      return _$_length_$_$1(canonicalizePath(path)) > _$_length_$_$1(`/` + m.name);
    }
    else if (__x__.__constructor === "Nothing") {
      return false;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__))(List.first(_P_))))(always(false)));
  let tryItemByKind = (constructor => retrieveName => items => path => module => (_P_ => (__x__ => ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      let found = __x__.__args[0];
      return constructor(module.name)(found);
    }
    else if (__x__.__constructor === "Nothing") {
      return NotFound;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__))(List.find((e => __eq__(Just(retrieveName(e)), List.last(split(`/`)(path)))))(_P_)))(items));
  let _findItem = (finders => path => module => {
      let $_result_;
      let $_continue_ = true;
      let $$finders = finders;
      let $$path = path;
      let $$module = module;

      while($_continue_) {
        let $finders = $$finders;
        let $path = $$path;
        let $module = $$module;

          $_continue_ = false;
          ((__x__) => {
    if (__x__ !== null && true && true) {
      let { v: _$_try_$_, n: others } = __x__;
      ((__x__) => {
    if (__x__.__constructor === "NotFound") {
      ($$finders = others, $$path = $path, $$module = $module, $_continue_ = true);
    }
    else {
      let found = __x__;
      ($_result_ = found);
    }
  })(_$_try_$_($path)($module));
    }
    else if (__x__ === null) {
      ($_result_ = NotFound);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })($finders);
      }
      return $_result_;
  });
  let findItem = (path => module => _findItem(({ v: tryItemByKind(ExpressionResult)(getName$1)(module.expressions), n: { v: tryItemByKind(TypeResult)((__R__ => __R__.name))(module.typeDeclarations), n: { v: tryItemByKind(AliasResult)((__R__ => __R__.name))(module.aliases), n: { v: tryItemByKind(InterfaceResult)((__R__ => __R__.name))(module.interfaces), n: { v: tryItemByKind(InstanceResult)((__R__ => __R__.declaration))(module.instances), n: null } } } } }))(path)(module));
  let processPath = (state => (_P_ => ifElse(isItemView(state.path))((_P_ => (__x__ => ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      let m = __x__.__args[0];
      return findItem(state.path)(m);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(__x__))(List.first(_P_))))(ModuleResult)(getModulesToShow(_P_)))(state));

  // file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Main.mad

  Inspect['Record_modulesf_0647b91766773ccf2b0c227a3761a40cd_pathf_1647b91766773ccf2b0c227a3761a40cd_searchf_2647b91766773ccf2b0c227a3761a40cd_targetf_3647b91766773ccf2b0c227a3761a40cd'] = {};
  Inspect['Record_modulesf_0647b91766773ccf2b0c227a3761a40cd_pathf_1647b91766773ccf2b0c227a3761a40cd_searchf_2647b91766773ccf2b0c227a3761a40cd_targetf_3647b91766773ccf2b0c227a3761a40cd']['inspect'] = () => (Inspect_k426) => (Inspect_d419) => (Inspect_w412) => (Inspect_p405) => (__$a__ => `{ ` + `modules: ` + Inspect_p405.inspect()(__$a__.modules) + `, ` + `path: ` + Inspect_w412.inspect()(__$a__.path) + `, ` + `search: ` + Inspect_d419.inspect()(__$a__.search) + `, ` + `target: ` + Inspect_k426.inspect()(__$a__.target) + ` }`);
  let docJson = `{\n  \"modules\": [\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Test.mad\",\n      \"moduleName\": \"Test\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        {\n          \"js\": {\n            \"name\": \"AssertionError\",\n            \"params\": \"a\",\n            \"constructors\": [\n              \"AssertionError a a\",\n              \"Error a\",\n              \"ErrorWithMessage String\",\n              \"NotImplemented \"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"AssertionError\",\n            \"params\": \"a\",\n            \"constructors\": [\n              \"AssertionError a a\",\n              \"Error a\",\n              \"ErrorWithMessage String\",\n              \"NotImplemented \"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"assertEquals\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Eq a => a -> a -> Wish (AssertionError a) {}\"\n          },\n          \"llvm\": {\n            \"name\": \"assertEquals\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Eq a => a -> a -> Wish (AssertionError a) {}\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"test\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Inspect a => String -> (String -> Wish (AssertionError a) {}) -> Wish String String\"\n          },\n          \"llvm\": {\n            \"name\": \"test\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Inspect a => String -> (String -> Wish (AssertionError a) {}) -> Wish String String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"runAllTestSuites\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List #[String, List (Wish String String)] -> {}\"\n          },\n          \"llvm\": {\n            \"name\": \"runAllTestSuites\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List #[String, List (Wish String String)] -> {}\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Set.mad\",\n      \"moduleName\": \"Set\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"js\": {\n            \"name\": \"Functor\",\n            \"declaration\": \"Functor Set\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Functor\",\n            \"declaration\": \"Functor Set\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"fromList\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Comparable a => List a -> Set a\"\n          },\n          \"llvm\": {\n            \"name\": \"fromList\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Comparable a => List a -> Set a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"toList\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Set a -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"toList\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Set a -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"insert\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Comparable a => a -> Set a -> Set a\"\n          },\n          \"llvm\": {\n            \"name\": \"insert\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Comparable a => a -> Set a -> Set a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"filter\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Boolean) -> Set a -> Set a\"\n          },\n          \"llvm\": {\n            \"name\": \"filter\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Boolean) -> Set a -> Set a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"reduce\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> a) -> a -> Set b -> a\"\n          },\n          \"llvm\": {\n            \"name\": \"reduce\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> a) -> a -> Set b -> a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"merge\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Comparable a => Set a -> Set a -> Set a\"\n          },\n          \"llvm\": {\n            \"name\": \"merge\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Comparable a => Set a -> Set a -> Set a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"includes\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Eq a => a -> Set a -> Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"includes\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Eq a => a -> Set a -> Boolean\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/IOError.mad\",\n      \"moduleName\": \"IOError\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        {\n          \"js\": {\n            \"name\": \"IOError\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"ArgumentListToLong \",\n              \"PermissionDenied \",\n              \"AddressAlreadyInUse \",\n              \"UnknownError \"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"IOError\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"ArgumentListToLong \",\n              \"PermissionDenied \",\n              \"AddressAlreadyInUse \",\n              \"UnknownError \"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"fromLibuvError\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> IOError\"\n          },\n          \"llvm\": {\n            \"name\": \"fromLibuvError\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> IOError\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Wish.mad\",\n      \"moduleName\": \"Wish\",\n      \"description\": \"Wish represents an async computation that needs to be fulfilled. It is\\ncold and will only be run when fulfilled.\",\n      \"typeDeclarations\": [\n        {\n          \"js\": {\n            \"name\": \"Wish\",\n            \"params\": \"e a\",\n            \"constructors\": [\n              \"Wish ((e -> f) -> (a -> b) -> {})\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Wish\",\n            \"params\": \"e a\",\n            \"constructors\": [\n              \"Wish ((e -> f) -> (a -> b) -> {})\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"js\": {\n            \"name\": \"Functor\",\n            \"declaration\": \"Functor (Wish e)\",\n            \"constraints\": \"\",\n            \"description\": \"The functor instance of Wish provides a way to map over the value it contains.\",\n            \"example\": \"map((x) => x + 1, good(3)) // good(4)\\nmap((x) => x + 1, bad(3))  // bad(3)\",\n            \"since\": \"0.0.5\"\n          },\n          \"llvm\": {\n            \"name\": \"Functor\",\n            \"declaration\": \"Functor (Wish e)\",\n            \"constraints\": \"\",\n            \"description\": \"The functor instance of Wish provides a way to map over the value it contains.\",\n            \"example\": \"map((x) => x + 1, good(3)) // good(4)\\nmap((x) => x + 1, bad(3))  // bad(3)\",\n            \"since\": \"0.0.5\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Applicative\",\n            \"declaration\": \"Applicative (Wish e)\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Applicative\",\n            \"declaration\": \"Applicative (Wish e)\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Monad\",\n            \"declaration\": \"Monad (Wish e)\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Monad\",\n            \"declaration\": \"Monad (Wish e)\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Bifunctor\",\n            \"declaration\": \"Bifunctor Wish\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Bifunctor\",\n            \"declaration\": \"Bifunctor Wish\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"mapRej\",\n            \"description\": \"Maps over the rejected value.\",\n            \"example\": \"mapRej((x) => x + 1, bad(3))  // bad(4)\\nmapRej((x) => x + 1, good(3)) // good(3)\",\n            \"since\": \"0.0.5\",\n            \"type\": \"(a -> b) -> Wish a c -> Wish b c\"\n          },\n          \"llvm\": {\n            \"name\": \"mapRej\",\n            \"description\": \"Maps over the rejected value.\",\n            \"example\": \"mapRej((x) => x + 1, bad(3))  // bad(4)\\nmapRej((x) => x + 1, good(3)) // good(3)\",\n            \"since\": \"0.0.5\",\n            \"type\": \"(a -> b) -> Wish a c -> Wish b c\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"chainRej\",\n            \"description\": \"Chains over the rejected value.\",\n            \"example\": \"chainRej((x) => good(x + 1), bad(3)) // good(4)\",\n            \"since\": \"0.0.5\",\n            \"type\": \"(a -> Wish b c) -> Wish a c -> Wish b c\"\n          },\n          \"llvm\": {\n            \"name\": \"chainRej\",\n            \"description\": \"Chains over the rejected value.\",\n            \"example\": \"chainRej((x) => good(x + 1), bad(3)) // good(4)\",\n            \"since\": \"0.0.5\",\n            \"type\": \"(a -> Wish b c) -> Wish a c -> Wish b c\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"bichain\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Wish b c) -> (d -> Wish b c) -> Wish a d -> Wish b c\"\n          },\n          \"llvm\": {\n            \"name\": \"bichain\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Wish b c) -> (d -> Wish b c) -> Wish a d -> Wish b c\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"good\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> Wish b a\"\n          },\n          \"llvm\": {\n            \"name\": \"good\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> Wish b a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"bad\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> Wish a b\"\n          },\n          \"llvm\": {\n            \"name\": \"bad\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> Wish a b\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"parallel\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List (Wish a b) -> Wish a (List b)\"\n          },\n          \"llvm\": {\n            \"name\": \"parallel\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List (Wish a b) -> Wish a (List b)\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"discardError\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b) -> Wish a b -> Wish {} b\"\n          },\n          \"llvm\": {\n            \"name\": \"discardError\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b) -> Wish a b -> Wish {} b\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"fulfill\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b) -> (c -> d) -> Wish a c -> {}\"\n          },\n          \"llvm\": {\n            \"name\": \"fulfill\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b) -> (c -> d) -> Wish a c -> {}\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"after\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> a -> Wish b a\"\n          },\n          \"llvm\": {\n            \"name\": \"after\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> a -> Wish b a\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Dictionary.mad\",\n      \"moduleName\": \"Dictionary\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"js\": {\n            \"name\": \"Functor\",\n            \"declaration\": \"Functor (Dictionary k)\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Functor\",\n            \"declaration\": \"Functor (Dictionary k)\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"fromList\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Comparable a => List #[a, b] -> Dictionary a b\"\n          },\n          \"llvm\": {\n            \"name\": \"fromList\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Comparable a => List #[a, b] -> Dictionary a b\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"empty\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Dictionary a b\"\n          },\n          \"llvm\": {\n            \"name\": \"empty\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Dictionary a b\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"insert\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Comparable a => a -> b -> Dictionary a b -> Dictionary a b\"\n          },\n          \"llvm\": {\n            \"name\": \"insert\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Comparable a => a -> b -> Dictionary a b -> Dictionary a b\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"get\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Comparable a => a -> Dictionary a b -> Maybe b\"\n          },\n          \"llvm\": {\n            \"name\": \"get\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Comparable a => a -> Dictionary a b -> Maybe b\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"merge\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Comparable a => Dictionary a b -> Dictionary a b -> Dictionary a b\"\n          },\n          \"llvm\": {\n            \"name\": \"merge\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Comparable a => Dictionary a b -> Dictionary a b -> Dictionary a b\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"length\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Dictionary a b -> Integer\"\n          },\n          \"llvm\": {\n            \"name\": \"length\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Dictionary a b -> Integer\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"toList\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Dictionary a b -> List #[a, b]\"\n          },\n          \"llvm\": {\n            \"name\": \"toList\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Dictionary a b -> List #[a, b]\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"mapM\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Applicative m => (a -> m b) -> Dictionary c a -> m (Dictionary c b)\"\n          },\n          \"llvm\": {\n            \"name\": \"mapM\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Applicative m => (a -> m b) -> Dictionary c a -> m (Dictionary c b)\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"mapWithKey\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c) -> Dictionary a b -> Dictionary a c\"\n          },\n          \"llvm\": {\n            \"name\": \"mapWithKey\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c) -> Dictionary a b -> Dictionary a c\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"keys\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Dictionary a b -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"keys\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Dictionary a b -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"values\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Dictionary a b -> List b\"\n          },\n          \"llvm\": {\n            \"name\": \"values\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Dictionary a b -> List b\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Http.mad\",\n      \"moduleName\": \"Http\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        {\n          \"js\": {\n            \"name\": \"TransferEncoding\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"Chunked \",\n              \"Compress \",\n              \"Deflate \",\n              \"Gzip \"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"TransferEncoding\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"Chunked \",\n              \"Compress \",\n              \"Deflate \",\n              \"Gzip \"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Header\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"Header String String\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Header\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"Header String String\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Method\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"CONNECT \",\n              \"DELETE \",\n              \"GET \",\n              \"HEAD \",\n              \"OPTIONS \",\n              \"PATCH \",\n              \"POST \",\n              \"PUT \",\n              \"TRACE \"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Method\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"CONNECT \",\n              \"DELETE \",\n              \"GET \",\n              \"HEAD \",\n              \"OPTIONS \",\n              \"PATCH \",\n              \"POST \",\n              \"PUT \",\n              \"TRACE \"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"ClientError\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"AccessDenied \",\n              \"AddressNotFound \",\n              \"BadTransferEncoding \",\n              \"BadUrl String\",\n              \"ConnectionFailed \",\n              \"Http2FramingError \",\n              \"IncompleteResponse \",\n              \"InternalError \",\n              \"InvalidSSLCertificate \",\n              \"MalformedResponse \",\n              \"NotSupported \",\n              \"SSLConnectionFailed \",\n              \"SSLEngineNotFound \",\n              \"SSLInitializationFailed \",\n              \"Timeout \",\n              \"TooManyRedirects \",\n              \"UnresolvedProxy \",\n              \"UnsupportedProtocol \"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"ClientError\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"AccessDenied \",\n              \"AddressNotFound \",\n              \"BadTransferEncoding \",\n              \"BadUrl String\",\n              \"ConnectionFailed \",\n              \"Http2FramingError \",\n              \"IncompleteResponse \",\n              \"InternalError \",\n              \"InvalidSSLCertificate \",\n              \"MalformedResponse \",\n              \"NotSupported \",\n              \"SSLConnectionFailed \",\n              \"SSLEngineNotFound \",\n              \"SSLInitializationFailed \",\n              \"Timeout \",\n              \"TooManyRedirects \",\n              \"UnresolvedProxy \",\n              \"UnsupportedProtocol \"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Error\",\n            \"params\": \"a\",\n            \"constructors\": [\n              \"BadResponse (Response a)\",\n              \"ClientError ClientError\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Error\",\n            \"params\": \"a\",\n            \"constructors\": [\n              \"BadResponse (Response a)\",\n              \"ClientError ClientError\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"aliases\": [\n        {\n          \"js\": {\n            \"name\": \"Status\",\n            \"params\": \"\",\n            \"aliasedType\": \"Integer\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Status\",\n            \"params\": \"\",\n            \"aliasedType\": \"Integer\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Response\",\n            \"params\": \"a\",\n            \"aliasedType\": \"{ body :: a, headers :: List Header, status :: Status }\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Response\",\n            \"params\": \"a\",\n            \"aliasedType\": \"{ body :: a, headers :: List Header, status :: Status }\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Request\",\n            \"params\": \"a\",\n            \"aliasedType\": \"{ body :: Maybe a, headers :: List Header, method :: Method, url :: String }\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Request\",\n            \"params\": \"a\",\n            \"aliasedType\": \"{ body :: Maybe a, headers :: List Header, method :: Method, url :: String }\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"BadRequest\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer\"\n          },\n          \"llvm\": {\n            \"name\": \"BadRequest\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Unauthorized\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer\"\n          },\n          \"llvm\": {\n            \"name\": \"Unauthorized\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"PaymentRequired\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer\"\n          },\n          \"llvm\": {\n            \"name\": \"PaymentRequired\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"NotFound\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer\"\n          },\n          \"llvm\": {\n            \"name\": \"NotFound\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"OK\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer\"\n          },\n          \"llvm\": {\n            \"name\": \"OK\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"getHeader\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> { body :: a, headers :: List Header, status :: Integer } -> List Header\"\n          },\n          \"llvm\": {\n            \"name\": \"getHeader\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> { body :: a, headers :: List Header, status :: Integer } -> List Header\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"methodStr\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Method -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"methodStr\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Method -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"request\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"{ body :: Maybe String, headers :: List Header, method :: Method, url :: String } -> Wish (Error String) { body :: String, headers :: List Header, status :: Integer }\"\n          },\n          \"llvm\": {\n            \"name\": \"request\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"{ body :: Maybe String, headers :: List Header, method :: Method, url :: String } -> Wish (Error String) { body :: String, headers :: List Header, status :: Integer }\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"requestBytes\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"{ body :: Maybe ByteArray, headers :: List Header, method :: Method, url :: String } -> Wish (Error ByteArray) { body :: ByteArray, headers :: List Header, status :: Integer }\"\n          },\n          \"llvm\": {\n            \"name\": \"requestBytes\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"{ body :: Maybe ByteArray, headers :: List Header, method :: Method, url :: String } -> Wish (Error ByteArray) { body :: ByteArray, headers :: List Header, status :: Integer }\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"get\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Wish (Error String) { body :: String, headers :: List Header, status :: Integer }\"\n          },\n          \"llvm\": {\n            \"name\": \"get\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Wish (Error String) { body :: String, headers :: List Header, status :: Integer }\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"getBytes\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Wish (Error ByteArray) { body :: ByteArray, headers :: List Header, status :: Integer }\"\n          },\n          \"llvm\": {\n            \"name\": \"getBytes\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Wish (Error ByteArray) { body :: ByteArray, headers :: List Header, status :: Integer }\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"getWithHeaders\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> List Header -> Wish (Error String) { body :: String, headers :: List Header, status :: Integer }\"\n          },\n          \"llvm\": {\n            \"name\": \"getWithHeaders\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> List Header -> Wish (Error String) { body :: String, headers :: List Header, status :: Integer }\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"getBytesWithHeaders\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> List Header -> Wish (Error ByteArray) { body :: ByteArray, headers :: List Header, status :: Integer }\"\n          },\n          \"llvm\": {\n            \"name\": \"getBytesWithHeaders\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> List Header -> Wish (Error ByteArray) { body :: ByteArray, headers :: List Header, status :: Integer }\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"post\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String -> Wish (Error String) { body :: String, headers :: List Header, status :: Integer }\"\n          },\n          \"llvm\": {\n            \"name\": \"post\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String -> Wish (Error String) { body :: String, headers :: List Header, status :: Integer }\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"postBytes\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> ByteArray -> Wish (Error ByteArray) { body :: ByteArray, headers :: List Header, status :: Integer }\"\n          },\n          \"llvm\": {\n            \"name\": \"postBytes\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> ByteArray -> Wish (Error ByteArray) { body :: ByteArray, headers :: List Header, status :: Integer }\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"postWithHeaders\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String -> List Header -> Wish (Error String) { body :: String, headers :: List Header, status :: Integer }\"\n          },\n          \"llvm\": {\n            \"name\": \"postWithHeaders\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String -> List Header -> Wish (Error String) { body :: String, headers :: List Header, status :: Integer }\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"postBytesWithHeaders\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> ByteArray -> List Header -> Wish (Error ByteArray) { body :: ByteArray, headers :: List Header, status :: Integer }\"\n          },\n          \"llvm\": {\n            \"name\": \"postBytesWithHeaders\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> ByteArray -> List Header -> Wish (Error ByteArray) { body :: ByteArray, headers :: List Header, status :: Integer }\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"put\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String -> Wish (Error String) { body :: String, headers :: List Header, status :: Integer }\"\n          },\n          \"llvm\": {\n            \"name\": \"put\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String -> Wish (Error String) { body :: String, headers :: List Header, status :: Integer }\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"putBytes\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> ByteArray -> Wish (Error ByteArray) { body :: ByteArray, headers :: List Header, status :: Integer }\"\n          },\n          \"llvm\": {\n            \"name\": \"putBytes\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> ByteArray -> Wish (Error ByteArray) { body :: ByteArray, headers :: List Header, status :: Integer }\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"putWithHeaders\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String -> List Header -> Wish (Error String) { body :: String, headers :: List Header, status :: Integer }\"\n          },\n          \"llvm\": {\n            \"name\": \"putWithHeaders\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String -> List Header -> Wish (Error String) { body :: String, headers :: List Header, status :: Integer }\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"putBytesWithHeaders\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> ByteArray -> List Header -> Wish (Error ByteArray) { body :: ByteArray, headers :: List Header, status :: Integer }\"\n          },\n          \"llvm\": {\n            \"name\": \"putBytesWithHeaders\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> ByteArray -> List Header -> Wish (Error ByteArray) { body :: ByteArray, headers :: List Header, status :: Integer }\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"deleteBytes\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> ByteArray -> Wish (Error ByteArray) { body :: ByteArray, headers :: List Header, status :: Integer }\"\n          },\n          \"llvm\": {\n            \"name\": \"deleteBytes\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> ByteArray -> Wish (Error ByteArray) { body :: ByteArray, headers :: List Header, status :: Integer }\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"deleteWithHeaders\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String -> List Header -> Wish (Error String) { body :: String, headers :: List Header, status :: Integer }\"\n          },\n          \"llvm\": {\n            \"name\": \"deleteWithHeaders\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String -> List Header -> Wish (Error String) { body :: String, headers :: List Header, status :: Integer }\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"deleteBytesWithHeaders\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> ByteArray -> List Header -> Wish (Error ByteArray) { body :: ByteArray, headers :: List Header, status :: Integer }\"\n          },\n          \"llvm\": {\n            \"name\": \"deleteBytesWithHeaders\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> ByteArray -> List Header -> Wish (Error ByteArray) { body :: ByteArray, headers :: List Header, status :: Integer }\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/File.mad\",\n      \"moduleName\": \"File\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"write\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String -> Wish IOError {}\"\n          },\n          \"llvm\": {\n            \"name\": \"write\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String -> Wish IOError {}\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"writeBytes\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> ByteArray -> Wish IOError {}\"\n          },\n          \"llvm\": {\n            \"name\": \"writeBytes\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> ByteArray -> Wish IOError {}\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"read\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Wish IOError String\"\n          },\n          \"llvm\": {\n            \"name\": \"read\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Wish IOError String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"readBytes\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Wish IOError ByteArray\"\n          },\n          \"llvm\": {\n            \"name\": \"readBytes\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Wish IOError ByteArray\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"exists\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Wish {} Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"exists\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Wish {} Boolean\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Date.mad\",\n      \"moduleName\": \"Date\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        {\n          \"js\": {\n            \"name\": \"DateTime\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"DateTime Integer\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"DateTime\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"DateTime Integer\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"LocalDateTime\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"LocalDateTime Integer\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"LocalDateTime\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"LocalDateTime Integer\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"TimeUnit\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"Millisecond \",\n              \"Second \",\n              \"Minute \",\n              \"Hour \",\n              \"Day \",\n              \"Month \",\n              \"Year \"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"TimeUnit\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"Millisecond \",\n              \"Second \",\n              \"Minute \",\n              \"Hour \",\n              \"Day \",\n              \"Month \",\n              \"Year \"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"aliases\": [\n        {\n          \"js\": {\n            \"name\": \"DateInfo\",\n            \"params\": \"\",\n            \"aliasedType\": \"{ day :: Integer, hours :: Integer, milliseconds :: Integer, minutes :: Integer, month :: Integer, seconds :: Integer, year :: Integer }\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"DateInfo\",\n            \"params\": \"\",\n            \"aliasedType\": \"{ day :: Integer, hours :: Integer, milliseconds :: Integer, minutes :: Integer, month :: Integer, seconds :: Integer, year :: Integer }\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"now\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"{} -> DateTime\"\n          },\n          \"llvm\": {\n            \"name\": \"now\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"{} -> DateTime\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"toISOString\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"DateTime -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"toISOString\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"DateTime -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"getTimestamp\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"DateTime -> Integer\"\n          },\n          \"llvm\": {\n            \"name\": \"getTimestamp\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"DateTime -> Integer\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"toDateInfo\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"DateTime -> { day :: Integer, hours :: Integer, milliseconds :: Integer, minutes :: Integer, month :: Integer, seconds :: Integer, year :: Integer }\"\n          },\n          \"llvm\": {\n            \"name\": \"toDateInfo\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"DateTime -> { day :: Integer, hours :: Integer, milliseconds :: Integer, minutes :: Integer, month :: Integer, seconds :: Integer, year :: Integer }\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"fromDateInfo\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"{ day :: Integer, hours :: Integer, milliseconds :: Integer, minutes :: Integer, month :: Integer, seconds :: Integer, year :: Integer } -> DateTime\"\n          },\n          \"llvm\": {\n            \"name\": \"fromDateInfo\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"{ day :: Integer, hours :: Integer, milliseconds :: Integer, minutes :: Integer, month :: Integer, seconds :: Integer, year :: Integer } -> DateTime\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"add\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> TimeUnit -> DateTime -> DateTime\"\n          },\n          \"llvm\": {\n            \"name\": \"add\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> TimeUnit -> DateTime -> DateTime\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"addMilliseconds\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> DateTime -> DateTime\"\n          },\n          \"llvm\": {\n            \"name\": \"addMilliseconds\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> DateTime -> DateTime\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"addSeconds\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> DateTime -> DateTime\"\n          },\n          \"llvm\": {\n            \"name\": \"addSeconds\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> DateTime -> DateTime\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"addMinutes\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> DateTime -> DateTime\"\n          },\n          \"llvm\": {\n            \"name\": \"addMinutes\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> DateTime -> DateTime\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"addHours\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> DateTime -> DateTime\"\n          },\n          \"llvm\": {\n            \"name\": \"addHours\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> DateTime -> DateTime\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"addDays\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> DateTime -> DateTime\"\n          },\n          \"llvm\": {\n            \"name\": \"addDays\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> DateTime -> DateTime\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"addMonths\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> DateTime -> DateTime\"\n          },\n          \"llvm\": {\n            \"name\": \"addMonths\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> DateTime -> DateTime\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"addYears\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> DateTime -> DateTime\"\n          },\n          \"llvm\": {\n            \"name\": \"addYears\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> DateTime -> DateTime\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Crypto.mad\",\n      \"moduleName\": \"Crypto\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"md5\",\n            \"description\": \"Hash the input String with md5 algorithm\",\n            \"example\": \"md5(\\\"rabbit\\\") // \\\"a51e47f646375ab6bf5dd2c42d3e6181\\\"\",\n            \"since\": \"0.13.0\",\n            \"type\": \"String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"md5\",\n            \"description\": \"Hash the input String with md5 algorithm\",\n            \"example\": \"md5(\\\"rabbit\\\") // \\\"a51e47f646375ab6bf5dd2c42d3e6181\\\"\",\n            \"since\": \"0.13.0\",\n            \"type\": \"String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"sha256\",\n            \"description\": \"Hash the input String with sha256 algorithm\",\n            \"example\": \"md5(\\\"rabbit\\\") // \\\"d37d96b42ad43384915e4513505c30c0b1c4e7c765b5577eda25b5dbd7f26d89\\\"\",\n            \"since\": \"0.13.0\",\n            \"type\": \"String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"sha256\",\n            \"description\": \"Hash the input String with sha256 algorithm\",\n            \"example\": \"md5(\\\"rabbit\\\") // \\\"d37d96b42ad43384915e4513505c30c0b1c4e7c765b5577eda25b5dbd7f26d89\\\"\",\n            \"since\": \"0.13.0\",\n            \"type\": \"String -> String\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/PrettyPrint.mad\",\n      \"moduleName\": \"PrettyPrint\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        {\n          \"js\": {\n            \"name\": \"Doc\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"EmptyDoc \",\n              \"CharDoc Char\",\n              \"TextDoc Integer String\",\n              \"LineDoc Boolean\",\n              \"CatDoc Doc Doc\",\n              \"NestDoc Integer Doc\",\n              \"UnionDoc Doc Doc\",\n              \"ColumnDoc (Integer -> Doc)\",\n              \"NestingDoc (Integer -> Doc)\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Doc\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"EmptyDoc \",\n              \"CharDoc Char\",\n              \"TextDoc Integer String\",\n              \"LineDoc Boolean\",\n              \"CatDoc Doc Doc\",\n              \"NestDoc Integer Doc\",\n              \"UnionDoc Doc Doc\",\n              \"ColumnDoc (Integer -> Doc)\",\n              \"NestingDoc (Integer -> Doc)\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"SimpleDoc\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"SEmpty \",\n              \"SChar Char SimpleDoc\",\n              \"SText Integer String SimpleDoc\",\n              \"SLine Integer SimpleDoc\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"SimpleDoc\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"SEmpty \",\n              \"SChar Char SimpleDoc\",\n              \"SText Integer String SimpleDoc\",\n              \"SLine Integer SimpleDoc\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"empty\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"empty\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"line\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"line\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"linebreak\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"linebreak\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"char\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Char -> Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"char\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Char -> Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"space\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"space\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"colon\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"colon\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"comma\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"comma\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"dot\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"dot\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"quote\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"quote\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"lbracket\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"lbracket\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"rbracket\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"rbracket\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"lbrace\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"lbrace\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"rbrace\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"rbrace\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"quotes\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc -> Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"quotes\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc -> Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"brackets\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc -> Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"brackets\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc -> Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"braces\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc -> Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"braces\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc -> Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"text\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"text\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"group\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc -> Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"group\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc -> Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"flatten\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc -> Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"flatten\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc -> Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"softline\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"softline\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"softbreak\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"softbreak\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"enclose\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc -> Doc -> Doc -> Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"enclose\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc -> Doc -> Doc -> Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"beside\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc -> Doc -> Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"beside\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc -> Doc -> Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"hcat\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List Doc -> Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"hcat\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List Doc -> Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"sepBy\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc -> List Doc -> Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"sepBy\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Doc -> List Doc -> Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"nest\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> Doc -> Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"nest\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> Doc -> Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"column\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(Integer -> Doc) -> Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"column\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(Integer -> Doc) -> Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"nesting\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(Integer -> Doc) -> Doc\"\n          },\n          \"llvm\": {\n            \"name\": \"nesting\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(Integer -> Doc) -> Doc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"renderPretty\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Float -> Integer -> Doc -> SimpleDoc\"\n          },\n          \"llvm\": {\n            \"name\": \"renderPretty\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Float -> Integer -> Doc -> SimpleDoc\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"prettyPrint\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> Doc -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"prettyPrint\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> Doc -> String\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Monad.mad\",\n      \"moduleName\": \"Monad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        {\n          \"js\": {\n            \"name\": \"Monad\",\n            \"vars\": \"m\",\n            \"constraints\": \"Applicative m\",\n            \"methods\": [\n              \"chain :: (a -> m b) -> m a -> m b\",\n              \"of :: a -> m a\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"0.0.8\"\n          },\n          \"llvm\": {\n            \"name\": \"Monad\",\n            \"vars\": \"m\",\n            \"constraints\": \"Applicative m\",\n            \"methods\": [\n              \"chain :: (a -> m b) -> m a -> m b\",\n              \"of :: a -> m a\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"0.0.8\"\n          }\n        }\n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"chain2\",\n            \"description\": \"chains two monads\",\n            \"example\": \"chain2((a, b) => of(a + b), Just(1), Just(3)) // Just(4)\",\n            \"since\": \"0.0.8\",\n            \"type\": \"Monad m => (a -> b -> m c) -> m a -> m b -> m c\"\n          },\n          \"llvm\": {\n            \"name\": \"chain2\",\n            \"description\": \"chains two monads\",\n            \"example\": \"chain2((a, b) => of(a + b), Just(1), Just(3)) // Just(4)\",\n            \"since\": \"0.0.8\",\n            \"type\": \"Monad m => (a -> b -> m c) -> m a -> m b -> m c\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"andDo\",\n            \"description\": \"chains two monads and returns the first one, discarding the\\nvalue from the second one.\",\n            \"example\": \"andDo(Just(3), Just(4)) // Just(3)\",\n            \"since\": \"0.0.8\",\n            \"type\": \"Monad m => m a -> m b -> m a\"\n          },\n          \"llvm\": {\n            \"name\": \"andDo\",\n            \"description\": \"chains two monads and returns the first one, discarding the\\nvalue from the second one.\",\n            \"example\": \"andDo(Just(3), Just(4)) // Just(3)\",\n            \"since\": \"0.0.8\",\n            \"type\": \"Monad m => m a -> m b -> m a\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Boolean.mad\",\n      \"moduleName\": \"Boolean\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"js\": {\n            \"name\": \"Comparable\",\n            \"declaration\": \"Comparable Boolean\",\n            \"constraints\": \"\",\n            \"description\": \"Comparable implementation for booleans\",\n            \"example\": \"compare(true, false) // MORE\\ncompare(false, true) // LESS\\ncompare(true, true) // EQUAL\\ncompare(false, false) // EQUAL\",\n            \"since\": \"0.11.0\"\n          },\n          \"llvm\": {\n            \"name\": \"Comparable\",\n            \"declaration\": \"Comparable Boolean\",\n            \"constraints\": \"\",\n            \"description\": \"Comparable implementation for booleans\",\n            \"example\": \"compare(true, false) // MORE\\ncompare(false, true) // LESS\\ncompare(true, true) // EQUAL\\ncompare(false, false) // EQUAL\",\n            \"since\": \"0.11.0\"\n          }\n        }\n      ],\n      \"expressions\": [\n        \n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/JsonParser.mad\",\n      \"moduleName\": \"JsonParser\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        {\n          \"js\": {\n            \"name\": \"Parser\",\n            \"params\": \"r\",\n            \"constructors\": [\n              \"Parser (JsonValue -> Either String r)\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Parser\",\n            \"params\": \"r\",\n            \"constructors\": [\n              \"Parser (JsonValue -> Either String r)\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"js\": {\n            \"name\": \"Functor\",\n            \"declaration\": \"Functor Parser\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Functor\",\n            \"declaration\": \"Functor Parser\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Applicative\",\n            \"declaration\": \"Applicative Parser\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Applicative\",\n            \"declaration\": \"Applicative Parser\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Monad\",\n            \"declaration\": \"Monad Parser\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Monad\",\n            \"declaration\": \"Monad Parser\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"parse\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Parser a -> String -> Either String a\"\n          },\n          \"llvm\": {\n            \"name\": \"parse\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Parser a -> String -> Either String a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"succeed\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> Parser a\"\n          },\n          \"llvm\": {\n            \"name\": \"succeed\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> Parser a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"fail\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Parser a\"\n          },\n          \"llvm\": {\n            \"name\": \"fail\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Parser a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"string\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Parser String\"\n          },\n          \"llvm\": {\n            \"name\": \"string\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Parser String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"integer\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Parser Integer\"\n          },\n          \"llvm\": {\n            \"name\": \"integer\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Parser Integer\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"float\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Parser Float\"\n          },\n          \"llvm\": {\n            \"name\": \"float\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Parser Float\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"boolean\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Parser Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"boolean\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Parser Boolean\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"list\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Parser a -> Parser (List a)\"\n          },\n          \"llvm\": {\n            \"name\": \"list\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Parser a -> Parser (List a)\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"dict\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Parser a -> Parser (Dictionary String a)\"\n          },\n          \"llvm\": {\n            \"name\": \"dict\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Parser a -> Parser (Dictionary String a)\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"maybe\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Parser a -> Parser (Maybe a)\"\n          },\n          \"llvm\": {\n            \"name\": \"maybe\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Parser a -> Parser (Maybe a)\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"lazy\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"({} -> Parser a) -> Parser a\"\n          },\n          \"llvm\": {\n            \"name\": \"lazy\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"({} -> Parser a) -> Parser a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"field\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Parser a -> Parser a\"\n          },\n          \"llvm\": {\n            \"name\": \"field\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Parser a -> Parser a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"path\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List String -> Parser a -> Parser a\"\n          },\n          \"llvm\": {\n            \"name\": \"path\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List String -> Parser a -> Parser a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"chain1\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Parser b) -> Parser a -> Parser b\"\n          },\n          \"llvm\": {\n            \"name\": \"chain1\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Parser b) -> Parser a -> Parser b\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"chain2\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> Parser c) -> Parser a -> Parser b -> Parser c\"\n          },\n          \"llvm\": {\n            \"name\": \"chain2\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> Parser c) -> Parser a -> Parser b -> Parser c\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"chain3\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c -> Parser d) -> Parser a -> Parser b -> Parser c -> Parser d\"\n          },\n          \"llvm\": {\n            \"name\": \"chain3\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c -> Parser d) -> Parser a -> Parser b -> Parser c -> Parser d\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"chain4\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c -> d -> Parser e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e\"\n          },\n          \"llvm\": {\n            \"name\": \"chain4\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c -> d -> Parser e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"chain5\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c -> d -> e -> Parser f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f\"\n          },\n          \"llvm\": {\n            \"name\": \"chain5\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c -> d -> e -> Parser f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"chain6\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c -> d -> e -> f -> Parser g) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g\"\n          },\n          \"llvm\": {\n            \"name\": \"chain6\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c -> d -> e -> f -> Parser g) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"chain7\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c -> d -> e -> f -> g -> Parser h) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser h\"\n          },\n          \"llvm\": {\n            \"name\": \"chain7\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c -> d -> e -> f -> g -> Parser h) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser h\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"chain8\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c -> d -> e -> f -> g -> h -> Parser i) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser h -> Parser i\"\n          },\n          \"llvm\": {\n            \"name\": \"chain8\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c -> d -> e -> f -> g -> h -> Parser i) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser h -> Parser i\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"map1\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b) -> Parser a -> Parser b\"\n          },\n          \"llvm\": {\n            \"name\": \"map1\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b) -> Parser a -> Parser b\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"map2\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c) -> Parser a -> Parser b -> Parser c\"\n          },\n          \"llvm\": {\n            \"name\": \"map2\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c) -> Parser a -> Parser b -> Parser c\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"map3\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d\"\n          },\n          \"llvm\": {\n            \"name\": \"map3\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"map4\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e\"\n          },\n          \"llvm\": {\n            \"name\": \"map4\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"map5\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f\"\n          },\n          \"llvm\": {\n            \"name\": \"map5\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"map6\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c -> d -> e -> f -> g) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g\"\n          },\n          \"llvm\": {\n            \"name\": \"map6\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c -> d -> e -> f -> g) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"map7\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c -> d -> e -> f -> g -> h) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser h\"\n          },\n          \"llvm\": {\n            \"name\": \"map7\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c -> d -> e -> f -> g -> h) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser h\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"map8\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c -> d -> e -> f -> g -> h -> i) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser h -> Parser i\"\n          },\n          \"llvm\": {\n            \"name\": \"map8\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> c -> d -> e -> f -> g -> h -> i) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser h -> Parser i\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Functor.mad\",\n      \"moduleName\": \"Functor\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        {\n          \"js\": {\n            \"name\": \"Functor\",\n            \"vars\": \"m\",\n            \"constraints\": \"\",\n            \"methods\": [\n              \"map :: (a -> b) -> m a -> m b\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"0.0.8\"\n          },\n          \"llvm\": {\n            \"name\": \"Functor\",\n            \"vars\": \"m\",\n            \"constraints\": \"\",\n            \"methods\": [\n              \"map :: (a -> b) -> m a -> m b\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"0.0.8\"\n          }\n        }\n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"mapL\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Functor m => a -> m b -> m a\"\n          },\n          \"llvm\": {\n            \"name\": \"mapL\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Functor m => a -> m b -> m a\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Bifunctor.mad\",\n      \"moduleName\": \"Bifunctor\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        {\n          \"js\": {\n            \"name\": \"Bifunctor\",\n            \"vars\": \"f\",\n            \"constraints\": \"\",\n            \"methods\": [\n              \"bimap :: (a -> b) -> (c -> d) -> f a c -> f b d\",\n              \"mapFirst :: (a -> b) -> f a c -> f b c\",\n              \"mapSecond :: (c -> d) -> f a c -> f a d\"\n            ],\n            \"description\": \"A bifunctor is a type constructor that takes two type arguments and is a functor in both\\narguments. That is, unlike with Functor, a type constructor such as Either does not need\\nto be partially applied for a Bifunctor instance, and the methods in this class permit\\nmapping functions over the Left value or the Right value, or both at the same time.\",\n            \"example\": \"\",\n            \"since\": \"0.11.0\"\n          },\n          \"llvm\": {\n            \"name\": \"Bifunctor\",\n            \"vars\": \"f\",\n            \"constraints\": \"\",\n            \"methods\": [\n              \"bimap :: (a -> b) -> (c -> d) -> f a c -> f b d\",\n              \"mapFirst :: (a -> b) -> f a c -> f b c\",\n              \"mapSecond :: (c -> d) -> f a c -> f a d\"\n            ],\n            \"description\": \"A bifunctor is a type constructor that takes two type arguments and is a functor in both\\narguments. That is, unlike with Functor, a type constructor such as Either does not need\\nto be partially applied for a Bifunctor instance, and the methods in this class permit\\nmapping functions over the Left value or the Right value, or both at the same time.\",\n            \"example\": \"\",\n            \"since\": \"0.11.0\"\n          }\n        }\n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        \n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Function.mad\",\n      \"moduleName\": \"Function\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"complement\",\n            \"description\": \"Wraps a predicate function and make it return the inverse. So if for a value\\nthe function would normally return true, after \\\"complemented\\\" it would return\\nfalse and vice versa.\",\n            \"example\": \"complement((x) => x % 2 == 0)(2) // false\",\n            \"since\": \"0.0.5\",\n            \"type\": \"(a -> Boolean) -> a -> Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"complement\",\n            \"description\": \"Wraps a predicate function and make it return the inverse. So if for a value\\nthe function would normally return true, after \\\"complemented\\\" it would return\\nfalse and vice versa.\",\n            \"example\": \"complement((x) => x % 2 == 0)(2) // false\",\n            \"since\": \"0.0.5\",\n            \"type\": \"(a -> Boolean) -> a -> Boolean\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"always\",\n            \"description\": \"It always returns the first supplied parameter no matter what. It is especially\\npractical during function composition when you want to discard whatever value\\nis passed to a function and always return the same thing.\",\n            \"example\": \"always(true, \\\"1\\\")            // true\\nmap(always(true), [1, 2 ,3]) // [true, true, true]\",\n            \"since\": \"0.0.5\",\n            \"type\": \"a -> b -> a\"\n          },\n          \"llvm\": {\n            \"name\": \"always\",\n            \"description\": \"It always returns the first supplied parameter no matter what. It is especially\\npractical during function composition when you want to discard whatever value\\nis passed to a function and always return the same thing.\",\n            \"example\": \"always(true, \\\"1\\\")            // true\\nmap(always(true), [1, 2 ,3]) // [true, true, true]\",\n            \"since\": \"0.0.5\",\n            \"type\": \"a -> b -> a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"identity\",\n            \"description\": \"Returns exactly what you give it.\",\n            \"example\": \"identity(4)       // 4\\nidentity(Just(3)) // Just(3)\",\n            \"since\": \"0.0.5\",\n            \"type\": \"a -> a\"\n          },\n          \"llvm\": {\n            \"name\": \"identity\",\n            \"description\": \"Returns exactly what you give it.\",\n            \"example\": \"identity(4)       // 4\\nidentity(Just(3)) // Just(3)\",\n            \"since\": \"0.0.5\",\n            \"type\": \"a -> a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"equals\",\n            \"description\": \"Compares two values and returns true if they are equal, false otherwise.\",\n            \"example\": \"equals(1, 1)                 // true\\nequals([1, 2, 3], [1, 2, 3]) // true\\nequals(Just(1), Nothing)     // false\",\n            \"since\": \"0.0.5\",\n            \"type\": \"Eq a => a -> a -> Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"equals\",\n            \"description\": \"Compares two values and returns true if they are equal, false otherwise.\",\n            \"example\": \"equals(1, 1)                 // true\\nequals([1, 2, 3], [1, 2, 3]) // true\\nequals(Just(1), Nothing)     // false\",\n            \"since\": \"0.0.5\",\n            \"type\": \"Eq a => a -> a -> Boolean\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"notEquals\",\n            \"description\": \"Complement of equals, it returns false if the values are equal, true otherwise.\",\n            \"example\": \"equals(1, 1)                 // false\\nequals([1, 2, 3], [1, 2, 3]) // false\\nequals(Just(1), Nothing)     // true\",\n            \"since\": \"0.7.0\",\n            \"type\": \"Eq a => a -> a -> Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"notEquals\",\n            \"description\": \"Complement of equals, it returns false if the values are equal, true otherwise.\",\n            \"example\": \"equals(1, 1)                 // false\\nequals([1, 2, 3], [1, 2, 3]) // false\\nequals(Just(1), Nothing)     // true\",\n            \"since\": \"0.7.0\",\n            \"type\": \"Eq a => a -> a -> Boolean\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"ifElse\",\n            \"description\": \"It models branching and allows to run different transformation based\\non the given predicate. The first parameter is the predicate, the second one\\nis the function run when the predicate returns true, and the third parameter\\nis the function run when the predicate returns false.\",\n            \"example\": \"ifElse(equals(4), (x) => x + 1, (x) => x * 2, 4) // 5\\nifElse(equals(4), (x) => x + 1, (x) => x * 2, 5) // 10\",\n            \"since\": \"0.0.5\",\n            \"type\": \"(a -> Boolean) -> (a -> b) -> (a -> b) -> a -> b\"\n          },\n          \"llvm\": {\n            \"name\": \"ifElse\",\n            \"description\": \"It models branching and allows to run different transformation based\\non the given predicate. The first parameter is the predicate, the second one\\nis the function run when the predicate returns true, and the third parameter\\nis the function run when the predicate returns false.\",\n            \"example\": \"ifElse(equals(4), (x) => x + 1, (x) => x * 2, 4) // 5\\nifElse(equals(4), (x) => x + 1, (x) => x * 2, 5) // 10\",\n            \"since\": \"0.0.5\",\n            \"type\": \"(a -> Boolean) -> (a -> b) -> (a -> b) -> a -> b\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"when\",\n            \"description\": \"It runs a transformation only if the predicate returns true, otherwise it\\nreturns the initial value.\",\n            \"example\": \"when(equals(4), (x) => x * 2, 4) // 8\\nwhen(equals(4), (x) => x * 2, 5) // 5\",\n            \"since\": \"0.0.5\",\n            \"type\": \"(a -> Boolean) -> (a -> a) -> a -> a\"\n          },\n          \"llvm\": {\n            \"name\": \"when\",\n            \"description\": \"It runs a transformation only if the predicate returns true, otherwise it\\nreturns the initial value.\",\n            \"example\": \"when(equals(4), (x) => x * 2, 4) // 8\\nwhen(equals(4), (x) => x * 2, 5) // 5\",\n            \"since\": \"0.0.5\",\n            \"type\": \"(a -> Boolean) -> (a -> a) -> a -> a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"not\",\n            \"description\": \"Returns the complement of the given Boolean value.\",\n            \"example\": \"not(true)  // false\\nnot(false) // true\",\n            \"since\": \"0.0.5\",\n            \"type\": \"Boolean -> Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"not\",\n            \"description\": \"Returns the complement of the given Boolean value.\",\n            \"example\": \"not(true)  // false\\nnot(false) // true\",\n            \"since\": \"0.0.5\",\n            \"type\": \"Boolean -> Boolean\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"noop\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> {}\"\n          },\n          \"llvm\": {\n            \"name\": \"noop\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> {}\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"flip\",\n            \"description\": \"Inverts the parameters of a binary function.\",\n            \"example\": \"\",\n            \"since\": \"0.5.0\",\n            \"type\": \"(a -> b -> c) -> b -> a -> c\"\n          },\n          \"llvm\": {\n            \"name\": \"flip\",\n            \"description\": \"Inverts the parameters of a binary function.\",\n            \"example\": \"\",\n            \"since\": \"0.5.0\",\n            \"type\": \"(a -> b -> c) -> b -> a -> c\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"any\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Boolean) -> List a -> Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"any\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Boolean) -> List a -> Boolean\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"all\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Boolean) -> List a -> Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"all\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Boolean) -> List a -> Boolean\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"either\",\n            \"description\": \"Functional \\\"or\\\", given two predicates and a value, return true if either predicate is true.\",\n            \"example\": \"either(equals(\\\"A\\\"), equals(\\\"B\\\"))(\\\"B\\\") // true\\neither(equals(\\\"A\\\"), equals(\\\"B\\\"))(\\\"C\\\") // false\",\n            \"since\": \"0.13.0\",\n            \"type\": \"(a -> Boolean) -> (a -> Boolean) -> a -> Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"either\",\n            \"description\": \"Functional \\\"or\\\", given two predicates and a value, return true if either predicate is true.\",\n            \"example\": \"either(equals(\\\"A\\\"), equals(\\\"B\\\"))(\\\"B\\\") // true\\neither(equals(\\\"A\\\"), equals(\\\"B\\\"))(\\\"C\\\") // false\",\n            \"since\": \"0.13.0\",\n            \"type\": \"(a -> Boolean) -> (a -> Boolean) -> a -> Boolean\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"both\",\n            \"description\": \"Functional \\\"and\\\", given two predicates and a value, return true if both predicates are true.\",\n            \"example\": \"import {lt} from \\\"Compare\\\"\\n// as \`lt\` applies arguments backwards, 70 is greater than both 50 and 60\\nboth(lt(50), lt(60))(70) // true\\nboth(lt(50), lt(60))(55) // false\",\n            \"since\": \"0.13.0\",\n            \"type\": \"(a -> Boolean) -> (a -> Boolean) -> a -> Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"both\",\n            \"description\": \"Functional \\\"and\\\", given two predicates and a value, return true if both predicates are true.\",\n            \"example\": \"import {lt} from \\\"Compare\\\"\\n// as \`lt\` applies arguments backwards, 70 is greater than both 50 and 60\\nboth(lt(50), lt(60))(70) // true\\nboth(lt(50), lt(60))(55) // false\",\n            \"since\": \"0.13.0\",\n            \"type\": \"(a -> Boolean) -> (a -> Boolean) -> a -> Boolean\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"memoize\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b) -> a -> b\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Url.mad\",\n      \"moduleName\": \"Url\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"encode\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Maybe String\"\n          },\n          \"llvm\": {\n            \"name\": \"encode\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Maybe String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"decode\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Maybe String\"\n          },\n          \"llvm\": {\n            \"name\": \"decode\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Maybe String\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Process.mad\",\n      \"moduleName\": \"Process\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        {\n          \"js\": {\n            \"name\": \"CommandResult\",\n            \"params\": \"\",\n            \"aliasedType\": \"{ exitCode :: Integer, stderr :: String, stdout :: String }\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"CommandResult\",\n            \"params\": \"\",\n            \"aliasedType\": \"{ exitCode :: Integer, stderr :: String, stdout :: String }\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"CommandOptions\",\n            \"params\": \"\",\n            \"aliasedType\": \"{ cwd :: String, env :: Dictionary String String }\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"CommandOptions\",\n            \"params\": \"\",\n            \"aliasedType\": \"{ cwd :: String, env :: Dictionary String String }\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"exec\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> List String -> { cwd :: String, env :: Dictionary String String } -> Wish { exitCode :: Integer, stderr :: String, stdout :: String } { exitCode :: Integer, stderr :: String, stdout :: String }\"\n          },\n          \"llvm\": {\n            \"name\": \"exec\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> List String -> { cwd :: String, env :: Dictionary String String } -> Wish { exitCode :: Integer, stderr :: String, stdout :: String } { exitCode :: Integer, stderr :: String, stdout :: String }\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Argv\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List String\"\n          },\n          \"llvm\": {\n            \"name\": \"Argv\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Env\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Dictionary String String\"\n          },\n          \"llvm\": {\n            \"name\": \"Env\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Dictionary String String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"getCurrentWorkingDirectory\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"getCurrentWorkingDirectory\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"exit\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> {}\"\n          },\n          \"llvm\": {\n            \"name\": \"exit\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> {}\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"getExecutablePath\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"getExecutablePath\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"getEnv\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Maybe String\"\n          },\n          \"llvm\": {\n            \"name\": \"getEnv\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Maybe String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"DEFAULT_COMMAND_OPTIONS\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"{ cwd :: String, env :: Dictionary String String }\"\n          },\n          \"llvm\": {\n            \"name\": \"DEFAULT_COMMAND_OPTIONS\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"{ cwd :: String, env :: Dictionary String String }\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Compare.mad\",\n      \"moduleName\": \"Compare\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        {\n          \"js\": {\n            \"name\": \"ComparisonResult\",\n            \"params\": \"\",\n            \"aliasedType\": \"Integer\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"ComparisonResult\",\n            \"params\": \"\",\n            \"aliasedType\": \"Integer\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"interfaces\": [\n        {\n          \"js\": {\n            \"name\": \"Comparable\",\n            \"vars\": \"a\",\n            \"constraints\": \"Eq a\",\n            \"methods\": [\n              \"compare :: a -> a -> ComparisonResult\"\n            ],\n            \"description\": \"The interface comparable allows a type to be compared. It contains only one method\\ncompare that can return one of 3 values:\\n  - 1 if the first parameter is greater than the second\\n  - -1 if the first parameter is less than the second\\n  - 0 if the two parameters are equal\\nFor convenience, the 3 values above have corresponding constants:\\n  - MORE\\n  - LESS\\n  - EQUAL\",\n            \"example\": \"compare(1, 2) // -1\\ncompare(2, 1) // 1\\ncompare(2, 2) // 0\",\n            \"since\": \"0.8.0\"\n          },\n          \"llvm\": {\n            \"name\": \"Comparable\",\n            \"vars\": \"a\",\n            \"constraints\": \"Eq a\",\n            \"methods\": [\n              \"compare :: a -> a -> ComparisonResult\"\n            ],\n            \"description\": \"The interface comparable allows a type to be compared. It contains only one method\\ncompare that can return one of 3 values:\\n  - 1 if the first parameter is greater than the second\\n  - -1 if the first parameter is less than the second\\n  - 0 if the two parameters are equal\\nFor convenience, the 3 values above have corresponding constants:\\n  - MORE\\n  - LESS\\n  - EQUAL\",\n            \"example\": \"compare(1, 2) // -1\\ncompare(2, 1) // 1\\ncompare(2, 2) // 0\",\n            \"since\": \"0.8.0\"\n          }\n        }\n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"MORE\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer\"\n          },\n          \"llvm\": {\n            \"name\": \"MORE\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"LESS\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer\"\n          },\n          \"llvm\": {\n            \"name\": \"LESS\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"EQUAL\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer\"\n          },\n          \"llvm\": {\n            \"name\": \"EQUAL\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"eq\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Comparable a => a -> a -> Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"eq\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Comparable a => a -> a -> Boolean\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"notEq\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Comparable a => a -> a -> Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"notEq\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Comparable a => a -> a -> Boolean\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"gt\",\n            \"description\": \"Takes two comparable values of the same type that implements Comparable and\\nreturn true if the first parameter is strictly greater than the second.\",\n            \"example\": \"gt(3, 2) // true\\ngt(3, 3) // false\",\n            \"since\": \"0.8.0\",\n            \"type\": \"Comparable a => a -> a -> Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"gt\",\n            \"description\": \"Takes two comparable values of the same type that implements Comparable and\\nreturn true if the first parameter is strictly greater than the second.\",\n            \"example\": \"gt(3, 2) // true\\ngt(3, 3) // false\",\n            \"since\": \"0.8.0\",\n            \"type\": \"Comparable a => a -> a -> Boolean\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"ge\",\n            \"description\": \"Takes two comparable values of the same type that implements Comparable and\\nreturn true if the first parameter is greater than the second or if they are\\nequal.\",\n            \"example\": \"gt(3, 2) // true\\ngt(3, 3) // true\",\n            \"since\": \"0.8.0\",\n            \"type\": \"Comparable a => a -> a -> Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"ge\",\n            \"description\": \"Takes two comparable values of the same type that implements Comparable and\\nreturn true if the first parameter is greater than the second or if they are\\nequal.\",\n            \"example\": \"gt(3, 2) // true\\ngt(3, 3) // true\",\n            \"since\": \"0.8.0\",\n            \"type\": \"Comparable a => a -> a -> Boolean\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"lt\",\n            \"description\": \"Takes two comparable values of the same type that implements Comparable and\\nreturn true if the first parameter is strictly less than the second.\",\n            \"example\": \"lt(3, 2) // false\\nlt(3, 4) // true\\nlt(3, 3) // false\",\n            \"since\": \"0.8.0\",\n            \"type\": \"Comparable a => a -> a -> Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"lt\",\n            \"description\": \"Takes two comparable values of the same type that implements Comparable and\\nreturn true if the first parameter is strictly less than the second.\",\n            \"example\": \"lt(3, 2) // false\\nlt(3, 4) // true\\nlt(3, 3) // false\",\n            \"since\": \"0.8.0\",\n            \"type\": \"Comparable a => a -> a -> Boolean\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"le\",\n            \"description\": \"Takes two comparable values of the same type that implements Comparable and\\nreturn true if the first parameter is less than the second or if they are\\nequal.\",\n            \"example\": \"le(3, 2) // false\\nle(3, 4) // true\\nle(3, 3) // true\",\n            \"since\": \"0.8.0\",\n            \"type\": \"Comparable a => a -> a -> Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"le\",\n            \"description\": \"Takes two comparable values of the same type that implements Comparable and\\nreturn true if the first parameter is less than the second or if they are\\nequal.\",\n            \"example\": \"le(3, 2) // false\\nle(3, 4) // true\\nle(3, 3) // true\",\n            \"since\": \"0.8.0\",\n            \"type\": \"Comparable a => a -> a -> Boolean\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Number.mad\",\n      \"moduleName\": \"Number\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"js\": {\n            \"name\": \"Comparable\",\n            \"declaration\": \"Comparable Integer\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Comparable\",\n            \"declaration\": \"Comparable Integer\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Comparable\",\n            \"declaration\": \"Comparable Byte\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Comparable\",\n            \"declaration\": \"Comparable Byte\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Comparable\",\n            \"declaration\": \"Comparable Float\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Comparable\",\n            \"declaration\": \"Comparable Float\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Show\",\n            \"declaration\": \"Show Integer\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Show\",\n            \"declaration\": \"Show Integer\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Show\",\n            \"declaration\": \"Show Byte\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Show\",\n            \"declaration\": \"Show Byte\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Show\",\n            \"declaration\": \"Show Float\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Show\",\n            \"declaration\": \"Show Float\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Scan\",\n            \"declaration\": \"Scan Integer\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Scan\",\n            \"declaration\": \"Scan Integer\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Scan\",\n            \"declaration\": \"Scan Float\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Scan\",\n            \"declaration\": \"Scan Float\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Scan\",\n            \"declaration\": \"Scan Byte\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Scan\",\n            \"declaration\": \"Scan Byte\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"integerToFloat\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> Float\"\n          },\n          \"llvm\": {\n            \"name\": \"integerToFloat\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> Float\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"integerToByte\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> Byte\"\n          },\n          \"llvm\": {\n            \"name\": \"integerToByte\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> Byte\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"byteToFloat\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Byte -> Float\"\n          },\n          \"llvm\": {\n            \"name\": \"byteToFloat\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Byte -> Float\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"byteToInteger\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Byte -> Integer\"\n          },\n          \"llvm\": {\n            \"name\": \"byteToInteger\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Byte -> Integer\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"floatToInteger\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Float -> Integer\"\n          },\n          \"llvm\": {\n            \"name\": \"floatToInteger\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Float -> Integer\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"floatToByte\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Float -> Byte\"\n          },\n          \"llvm\": {\n            \"name\": \"floatToByte\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Float -> Byte\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"formatDecimal\",\n            \"description\": \"Formats a a number given as the first parameter to a string containing the\\namount of numbers after the . given by the second parameter.\",\n            \"example\": \"formatDecimal(3.14159, 2) // \\\"3.14\\\"\",\n            \"since\": \"0.4.0\",\n            \"type\": \"Number a => a -> Float -> String\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Control.mad\",\n      \"moduleName\": \"Control\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"while\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"({} -> Boolean) -> {}\"\n          },\n          \"llvm\": {\n            \"name\": \"while\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"({} -> Boolean) -> {}\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"loop\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> (a -> Boolean) -> (a -> a) -> a\"\n          },\n          \"llvm\": {\n            \"name\": \"loop\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> (a -> Boolean) -> (a -> a) -> a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"maybeLoop\",\n            \"description\": \"A function to perform loops. Might be useful as an alternative to recursion\\nwhen the stack size might be a concern.\\nIt takes an initial value, a transformation function that must return a Maybe.\\nIt returns the value contained in the Just in the last iteration before a\\nNothing was returned.\",\n            \"example\": \"maybeLoop(\\n  1,\\n  (x) => x < 10\\n    ? Just(x * 2)\\n    : Nothing\\n)\\n// 16\",\n            \"since\": \"0.7.0\",\n            \"type\": \"a -> (a -> Maybe a) -> a\"\n          },\n          \"llvm\": {\n            \"name\": \"maybeLoop\",\n            \"description\": \"A function to perform loops. Might be useful as an alternative to recursion\\nwhen the stack size might be a concern.\\nIt takes an initial value, a transformation function that must return a Maybe.\\nIt returns the value contained in the Just in the last iteration before a\\nNothing was returned.\",\n            \"example\": \"maybeLoop(\\n  1,\\n  (x) => x < 10\\n    ? Just(x * 2)\\n    : Nothing\\n)\\n// 16\",\n            \"since\": \"0.7.0\",\n            \"type\": \"a -> (a -> Maybe a) -> a\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Tuple.mad\",\n      \"moduleName\": \"Tuple\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"js\": {\n            \"name\": \"Show\",\n            \"declaration\": \"Show (#[a, b])\",\n            \"constraints\": \"Show a, Show b\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Show\",\n            \"declaration\": \"Show (#[a, b])\",\n            \"constraints\": \"Show a, Show b\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Show\",\n            \"declaration\": \"Show (#[a, b, c])\",\n            \"constraints\": \"Show a, Show b, Show c\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Show\",\n            \"declaration\": \"Show (#[a, b, c])\",\n            \"constraints\": \"Show a, Show b, Show c\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Show\",\n            \"declaration\": \"Show (#[a, b, c, d])\",\n            \"constraints\": \"Show a, Show b, Show c, Show d\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Show\",\n            \"declaration\": \"Show (#[a, b, c, d])\",\n            \"constraints\": \"Show a, Show b, Show c, Show d\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"fst\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"#[a, b] -> a\"\n          },\n          \"llvm\": {\n            \"name\": \"fst\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"#[a, b] -> a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"snd\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"#[a, b] -> b\"\n          },\n          \"llvm\": {\n            \"name\": \"snd\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"#[a, b] -> b\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Parse.mad\",\n      \"moduleName\": \"Parse\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        {\n          \"js\": {\n            \"name\": \"Location\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"Loc Integer Integer Integer\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Location\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"Loc Integer Integer Integer\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Parser\",\n            \"params\": \"a\",\n            \"constructors\": [\n              \"Parser (String -> Location -> #[List #[a, String], Location])\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Parser\",\n            \"params\": \"a\",\n            \"constructors\": [\n              \"Parser (String -> Location -> #[List #[a, String], Location])\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Error\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"Error Location\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Error\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"Error Location\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"js\": {\n            \"name\": \"Functor\",\n            \"declaration\": \"Functor Parser\",\n            \"constraints\": \"\",\n            \"description\": \"maps the contained value of a Parser.\",\n            \"example\": \"type Letter = Letter String\\nmap(Letter, anyChar) // Parser Letter\",\n            \"since\": \"0.0.1\"\n          },\n          \"llvm\": {\n            \"name\": \"Functor\",\n            \"declaration\": \"Functor Parser\",\n            \"constraints\": \"\",\n            \"description\": \"maps the contained value of a Parser.\",\n            \"example\": \"type Letter = Letter String\\nmap(Letter, anyChar) // Parser Letter\",\n            \"since\": \"0.0.1\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Applicative\",\n            \"declaration\": \"Applicative Parser\",\n            \"constraints\": \"\",\n            \"description\": \"This the heart of how parser combinators work. With ap you can apply many\\narguments to a mapping function.\",\n            \"example\": \"parser = pipe(\\n  map((a, b, c) => a ++ b ++ c),\\n  ap($, abcParser),\\n  ap($, abcParser)\\n)(abcParser)\",\n            \"since\": \"0.0.1\"\n          },\n          \"llvm\": {\n            \"name\": \"Applicative\",\n            \"declaration\": \"Applicative Parser\",\n            \"constraints\": \"\",\n            \"description\": \"This the heart of how parser combinators work. With ap you can apply many\\narguments to a mapping function.\",\n            \"example\": \"parser = pipe(\\n  map((a, b, c) => a ++ b ++ c),\\n  ap($, abcParser),\\n  ap($, abcParser)\\n)(abcParser)\",\n            \"since\": \"0.0.1\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Monad\",\n            \"declaration\": \"Monad Parser\",\n            \"constraints\": \"\",\n            \"description\": \"The Monad instance of Parser helps when you need to parse something based on\\nthe previous computation.\",\n            \"example\": \"\",\n            \"since\": \"0.0.1\"\n          },\n          \"llvm\": {\n            \"name\": \"Monad\",\n            \"declaration\": \"Monad Parser\",\n            \"constraints\": \"\",\n            \"description\": \"The Monad instance of Parser helps when you need to parse something based on\\nthe previous computation.\",\n            \"example\": \"\",\n            \"since\": \"0.0.1\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Alternative\",\n            \"declaration\": \"Alternative Parser\",\n            \"constraints\": \"\",\n            \"description\": \"Alternative provides a way to fail over in case a parser failed. alt takes two\\nparsers, and if the first one fails, it tries to run the second one.\",\n            \"example\": \"runParser(alt(char(\\\"c\\\"), char(\\\"a\\\")), \\\"a\\\") // Right \\\"a\\\"\",\n            \"since\": \"0.0.1\"\n          },\n          \"llvm\": {\n            \"name\": \"Alternative\",\n            \"declaration\": \"Alternative Parser\",\n            \"constraints\": \"\",\n            \"description\": \"Alternative provides a way to fail over in case a parser failed. alt takes two\\nparsers, and if the first one fails, it tries to run the second one.\",\n            \"example\": \"runParser(alt(char(\\\"c\\\"), char(\\\"a\\\")), \\\"a\\\") // Right \\\"a\\\"\",\n            \"since\": \"0.0.1\"\n          }\n        }\n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"runParser\",\n            \"description\": \"Runs a given parser with a given input. If it successful it returns a Right\\nof the parsed type, otherwise it returns an error with the location of where\\nit failed.\",\n            \"example\": \"runParser(anyChar, \\\"a\\\")\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser a -> String -> Either Error a\"\n          },\n          \"llvm\": {\n            \"name\": \"runParser\",\n            \"description\": \"Runs a given parser with a given input. If it successful it returns a Right\\nof the parsed type, otherwise it returns an error with the location of where\\nit failed.\",\n            \"example\": \"runParser(anyChar, \\\"a\\\")\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser a -> String -> Either Error a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"fail\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Parser a\"\n          },\n          \"llvm\": {\n            \"name\": \"fail\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Parser a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"anyChar\",\n            \"description\": \"A parser combinator that matches any character and returns a Parser String\\ncontaining that character.\",\n            \"example\": \"parse(anyChar, \\\"?\\\") // Right \\\"?\\\"\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser Char\"\n          },\n          \"llvm\": {\n            \"name\": \"anyChar\",\n            \"description\": \"A parser combinator that matches any character and returns a Parser String\\ncontaining that character.\",\n            \"example\": \"parse(anyChar, \\\"?\\\") // Right \\\"?\\\"\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser Char\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"location\",\n            \"description\": \"A parser combinator that returns the current location in the given input. This\\ncombinator can be used to collect location information for your parsed AST.\",\n            \"example\": \"type Letter = L Location Location String\\nexpected = Right()\\n\\nabcParser = pipe(\\n  map((start, c, end) => L(start, end, c)),\\n  ap($, oneOf([\\\"a\\\", \\\"b\\\", \\\"c\\\"])),\\n  ap($, location)\\n)(location)\\n\\nparser = pipe(\\n  map((a, b, c) => [a, b, c]),\\n  ap($, abcParser),\\n  ap($, abcParser)\\n)(abcParser)\\n\\nrunParser(parser, \\\"cba\\\")\\n// Right [\\n//   L(Loc(0, 0, 0), Loc(1, 0, 1), \\\"c\\\"),\\n//   L(Loc(1, 0, 1), Loc(2, 0, 2), \\\"b\\\"),\\n//   L(Loc(2, 0, 2), Loc(3, 0, 3), \\\"a\\\")\\n// ]\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser Location\"\n          },\n          \"llvm\": {\n            \"name\": \"location\",\n            \"description\": \"A parser combinator that returns the current location in the given input. This\\ncombinator can be used to collect location information for your parsed AST.\",\n            \"example\": \"type Letter = L Location Location String\\nexpected = Right()\\n\\nabcParser = pipe(\\n  map((start, c, end) => L(start, end, c)),\\n  ap($, oneOf([\\\"a\\\", \\\"b\\\", \\\"c\\\"])),\\n  ap($, location)\\n)(location)\\n\\nparser = pipe(\\n  map((a, b, c) => [a, b, c]),\\n  ap($, abcParser),\\n  ap($, abcParser)\\n)(abcParser)\\n\\nrunParser(parser, \\\"cba\\\")\\n// Right [\\n//   L(Loc(0, 0, 0), Loc(1, 0, 1), \\\"c\\\"),\\n//   L(Loc(1, 0, 1), Loc(2, 0, 2), \\\"b\\\"),\\n//   L(Loc(2, 0, 2), Loc(3, 0, 3), \\\"a\\\")\\n// ]\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser Location\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"oneOf\",\n            \"description\": \"A parser combinator that matches any of the given characters.\",\n            \"example\": \"runParser(oneOf([\\\"1\\\", \\\"-\\\", \\\"?\\\"]), \\\"?\\\") // Right \\\"?\\\"\\nrunParser(oneOf([\\\"1\\\", \\\"-\\\", \\\"?\\\"]), \\\"1\\\") // Right \\\"1\\\"\\nrunParser(oneOf([\\\"1\\\", \\\"-\\\", \\\"?\\\"]), \\\"2\\\") // Left (Loc 0 0 0)\",\n            \"since\": \"0.0.1\",\n            \"type\": \"List Char -> Parser Char\"\n          },\n          \"llvm\": {\n            \"name\": \"oneOf\",\n            \"description\": \"A parser combinator that matches any of the given characters.\",\n            \"example\": \"runParser(oneOf([\\\"1\\\", \\\"-\\\", \\\"?\\\"]), \\\"?\\\") // Right \\\"?\\\"\\nrunParser(oneOf([\\\"1\\\", \\\"-\\\", \\\"?\\\"]), \\\"1\\\") // Right \\\"1\\\"\\nrunParser(oneOf([\\\"1\\\", \\\"-\\\", \\\"?\\\"]), \\\"2\\\") // Left (Loc 0 0 0)\",\n            \"since\": \"0.0.1\",\n            \"type\": \"List Char -> Parser Char\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"notOneOf\",\n            \"description\": \"A parser combinator that matches all except the given characters.\",\n            \"example\": \"runParser(notOneOf([\\\"1\\\", \\\"-\\\", \\\"?\\\"]), \\\"?\\\") // Left (Loc 0 0 0)\\nrunParser(notOneOf([\\\"1\\\", \\\"-\\\", \\\"?\\\"]), \\\"1\\\") // Left (Loc 0 0 0)\\nrunParser(notOneOf([\\\"1\\\", \\\"-\\\", \\\"?\\\"]), \\\"2\\\") // Right \\\"2\\\"\",\n            \"since\": \"0.0.1\",\n            \"type\": \"List Char -> Parser Char\"\n          },\n          \"llvm\": {\n            \"name\": \"notOneOf\",\n            \"description\": \"A parser combinator that matches all except the given characters.\",\n            \"example\": \"runParser(notOneOf([\\\"1\\\", \\\"-\\\", \\\"?\\\"]), \\\"?\\\") // Left (Loc 0 0 0)\\nrunParser(notOneOf([\\\"1\\\", \\\"-\\\", \\\"?\\\"]), \\\"1\\\") // Left (Loc 0 0 0)\\nrunParser(notOneOf([\\\"1\\\", \\\"-\\\", \\\"?\\\"]), \\\"2\\\") // Right \\\"2\\\"\",\n            \"since\": \"0.0.1\",\n            \"type\": \"List Char -> Parser Char\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"choice\",\n            \"description\": \"A parser combinator that successively tries all given parsers until one\\nsucceeds, or fails if none has succeeded.\",\n            \"example\": \"parser = choice([string(\\\"good\\\"), string(\\\"really good\\\")])\\nrunParser(parser, \\\"good\\\")        // Right \\\"good\\\"\\nrunParser(parser, \\\"really good\\\") // Right \\\"really good\\\"\\nrunParser(parser, \\\"really\\\")      // Left (Loc 0 0 0)\",\n            \"since\": \"0.0.1\",\n            \"type\": \"List (Parser a) -> Parser a\"\n          },\n          \"llvm\": {\n            \"name\": \"choice\",\n            \"description\": \"A parser combinator that successively tries all given parsers until one\\nsucceeds, or fails if none has succeeded.\",\n            \"example\": \"parser = choice([string(\\\"good\\\"), string(\\\"really good\\\")])\\nrunParser(parser, \\\"good\\\")        // Right \\\"good\\\"\\nrunParser(parser, \\\"really good\\\") // Right \\\"really good\\\"\\nrunParser(parser, \\\"really\\\")      // Left (Loc 0 0 0)\",\n            \"since\": \"0.0.1\",\n            \"type\": \"List (Parser a) -> Parser a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"many\",\n            \"description\": \"A parser combinator that applies 0 or more times the given parser.\",\n            \"example\": \"runParser(many(string(\\\"OK\\\")), \\\"OKOKOK\\\") // Right [\\\"OK\\\", \\\"OK\\\", \\\"OK\\\"]\\nrunParser(many(string(\\\"O\\\")), \\\"OKOKOK\\\")  // Left (Loc 1 0 1)\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser a -> Parser (List a)\"\n          },\n          \"llvm\": {\n            \"name\": \"many\",\n            \"description\": \"A parser combinator that applies 0 or more times the given parser.\",\n            \"example\": \"runParser(many(string(\\\"OK\\\")), \\\"OKOKOK\\\") // Right [\\\"OK\\\", \\\"OK\\\", \\\"OK\\\"]\\nrunParser(many(string(\\\"O\\\")), \\\"OKOKOK\\\")  // Left (Loc 1 0 1)\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser a -> Parser (List a)\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"some\",\n            \"description\": \"A parser combinator that applies 1 or more times the given parser. If no parse\\nfound at all it\'ll fail.\",\n            \"example\": \"runParser(some(string(\\\"OK\\\")), \\\"OKOKOK\\\") // Right [\\\"OK\\\", \\\"OK\\\", \\\"OK\\\"]\\nrunParser(some(string(\\\"OK\\\")), \\\"NOPE\\\")   // Left (Loc 0 0 0)\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser a -> Parser (List a)\"\n          },\n          \"llvm\": {\n            \"name\": \"some\",\n            \"description\": \"A parser combinator that applies 1 or more times the given parser. If no parse\\nfound at all it\'ll fail.\",\n            \"example\": \"runParser(some(string(\\\"OK\\\")), \\\"OKOKOK\\\") // Right [\\\"OK\\\", \\\"OK\\\", \\\"OK\\\"]\\nrunParser(some(string(\\\"OK\\\")), \\\"NOPE\\\")   // Left (Loc 0 0 0)\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser a -> Parser (List a)\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"manyTill\",\n            \"description\": \"A parser combinator that matches many times the first given parser until the\\nsecond one matches. Note that the input matched by the end parser will then\\nbe consumed. If you don\'t want to consume the end parser\'s matched input,\\nyou can use lookAhead.\",\n            \"example\": \"parser = manyTill(char(\\\"a\\\"), char(\\\"b\\\"))\\nrunParser(parser, \\\"aaaaab\\\") // Right \\\"aaaaa\\\"\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser a -> Parser b -> Parser (List a)\"\n          },\n          \"llvm\": {\n            \"name\": \"manyTill\",\n            \"description\": \"A parser combinator that matches many times the first given parser until the\\nsecond one matches. Note that the input matched by the end parser will then\\nbe consumed. If you don\'t want to consume the end parser\'s matched input,\\nyou can use lookAhead.\",\n            \"example\": \"parser = manyTill(char(\\\"a\\\"), char(\\\"b\\\"))\\nrunParser(parser, \\\"aaaaab\\\") // Right \\\"aaaaa\\\"\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser a -> Parser b -> Parser (List a)\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"someTill\",\n            \"description\": \"A parser combinator that matches one or more times the first given parser until the\\nsecond one matches. If the first parser does not match the input, it will fail.\\nNote that the input matched by the end parser will then be consumed. If you don\'t\\nwant to consume the end parser\'s matched input, you can use lookAhead.\",\n            \"example\": \"parser1 = someTill(char(\\\"a\\\"), char(\\\"b\\\"))\\nrunParser(parser1, \\\"aaaaab\\\") // Right \\\"aaaaa\\\"\\n\\nparser2 = someTill(char(\\\"a\\\"), char(\\\"b\\\"))\\nrunParser(parser2, \\\"b\\\") // Left (Loc 0 0 0)\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser a -> Parser b -> Parser (List a)\"\n          },\n          \"llvm\": {\n            \"name\": \"someTill\",\n            \"description\": \"A parser combinator that matches one or more times the first given parser until the\\nsecond one matches. If the first parser does not match the input, it will fail.\\nNote that the input matched by the end parser will then be consumed. If you don\'t\\nwant to consume the end parser\'s matched input, you can use lookAhead.\",\n            \"example\": \"parser1 = someTill(char(\\\"a\\\"), char(\\\"b\\\"))\\nrunParser(parser1, \\\"aaaaab\\\") // Right \\\"aaaaa\\\"\\n\\nparser2 = someTill(char(\\\"a\\\"), char(\\\"b\\\"))\\nrunParser(parser2, \\\"b\\\") // Left (Loc 0 0 0)\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser a -> Parser b -> Parser (List a)\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"lookAhead\",\n            \"description\": \"A parser combinator that makes the given parser not consume any input.\",\n            \"example\": \"alt(char(\\\"a\\\"), lookAhead(anyChar))\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser a -> Parser a\"\n          },\n          \"llvm\": {\n            \"name\": \"lookAhead\",\n            \"description\": \"A parser combinator that makes the given parser not consume any input.\",\n            \"example\": \"alt(char(\\\"a\\\"), lookAhead(anyChar))\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser a -> Parser a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"takeWhile\",\n            \"description\": \"A parser combinator that parses all characters while the given predicate\\nreturns true.\",\n            \"example\": \"runParser(takeWhile(notEquals(\\\"-\\\")), \\\"abcdef-\\\")\",\n            \"since\": \"0.0.1\",\n            \"type\": \"(Char -> Boolean) -> Parser (List Char)\"\n          },\n          \"llvm\": {\n            \"name\": \"takeWhile\",\n            \"description\": \"A parser combinator that parses all characters while the given predicate\\nreturns true.\",\n            \"example\": \"runParser(takeWhile(notEquals(\\\"-\\\")), \\\"abcdef-\\\")\",\n            \"since\": \"0.0.1\",\n            \"type\": \"(Char -> Boolean) -> Parser (List Char)\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"sepBy\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Parser a -> Parser b -> Parser (List a)\"\n          },\n          \"llvm\": {\n            \"name\": \"sepBy\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Parser a -> Parser b -> Parser (List a)\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"satisfy\",\n            \"description\": \"A parser combinator that parses a character based on a given predicate.\",\n            \"example\": \"runParser(satisfy(isDigit), \\\"1\\\")  // Right \\\"1\\\"\\nrunParser(satisfy(isLetter), \\\"1\\\") // Left (Loc 0 0 0)\",\n            \"since\": \"0.0.1\",\n            \"type\": \"(Char -> Boolean) -> Parser Char\"\n          },\n          \"llvm\": {\n            \"name\": \"satisfy\",\n            \"description\": \"A parser combinator that parses a character based on a given predicate.\",\n            \"example\": \"runParser(satisfy(isDigit), \\\"1\\\")  // Right \\\"1\\\"\\nrunParser(satisfy(isLetter), \\\"1\\\") // Left (Loc 0 0 0)\",\n            \"since\": \"0.0.1\",\n            \"type\": \"(Char -> Boolean) -> Parser Char\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"char\",\n            \"description\": \"A parser combinator that parses a single given character.\",\n            \"example\": \"runParser(char(\\\"a\\\"), \\\"a\\\") // Right \\\"a\\\"\\nrunParser(char(\\\"a\\\"), \\\"b\\\") // Left (Loc 0 0 0)\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Char -> Parser Char\"\n          },\n          \"llvm\": {\n            \"name\": \"char\",\n            \"description\": \"A parser combinator that parses a single given character.\",\n            \"example\": \"runParser(char(\\\"a\\\"), \\\"a\\\") // Right \\\"a\\\"\\nrunParser(char(\\\"a\\\"), \\\"b\\\") // Left (Loc 0 0 0)\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Char -> Parser Char\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"notChar\",\n            \"description\": \"The complement of char, it parses any char that not the one given.\",\n            \"example\": \"runParser(notChar(\\\"a\\\"), \\\"a\\\") // Left (Loc 0 0 0)\\nrunParser(notChar(\\\"a\\\"), \\\"b\\\") // Right \\\"a\\\"\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Char -> Parser Char\"\n          },\n          \"llvm\": {\n            \"name\": \"notChar\",\n            \"description\": \"The complement of char, it parses any char that not the one given.\",\n            \"example\": \"runParser(notChar(\\\"a\\\"), \\\"a\\\") // Left (Loc 0 0 0)\\nrunParser(notChar(\\\"a\\\"), \\\"b\\\") // Right \\\"a\\\"\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Char -> Parser Char\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"eof\",\n            \"description\": \"A parser combinator that parses the eof token, or the end of the input.\",\n            \"example\": \"runParser(eof, \\\"\\\")  // Right ()\\nrunParser(eof, \\\"a\\\") // Left (Loc 0 0 0)\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser {}\"\n          },\n          \"llvm\": {\n            \"name\": \"eof\",\n            \"description\": \"A parser combinator that parses the eof token, or the end of the input.\",\n            \"example\": \"runParser(eof, \\\"\\\")  // Right ()\\nrunParser(eof, \\\"a\\\") // Left (Loc 0 0 0)\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser {}\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"string\",\n            \"description\": \"A parser combinator that parses a given string.\",\n            \"example\": \"runParser(string(\\\"hello world\\\"), \\\"hello world\\\") // Right \\\"hello world\\\"\\nrunParser(string(\\\"hello world\\\"), \\\"hello\\\")       // Left (Loc 0 0 0)\",\n            \"since\": \"0.0.1\",\n            \"type\": \"String -> Parser String\"\n          },\n          \"llvm\": {\n            \"name\": \"string\",\n            \"description\": \"A parser combinator that parses a given string.\",\n            \"example\": \"runParser(string(\\\"hello world\\\"), \\\"hello world\\\") // Right \\\"hello world\\\"\\nrunParser(string(\\\"hello world\\\"), \\\"hello\\\")       // Left (Loc 0 0 0)\",\n            \"since\": \"0.0.1\",\n            \"type\": \"String -> Parser String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"spaces\",\n            \"description\": \"A parser combinator that parses empty characters such as spaces, line returns\\nor tabs.\",\n            \"example\": \"runParser(spaces, \\\" \\\\t\\\\n\\\")  // Right \\\" \\\\t\\\\n\\\"\\nrunParser(spaces, \\\" \\\\t\\\\na\\\") // Left (Loc 0 0 0)\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser (List Char)\"\n          },\n          \"llvm\": {\n            \"name\": \"spaces\",\n            \"description\": \"A parser combinator that parses empty characters such as spaces, line returns\\nor tabs.\",\n            \"example\": \"runParser(spaces, \\\" \\\\t\\\\n\\\")  // Right \\\" \\\\t\\\\n\\\"\\nrunParser(spaces, \\\" \\\\t\\\\na\\\") // Left (Loc 0 0 0)\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser (List Char)\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"token\",\n            \"description\": \"A parser combinator that parses the given parser and discards all trailing\\nspaces.\",\n            \"example\": \"runParser(token(string(\\\"hello\\\")), \\\"hello\\\\n\\\")  // Right \\\"hello\\\"\\nrunParser(token(string(\\\"hello\\\")), \\\"hello\\\\n!\\\") // Left (Loc 0 0 0)\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser a -> Parser a\"\n          },\n          \"llvm\": {\n            \"name\": \"token\",\n            \"description\": \"A parser combinator that parses the given parser and discards all trailing\\nspaces.\",\n            \"example\": \"runParser(token(string(\\\"hello\\\")), \\\"hello\\\\n\\\")  // Right \\\"hello\\\"\\nrunParser(token(string(\\\"hello\\\")), \\\"hello\\\\n!\\\") // Left (Loc 0 0 0)\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser a -> Parser a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"symbol\",\n            \"description\": \"A parser combinator that parses a given string and discards all trailing\\nspaces.\",\n            \"example\": \"runParser(symbol(\\\"hello\\\"), \\\"hello\\\\n\\\")  // Right \\\"hello\\\"\\nrunParser(symbol(\\\"hello\\\"), \\\"hello\\\\n!\\\") // Left (Loc 0 0 0)\",\n            \"since\": \"0.0.1\",\n            \"type\": \"String -> Parser String\"\n          },\n          \"llvm\": {\n            \"name\": \"symbol\",\n            \"description\": \"A parser combinator that parses a given string and discards all trailing\\nspaces.\",\n            \"example\": \"runParser(symbol(\\\"hello\\\"), \\\"hello\\\\n\\\")  // Right \\\"hello\\\"\\nrunParser(symbol(\\\"hello\\\"), \\\"hello\\\\n!\\\") // Left (Loc 0 0 0)\",\n            \"since\": \"0.0.1\",\n            \"type\": \"String -> Parser String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"digit\",\n            \"description\": \"A parser combinator that parses a digit.\",\n            \"example\": \"\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser Char\"\n          },\n          \"llvm\": {\n            \"name\": \"digit\",\n            \"description\": \"A parser combinator that parses a digit.\",\n            \"example\": \"\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser Char\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"letter\",\n            \"description\": \"A parser combinator that parses a letter.\",\n            \"example\": \"\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser Char\"\n          },\n          \"llvm\": {\n            \"name\": \"letter\",\n            \"description\": \"A parser combinator that parses a letter.\",\n            \"example\": \"\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser Char\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"letters\",\n            \"description\": \"A parser combinator that parses many letters.\",\n            \"example\": \"\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser (List Char)\"\n          },\n          \"llvm\": {\n            \"name\": \"letters\",\n            \"description\": \"A parser combinator that parses many letters.\",\n            \"example\": \"\",\n            \"since\": \"0.0.1\",\n            \"type\": \"Parser (List Char)\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Applicative.mad\",\n      \"moduleName\": \"Applicative\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        {\n          \"js\": {\n            \"name\": \"Applicative\",\n            \"vars\": \"m\",\n            \"constraints\": \"Functor m\",\n            \"methods\": [\n              \"ap :: m (a -> b) -> m a -> m b\",\n              \"pure :: a -> m a\"\n            ],\n            \"description\": \"Applicative is useful to sequence computations and combine their results. It\\nis best used in combination with Functor\'s map function.\",\n            \"example\": \"pipe(\\n  map((a, b) => a + b),\\n  ap($, Just(4))\\n)(Just(3))\\n// output: Just(7)\\n// in this example we can see how we can extend map and make it work with a\\n// binary function.\",\n            \"since\": \"0.2.0\"\n          },\n          \"llvm\": {\n            \"name\": \"Applicative\",\n            \"vars\": \"m\",\n            \"constraints\": \"Functor m\",\n            \"methods\": [\n              \"ap :: m (a -> b) -> m a -> m b\",\n              \"pure :: a -> m a\"\n            ],\n            \"description\": \"Applicative is useful to sequence computations and combine their results. It\\nis best used in combination with Functor\'s map function.\",\n            \"example\": \"pipe(\\n  map((a, b) => a + b),\\n  ap($, Just(4))\\n)(Just(3))\\n// output: Just(7)\\n// in this example we can see how we can extend map and make it work with a\\n// binary function.\",\n            \"since\": \"0.2.0\"\n          }\n        }\n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"apL\",\n            \"description\": \"Sequence actions, discarding the value of the second argument.\",\n            \"example\": \"\",\n            \"since\": \"0.2.0\",\n            \"type\": \"Applicative m => m a -> m b -> m a\"\n          },\n          \"llvm\": {\n            \"name\": \"apL\",\n            \"description\": \"Sequence actions, discarding the value of the second argument.\",\n            \"example\": \"\",\n            \"since\": \"0.2.0\",\n            \"type\": \"Applicative m => m a -> m b -> m a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"liftA2\",\n            \"description\": \"Maps two Applicative inputs\",\n            \"example\": \"liftA2((a, b) => a + b, Just(2), Just(3)) // Just(5)\",\n            \"since\": \"0.11.0\",\n            \"type\": \"Applicative m => (a -> b -> c) -> m a -> m b -> m c\"\n          },\n          \"llvm\": {\n            \"name\": \"liftA2\",\n            \"description\": \"Maps two Applicative inputs\",\n            \"example\": \"liftA2((a, b) => a + b, Just(2), Just(3)) // Just(5)\",\n            \"since\": \"0.11.0\",\n            \"type\": \"Applicative m => (a -> b -> c) -> m a -> m b -> m c\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"liftA3\",\n            \"description\": \"Maps three Applicative inputs\",\n            \"example\": \"liftA2((a, b, c) => a + b + c, Just(2), Just(1), Just(2)) // Just(5)\",\n            \"since\": \"0.11.0\",\n            \"type\": \"Applicative m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d\"\n          },\n          \"llvm\": {\n            \"name\": \"liftA3\",\n            \"description\": \"Maps three Applicative inputs\",\n            \"example\": \"liftA2((a, b, c) => a + b + c, Just(2), Just(1), Just(2)) // Just(5)\",\n            \"since\": \"0.11.0\",\n            \"type\": \"Applicative m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/IO.mad\",\n      \"moduleName\": \"IO\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        {\n          \"js\": {\n            \"name\": \"Row\",\n            \"params\": \"\",\n            \"aliasedType\": \"{ cols :: List #[String, String], id :: String }\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"ansi\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"{ BGBlack :: String, BGBlue :: String, BGBrightBlack :: String, BGBrightBlue :: String, BGBrightCyan :: String, BGBrightGreen :: String, BGBrightMagenta :: String, BGBrightRed :: String, BGBrightWhite :: String, BGBrightYellow :: String, BGCyan :: String, BGGreen :: String, BGMagenta :: String, BGRed :: String, BGWhite :: String, BGYellow :: String, FGBlack :: String, FGBlue :: String, FGBrightBlack :: String, FGBrightBlue :: String, FGBrightCyan :: String, FGBrightGreen :: String, FGBrightMagenta :: String, FGBrightRed :: String, FGBrightWhite :: String, FGBrightYellow :: String, FGCyan :: String, FGGreen :: String, FGMagenta :: String, FGRed :: String, FGWhite :: String, FGYellow :: String, FormatBold :: String, FormatInvert :: String, FormatNoBold :: String, FormatNoUnderline :: String, FormatUnderline :: String }\"\n          },\n          \"llvm\": {\n            \"name\": \"ansi\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"{ BGBlack :: String, BGBlue :: String, BGBrightBlack :: String, BGBrightBlue :: String, BGBrightCyan :: String, BGBrightGreen :: String, BGBrightMagenta :: String, BGBrightRed :: String, BGBrightWhite :: String, BGBrightYellow :: String, BGCyan :: String, BGGreen :: String, BGMagenta :: String, BGRed :: String, BGWhite :: String, BGYellow :: String, FGBlack :: String, FGBlue :: String, FGBrightBlack :: String, FGBrightBlue :: String, FGBrightCyan :: String, FGBrightGreen :: String, FGBrightMagenta :: String, FGBrightRed :: String, FGBrightWhite :: String, FGBrightYellow :: String, FGCyan :: String, FGGreen :: String, FGMagenta :: String, FGRed :: String, FGWhite :: String, FGYellow :: String, FormatBold :: String, FormatInvert :: String, FormatNoBold :: String, FormatNoUnderline :: String, FormatUnderline :: String }\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"ansiColor\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List String -> String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"ansiColor\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List String -> String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"text\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"{ black :: String -> String, blue :: String -> String, bold :: String -> String, boldUnderline :: String -> String, brightBlack :: String -> String, brightBlue :: String -> String, brightCyan :: String -> String, brightGreen :: String -> String, brightMagenta :: String -> String, brightRed :: String -> String, brightWhite :: String -> String, brightYellow :: String -> String, cyan :: String -> String, green :: String -> String, magenta :: String -> String, red :: String -> String, underline :: String -> String, white :: String -> String, yellow :: String -> String }\"\n          },\n          \"llvm\": {\n            \"name\": \"text\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"{ black :: String -> String, blue :: String -> String, bold :: String -> String, boldUnderline :: String -> String, brightBlack :: String -> String, brightBlue :: String -> String, brightCyan :: String -> String, brightGreen :: String -> String, brightMagenta :: String -> String, brightRed :: String -> String, brightWhite :: String -> String, brightYellow :: String -> String, cyan :: String -> String, green :: String -> String, magenta :: String -> String, red :: String -> String, underline :: String -> String, white :: String -> String, yellow :: String -> String }\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"dark\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"{ black :: String -> String, blue :: String -> String, cyan :: String -> String, green :: String -> String, magenta :: String -> String, red :: String -> String, white :: String -> String, yellow :: String -> String }\"\n          },\n          \"llvm\": {\n            \"name\": \"dark\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"{ black :: String -> String, blue :: String -> String, cyan :: String -> String, green :: String -> String, magenta :: String -> String, red :: String -> String, white :: String -> String, yellow :: String -> String }\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"light\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"{ black :: String -> String, blue :: String -> String, cyan :: String -> String, green :: String -> String, magenta :: String -> String, red :: String -> String, white :: String -> String, yellow :: String -> String }\"\n          },\n          \"llvm\": {\n            \"name\": \"light\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"{ black :: String -> String, blue :: String -> String, cyan :: String -> String, green :: String -> String, magenta :: String -> String, red :: String -> String, white :: String -> String, yellow :: String -> String }\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"colortrace\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Inspect a => (String -> String) -> String -> a -> a\"\n          },\n          \"llvm\": {\n            \"name\": \"colortrace\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Inspect a => (String -> String) -> String -> a -> a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"red\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"red\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"green\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"green\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"yellow\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"yellow\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"grey\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"grey\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"put\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> {}\"\n          },\n          \"llvm\": {\n            \"name\": \"put\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> {}\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"putLine\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> {}\"\n          },\n          \"llvm\": {\n            \"name\": \"putLine\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> {}\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"err\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> {}\"\n          },\n          \"llvm\": {\n            \"name\": \"err\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> {}\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"getLine\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Wish IOError String\"\n          },\n          \"llvm\": {\n            \"name\": \"getLine\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Wish IOError String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"get\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Wish IOError String\"\n          },\n          \"llvm\": {\n            \"name\": \"get\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Wish IOError String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"table\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List { cols :: List #[String, String], id :: String } -> a -> a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"log\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Inspect a => a -> {}\"\n          },\n          \"llvm\": {\n            \"name\": \"log\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Inspect a => a -> {}\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"errLine\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> {}\"\n          },\n          \"llvm\": {\n            \"name\": \"errLine\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> {}\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"putLineAndPass\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> a -> a\"\n          },\n          \"llvm\": {\n            \"name\": \"putLineAndPass\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> a -> a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"newLine\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> a\"\n          },\n          \"llvm\": {\n            \"name\": \"newLine\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"trace\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Inspect a => String -> a -> a\"\n          },\n          \"llvm\": {\n            \"name\": \"trace\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Inspect a => String -> a -> a\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Char.mad\",\n      \"moduleName\": \"Char\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"js\": {\n            \"name\": \"Comparable\",\n            \"declaration\": \"Comparable Char\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Comparable\",\n            \"declaration\": \"Comparable Char\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Show\",\n            \"declaration\": \"Show Char\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Show\",\n            \"declaration\": \"Show Char\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"isDigit\",\n            \"description\": \"Returns true if the character is a digit, false otherwise.\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Char -> Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"isDigit\",\n            \"description\": \"Returns true if the character is a digit, false otherwise.\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Char -> Boolean\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"isLetter\",\n            \"description\": \"Returns true if the character is a letter, false otherwise. Note that if the\\ninput contains more than one character, false is returned.\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Char -> Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"isLetter\",\n            \"description\": \"Returns true if the character is a letter, false otherwise. Note that if the\\ninput contains more than one character, false is returned.\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Char -> Boolean\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"toLower\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Char -> Char\"\n          },\n          \"llvm\": {\n            \"name\": \"toLower\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Char -> Char\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"toUpper\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Char -> Char\"\n          },\n          \"llvm\": {\n            \"name\": \"toUpper\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Char -> Char\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Math.mad\",\n      \"moduleName\": \"Math\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"add\",\n            \"description\": \"Sum two numbers\",\n            \"example\": \"\",\n            \"since\": \"0.0.5\",\n            \"type\": \"Number a => a -> a -> a\"\n          },\n          \"llvm\": {\n            \"name\": \"add\",\n            \"description\": \"Sum two numbers\",\n            \"example\": \"\",\n            \"since\": \"0.0.5\",\n            \"type\": \"Number a => a -> a -> a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"substract\",\n            \"description\": \"Calculate the difference between two numbers\",\n            \"example\": \"\",\n            \"since\": \"0.0.5\",\n            \"type\": \"Number a => a -> a -> a\"\n          },\n          \"llvm\": {\n            \"name\": \"substract\",\n            \"description\": \"Calculate the difference between two numbers\",\n            \"example\": \"\",\n            \"since\": \"0.0.5\",\n            \"type\": \"Number a => a -> a -> a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"multiply\",\n            \"description\": \"Return the product of two numbers\",\n            \"example\": \"\",\n            \"since\": \"0.0.5\",\n            \"type\": \"Number a => a -> a -> a\"\n          },\n          \"llvm\": {\n            \"name\": \"multiply\",\n            \"description\": \"Return the product of two numbers\",\n            \"example\": \"\",\n            \"since\": \"0.0.5\",\n            \"type\": \"Number a => a -> a -> a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"divide\",\n            \"description\": \"Calculate how many times a number can be contained in another number\",\n            \"example\": \"\",\n            \"since\": \"0.0.5\",\n            \"type\": \"Float -> Float -> Float\"\n          },\n          \"llvm\": {\n            \"name\": \"divide\",\n            \"description\": \"Calculate how many times a number can be contained in another number\",\n            \"example\": \"\",\n            \"since\": \"0.0.5\",\n            \"type\": \"Float -> Float -> Float\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"mod\",\n            \"description\": \"Return the euclidean division of one number by another\",\n            \"example\": \"\",\n            \"since\": \"0.0.5\",\n            \"type\": \"Integer -> Integer -> Integer\"\n          },\n          \"llvm\": {\n            \"name\": \"mod\",\n            \"description\": \"Return the euclidean division of one number by another\",\n            \"example\": \"\",\n            \"since\": \"0.0.5\",\n            \"type\": \"Integer -> Integer -> Integer\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"sqrt\",\n            \"description\": \"Find the number which, when multiplied by itself equals the given number.\",\n            \"example\": \"\",\n            \"since\": \"0.0.5\",\n            \"type\": \"Float -> Float\"\n          },\n          \"llvm\": {\n            \"name\": \"sqrt\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Float -> Float\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"round\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Float -> Float\"\n          },\n          \"llvm\": {\n            \"name\": \"round\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Float -> Float\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"ceil\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Float -> Float\"\n          },\n          \"llvm\": {\n            \"name\": \"ceil\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Float -> Float\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"floor\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Float -> Float\"\n          },\n          \"llvm\": {\n            \"name\": \"floor\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Float -> Float\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"pow\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Float -> Float -> Float\"\n          },\n          \"llvm\": {\n            \"name\": \"pow\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Float -> Float -> Float\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"random\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> Float\"\n          },\n          \"llvm\": {\n            \"name\": \"random\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> Float\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"square\",\n            \"description\": \"Find the result of a number times itself\",\n            \"example\": \"\",\n            \"since\": \"0.0.5\",\n            \"type\": \"Number a => a -> a\"\n          },\n          \"llvm\": {\n            \"name\": \"square\",\n            \"description\": \"Find the result of a number times itself\",\n            \"example\": \"\",\n            \"since\": \"0.0.5\",\n            \"type\": \"Number a => a -> a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"min\",\n            \"description\": \"Returns the smallest number from the two given\",\n            \"example\": \"\",\n            \"since\": \"0.12.0\",\n            \"type\": \"Number a => a -> a -> a\"\n          },\n          \"llvm\": {\n            \"name\": \"min\",\n            \"description\": \"Returns the smallest number from the two given\",\n            \"example\": \"\",\n            \"since\": \"0.12.0\",\n            \"type\": \"Number a => a -> a -> a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"max\",\n            \"description\": \"Returns the biggest number from the two given\",\n            \"example\": \"\",\n            \"since\": \"0.12.0\",\n            \"type\": \"Number a => a -> a -> a\"\n          },\n          \"llvm\": {\n            \"name\": \"max\",\n            \"description\": \"Returns the biggest number from the two given\",\n            \"example\": \"\",\n            \"since\": \"0.12.0\",\n            \"type\": \"Number a => a -> a -> a\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/ByteArray.mad\",\n      \"moduleName\": \"ByteArray\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"toString\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"ByteArray -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"toString\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"ByteArray -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"fromString\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> ByteArray\"\n          },\n          \"llvm\": {\n            \"name\": \"fromString\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> ByteArray\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"toList\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"ByteArray -> List Byte\"\n          },\n          \"llvm\": {\n            \"name\": \"toList\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"ByteArray -> List Byte\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"fromList\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List Byte -> ByteArray\"\n          },\n          \"llvm\": {\n            \"name\": \"fromList\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List Byte -> ByteArray\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"concat\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"ByteArray -> ByteArray -> ByteArray\"\n          },\n          \"llvm\": {\n            \"name\": \"concat\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"ByteArray -> ByteArray -> ByteArray\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"mapBytes\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(Byte -> Byte) -> ByteArray -> ByteArray\"\n          },\n          \"llvm\": {\n            \"name\": \"mapBytes\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(Byte -> Byte) -> ByteArray -> ByteArray\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"reduce\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(Byte -> Byte -> a) -> a -> ByteArray -> a\"\n          },\n          \"llvm\": {\n            \"name\": \"reduce\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(Byte -> Byte -> a) -> a -> ByteArray -> a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"length\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"ByteArray -> Integer\"\n          },\n          \"llvm\": {\n            \"name\": \"length\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"ByteArray -> Integer\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"empty\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"ByteArray\"\n          },\n          \"llvm\": {\n            \"name\": \"empty\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"ByteArray\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Maybe.mad\",\n      \"moduleName\": \"Maybe\",\n      \"description\": \"Maybe is one of the major data types in functional programming and serves to\\nrepresent and encapsulate values which may not exist.\\n\\nWhereas other languages may use values like \`null\` / \`nil\` / \`undefined\`, Madlib provides this monadic\\nstructure to encapsulate uncertainty in a computationally safe way.\",\n      \"typeDeclarations\": [\n        {\n          \"js\": {\n            \"name\": \"Maybe\",\n            \"params\": \"a\",\n            \"constructors\": [\n              \"Just a\",\n              \"Nothing \"\n            ],\n            \"description\": \"The Maybe type is used to model uncertain values. It is ideal for values which may not exist,\\nand is used to provide safety and certainty when calculating against potentially uncertain or incorrect inputs.\",\n            \"example\": \"Just(3)     // Just(3) :: Maybe Number\\nJust(false) // Just(false) :: Maybe Boolean\\nNothing     // Nothing :: Maybe a\",\n            \"since\": \"0.0.5\"\n          },\n          \"llvm\": {\n            \"name\": \"Maybe\",\n            \"params\": \"a\",\n            \"constructors\": [\n              \"Just a\",\n              \"Nothing \"\n            ],\n            \"description\": \"The Maybe type is used to model uncertain values. It is ideal for values which may not exist,\\nand is used to provide safety and certainty when calculating against potentially uncertain or incorrect inputs.\",\n            \"example\": \"Just(3)     // Just(3) :: Maybe Number\\nJust(false) // Just(false) :: Maybe Boolean\\nNothing     // Nothing :: Maybe a\",\n            \"since\": \"0.0.5\"\n          }\n        }\n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"js\": {\n            \"name\": \"Functor\",\n            \"declaration\": \"Functor Maybe\",\n            \"constraints\": \"\",\n            \"description\": \"Map over a given Maybe, returning either Just the transformed value\\n(in the case of successful computation) or Nothing.\",\n            \"example\": \"map((x) => x + 1, Just(0)) // Just(1)\\nmap((x) => x + 1, Nothing) // Nothing\",\n            \"since\": \"0.0.6\"\n          },\n          \"llvm\": {\n            \"name\": \"Functor\",\n            \"declaration\": \"Functor Maybe\",\n            \"constraints\": \"\",\n            \"description\": \"Map over a given Maybe, returning either Just the transformed value\\n(in the case of successful computation) or Nothing.\",\n            \"example\": \"map((x) => x + 1, Just(0)) // Just(1)\\nmap((x) => x + 1, Nothing) // Nothing\",\n            \"since\": \"0.0.6\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Applicative\",\n            \"declaration\": \"Applicative Maybe\",\n            \"constraints\": \"\",\n            \"description\": \"Apply Maybe a function to Maybe a value.\\nIf either value is Nothing, return Nothing.\",\n            \"example\": \"ap(Just((x) => x + 1), Just(0)) // Just(1)\\nap(Just((x) => x + 1), Nothing) // Nothing\\nap(Nothing, Just(3))            // Nothing\",\n            \"since\": \"0.0.6\"\n          },\n          \"llvm\": {\n            \"name\": \"Applicative\",\n            \"declaration\": \"Applicative Maybe\",\n            \"constraints\": \"\",\n            \"description\": \"Apply Maybe a function to Maybe a value.\\nIf either value is Nothing, return Nothing.\",\n            \"example\": \"ap(Just((x) => x + 1), Just(0)) // Just(1)\\nap(Just((x) => x + 1), Nothing) // Nothing\\nap(Nothing, Just(3))            // Nothing\",\n            \"since\": \"0.0.6\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Monad\",\n            \"declaration\": \"Monad Maybe\",\n            \"constraints\": \"\",\n            \"description\": \"Use \`chain\` to avoid a double-wrapped Maybe.\\nInstead of a Just of a Just, \`chain\` will flatten the contained transformation to be only one Monad deep.\",\n            \"example\": \"chain((x) => Just(x + 1), Just(1)) // Just(2)\",\n            \"since\": \"0.0.6\"\n          },\n          \"llvm\": {\n            \"name\": \"Monad\",\n            \"declaration\": \"Monad Maybe\",\n            \"constraints\": \"\",\n            \"description\": \"Use \`chain\` to avoid a double-wrapped Maybe.\\nInstead of a Just of a Just, \`chain\` will flatten the contained transformation to be only one Monad deep.\",\n            \"example\": \"chain((x) => Just(x + 1), Just(1)) // Just(2)\",\n            \"since\": \"0.0.6\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Show\",\n            \"declaration\": \"Show (Maybe a)\",\n            \"constraints\": \"Show a\",\n            \"description\": \"Use \`show\` to transform a Maybe to a string.\",\n            \"example\": \"show(Just(3))                   // \\\"Just 3\\\"\\nshow((Nothing :: Maybe Number)) // \\\"Nothing\\\"\",\n            \"since\": \"0.0.6\"\n          },\n          \"llvm\": {\n            \"name\": \"Show\",\n            \"declaration\": \"Show (Maybe a)\",\n            \"constraints\": \"Show a\",\n            \"description\": \"Use \`show\` to transform a Maybe to a string.\",\n            \"example\": \"show(Just(3))                   // \\\"Just 3\\\"\\nshow((Nothing :: Maybe Number)) // \\\"Nothing\\\"\",\n            \"since\": \"0.0.6\"\n          }\n        }\n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"fromMaybe\",\n            \"description\": \"Extricate the value contained in a Just, or the given fallback value if given a Nothing\",\n            \"example\": \"fromMaybe(3, Just(4)) // 4\\nfromMaybe(3, Nothing) // 3\",\n            \"since\": \"0.0.5\",\n            \"type\": \"a -> Maybe a -> a\"\n          },\n          \"llvm\": {\n            \"name\": \"fromMaybe\",\n            \"description\": \"Extricate the value contained in a Just, or the given fallback value if given a Nothing\",\n            \"example\": \"fromMaybe(3, Just(4)) // 4\\nfromMaybe(3, Nothing) // 3\",\n            \"since\": \"0.0.5\",\n            \"type\": \"a -> Maybe a -> a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"isJust\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Maybe a -> Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"isJust\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Maybe a -> Boolean\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Directory.mad\",\n      \"moduleName\": \"Directory\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"read\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Wish IOError (List String)\"\n          },\n          \"llvm\": {\n            \"name\": \"read\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Wish IOError (List String)\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/JsonPrinter.mad\",\n      \"moduleName\": \"JsonPrinter\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"string\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> JsonValue\"\n          },\n          \"llvm\": {\n            \"name\": \"string\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> JsonValue\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"integer\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> JsonValue\"\n          },\n          \"llvm\": {\n            \"name\": \"integer\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> JsonValue\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"float\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Float -> JsonValue\"\n          },\n          \"llvm\": {\n            \"name\": \"float\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Float -> JsonValue\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"boolean\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Boolean -> JsonValue\"\n          },\n          \"llvm\": {\n            \"name\": \"boolean\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Boolean -> JsonValue\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"null\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"JsonValue\"\n          },\n          \"llvm\": {\n            \"name\": \"null\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"JsonValue\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"list\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> JsonValue) -> List a -> JsonValue\"\n          },\n          \"llvm\": {\n            \"name\": \"list\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> JsonValue) -> List a -> JsonValue\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"dict\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> JsonValue) -> Dictionary String a -> JsonValue\"\n          },\n          \"llvm\": {\n            \"name\": \"dict\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> JsonValue) -> Dictionary String a -> JsonValue\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"object\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List #[String, JsonValue] -> JsonValue\"\n          },\n          \"llvm\": {\n            \"name\": \"object\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List #[String, JsonValue] -> JsonValue\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"printJson\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> JsonValue -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"printJson\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> JsonValue -> String\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/List.mad\",\n      \"moduleName\": \"List\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"js\": {\n            \"name\": \"Functor\",\n            \"declaration\": \"Functor List\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Functor\",\n            \"declaration\": \"Functor List\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Applicative\",\n            \"declaration\": \"Applicative List\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Applicative\",\n            \"declaration\": \"Applicative List\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Monad\",\n            \"declaration\": \"Monad List\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Monad\",\n            \"declaration\": \"Monad List\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Semigroup\",\n            \"declaration\": \"Semigroup (List a)\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Semigroup\",\n            \"declaration\": \"Semigroup (List a)\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Monoid\",\n            \"declaration\": \"Monoid (List a)\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Monoid\",\n            \"declaration\": \"Monoid (List a)\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"mapMaybe\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Maybe b) -> List a -> List b\"\n          },\n          \"llvm\": {\n            \"name\": \"mapMaybe\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Maybe b) -> List a -> List b\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"repeat\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> Integer -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"repeat\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> Integer -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"repeatWith\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(Integer -> a) -> Integer -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"repeatWith\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(Integer -> a) -> Integer -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"range\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> Integer -> List Integer\"\n          },\n          \"llvm\": {\n            \"name\": \"range\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> Integer -> List Integer\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"mapM\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Applicative m => (a -> m b) -> List a -> m (List b)\"\n          },\n          \"llvm\": {\n            \"name\": \"mapM\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Applicative m => (a -> m b) -> List a -> m (List b)\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"singleton\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"singleton\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"intercalate\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List a -> List a -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"intercalate\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List a -> List a -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"intersperse\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> List a -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"intersperse\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> List a -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"intercalateWithIndex\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(Integer -> a) -> List a -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"intercalateWithIndex\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(Integer -> a) -> List a -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"mapWithIndex\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Integer -> b) -> List a -> List b\"\n          },\n          \"llvm\": {\n            \"name\": \"mapWithIndex\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Integer -> b) -> List a -> List b\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"concat\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List a -> List a -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"concat\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List a -> List a -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"append\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> List a -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"append\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"a -> List a -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"last\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List a -> Maybe a\"\n          },\n          \"llvm\": {\n            \"name\": \"last\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List a -> Maybe a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"first\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List a -> Maybe a\"\n          },\n          \"llvm\": {\n            \"name\": \"first\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List a -> Maybe a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"init\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List a -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"init\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List a -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"tail\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List a -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"tail\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List a -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"nth\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> List a -> Maybe a\"\n          },\n          \"llvm\": {\n            \"name\": \"nth\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> List a -> Maybe a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"reduceRight\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> b) -> b -> List a -> b\"\n          },\n          \"llvm\": {\n            \"name\": \"reduceRight\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> b) -> b -> List a -> b\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"reduceLeft\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> a) -> a -> List b -> a\"\n          },\n          \"llvm\": {\n            \"name\": \"reduceLeft\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> a) -> a -> List b -> a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"reduce\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> a) -> a -> List b -> a\"\n          },\n          \"llvm\": {\n            \"name\": \"reduce\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> b -> a) -> a -> List b -> a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"reduceM\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Monad m => (a -> b -> m a) -> a -> List b -> m a\"\n          },\n          \"llvm\": {\n            \"name\": \"reduceM\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Monad m => (a -> b -> m a) -> a -> List b -> m a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"filter\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Boolean) -> List a -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"filter\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Boolean) -> List a -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"reject\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Boolean) -> List a -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"reject\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Boolean) -> List a -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"find\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Boolean) -> List a -> Maybe a\"\n          },\n          \"llvm\": {\n            \"name\": \"find\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Boolean) -> List a -> Maybe a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"length\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List a -> Integer\"\n          },\n          \"llvm\": {\n            \"name\": \"length\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List a -> Integer\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"slice\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> Integer -> List a -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"slice\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> Integer -> List a -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"isEmpty\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Eq a => List a -> Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"isEmpty\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Eq a => List a -> Boolean\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"uniqueBy\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> a -> Boolean) -> List a -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"uniqueBy\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> a -> Boolean) -> List a -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"sortBy\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> a -> Integer) -> List a -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"sortBy\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> a -> Integer) -> List a -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"sort\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Comparable a => List a -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"sort\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Comparable a => List a -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"sortAsc\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Comparable a => List a -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"sortAsc\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Comparable a => List a -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"sortDesc\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Comparable a => List a -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"sortDesc\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Comparable a => List a -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"flatten\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List (List a) -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"flatten\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List (List a) -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"zip\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List a -> List b -> List #[a, b]\"\n          },\n          \"llvm\": {\n            \"name\": \"zip\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List a -> List b -> List #[a, b]\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"includes\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Eq a => a -> List a -> Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"includes\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Eq a => a -> List a -> Boolean\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"drop\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> List a -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"drop\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> List a -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"dropLast\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> List a -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"dropLast\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> List a -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"take\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> List a -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"take\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> List a -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"takeLast\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> List a -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"takeLast\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> List a -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"dropWhile\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Boolean) -> List a -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"dropWhile\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Boolean) -> List a -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"takeWhile\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Boolean) -> List a -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"takeWhile\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Boolean) -> List a -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"reverse\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List a -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"reverse\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List a -> List a\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Alternative.mad\",\n      \"moduleName\": \"Alternative\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        {\n          \"js\": {\n            \"name\": \"Alternative\",\n            \"vars\": \"f\",\n            \"constraints\": \"Applicative f\",\n            \"methods\": [\n              \"aempty :: f a\",\n              \"alt :: f a -> f a -> f a\"\n            ],\n            \"description\": \"Alternative instance brings retry mechanism. It tries the first computation and\\nif it fails it tries the second one.\",\n            \"example\": \"\",\n            \"since\": \"0.8.0\"\n          },\n          \"llvm\": {\n            \"name\": \"Alternative\",\n            \"vars\": \"f\",\n            \"constraints\": \"Applicative f\",\n            \"methods\": [\n              \"aempty :: f a\",\n              \"alt :: f a -> f a -> f a\"\n            ],\n            \"description\": \"Alternative instance brings retry mechanism. It tries the first computation and\\nif it fails it tries the second one.\",\n            \"example\": \"\",\n            \"since\": \"0.8.0\"\n          }\n        }\n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        \n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/String.mad\",\n      \"moduleName\": \"String\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"js\": {\n            \"name\": \"Semigroup\",\n            \"declaration\": \"Semigroup String\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Semigroup\",\n            \"declaration\": \"Semigroup String\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Monoid\",\n            \"declaration\": \"Monoid String\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Monoid\",\n            \"declaration\": \"Monoid String\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Comparable\",\n            \"declaration\": \"Comparable String\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Comparable\",\n            \"declaration\": \"Comparable String\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"toLower\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"toLower\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"toUpper\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"toUpper\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"split\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String -> List String\"\n          },\n          \"llvm\": {\n            \"name\": \"split\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String -> List String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"join\",\n            \"description\": \"Joins a list with a separator and returns a String.\\n\\nIMPORTANT:\\nWhen coming from JS, beware that there is a small difference with Array.prototype.join.\\nIn JS, join relies on its dynamic nature and accepts any type as being valid, and transforms it\\nto a string for you. In Madlib you need to provide an instance of Show for your custom types, and\\nfor Madlib types (eg. tuples, List, Boolean, Maybe) it uses the Show instance defined for them.\",\n            \"example\": \"join(\\\" and \\\", [\\\"cats\\\", \\\"dogs\\\"])   // \\\"cats and dogs\\\"\\njoin(\\\"\\\", [\\\"one\\\", \\\"two\\\", \\\"three\\\"]) // \\\"onetwothree\\\"\",\n            \"since\": \"0.0.5\",\n            \"type\": \"String -> List String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"join\",\n            \"description\": \"Joins a list with a separator and returns a String.\\n\\nIMPORTANT:\\nWhen coming from JS, beware that there is a small difference with Array.prototype.join.\\nIn JS, join relies on its dynamic nature and accepts any type as being valid, and transforms it\\nto a string for you. In Madlib you need to provide an instance of Show for your custom types, and\\nfor Madlib types (eg. tuples, List, Boolean, Maybe) it uses the Show instance defined for them.\",\n            \"example\": \"join(\\\" and \\\", [\\\"cats\\\", \\\"dogs\\\"])   // \\\"cats and dogs\\\"\\njoin(\\\"\\\", [\\\"one\\\", \\\"two\\\", \\\"three\\\"]) // \\\"onetwothree\\\"\",\n            \"since\": \"0.0.5\",\n            \"type\": \"String -> List String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"lines\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> List String\"\n          },\n          \"llvm\": {\n            \"name\": \"lines\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> List String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"unlines\",\n            \"description\": \"Transform a List of Strings into a single String,\\nby adding newlines between each item in the List.\",\n            \"example\": \"unlines([\\\"line1\\\", \\\"line2\\\", \\\"line3\\\"]) // \\\"line1\\\\nline2\\\\nline3\\\"\",\n            \"since\": \"0.0.5\",\n            \"type\": \"List String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"unlines\",\n            \"description\": \"Transform a List of Strings into a single String,\\nby adding newlines between each item in the List.\",\n            \"example\": \"unlines([\\\"line1\\\", \\\"line2\\\", \\\"line3\\\"]) // \\\"line1\\\\nline2\\\\nline3\\\"\",\n            \"since\": \"0.0.5\",\n            \"type\": \"List String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"words\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> List String\"\n          },\n          \"llvm\": {\n            \"name\": \"words\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> List String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"unwords\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"unwords\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"toList\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> List Char\"\n          },\n          \"llvm\": {\n            \"name\": \"toList\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> List Char\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"fromList\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List Char -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"fromList\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"List Char -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"mapChars\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(Char -> Char) -> String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"mapChars\",\n            \"description\": \"maps the chars of a String\",\n            \"example\": \"\",\n            \"since\": \"0.12.0\",\n            \"type\": \"(Char -> Char) -> String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"filterChars\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(Char -> Boolean) -> String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"filterChars\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(Char -> Boolean) -> String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"reduceChars\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Char -> a) -> a -> String -> a\"\n          },\n          \"llvm\": {\n            \"name\": \"reduceChars\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(a -> Char -> a) -> a -> String -> a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"slice\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> Integer -> String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"slice\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> Integer -> String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"isEmpty\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"isEmpty\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Boolean\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"drop\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"drop\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"dropLast\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"dropLast\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"dropWhile\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(Char -> Boolean) -> String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"dropWhile\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(Char -> Boolean) -> String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"take\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"take\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"takeLast\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"takeLast\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"takeWhile\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(Char -> Boolean) -> String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"takeWhile\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"(Char -> Boolean) -> String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"charAt\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> String -> Maybe Char\"\n          },\n          \"llvm\": {\n            \"name\": \"charAt\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Integer -> String -> Maybe Char\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"firstChar\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Maybe Char\"\n          },\n          \"llvm\": {\n            \"name\": \"firstChar\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Maybe Char\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"lastChar\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Maybe Char\"\n          },\n          \"llvm\": {\n            \"name\": \"lastChar\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Maybe Char\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"trim\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"trim\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"trimStart\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"trimStart\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"trimEnd\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"trimEnd\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"length\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Integer\"\n          },\n          \"llvm\": {\n            \"name\": \"length\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> Integer\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"repeat\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Char -> Integer -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"repeat\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"Char -> Integer -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"match\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String -> Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"match\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String -> Boolean\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"replace\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String -> String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"replace\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String -> String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"pushChar\",\n            \"description\": \"pushes a char at the beginning of a String\",\n            \"example\": \"\",\n            \"since\": \"0.12.0\",\n            \"type\": \"Char -> String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"pushChar\",\n            \"description\": \"pushes a char at the beginning of a String\",\n            \"example\": \"\",\n            \"since\": \"0.12.0\",\n            \"type\": \"Char -> String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"appendChar\",\n            \"description\": \"appends a char at the end of a String\",\n            \"example\": \"\",\n            \"since\": \"0.12.0\",\n            \"type\": \"Char -> String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"appendChar\",\n            \"description\": \"appends a char at the end of a String\",\n            \"example\": \"\",\n            \"since\": \"0.12.0\",\n            \"type\": \"Char -> String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"reverse\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"reverse\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\",\n            \"type\": \"String -> String\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Either.mad\",\n      \"moduleName\": \"Either\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        {\n          \"js\": {\n            \"name\": \"Either\",\n            \"params\": \"e a\",\n            \"constructors\": [\n              \"Left e\",\n              \"Right a\"\n            ],\n            \"description\": \"An Either is a type which allows for modeling a disjoint union (\\\"a\\\" or \\\"b\\\", but in a safe way).\\nAn Either can be a Right: a successful computation, or a Left: an unsuccessful one.\\nBy capturing a value in this way, we can separate the transformation from the value contained in the Either,\\nmaking it easier to reason about and safer to pass around. (mnemonic: \\\"right\\\" == \\\"correct\\\")\",\n            \"example\": \"\",\n            \"since\": \"0.0.5\"\n          },\n          \"llvm\": {\n            \"name\": \"Either\",\n            \"params\": \"e a\",\n            \"constructors\": [\n              \"Left e\",\n              \"Right a\"\n            ],\n            \"description\": \"An Either is a type which allows for modeling a disjoint union (\\\"a\\\" or \\\"b\\\", but in a safe way).\\nAn Either can be a Right: a successful computation, or a Left: an unsuccessful one.\\nBy capturing a value in this way, we can separate the transformation from the value contained in the Either,\\nmaking it easier to reason about and safer to pass around. (mnemonic: \\\"right\\\" == \\\"correct\\\")\",\n            \"example\": \"\",\n            \"since\": \"0.0.5\"\n          }\n        }\n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"js\": {\n            \"name\": \"Functor\",\n            \"declaration\": \"Functor (Either e)\",\n            \"constraints\": \"\",\n            \"description\": \"We can map over an Either: if it is a Right, it transforms the contained value. \\nIf it is a Left it does nothing. In this way we can freely frame the computations we want\\nto perform without being concerned as to whether the transformation was succcessful until later.\",\n            \"example\": \"map((x) => x + 1, Right(3))      // Right(4)\\nmap((x) => x + 1, Left(\\\"error\\\")) // Left(\\\"error\\\")\",\n            \"since\": \"0.0.6\"\n          },\n          \"llvm\": {\n            \"name\": \"Functor\",\n            \"declaration\": \"Functor (Either e)\",\n            \"constraints\": \"\",\n            \"description\": \"We can map over an Either: if it is a Right, it transforms the contained value. \\nIf it is a Left it does nothing. In this way we can freely frame the computations we want\\nto perform without being concerned as to whether the transformation was succcessful until later.\",\n            \"example\": \"map((x) => x + 1, Right(3))      // Right(4)\\nmap((x) => x + 1, Left(\\\"error\\\")) // Left(\\\"error\\\")\",\n            \"since\": \"0.0.6\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Applicative\",\n            \"declaration\": \"Applicative (Either e)\",\n            \"constraints\": \"\",\n            \"description\": \"By wrapping a function in \`pure\` we can apply it from one Either to another Either.\\nIf both contained values are Rights, it returns a Right of the result. If either Either is a Left,\\nthat value persists and the contained value is not transformed.\",\n            \"example\": \"ap(Right((x) => x + 1), Right(2))     // Right(3)\\nap(Left(\\\"oups\\\"), Right(2))            // Left(\\\"oups\\\")\\nap(Right((x) => x + 1), Left(\\\"oups\\\")) // Left(\\\"oups\\\")\",\n            \"since\": \"0.0.6\"\n          },\n          \"llvm\": {\n            \"name\": \"Applicative\",\n            \"declaration\": \"Applicative (Either e)\",\n            \"constraints\": \"\",\n            \"description\": \"By wrapping a function in \`pure\` we can apply it from one Either to another Either.\\nIf both contained values are Rights, it returns a Right of the result. If either Either is a Left,\\nthat value persists and the contained value is not transformed.\",\n            \"example\": \"ap(Right((x) => x + 1), Right(2))     // Right(3)\\nap(Left(\\\"oups\\\"), Right(2))            // Left(\\\"oups\\\")\\nap(Right((x) => x + 1), Left(\\\"oups\\\")) // Left(\\\"oups\\\")\",\n            \"since\": \"0.0.6\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Monad\",\n            \"declaration\": \"Monad (Either e)\",\n            \"constraints\": \"\",\n            \"description\": \"Use \`chain\` when you want to flatten a potentially double-wrapped Either.\\nInstead of a Right of a Right or a Right of a Left, \`chain\` will flatten the contained transformation\\nand return a Right (if successful) or a Left (if unsuccessful)\",\n            \"example\": \"chain((x) => Right(x + 1), Right(1))   // Right(2)\\nchain((x) => Right(x + 1), Left(\\\"no\\\")) // Left(\\\"no\\\")\",\n            \"since\": \"0.0.6\"\n          },\n          \"llvm\": {\n            \"name\": \"Monad\",\n            \"declaration\": \"Monad (Either e)\",\n            \"constraints\": \"\",\n            \"description\": \"Use \`chain\` when you want to flatten a potentially double-wrapped Either.\\nInstead of a Right of a Right or a Right of a Left, \`chain\` will flatten the contained transformation\\nand return a Right (if successful) or a Left (if unsuccessful)\",\n            \"example\": \"chain((x) => Right(x + 1), Right(1))   // Right(2)\\nchain((x) => Right(x + 1), Left(\\\"no\\\")) // Left(\\\"no\\\")\",\n            \"since\": \"0.0.6\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Bifunctor\",\n            \"declaration\": \"Bifunctor Either\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Bifunctor\",\n            \"declaration\": \"Bifunctor Either\",\n            \"constraints\": \"\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"Show\",\n            \"declaration\": \"Show (Either e a)\",\n            \"constraints\": \"Show e, Show a\",\n            \"description\": \"Use \`show\` to transform an Either to a string.\",\n            \"example\": \"show((Right(3) :: Either Number Number)) // \\\"Right 3\\\"\\nshow((Left(3) :: Either Number Number))  // \\\"Left 3\\\"\",\n            \"since\": \"0.0.6\"\n          },\n          \"llvm\": {\n            \"name\": \"Show\",\n            \"declaration\": \"Show (Either e a)\",\n            \"constraints\": \"Show e, Show a\",\n            \"description\": \"Use \`show\` to transform an Either to a string.\",\n            \"example\": \"show((Right(3) :: Either Number Number)) // \\\"Right 3\\\"\\nshow((Left(3) :: Either Number Number))  // \\\"Left 3\\\"\",\n            \"since\": \"0.0.6\"\n          }\n        }\n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"mapRight\",\n            \"description\": \"An alias for \`map\`, use \`mapRight\` to transform the interior value of a Right.\",\n            \"example\": \"mapRight((x) => x + 1, Right(3)) // Right(4)\",\n            \"since\": \"0.0.5\",\n            \"type\": \"(a -> b) -> Either c a -> Either c b\"\n          },\n          \"llvm\": {\n            \"name\": \"mapRight\",\n            \"description\": \"An alias for \`map\`, use \`mapRight\` to transform the interior value of a Right.\",\n            \"example\": \"mapRight((x) => x + 1, Right(3)) // Right(4)\",\n            \"since\": \"0.0.5\",\n            \"type\": \"(a -> b) -> Either c a -> Either c b\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"mapLeft\",\n            \"description\": \"Unlike most other Either functions, \`mapLeft\` is one of the few which freely transforms the\\ninterior value of a Left. (It is the left-branch analogue of \`mapRight\`.)\",\n            \"example\": \"mapLeft((x) => x + 1, Left(3))  // Left(4)\\nmapLeft((x) => x + 1, Right(3)) // Right(3)\",\n            \"since\": \"0.0.5\",\n            \"type\": \"(a -> b) -> Either a c -> Either b c\"\n          },\n          \"llvm\": {\n            \"name\": \"mapLeft\",\n            \"description\": \"Unlike most other Either functions, \`mapLeft\` is one of the few which freely transforms the\\ninterior value of a Left. (It is the left-branch analogue of \`mapRight\`.)\",\n            \"example\": \"mapLeft((x) => x + 1, Left(3))  // Left(4)\\nmapLeft((x) => x + 1, Right(3)) // Right(3)\",\n            \"since\": \"0.0.5\",\n            \"type\": \"(a -> b) -> Either a c -> Either b c\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"isLeft\",\n            \"description\": \"Returns true if it is a Left, false otherwise.\",\n            \"example\": \"isLeft(Left(1))  // true\\nisLeft(Right(1)) // false\",\n            \"since\": \"0.0.5\",\n            \"type\": \"Either a b -> Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"isLeft\",\n            \"description\": \"Returns true if it is a Left, false otherwise.\",\n            \"example\": \"isLeft(Left(1))  // true\\nisLeft(Right(1)) // false\",\n            \"since\": \"0.0.5\",\n            \"type\": \"Either a b -> Boolean\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"isRight\",\n            \"description\": \"Returns true if it is a Right, false otherwise.\",\n            \"example\": \"isRight(Left(1))  // false\\nisRight(Right(1)) // true\",\n            \"since\": \"0.0.5\",\n            \"type\": \"Either a b -> Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"isRight\",\n            \"description\": \"Returns true if it is a Right, false otherwise.\",\n            \"example\": \"isRight(Left(1))  // false\\nisRight(Right(1)) // true\",\n            \"since\": \"0.0.5\",\n            \"type\": \"Either a b -> Boolean\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"fromRight\",\n            \"description\": \"Pull the value contained in the Either if it is a Right, or the given\\nfallback value if it is a Left.\",\n            \"example\": \"fromRight(1, Right(4)) // 4\\nfromRight(1, Left(4))  // 1\",\n            \"since\": \"0.0.5\",\n            \"type\": \"a -> Either b a -> a\"\n          },\n          \"llvm\": {\n            \"name\": \"fromRight\",\n            \"description\": \"Pull the value contained in the Either if it is a Right, or the given\\nfallback value if it is a Left.\",\n            \"example\": \"fromRight(1, Right(4)) // 4\\nfromRight(1, Left(4))  // 1\",\n            \"since\": \"0.0.5\",\n            \"type\": \"a -> Either b a -> a\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Monoid.mad\",\n      \"moduleName\": \"Monoid\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        {\n          \"js\": {\n            \"name\": \"Monoid\",\n            \"vars\": \"w\",\n            \"constraints\": \"Semigroup w\",\n            \"methods\": [\n              \"mconcat :: w -> w -> w\",\n              \"mempty :: w\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Monoid\",\n            \"vars\": \"w\",\n            \"constraints\": \"Semigroup w\",\n            \"methods\": [\n              \"mconcat :: w -> w -> w\",\n              \"mempty :: w\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        \n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Scan.mad\",\n      \"moduleName\": \"Scan\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        {\n          \"js\": {\n            \"name\": \"Scan\",\n            \"vars\": \"a\",\n            \"constraints\": \"\",\n            \"methods\": [\n              \"scan :: String -> Maybe a\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Scan\",\n            \"vars\": \"a\",\n            \"constraints\": \"\",\n            \"methods\": [\n              \"scan :: String -> Maybe a\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        \n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Array.mad\",\n      \"moduleName\": \"Array\",\n      \"description\": \"Array serves a similar purpose as List but is composed of one memory block.\\nThis means that most operations that add or remove elements might need to reallocate the\\nwhole array whether adding an item at the front of a List has a very small memory footprint.\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"js\": {\n            \"name\": \"Functor\",\n            \"declaration\": \"Functor Array\",\n            \"constraints\": \"\",\n            \"description\": \"Maps over an array, applying the given function to each element.\",\n            \"example\": \"map(add(1), fromList([1, 2, 3])) // fromList([2, 3, 4])\",\n            \"since\": \"0.11.0\"\n          },\n          \"llvm\": {\n            \"name\": \"Functor\",\n            \"declaration\": \"Functor Array\",\n            \"constraints\": \"\",\n            \"description\": \"Maps over an array, applying the given function to each element.\",\n            \"example\": \"map(add(1), fromList([1, 2, 3])) // fromList([2, 3, 4])\",\n            \"since\": \"0.11.0\"\n          }\n        }\n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"fromList\",\n            \"description\": \"Creates an array from a list\",\n            \"example\": \"fromList([1, 2, 3]) // Array([1, 2, 3])\",\n            \"since\": \"0.11.0\",\n            \"type\": \"List a -> Array a\"\n          },\n          \"llvm\": {\n            \"name\": \"fromList\",\n            \"description\": \"Creates an array from a List\",\n            \"example\": \"fromList([1, 2, 3]) // Array([1, 2, 3])\",\n            \"since\": \"0.11.0\",\n            \"type\": \"List a -> Array a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"toList\",\n            \"description\": \"Converts an array to a list\",\n            \"example\": \"toList(fromList([1, 2, 3])) // [1, 2, 3]\",\n            \"since\": \"0.11.0\",\n            \"type\": \"Array a -> List a\"\n          },\n          \"llvm\": {\n            \"name\": \"toList\",\n            \"description\": \"Converts an array to a List\",\n            \"example\": \"toList(fromList([1, 2, 3])) // [1, 2, 3]\",\n            \"since\": \"0.11.0\",\n            \"type\": \"Array a -> List a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"length\",\n            \"description\": \"Returns the length of the given array\",\n            \"example\": \"length(fromList([1, 2, 3])) // 3\",\n            \"since\": \"0.11.0\",\n            \"type\": \"Array a -> Integer\"\n          },\n          \"llvm\": {\n            \"name\": \"length\",\n            \"description\": \"Returns the length of the given array\",\n            \"example\": \"length(fromList([1, 2, 3])) // 3\",\n            \"since\": \"0.11.0\",\n            \"type\": \"Array a -> Integer\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"reduce\",\n            \"description\": \"Reduces an array to a value, given a reducer function, an initial value, and an array.\",\n            \"example\": \"reduce(add, 0, fromList([1, 2, 3])) // 6\",\n            \"since\": \"0.11.0\",\n            \"type\": \"(a -> b -> a) -> a -> Array b -> a\"\n          },\n          \"llvm\": {\n            \"name\": \"reduce\",\n            \"description\": \"Reduces an array to a value, given a reducer function, an initial value, and an array.\",\n            \"example\": \"reduce(add, 0, fromList([1, 2, 3])) // 6\",\n            \"since\": \"0.11.0\",\n            \"type\": \"(a -> b -> a) -> a -> Array b -> a\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"concat\",\n            \"description\": \"Concatenates two arrays\",\n            \"example\": \"concat(fromList([1, 2, 3]), fromList([4, 5, 6])) // fromList([1, 2, 3, 4, 5, 6])\",\n            \"since\": \"0.11.0\",\n            \"type\": \"Array a -> Array a -> Array a\"\n          },\n          \"llvm\": {\n            \"name\": \"concat\",\n            \"description\": \"Concatenates two arrays\",\n            \"example\": \"concat(fromList([1, 2, 3]), fromList([4, 5, 6])) // fromList([1, 2, 3, 4, 5, 6])\",\n            \"since\": \"0.11.0\",\n            \"type\": \"Array a -> Array a -> Array a\"\n          }\n        }\n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Semigroup.mad\",\n      \"moduleName\": \"Semigroup\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        {\n          \"js\": {\n            \"name\": \"Semigroup\",\n            \"vars\": \"a\",\n            \"constraints\": \"\",\n            \"methods\": [\n              \"assoc :: a -> a -> a\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Semigroup\",\n            \"vars\": \"a\",\n            \"constraints\": \"\",\n            \"methods\": [\n              \"assoc :: a -> a -> a\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        \n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/JsonValue.mad\",\n      \"moduleName\": \"JsonValue\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        {\n          \"js\": {\n            \"name\": \"JsonValue\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"JsonString String\",\n              \"JsonInteger Integer\",\n              \"JsonFloat Float\",\n              \"JsonBoolean Boolean\",\n              \"JsonNull \",\n              \"JsonObject (Dictionary String JsonValue)\",\n              \"JsonArray (List JsonValue)\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"JsonValue\",\n            \"params\": \"\",\n            \"constructors\": [\n              \"JsonString String\",\n              \"JsonInteger Integer\",\n              \"JsonFloat Float\",\n              \"JsonBoolean Boolean\",\n              \"JsonNull \",\n              \"JsonObject (Dictionary String JsonValue)\",\n              \"JsonArray (List JsonValue)\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        \n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/Show.mad\",\n      \"moduleName\": \"Show\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        {\n          \"js\": {\n            \"name\": \"Show\",\n            \"vars\": \"a\",\n            \"constraints\": \"\",\n            \"methods\": [\n              \"show :: a -> String\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"Show\",\n            \"vars\": \"a\",\n            \"constraints\": \"\",\n            \"methods\": [\n              \"show :: a -> String\"\n            ],\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        \n      ]\n    },\n    {\n      \"path\": \"/home/runner/work/madlib/madlib/prelude/__internal__/FilePath/Posix.mad\",\n      \"moduleName\": \"FilePath/Posix\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        {\n          \"js\": {\n            \"name\": \"FilePath\",\n            \"params\": \"\",\n            \"aliasedType\": \"String\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          },\n          \"llvm\": {\n            \"name\": \"FilePath\",\n            \"params\": \"\",\n            \"aliasedType\": \"String\",\n            \"description\": \"\",\n            \"example\": \"\",\n            \"since\": \"\"\n          }\n        }\n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"js\": {\n            \"name\": \"dropTrailingPathSeparator\",\n            \"description\": \"Drops the trailing slash of a path.\",\n            \"example\": \"dropTrailingPathSeparator(\\\"/path/\\\") // \\\"/path\\\"\",\n            \"since\": \"0.8.0\",\n            \"type\": \"String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"dropTrailingPathSeparator\",\n            \"description\": \"Drops the trailing slash of a path.\",\n            \"example\": \"dropTrailingPathSeparator(\\\"/path/\\\") // \\\"/path\\\"\",\n            \"since\": \"0.8.0\",\n            \"type\": \"String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"splitPath\",\n            \"description\": \"Splits all segments of a path in a list.\",\n            \"example\": \"splitPath(\\\"/root/path\\\") // [\\\"/\\\", \\\"root/\\\", \\\"path\\\"]\",\n            \"since\": \"0.8.0\",\n            \"type\": \"String -> List String\"\n          },\n          \"llvm\": {\n            \"name\": \"splitPath\",\n            \"description\": \"Splits all segments of a path in a list.\",\n            \"example\": \"splitPath(\\\"/root/path\\\") // [\\\"/\\\", \\\"root/\\\", \\\"path\\\"]\",\n            \"since\": \"0.8.0\",\n            \"type\": \"String -> List String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"joinPath\",\n            \"description\": \"Joins a list of path segments.\",\n            \"example\": \"joinPath([\\\"/\\\", \\\"root/\\\", \\\"path\\\"]) // \\\"/root/path\\\"\",\n            \"since\": \"0.8.0\",\n            \"type\": \"List String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"joinPath\",\n            \"description\": \"Joins a list of path segments.\",\n            \"example\": \"joinPath([\\\"/\\\", \\\"root/\\\", \\\"path\\\"]) // \\\"/root/path\\\"\",\n            \"since\": \"0.8.0\",\n            \"type\": \"List String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"canonicalizePath\",\n            \"description\": \"Removes all extra slashes from a path.\",\n            \"example\": \"canonicalizePath(\\\"/root//path\\\") // \\\"/root/path\\\"\",\n            \"since\": \"0.8.0\",\n            \"type\": \"String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"canonicalizePath\",\n            \"description\": \"Removes all extra slashes from a path.\",\n            \"example\": \"canonicalizePath(\\\"/root//path\\\") // \\\"/root/path\\\"\",\n            \"since\": \"0.8.0\",\n            \"type\": \"String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"dropPathSegments\",\n            \"description\": \"Drops the given amount of segments from the beginning of the path.\\nNote that a leading / is considered as a path segment.\",\n            \"example\": \"dropPathSegments(2, \\\"/root/path/extra\\\") // \\\"path/extra\\\"\",\n            \"since\": \"0.8.0\",\n            \"type\": \"Integer -> String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"dropPathSegments\",\n            \"description\": \"Drops the given amount of segments from the beginning of the path.\\nNote that a leading / is considered as a path segment.\",\n            \"example\": \"dropPathSegments(2, \\\"/root/path/extra\\\") // \\\"path/extra\\\"\",\n            \"since\": \"0.8.0\",\n            \"type\": \"Integer -> String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"parentPath\",\n            \"description\": \"Returns the parent path of the given path.\",\n            \"example\": \"parentPath(\\\"/root/path\\\") // \\\"/root\\\"\",\n            \"since\": \"0.8.0\",\n            \"type\": \"String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"parentPath\",\n            \"description\": \"Returns the parent path of the given path.\",\n            \"example\": \"parentPath(\\\"/root/path\\\") // \\\"/root\\\"\",\n            \"since\": \"0.8.0\",\n            \"type\": \"String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"isRootPathOf\",\n            \"description\": \"Returns true if the first path is a parent path of the second path.\",\n            \"example\": \"isRootPathOf(\\\"/root/path\\\", \\\"/root/path/child\\\")      // true\\nisRootPathOf(\\\"/root/different\\\", \\\"/root/path/child\\\") // false\",\n            \"since\": \"0.8.0\",\n            \"type\": \"String -> String -> Boolean\"\n          },\n          \"llvm\": {\n            \"name\": \"isRootPathOf\",\n            \"description\": \"Returns true if the first path is a parent path of the second path.\",\n            \"example\": \"isRootPathOf(\\\"/root/path\\\", \\\"/root/path/child\\\")      // true\\nisRootPathOf(\\\"/root/different\\\", \\\"/root/path/child\\\") // false\",\n            \"since\": \"0.8.0\",\n            \"type\": \"String -> String -> Boolean\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"takeFileName\",\n            \"description\": \"Extracts the filename part of a path if there is one, returns an empty string otherwise.\",\n            \"example\": \"takeFileName(\\\"/path/filename.ext\\\") // \\\"filename.ext\\\"\\ntakeFileName(\\\"/path/folder/\\\")      // \\\"\\\"\",\n            \"since\": \"0.8.0\",\n            \"type\": \"String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"takeFileName\",\n            \"description\": \"Extracts the filename part of a path if there is one, returns an empty string otherwise.\",\n            \"example\": \"takeFileName(\\\"/path/filename.ext\\\") // \\\"filename.ext\\\"\\ntakeFileName(\\\"/path/folder/\\\")      // \\\"\\\"\",\n            \"since\": \"0.8.0\",\n            \"type\": \"String -> String\"\n          }\n        },\n        {\n          \"js\": {\n            \"name\": \"takeExtension\",\n            \"description\": \"Extracts the extension of a path if there\'s one, returns an empty string otherwise.\",\n            \"example\": \"takeExtension(\\\"/path/file.ext\\\") // \\\"ext\\\"\\ntakeExtension(\\\"/path/file\\\")     // \\\"\\\"\",\n            \"since\": \"0.8.0\",\n            \"type\": \"String -> String\"\n          },\n          \"llvm\": {\n            \"name\": \"takeExtension\",\n            \"description\": \"Extracts the extension of a path if there\'s one, returns an empty string otherwise.\",\n            \"example\": \"takeExtension(\\\"/path/file.ext\\\") // \\\"ext\\\"\\ntakeExtension(\\\"/path/file\\\")     // \\\"\\\"\",\n            \"since\": \"0.8.0\",\n            \"type\": \"String -> String\"\n          }\n        }\n      ]\n    }\n  ]\n}\n`;
  let parsedDocumentation = Json.parse(parser)(docJson);
  let initialState = ((__x__) => {
    if (__x__.__constructor === "Right" && true) {
      let modules = __x__.__args[0];
      return ({ modules: modules, search: ``, path: Maybe.fromMaybe(``)(Url.decode(getUrl())), target: JS });
    }
    else if (__x__.__constructor === "Left" && true) {
      __x__.__args[0];
      return (() => {
    
    return ({ modules: (null), search: ``, path: ``, target: JS })
  })();
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(parsedDocumentation);
  let ModuleView = (target => module => div(({ v: className(`module`), n: null }))(({ v: h2(({ v: className(`module__title`), n: null }))(({ v: link$1(({ v: to(`/` + module.name), n: null }))(({ v: module.name, n: null })), n: null })), n: { v: (String$1.isEmpty(module.description) ? empty()((null)) : p(({ v: className(`module__description`), n: null }))(({ v: renderMarkdown(module.description), n: null }))), n: { v: ul(({ v: className(`content__items`), n: null }))((__listCtorSpread__(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(Type(module.name))(module.typeDeclarations), __listCtorSpread__(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(Alias(module.name))(module.aliases), __listCtorSpread__(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(Interface(module.name))(module.interfaces), __listCtorSpread__(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(Instance(module.name))(module.instances), Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(Expression(target)(module.name))(module.expressions))))))), n: null } } })));
  let ContentView = (target => pathResult => ((__x__) => {
    if (__x__.__constructor === "ModuleResult" && true) {
      let modules = __x__.__args[0];
      return div((null))((Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(ModuleView(target))(modules)));
    }
    else if (__x__.__constructor === "ExpressionResult" && true && true) {
      let moduleName = __x__.__args[0];
      let exp = __x__.__args[1];
      return ul(({ v: className(`content__items`), n: null }))(({ v: Expression(target)(moduleName)(exp), n: null }));
    }
    else if (__x__.__constructor === "TypeResult" && true && true) {
      let moduleName = __x__.__args[0];
      let t = __x__.__args[1];
      return ul(({ v: className(`content__items`), n: null }))(({ v: Type(moduleName)(t), n: null }));
    }
    else if (__x__.__constructor === "AliasResult" && true && true) {
      let moduleName = __x__.__args[0];
      let t = __x__.__args[1];
      return ul(({ v: className(`content__items`), n: null }))(({ v: Alias(moduleName)(t), n: null }));
    }
    else if (__x__.__constructor === "InterfaceResult" && true && true) {
      let moduleName = __x__.__args[0];
      let t = __x__.__args[1];
      return ul(({ v: className(`content__items`), n: null }))(({ v: Interface(moduleName)(t), n: null }));
    }
    else if (__x__.__constructor === "InstanceResult" && true && true) {
      let moduleName = __x__.__args[0];
      let t = __x__.__args[1];
      return ul(({ v: className(`content__items`), n: null }))(({ v: Instance(moduleName)(t), n: null }));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(pathResult));
  let DocApp = (state => {
      getModulesToShow(state);
      let pathResult = processPath(state);
      return div(({ v: className(`documentation`), n: null }))(({ v: Header(state.target), n: { v: SideMenu(state.search)(state.modules), n: { v: main(({ v: className(`documentation__content`), n: null }))(({ v: Breadcrumbs(state), n: { v: ContentView(state.target)(pathResult), n: null } })), n: null } } }));
  });
  let handleUrlChanged = onUrlChanged(syncAction((state => event => ((__x__) => {
    if (__x__.__constructor === "PopStateEvent" && true) {
      let { url: url } = __x__.__args[0];
      return ({ ...state, path: Maybe.fromMaybe(``)(Url.decode(url)) });
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      console.trace(); 
      throw 'non exhaustive patterns!';
    }
  })(event))));
  renderWithConfig(addGlobalEventHandler(handleUrlChanged)(DEFAULT_CONFIG))(DocApp)(initialState)(`app`);
  var Main = {};

  return Main;

}));
