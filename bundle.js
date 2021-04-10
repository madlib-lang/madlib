(function (global, factory) {
  typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports, require('util')) :
  typeof define === 'function' && define.amd ? define(['exports', 'util'], factory) :
  (global = typeof globalThis !== 'undefined' ? globalThis : global || self, factory(global.exe = {}, global.util));
}(this, (function (exports, util) { 'use strict';

  function _interopDefaultLegacy (e) { return e && typeof e === 'object' && 'default' in e ? e : { 'default': e }; }

  var util__default = /*#__PURE__*/_interopDefaultLegacy(util);

  window.$ = '__$__';
  const PLACEHOLDER = '__$__';
  window.__curry__ = fn => {
    const test = x => x === PLACEHOLDER;
    return function curried() {
      const argLength = arguments.length;
      let args = new Array(argLength);

      for (let i = 0; i < argLength; ++i) {
        args[i] = arguments[i];
      }
      const countNonPlaceholders = toCount => {
        let count = toCount.length;
        while (!test(toCount[count])) {
          count--;
        }
        return count;
      };
      const length = as => (as.some(test) ? countNonPlaceholders(as) : as.length);
      function saucy() {
        const arg2Length = arguments.length;
        const args2 = new Array(arg2Length);
        for (let j = 0; j < arg2Length; ++j) {
          args2[j] = arguments[j];
        }

        return curried.apply(
          this,
          args
            .map(y =>
              test(y) && args2[0]
                ? args2.shift()
                : y
            )
            .concat(args2)
        );
      }

      if (length(args) >= fn.length) {
        const currentArgs = args.slice(0, fn.length);
        const result = fn.apply(this, currentArgs);
        const nextArgs = args.slice(fn.length);

        if (typeof result === "function" && length(nextArgs) > 0) {
          return result.apply(this, nextArgs);
        } else {
          return result;
        }
      } else {
        return saucy;
      }
    };
  };

  window.__eq__ = (l, r) => {
    if (l === r) {
      return true;
    }
    if (typeof l !== typeof r) {
      return false;
    }
    if (typeof l === `object`) {
      if (Array.isArray(l)) {
        return l.reduce((res, _, i) => res && __eq__(l[i], r[i]), true);
      }
      const keysL = Object.keys(l);
      const keysR = Object.keys(r);
      return keysL.length === keysR.length && keysL.reduce((res, k) => res && __eq__(l[k], r[k]), true);
    }
    return l === r;
  };

  const __applyMany__ = (f, params) => params.reduce((_f, param) => _f(param), f);
  window.__apMtdDicts__ = (dict, dicts) =>
    Object.keys(dict).reduce((o, k) => ({ ...o, [k]: __applyMany__(dict[k], dicts) }), {});

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

  // file: /Users/a.boeglin/Code/madlib/pkg/node_modules/binary-install/bin/prelude/__internal__/Show.mad
  window.Show = {};

  // file: /Users/a.boeglin/Code/madlib/pkg/node_modules/binary-install/bin/prelude/__internal__/Functor.mad
  window.Functor = {};

  // file: /Users/a.boeglin/Code/madlib/pkg/node_modules/binary-install/bin/prelude/__internal__/Applicative.mad

  window.Applicative = {};

  // file: /Users/a.boeglin/Code/madlib/pkg/node_modules/binary-install/bin/prelude/__internal__/Monad.mad

  window.Monad = {};
  const andDo__ND__ = __once__(() => __curry__((b, a) => Monad_a0.chain(__curry__((_) => b), a)));

  // file: /Users/a.boeglin/Code/madlib/pkg/node_modules/binary-install/bin/prelude/__internal__/Maybe.mad

  const Just = __curry__((a) => ({ __constructor: "Just", __args: [ a ] }));
  const Nothing = ({ __constructor: "Nothing", __args: [  ] });
  Functor['Maybe'] = {};
  Functor['Maybe']['map'] = __curry__((f, __x__) => ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      const x = __x__.__args[0];
      return Just(f(x));
    }
    else if (__x__.__constructor === "Nothing") {
      return Nothing;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  Applicative['Maybe'] = {};
  Applicative['Maybe']['ap'] = __curry__((mf, mx) => ((__x__) => {
    if (__x__.length === 2 && __x__[0].__constructor === "Just" && true && __x__[1].__constructor === "Just" && true) {
      const [{ __args: [f]},{ __args: [x]}] = __x__;
      return Applicative.Maybe.pure(f(x));
    }
    else {
      return Nothing;
    }
  })(([mf, mx])));
  Applicative['Maybe']['pure'] = Just;
  Monad['Maybe'] = {};
  Monad['Maybe']['chain'] = __curry__((f, m) => ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      const x = __x__.__args[0];
      return f(x);
    }
    else if (__x__.__constructor === "Nothing") {
      return Nothing;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(m));
  Monad['Maybe']['of'] = Applicative.Maybe.pure;
  Show['Maybe'] = {};
  const __ShowMaybeshow = __once__(() => __curry__((__x__) => ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      const a = __x__.__args[0];
      return "Just " + Show_z77.show(a);
    }
    else if (__x__.__constructor === "Nothing") {
      return "Nothing";
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(__x__)));
  Show['Maybe']['show'] = (Show_z77) => {
    global.Show_z77 = Show_z77;
    return __ShowMaybeshow();
  };
  const fromMaybe = __curry__((or, __x__) => ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      const a = __x__.__args[0];
      return a;
    }
    else if (__x__.__constructor === "Nothing") {
      return or;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  const isJust = __curry__((__x__) => ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      return true;
    }
    else if (__x__.__constructor === "Nothing") {
      return false;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));

  // file: /Users/a.boeglin/Code/madlib/pkg/node_modules/binary-install/bin/prelude/__internal__/Number.mad

  Show['Number'] = {};
  Show['Number']['show'] = __curry__((x) => new Number(x).toString());
  const fromString = __curry__((str) => {
    const n = parseFloat(str);
    return isNaN(n) ? Nothing : Just(n)
  });
  const formatDecimal = __curry__((a, x) => {
    const n = x.toFixed(a);
    return isNaN(n) ? "0" : n
  });
  const range = __curry__((a, b) => {
    const out = [];
    let x = a;
    while (x < b) {
      out.push(x);
      x += 1;
    }
    return out
  });

  // file: /Users/a.boeglin/Code/madlib/pkg/node_modules/binary-install/bin/prelude/__internal__/Semigroup.mad
  window.Semigroup = {};

  // file: /Users/a.boeglin/Code/madlib/pkg/node_modules/binary-install/bin/prelude/__internal__/Monoid.mad

  window.Monoid = {};

  // file: /Users/a.boeglin/Code/madlib/pkg/node_modules/binary-install/bin/prelude/__internal__/String.mad

  Semigroup['String'] = {};
  Semigroup['String']['assoc'] = __curry__((a, b) => a + b);
  Monoid['String'] = {};
  Monoid['String']['mappend'] = __curry__((a, b) => a + b);
  Monoid['String']['mempty'] = "";
  Show['String'] = {};
  Show['String']['show'] = __curry__((a) => a);
  const replace = __curry__((regex, replacing, input) => input.replace(new RegExp(regex), replacing));
  const split = __curry__((separator, str) => str.split(separator));
  const lines = split("\n");
  const mapChars = __curry__((f, s) => s.split("").map(f).join(""));

  // file: /Users/a.boeglin/Code/madlib/pkg/node_modules/binary-install/bin/prelude/__internal__/Function.mad
  const complement = __curry__((fn, x) => !(fn(x)));
  const always = __curry__((a, b) => a);
  const identity = __curry__((a) => a);
  const equals = __curry__((val, a) => __eq__(val, a));
  const ifElse = __curry__((predicate, truthy, falsy, value) => (predicate(value) ? truthy(value) : falsy(value)));
  const when = __curry__((predicate, truthy, value) => ifElse(predicate, truthy, always(value), value));
  const not = __curry__((b) => !(b));
  const flip = __curry__((f, b, a) => f(a, b));
  const nativeMemoize = (fn) => {
    let cache = {};
    return (a) => {
      const key = JSON.stringify(a);
      if (!cache[key]) {
        cache[key] = fn.apply(undefined, a);
      }
      return cache[key]
    }
  };
  const memoize = __curry__((fn) => nativeMemoize(fn));
  const nativeMemoize2 = (fn) => {
    let cache = {};
    return __curry__((a, b) => {
      const key = JSON.stringify([a, b]);
      if (!cache[key]) {
        cache[key] = fn.apply(undefined, [a, b]);
      }
      return cache[key]
    })
  };
  const memoize2 = __curry__((fn) => nativeMemoize2(fn));
  const nativeMemoize3 = (fn) => {
    let cache = {};
    return __curry__((a, b, c) => {
      const key = JSON.stringify([a, b, c]);
      if (!cache[key]) {
        cache[key] = fn.apply(undefined, [a, b, c]);
      }
      return cache[key]
    })
  };
  const memoize3 = __curry__((fn) => nativeMemoize3(fn));
  var Fun = { complement, always, identity, equals, ifElse, when, not, flip, memoize, memoize2, memoize3 };

  // file: /Users/a.boeglin/Code/madlib/pkg/node_modules/binary-install/bin/prelude/__internal__/Compare.mad
  window.Comparable = {};
  Comparable['Number'] = {};
  Comparable['Number']['compare'] = __curry__((a, b) => (a > b ? MORE : (__eq__(a, b) ? EQUAL : LESS)));
  Comparable['String'] = {};
  Comparable['String']['compare'] = __curry__((a, b) => a > b ? MORE : a == b ? EQUAL : LESS);
  Comparable['Boolean'] = {};
  Comparable['Boolean']['compare'] = __curry__((a, b) => ((__x__) => {
    if (__x__.length === 2 && __x__[0] === true && __x__[1] === false) {
      return MORE;
    }
    else if (__x__.length === 2 && __x__[0] === false && __x__[1] === true) {
      return LESS;
    }
    else {
      return EQUAL;
    }
  })(([a, b])));
  const MORE = 1;
  const LESS = -1;
  const EQUAL = 1;

  // file: /Users/a.boeglin/Code/madlib/pkg/node_modules/binary-install/bin/prelude/__internal__/List.mad

  Functor['List'] = {};
  Functor['List']['map'] = __curry__((f, xs) => xs.map((x) => f(x)));
  Applicative['List'] = {};
  Applicative['List']['ap'] = __curry__((mf, ma) => __curry__((_P_) => flatten(Functor.List.map(__curry__((f) => Functor.List.map(f, ma)))(_P_)))(mf));
  Applicative['List']['pure'] = __curry__((x) => ([x]));
  Monad['List'] = {};
  Monad['List']['chain'] = __curry__((f, xs) => __curry__((_P_) => flatten(Functor.List.map(f)(_P_)))(xs));
  Monad['List']['of'] = Applicative.List.pure;
  Semigroup['List'] = {};
  Semigroup['List']['assoc'] = __curry__((xs1, xs2) => xs1.concat(xs2));
  Monoid['List'] = {};
  Monoid['List']['mappend'] = Semigroup.List.assoc;
  Monoid['List']['mempty'] = ([]);
  Show['List'] = {};
  const __ShowListshow = __once__(() => __curry__((_P_) => __curry__((x) => `[${x}]`)(reduceL(Monoid.String.mappend, "")(intercalate(", ")(Functor.List.map(Show_q354.show)(_P_))))));
  Show['List']['show'] = (Show_q354) => {
    global.Show_q354 = Show_q354;
    return __ShowListshow();
  };
  const singleton = Applicative.List.pure;
  const unlines = __curry__((_P_) => reduce(Monoid.String.mappend, "")(intercalate("\n")(_P_)));
  const intercalate = __curry__((a, xs) => ((__x__) => {
    if (__x__.length === 0) {
      return ([]);
    }
    else if (__x__.length === 1 && true) {
      const [one] = __x__;
      return ([one]);
    }
    else if (__x__.length === 2 && true && true) {
      const [one,two] = __x__;
      return ([one, a, two]);
    }
    else if (__x__.length >= 2 && true && true) {
      const [one,...rest] = __x__;
      return ([one, a,  ...intercalate(a, rest)]);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(xs));
  const join = (Show_f395) => (Show_e394) => {
    global.Show_e394 = Show_e394;
    global.Show_f395 = Show_f395;

    return join__ND__()
  };
  const join__ND__ = __once__(() => __curry__((a, xs) => __curry__((_P_) => reduce(Monoid.String.mappend, "")(intercalate(Show_e394.show(a))(Functor.List.map(Show_f395.show)(_P_))))(xs)));
  const mapWithIndex = __curry__((f, xs) => xs.map(f));
  const concat = __curry__((xs1, xs2) => xs1.concat(xs2));
  const append = __curry__((v, xs) => [...xs, v]);
  const last = __curry__((xs) => {
    const item = xs.slice(-1)[0];
    return item ? Just(item) : Nothing;
  });
  const first = __curry__((xs) => {
    const item = xs[0];
    return item ? Just(item) : Nothing;
  });
  const init = __curry__((xs) => xs.slice(0, -1));
  const nth = __curry__((i, xs) => {
    const x = xs[i];
    return x === undefined
      ? Nothing
      : Just(x);
  });
  const reduceR = __curry__((f, initial, xs) => xs.reduceRight(f, initial));
  const reduceL = __curry__((f, initial, xs) => xs.reduce(f, initial));
  const reduce = reduceL;
  const filter = __curry__((predicate, xs) => xs.filter(predicate));
  const reject = __curry__((predicate, xs) => xs.filter(Fun.complement(predicate)));
  const find = __curry__((predicate, xs) => {
    const found = xs.find(predicate);
    if (found === undefined) {
      return Nothing
    }
    else {
      return Just(found)
    }
  });
  const len = __curry__((xs) => xs.length);
  const slice = __curry__((start, end, xs) => xs.slice(start, end));
  const isEmpty = __curry__((xs) => __eq__(len(xs), 0));
  const uniqueBy = __curry__((f) => reduce(__curry__((result, elem) => ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      return result;
    }
    else if (__x__.__constructor === "Nothing") {
      return ([ ...result, elem]);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(find(f(elem), result))), ([])));
  const sortBy = __curry__((fn, xs) => xs.sort(fn));
  const sort = (Comparable_l531) => {
    global.Comparable_l531 = Comparable_l531;

    return sort__ND__()
  };
  const sort__ND__ = __once__(() => sortBy(Comparable_l531.compare));
  const sortAsc = (Comparable_q536) => {
    global.Comparable_q536 = Comparable_q536;

    return sortAsc__ND__()
  };
  const sortAsc__ND__ = __once__(() => sort(Comparable_q536));
  const sortDesc = (Comparable_t539) => {
    global.Comparable_t539 = Comparable_t539;

    return sortDesc__ND__()
  };
  const sortDesc__ND__ = __once__(() => sortBy(__curry__((a, b) => Comparable_t539.compare(a, b) * -1)));
  const flatten = reduceL(concat, ([]));
  const zip = __curry__((as, bs) => ((__x__) => {
    if (__x__.length === 2 && __x__[0].length >= 2 && true && true && __x__[1].length >= 2 && true && true) {
      const [[a, ...aa],[b, ...bb]] = __x__;
      return Monoid.List.mappend(([([a, b])]), zip(aa, bb));
    }
    else if (__x__.length === 2 && __x__[0].length === 1 && true && __x__[1].length === 1 && true) {
      const [[a],[b]] = __x__;
      return ([([a, b])]);
    }
    else if (__x__.length === 2 && __x__[0].length === 0 && __x__[1].length === 0) {
      return ([]);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(([as, bs])));
  const includes = __curry__((x, xs) => xs.includes(x));
  const drop = __curry__((amount, xs) => slice(0, 0 - amount, xs));
  var L = { singleton, unlines, intercalate, join, mapWithIndex, concat, append, last, first, init, nth, reduceR, reduceL, reduce, filter, reject, find, len, slice, isEmpty, uniqueBy, sortBy, sort, sortAsc, sortDesc, flatten, zip, includes, drop };

  // file: /Users/a.boeglin/Code/madlib/pkg/node_modules/binary-install/bin/prelude/__internal__/Wish.mad

  const Wish = __curry__((a) => ({ __constructor: "Wish", __args: [ a ] }));
  Functor['Wish'] = {};
  Functor['Wish']['map'] = __curry__((f, m) => Wish(__curry__((bad, good) => ((__x__) => {
    if (__x__.__constructor === "Wish" && true) {
      const run = __x__.__args[0];
      return run(bad, __curry__((x) => good(f(x))));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(m))));
  Applicative['Wish'] = {};
  Applicative['Wish']['ap'] = __curry__((mf, m) => Wish(__curry__((bad, good) => ((__x__) => {
    if (__x__.length === 2 && __x__[0].__constructor === "Wish" && true && __x__[1].__constructor === "Wish" && true) {
      const [{ __args: [runMF]},{ __args: [runM]}] = __x__;
      return runM(bad, __curry__((x) => runMF(bad, __curry__((f) => good(f(x))))));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(([mf, m])))));
  Applicative['Wish']['pure'] = __curry__((a) => Wish(__curry__((bad, good) => good(a))));
  Monad['Wish'] = {};
  Monad['Wish']['chain'] = __curry__((f, m) => Wish(__curry__((bad, good) => ((__x__) => {
    if (__x__.__constructor === "Wish" && true) {
      const run = __x__.__args[0];
      return run(bad, __curry__((x) => ((__x__) => {
    if (__x__.__constructor === "Wish" && true) {
      const r = __x__.__args[0];
      return r(bad, good);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(f(x))));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(m))));
  Monad['Wish']['of'] = Applicative.Wish.pure;
  const mapRej = __curry__((f, m) => Wish(__curry__((bad, good) => ((__x__) => {
    if (__x__.__constructor === "Wish" && true) {
      const run = __x__.__args[0];
      return run(__curry__((x) => bad(f(x))), good);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(m))));
  const chainRej = __curry__((f, m) => Wish(__curry__((bad, good) => ((__x__) => {
    if (__x__.__constructor === "Wish" && true) {
      const run = __x__.__args[0];
      return run(__curry__((x) => ((__x__) => {
    if (__x__.__constructor === "Wish" && true) {
      const r = __x__.__args[0];
      return r(bad, good);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(f(x))), good);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(m))));
  const good = __curry__((a) => Wish(__curry__((bad, good) => good(a))));
  const bad = __curry__((e) => Wish(__curry__((bad, good) => bad(e))));
  const getWishFn = __curry__((__x__) => ((__x__) => {
    if (__x__.__constructor === "Wish" && true) {
      const run = __x__.__args[0];
      return run;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  const parallel = __curry__((wishes) => Wish(__curry__((bad, good) => {
      const l = wishes.length;
      let ko = false;
      let ok = 0;
      const out = new Array(l);
      const next = j => (j === l && good(out));
      const fork = (w, j) => (getWishFn(w)(
        e => ko || (bad(e), ko = true),
        x => ko || (out[j] = x, next(++ok))
      ));
      wishes.forEach(fork);

      if (l === 0) {
        good([]);
      }
    })));
  const fulfill = __curry__((bad, good, m) => {
      ((__x__) => {
    if (__x__.__constructor === "Wish" && true) {
      const run = __x__.__args[0];
      return setTimeout(() => run(bad, good), 0);  }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(m);
      return ({ __constructor: "Unit", __args: [] });
  });
  const after = __curry__((time, a) => Wish(__curry__((bad, good) => {
    setTimeout(() => good(a), time);
  })));

  // file: /Users/a.boeglin/Code/madlib/pkg/node_modules/binary-install/bin/prelude/__internal__/Tuple.mad

  Show['Tuple_2'] = {};
  const __ShowTuple_2show = __once__(() => __curry__((__x__) => ((__x__) => {
    if (__x__.length === 2 && true && true) {
      const [a,b] = __x__;
      return "<" + Show_c860.show(a) + ", " + Show_d861.show(b) + ">";
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(__x__)));
  Show['Tuple_2']['show'] = (Show_d861) => (Show_c860) => {
    global.Show_c860 = Show_c860;
    global.Show_d861 = Show_d861;
    return __ShowTuple_2show();
  };
  Show['Tuple_3'] = {};
  const __ShowTuple_3show = __once__(() => __curry__((__x__) => ((__x__) => {
    if (__x__.length === 3 && true && true && true) {
      const [a,b,c] = __x__;
      return "<" + Show_v879.show(a) + ", " + Show_w880.show(b) + ", " + Show_x881.show(c) + ">";
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(__x__)));
  Show['Tuple_3']['show'] = (Show_x881) => (Show_w880) => (Show_v879) => {
    global.Show_v879 = Show_v879;
    global.Show_w880 = Show_w880;
    global.Show_x881 = Show_x881;
    return __ShowTuple_3show();
  };
  Show['Tuple_4'] = {};
  const __ShowTuple_4show = __once__(() => __curry__((__x__) => ((__x__) => {
    if (__x__.length === 4 && true && true && true && true) {
      const [a,b,c,d] = __x__;
      return "<" + Show_w906.show(a) + ", " + Show_x907.show(b) + ", " + Show_y908.show(c) + ", " + Show_z909.show(d) + ">";
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(__x__)));
  Show['Tuple_4']['show'] = (Show_z909) => (Show_y908) => (Show_x907) => (Show_w906) => {
    global.Show_w906 = Show_w906;
    global.Show_x907 = Show_x907;
    global.Show_y908 = Show_y908;
    global.Show_z909 = Show_z909;
    return __ShowTuple_4show();
  };
  const fst = __curry__((tuple) => ((__x__) => {
    if (__x__.length === 2 && true && true) {
      const [a,] = __x__;
      return a;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(tuple));
  const snd = __curry__((tuple) => ((__x__) => {
    if (__x__.length === 2 && true && true) {
      const [,b] = __x__;
      return b;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(tuple));
  var T = { fst, snd };

  // file: /Users/a.boeglin/Code/madlib/pkg/node_modules/binary-install/bin/prelude/__internal__/IO.mad
  const log = __curry__((a) => { console.log(a); return a; });
  const trace = __curry__((v, a) => { console.log(v, a); return a; });
  const err = __curry__((e) => { console.log(e); return e; });
  const warn = __curry__((w) => { console.warn(w); return w; });
  const inspect = __curry__((a) => { console.log(util__default['default'].inspect(a, {showHidden: false, depth: null})); return a; });
  const table = __curry__((rows, a) => {
    const xSpaces = x => new Array(x).fill(' ').join('');

    const longestId = rows.map(x => x.id.length).reduce((a, b) => Math.max(a, b), 0);

    const readyRows = rows
      .map(x => ({ ...x, id: x.id + xSpaces(longestId - x.id.length) }))
      .reduce((rows, row) => {
        return {
          ...rows,
          [row.id]: row.cols.reduce((o, [colName, colValue]) => { o[colName] = colValue; return o; }, {})
        }
      }, {});
    console.table(readyRows);
    return a
  });

  // file: /Users/a.boeglin/Code/madlib/pkg/node_modules/binary-install/bin/prelude/__internal__/Either.mad

  const Left = __curry__((a) => ({ __constructor: "Left", __args: [ a ] }));
  const Right = __curry__((a) => ({ __constructor: "Right", __args: [ a ] }));
  Functor['Either'] = {};
  Functor['Either']['map'] = __curry__((f, __x__) => ((__x__) => {
    if (__x__.__constructor === "Right" && true) {
      const a = __x__.__args[0];
      return Right(f(a));
    }
    else if (__x__.__constructor === "Left" && true) {
      const e = __x__.__args[0];
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  Applicative['Either'] = {};
  Applicative['Either']['ap'] = __curry__((mf, m) => ((__x__) => {
    if (__x__.__constructor === "Left" && true) {
      const e = __x__.__args[0];
      return Left(e);
    }
    else if (__x__.__constructor === "Right" && true) {
      const f = __x__.__args[0];
      return Functor.Either.map(f, m);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(mf));
  Applicative['Either']['pure'] = Right;
  Monad['Either'] = {};
  Monad['Either']['chain'] = __curry__((f, __x__) => ((__x__) => {
    if (__x__.__constructor === "Right" && true) {
      const a = __x__.__args[0];
      return f(a);
    }
    else if (__x__.__constructor === "Left" && true) {
      const e = __x__.__args[0];
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  Monad['Either']['of'] = Applicative.Either.pure;
  Show['Either'] = {};
  const __ShowEithershow = __once__(() => __curry__((__x__) => ((__x__) => {
    if (__x__.__constructor === "Right" && true) {
      const a = __x__.__args[0];
      return "Right " + Show_e1070.show(a);
    }
    else if (__x__.__constructor === "Left" && true) {
      const e = __x__.__args[0];
      return "Left " + Show_h1073.show(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(__x__)));
  Show['Either']['show'] = (Show_e1070) => (Show_h1073) => {
    global.Show_h1073 = Show_h1073;
    global.Show_e1070 = Show_e1070;
    return __ShowEithershow();
  };
  const mapRight = Functor.Either.map;
  const mapLeft = __curry__((f, m) => ((__x__) => {
    if (__x__.__constructor === "Right" && true) {
      const a = __x__.__args[0];
      return Right(a);
    }
    else if (__x__.__constructor === "Left" && true) {
      const e = __x__.__args[0];
      return Left(f(e));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(m));
  const isLeft = __curry__((either) => ((__x__) => {
    if (__x__.__constructor === "Left" && true) {
      return true;
    }
    else {
      return false;
    }
  })(either));
  const isRight = __curry__((either) => ((__x__) => {
    if (__x__.__constructor === "Right" && true) {
      return true;
    }
    else {
      return false;
    }
  })(either));
  const fromRight = __curry__((a, either) => ((__x__) => {
    if (__x__.__constructor === "Right" && true) {
      const x = __x__.__args[0];
      return x;
    }
    else {
      return a;
    }
  })(either));

  // file: /Users/a.boeglin/Code/madlib/pkg/node_modules/binary-install/bin/prelude/__internal__/Dictionary.mad

  const Dictionary = __curry__((a) => ({ __constructor: "Dictionary", __args: [ a ] }));
  Functor['Dictionary'] = {};
  Functor['Dictionary']['map'] = __curry__((fn, __x__) => ((__x__) => {
    if (__x__.__constructor === "Dictionary" && true) {
      const items = __x__.__args[0];
      return fromList(Functor.List.map(__curry__((i) => ([T.fst(i), fn(T.snd(i))])), items));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  const fromList = __curry__((_P_) => Dictionary(L.uniqueBy(__curry__((a, b) => __eq__(T.fst(a), T.fst(b))))(_P_)));
  const empty = fromList(([]));
  const insert = __curry__((k, v, m) => ((__x__) => {
    if (__x__.__constructor === "Dictionary" && true) {
      const items = __x__.__args[0];
      return Dictionary(L.append(([k, v]))(L.reject(__curry__((item) => ((__x__) => {
    if (__x__.length === 2 && true && true) {
      const [kk,] = __x__;
      return __eq__(kk, k);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(item)), items)));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(m));
  const get = __curry__((k, __x__) => ((__x__) => {
    if (__x__.__constructor === "Dictionary" && true) {
      const items = __x__.__args[0];
      return __curry__((_P_) => Functor.Maybe.map(T.snd)(L.find(__curry__((item) => ((__x__) => {
    if (__x__.length === 2 && true && true) {
      const [kk,] = __x__;
      return __eq__(k, kk);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(item)))(_P_)))(items);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  const keys = __curry__((m) => ((__x__) => {
    if (__x__.__constructor === "Dictionary" && true) {
      const items = __x__.__args[0];
      return Functor.List.map(T.fst, items);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(m));
  const values = __curry__((m) => ((__x__) => {
    if (__x__.__constructor === "Dictionary" && true) {
      const items = __x__.__args[0];
      return Functor.List.map(T.snd, items);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(m));
  const len$1 = __curry__((m) => L.len(keys(m)));
  const mapWithKey = __curry__((fn, __x__) => ((__x__) => {
    if (__x__.__constructor === "Dictionary" && true) {
      const items = __x__.__args[0];
      return fromList(Functor.List.map(__curry__((i) => ([T.fst(i), fn(T.fst(i), T.snd(i))])), items));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  const merge = __curry__((a, b) => ((__x__) => {
    if (__x__.length === 2 && __x__[0].__constructor === "Dictionary" && true && __x__[1].__constructor === "Dictionary" && true) {
      const [{ __args: [itemsA]},{ __args: [itemsB]}] = __x__;
      return fromList(L.concat(itemsA, itemsB));
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(([a, b])));
  var D = { fromList, empty, insert, get, keys, values, len: len$1, mapWithKey, merge, Dictionary };

  // file: /Users/a.boeglin/Code/madlib/pkg/node_modules/binary-install/bin/prelude/__internal__/Json.mad

  const string = __curry__((input) => typeof input === "string"
      ? Right(input)
      : Left(`${input} is not a string`));
  const number = __curry__((input) => typeof input === "number"
      ? Right(input)
      : Left(`${input} is not a number`));
  const boolean = __curry__((input) => typeof input === "boolean"
      ? Right(input)
      : Left(`${input} is not a boolean`));
  const dict = __curry__((parser, input) => {
    try {
      const keys = Object.keys(input);
      let result = D.empty;
      keys.forEach((k) => {
        const parsed = parser(input[k]);
        if (isLeft(parsed)) {
          throw parsed;
        } else {
          result = D.insert(k, fromRight("", parsed), result);
        }
      });

      return Right(result);
    } catch(e) {
      return Left("Mapping failed!");
    }
  });
  const list = __curry__((parser, input) => {
    try {
      let result = [];
      input.forEach((a) => {
        const parsed = parser(a);
        if (isLeft(parsed)) {
          throw parsed;
        } else {
          result.push(fromRight("", parsed));
        }
      });
      return Right(result);
    } catch(e) {
      return Left("Mapping failed!");
    }
  });
  const map1 = __curry__((fn, parser, input) => ((__x__) => {
    if (__x__.__constructor === "Right" && true) {
      const a = __x__.__args[0];
      return Right(fn(a));
    }
    else if (__x__.__constructor === "Left" && true) {
      const e = __x__.__args[0];
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(parser(input)));
  const map2 = __curry__((fn, parserA, parserB, input) => ((__x__) => {
    if (__x__.length === 2 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true) {
      const [{ __args: [a]},{ __args: [b]}] = __x__;
      return Right(fn(a, b));
    }
    else if (__x__.length === 2 && __x__[0].__constructor === "Left" && true && true) {
      const [{ __args: [e]},] = __x__;
      return Left(e);
    }
    else if (__x__.length === 2 && true && __x__[1].__constructor === "Left" && true) {
      const [,{ __args: [e]}] = __x__;
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(([parserA(input), parserB(input)])));
  const map3 = __curry__((fn, parserA, parserB, parserC, input) => ((__x__) => {
    if (__x__.length === 3 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true) {
      const [{ __args: [a]},{ __args: [b]},{ __args: [c]}] = __x__;
      return Right(fn(a, b, c));
    }
    else if (__x__.length === 3 && __x__[0].__constructor === "Left" && true && true && true) {
      const [{ __args: [e]},,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 3 && true && __x__[1].__constructor === "Left" && true && true) {
      const [,{ __args: [e]},] = __x__;
      return Left(e);
    }
    else if (__x__.length === 3 && true && true && __x__[2].__constructor === "Left" && true) {
      const [,,{ __args: [e]}] = __x__;
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(([parserA(input), parserB(input), parserC(input)])));
  const map4 = __curry__((fn, parserA, parserB, parserC, parserD, input) => ((__x__) => {
    if (__x__.length === 4 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true && __x__[3].__constructor === "Right" && true) {
      const [{ __args: [a]},{ __args: [b]},{ __args: [c]},{ __args: [d]}] = __x__;
      return Right(fn(a, b, c, d));
    }
    else if (__x__.length === 4 && __x__[0].__constructor === "Left" && true && true && true && true) {
      const [{ __args: [e]},,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 4 && true && __x__[1].__constructor === "Left" && true && true && true) {
      const [,{ __args: [e]},,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 4 && true && true && __x__[2].__constructor === "Left" && true && true) {
      const [,,{ __args: [e]},] = __x__;
      return Left(e);
    }
    else if (__x__.length === 4 && true && true && true && __x__[3].__constructor === "Left" && true) {
      const [,,,{ __args: [e]}] = __x__;
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(([parserA(input), parserB(input), parserC(input), parserD(input)])));
  const map5 = __curry__((fn, parserA, parserB, parserC, parserD, parserE, input) => ((__x__) => {
    if (__x__.length === 5 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true && __x__[3].__constructor === "Right" && true && __x__[4].__constructor === "Right" && true) {
      const [{ __args: [a]},{ __args: [b]},{ __args: [c]},{ __args: [d]},{ __args: [e]}] = __x__;
      return Right(fn(a, b, c, d, e));
    }
    else if (__x__.length === 5 && __x__[0].__constructor === "Left" && true && true && true && true && true) {
      const [{ __args: [e]},,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 5 && true && __x__[1].__constructor === "Left" && true && true && true && true) {
      const [,{ __args: [e]},,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 5 && true && true && __x__[2].__constructor === "Left" && true && true && true) {
      const [,,{ __args: [e]},,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 5 && true && true && true && __x__[3].__constructor === "Left" && true && true) {
      const [,,,{ __args: [e]},] = __x__;
      return Left(e);
    }
    else if (__x__.length === 5 && true && true && true && true && __x__[4].__constructor === "Left" && true) {
      const [,,,,{ __args: [e]}] = __x__;
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(([parserA(input), parserB(input), parserC(input), parserD(input), parserE(input)])));
  const map6 = __curry__((fn, parserA, parserB, parserC, parserD, parserE, parserF, input) => ((__x__) => {
    if (__x__.length === 6 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true && __x__[3].__constructor === "Right" && true && __x__[4].__constructor === "Right" && true && __x__[5].__constructor === "Right" && true) {
      const [{ __args: [a]},{ __args: [b]},{ __args: [c]},{ __args: [d]},{ __args: [e]},{ __args: [f]}] = __x__;
      return Right(fn(a, b, c, d, e, f));
    }
    else if (__x__.length === 6 && __x__[0].__constructor === "Left" && true && true && true && true && true && true) {
      const [{ __args: [e]},,,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 6 && true && __x__[1].__constructor === "Left" && true && true && true && true && true) {
      const [,{ __args: [e]},,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 6 && true && true && __x__[2].__constructor === "Left" && true && true && true && true) {
      const [,,{ __args: [e]},,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 6 && true && true && true && __x__[3].__constructor === "Left" && true && true && true) {
      const [,,,{ __args: [e]},,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 6 && true && true && true && true && __x__[4].__constructor === "Left" && true && true) {
      const [,,,,{ __args: [e]},] = __x__;
      return Left(e);
    }
    else if (__x__.length === 6 && true && true && true && true && true && __x__[5].__constructor === "Left" && true) {
      const [,,,,,{ __args: [e]}] = __x__;
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(([parserA(input), parserB(input), parserC(input), parserD(input), parserE(input), parserF(input)])));
  const map7 = __curry__((fn, parserA, parserB, parserC, parserD, parserE, parserF, parserG, input) => ((__x__) => {
    if (__x__.length === 7 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true && __x__[3].__constructor === "Right" && true && __x__[4].__constructor === "Right" && true && __x__[5].__constructor === "Right" && true && __x__[6].__constructor === "Right" && true) {
      const [{ __args: [a]},{ __args: [b]},{ __args: [c]},{ __args: [d]},{ __args: [e]},{ __args: [f]},{ __args: [g]}] = __x__;
      return Right(fn(a, b, c, d, e, f, g));
    }
    else if (__x__.length === 7 && __x__[0].__constructor === "Left" && true && true && true && true && true && true && true) {
      const [{ __args: [e]},,,,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 7 && true && __x__[1].__constructor === "Left" && true && true && true && true && true && true) {
      const [,{ __args: [e]},,,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 7 && true && true && __x__[2].__constructor === "Left" && true && true && true && true && true) {
      const [,,{ __args: [e]},,,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 7 && true && true && true && __x__[3].__constructor === "Left" && true && true && true && true) {
      const [,,,{ __args: [e]},,,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 7 && true && true && true && true && __x__[4].__constructor === "Left" && true && true && true) {
      const [,,,,{ __args: [e]},,] = __x__;
      return Left(e);
    }
    else if (__x__.length === 7 && true && true && true && true && true && __x__[5].__constructor === "Left" && true && true) {
      const [,,,,,{ __args: [e]},] = __x__;
      return Left(e);
    }
    else if (__x__.length === 7 && true && true && true && true && true && true && __x__[6].__constructor === "Left" && true) {
      const [,,,,,,{ __args: [e]}] = __x__;
      return Left(e);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(([parserA(input), parserB(input), parserC(input), parserD(input), parserE(input), parserF(input), parserG(input)])));
  const maybe = __curry__((parser, input) => {
    if (input) {
      const parsed = parser(input);
      if (isLeft(parsed)) {
        return Right(Nothing);
      }
      return mapRight(Just, parsed);
    } else {
      return Right(Nothing)
    }
  });
  const lazy = __curry__((wrapped, input) => wrapped(({ __constructor: "Unit", __args: [] }), input));
  const field = __curry__((fieldName, parser, input) => parser(input[fieldName]));
  const parse = __curry__((parser, input) => {
    try {
      return parser(JSON.parse(input))
    } catch(e) {
      console.log(e);
      return Left("Parsing error!\nInvalid input you might have called parse on an already parsed input or the given JSON is invalid.")
    }
  });
  var J = { string, number, boolean, dict, list, map1, map2, map3, map4, map5, map6, map7, maybe, lazy, field, parse };

  function vnode(sel, data, children, text, elm) {
      const key = data === undefined ? undefined : data.key;
      return { sel, data, children, text, elm, key };
  }

  const array = Array.isArray;
  function primitive(s) {
      return typeof s === 'string' || typeof s === 'number';
  }

  function createElement(tagName) {
      return document.createElement(tagName);
  }
  function createElementNS(namespaceURI, qualifiedName) {
      return document.createElementNS(namespaceURI, qualifiedName);
  }
  function createTextNode(text) {
      return document.createTextNode(text);
  }
  function createComment(text) {
      return document.createComment(text);
  }
  function insertBefore(parentNode, newNode, referenceNode) {
      parentNode.insertBefore(newNode, referenceNode);
  }
  function removeChild(node, child) {
      node.removeChild(child);
  }
  function appendChild(node, child) {
      node.appendChild(child);
  }
  function parentNode(node) {
      return node.parentNode;
  }
  function nextSibling(node) {
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
  function isElement(node) {
      return node.nodeType === 1;
  }
  function isText(node) {
      return node.nodeType === 3;
  }
  function isComment(node) {
      return node.nodeType === 8;
  }
  const htmlDomApi = {
      createElement,
      createElementNS,
      createTextNode,
      createComment,
      insertBefore,
      removeChild,
      appendChild,
      parentNode,
      nextSibling,
      tagName,
      setTextContent,
      getTextContent,
      isElement,
      isText,
      isComment,
  };

  function isUndef(s) {
      return s === undefined;
  }
  function isDef(s) {
      return s !== undefined;
  }
  const emptyNode = vnode('', {}, [], undefined, undefined);
  function sameVnode(vnode1, vnode2) {
      return vnode1.key === vnode2.key && vnode1.sel === vnode2.sel;
  }
  function isVnode(vnode) {
      return vnode.sel !== undefined;
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
  const hooks = ['create', 'update', 'remove', 'destroy', 'pre', 'post'];
  function init$1(modules, domApi) {
      let i;
      let j;
      const cbs = {
          create: [],
          update: [],
          remove: [],
          destroy: [],
          pre: [],
          post: []
      };
      const api = domApi !== undefined ? domApi : htmlDomApi;
      for (i = 0; i < hooks.length; ++i) {
          cbs[hooks[i]] = [];
          for (j = 0; j < modules.length; ++j) {
              const hook = modules[j][hooks[i]];
              if (hook !== undefined) {
                  cbs[hooks[i]].push(hook);
              }
          }
      }
      function emptyNodeAt(elm) {
          const id = elm.id ? '#' + elm.id : '';
          const c = elm.className ? '.' + elm.className.split(' ').join('.') : '';
          return vnode(api.tagName(elm).toLowerCase() + id + c, {}, [], undefined, elm);
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
          var _a, _b;
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
          if (sel === '!') {
              if (isUndef(vnode.text)) {
                  vnode.text = '';
              }
              vnode.elm = api.createComment(vnode.text);
          }
          else if (sel !== undefined) {
              // Parse selector
              const hashIdx = sel.indexOf('#');
              const dotIdx = sel.indexOf('.', hashIdx);
              const hash = hashIdx > 0 ? hashIdx : sel.length;
              const dot = dotIdx > 0 ? dotIdx : sel.length;
              const tag = hashIdx !== -1 || dotIdx !== -1 ? sel.slice(0, Math.min(hash, dot)) : sel;
              const elm = vnode.elm = isDef(data) && isDef(i = data.ns)
                  ? api.createElementNS(i, tag)
                  : api.createElement(tag);
              if (hash < dot)
                  elm.setAttribute('id', sel.slice(hash + 1, dot));
              if (dotIdx > 0)
                  elm.setAttribute('class', sel.slice(dot + 1).replace(/\./g, ' '));
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
                      if (child != null && typeof child !== 'string') {
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
                  else { // Text node
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
              else if (sameVnode(oldStartVnode, newEndVnode)) { // Vnode moved right
                  patchVnode(oldStartVnode, newEndVnode, insertedVnodeQueue);
                  api.insertBefore(parentElm, oldStartVnode.elm, api.nextSibling(oldEndVnode.elm));
                  oldStartVnode = oldCh[++oldStartIdx];
                  newEndVnode = newCh[--newEndIdx];
              }
              else if (sameVnode(oldEndVnode, newStartVnode)) { // Vnode moved left
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
                  if (isUndef(idxInOld)) { // New element
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
          if (oldStartIdx <= oldEndIdx || newStartIdx <= newEndIdx) {
              if (oldStartIdx > oldEndIdx) {
                  before = newCh[newEndIdx + 1] == null ? null : newCh[newEndIdx + 1].elm;
                  addVnodes(parentElm, before, newCh, newStartIdx, newEndIdx, insertedVnodeQueue);
              }
              else {
                  removeVnodes(parentElm, oldCh, oldStartIdx, oldEndIdx);
              }
          }
      }
      function patchVnode(oldVnode, vnode, insertedVnodeQueue) {
          var _a, _b, _c, _d, _e;
          const hook = (_a = vnode.data) === null || _a === void 0 ? void 0 : _a.hook;
          (_b = hook === null || hook === void 0 ? void 0 : hook.prepatch) === null || _b === void 0 ? void 0 : _b.call(hook, oldVnode, vnode);
          const elm = vnode.elm = oldVnode.elm;
          const oldCh = oldVnode.children;
          const ch = vnode.children;
          if (oldVnode === vnode)
              return;
          if (vnode.data !== undefined) {
              for (let i = 0; i < cbs.update.length; ++i)
                  cbs.update[i](oldVnode, vnode);
              (_d = (_c = vnode.data.hook) === null || _c === void 0 ? void 0 : _c.update) === null || _d === void 0 ? void 0 : _d.call(_c, oldVnode, vnode);
          }
          if (isUndef(vnode.text)) {
              if (isDef(oldCh) && isDef(ch)) {
                  if (oldCh !== ch)
                      updateChildren(elm, oldCh, ch, insertedVnodeQueue);
              }
              else if (isDef(ch)) {
                  if (isDef(oldVnode.text))
                      api.setTextContent(elm, '');
                  addVnodes(elm, null, ch, 0, ch.length - 1, insertedVnodeQueue);
              }
              else if (isDef(oldCh)) {
                  removeVnodes(elm, oldCh, 0, oldCh.length - 1);
              }
              else if (isDef(oldVnode.text)) {
                  api.setTextContent(elm, '');
              }
          }
          else if (oldVnode.text !== vnode.text) {
              if (isDef(oldCh)) {
                  removeVnodes(elm, oldCh, 0, oldCh.length - 1);
              }
              api.setTextContent(elm, vnode.text);
          }
          (_e = hook === null || hook === void 0 ? void 0 : hook.postpatch) === null || _e === void 0 ? void 0 : _e.call(hook, oldVnode, vnode);
      }
      return function patch(oldVnode, vnode) {
          let i, elm, parent;
          const insertedVnodeQueue = [];
          for (i = 0; i < cbs.pre.length; ++i)
              cbs.pre[i]();
          if (!isVnode(oldVnode)) {
              oldVnode = emptyNodeAt(oldVnode);
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
      data.ns = 'http://www.w3.org/2000/svg';
      if (sel !== 'foreignObject' && children !== undefined) {
          for (let i = 0; i < children.length; ++i) {
              const childData = children[i].data;
              if (childData !== undefined) {
                  addNS(childData, children[i].children, children[i].sel);
              }
          }
      }
  }
  function h(sel, b, c) {
      var data = {};
      var children;
      var text;
      var i;
      if (c !== undefined) {
          if (b !== null) {
              data = b;
          }
          if (array(c)) {
              children = c;
          }
          else if (primitive(c)) {
              text = c;
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
              text = b;
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
      if (sel[0] === 's' && sel[1] === 'v' && sel[2] === 'g' &&
          (sel.length === 3 || sel[3] === '.' || sel[3] === '#')) {
          addNS(data, children, sel);
      }
      return vnode(sel, data, children, text, undefined);
  }

  const xlinkNS = 'http://www.w3.org/1999/xlink';
  const xmlNS = 'http://www.w3.org/XML/1998/namespace';
  const colonChar = 58;
  const xChar = 120;
  function updateAttrs(oldVnode, vnode) {
      var key;
      var elm = vnode.elm;
      var oldAttrs = oldVnode.data.attrs;
      var attrs = vnode.data.attrs;
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
                  elm.setAttribute(key, '');
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
  const attributesModule = { create: updateAttrs, update: updateAttrs };

  function updateProps(oldVnode, vnode) {
      var key;
      var cur;
      var old;
      var elm = vnode.elm;
      var oldProps = oldVnode.data.props;
      var props = vnode.data.props;
      if (!oldProps && !props)
          return;
      if (oldProps === props)
          return;
      oldProps = oldProps || {};
      props = props || {};
      for (key in props) {
          cur = props[key];
          old = oldProps[key];
          if (old !== cur && (key !== 'value' || elm[key] !== cur)) {
              elm[key] = cur;
          }
      }
  }
  const propsModule = { create: updateProps, update: updateProps };

  function invokeHandler(handler, vnode, event) {
      if (typeof handler === 'function') {
          // call function handler
          handler.call(vnode, event, vnode);
      }
      else if (typeof handler === 'object') {
          // call multiple handlers
          for (var i = 0; i < handler.length; i++) {
              invokeHandler(handler[i], vnode, event);
          }
      }
  }
  function handleEvent(event, vnode) {
      var name = event.type;
      var on = vnode.data.on;
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
      var oldOn = oldVnode.data.on;
      var oldListener = oldVnode.listener;
      var oldElm = oldVnode.elm;
      var on = vnode && vnode.data.on;
      var elm = (vnode && vnode.elm);
      var name;
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
          var listener = vnode.listener = oldVnode.listener || createListener();
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
      destroy: updateEventListeners
  };

  // file: /Users/a.boeglin/Code/maddoc/madlib_modules/https___github_com_madlib_lang_madui_archive_master_zip/src/Main.mad
  const KEY_ENTER = ({ __constructor: "KEY_ENTER", __args: [  ] });
  const KEY_BACKSPACE = ({ __constructor: "KEY_BACKSPACE", __args: [  ] });
  const KEY_ANY = ({ __constructor: "KEY_ANY", __args: [  ] });
  const AbstractEvent = __curry__((a) => ({ __constructor: "AbstractEvent", __args: [ a ] }));
  const ClickEvent = __curry__((a) => ({ __constructor: "ClickEvent", __args: [ a ] }));
  const InputEvent = __curry__((a) => ({ __constructor: "InputEvent", __args: [ a ] }));
  const KeyPressEvent = __curry__((a) => ({ __constructor: "KeyPressEvent", __args: [ a ] }));
  const UrlEvent = __curry__((a) => ({ __constructor: "UrlEvent", __args: [ a ] }));
  const AttributeId = __curry__((a) => ({ __constructor: "AttributeId", __args: [ a ] }));
  const AttributeClass = __curry__((a) => ({ __constructor: "AttributeClass", __args: [ a ] }));
  const AttributeValue = __curry__((a) => ({ __constructor: "AttributeValue", __args: [ a ] }));
  const AttributePlaceholder = __curry__((a) => ({ __constructor: "AttributePlaceholder", __args: [ a ] }));
  const AttributeType = __curry__((a) => ({ __constructor: "AttributeType", __args: [ a ] }));
  const AttributeKey = __curry__((a) => ({ __constructor: "AttributeKey", __args: [ a ] }));
  const AttributeTo = __curry__((a) => ({ __constructor: "AttributeTo", __args: [ a ] }));
  const AttributeOnClick = __curry__((a) => ({ __constructor: "AttributeOnClick", __args: [ a ] }));
  const AttributeOnMouseOver = __curry__((a) => ({ __constructor: "AttributeOnMouseOver", __args: [ a ] }));
  const AttributeOnMouseOut = __curry__((a) => ({ __constructor: "AttributeOnMouseOut", __args: [ a ] }));
  const AttributeOnChange = __curry__((a) => ({ __constructor: "AttributeOnChange", __args: [ a ] }));
  const AttributeOnInput = __curry__((a) => ({ __constructor: "AttributeOnInput", __args: [ a ] }));
  const AttributeOnKeyPress = __curry__((a) => ({ __constructor: "AttributeOnKeyPress", __args: [ a ] }));

  const AppEnv = {
    patch: null,
    currentElement: null,
    currentState: null,
    rootView: null,
    onUrlChangedAction: null,
  };
  const KEY_CODE_MAPPINGS = fromList(([([13, KEY_ENTER]), ([8, KEY_BACKSPACE])]));
  const getKeyFromCode = __curry__((keyCode) => fromMaybe(KEY_ANY, get(keyCode, KEY_CODE_MAPPINGS)));
  const buildKeyPressEvent = event => {
    const key = getKeyFromCode(event.keyCode, KEY_CODE_MAPPINGS);

    return KeyPressEvent({ ...event, key })
  };

  const EventConstructors = Object.freeze({
    mouseout: AbstractEvent,
    mouseover: AbstractEvent,
    change: AbstractEvent,
    click: ClickEvent,
    input: InputEvent,
    keypress: buildKeyPressEvent
  });
  const getCurrentState = () => AppEnv.currentState;

  const runAction = updater => {
    AppEnv.currentState = updater(getCurrentState());
    const newElement = AppEnv.rootView(AppEnv.currentState);
    AppEnv.patch(AppEnv.currentElement, newElement);
    AppEnv.currentElement = newElement;
  };

  const wrapEventHandler = (ctor, handler) => {
    return event => {
      event.eventType = event.type;
      // So now calling an event handler gives us a list of wishes
      const wishes = handler(AppEnv.currentState)(ctor(event));
      wishes.forEach(fulfill(runAction)(runAction));
    }
  };

  const getAttributeTuple = attr =>
    [attr.__constructor.substr(9).toLowerCase(), attr.__args[0]];

  const ATTR_NAMES = [
    "id",
    "class",
    "placeholder",
    "type"
  ];
  const PROP_NAMES = [
    "value"
  ];

  const objectifyAttrs = attrs => attrs.reduce((obj, attr) => {
    const [attrName, attrValue] = getAttributeTuple(attr);
    if (attrName === "key") {
      return { ...obj, key: attrValue }
    } else if (ATTR_NAMES.includes(attrName)) {
      return { ...obj, attrs: { ...obj.attrs, [attrName]: attrValue }}
    } else if (PROP_NAMES.includes(attrName)) {
      return { ...obj, props: { ...obj.props, [attrName]: attrValue }}
    } else if (attrName.substr(0, 2) === "on") {
      const eventName = attrName.substr(2);
      const ctor = EventConstructors[eventName];
      return { ...obj, on: { ...obj.on, [eventName]: wrapEventHandler(ctor, attrValue) }}
    } else {
      return { ...obj, [attrName]: attrValue };
    }
  }, {});
  const className = AttributeClass;
  const placeholder = AttributePlaceholder;
  const inputType = AttributeType;
  const onInput = AttributeOnInput;
  const text = __curry__((content) => content);
  const div = __curry__((attrs, children) => h("div", objectifyAttrs(attrs), children));
  const span = __curry__((attrs, children) => h("span", objectifyAttrs(attrs), children));
  const h1 = __curry__((attrs, children) => h("h1", objectifyAttrs(attrs), children));
  const h2 = __curry__((attrs, children) => h("h2", objectifyAttrs(attrs), children));
  const h3 = __curry__((attrs, children) => h("h3", objectifyAttrs(attrs), children));
  const h4 = __curry__((attrs, children) => h("h4", objectifyAttrs(attrs), children));
  const h5 = __curry__((attrs, children) => h("h5", objectifyAttrs(attrs), children));
  const h6 = __curry__((attrs, children) => h("h6", objectifyAttrs(attrs), children));
  const p = __curry__((attrs, children) => h("p", objectifyAttrs(attrs), children));
  const ul = __curry__((attrs, children) => h("ul", objectifyAttrs(attrs), children));
  const ol = __curry__((attrs, children) => h("ol", objectifyAttrs(attrs), children));
  const li = __curry__((attrs, children) => h("li", objectifyAttrs(attrs), children));
  const header = __curry__((attrs, children) => h("header", objectifyAttrs(attrs), children));
  const main = __curry__((attrs, children) => h("main", objectifyAttrs(attrs), children));
  const section = __curry__((attrs, children) => h("section", objectifyAttrs(attrs), children));
  const aside = __curry__((attrs, children) => h("aside", objectifyAttrs(attrs), children));
  const empty$1 = __curry__((attrs, children) => null);
  const input = __curry__((attrs, children) => h("input", objectifyAttrs(attrs), children));
  const button = __curry__((attrs, children) => h("button", objectifyAttrs(attrs), children));
  const form = __curry__((attrs, children) => h("form", objectifyAttrs(attrs), children));
  const link = __curry__((attrs, children) => {
    const objAttrs = objectifyAttrs(attrs);
    const clickHandler = (event) => {
      event.preventDefault();

      if (objAttrs.to) {
        const builtUrl = document.location.origin + document.location.pathname + '\#' + objAttrs.to;

        history.pushState({}, '', builtUrl);

        if (AppEnv.onUrlChangedAction) {
          const wishes = AppEnv.onUrlChangedAction(getCurrentState())(UrlEvent({ url: objAttrs.to }));
          wishes.forEach(fulfill(runAction)(runAction));
        }
      }
    };

    return h("a", { ...objAttrs, on: { click: clickHandler }}, children);
  });
  const onUrlChanged = __curry__((action) => {
    AppEnv.onUrlChangedAction = action;

    window.onpopstate = function(event) {
      const path = document.location.hash.substr(1) || "/";
      const wishes = AppEnv.onUrlChangedAction(getCurrentState())(UrlEvent({ url: path }));
      wishes.forEach(fulfill(runAction)(runAction));
      console.log(document.location);
      // alert(`location: ${document.location}, state: ${JSON.stringify(event.state)}`)
    };
  });
  const syncAction = __curry__((stateUpdate, state, event) => ([Monad.Wish.of(__curry__((_) => stateUpdate(state, event)))]));
  const render = __curry__((view, initialState, containerId) => {
      const initialElement = view(initialState);
      const patch = init$1([attributesModule, propsModule, eventListenersModule]);
    patch(document.getElementById(containerId), initialElement);

    AppEnv.patch = patch;
    AppEnv.currentElement = initialElement;
    AppEnv.rootView = view;
    AppEnv.currentState = initialState;
      return ({ __constructor: "Unit", __args: [] });
  });

  // file: /Users/a.boeglin/Code/maddoc/src/Main.mad

  const Definition = __curry__((a, b, c, d, e) => ({ __constructor: "Definition", __args: [ a, b, c, d, e ] }));
  const TypeDefinition = __curry__((a, b, c) => ({ __constructor: "TypeDefinition", __args: [ a, b, c ] }));
  const AliasDefinition = __curry__((a, b, c) => ({ __constructor: "AliasDefinition", __args: [ a, b, c ] }));
  const InterfaceDefinition = __curry__((a, b, c, d) => ({ __constructor: "InterfaceDefinition", __args: [ a, b, c, d ] }));
  const InstanceDefinition = __curry__((a, b) => ({ __constructor: "InstanceDefinition", __args: [ a, b ] }));
  const ModuleDocumentation = __curry__((a, b, c, d, e, f, g) => ({ __constructor: "ModuleDocumentation", __args: [ a, b, c, d, e, f, g ] }));
  const ExpressionItem = __curry__((a, b) => ({ __constructor: "ExpressionItem", __args: [ a, b ] }));
  const TypeItem = __curry__((a, b) => ({ __constructor: "TypeItem", __args: [ a, b ] }));
  const AliasItem = __curry__((a, b) => ({ __constructor: "AliasItem", __args: [ a, b ] }));
  const InterfaceItem = __curry__((a, b) => ({ __constructor: "InterfaceItem", __args: [ a, b ] }));
  const InstanceItem = __curry__((a, b) => ({ __constructor: "InstanceItem", __args: [ a, b ] }));
  const docJson = "{\n  \"modules\": [\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/Compare.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        {\n          \"type\": \"Alias\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"name\": \"ComparisonResult\",\n          \"params\": \"\",\n          \"aliasedType\": \"Number\"\n        }\n      ],\n      \"interfaces\": [\n        {\n          \"name\": \"Comparable\",\n          \"vars\": \"a\",\n          \"constraints\": \"\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"methods\": [\n            \"compare :: a -> a -> ComparisonResult\"\n          ]\n        }\n      ],\n      \"instances\": [\n        {\n          \"name\": \"Comparable\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"\",\n          \"declaration\": \"Comparable Number\"\n        },\n        {\n          \"name\": \"Comparable\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"\",\n          \"declaration\": \"Comparable String\"\n        },\n        {\n          \"name\": \"Comparable\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"\",\n          \"declaration\": \"Comparable Boolean\"\n        }\n      ],\n      \"expressions\": [\n        {\n          \"name\": \"MORE\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"Number\"\n        },\n        {\n          \"name\": \"LESS\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"Number\"\n        },\n        {\n          \"name\": \"EQUAL\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"Number\"\n        }\n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/Dictionary.spec.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        \n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/Monad.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        {\n          \"name\": \"Monad\",\n          \"vars\": \"m\",\n          \"constraints\": \"Applicative m\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"methods\": [\n            \"chain :: (a -> m b) -> m a -> m b\",\n            \"of :: a -> m a\"\n          ]\n        }\n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"name\": \"andDo\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"m a -> m b -> m a\"\n        }\n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/Maybe.mad\",\n      \"description\": \"Maybe is one of the major data types in functional programming and serves to\\nrepresent and encapsulate values which may not exist.\\n\\nWhereas other languages may use values like `null` / `nil` / `undefined`, Madlib provides this monadic\\nstructure to encapsulate uncertainty in a computationally safe way.\",\n      \"typeDeclarations\": [\n        {\n          \"type\": \"ADT\",\n          \"description\": \"The Maybe type is used to model uncertain values. It is ideal for values which may not exist,\\nand is used to provide safety and certainty when calculating against potentially uncertain or incorrect inputs.\",\n          \"example\": \"Just(3)     // Just(3) :: Maybe Number\\nJust(false) // Just(false) :: Maybe Boolean\\nNothing     // Nothing :: Maybe a\",\n          \"since\": \"0.0.5\",\n          \"name\": \"Maybe\",\n          \"params\": \"a\",\n          \"constructors\": [\n            \"Just a\",\n            \"Nothing \"\n          ]\n        }\n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"name\": \"Functor\",\n          \"description\": \"Map over a given Maybe, returning either Just the transformed value\\n(in the case of successful computation) or Nothing.\",\n          \"example\": \"map((x) => x + 1, Just(0)) // Just(1)\\nmap((x) => x + 1, Nothing) // Nothing\",\n          \"since\": \"0.0.6\",\n          \"constraints\": \"\",\n          \"declaration\": \"Functor Maybe\"\n        },\n        {\n          \"name\": \"Applicative\",\n          \"description\": \"Apply Maybe a function to Maybe a value.\\nIf either value is Nothing, return Nothing.\",\n          \"example\": \"ap(Just((x) => x + 1), Just(0)) // Just(1)\\nap(Just((x) => x + 1), Nothing) // Nothing\\nap(Nothing, Just(3))            // Nothing\",\n          \"since\": \"0.0.6\",\n          \"constraints\": \"\",\n          \"declaration\": \"Applicative Maybe\"\n        },\n        {\n          \"name\": \"Monad\",\n          \"description\": \"Use `chain` to avoid a double-wrapped Maybe.\\nInstead of a Just of a Just, `chain` will flatten the contained transformation to be only one Monad deep.\",\n          \"example\": \"chain((x) => Just(x + 1), Just(1)) // Just(2)\",\n          \"since\": \"0.0.6\",\n          \"constraints\": \"\",\n          \"declaration\": \"Monad Maybe\"\n        },\n        {\n          \"name\": \"Show\",\n          \"description\": \"Use `show` to transform a Maybe to a string.\",\n          \"example\": \"show(Just(3))                   // \\\"Just 3\\\"\\nshow((Nothing :: Maybe Number)) // \\\"Nothing\\\"\",\n          \"since\": \"0.0.6\",\n          \"constraints\": \"Show a\",\n          \"declaration\": \"Show (Maybe a)\"\n        }\n      ],\n      \"expressions\": [\n        {\n          \"name\": \"fromMaybe\",\n          \"description\": \"Extricate the value contained in a Just, or the given fallback value if given a Nothing\",\n          \"example\": \"fromMaybe(3, Just(4)) // 4\\nfromMaybe(3, Nothing) // 3\",\n          \"since\": \"0.0.5\",\n          \"type\": \"a -> Maybe a -> a\"\n        },\n        {\n          \"name\": \"isJust\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"Maybe a -> Boolean\"\n        }\n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/MonadWriter.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        {\n          \"name\": \"MonadWriter\",\n          \"vars\": \"w m\",\n          \"constraints\": \"(Monoid w, Monad m)\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"methods\": [\n            \"tell :: w -> m ()\"\n          ]\n        }\n      ],\n      \"instances\": [\n        {\n          \"name\": \"MonadWriter\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"(Monoid w, Monad m)\",\n          \"declaration\": \"MonadWriter w (WriterT w m)\"\n        },\n        {\n          \"name\": \"MonadWriter\",\n          \"description\": \"instance MonadWriter for StateT\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"(Monoid w, Monad m, MonadWriter w m)\",\n          \"declaration\": \"MonadWriter w (StateT s m)\"\n        }\n      ],\n      \"expressions\": [\n        \n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/Maybe.spec.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        \n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/Applicative.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        {\n          \"name\": \"Applicative\",\n          \"vars\": \"m\",\n          \"constraints\": \"Functor m\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"methods\": [\n            \"ap :: m (a -> b) -> m a -> m b\",\n            \"pure :: a -> m a\"\n          ]\n        }\n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        \n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/LcovDotInfo.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        {\n          \"type\": \"ADT\",\n          \"description\": \"LcovLine type\",\n          \"example\": \"\",\n          \"since\": \"v0.0.5\",\n          \"name\": \"LcovLine\",\n          \"params\": \"\",\n          \"constructors\": [\n            \"TN String\",\n            \"SF String\",\n            \"FN Number String\",\n            \"FNDA Number String\",\n            \"FNF Number\",\n            \"FNH Number\",\n            \"DA Number Number\",\n            \"LF Number\",\n            \"LH Number\"\n          ]\n        }\n      ],\n      \"aliases\": [\n        {\n          \"type\": \"Alias\",\n          \"description\": \"LcovLine type\",\n          \"example\": \"\",\n          \"since\": \"v0.0.5\",\n          \"name\": \"LcovSection\",\n          \"params\": \"\",\n          \"aliasedType\": \"{ das :: List LcovLine, fndas :: List LcovLine, fnf :: LcovLine, fnh :: LcovLine, fns :: List LcovLine, lf :: LcovLine, lh :: LcovLine, sf :: LcovLine, tn :: LcovLine }\"\n        },\n        {\n          \"type\": \"Alias\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"name\": \"LcovInfo\",\n          \"params\": \"\",\n          \"aliasedType\": \"(List LcovSection)\"\n        }\n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"name\": \"getPath\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"{ das :: List LcovLine, fndas :: List LcovLine, fnf :: LcovLine, fnh :: LcovLine, fns :: List LcovLine, lf :: LcovLine, lh :: LcovLine, sf :: LcovLine, tn :: LcovLine } -> String\"\n        },\n        {\n          \"name\": \"parseLcov\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"String -> List { das :: List LcovLine, fndas :: List LcovLine, fnf :: LcovLine, fnh :: LcovLine, fns :: List LcovLine, lf :: LcovLine, lh :: LcovLine, sf :: LcovLine, tn :: LcovLine }\"\n        },\n        {\n          \"name\": \"stringify\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"List { das :: List LcovLine, fndas :: List LcovLine, fnf :: LcovLine, fnh :: LcovLine, fns :: List LcovLine, lf :: LcovLine, lh :: LcovLine, sf :: LcovLine, tn :: LcovLine } -> String\"\n        }\n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/Function.spec.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        \n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/String.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"name\": \"Semigroup\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"\",\n          \"declaration\": \"Semigroup String\"\n        },\n        {\n          \"name\": \"Monoid\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"\",\n          \"declaration\": \"Monoid String\"\n        },\n        {\n          \"name\": \"Show\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"\",\n          \"declaration\": \"Show String\"\n        }\n      ],\n      \"expressions\": [\n        {\n          \"name\": \"replace\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"String -> String -> String -> String\"\n        },\n        {\n          \"name\": \"split\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"String -> String -> List String\"\n        },\n        {\n          \"name\": \"lines\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"String -> List String\"\n        },\n        {\n          \"name\": \"mapChars\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"(String -> String) -> String -> String\"\n        }\n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/Binary.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        {\n          \"type\": \"ADT\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"name\": \"ByteWord\",\n          \"params\": \"\",\n          \"constructors\": [\n            \"Int8Bit a\",\n            \"Int16Bit a\",\n            \"Int32Bit a\"\n          ]\n        },\n        {\n          \"type\": \"ADT\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"name\": \"ByteArray\",\n          \"params\": \"\",\n          \"constructors\": [\n            \"ByteArray (List ByteWord)\"\n          ]\n        }\n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        \n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/Identity.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        {\n          \"type\": \"ADT\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"name\": \"Identity\",\n          \"params\": \"a\",\n          \"constructors\": [\n            \"Identity a\"\n          ]\n        },\n        {\n          \"type\": \"ADT\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"name\": \"IdentityT\",\n          \"params\": \"m a\",\n          \"constructors\": [\n            \"IdentityT (m a)\"\n          ]\n        }\n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"name\": \"Functor\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"\",\n          \"declaration\": \"Functor Identity\"\n        },\n        {\n          \"name\": \"Applicative\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"\",\n          \"declaration\": \"Applicative Identity\"\n        },\n        {\n          \"name\": \"Monad\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"\",\n          \"declaration\": \"Monad Identity\"\n        },\n        {\n          \"name\": \"Functor\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"Functor m\",\n          \"declaration\": \"Functor (IdentityT m)\"\n        },\n        {\n          \"name\": \"Applicative\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"Applicative m\",\n          \"declaration\": \"Applicative (IdentityT m)\"\n        },\n        {\n          \"name\": \"Monad\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"Monad m\",\n          \"declaration\": \"Monad (IdentityT m)\"\n        },\n        {\n          \"name\": \"MonadTrans\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"Monad m\",\n          \"declaration\": \"MonadTrans m IdentityT\"\n        }\n      ],\n      \"expressions\": [\n        {\n          \"name\": \"runIdentity\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"Identity a -> a\"\n        },\n        {\n          \"name\": \"runIdentityT\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"IdentityT m a -> m a\"\n        }\n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/Either.spec.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        \n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/Math.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"name\": \"add\",\n          \"description\": \"Sum two numbers\",\n          \"example\": \"\",\n          \"since\": \"0.0.5\",\n          \"type\": \"Number -> Number -> Number\"\n        },\n        {\n          \"name\": \"substract\",\n          \"description\": \"Calculate the difference between two numbers\",\n          \"example\": \"\",\n          \"since\": \"0.0.5\",\n          \"type\": \"Number -> Number -> Number\"\n        },\n        {\n          \"name\": \"multiply\",\n          \"description\": \"Return the product of two numbers\",\n          \"example\": \"\",\n          \"since\": \"0.0.5\",\n          \"type\": \"Number -> Number -> Number\"\n        },\n        {\n          \"name\": \"divide\",\n          \"description\": \"Calculate how many times a number can be contained in another number\",\n          \"example\": \"\",\n          \"since\": \"0.0.5\",\n          \"type\": \"Number -> Number -> Number\"\n        },\n        {\n          \"name\": \"mod\",\n          \"description\": \"Return the euclidean division of one number by another\",\n          \"example\": \"\",\n          \"since\": \"0.0.5\",\n          \"type\": \"Number -> Number -> Number\"\n        }\n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/TestTools.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        {\n          \"type\": \"ADT\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"name\": \"AssertionError\",\n          \"params\": \"\",\n          \"constructors\": [\n            \"AssertionError a a\"\n          ]\n        }\n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"name\": \"collectCoverage\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"List HitPoint -> Wish a (List { cols :: List <String, String>, id :: String })\"\n        },\n        {\n          \"name\": \"test\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"String -> (String -> Wish AssertionError String) -> ()\"\n        },\n        {\n          \"name\": \"assertEquals\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"a -> a -> Wish AssertionError String\"\n        }\n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/String.spec.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        \n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/Semigroup.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        {\n          \"name\": \"Semigroup\",\n          \"vars\": \"a\",\n          \"constraints\": \"\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"methods\": [\n            \"assoc :: a -> a -> a\"\n          ]\n        }\n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        \n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/WriterT.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        {\n          \"type\": \"ADT\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"name\": \"WriterT\",\n          \"params\": \"w m a\",\n          \"constructors\": [\n            \"WriterT (m <a, w>)\"\n          ]\n        }\n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"name\": \"Functor\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"Functor m\",\n          \"declaration\": \"Functor (WriterT w m)\"\n        },\n        {\n          \"name\": \"Applicative\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"(Applicative m, Monoid w)\",\n          \"declaration\": \"Applicative (WriterT w m)\"\n        },\n        {\n          \"name\": \"Monad\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"(Monoid w, Monad m)\",\n          \"declaration\": \"Monad (WriterT w m)\"\n        },\n        {\n          \"name\": \"MonadTrans\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"(Monad m, Monoid w)\",\n          \"declaration\": \"MonadTrans m (WriterT w)\"\n        }\n      ],\n      \"expressions\": [\n        {\n          \"name\": \"runWriterT\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"WriterT a m b -> m <b, a>\"\n        }\n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/Tuple.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"name\": \"Show\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"(Show a, Show b)\",\n          \"declaration\": \"Show (<a, b>)\"\n        },\n        {\n          \"name\": \"Show\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"(Show a, Show b, Show c)\",\n          \"declaration\": \"Show (<a, b, c>)\"\n        },\n        {\n          \"name\": \"Show\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"(Show a, Show b, Show c, Show d)\",\n          \"declaration\": \"Show (<a, b, c, d>)\"\n        }\n      ],\n      \"expressions\": [\n        {\n          \"name\": \"fst\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"<a, b> -> a\"\n        },\n        {\n          \"name\": \"snd\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"<a, b> -> b\"\n        }\n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/System.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        {\n          \"type\": \"ADT\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"name\": \"CommandError\",\n          \"params\": \"\",\n          \"constructors\": [\n            \"CommandError Number String\"\n          ]\n        }\n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"name\": \"getCommandErrorMessage\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"CommandError -> String\"\n        },\n        {\n          \"name\": \"exec\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"String -> Wish CommandError String\"\n        },\n        {\n          \"name\": \"spawn\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"String -> Wish CommandError Number\"\n        },\n        {\n          \"name\": \"getArgs\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"Wish String (List String)\"\n        },\n        {\n          \"name\": \"getEnv\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"String -> Wish String String\"\n        },\n        {\n          \"name\": \"getCurrentPath\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"Wish String String\"\n        },\n        {\n          \"name\": \"getExecutablePath\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"Wish String String\"\n        }\n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/List.mad\",\n      \"description\": \"List utility functions.\\nIn this module you\'ll find everything that is related to List and make working with lists convenient.\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"name\": \"Functor\",\n          \"description\": \"As with all implementations of `map`, transform each value contained in the List.\",\n          \"example\": \"map((x) => x * 2, [1, 2, 3]) // [2, 4, 6]\",\n          \"since\": \"v0.0.6\",\n          \"constraints\": \"\",\n          \"declaration\": \"Functor List\"\n        },\n        {\n          \"name\": \"Applicative\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"\",\n          \"declaration\": \"Applicative List\"\n        },\n        {\n          \"name\": \"Monad\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"\",\n          \"declaration\": \"Monad List\"\n        },\n        {\n          \"name\": \"Semigroup\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"\",\n          \"declaration\": \"Semigroup (List a)\"\n        },\n        {\n          \"name\": \"Monoid\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"\",\n          \"declaration\": \"Monoid (List a)\"\n        },\n        {\n          \"name\": \"Show\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"Show a\",\n          \"declaration\": \"Show (List a)\"\n        }\n      ],\n      \"expressions\": [\n        {\n          \"name\": \"singleton\",\n          \"description\": \"Constructor function that creates a list with length 1. It takes an item of any type, and returns a List of that type.\",\n          \"example\": \"singleton(1)   // [1]\\nsingleton(\\\"1\\\") // [\\\"1\\\"]\",\n          \"since\": \"0.0.5\",\n          \"type\": \"a -> List a\"\n        },\n        {\n          \"name\": \"unlines\",\n          \"description\": \"Transform a List of Strings into a single String,\\nby adding newlines between each item in the List.\",\n          \"example\": \"unlines([\\\"line1\\\", \\\"line2\\\", \\\"line3\\\"]) // \\\"line1\\\\nline2\\\\nline3\\\"\",\n          \"since\": \"0.0.5\",\n          \"type\": \"List String -> String\"\n        },\n        {\n          \"name\": \"intercalate\",\n          \"description\": \"Inserts a value between all elements of a list.\",\n          \"example\": \"intercalate(\\\"and\\\", [\\\"cats\\\", \\\"dogs\\\"]) // [\\\"cats\\\", \\\"and\\\", \\\"dogs\\\"]\",\n          \"since\": \"0.0.5\",\n          \"type\": \"a -> List a -> List a\"\n        },\n        {\n          \"name\": \"join\",\n          \"description\": \"Joins a list with a separator and returns a String.\\n\\nIMPORTANT:\\nWhen coming from JS, beware that there is a small difference with Array.prototype.join.\\nIn JS, join relies on its dynamic nature and accepts any type as being valid, and transforms it\\nto a string for you. In Madlib you need to provide an instance of Show for your custom types, and\\nfor Madlib types (eg. tuples, List, Boolean, Maybe) it uses the Show instance defined for them.\",\n          \"example\": \"join(\\\" and \\\", [\\\"cats\\\", \\\"dogs\\\"])   // \\\"cats and dogs\\\"\\njoin(\\\"\\\", [\\\"one\\\", \\\"two\\\", \\\"three\\\"]) // \\\"onetwothree\\\"\",\n          \"since\": \"0.0.5\",\n          \"type\": \"a -> List b -> String\"\n        },\n        {\n          \"name\": \"mapWithIndex\",\n          \"description\": \"Applies a function to each item of a list. This function receives both the current element\\nand its index as parameters, and whatever is returned is used as the new element\'s value.\",\n          \"example\": \"mapWithIndex((x, i) => x ++ show(i), [\\\"a\\\", \\\"b\\\", \\\"c\\\"]) // [\\\"a0\\\", \\\"b1\\\", \\\"c2\\\"]\",\n          \"since\": \"0.0.5\",\n          \"type\": \"(a -> Number -> b) -> List a -> List b\"\n        },\n        {\n          \"name\": \"concat\",\n          \"description\": \"Merge two Lists together.\",\n          \"example\": \"concat([1, 2, 3], [4, 5, 6]) // [1, 2, 3, 4, 5, 6]\",\n          \"since\": \"0.0.5\",\n          \"type\": \"List a -> List a -> List a\"\n        },\n        {\n          \"name\": \"append\",\n          \"description\": \"Add an item to the end of a List.\",\n          \"example\": \"append(2, [1]) // [1, 2]\",\n          \"since\": \"0.0.5\",\n          \"type\": \"a -> List a -> List a\"\n        },\n        {\n          \"name\": \"last\",\n          \"description\": \"Safely access the last value in a List (wrapped in Just), or Nothing if the array is empty.\",\n          \"example\": \"last([1, 2, 3]) // Just(3)\\nlast([])        // Nothing\",\n          \"since\": \"0.0.5\",\n          \"type\": \"List a -> Maybe a\"\n        },\n        {\n          \"name\": \"first\",\n          \"description\": \"Safely access the first value in a List (wrapped in Just), or Nothing if the array is empty.\",\n          \"example\": \"first([1, 2, 3]) // Just(1)\\nfirst([])        // Nothing\",\n          \"since\": \"0.0.5\",\n          \"type\": \"List a -> Maybe a\"\n        },\n        {\n          \"name\": \"init\",\n          \"description\": \"Returns a List minus its last item.\",\n          \"example\": \"init([1, 2, 3]) // [1, 2]\\ninit([])        // []\",\n          \"since\": \"0.0.5\",\n          \"type\": \"List a -> List a\"\n        },\n        {\n          \"name\": \"nth\",\n          \"description\": \"Safely access a value at a given index in a List (wrapped in Just), or Nothing.\",\n          \"example\": \"nth(1, [1, 2, 3]) // Just(2)\\nnth(3, [1, 2, 3]) // Nothing\",\n          \"since\": \"0.0.5\",\n          \"type\": \"Number -> List a -> Maybe a\"\n        },\n        {\n          \"name\": \"reduceR\",\n          \"description\": \"Aggregate a single value by iterating over a list, from right-to-left.\\nTakes three parameters:\\n- an aggregation function, which takes the current aggregated value and the next item in the list.\\n- an initial value (which must share the same type as the value returned from the transformation function)\\n- a List\",\n          \"example\": \"\",\n          \"since\": \"0.0.5\",\n          \"type\": \"(a -> b -> a) -> a -> List b -> a\"\n        },\n        {\n          \"name\": \"reduceL\",\n          \"description\": \"Aggregate a single value by iterating over a list, from left-to-right.\\nTakes three parameters:\\n- an aggregation function, which takes the current aggregated value and the next item in the list.\\n- an initial value (which must share the same type as the value returned from the transformation function)\\n- a List\",\n          \"example\": \"\",\n          \"since\": \"0.0.5\",\n          \"type\": \"(a -> b -> a) -> a -> List b -> a\"\n        },\n        {\n          \"name\": \"reduce\",\n          \"description\": \"Aggregate a single value by iterating over a list, from left-to-right.\\nTakes three parameters:\\n- an aggregation function, which takes the current aggregated value and the next item in the list.\\n- an initial value (which must share the same type as the value returned from the transformation function)\\n- a List\",\n          \"example\": \"reduce((a, b) => a + b, 0, [1, 2, 3]) // 6\",\n          \"since\": \"0.0.5\",\n          \"type\": \"(a -> b -> a) -> a -> List b -> a\"\n        },\n        {\n          \"name\": \"filter\",\n          \"description\": \"Iterate over a List, selecting only values which are matched by predicate function.\",\n          \"example\": \"filter((a) => a % 2 == 0, [1, 2, 3, 4, 5, 6]) // [2, 4, 6]\",\n          \"since\": \"0.0.5\",\n          \"type\": \"(a -> Boolean) -> List a -> List a\"\n        },\n        {\n          \"name\": \"reject\",\n          \"description\": \"Iterate over a List, selecting only values which are not matched by a predicate function.\\nThis is the complement of `filter`.\",\n          \"example\": \"reject((a) => a % 2 == 0, [1, 2, 3, 4, 5, 6]) // [1, 3, 5]\",\n          \"since\": \"0.0.5\",\n          \"type\": \"(a -> Boolean) -> List a -> List a\"\n        },\n        {\n          \"name\": \"find\",\n          \"description\": \"Safely access the first element in a List for which a given\\npredicate function returns true; returning Just the found value or Nothing.\",\n          \"example\": \"find((a) => a % 2 == 0, [1, 2, 3, 4, 5, 6]) // Just(2)\\nfind((a) => a == 0, [1, 2, 3, 4, 5, 6])     // Nothing\",\n          \"since\": \"0.0.5\",\n          \"type\": \"(a -> Boolean) -> List a -> Maybe a\"\n        },\n        {\n          \"name\": \"len\",\n          \"description\": \"Get the length of a List.\",\n          \"example\": \"len([1, 2, 3]) // 3\",\n          \"since\": \"0.0.5\",\n          \"type\": \"List a -> Number\"\n        },\n        {\n          \"name\": \"slice\",\n          \"description\": \"Cut a contiguous segment from a List, from start index to end index.\",\n          \"example\": \"\",\n          \"since\": \"0.0.5\",\n          \"type\": \"Number -> Number -> List a -> List a\"\n        },\n        {\n          \"name\": \"isEmpty\",\n          \"description\": \"Test whether a given List has no elements.\",\n          \"example\": \"\",\n          \"since\": \"0.0.5\",\n          \"type\": \"List a -> Boolean\"\n        },\n        {\n          \"name\": \"uniqueBy\",\n          \"description\": \"Using a comparison function, select values which are distinct.\",\n          \"example\": \"uniqueBy(\\n(a, b) => a.id == b.id,\\n[\\n{ id: 1, name: \\\"John\\\" },\\n{ id: 2, name: \\\"Paul\\\" },\\n{ id: 1, name: \\\"George\\\" },\\n{ id: 4, name: \\\"Ringo\\\" }\\n]\\n)\\n// [{ id: 1, name: \\\"John\\\" }, { id: 2, name: \\\"Paul\\\" }, { id: 4, name: \\\"Ringo\\\" }]\",\n          \"since\": \"0.0.5\",\n          \"type\": \"(a -> a -> Boolean) -> List a -> List a\"\n        },\n        {\n          \"name\": \"sortBy\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"(a -> a -> Number) -> List a -> List a\"\n        },\n        {\n          \"name\": \"sort\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"List a -> List a\"\n        },\n        {\n          \"name\": \"sortAsc\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"List a -> List a\"\n        },\n        {\n          \"name\": \"sortDesc\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"List a -> List a\"\n        },\n        {\n          \"name\": \"flatten\",\n          \"description\": \"Remove nested Lists from a given List, one level deep.\",\n          \"example\": \"\",\n          \"since\": \"0.0.5\",\n          \"type\": \"List (List a) -> List a\"\n        },\n        {\n          \"name\": \"zip\",\n          \"description\": \"Take two lists and combine them pair-wise, such that the elements of List x become the first value\\nof the resulting Tuple, and the elements of List y become the the second value per element.\",\n          \"example\": \"zip([\\\"a\\\", \\\"b\\\", \\\"c\\\"], [1, 2, 3]) // [<\\\"a\\\", 1>, <\\\"b\\\", 2>, <\\\"c\\\", 3>]\",\n          \"since\": \"0.0.5\",\n          \"type\": \"List a -> List b -> List <a, b>\"\n        },\n        {\n          \"name\": \"includes\",\n          \"description\": \"Test a given List to see whether it contains a given static value.\",\n          \"example\": \"\",\n          \"since\": \"0.6.0\",\n          \"type\": \"a -> List a -> Boolean\"\n        },\n        {\n          \"name\": \"drop\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"Number -> List a -> List a\"\n        }\n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/Either.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        {\n          \"type\": \"ADT\",\n          \"description\": \"An Either is a type which allows for modeling a disjoint union (\\\"a\\\" or \\\"b\\\", but in a safe way).\\nAn Either can be a Right: a successful computation, or a Left: an unsuccessful one.\\nBy capturing a value in this way, we can separate the transformation from the value contained in the Either,\\nmaking it easier to reason about and safer to pass around. (mnemonic: \\\"right\\\" == \\\"correct\\\")\",\n          \"example\": \"\",\n          \"since\": \"0.0.5\",\n          \"name\": \"Either\",\n          \"params\": \"e a\",\n          \"constructors\": [\n            \"Left e\",\n            \"Right a\"\n          ]\n        }\n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"name\": \"Functor\",\n          \"description\": \"We can map over an Either: if it is a Right, it transforms the contained value. \\nIf it is a Left it does nothing. In this way we can freely frame the computations we want\\nto perform without being concerned as to whether the transformation was succcessful until later.\",\n          \"example\": \"map((x) => x + 1, Right(3))      // Right(4)\\nmap((x) => x + 1, Left(\\\"error\\\")) // Left(\\\"error\\\")\",\n          \"since\": \"0.0.6\",\n          \"constraints\": \"\",\n          \"declaration\": \"Functor (Either e)\"\n        },\n        {\n          \"name\": \"Applicative\",\n          \"description\": \"By wrapping a function in `ap` we can apply it from one Either to another Either.\\nIf both contained values are Rights, it returns a Right of the result. If either Either is a Left,\\nthat value persists and the contained value is not transformed.\",\n          \"example\": \"ap(Right((x) => x + 1), Right(2))     // Right(3)\\nap(Left(\\\"oups\\\"), Right(2))            // Left(\\\"oups\\\")\\nap(Right((x) => x + 1), Left(\\\"oups\\\")) // Left(\\\"oups\\\")\",\n          \"since\": \"0.0.6\",\n          \"constraints\": \"\",\n          \"declaration\": \"Applicative (Either e)\"\n        },\n        {\n          \"name\": \"Monad\",\n          \"description\": \"Use `chain` when you want to flatten a potentially double-wrapped Either.\\nInstead of a Right of a Right or a Right of a Left, `chain` will flatten the contained transformation\\nand return a Right (if successful) or a Left (if unsuccessful)\",\n          \"example\": \"chain((x) => Right(x + 1), Right(1))   // Right(2)\\nchain((x) => Right(x + 1), Left(\\\"no\\\")) // Left(\\\"no\\\")\",\n          \"since\": \"0.0.6\",\n          \"constraints\": \"\",\n          \"declaration\": \"Monad (Either e)\"\n        },\n        {\n          \"name\": \"Show\",\n          \"description\": \"Use `show` to transform an Either to a string.\",\n          \"example\": \"show((Right(3) :: Either Number Number)) // \\\"Right 3\\\"\\nshow((Left(3) :: Either Number Number))  // \\\"Left 3\\\"\",\n          \"since\": \"0.0.6\",\n          \"constraints\": \"(Show e, Show a)\",\n          \"declaration\": \"Show (Either e a)\"\n        }\n      ],\n      \"expressions\": [\n        {\n          \"name\": \"mapRight\",\n          \"description\": \"An alias for `map`, use `mapRight` to transform the interior value of a Right.\",\n          \"example\": \"mapRight((x) => x + 1, Right(3)) // Right(4)\",\n          \"since\": \"0.0.5\",\n          \"type\": \"(a -> b) -> Either c a -> Either c b\"\n        },\n        {\n          \"name\": \"mapLeft\",\n          \"description\": \"Unlike most other Either functions, `mapLeft` is one of the few which freely transforms the\\ninterior value of a Left. (It is the left-branch analogue of `mapRight`.)\",\n          \"example\": \"mapLeft((x) => x + 1, Left(3))  // Left(4)\\nmapLeft((x) => x + 1, Right(3)) // Right(3)\",\n          \"since\": \"0.0.5\",\n          \"type\": \"(a -> b) -> Either a c -> Either b c\"\n        },\n        {\n          \"name\": \"isLeft\",\n          \"description\": \"Returns true if it is a Left, false otherwise.\",\n          \"example\": \"isLeft(Left(1))  // true\\nisLeft(Right(1)) // false\",\n          \"since\": \"0.0.5\",\n          \"type\": \"Either a b -> Boolean\"\n        },\n        {\n          \"name\": \"isRight\",\n          \"description\": \"Returns true if it is a Right, false otherwise.\",\n          \"example\": \"isRight(Left(1))  // false\\nisRight(Right(1)) // true\",\n          \"since\": \"0.0.5\",\n          \"type\": \"Either a b -> Boolean\"\n        },\n        {\n          \"name\": \"fromRight\",\n          \"description\": \"Pull the value contained in the Either if it is a Right, or the given\\nfallback value if it is a Left.\",\n          \"example\": \"fromRight(1, Right(4)) // 4\\nfromRight(1, Left(4))  // 1\",\n          \"since\": \"0.0.5\",\n          \"type\": \"a -> Either b a -> a\"\n        }\n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/Http.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        {\n          \"type\": \"ADT\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"name\": \"Body\",\n          \"params\": \"\",\n          \"constructors\": [\n            \"TextBody String\",\n            \"BinaryBody B.ByteArray\"\n          ]\n        },\n        {\n          \"type\": \"ADT\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"name\": \"Response\",\n          \"params\": \"\",\n          \"constructors\": [\n            \"Response { body :: Body }\"\n          ]\n        }\n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"name\": \"get\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"String -> Wish a Response\"\n        }\n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/List.spec.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        \n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/Function.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"name\": \"complement\",\n          \"description\": \"Wraps a predicate function and make it return the inverse. So if for a value\\nthe function would normally return true, after \\\"complemented\\\" it would return\\nfalse and vice versa.\",\n          \"example\": \"complement((x) => x % 2 == 0)(2) // false\",\n          \"since\": \"0.0.5\",\n          \"type\": \"(a -> Boolean) -> a -> Boolean\"\n        },\n        {\n          \"name\": \"always\",\n          \"description\": \"It always returns the first supplied parameter no matter what. It is especially\\npractical during function composition when you want to discard whatever value\\nis passed to a function and always return the same thing.\",\n          \"example\": \"always(true, \\\"1\\\")            // true\\nmap(always(true), [1, 2 ,3]) // [true, true, true]\",\n          \"since\": \"0.0.5\",\n          \"type\": \"a -> b -> a\"\n        },\n        {\n          \"name\": \"identity\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"a -> a\"\n        },\n        {\n          \"name\": \"equals\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"a -> a -> Boolean\"\n        },\n        {\n          \"name\": \"ifElse\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"(a -> Boolean) -> (a -> b) -> (a -> b) -> a -> b\"\n        },\n        {\n          \"name\": \"when\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"(a -> Boolean) -> (a -> a) -> a -> a\"\n        },\n        {\n          \"name\": \"not\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"Boolean -> Boolean\"\n        },\n        {\n          \"name\": \"flip\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"(a -> b -> c) -> b -> a -> c\"\n        },\n        {\n          \"name\": \"memoize\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"(a -> b) -> a -> b\"\n        },\n        {\n          \"name\": \"memoize2\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"(a -> b -> c) -> a -> b -> c\"\n        },\n        {\n          \"name\": \"memoize3\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"(a -> b -> c -> d) -> a -> b -> c -> d\"\n        }\n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/FileSystem.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        {\n          \"type\": \"ADT\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"name\": \"Data\",\n          \"params\": \"\",\n          \"constructors\": [\n            \"TextData String\",\n            \"BinaryData B.ByteArray\"\n          ]\n        }\n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"name\": \"writeFile\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"String -> Data -> Wish a String\"\n        },\n        {\n          \"name\": \"readFile\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"String -> Wish a String\"\n        },\n        {\n          \"name\": \"exists\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"String -> Wish a String\"\n        }\n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/IO.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        {\n          \"type\": \"Alias\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"name\": \"Row\",\n          \"params\": \"\",\n          \"aliasedType\": \"{ cols :: List <String, String>, id :: String }\"\n        }\n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"name\": \"log\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"a -> a\"\n        },\n        {\n          \"name\": \"trace\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"String -> a -> a\"\n        },\n        {\n          \"name\": \"err\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"a -> a\"\n        },\n        {\n          \"name\": \"warn\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"a -> a\"\n        },\n        {\n          \"name\": \"inspect\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"a -> a\"\n        },\n        {\n          \"name\": \"table\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"List { cols :: List <String, String>, id :: String } -> a -> a\"\n        }\n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/Show.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        {\n          \"name\": \"Show\",\n          \"vars\": \"a\",\n          \"constraints\": \"\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"methods\": [\n            \"show :: a -> String\"\n          ]\n        }\n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        \n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/Json.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        {\n          \"type\": \"Alias\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"name\": \"Parser\",\n          \"params\": \"r\",\n          \"aliasedType\": \"(String -> Either String r)\"\n        }\n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"name\": \"string\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"String -> Either String String\"\n        },\n        {\n          \"name\": \"number\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"String -> Either String Number\"\n        },\n        {\n          \"name\": \"boolean\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"String -> Either String Boolean\"\n        },\n        {\n          \"name\": \"dict\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"(String -> Either String a) -> String -> Either String (Dictionary String a)\"\n        },\n        {\n          \"name\": \"list\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"(String -> Either String a) -> String -> Either String (List a)\"\n        },\n        {\n          \"name\": \"map1\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"(a -> b) -> (String -> Either String a) -> String -> Either String b\"\n        },\n        {\n          \"name\": \"map2\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"(a -> b -> c) -> (String -> Either String a) -> (String -> Either String b) -> String -> Either String c\"\n        },\n        {\n          \"name\": \"map3\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"(a -> b -> c -> d) -> (String -> Either String a) -> (String -> Either String b) -> (String -> Either String c) -> String -> Either String d\"\n        },\n        {\n          \"name\": \"map4\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"(a -> b -> c -> d -> e) -> (String -> Either String a) -> (String -> Either String b) -> (String -> Either String c) -> (String -> Either String d) -> String -> Either String e\"\n        },\n        {\n          \"name\": \"map5\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"(a -> b -> c -> d -> e -> f) -> (String -> Either String a) -> (String -> Either String b) -> (String -> Either String c) -> (String -> Either String d) -> (String -> Either String e) -> String -> Either String f\"\n        },\n        {\n          \"name\": \"map6\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"(a -> b -> c -> d -> e -> f -> g) -> (String -> Either String a) -> (String -> Either String b) -> (String -> Either String c) -> (String -> Either String d) -> (String -> Either String e) -> (String -> Either String f) -> String -> Either String g\"\n        },\n        {\n          \"name\": \"map7\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"(a -> b -> c -> d -> e -> f -> g -> h) -> (String -> Either String a) -> (String -> Either String b) -> (String -> Either String c) -> (String -> Either String d) -> (String -> Either String e) -> (String -> Either String f) -> (String -> Either String g) -> String -> Either String h\"\n        },\n        {\n          \"name\": \"maybe\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"(String -> Either String a) -> String -> Either String (Maybe a)\"\n        },\n        {\n          \"name\": \"lazy\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"(() -> String -> Either String a) -> String -> Either String a\"\n        },\n        {\n          \"name\": \"field\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"String -> (String -> Either String a) -> String -> Either String a\"\n        },\n        {\n          \"name\": \"parse\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"(String -> Either String a) -> String -> Either String a\"\n        }\n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/HitPoint.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        {\n          \"type\": \"ADT\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"name\": \"HitPoint\",\n          \"params\": \"\",\n          \"constructors\": [\n            \"FunctionHit String String\",\n            \"LineHit String Number\"\n          ]\n        }\n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        {\n          \"name\": \"getHitPoints\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"a -> List HitPoint\"\n        }\n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/MonadTrans.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        {\n          \"name\": \"MonadTrans\",\n          \"vars\": \"m t\",\n          \"constraints\": \"Monad m\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"methods\": [\n            \"lift :: m a -> t m a\"\n          ]\n        }\n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        \n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/Monoid.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        {\n          \"name\": \"Monoid\",\n          \"vars\": \"w\",\n          \"constraints\": \"Semigroup w\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"methods\": [\n            \"mappend :: w -> w -> w\",\n            \"mempty :: w\"\n          ]\n        }\n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        \n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/Number.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"name\": \"Show\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"\",\n          \"declaration\": \"Show Number\"\n        }\n      ],\n      \"expressions\": [\n        {\n          \"name\": \"fromString\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"String -> Maybe Number\"\n        },\n        {\n          \"name\": \"formatDecimal\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"Number -> Number -> String\"\n        },\n        {\n          \"name\": \"range\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"Number -> Number -> List Number\"\n        }\n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/Dictionary.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        {\n          \"type\": \"ADT\",\n          \"description\": \"Dictionary represents a key - value data structure.\",\n          \"example\": \"\",\n          \"since\": \"0.0.5\",\n          \"name\": \"Dictionary\",\n          \"params\": \"k v\",\n          \"constructors\": [\n            \"Dictionary (List <k, v>)\"\n          ]\n        }\n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"name\": \"Functor\",\n          \"description\": \"Map over a Dictionary with a function that takes the value and transforms it.\",\n          \"example\": \"map((x) => x / 2, fromList([<\\\"Sidney\\\", 3>, <\\\"New York\\\", 18>, <\\\"London\\\", 2>]))\\n// fromList([<\\\"Sidney\\\", 1.5>, <\\\"New York\\\", 9>, <\\\"London\\\", 1>])\",\n          \"since\": \"0.0.5\",\n          \"constraints\": \"\",\n          \"declaration\": \"Functor (Dictionary k)\"\n        }\n      ],\n      \"expressions\": [\n        {\n          \"name\": \"fromList\",\n          \"description\": \"Creates a Dictionary from a list of <key, value> tuples\",\n          \"example\": \"fromList([<\\\"one\\\", 2>, <\\\"two\\\", 2>])\\nfromList([<\\\"a\\\", 2>, <\\\"b\\\", 2>, <\\\"c\\\", 3>])\",\n          \"since\": \"0.0.5\",\n          \"type\": \"List <a, b> -> Dictionary a b\"\n        },\n        {\n          \"name\": \"empty\",\n          \"description\": \"The empty Dictionary\",\n          \"example\": \"\",\n          \"since\": \"0.0.5\",\n          \"type\": \"Dictionary a b\"\n        },\n        {\n          \"name\": \"insert\",\n          \"description\": \"Inserts a value into the Dictionary at a given key.\\nHowever, if the key already exists, it updates the value at that key.\",\n          \"example\": \"insert(1, 2, empty)              // fromList([<1, 2>])\\ninsert(1, 7, fromList([<1, 2>])) // fromList([<1, 7>])\",\n          \"since\": \"0.0.5\",\n          \"type\": \"a -> b -> Dictionary a b -> Dictionary a b\"\n        },\n        {\n          \"name\": \"get\",\n          \"description\": \"Get a value from a Dictionary given a key.\",\n          \"example\": \"get(\\\"john\\\", fromList([<\\\"john\\\", { age: 32 }>])) // Just({ age: 32 })\\nget(\\\"paul\\\", fromList([<\\\"john\\\", { age: 32 }>])) // Nothing\",\n          \"since\": \"0.0.5\",\n          \"type\": \"a -> Dictionary a b -> Maybe b\"\n        },\n        {\n          \"name\": \"keys\",\n          \"description\": \"List all keys in a Dictionary\",\n          \"example\": \"keys(fromList([<true, \\\"Go for it\\\">, <false, \\\"NOGO\\\">])) // [true, false]\",\n          \"since\": \"0.0.5\",\n          \"type\": \"Dictionary a b -> List a\"\n        },\n        {\n          \"name\": \"values\",\n          \"description\": \"List all values in a Dictionary\",\n          \"example\": \"values(fromList([<true, \\\"Go for it\\\">, <false, \\\"NOGO\\\">])) // [\\\"Go for it\\\", \\\"NOGO\\\"]\",\n          \"since\": \"0.0.5\",\n          \"type\": \"Dictionary a b -> List b\"\n        },\n        {\n          \"name\": \"len\",\n          \"description\": \"Enumerate the number of keys in a Dictionary\",\n          \"example\": \"len(fromList([<\\\"Sidney\\\", 3>, <\\\"New York\\\", 18>, <\\\"London\\\", 2>])) // 3\",\n          \"since\": \"0.0.5\",\n          \"type\": \"Dictionary a b -> Number\"\n        },\n        {\n          \"name\": \"mapWithKey\",\n          \"description\": \"Map over a Dictionary with a function that takes the key and value and returns the new value.\",\n          \"example\": \"mapWithKey(\\n(city, x) => city == \\\"Sidney\\\" ? 9 : x / 2,\\nfromList([<\\\"Sidney\\\", 3>, <\\\"New York\\\", 18>, <\\\"London\\\", 2>])\\n)\\n// fromList([<\\\"Sidney\\\", 9>, <\\\"New York\\\", 9>, <\\\"London\\\", 1>])\",\n          \"since\": \"0.0.5\",\n          \"type\": \"(a -> b -> c) -> Dictionary a b -> Dictionary a c\"\n        },\n        {\n          \"name\": \"merge\",\n          \"description\": \"Combine two Dictionaries together.\",\n          \"example\": \"\",\n          \"since\": \"0.0.5\",\n          \"type\": \"Dictionary a b -> Dictionary a b -> Dictionary a b\"\n        }\n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/Compare.spec.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        \n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/MonadState.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        {\n          \"name\": \"MonadState\",\n          \"vars\": \"s m\",\n          \"constraints\": \"Monad m\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"methods\": [\n            \"get :: m s\",\n            \"modify :: (s -> s) -> m ()\",\n            \"put :: s -> m ()\"\n          ]\n        }\n      ],\n      \"instances\": [\n        {\n          \"name\": \"MonadState\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"Monad m\",\n          \"declaration\": \"MonadState s (StateT s m)\"\n        },\n        {\n          \"name\": \"MonadState\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"(Monoid w, Monad m, MonadState s m)\",\n          \"declaration\": \"MonadState s (WriterT w m)\"\n        }\n      ],\n      \"expressions\": [\n        \n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/StateT.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        {\n          \"type\": \"ADT\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"name\": \"StateT\",\n          \"params\": \"s m a\",\n          \"constructors\": [\n            \"StateT (s -> m <a, s>)\"\n          ]\n        }\n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"name\": \"Functor\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"Functor m\",\n          \"declaration\": \"Functor (StateT s m)\"\n        },\n        {\n          \"name\": \"Applicative\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"Monad m\",\n          \"declaration\": \"Applicative (StateT s m)\"\n        },\n        {\n          \"name\": \"Monad\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"Monad m\",\n          \"declaration\": \"Monad (StateT s m)\"\n        },\n        {\n          \"name\": \"MonadTrans\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"Monad m\",\n          \"declaration\": \"MonadTrans m (StateT s)\"\n        }\n      ],\n      \"expressions\": [\n        {\n          \"name\": \"runStateT\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"StateT a m b -> a -> m <b, a>\"\n        }\n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/Wish.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        {\n          \"type\": \"ADT\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"name\": \"Wish\",\n          \"params\": \"e a\",\n          \"constructors\": [\n            \"Wish ((e -> f) -> (a -> b) -> ())\"\n          ]\n        }\n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"name\": \"Functor\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"\",\n          \"declaration\": \"Functor (Wish e)\"\n        },\n        {\n          \"name\": \"Applicative\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"\",\n          \"declaration\": \"Applicative (Wish e)\"\n        },\n        {\n          \"name\": \"Monad\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"\",\n          \"declaration\": \"Monad (Wish e)\"\n        }\n      ],\n      \"expressions\": [\n        {\n          \"name\": \"mapRej\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"(a -> b) -> Wish a c -> Wish b c\"\n        },\n        {\n          \"name\": \"chainRej\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"(a -> Wish b c) -> Wish a c -> Wish b c\"\n        },\n        {\n          \"name\": \"good\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"a -> Wish b a\"\n        },\n        {\n          \"name\": \"bad\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"a -> Wish a b\"\n        },\n        {\n          \"name\": \"parallel\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"List (Wish a b) -> Wish a (List b)\"\n        },\n        {\n          \"name\": \"fulfill\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"(a -> b) -> (c -> d) -> Wish a c -> ()\"\n        },\n        {\n          \"name\": \"after\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"type\": \"Number -> a -> Wish b a\"\n        }\n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/Functor.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        {\n          \"name\": \"Functor\",\n          \"vars\": \"m\",\n          \"constraints\": \"\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"methods\": [\n            \"map :: (a -> b) -> m a -> m b\"\n          ]\n        }\n      ],\n      \"instances\": [\n        \n      ],\n      \"expressions\": [\n        \n      ]\n    },\n    {\n      \"path\": \"/Users/a.boeglin/Code/madlib/prelude/__internal__/Boolean.mad\",\n      \"description\": \"\",\n      \"typeDeclarations\": [\n        \n      ],\n      \"aliases\": [\n        \n      ],\n      \"interfaces\": [\n        \n      ],\n      \"instances\": [\n        {\n          \"name\": \"Show\",\n          \"description\": \"\",\n          \"example\": \"\",\n          \"since\": \"\",\n          \"constraints\": \"\",\n          \"declaration\": \"Show Boolean\"\n        }\n      ],\n      \"expressions\": [\n        \n      ]\n    }\n  ]\n}\n";
  const parser = J.field("modules", J.list(J.map7(ModuleDocumentation, J.field("path", J.string), J.field("description", J.string), J.field("expressions", J.list(J.map5(Definition, J.field("name", J.string), J.field("description", J.string), J.field("type", J.string), J.field("since", J.string), J.field("example", J.string)))), J.field("typeDeclarations", J.list(J.map3(TypeDefinition, J.field("name", J.string), J.field("params", J.string), J.field("constructors", J.list(J.string))))), J.field("aliases", J.list(J.map3(AliasDefinition, J.field("name", J.string), J.field("params", J.string), J.field("aliasedType", J.string)))), J.field("interfaces", J.list(J.map4(InterfaceDefinition, J.field("name", J.string), J.field("vars", J.string), J.field("constraints", J.string), J.field("methods", J.list(J.string))))), J.field("instances", J.list(J.map2(InstanceDefinition, J.field("declaration", J.string), J.field("constraints", J.string)))))));
  const parsedDocumentation = J.parse(parser, docJson);
  const sort$1 = __curry__((fn, xs) => xs.sort(fn));
  const deriveModuleName = __curry__((_P_) => fromMaybe("")(nth(0)(split(".")(fromMaybe("???")(last(split("/")(_P_)))))));
  const getDocItemName = __curry__((__x__) => ((__x__) => {
    if (__x__.__constructor === "ExpressionItem" && true && __x__.__args[1].__constructor === "Definition" && true && true && true && true && true) {
      const name = __x__.__args[1].__args[0];
      return name;
    }
    else if (__x__.__constructor === "TypeItem" && true && __x__.__args[1].__constructor === "TypeDefinition" && true && true && true) {
      const name = __x__.__args[1].__args[0];
      return name;
    }
    else if (__x__.__constructor === "AliasItem" && true && __x__.__args[1].__constructor === "AliasDefinition" && true && true && true) {
      const name = __x__.__args[1].__args[0];
      return name;
    }
    else if (__x__.__constructor === "InterfaceItem" && true && __x__.__args[1].__constructor === "InterfaceDefinition" && true && true && true && true) {
      const name = __x__.__args[1].__args[0];
      return name;
    }
    else if (__x__.__constructor === "InstanceItem" && true && __x__.__args[1].__constructor === "InstanceDefinition" && true && true) {
      const name = __x__.__args[1].__args[0];
      return name;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(__x__));
  const initialState = ((__x__) => {
    if (__x__.__constructor === "Right" && true) {
      const modules = __x__.__args[0];
      return __curry__((_P_) => __curry__((docItems) => ({ docItems: docItems, search: "" }))(sort$1(__curry__((a, b) => getDocItemName(a) > getDocItemName(b) ? 1 : -1))(flatten(Functor.List.map(__curry__((__x__) => ((__x__) => {
    if (__x__.__constructor === "ModuleDocumentation" && true && true && true && true && true && true && true) {
      const path = __x__.__args[0];
      const exps = __x__.__args[2];
      const typeDefs = __x__.__args[3];
      const aliasDefs = __x__.__args[4];
      const interfaces = __x__.__args[5];
      const instances = __x__.__args[6];
      return ([ ...Functor.List.map(__curry__((exp) => ExpressionItem(deriveModuleName(path), exp)), exps),  ...Functor.List.map(__curry__((typeDef) => TypeItem(deriveModuleName(path), typeDef)), typeDefs),  ...Functor.List.map(__curry__((aliasDef) => AliasItem(deriveModuleName(path), aliasDef)), aliasDefs),  ...Functor.List.map(__curry__((interfac) => InterfaceItem(deriveModuleName(path), interfac)), interfaces),  ...Functor.List.map(__curry__((inst) => InstanceItem(deriveModuleName(path), inst)), instances)]);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(__x__)))(_P_)))))(modules);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(parsedDocumentation);
  const getDefName = __curry__((__x__) => ((__x__) => {
    if (__x__.__constructor === "Definition" && true && true && true && true && true) {
      const name = __x__.__args[0];
      return name;
    }
    else {
      return "";
    }
  })(__x__));
  const getDefType = __curry__((__x__) => ((__x__) => {
    if (__x__.__constructor === "Definition" && true && true && true && true && true) {
      const tipe = __x__.__args[2];
      return tipe;
    }
    else {
      return "";
    }
  })(__x__));
  const getDefSince = __curry__((__x__) => ((__x__) => {
    if (__x__.__constructor === "Definition" && true && true && true && true && true) {
      const since = __x__.__args[3];
      return since;
    }
    else {
      return "";
    }
  })(__x__));
  const getDefExample = __curry__((__x__) => ((__x__) => {
    if (__x__.__constructor === "Definition" && true && true && true && true && true) {
      const example = __x__.__args[4];
      return example;
    }
    else {
      return "";
    }
  })(__x__));
  const getDefDescription = __curry__((__x__) => ((__x__) => {
    if (__x__.__constructor === "Definition" && true && true && true && true && true) {
      const desc = __x__.__args[1];
      return desc;
    }
    else {
      return "";
    }
  })(__x__));
  const getTypeDefName = __curry__((__x__) => ((__x__) => {
    if (__x__.__constructor === "TypeDefinition" && true && true && true) {
      const name = __x__.__args[0];
      return name;
    }
    else {
      return "";
    }
  })(__x__));
  const getTypeDefParams = __curry__((__x__) => ((__x__) => {
    if (__x__.__constructor === "TypeDefinition" && true && true && true) {
      const params = __x__.__args[1];
      return params;
    }
    else {
      return "";
    }
  })(__x__));
  const getTypeDefConstructors = __curry__((__x__) => ((__x__) => {
    if (__x__.__constructor === "TypeDefinition" && true && true && true) {
      const ctors = __x__.__args[2];
      return ctors;
    }
    else {
      return ([]);
    }
  })(__x__));
  const getAliasName = __curry__((__x__) => ((__x__) => {
    if (__x__.__constructor === "AliasDefinition" && true && true && true) {
      const n = __x__.__args[0];
      return n;
    }
    else {
      return "";
    }
  })(__x__));
  const getAliasParams = __curry__((__x__) => ((__x__) => {
    if (__x__.__constructor === "AliasDefinition" && true && __x__.__args[1] === "" && true) {
      return "";
    }
    else if (__x__.__constructor === "AliasDefinition" && true && true && true) {
      const params = __x__.__args[1];
      return " " + params;
    }
    else {
      return "";
    }
  })(__x__));
  const getAliasType = __curry__((__x__) => ((__x__) => {
    if (__x__.__constructor === "AliasDefinition" && true && true && true) {
      const tipe = __x__.__args[2];
      return tipe;
    }
    else {
      return "";
    }
  })(__x__));
  const getInterfaceName = __curry__((__x__) => ((__x__) => {
    if (__x__.__constructor === "InterfaceDefinition" && true && true && true && true) {
      const n = __x__.__args[0];
      return n;
    }
    else {
      return "";
    }
  })(__x__));
  const getInterfaceVars = __curry__((__x__) => ((__x__) => {
    if (__x__.__constructor === "InterfaceDefinition" && true && true && true && true) {
      const vars = __x__.__args[1];
      return vars;
    }
    else {
      return "";
    }
  })(__x__));
  const getInterfaceConstraints = __curry__((__x__) => ((__x__) => {
    if (__x__.__constructor === "InterfaceDefinition" && true && true && true && true) {
      const constraints = __x__.__args[2];
      return constraints;
    }
    else {
      return "";
    }
  })(__x__));
  const getInterfaceMethods = __curry__((__x__) => ((__x__) => {
    if (__x__.__constructor === "InterfaceDefinition" && true && true && true && true) {
      const methods = __x__.__args[3];
      return methods;
    }
    else {
      return ([]);
    }
  })(__x__));
  const getInstanceDeclaration = __curry__((__x__) => ((__x__) => {
    if (__x__.__constructor === "InstanceDefinition" && true && true) {
      const decl = __x__.__args[0];
      return decl;
    }
    else {
      return "";
    }
  })(__x__));
  const getInstanceConstraints = __curry__((__x__) => ((__x__) => {
    if (__x__.__constructor === "InstanceDefinition" && true && true) {
      const constraints = __x__.__args[1];
      return constraints;
    }
    else {
      return "";
    }
  })(__x__));
  const handleInput = __curry__((state, event) => ((__x__) => {
    if (__x__.__constructor === "InputEvent" && true) {
      const e = __x__.__args[0];
      return ([Monad.Wish.of(always(({ ...state, search: e.target.value })))]);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(event));
  const DocApp = __curry__((state) => {
      const filteredItems = state.docItems.filter((s) => getDocItemName(s).match(state.search));
      return div(([className("documentation")]), ([header(([className("documentation__header")]), ([input(([inputType("text"), placeholder("What are you looking for?"), className("search-field"), onInput(handleInput)]), ([]))])), main(([className("documentation__content")]), ([DocItemList(filteredItems)]))]));
  });
  const DocItemList = __curry__((docItems) => ul(([]), ([ ...Functor.List.map(DocItem, docItems)])));
  const DocItem = __curry__((docItem) => ((__x__) => {
    if (__x__.__constructor === "ExpressionItem" && true && true) {
      return ExpressionItemView(docItem);
    }
    else if (__x__.__constructor === "TypeItem" && true && true) {
      return TypeItemView(docItem);
    }
    else if (__x__.__constructor === "AliasItem" && true && true) {
      return AliasItemView(docItem);
    }
    else if (__x__.__constructor === "InterfaceItem" && true && true) {
      return InterfaceItemView(docItem);
    }
    else if (__x__.__constructor === "InstanceItem" && true && true) {
      return InstanceItemView(docItem);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(docItem));
  const ExpressionItemView = __curry__((docItem) => {
      const moduleName = ((__x__) => {
    if (__x__.__constructor === "ExpressionItem" && true && true) {
      const n = __x__.__args[0];
      return n;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(docItem);
      const definition = ((__x__) => {
    if (__x__.__constructor === "ExpressionItem" && true && true) {
      const def = __x__.__args[1];
      return def;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(docItem);
      const descriptionParagraphs = __curry__((_P_) => Functor.List.map(__curry__((desc) => p(([]), ([desc]))))(lines(getDefDescription(_P_))))(definition);
      return li(([className("definition")]), ([div(([className("definition__etiquette")]), ([text(`Function`)])), h2(([className("definition__title")]), ([span(([]), ([text(`${getDefName(definition)}`)])), span(([className("definition__module")]), ([text(moduleName)]))])), p(([]), ([span(([className("definition__type")]), ([text(getDefType(definition))]))])), p(([className("definition__since")]), ([text("Added in v"), text(getDefSince(definition))])), div(([className("definition__description")]), ([ ...descriptionParagraphs])), p(([className((__eq__(getDefExample(definition), "") ? "" : "definition__example"))]), ([Example(getDefExample(definition))]))]));
  });
  const AliasItemView = __curry__((docItem) => {
      const moduleName = ((__x__) => {
    if (__x__.__constructor === "AliasItem" && true && true) {
      const n = __x__.__args[0];
      return n;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(docItem);
      const aliasDef = ((__x__) => {
    if (__x__.__constructor === "AliasItem" && true && true) {
      const def = __x__.__args[1];
      return def;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(docItem);
      const aliasedType = getAliasType(aliasDef);
      return li(([className("definition")]), ([div(([className("definition__etiquette")]), ([text("Alias")])), h2(([className("definition__title")]), ([span(([]), ([text(getAliasName(aliasDef))])), span(([className("definition__module")]), ([text(moduleName)]))])), div(([className("definition__adt")]), ([span(([className("highlight")]), ([text("alias")])), span(([]), ([text(" "), text(getAliasName(aliasDef)), text(getAliasParams(aliasDef))])), span(([className("definition__constructors")]), ([span(([className("definition__constructor")]), ([span(([className("highlight")]), ([text(" = ")])), span(([]), ([text(aliasedType)]))]))]))]))]));
  });
  const InterfaceItemView = __curry__((docItem) => {
      const moduleName = ((__x__) => {
    if (__x__.__constructor === "InterfaceItem" && true && true) {
      const n = __x__.__args[0];
      return n;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(docItem);
      const interfaceDef = ((__x__) => {
    if (__x__.__constructor === "InterfaceItem" && true && true) {
      const def = __x__.__args[1];
      return def;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(docItem);
      const methods = getInterfaceMethods(interfaceDef);
      const constraints = getInterfaceConstraints(interfaceDef);
      const constraintElements = (!__eq__(constraints, "") ? ([span(([]), ([text(constraints)])), span(([className("highlight")]), ([text(` => `)]))]) : ([]));
      return li(([className("definition")]), ([div(([className("definition__etiquette")]), ([text("Interface")])), h2(([className("definition__title")]), ([span(([]), ([text(getInterfaceName(interfaceDef))])), span(([className("definition__module")]), ([text(moduleName)]))])), div(([className("definition__interface")]), ([span(([className("highlight")]), ([text("interface ")])), span(([]), ([ ...constraintElements])), span(([]), ([text(getInterfaceName(interfaceDef)), text(" "), text(getInterfaceVars(interfaceDef))])), span(([className("highlight")]), ([text(` {`)])), div(([]), ([ ...Functor.List.map(__curry__((method) => div(([]), ([text("  "), method]))), methods)])), span(([className("highlight")]), ([text(`}`)]))]))]));
  });
  const InstanceItemView = __curry__((docItem) => {
      const moduleName = ((__x__) => {
    if (__x__.__constructor === "InstanceItem" && true && true) {
      const n = __x__.__args[0];
      return n;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(docItem);
      const instanceDef = ((__x__) => {
    if (__x__.__constructor === "InstanceItem" && true && true) {
      const def = __x__.__args[1];
      return def;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(docItem);
      const constraints = getInstanceConstraints(instanceDef);
      const constraintElements = (!__eq__(constraints, "") ? ([span(([]), ([text(constraints)])), span(([className("highlight")]), ([text(` => `)]))]) : ([]));
      return li(([className("definition")]), ([div(([className("definition__etiquette")]), ([text("Instance")])), h2(([className("definition__title")]), ([span(([]), ([text(getInstanceDeclaration(instanceDef))])), span(([className("definition__module")]), ([text(moduleName)]))])), div(([className("definition__interface")]), ([span(([className("highlight")]), ([text("instance ")])), span(([]), ([ ...constraintElements])), span(([]), ([text(getInstanceDeclaration(instanceDef))]))]))]));
  });
  const TypeItemView = __curry__((docItem) => {
      const moduleName = ((__x__) => {
    if (__x__.__constructor === "TypeItem" && true && true) {
      const n = __x__.__args[0];
      return n;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(docItem);
      const typeDefinition = ((__x__) => {
    if (__x__.__constructor === "TypeItem" && true && true) {
      const def = __x__.__args[1];
      return def;
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(docItem);
      const constructors = getTypeDefConstructors(typeDefinition);
      const manyCtors = len(constructors) > 1;
      const renderedConstructors = (manyCtors ? ConstructorsView("=", constructors) : ([span(([className("definition__constructor")]), ([span(([className("highlight")]), ([text("= ")])), span(([]), ([text(fromMaybe("???", first(constructors)))]))]))]));
      return li(([className("definition")]), ([div(([className("definition__etiquette")]), ([text("Type")])), h2(([className("definition__title")]), ([span(([]), ([text(getTypeDefName(typeDefinition))])), span(([className("definition__module")]), ([text(moduleName)]))])), div(([className("definition__adt")]), ([span(([className("highlight")]), ([text("type")])), span(([]), ([text(" "), text(getTypeDefName(typeDefinition)), text(" "), text(getTypeDefParams(typeDefinition))])), span(([className("definition__constructors")]), ([ ...renderedConstructors]))]))]));
  });
  const ConstructorsView = __curry__((separator, items) => ((__x__) => {
    if (__x__.length >= 2 && true && true) {
      const [ctor,...more] = __x__;
      return ([ConstructorView(separator, ctor),  ...ConstructorsView("|", more)]);
    }
    else if (__x__.length === 1 && true) {
      const [ctor] = __x__;
      return ([ConstructorView(separator, ctor)]);
    }
    else if (__x__.length === 0) {
      return ([]);
    }
    else {
      console.log('non exhaustive patterns for value: ', __x__.toString()); 
      throw 'non exhaustive patterns!';
    }
  })(items));
  const ConstructorView = __curry__((separator, constructor) => div(([className("definition__constructor")]), ([span(([className("highlight")]), ([text("  "), separator])), span(([]), ([text(" "), constructor]))])));
  const Example = __curry__((example) => {
      const lines = split("\n", example);
      return div(([]), ([ ...Functor.List.map(__curry__((l) => div(([className("example__line")]), ([l]))), lines)]));
  });
  render(DocApp, initialState, "app");
  var Main = { parser };

  exports.default = Main;
  exports.parser = parser;

  Object.defineProperty(exports, '__esModule', { value: true });

})));
