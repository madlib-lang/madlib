// file: /Users/arnaudboeglin/Code/madlib/prelude/__internal__/__BUILTINS__.mad


export var _10be0_DictRBBlack = ({__constructor: "DictRBBlack",__a: 0});
export var _10be0_DictRBRed = ({__constructor: "DictRBRed",__a: 0});

export var _10be0_DictRBEmpty = ({__constructor: "DictRBEmpty",__a: 0});
export var _10be0_DictRBNode = (a =>
b =>
c =>
d =>
e =>
({__constructor: "DictRBNode",__a: 5,_0: a,_1: b,_2: c,_3: d,_4: e}));

export var _10be0_LT = ({__constructor: "LT",__a: 0});
export var _10be0_EQ = ({__constructor: "EQ",__a: 0});
export var _10be0_GT = ({__constructor: "GT",__a: 0});



export function _10be0_show__20(n) {
  return (n =>  "" + n )(n);
}
let _10be0_reduceLeft__7$$ = ((f, acc, list) =>
{
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
      if (__x__ !== null) {
        let { v: a, n: xs } = __x__;
        ($$f = $f, $$acc = $f($acc)(a), $$list = xs, $_continue_ = true);
      } else if (__x__ === null) {
        ($_result_ = $acc);
      } else {
        console.log('non exhaustive patterns for value: ', __x__.toString());
        console.trace();
        throw 'non exhaustive patterns!';
      }
    })($list)
  }
  return $_result_;
});
export function _10be0_reduceLeft__7(f) {
  return (acc =>
  list =>
  _10be0_reduceLeft__7$$(f, acc, list));
}
let _10be0_reduceLeft__43$$ = ((f, acc, list) =>
{
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
      if (__x__ !== null) {
        let { v: a, n: xs } = __x__;
        ($$f = $f, $$acc = $f($acc)(a), $$list = xs, $_continue_ = true);
      } else if (__x__ === null) {
        ($_result_ = $acc);
      } else {
        console.log('non exhaustive patterns for value: ', __x__.toString());
        console.trace();
        throw 'non exhaustive patterns!';
      }
    })($list)
  }
  return $_result_;
});
export function _10be0_reduceLeft__43(f) {
  return (acc =>
  list =>
  _10be0_reduceLeft__43$$(f, acc, list));
}
let _10be0_reduceLeft__27$$ = ((f, acc, list) =>
{
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
      if (__x__ !== null) {
        let { v: a, n: xs } = __x__;
        ($$f = $f, $$acc = $f($acc)(a), $$list = xs, $_continue_ = true);
      } else if (__x__ === null) {
        ($_result_ = $acc);
      } else {
        console.log('non exhaustive patterns for value: ', __x__.toString());
        console.trace();
        throw 'non exhaustive patterns!';
      }
    })($list)
  }
  return $_result_;
});
export function _10be0_reduceLeft__27(f) {
  return (acc =>
  list =>
  _10be0_reduceLeft__27$$(f, acc, list));
}
function escapeChar(c) {
  if (c === '\\') {
    return `\\\\`
  } else if (c === '"') {
    return `\\"`
  } else if (c === '\n') {
    return `\\n`
  } else if (c === '\t') {
    return `\\t`
  } else if (c === '\r') {
    return `\\r`
  } else {
    return c
  }
}
let _10be0_concatString__5$$ = ((a, b) =>  a + b );
function _10be0_concatString__5(a) {
  return (b =>
  _10be0_concatString__5$$(a, b));
}
export function _10be0_assoc__4(_) {
  return (_ => _10be0_concatString__5)(_);
}
export function _10be0_mappend__41(_) {
  return (_ => _10be0_concatString__5)(_);
}
export function __moduleInit_10be0() {}


export default {
  _10be0_show__20,
  _10be0_reduceLeft__7,
  _10be0_reduceLeft__43,
  _10be0_reduceLeft__27,
  _10be0_assoc__4,
  _10be0_mappend__41,
  _10be0_DictRBBlack,
  _10be0_DictRBRed,
  _10be0_DictRBEmpty,
  _10be0_DictRBNode,
  _10be0_LT,
  _10be0_EQ,
  _10be0_GT
};