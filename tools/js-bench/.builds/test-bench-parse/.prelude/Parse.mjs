// file: /Users/arnaudboeglin/Code/madlib/prelude/__internal__/Parse.mad


import { _0c6b8_maybeLoop__36 } from "./Control.mjs";
import { _b6337_Left, _b6337_Right } from "./Either.mjs";
import { _6fb04_always__49, _6fb04_equals__45, _6fb04_ifElse__52 } from "./Function.mjs";
import { _e6cdb_reduce__26, _e6cdb_reverse__14, _e6cdb_reverse__30 } from "./List.mjs";
import { _be211_Just, _be211_Nothing } from "./Maybe.mjs";
import { _360b0_andDo__57 } from "./Monad.mjs";
import { _64fbe_byteCharAt__9, _64fbe_byteCharWidth__10, _64fbe_byteLength__16, _64fbe_byteStartsWith__24, _64fbe_toList__25 } from "./String.mjs";


export var _7d446_Loc = (a =>
b =>
c =>
({__constructor: "Loc",__a: 3,_0: a,_1: b,_2: c}));

export var _7d446_Parser = (a =>
({__constructor: "Parser",__a: 1,_0: a}));

export var _7d446_Error = (a =>
({__constructor: "Error",__a: 1,_0: a}));

export var _7d446_Config = (a =>
({__constructor: "Config",__a: 1,_0: a}));



export function _7d446_pure__59(a) {
  return (a =>
  ({
    __constructor: "Parser",
    __a: 1,
    _0: (_ => idx => l => ([({v: ([a, idx]),n: null}), l]))
  }))
  (
    a
  );
}
export function _7d446_pure__51(a) {
  return (a =>
  ({
    __constructor: "Parser",
    __a: 1,
    _0: (_ => idx => l => ([({v: ([a, idx]),n: null}), l]))
  }))
  (
    a
  );
}
let _7d446_parse__32$$ = ((parser, input, idx, loc) =>
((__x__) => {
  if (__x__.__constructor === "Parser") {
    let fn = __x__._0;
    return fn(input)(idx)(loc);
  } else {
    console.log('non exhaustive patterns for value: ', __x__.toString());
    console.trace();
    throw 'non exhaustive patterns!';
  }
})(parser));
function _7d446_parse__32(parser) {
  return (input =>
  idx =>
  loc =>
  _7d446_parse__32$$(parser, input, idx, loc));
}
let _7d446_runParser__31$$ = ((m, s) =>
{
  let totalLen = _64fbe_byteLength__16(s);
  return ((__x__) => {
    if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && __x__[0].n === null) {
      let [{ v: [res, idx] },] = __x__;
      return (__eq__(idx, totalLen) ? ({
        __constructor: "Right",
        __a: 1,
        _0: res
      }) : ({
        __constructor: "Left",
        __a: 1,
        _0: ({
          __constructor: "Error",
          __a: 1,
          _0: ({__constructor: "Loc",__a: 3,_0: 0,_1: 0,_2: 0})
        })
      }));
    } else if (__x__.length === 2) {
      let [,l] = __x__;
      return ({
        __constructor: "Left",
        __a: 1,
        _0: ({__constructor: "Error",__a: 1,_0: l})
      });
    } else {
      console.log('non exhaustive patterns for value: ', __x__.toString());
      console.trace();
      throw 'non exhaustive patterns!';
    }
  })(_7d446_parse__32$$(m, s, 0, ({
    __constructor: "Loc",
    __a: 3,
    _0: 0,
    _1: 0,
    _2: 0
  })));
});
export function _7d446_runParser__31(m) {
  return (s =>
  _7d446_runParser__31$$(m, s));
}
let _7d446_parse__29$$ = ((parser, input, idx, loc) =>
((__x__) => {
  if (__x__.__constructor === "Parser") {
    let fn = __x__._0;
    return fn(input)(idx)(loc);
  } else {
    console.log('non exhaustive patterns for value: ', __x__.toString());
    console.trace();
    throw 'non exhaustive patterns!';
  }
})(parser));
function _7d446_parse__29(parser) {
  return (input =>
  idx =>
  loc =>
  _7d446_parse__29$$(parser, input, idx, loc));
}
let _7d446_parse__17$$ = ((parser, input, idx, loc) =>
((__x__) => {
  if (__x__.__constructor === "Parser") {
    let fn = __x__._0;
    return fn(input)(idx)(loc);
  } else {
    console.log('non exhaustive patterns for value: ', __x__.toString());
    console.trace();
    throw 'non exhaustive patterns!';
  }
})(parser));
function _7d446_parse__17(parser) {
  return (input =>
  idx =>
  loc =>
  _7d446_parse__17$$(parser, input, idx, loc));
}
let _7d446_runParser__15$$ = ((m, s) =>
{
  let totalLen = _64fbe_byteLength__16(s);
  return ((__x__) => {
    if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && __x__[0].n === null) {
      let [{ v: [res, idx] },] = __x__;
      return (__eq__(idx, totalLen) ? ({
        __constructor: "Right",
        __a: 1,
        _0: res
      }) : ({
        __constructor: "Left",
        __a: 1,
        _0: ({
          __constructor: "Error",
          __a: 1,
          _0: ({__constructor: "Loc",__a: 3,_0: 0,_1: 0,_2: 0})
        })
      }));
    } else if (__x__.length === 2) {
      let [,l] = __x__;
      return ({
        __constructor: "Left",
        __a: 1,
        _0: ({__constructor: "Error",__a: 1,_0: l})
      });
    } else {
      console.log('non exhaustive patterns for value: ', __x__.toString());
      console.trace();
      throw 'non exhaustive patterns!';
    }
  })(_7d446_parse__17$$(m, s, 0, ({
    __constructor: "Loc",
    __a: 3,
    _0: 0,
    _1: 0,
    _2: 0
  })));
});
export function _7d446_runParser__15(m) {
  return (s =>
  _7d446_runParser__15$$(m, s));
}
let _7d446_parse__13$$ = ((parser, input, idx, loc) =>
((__x__) => {
  if (__x__.__constructor === "Parser") {
    let fn = __x__._0;
    return fn(input)(idx)(loc);
  } else {
    console.log('non exhaustive patterns for value: ', __x__.toString());
    console.trace();
    throw 'non exhaustive patterns!';
  }
})(parser));
function _7d446_parse__13(parser) {
  return (input =>
  idx =>
  loc =>
  _7d446_parse__13$$(parser, input, idx, loc));
}
export function _7d446_of__58(_) {
  return (_ => _7d446_pure__59)(_);
}
export function _7d446_of__50(_) {
  return (_ => _7d446_pure__51)(_);
}
export function _7d446_many__28(p) {
  return (p =>
  ({
    __constructor: "Parser",
    __a: 1,
    _0: (input =>
    startIdx =>
    l =>
    {
      let idx = startIdx;
      let loc = l;
      let acc = (null);
      while(((__x__) => {
        if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && __x__[0].n === null) {
          let [{ v: [parsed, newIdx] },loc_] = __x__;
          return (__eq__(newIdx, idx) ? false : (() => {
            idx = newIdx;
            loc = loc_;
            acc = ({v: parsed,n: acc});
            return true;
          })());
        } else {
          return false;
        }
      })(((__x__) => {
        if (__x__.__constructor === "Parser") {
          let fn = __x__._0;
          return fn(input)(idx)(loc);
        } else {
          console.log('non exhaustive patterns for value: ', __x__.toString());
          console.trace();
          throw 'non exhaustive patterns!';
        }
      })(p))) {
        undefined
      };
      return ([({v: ([_e6cdb_reverse__30(acc), idx]),n: null}), loc]);
    })
  }))
  (
    p
  );
}
export function _7d446_many__12(p) {
  return (p =>
  ({
    __constructor: "Parser",
    __a: 1,
    _0: (input =>
    startIdx =>
    l =>
    {
      let idx = startIdx;
      let loc = l;
      let acc = (null);
      while(((__x__) => {
        if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && __x__[0].n === null) {
          let [{ v: [parsed, newIdx] },loc_] = __x__;
          return (__eq__(newIdx, idx) ? false : (() => {
            idx = newIdx;
            loc = loc_;
            acc = ({v: parsed,n: acc});
            return true;
          })());
        } else {
          return false;
        }
      })(((__x__) => {
        if (__x__.__constructor === "Parser") {
          let fn = __x__._0;
          return fn(input)(idx)(loc);
        } else {
          console.log('non exhaustive patterns for value: ', __x__.toString());
          console.trace();
          throw 'non exhaustive patterns!';
        }
      })(p))) {
        undefined
      };
      return ([({v: ([_e6cdb_reverse__14(acc), idx]),n: null}), loc]);
    })
  }))
  (
    p
  );
}
let _7d446_incLoc__11$$ = ((c, __W__1) =>
((__x__) => {
  if (__x__.__constructor === "Loc") {
    let abs = __x__._0;
    let line = __x__._1;
    let col = __x__._2;
    return (__eq__(c, String.fromCodePoint(10)) ? ({
      __constructor: "Loc",
      __a: 3,
      _0: (abs + 1),
      _1: (line + 1),
      _2: 0
    }) : ({__constructor: "Loc",__a: 3,_0: (abs + 1),_1: line,_2: (col + 1)}));
  } else {
    console.log('non exhaustive patterns for value: ', __x__.toString());
    console.trace();
    throw 'non exhaustive patterns!';
  }
})(__W__1));
function _7d446_incLoc__11(c) {
  return (__W__1 =>
  _7d446_incLoc__11$$(c, __W__1));
}
export function _7d446_string__23(target) {
  return (target =>
  ({
    __constructor: "Parser",
    __a: 1,
    _0: (input =>
    idx =>
    l =>
    (_64fbe_byteStartsWith__24(target)(idx)(input) ? ([({
      v: ([target, (idx + _64fbe_byteLength__16(target))]),
      n: null
    }), _e6cdb_reduce__26((loc => c => _7d446_incLoc__11$$(c, loc)))(l)
    (
      _64fbe_toList__25(target)
    )]) : ([(null), l])))
  }))
  (
    target
  );
}
let _7d446_chain__61$$ = ((f, m) =>
({
  __constructor: "Parser",
  __a: 1,
  _0: (input =>
  idx =>
  l =>
  ((__x__) => {
    if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && __x__[0].n === null) {
      let [{ v: [a, idx1] },l1] = __x__;
      return _7d446_parse__17$$(f(a), input, idx1, l1);
    } else if (__x__.length === 2) {
      let [,ll] = __x__;
      return ([(null), ll]);
    } else {
      console.log('non exhaustive patterns for value: ', __x__.toString());
      console.trace();
      throw 'non exhaustive patterns!';
    }
  })(((__x__) => {
    if (__x__.__constructor === "Parser") {
      let fn = __x__._0;
      return fn(input)(idx)(l);
    } else {
      console.log('non exhaustive patterns for value: ', __x__.toString());
      console.trace();
      throw 'non exhaustive patterns!';
    }
  })(m)))
}));
export function _7d446_chain__61(f) {
  return (m =>
  _7d446_chain__61$$(f, m));
}
let _7d446_chain__60$$ = ((f, m) =>
({
  __constructor: "Parser",
  __a: 1,
  _0: (input =>
  idx =>
  l =>
  ((__x__) => {
    if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && __x__[0].n === null) {
      let [{ v: [a, idx1] },l1] = __x__;
      return _7d446_parse__17$$(f(a), input, idx1, l1);
    } else if (__x__.length === 2) {
      let [,ll] = __x__;
      return ([(null), ll]);
    } else {
      console.log('non exhaustive patterns for value: ', __x__.toString());
      console.trace();
      throw 'non exhaustive patterns!';
    }
  })(((__x__) => {
    if (__x__.__constructor === "Parser") {
      let fn = __x__._0;
      return fn(input)(idx)(l);
    } else {
      console.log('non exhaustive patterns for value: ', __x__.toString());
      console.trace();
      throw 'non exhaustive patterns!';
    }
  })(m)))
}));
export function _7d446_chain__60(f) {
  return (m =>
  _7d446_chain__60$$(f, m));
}
let _7d446_chain__53$$ = ((f, m) =>
({
  __constructor: "Parser",
  __a: 1,
  _0: (input =>
  idx =>
  l =>
  ((__x__) => {
    if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && __x__[0].n === null) {
      let [{ v: [a, idx1] },l1] = __x__;
      return _7d446_parse__13$$(f(a), input, idx1, l1);
    } else if (__x__.length === 2) {
      let [,ll] = __x__;
      return ([(null), ll]);
    } else {
      console.log('non exhaustive patterns for value: ', __x__.toString());
      console.trace();
      throw 'non exhaustive patterns!';
    }
  })(((__x__) => {
    if (__x__.__constructor === "Parser") {
      let fn = __x__._0;
      return fn(input)(idx)(l);
    } else {
      console.log('non exhaustive patterns for value: ', __x__.toString());
      console.trace();
      throw 'non exhaustive patterns!';
    }
  })(m)))
}));
export function _7d446_chain__53(f) {
  return (m =>
  _7d446_chain__53$$(f, m));
}
export var _7d446_anyChar__8;
export function _7d446_takeWhile__35(pred) {
  return (pred =>
  ({
    __constructor: "Parser",
    __a: 1,
    _0: (input =>
    startIdx =>
    l =>
    {
      let result = _0c6b8_maybeLoop__36(([startIdx, l, (null)]))
      (
        (state =>
        ((__x__) => {
          if (__x__.length === 3) {
            let [idx,ll,acc] = __x__;
            return ((__x__) => {
              if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && __x__[0].n === null) {
                let [{ v: [parsed, newIdx] },loc] = __x__;
                return (pred(parsed) ? ({
                  __constructor: "Just",
                  __a: 1,
                  _0: ([newIdx, loc, ({v: parsed,n: acc})])
                }) : ({__constructor: "Nothing",__a: 0}));
              } else {
                return ({__constructor: "Nothing",__a: 0});
              }
            })(((__x__) => {
              if (__x__.__constructor === "Parser") {
                let fn = __x__._0;
                return fn(input)(idx)(ll);
              } else {
                console.log('non exhaustive patterns for value: ', __x__.toString());
                console.trace();
                throw 'non exhaustive patterns!';
              }
            })(_7d446_anyChar__8));
          } else {
            console.log('non exhaustive patterns for value: ', __x__.toString());
            console.trace();
            throw 'non exhaustive patterns!';
          }
        })(state))
      );
      return ((__x__) => {
        if (__x__.length === 3) {
          let [idx,loc,parseResult] = __x__;
          return ([({
            v: ([_e6cdb_reverse__14(parseResult), idx]),
            n: null
          }), loc]);
        } else {
          console.log('non exhaustive patterns for value: ', __x__.toString());
          console.trace();
          throw 'non exhaustive patterns!';
        }
      })(result);
    })
  }))
  (
    pred
  );
}
let _7d446_alt__62$$ = ((ma, mb) =>
({
  __constructor: "Parser",
  __a: 1,
  _0: (input =>
  idx =>
  l =>
  ((__x__) => {
    if (__x__.length === 2 && __x__[0] === null) {
      let [,] = __x__;
      return ((__x__) => {
        if (__x__.__constructor === "Parser") {
          let fn = __x__._0;
          return fn(input)(idx)(l);
        } else {
          console.log('non exhaustive patterns for value: ', __x__.toString());
          console.trace();
          throw 'non exhaustive patterns!';
        }
      })(mb);
    } else {
      let res = __x__;
      return res;
    }
  })(((__x__) => {
    if (__x__.__constructor === "Parser") {
      let fn = __x__._0;
      return fn(input)(idx)(l);
    } else {
      console.log('non exhaustive patterns for value: ', __x__.toString());
      console.trace();
      throw 'non exhaustive patterns!';
    }
  })(ma)))
}));
export function _7d446_alt__62(ma) {
  return (mb =>
  _7d446_alt__62$$(ma, mb));
}
export function _7d446_aempty__56(_) {
  return (_ =>
  ({__constructor: "Parser",__a: 1,_0: (_ => _ => l => ([(null), l]))}))
  (
    _
  );
}
export var _7d446_fail__55;
let _7d446_sepBy__54$$ = ((parser, separator) =>
_7d446_alt__62$$((() => {
  return _7d446_chain__61$$((first =>
  _7d446_chain__60$$((rest =>
  _7d446_pure__59(({v: first,n: rest}))), _7d446_many__12
  (
    _360b0_andDo__57(parser)(separator)
  ))), parser);
})(), _7d446_fail__55));
export function _7d446_sepBy__54(parser) {
  return (separator =>
  _7d446_sepBy__54$$(parser, separator));
}
export function _7d446_aempty__48(_) {
  return (_ =>
  ({__constructor: "Parser",__a: 1,_0: (_ => _ => l => ([(null), l]))}))
  (
    _
  );
}
export var _7d446_fail__47;
export function _7d446_satisfy__46(pred) {
  return (pred =>
  _7d446_chain__53$$(_6fb04_ifElse__52(pred)(_7d446_pure__51)
  (
    _6fb04_always__49(_7d446_fail__47)
  ), _7d446_anyChar__8))
  (
    pred
  );
}
export function _7d446_char__44(__P__3) {
  return (__P__3 => _7d446_satisfy__46(_6fb04_equals__45(__P__3)))(__P__3);
}
export function __moduleInit_7d446() {
  _7d446_anyChar__8 = ({
    __constructor: "Parser",
    __a: 1,
    _0: (input =>
    idx =>
    l =>
    ((__x__) => {
      switch (__x__.__constructor) {
        case "Just": {
          let c = __x__._0;
          return ([({
            v: ([c, (idx + _64fbe_byteCharWidth__10(idx)(input))]),
            n: null
          }), _7d446_incLoc__11$$(c, l)]);
        }
        case "Nothing": {
          return ([(null), l]);
        }
        default: {
          console.log('non exhaustive patterns for value: ', __x__.toString());
          console.trace();
          throw 'non exhaustive patterns!';
        }
      }
    })(_64fbe_byteCharAt__9(idx)(input)))
  })
  _7d446_fail__55 = ({
    __constructor: "Parser",
    __a: 1,
    _0: (_ => _ => l => ([(null), l]))
  })
  _7d446_fail__47 = ({
    __constructor: "Parser",
    __a: 1,
    _0: (_ => _ => l => ([(null), l]))
  })
}


export default {
  _7d446_pure__59,
  _7d446_pure__51,
  _7d446_runParser__31,
  _7d446_runParser__15,
  _7d446_of__58,
  _7d446_of__50,
  _7d446_many__28,
  _7d446_many__12,
  _7d446_string__23,
  _7d446_chain__61,
  _7d446_chain__60,
  _7d446_chain__53,
  _7d446_anyChar__8,
  _7d446_takeWhile__35,
  _7d446_alt__62,
  _7d446_aempty__56,
  _7d446_fail__55,
  _7d446_sepBy__54,
  _7d446_aempty__48,
  _7d446_fail__47,
  _7d446_satisfy__46,
  _7d446_char__44,
  _7d446_Loc,
  _7d446_Parser,
  _7d446_Error,
  _7d446_Config
};