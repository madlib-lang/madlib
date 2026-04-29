// file: /Users/arnaudboeglin/Code/madlib/prelude/__internal__/Maybe.mad


import { _10be0_show__8 } from "./__BUILTINS__.mjs";


export var _be211_Just = (a =>
({__constructor: "Just",__args: [a]}));
export var _be211_Nothing = ({__constructor: "Nothing",__args: []});



export function _be211_show__7(__$a__) {
  return (__$a__ =>
  ((__x__) => {
    if (__x__.__constructor === "Just" && true) {
      let a0 = __x__.__args[0];
      return (("Just(" + _10be0_show__8(a0)) + ")");
    } else if (__x__.__constructor === "Nothing") {
      return "Nothing";
    } else if (true) {
      return "Unknown";
    } else {
      console.log('non exhaustive patterns for value: ', __x__.toString());
      console.trace();
      throw 'non exhaustive patterns!';
    }
  })(__$a__))
  (
    __$a__
  );
}
export function _be211_pure__4(_) {
  return (_ => (a => ({__constructor: "Just",__args: [a]})))(_);
}
export function _be211_of__3(_) {
  return (_ => _be211_pure__4())(_);
}
export let _be211_fromMaybe__11$$ = ((or, __W__1) =>
((__x__) => {
  if (__x__.__constructor === "Just" && true) {
    let a = __x__.__args[0];
    return a;
  } else if (__x__.__constructor === "Nothing") {
    return or;
  } else {
    console.log('non exhaustive patterns for value: ', __x__.toString());
    console.trace();
    throw 'non exhaustive patterns!';
  }
})(__W__1));
export function _be211_fromMaybe__11(or) {
  return (__W__1 =>
  _be211_fromMaybe__11$$(or, __W__1));
}
export let _be211_chain__5$$ = ((f, m) =>
((__x__) => {
  if (__x__.__constructor === "Just" && true) {
    let x = __x__.__args[0];
    return f(x);
  } else if (__x__.__constructor === "Nothing") {
    return ({__constructor: "Nothing",__args: []});
  } else {
    console.log('non exhaustive patterns for value: ', __x__.toString());
    console.trace();
    throw 'non exhaustive patterns!';
  }
})(m));
export function _be211_chain__5(f) {
  return (m =>
  _be211_chain__5$$(f, m));
}
export function __moduleInit_be211() {}


export default {
  _be211_show__7,
  _be211_pure__4,
  _be211_of__3,
  _be211_fromMaybe__11,
  _be211_chain__5,
  _be211_Just,
  _be211_Nothing
};