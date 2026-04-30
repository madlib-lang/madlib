// file: /Users/arnaudboeglin/Code/madlib/prelude/__internal__/Function.mad


export var _6fb04_Loop = (a =>
({__constructor: "Loop",__a: 1,_0: a}));
export var _6fb04_Done = (a =>
({__constructor: "Done",__a: 1,_0: a}));



let _6fb04_ifElse__52$$ = ((predicate, truthy, falsy, value) =>
(predicate(value) ? truthy(value) : falsy(value)));
export function _6fb04_ifElse__52(predicate) {
  return (truthy =>
  falsy =>
  value =>
  _6fb04_ifElse__52$$(predicate, truthy, falsy, value));
}
let _6fb04_equals__45$$ = ((val, a) => __eq__(val, a));
export function _6fb04_equals__45(val) {
  return (a =>
  _6fb04_equals__45$$(val, a));
}
let _6fb04_always__49$$ = ((a, _) => a);
export function _6fb04_always__49(a) {
  return (_ =>
  _6fb04_always__49$$(a, _));
}
export function __moduleInit_6fb04() {}


export default {
  _6fb04_ifElse__52,
  _6fb04_equals__45,
  _6fb04_always__49,
  _6fb04_Loop,
  _6fb04_Done
};