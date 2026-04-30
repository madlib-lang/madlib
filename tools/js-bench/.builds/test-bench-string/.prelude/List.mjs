// file: /Users/arnaudboeglin/Code/madlib/prelude/__internal__/List.mad


import { _10be0_reduceLeft__18, _10be0_reduceLeft__23, _10be0_reduceLeft__6 } from "./__BUILTINS__.mjs";


let _e6cdb_repeat__20$$ = ((a, count) =>
{
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
    (($count <= 0) ? ($_end_.n = (null), $_result_ = $_start_.n) : ($_end_ = $_end_.n = { v: $a, n: null }, $$a = $a, $$count = ($count - 1), $_continue_ = true))
  }
  return $_result_;
});
export function _e6cdb_repeat__20(a) {
  return (count =>
  _e6cdb_repeat__20$$(a, count));
}
let _e6cdb_repeat__2$$ = ((a, count) =>
{
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
    (($count <= 0) ? ($_end_.n = (null), $_result_ = $_start_.n) : ($_end_ = $_end_.n = { v: $a, n: null }, $$a = $a, $$count = ($count - 1), $_continue_ = true))
  }
  return $_result_;
});
export function _e6cdb_repeat__2(a) {
  return (count =>
  _e6cdb_repeat__2$$(a, count));
}
let _e6cdb_repeat__16$$ = ((a, count) =>
{
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
    (($count <= 0) ? ($_end_.n = (null), $_result_ = $_start_.n) : ($_end_ = $_end_.n = { v: $a, n: null }, $$a = $a, $$count = ($count - 1), $_continue_ = true))
  }
  return $_result_;
});
export function _e6cdb_repeat__16(a) {
  return (count =>
  _e6cdb_repeat__16$$(a, count));
}

export function _e6cdb_reduce__5(...__args) {
  return __args.length === 0 ? _10be0_reduceLeft__6 : __args.reduce((f, a) => f(a), _10be0_reduceLeft__6);
}
export function _e6cdb_reduce__22(...__args) {
  return __args.length === 0 ? _10be0_reduceLeft__23 : __args.reduce((f, a) => f(a), _10be0_reduceLeft__23);
}
export function _e6cdb_reduce__17(...__args) {
  return __args.length === 0 ? _10be0_reduceLeft__18 : __args.reduce((f, a) => f(a), _10be0_reduceLeft__18);
}
export function _e6cdb_flatten__21(list) {
  return (list =>
  {
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
          ($_end_.n = (null), $_result_ = $_start_.n);
        } else if (__x__ !== null && __x__.v === null) {
          let { n: vs } = __x__;
          ($$list = vs, $_continue_ = true);
        } else if (__x__ !== null && __x__.v !== null) {
          let { v: { v: x, n: xs }, n: vs } = __x__;
          ($_end_ = $_end_.n = { v: x, n: null }, $$list = ({
            v: xs,
            n: vs
          }), $_continue_ = true);
        } else {
          console.log('non exhaustive patterns for value: ', __x__.toString());
          console.trace();
          throw 'non exhaustive patterns!';
        }
      })($list)
    }
    return $_result_;
  })
  (
    list
  );
}
export function __moduleInit_e6cdb() {
  _e6cdb_reduce__5 = _10be0_reduceLeft__6
  _e6cdb_reduce__22 = _10be0_reduceLeft__23
  _e6cdb_reduce__17 = _10be0_reduceLeft__18
}


export default {
  _e6cdb_repeat__20,
  _e6cdb_repeat__2,
  _e6cdb_repeat__16,
  _e6cdb_reduce__5,
  _e6cdb_reduce__22,
  _e6cdb_reduce__17,
  _e6cdb_flatten__21
};