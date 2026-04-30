// file: /Users/arnaudboeglin/Code/madlib/prelude/__internal__/List.mad


import { _10be0_reduceLeft__10 } from "./__BUILTINS__.mjs";


let _e6cdb_repeatWith__5$$ = ((f, count) =>
{
  let helper__0 = (index =>
  {
    let $_result_;
    let $_continue_ = true;
    let $_start_ = {};
    let $_end_ = $_start_;
    let $$index = index;
    while($_continue_) {
      let $index = $$index;
      $_continue_ = false;
      (($index >= count) ? ($_end_.n = (null), $_result_ = $_start_.n) : ($_end_ = $_end_.n = { v: f
      (
        $index
      ), n: null }, $$index = ($index + 1), $_continue_ = true))
    }
    return $_result_;
  });
  return helper__0(0);
});
export function _e6cdb_repeatWith__5(f) {
  return (count =>
  _e6cdb_repeatWith__5$$(f, count));
}

export function _e6cdb_reduce__9(...__args) {
  return __args.length === 0 ? _10be0_reduceLeft__10 : __args.reduce((f, a) => f(a), _10be0_reduceLeft__10);
}
let _e6cdb_range__4$$ = ((start, end) =>
_e6cdb_repeatWith__5$$((i => (i + start)), (end - start)));
export function _e6cdb_range__4(start) {
  return (end =>
  _e6cdb_range__4$$(start, end));
}
let _e6cdb_map__7$$ = ((f, list) =>
{
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
      if (__x__ !== null) {
        let { v: a, n: xs } = __x__;
        ($_end_ = $_end_.n = { v: $f
        (
          a
        ), n: null }, $$f = $f, $$list = xs, $_continue_ = true);
      } else if (__x__ === null) {
        ($_end_.n = (null), $_result_ = $_start_.n);
      } else {
        console.log('non exhaustive patterns for value: ', __x__.toString());
        console.trace();
        throw 'non exhaustive patterns!';
      }
    })($list)
  }
  return $_result_;
});
export function _e6cdb_map__7(f) {
  return (list =>
  _e6cdb_map__7$$(f, list));
}
export function __moduleInit_e6cdb() {
  _e6cdb_reduce__9 = _10be0_reduceLeft__10
}


export default {
  _e6cdb_repeatWith__5,
  _e6cdb_reduce__9,
  _e6cdb_range__4,
  _e6cdb_map__7
};