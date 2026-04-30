// file: /Users/arnaudboeglin/Code/madlib/prelude/__internal__/List.mad


import { _10be0_reduceLeft__27, _10be0_reduceLeft__43, _10be0_reduceLeft__7 } from "./__BUILTINS__.mjs";


export function _e6cdb_reverse__30(list) {
  return (list =>
  {
    let helper__0 = (acc =>
    l =>
    {
      let $_result_;
      let $_continue_ = true;
      let $$acc = acc;
      let $$l = l;
      while($_continue_) {
        let $acc = $$acc;
        let $l = $$l;
        $_continue_ = false;
        ((__x__) => {
          if (__x__ !== null) {
            let { v: h, n: xs } = __x__;
            ($$acc = ({v: h,n: $acc}), $$l = xs, $_continue_ = true);
          } else if (__x__ === null) {
            ($_result_ = $acc);
          } else {
            console.log('non exhaustive patterns for value: ', __x__.toString());
            console.trace();
            throw 'non exhaustive patterns!';
          }
        })($l)
      }
      return $_result_;
    });
    return helper__0((null))(list);
  })
  (
    list
  );
}
export function _e6cdb_reverse__14(list) {
  return (list =>
  {
    let helper__0 = (acc =>
    l =>
    {
      let $_result_;
      let $_continue_ = true;
      let $$acc = acc;
      let $$l = l;
      while($_continue_) {
        let $acc = $$acc;
        let $l = $$l;
        $_continue_ = false;
        ((__x__) => {
          if (__x__ !== null) {
            let { v: h, n: xs } = __x__;
            ($$acc = ({v: h,n: $acc}), $$l = xs, $_continue_ = true);
          } else if (__x__ === null) {
            ($_result_ = $acc);
          } else {
            console.log('non exhaustive patterns for value: ', __x__.toString());
            console.trace();
            throw 'non exhaustive patterns!';
          }
        })($l)
      }
      return $_result_;
    });
    return helper__0((null))(list);
  })
  (
    list
  );
}
let _e6cdb_repeat__38$$ = ((a, count) =>
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
export function _e6cdb_repeat__38(a) {
  return (count =>
  _e6cdb_repeat__38$$(a, count));
}
let _e6cdb_repeat__3$$ = ((a, count) =>
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
export function _e6cdb_repeat__3(a) {
  return (count =>
  _e6cdb_repeat__3$$(a, count));
}

export function _e6cdb_reduce__6(...__args) {
  return __args.length === 0 ? _10be0_reduceLeft__7 : __args.reduce((f, a) => f(a), _10be0_reduceLeft__7);
}
export function _e6cdb_reduce__42(...__args) {
  return __args.length === 0 ? _10be0_reduceLeft__43 : __args.reduce((f, a) => f(a), _10be0_reduceLeft__43);
}
export function _e6cdb_reduce__26(...__args) {
  return __args.length === 0 ? _10be0_reduceLeft__27 : __args.reduce((f, a) => f(a), _10be0_reduceLeft__27);
}
export function _e6cdb_length__33(list) {
  return (list =>
  {
    let helper__0 = (list_ =>
    count =>
    {
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
          } else if (__x__ !== null) {
            let { v: a, n: xs } = __x__;
            ($$list_ = xs, $$count = ($count + 1), $_continue_ = true);
          } else {
            console.log('non exhaustive patterns for value: ', __x__.toString());
            console.trace();
            throw 'non exhaustive patterns!';
          }
        })($list_)
      }
      return $_result_;
    });
    return helper__0(list)(0);
  })
  (
    list
  );
}
export function _e6cdb_length__18(list) {
  return (list =>
  {
    let helper__0 = (list_ =>
    count =>
    {
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
          } else if (__x__ !== null) {
            let { v: a, n: xs } = __x__;
            ($$list_ = xs, $$count = ($count + 1), $_continue_ = true);
          } else {
            console.log('non exhaustive patterns for value: ', __x__.toString());
            console.trace();
            throw 'non exhaustive patterns!';
          }
        })($list_)
      }
      return $_result_;
    });
    return helper__0(list)(0);
  })
  (
    list
  );
}
let _e6cdb_intersperse__40$$ = ((a, xs) =>
{
  let $_result_;
  let $_continue_ = true;
  let $_start_ = {};
  let $_end_ = $_start_;
  let $$a = a;
  let $$xs = xs;
  while($_continue_) {
    let $a = $$a;
    let $xs = $$xs;
    $_continue_ = false;
    ((__x__) => {
      if (__x__ === null) {
        ($_end_.n = (null), $_result_ = $_start_.n);
      } else if (__x__ !== null && __x__.n === null) {
        let { v: one } = __x__;
        ($_end_.n = ({v: one,n: null}), $_result_ = $_start_.n);
      } else if (__x__ !== null && __x__.n !== null && __x__.n.n === null) {
        let { v: one, n: { v: two } } = __x__;
        ($_end_.n = ({
          v: one,
          n: {v: $a,n: {v: two,n: null}}
        }), $_result_ = $_start_.n);
      } else if (__x__ !== null) {
        let { v: one, n: rest } = __x__;
        ($_end_.n = { v: one, n: { v: $a, n: null }}, $_end_ = $_end_.n.n, $$a = $a, $$xs = rest, $_continue_ = true);
      } else {
        console.log('non exhaustive patterns for value: ', __x__.toString());
        console.trace();
        throw 'non exhaustive patterns!';
      }
    })($xs)
  }
  return $_result_;
});
export function _e6cdb_intersperse__40(a) {
  return (xs =>
  _e6cdb_intersperse__40$$(a, xs));
}
export function __moduleInit_e6cdb() {
  _e6cdb_reduce__6 = _10be0_reduceLeft__7
  _e6cdb_reduce__42 = _10be0_reduceLeft__43
  _e6cdb_reduce__26 = _10be0_reduceLeft__27
}


export default {
  _e6cdb_reverse__30,
  _e6cdb_reverse__14,
  _e6cdb_repeat__38,
  _e6cdb_repeat__3,
  _e6cdb_reduce__6,
  _e6cdb_reduce__42,
  _e6cdb_reduce__26,
  _e6cdb_length__33,
  _e6cdb_length__18,
  _e6cdb_intersperse__40
};