// file: /opt/hostedtoolcache/node/14.20.1/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Function.mad
import {} from "./../__internals__.mjs"
export let complement = (fn => x => !(fn(x)));
export let always = (a => _ => a);
export let identity = (a => a);
export let equals = (val => a => __eq__(val, a));
export let notEquals = (val => a => !__eq__(val, a));
export let ifElse = (predicate => truthy => falsy => value => (predicate(value) ? truthy(value) : falsy(value)));
export let when = (predicate => truthy => value => ifElse(predicate)(truthy)(always(value))(value));
export let not = (b => !(b));
export let noop = (_ => ({ __constructor: "Unit", __args: [] }));
export let flip = (f => b => a => f(a)(b));
export let any = (predicate => list => {
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
})($list)
    }
    return $_result_;
});
export let all = (predicate => list => {
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
})($list)
    }
    return $_result_;
});
export let either = (predA => predB => x => predA(x) || predB(x));
export let both = (predA => predB => x => predA(x) && predB(x));

const nativeMemoize = (fn) => {
  let cache = {};
  return (a) => {
    const key = JSON.stringify(a)
    if (!cache[key]) {
      cache[key] = fn.apply(this, [a])
    }
    return cache[key]
  }
}
;
export let memoize = (fn =>  nativeMemoize(fn) );
export default { complement, always, identity, equals, notEquals, ifElse, when, not, noop, flip, any, all, either, both, memoize };
