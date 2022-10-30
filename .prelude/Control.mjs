// file: /opt/hostedtoolcache/node/14.20.1/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Control.mad
import {} from "./../__internals__.mjs"
import { Just, Nothing } from "./Maybe.mjs";

export let _$_while_$_ = (f => {
    let $_result_;
    let $_continue_ = true;
    let $$f = f;

    while($_continue_) {
      let $f = $$f;

        $_continue_ = false;
        ($f(({ __constructor: "Unit", __args: [] })) ? ($$f = $f, $_continue_ = true) : ($_result_ = ({ __constructor: "Unit", __args: [] })))
    }
    return $_result_;
});
export let loop = (start => pred => evaluate => {
    let $_result_;
    let $_continue_ = true;
    let $$start = start;
    let $$pred = pred;
    let $$evaluate = evaluate;

    while($_continue_) {
      let $start = $$start;
      let $pred = $$pred;
      let $evaluate = $$evaluate;

        $_continue_ = false;
        ($pred($start) ? ($$start = $evaluate($start), $$pred = $pred, $$evaluate = $evaluate, $_continue_ = true) : ($_result_ = $start))
    }
    return $_result_;
});
export let maybeLoop = (start => evaluate => {
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
})($evaluate($start))
    }
    return $_result_;
});
export default { _$_while_$_, loop, maybeLoop };
