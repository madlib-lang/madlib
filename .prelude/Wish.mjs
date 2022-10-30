// file: /opt/hostedtoolcache/node/14.20.1/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Wish.mad
import {} from "./../__internals__.mjs"
import {  } from "./Monad.mjs";
import {  } from "./Bifunctor.mjs";
import { sortBy, _$_length_$_ } from "./List.mjs";
import Tuple from "./Tuple.mjs";
import {  } from "./Number.mjs";

export let Wish = (a => ({ __constructor: "Wish", __args: [ a ] }));
Inspect['Wish_48e4c68256950cf75b1d1da053be4dfb'] = {};
Inspect['Wish_48e4c68256950cf75b1d1da053be4dfb']['inspect'] = () => (Inspect_n403) => (Inspect_p405) => (__$a__ => ((__x__) => {
  if (__x__.__constructor === "Wish" && true) {
    let a0 = __x__.__args[0];
    return `Wish(` + Inspect.a_arr_b.inspect()(a0) + `)`;
  }
  else if (true) {
    return `Unknown`;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__$a__));
Functor['Wish_48e4c68256950cf75b1d1da053be4dfb'] = {};
Functor['Wish_48e4c68256950cf75b1d1da053be4dfb']['map'] = () => (f => m => Wish((badCB => goodCB => ((__x__) => {
  if (__x__.__constructor === "Wish" && true) {
    let run = __x__.__args[0];
    return run(badCB)((x => goodCB(f(x))));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(m))));
Applicative['Wish_48e4c68256950cf75b1d1da053be4dfb'] = {};
Applicative['Wish_48e4c68256950cf75b1d1da053be4dfb']['ap'] = () => (mf => m => Wish((badCB => goodCB => ((__x__) => {
  if (__x__.length === 2 && __x__[0].__constructor === "Wish" && true && __x__[1].__constructor === "Wish" && true) {
    let [{ __args: [runMF]},{ __args: [runM]}] = __x__;
    return runM(badCB)((x => runMF(badCB)((f => goodCB(f(x))))));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(([mf, m])))));
Applicative['Wish_48e4c68256950cf75b1d1da053be4dfb']['pure'] = () => (a => Wish((_ => goodCB => goodCB(a))));
Monad['Wish_48e4c68256950cf75b1d1da053be4dfb'] = {};
Monad['Wish_48e4c68256950cf75b1d1da053be4dfb']['chain'] = () => (f => m => Wish((badCB => goodCB => ((__x__) => {
  if (__x__.__constructor === "Wish" && true) {
    let run = __x__.__args[0];
    return run(badCB)((x => ((__x__) => {
  if (__x__.__constructor === "Wish" && true) {
    let r = __x__.__args[0];
    return r(badCB)(goodCB);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(f(x))));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(m))));
Monad['Wish_48e4c68256950cf75b1d1da053be4dfb']['of'] = () => Applicative.Wish_48e4c68256950cf75b1d1da053be4dfb.pure();
Bifunctor['Wish_48e4c68256950cf75b1d1da053be4dfb'] = {};
Bifunctor['Wish_48e4c68256950cf75b1d1da053be4dfb']['bimap'] = () => (leftF => rightF => m => Wish((badCB => goodCB => ((__x__) => {
  if (__x__.__constructor === "Wish" && true) {
    let run = __x__.__args[0];
    return run((_P_ => badCB(leftF(_P_))))((_P_ => goodCB(rightF(_P_))));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(m))));
Bifunctor['Wish_48e4c68256950cf75b1d1da053be4dfb']['mapFirst'] = () => mapRej;
Bifunctor['Wish_48e4c68256950cf75b1d1da053be4dfb']['mapSecond'] = () => Functor.Wish_48e4c68256950cf75b1d1da053be4dfb.map();
export let mapRej = (f => m => Wish((badCB => goodCB => ((__x__) => {
  if (__x__.__constructor === "Wish" && true) {
    let run = __x__.__args[0];
    return run((x => badCB(f(x))))(goodCB);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(m))));
export let chainRej = (f => m => Wish((badCB => goodCB => ((__x__) => {
  if (__x__.__constructor === "Wish" && true) {
    let run = __x__.__args[0];
    return run((x => ((__x__) => {
  if (__x__.__constructor === "Wish" && true) {
    let r = __x__.__args[0];
    return r(badCB)(goodCB);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(f(x))))(goodCB);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(m))));
export let bichain = (badF => goodF => m => Wish((badCB => goodCB => ((__x__) => {
  if (__x__.__constructor === "Wish" && true) {
    let run = __x__.__args[0];
    return run((x => ((__x__) => {
  if (__x__.__constructor === "Wish" && true) {
    let r = __x__.__args[0];
    return r(badCB)(goodCB);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(badF(x))))((x => ((__x__) => {
  if (__x__.__constructor === "Wish" && true) {
    let r = __x__.__args[0];
    return r(badCB)(goodCB);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(goodF(x))));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(m))));
export let good = (a => Wish((_ => goodCB => goodCB(a))));
export let bad = (e => Wish((badCB => _ => badCB(e))));
export let parallel = (wishes => Wish((badCB => goodCB => {
    let amountOfWishesToProcess = _$_length_$_(wishes);
    let ko = false;
    let ok = 0;
    let result = (null);
    let next = (amountResolved => (__eq__(amountResolved, amountOfWishesToProcess) ? (_P_ => goodCB(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(Tuple.snd)(sortBy((a => b => Comparable.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.compare()(Tuple.fst(a))(Tuple.fst(b))))(_P_))))(result) : ({ __constructor: "Unit", __args: [] })));
    let fork = (index => ws => ((__x__) => {
  if (__x__ !== null && true && true) {
    let { v: wish, n: nextWishes } = __x__;
    return (() => {
  ((__x__) => {
  if (__x__.__constructor === "Wish" && true) {
    let run = __x__.__args[0];
    return run((err => (__eq__(ko, false) ? (() => {
  ko = true
  return badCB(err)
})() : ({ __constructor: "Unit", __args: [] }))))((x => {
    result = ({ v: ([index, x]), n: result });
    ok = (ok + 1);
    return next(ok);
}));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(wish)
  return fork((index + 1))(nextWishes)
})();
  }
  else if (__x__ === null) {
    return (__eq__(amountOfWishesToProcess, 0) ? goodCB((null)) : ({ __constructor: "Unit", __args: [] }));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(ws));
    return fork(0)(wishes);
})));
export let discardError = (recover => wish => Wish((_ => goodCB => ((__x__) => {
  if (__x__.__constructor === "Wish" && true) {
    let run = __x__.__args[0];
    return run((_P_ => goodCB(recover(_P_))))(goodCB);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(wish))));
export let fulfill = (badCB => goodCB => m => {
    ((__x__) => {
  if (__x__.__constructor === "Wish" && true) {
    let run = __x__.__args[0];
    return  setTimeout(() => run(badCB)(goodCB), 0); ;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(m);
    return ({ __constructor: "Unit", __args: [] });
});
export let after = (time => a => Wish((_ => goodCB =>  {
  setTimeout(() => goodCB(a), time);
} )));
export default { mapRej, chainRej, bichain, good, bad, parallel, discardError, fulfill, after, Wish };
