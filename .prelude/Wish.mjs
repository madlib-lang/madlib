// file: /opt/hostedtoolcache/node/14.21.1/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Wish.mad
import {} from "./../__internals__.mjs"
import {  } from "./Monad.mjs";
import {  } from "./Bifunctor.mjs";
import { sortBy, _$_length_$_ } from "./List.mjs";
import Tuple from "./Tuple.mjs";
import {  } from "./Number.mjs";

export let Wish = (a => ({ __constructor: "Wish", __args: [ a ] }));
Inspect['Wish_a42e274f9391b247f6eb25b247963687'] = {};
Inspect['Wish_a42e274f9391b247f6eb25b247963687']['inspect'] = () => (Inspect_f681) => (Inspect_h683) => (__$a__ => ((__x__) => {
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
Functor['Wish_a42e274f9391b247f6eb25b247963687'] = {};
Functor['Wish_a42e274f9391b247f6eb25b247963687']['map'] = () => (f => m => Wish((badCB => goodCB => ((__x__) => {
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
Applicative['Wish_a42e274f9391b247f6eb25b247963687'] = {};
Applicative['Wish_a42e274f9391b247f6eb25b247963687']['ap'] = () => (mf => m => Wish((badCB => goodCB => ((__x__) => {
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
Applicative['Wish_a42e274f9391b247f6eb25b247963687']['pure'] = () => (a => Wish((_ => goodCB => goodCB(a))));
Monad['Wish_a42e274f9391b247f6eb25b247963687'] = {};
Monad['Wish_a42e274f9391b247f6eb25b247963687']['chain'] = () => (f => m => Wish((badCB => goodCB => ((__x__) => {
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
Monad['Wish_a42e274f9391b247f6eb25b247963687']['of'] = () => Applicative.Wish_a42e274f9391b247f6eb25b247963687.pure();
Bifunctor['Wish_a42e274f9391b247f6eb25b247963687'] = {};
Bifunctor['Wish_a42e274f9391b247f6eb25b247963687']['bimap'] = () => (leftF => rightF => m => Wish((badCB => goodCB => ((__x__) => {
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
Bifunctor['Wish_a42e274f9391b247f6eb25b247963687']['mapFirst'] = () => mapRej;
Bifunctor['Wish_a42e274f9391b247f6eb25b247963687']['mapSecond'] = () => Functor.Wish_a42e274f9391b247f6eb25b247963687.map();
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
export let parallelN = (concurrent => wishes => Wish((badCB => goodCB => {
    let amountOfWishesToProcess = _$_length_$_(wishes);
    let ko = false;
    let ok = 0;
    let runningWishes = 0;
    let wishesNotStarted = wishes;
    let indexOfLastStartedWish = 0;
    let result = (null);
    let fork = (index => ws => ((__x__) => {
  if (__x__ !== null && true && true) {
    let { v: wish, n: nextWishes } = __x__;
    return (() => {
  wishesNotStarted = nextWishes;
  runningWishes = (runningWishes + 1);
  indexOfLastStartedWish = (indexOfLastStartedWish + 1);
  ((__x__) => {
  if (__x__.__constructor === "Wish" && true) {
    let run = __x__.__args[0];
    return run((err => (!(ko) ? (() => {
  ko = true
  return badCB(err)
})() : ({ __constructor: "Unit", __args: [] }))))((x => {
    result = ({ v: ([index, x]), n: result });
    ok = (ok + 1);
    runningWishes = (runningWishes - 1);
    return (__eq__(ok, amountOfWishesToProcess) ? (_P_ => goodCB(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(Tuple.snd)(sortBy((a => b => Comparable.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.compare()(Tuple.fst(a))(Tuple.fst(b))))(_P_))))(result) : fork((indexOfLastStartedWish + 1))(wishesNotStarted));
}));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(wish);
  (concurrent < 0 || runningWishes < concurrent ? fork(indexOfLastStartedWish)(wishesNotStarted) : ({ __constructor: "Unit", __args: [] }))
  return ({ __constructor: "Unit", __args: [] })
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
export let parallel = parallelN(-1);
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
export default { mapRej, chainRej, bichain, good, bad, parallelN, parallel, discardError, fulfill, after, Wish };
