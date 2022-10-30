// file: /opt/hostedtoolcache/node/14.20.1/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Maybe.mad
import {} from "./../__internals__.mjs"
import {  } from "./Monad.mjs";
import {  } from "./Show.mjs";

export let Just = (a => ({ __constructor: "Just", __args: [ a ] }));
export let Nothing = ({ __constructor: "Nothing", __args: [  ] });
Inspect['Maybe_eadd07e55d46112f77467431f86f5e2d'] = {};
Inspect['Maybe_eadd07e55d46112f77467431f86f5e2d']['inspect'] = () => (Inspect_t45) => (__$a__ => ((__x__) => {
  if (__x__.__constructor === "Just" && true) {
    let a0 = __x__.__args[0];
    return `Just(` + Inspect_t45.inspect()(a0) + `)`;
  }
  else if (__x__.__constructor === "Nothing") {
    return `Nothing`;
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
Functor['Maybe_eadd07e55d46112f77467431f86f5e2d'] = {};
Functor['Maybe_eadd07e55d46112f77467431f86f5e2d']['map'] = () => (f => __x__ => ((__x__) => {
  if (__x__.__constructor === "Just" && true) {
    let x = __x__.__args[0];
    return Just(f(x));
  }
  else if (__x__.__constructor === "Nothing") {
    return Nothing;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__));
Applicative['Maybe_eadd07e55d46112f77467431f86f5e2d'] = {};
Applicative['Maybe_eadd07e55d46112f77467431f86f5e2d']['ap'] = () => (mf => mx => ((__x__) => {
  if (__x__.length === 2 && __x__[0].__constructor === "Just" && true && __x__[1].__constructor === "Just" && true) {
    let [{ __args: [f]},{ __args: [x]}] = __x__;
    return Applicative.Maybe_eadd07e55d46112f77467431f86f5e2d.pure()(f(x));
  }
  else if (true) {
    return Nothing;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(([mf, mx])));
Applicative['Maybe_eadd07e55d46112f77467431f86f5e2d']['pure'] = () => Just;
Monad['Maybe_eadd07e55d46112f77467431f86f5e2d'] = {};
Monad['Maybe_eadd07e55d46112f77467431f86f5e2d']['chain'] = () => (f => m => ((__x__) => {
  if (__x__.__constructor === "Just" && true) {
    let x = __x__.__args[0];
    return f(x);
  }
  else if (__x__.__constructor === "Nothing") {
    return Nothing;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(m));
Monad['Maybe_eadd07e55d46112f77467431f86f5e2d']['of'] = () => Applicative.Maybe_eadd07e55d46112f77467431f86f5e2d.pure();
Show['Maybe_eadd07e55d46112f77467431f86f5e2d'] = {};
Show['Maybe_eadd07e55d46112f77467431f86f5e2d']['show'] = () => (Show_p119) => (__x__ => ((__x__) => {
  if (__x__.__constructor === "Just" && true) {
    let a = __x__.__args[0];
    return `Just ` + Show_p119.show()(a);
  }
  else if (__x__.__constructor === "Nothing") {
    return `Nothing`;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__));
export let fromMaybe = (or => __x__ => ((__x__) => {
  if (__x__.__constructor === "Just" && true) {
    let a = __x__.__args[0];
    return a;
  }
  else if (__x__.__constructor === "Nothing") {
    return or;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__));
export let isJust = (__x__ => ((__x__) => {
  if (__x__.__constructor === "Just" && true) {
    return true;
  }
  else if (__x__.__constructor === "Nothing") {
    return false;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__));
export default { fromMaybe, isJust, Just, Nothing };
