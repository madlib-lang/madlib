// file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/bin/prelude/__internal__/Maybe.mad
import {} from "./../__internals__.mjs"
import {  } from "./Monad.mjs";
import {  } from "./Show.mjs";

export let Just = (a => ({ __constructor: "Just", __args: [ a ] }));
export let Nothing = ({ __constructor: "Nothing", __args: [  ] });
Inspect['Maybe_88ee2bbfb9fb271939df78a4e07d651c'] = {};
Inspect['Maybe_88ee2bbfb9fb271939df78a4e07d651c']['inspect'] = () => (Inspect_t45) => (__$a__ => ((__x__) => {
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
Functor['Maybe_88ee2bbfb9fb271939df78a4e07d651c'] = {};
Functor['Maybe_88ee2bbfb9fb271939df78a4e07d651c']['map'] = () => (f => __x__ => ((__x__) => {
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
Applicative['Maybe_88ee2bbfb9fb271939df78a4e07d651c'] = {};
Applicative['Maybe_88ee2bbfb9fb271939df78a4e07d651c']['ap'] = () => (mf => mx => ((__x__) => {
  if (__x__.length === 2 && __x__[0].__constructor === "Just" && true && __x__[1].__constructor === "Just" && true) {
    let [{ __args: [f]},{ __args: [x]}] = __x__;
    return Applicative.Maybe_88ee2bbfb9fb271939df78a4e07d651c.pure()(f(x));
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
Applicative['Maybe_88ee2bbfb9fb271939df78a4e07d651c']['pure'] = () => Just;
Monad['Maybe_88ee2bbfb9fb271939df78a4e07d651c'] = {};
Monad['Maybe_88ee2bbfb9fb271939df78a4e07d651c']['chain'] = () => (f => m => ((__x__) => {
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
Monad['Maybe_88ee2bbfb9fb271939df78a4e07d651c']['of'] = () => Applicative.Maybe_88ee2bbfb9fb271939df78a4e07d651c.pure();
Show['Maybe_88ee2bbfb9fb271939df78a4e07d651c'] = {};
Show['Maybe_88ee2bbfb9fb271939df78a4e07d651c']['show'] = () => (Show_p119) => (__x__ => ((__x__) => {
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
