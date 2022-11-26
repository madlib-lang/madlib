// file: /opt/hostedtoolcache/node/14.21.1/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Tuple.mad
import {} from "./../__internals__.mjs"
import {  } from "./Show.mjs";

Show['Tuple_2'] = {};
Show['Tuple_2']['show'] = () => (Show_y24) => (Show_x23) => (__x__ => ((__x__) => {
  if (__x__.length === 2 && true && true) {
    let [a,b] = __x__;
    return `#[` + Show_x23.show()(a) + `, ` + Show_y24.show()(b) + `]`;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__));
Show['Tuple_3'] = {};
Show['Tuple_3']['show'] = () => (Show_s44) => (Show_r43) => (Show_q42) => (__x__ => ((__x__) => {
  if (__x__.length === 3 && true && true && true) {
    let [a,b,c] = __x__;
    return `#[` + Show_q42.show()(a) + `, ` + Show_r43.show()(b) + `, ` + Show_s44.show()(c) + `]`;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__));
Show['Tuple_4'] = {};
Show['Tuple_4']['show'] = () => (Show_u72) => (Show_t71) => (Show_s70) => (Show_r69) => (__x__ => ((__x__) => {
  if (__x__.length === 4 && true && true && true && true) {
    let [a,b,c,d] = __x__;
    return `#[` + Show_r69.show()(a) + `, ` + Show_s70.show()(b) + `, ` + Show_t71.show()(c) + `, ` + Show_u72.show()(d) + `]`;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__));
export let fst = (tuple => ((__x__) => {
  if (__x__.length === 2 && true && true) {
    let [a,] = __x__;
    return a;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(tuple));
export let snd = (tuple => ((__x__) => {
  if (__x__.length === 2 && true && true) {
    let [,b] = __x__;
    return b;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(tuple));
export default { fst, snd };
