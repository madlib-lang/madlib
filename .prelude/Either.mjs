// file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Either.mad
import {} from "./../__internals__.mjs"
import {  } from "./Monad.mjs";
import {  } from "./Bifunctor.mjs";
import {  } from "./Show.mjs";

export let Left = (a => ({ __constructor: "Left", __args: [ a ] }));
export let Right = (a => ({ __constructor: "Right", __args: [ a ] }));
Inspect['Either_8b29150c5dbf7739dbfa340875ffa50e'] = {};
Inspect['Either_8b29150c5dbf7739dbfa340875ffa50e']['inspect'] = () => (Inspect_l115) => (Inspect_o118) => (__$a__ => ((__x__) => {
  if (__x__.__constructor === "Left" && true) {
    let a0 = __x__.__args[0];
    return `Left(` + Inspect_l115.inspect()(a0) + `)`;
  }
  else if (__x__.__constructor === "Right" && true) {
    let a0 = __x__.__args[0];
    return `Right(` + Inspect_o118.inspect()(a0) + `)`;
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
Functor['Either_8b29150c5dbf7739dbfa340875ffa50e'] = {};
Functor['Either_8b29150c5dbf7739dbfa340875ffa50e']['map'] = () => (f => __x__ => ((__x__) => {
  if (__x__.__constructor === "Right" && true) {
    let a = __x__.__args[0];
    return Right(f(a));
  }
  else if (__x__.__constructor === "Left" && true) {
    let e = __x__.__args[0];
    return Left(e);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__));
Applicative['Either_8b29150c5dbf7739dbfa340875ffa50e'] = {};
Applicative['Either_8b29150c5dbf7739dbfa340875ffa50e']['ap'] = () => (mf => m => ((__x__) => {
  if (__x__.__constructor === "Left" && true) {
    let e = __x__.__args[0];
    return Left(e);
  }
  else if (__x__.__constructor === "Right" && true) {
    let f = __x__.__args[0];
    return Functor.Either_8b29150c5dbf7739dbfa340875ffa50e.map()(f)(m);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(mf));
Applicative['Either_8b29150c5dbf7739dbfa340875ffa50e']['pure'] = () => Right;
Monad['Either_8b29150c5dbf7739dbfa340875ffa50e'] = {};
Monad['Either_8b29150c5dbf7739dbfa340875ffa50e']['chain'] = () => (f => __x__ => ((__x__) => {
  if (__x__.__constructor === "Right" && true) {
    let a = __x__.__args[0];
    return f(a);
  }
  else if (__x__.__constructor === "Left" && true) {
    let e = __x__.__args[0];
    return Left(e);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__));
Monad['Either_8b29150c5dbf7739dbfa340875ffa50e']['of'] = () => Applicative.Either_8b29150c5dbf7739dbfa340875ffa50e.pure();
Bifunctor['Either_8b29150c5dbf7739dbfa340875ffa50e'] = {};
Bifunctor['Either_8b29150c5dbf7739dbfa340875ffa50e']['bimap'] = () => (leftF => rightF => __x__ => ((__x__) => {
  if (__x__.__constructor === "Right" && true) {
    let a = __x__.__args[0];
    return Right(rightF(a));
  }
  else if (__x__.__constructor === "Left" && true) {
    let e = __x__.__args[0];
    return Left(leftF(e));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__));
Bifunctor['Either_8b29150c5dbf7739dbfa340875ffa50e']['mapFirst'] = () => mapLeft;
Bifunctor['Either_8b29150c5dbf7739dbfa340875ffa50e']['mapSecond'] = () => Functor.Either_8b29150c5dbf7739dbfa340875ffa50e.map();
Show['Either_8b29150c5dbf7739dbfa340875ffa50e'] = {};
Show['Either_8b29150c5dbf7739dbfa340875ffa50e']['show'] = () => (Show_m272) => (Show_p275) => (__x__ => ((__x__) => {
  if (__x__.__constructor === "Right" && true) {
    let a = __x__.__args[0];
    return `Right ` + Show_m272.show()(a);
  }
  else if (__x__.__constructor === "Left" && true) {
    let e = __x__.__args[0];
    return `Left ` + Show_p275.show()(e);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__));
export let mapRight = Functor.Either_8b29150c5dbf7739dbfa340875ffa50e.map();
export let mapLeft = (f => m => ((__x__) => {
  if (__x__.__constructor === "Right" && true) {
    let a = __x__.__args[0];
    return Right(a);
  }
  else if (__x__.__constructor === "Left" && true) {
    let e = __x__.__args[0];
    return Left(f(e));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(m));
export let isLeft = (either => ((__x__) => {
  if (__x__.__constructor === "Left" && true) {
    return true;
  }
  else if (true) {
    return false;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(either));
export let isRight = (either => ((__x__) => {
  if (__x__.__constructor === "Right" && true) {
    return true;
  }
  else if (true) {
    return false;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(either));
export let fromRight = (a => either => ((__x__) => {
  if (__x__.__constructor === "Right" && true) {
    let x = __x__.__args[0];
    return x;
  }
  else if (true) {
    return a;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(either));
export default { mapRight, mapLeft, isLeft, isRight, fromRight, Left, Right };
