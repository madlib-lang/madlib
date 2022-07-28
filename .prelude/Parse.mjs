// file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/bin/prelude/__internal__/Parse.mad
import {} from "./../__internals__.mjs"
import { andDo } from "./Monad.mjs";
import {  } from "./Alternative.mjs";
import {  } from "./Number.mjs";
import Char from "./Char.mjs";
import { apL } from "./Applicative.mjs";
import { Right, Left } from "./Either.mjs";
import { Just, Nothing } from "./Maybe.mjs";
import String from "./String.mjs";
import List from "./List.mjs";
import { always, complement, equals, ifElse, notEquals } from "./Function.mjs";
import { maybeLoop, _$_while_$_ } from "./Control.mjs";

export let Loc = (a => b => c => ({ __constructor: "Loc", __args: [ a, b, c ] }));
export let Parser = (a => ({ __constructor: "Parser", __args: [ a ] }));
export let Error = (a => ({ __constructor: "Error", __args: [ a ] }));
Inspect['Location_4ace3135073247c65fa2846bd5b7b62d'] = {};
Inspect['Location_4ace3135073247c65fa2846bd5b7b62d']['inspect'] = () => (__$a__ => ((__x__) => {
  if (__x__.__constructor === "Loc" && true && true && true) {
    let a0 = __x__.__args[0];
    let a1 = __x__.__args[1];
    let a2 = __x__.__args[2];
    return `Loc(` + Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `, ` + Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a1) + `, ` + Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a2) + `)`;
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
Inspect['Parser_4ace3135073247c65fa2846bd5b7b62d'] = {};
Inspect['Parser_4ace3135073247c65fa2846bd5b7b62d']['inspect'] = () => (Inspect_d549) => (__$a__ => ((__x__) => {
  if (__x__.__constructor === "Parser" && true) {
    let a0 = __x__.__args[0];
    return `Parser(` + Inspect.a_arr_b.inspect()(a0) + `)`;
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
Inspect['Error_4ace3135073247c65fa2846bd5b7b62d'] = {};
Inspect['Error_4ace3135073247c65fa2846bd5b7b62d']['inspect'] = () => (__$a__ => ((__x__) => {
  if (__x__.__constructor === "Error" && true) {
    let a0 = __x__.__args[0];
    return `Error(` + Inspect.Location_4ace3135073247c65fa2846bd5b7b62d.inspect()(a0) + `)`;
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
Functor['Parser_4ace3135073247c65fa2846bd5b7b62d'] = {};
Functor['Parser_4ace3135073247c65fa2846bd5b7b62d']['map'] = () => (f => m => Parser((s => l => ((__x__) => {
  if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && true && true && __x__[0].n === null && true) {
    let [{ v: [a, b] },loc] = __x__;
    return ([({ v: ([f(a), b]), n: null }), loc]);
  }
  else if (__x__.length === 2 && __x__[0] === null && true) {
    let [,loc] = __x__;
    return ([(null), loc]);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(parse(m)(s)(l)))));
Applicative['Parser_4ace3135073247c65fa2846bd5b7b62d'] = {};
Applicative['Parser_4ace3135073247c65fa2846bd5b7b62d']['ap'] = () => (parserA => parserB => Parser((s => l => ((__x__) => {
  if (__x__.length === 2 && __x__[0] === null && true) {
    let [,l1] = __x__;
    return ([(null), l1]);
  }
  else if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && true && true && __x__[0].n === null && true) {
    let [{ v: [f, s1] },l1] = __x__;
    return ((__x__) => {
  if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && true && true && __x__[0].n === null && true) {
    let [{ v: [a, s2] },l2] = __x__;
    return ([({ v: ([f(a), s2]), n: null }), l2]);
  }
  else if (__x__.length === 2 && __x__[0] === null && true) {
    let [,l2] = __x__;
    return ([(null), l2]);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(parse(parserB)(s1)(l1));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(parse(parserA)(s)(l)))));
Applicative['Parser_4ace3135073247c65fa2846bd5b7b62d']['pure'] = () => (a => Parser((s => l => ([({ v: ([a, s]), n: null }), l]))));
Monad['Parser_4ace3135073247c65fa2846bd5b7b62d'] = {};
Monad['Parser_4ace3135073247c65fa2846bd5b7b62d']['chain'] = () => (f => m => Parser((s => l => ((__x__) => {
  if (__x__.length === 2 && __x__[0] === null && true) {
    let [,ll] = __x__;
    return ([(null), ll]);
  }
  else if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && true && true && __x__[0].n === null && true) {
    let [{ v: [a, s1] },l1] = __x__;
    return parse(f(a))(s1)(l1);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(parse(m)(s)(l)))));
Monad['Parser_4ace3135073247c65fa2846bd5b7b62d']['of'] = () => Applicative.Parser_4ace3135073247c65fa2846bd5b7b62d.pure();
Alternative['Parser_4ace3135073247c65fa2846bd5b7b62d'] = {};
Alternative['Parser_4ace3135073247c65fa2846bd5b7b62d']['aempty'] = () => Parser((_ => l => ([(null), l])));
Alternative['Parser_4ace3135073247c65fa2846bd5b7b62d']['alt'] = () => (ma => mb => Parser((s => l => ((__x__) => {
  if (__x__.length === 2 && __x__[0] === null && true) {
    let [,] = __x__;
    return parse(mb)(s)(l);
  }
  else if (true) {
    let res = __x__;
    return res;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(parse(ma)(s)(l)))));
let incLoc = (c => __x__ => ((__x__) => {
  if (__x__.__constructor === "Loc" && true && true && true) {
    let abs = __x__.__args[0];
    let line = __x__.__args[1];
    let col = __x__.__args[2];
    return (__eq__(c, __String.fromCharCode(10)) ? Loc(abs + 1)(line + 1)(0) : Loc(abs + 1)(line)(col + 1));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__));
let parse = (parser => input => loc => ((__x__) => {
  if (__x__.__constructor === "Parser" && true) {
    let fn = __x__.__args[0];
    return fn(input)(loc);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(parser));
export let runParser = (m => s => ((__x__) => {
  if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && true && __x__[0].v[1] === "" && __x__[0].n === null && true) {
    let [{ v: [res, ] },] = __x__;
    return Right(res);
  }
  else if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && true && true && __x__[0].n === null && true) {
    let [{ v: [, rest] },l] = __x__;
    return Left(Error(l));
  }
  else if (__x__.length === 2 && true && true) {
    let [,l] = __x__;
    return Left(Error(l));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(parse(m)(s)(Loc(0)(0)(0))));
export let fail = Alternative.Parser_4ace3135073247c65fa2846bd5b7b62d.aempty();
export let anyChar = Parser((s => l => ((__x__) => {
  if (__x__.__constructor === "Just" && true) {
    let c = __x__.__args[0];
    return ([({ v: ([c, String.drop(1)(s)]), n: null }), incLoc(c)(l)]);
  }
  else if (__x__.__constructor === "Nothing") {
    return ([(null), l]);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(String.charAt(0)(s))));
export let location = Parser((s => l => ([({ v: ([l, s]), n: null }), l])));
export let oneOf = (cs => satisfy((__$PH1__ => List.includes(__$PH1__)(cs))));
export let notOneOf = (cs => satisfy(complement((__$PH2__ => List.includes(__$PH2__)(cs)))));
export let choice = (ps => List.reduce(Alternative.Parser_4ace3135073247c65fa2846bd5b7b62d.alt())(fail)(ps));
export let many = (p => Parser((s => l => {
    let rest = s;
    let loc = l;
    let acc = (null);
    _$_while_$_((_ => ((__x__) => {
  if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && true && true && __x__[0].n === null && true) {
    let [{ v: [parsed, r] },loc_] = __x__;
    return (() => {
  rest = r
  loc = loc_
  acc = ({ v: parsed, n: acc })
  return true
})();
  }
  else if (true) {
    return false;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(parse(p)(rest)(loc))));
    return ([({ v: ([List.reverse(acc), rest]), n: null }), loc]);
})));
export let some = (p => Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()((first => Functor.Parser_4ace3135073247c65fa2846bd5b7b62d.map()((rest => ({ v: first, n: rest })))(many(p))))(p));
export let manyTill = (p => end => Parser((s => l => {
    let result = maybeLoop(([s, l, (null)]))((state => ((__x__) => {
  if (__x__.length === 3 && true && true && true) {
    let [ss,ll,acc] = __x__;
    return ((__x__) => {
  if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && true && true && __x__[0].n === null && true) {
    let [{ v: [, ] },] = __x__;
    return Nothing;
  }
  else if (true) {
    return ((__x__) => {
  if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && true && true && __x__[0].n === null && true) {
    let [{ v: [parsed, rest] },loc] = __x__;
    return Just(([rest, loc, ({ v: parsed, n: acc })]));
  }
  else if (true) {
    return Nothing;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(parse(p)(ss)(ll));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(parse(end)(ss)(ll));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(state)));
    return ((__x__) => {
  if (__x__.length === 3 && true && true && true) {
    let [rest,loc,parseResult] = __x__;
    return ([({ v: ([List.reverse(parseResult), rest]), n: null }), loc]);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(result);
})));
export let someTill = (p => end => Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()((first => Functor.Parser_4ace3135073247c65fa2846bd5b7b62d.map()((rest => Monoid.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.mconcat()(({ v: first, n: null }))(rest)))(manyTill(p)(end))))(p));
export let lookAhead = (p => Parser((s => l => ((__x__) => {
  if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && true && true && __x__[0].n === null && true) {
    let [{ v: [a, ] },] = __x__;
    return ([({ v: ([a, s]), n: null }), l]);
  }
  else if (true) {
    return ([(null), l]);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(parse(p)(s)(l)))));
export let takeWhile = (pred => Parser((s => l => {
    let result = maybeLoop(([s, l, (null)]))((state => ((__x__) => {
  if (__x__.length === 3 && true && true && true) {
    let [ss,ll,acc] = __x__;
    return ((__x__) => {
  if (__x__.length === 2 && __x__[0] !== null && __x__[0].v.length === 2 && true && true && __x__[0].n === null && true) {
    let [{ v: [parsed, rest] },loc] = __x__;
    return (pred(parsed) ? Just(([rest, loc, ({ v: parsed, n: acc })])) : Nothing);
  }
  else if (true) {
    return Nothing;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(parse(anyChar)(ss)(ll));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(state)));
    return ((__x__) => {
  if (__x__.length === 3 && true && true && true) {
    let [rest,loc,parseResult] = __x__;
    return ([({ v: ([List.reverse(parseResult), rest]), n: null }), loc]);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(result);
})));
export let sepBy = (parser => separator => Alternative.Parser_4ace3135073247c65fa2846bd5b7b62d.alt()((() => {
  
  return Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()((first => Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()((rest => Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.of()(({ v: first, n: rest }))))(many(andDo(Functor.Parser_4ace3135073247c65fa2846bd5b7b62d)(Applicative.Parser_4ace3135073247c65fa2846bd5b7b62d)(Monad.Parser_4ace3135073247c65fa2846bd5b7b62d)(parser)(separator)))))(parser)
})())(fail));
export let satisfy = (pred => Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()(ifElse(pred)(Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.of())(always(fail)))(anyChar));
export let char = (_P_ => satisfy(equals(_P_)));
export let notChar = (_P_ => satisfy(notEquals(_P_)));
export let eof = Parser((s => l => ((__x__) => {
  if (__x__.length === 2 && __x__[0] === null && true) {
    let [,] = __x__;
    return ([({ v: ([({ __constructor: "Unit", __args: [] }), ``]), n: null }), l]);
  }
  else if (true) {
    return ([(null), l]);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(parse(anyChar)(s)(l))));
export let string = (s => ((__x__) => {
  if (__x__.__constructor === "Just" && true) {
    let c = __x__.__args[0];
    return (_P_ => (__$PH3__ => Applicative.Parser_4ace3135073247c65fa2846bd5b7b62d.ap()(__$PH3__)(string(String.drop(1)(s))))(Functor.Parser_4ace3135073247c65fa2846bd5b7b62d.map()((a => b => String.pushChar(a)(b)))(_P_)))(char(c));
  }
  else if (__x__.__constructor === "Nothing") {
    return Applicative.Parser_4ace3135073247c65fa2846bd5b7b62d.pure()(``);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(String.firstChar(s)));
export let spaces = (_P_ => some(oneOf(_P_)))(({ v: __String.fromCharCode(32), n: { v: __String.fromCharCode(10), n: { v: __String.fromCharCode(13), n: { v: __String.fromCharCode(9), n: null } } } }));
export let token = (__$PH4__ => apL(Functor.Parser_4ace3135073247c65fa2846bd5b7b62d)(Applicative.Parser_4ace3135073247c65fa2846bd5b7b62d)(__$PH4__)(Alternative.Parser_4ace3135073247c65fa2846bd5b7b62d.alt()(spaces)(Applicative.Parser_4ace3135073247c65fa2846bd5b7b62d.pure()((null)))));
export let symbol = (_P_ => token(string(_P_)));
export let digit = satisfy(Char.isDigit);
export let letter = satisfy(Char.isLetter);
export let letters = many(letter);
export default { runParser, fail, anyChar, location, oneOf, notOneOf, choice, many, some, manyTill, someTill, lookAhead, takeWhile, sepBy, satisfy, char, notChar, eof, string, spaces, token, symbol, digit, letter, letters, Loc, Parser, Error };
