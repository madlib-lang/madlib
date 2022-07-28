// file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/bin/prelude/__internal__/JsonParser.mad
import {} from "./../__internals__.mjs"
import Parse from "./Parse.mjs";
import { char, choice, digit, many, notChar, runParser, sepBy, some, spaces, symbol } from "./Parse.mjs";
import String from "./String.mjs";
import List from "./List.mjs";
import { always } from "./Function.mjs";
import { Just, Nothing } from "./Maybe.mjs";
import { Left, Right } from "./Either.mjs";
import {  } from "./Number.mjs";
import Dictionary from "./Dictionary.mjs";
import { JsonString, JsonInteger, JsonFloat, JsonBoolean, JsonNull, JsonArray, JsonObject } from "./JsonValue.mjs";

export let Parser = (a => ({ __constructor: "Parser", __args: [ a ] }));
Inspect['Parser_2379ca5c5920d41be717dbbacc2a7786'] = {};
Inspect['Parser_2379ca5c5920d41be717dbbacc2a7786']['inspect'] = () => (Inspect_f2553) => (__$a__ => ((__x__) => {
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
Functor['Parser_2379ca5c5920d41be717dbbacc2a7786'] = {};
Functor['Parser_2379ca5c5920d41be717dbbacc2a7786']['map'] = () => (f => parser => Parser((input => ((__x__) => {
  if (__x__.__constructor === "Parser" && true) {
    let parserFn = __x__.__args[0];
    return Functor.Either_11f2a5613ec0faa56c0cc840a0dd5288.map()(f)(parserFn(input));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(parser))));
Applicative['Parser_2379ca5c5920d41be717dbbacc2a7786'] = {};
Applicative['Parser_2379ca5c5920d41be717dbbacc2a7786']['ap'] = () => (mf => parser => Parser((input => ((__x__) => {
  if (__x__.__constructor === "Parser" && true) {
    let f = __x__.__args[0];
    return ((__x__) => {
  if (__x__.__constructor === "Parser" && true) {
    let parserFn = __x__.__args[0];
    return Applicative.Either_11f2a5613ec0faa56c0cc840a0dd5288.ap()(f(input))(parserFn(input));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(parser);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(mf))));
Applicative['Parser_2379ca5c5920d41be717dbbacc2a7786']['pure'] = () => succeed;
Monad['Parser_2379ca5c5920d41be717dbbacc2a7786'] = {};
Monad['Parser_2379ca5c5920d41be717dbbacc2a7786']['chain'] = () => (fn => parser => Parser((input => ((__x__) => {
  if (__x__.__constructor === "Parser" && true) {
    let parserFn = __x__.__args[0];
    return ((__x__) => {
  if (__x__.__constructor === "Right" && true) {
    let a = __x__.__args[0];
    return ((__x__) => {
  if (__x__.__constructor === "Parser" && true) {
    let parserFn_ = __x__.__args[0];
    return parserFn_(input);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(fn(a));
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
})(parserFn(input));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(parser))));
Monad['Parser_2379ca5c5920d41be717dbbacc2a7786']['of'] = () => succeed;
let stringCharacter = choice(({ v: Functor.Parser_4ace3135073247c65fa2846bd5b7b62d.map()(always(__String.fromCharCode(34)))(Parse.string(`\\\"`)), n: { v: Functor.Parser_4ace3135073247c65fa2846bd5b7b62d.map()(always(__String.fromCharCode(10)))(Parse.string(`\\n`)), n: { v: Functor.Parser_4ace3135073247c65fa2846bd5b7b62d.map()(always(__String.fromCharCode(9)))(Parse.string(`\\t`)), n: { v: notChar(__String.fromCharCode(34)), n: null } } } }));
let jsonString = (() => {
  
  return Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()((_ => Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()((cs => Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()((_ => (_P_ => Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.of()(JsonString(String.fromList(_P_))))(cs)))(symbol(`"`))))(many(stringCharacter))))(char(__String.fromCharCode(34)))
})();
let jsonInteger = (_P_ => Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()((_P_ => (__x__ => ((__x__) => {
  if (__x__.__constructor === "Just" && true) {
    let i = __x__.__args[0];
    return Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.of()(JsonInteger(i));
  }
  else if (__x__.__constructor === "Nothing") {
    return Parse.fail;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__))(Scan.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.scan()(String.fromList(_P_)))))(some(_P_)))(digit);
let jsonFloat = (() => {
  
  return Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()((beforeDot => Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()((dot => Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()((afterDot => (_P_ => (__x__ => ((__x__) => {
  if (__x__.__constructor === "Just" && true) {
    let f = __x__.__args[0];
    return Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.of()(JsonFloat(f));
  }
  else if (__x__.__constructor === "Nothing") {
    return Parse.fail;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__))(Scan.Float_5b7ebeeaa5acfe1eeea5a9e9845b152d.scan()(String.fromList((__$PH1__ => List.concat(__$PH1__)(afterDot))(_P_)))))(List.append(dot)(beforeDot))))(some(digit))))(char(__String.fromCharCode(46)))))(some(digit))
})();
let jsonNull = (_P_ => Functor.Parser_4ace3135073247c65fa2846bd5b7b62d.map()((_ => JsonNull))(symbol(_P_)))(`null`);
let jsonBoolean = (_P_ => Functor.Parser_4ace3135073247c65fa2846bd5b7b62d.map()((b => (__eq__(b, `true`) ? JsonBoolean(true) : JsonBoolean(false))))(choice(_P_)))(({ v: symbol(`true`), n: { v: symbol(`false`), n: null } }));
let jsonArray = (() => {
  
  return Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()((_ => Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()((items => Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()((_ => Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()((_ => Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.of()(JsonArray(items))))(symbol(`]`))))(Alternative.Parser_4ace3135073247c65fa2846bd5b7b62d.alt()(spaces)(Applicative.Parser_4ace3135073247c65fa2846bd5b7b62d.pure()((null))))))(Alternative.Parser_4ace3135073247c65fa2846bd5b7b62d.alt()(sepBy(jsonValue)(symbol(`,`)))(Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.of()((null))))))(symbol(`[`))
})();
let objectField = (() => {
  
  return Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()((_ => Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()((fieldName => Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()((_ => Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()((_ => Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()((fieldValue => Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.of()(([String.fromList(fieldName), fieldValue]))))(jsonValue)))(symbol(`:`))))(char(__String.fromCharCode(34)))))(many(notChar(__String.fromCharCode(34))))))(char(__String.fromCharCode(34)))
})();
let jsonObject = (() => {
  
  return Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()((_ => Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()((fields => Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()((_ => Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()((_ => Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.of()(JsonObject(Dictionary.fromList(Comparable.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(fields)))))(symbol(`}`))))(Alternative.Parser_4ace3135073247c65fa2846bd5b7b62d.alt()(spaces)(Applicative.Parser_4ace3135073247c65fa2846bd5b7b62d.pure()((null))))))(Alternative.Parser_4ace3135073247c65fa2846bd5b7b62d.alt()(sepBy(objectField)(symbol(`,`)))(Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.of()((null))))))(symbol(`{`))
})();
let jsonValue = (() => {
  
  return Monad.Parser_4ace3135073247c65fa2846bd5b7b62d.chain()((_ => choice(({ v: jsonFloat, n: { v: jsonInteger, n: { v: jsonNull, n: { v: jsonBoolean, n: { v: jsonString, n: { v: jsonArray, n: { v: jsonObject, n: null } } } } } } }))))(Alternative.Parser_4ace3135073247c65fa2846bd5b7b62d.alt()(spaces)(Applicative.Parser_4ace3135073247c65fa2846bd5b7b62d.pure()((null))))
})();
let getParserFn = (parser => ((__x__) => {
  if (__x__.__constructor === "Parser" && true) {
    let fn = __x__.__args[0];
    return fn;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(parser));
export let parse = (parser => input => (_P_ => (__x__ => ((__x__) => {
  if (__x__.__constructor === "Left" && true) {
    let e = __x__.__args[0];
    return Left(`Invalid json: ` + Inspect.Error_4ace3135073247c65fa2846bd5b7b62d.inspect()(e));
  }
  else if (__x__.__constructor === "Right" && true) {
    let parsed = __x__.__args[0];
    return ((__x__) => {
  if (__x__.__constructor === "Parser" && true) {
    let parserFn = __x__.__args[0];
    return parserFn(parsed);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(parser);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__))(runParser(jsonValue)(_P_)))(input));
export let succeed = (a => Parser((_ => Right(a))));
export let fail = (err => Parser((_ => Left(err))));
export let string = Parser((input => ((__x__) => {
  if (__x__.__constructor === "JsonString" && true) {
    let s = __x__.__args[0];
    return Right(s);
  }
  else if (true) {
    return Left(`Error parsing string`);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(input)));
export let integer = Parser((input => ((__x__) => {
  if (__x__.__constructor === "JsonInteger" && true) {
    let i = __x__.__args[0];
    return Right(i);
  }
  else if (true) {
    return Left(`Error parsing integer`);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(input)));
export let float = Parser((input => ((__x__) => {
  if (__x__.__constructor === "JsonFloat" && true) {
    let f = __x__.__args[0];
    return Right(f);
  }
  else if (true) {
    return Left(`Error parsing float`);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(input)));
export let boolean = Parser((input => ((__x__) => {
  if (__x__.__constructor === "JsonBoolean" && true) {
    let b = __x__.__args[0];
    return Right(b);
  }
  else if (true) {
    return Left(`Error parsing boolean`);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(input)));
export let list = (parser => Parser((input => ((__x__) => {
  if (__x__.__constructor === "JsonArray" && true) {
    let arr = __x__.__args[0];
    return ((__x__) => {
  if (__x__.__constructor === "Parser" && true) {
    let parserFn = __x__.__args[0];
    return List.mapM(Functor.Either_11f2a5613ec0faa56c0cc840a0dd5288)(Applicative.Either_11f2a5613ec0faa56c0cc840a0dd5288)(parserFn)(arr);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(parser);
  }
  else if (true) {
    return Left(`Error parsing list`);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(input))));
export let dict = (parser => Parser((input => ((__x__) => {
  if (__x__.length === 2 && __x__[0].__constructor === "JsonObject" && true && __x__[1].__constructor === "Parser" && true) {
    let [{ __args: [d]},{ __args: [parserFn]}] = __x__;
    return Dictionary.mapM(Functor.Either_11f2a5613ec0faa56c0cc840a0dd5288)(Applicative.Either_11f2a5613ec0faa56c0cc840a0dd5288)(parserFn)(d);
  }
  else if (true) {
    return Left(`Error parsing dict`);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(([input, parser])))));
export let maybe = (parser => Parser((input => ((__x__) => {
  if (__x__.__constructor === "Right" && true) {
    let a = __x__.__args[0];
    return Right(Just(a));
  }
  else if (__x__.__constructor === "Left" && true) {
    return Right(Nothing);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(getParserFn(parser)(input)))));
export let lazy = (wrapped => Parser((input => ((__x__) => {
  if (__x__.__constructor === "Parser" && true) {
    let parserFn = __x__.__args[0];
    return parserFn(input);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(wrapped(({ __constructor: "Unit", __args: [] }))))));
export let field = (fieldName => parser => Parser((input => ((__x__) => {
  if (__x__.__constructor === "JsonObject" && true) {
    let d = __x__.__args[0];
    return (_P_ => (__x__ => ((__x__) => {
  if (__x__.__constructor === "Just" && true) {
    let value = __x__.__args[0];
    return getParserFn(parser)(value);
  }
  else if (__x__.__constructor === "Nothing") {
    return Left(`Error parsing fieldname '` + fieldName + `'`);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__))(Dictionary.get(Comparable.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(fieldName)(_P_)))(d);
  }
  else if (true) {
    return Left(`Error parsing fieldname '` + fieldName + `'`);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(input))));
export let path = (pathParts => parser => Parser((input => (_P_ => Monad.Either_11f2a5613ec0faa56c0cc840a0dd5288.chain()(getParserFn(parser))(List.reduce((val => fieldName => ((__x__) => {
  if (__x__.__constructor === "Right" && __x__.__args[0].__constructor === "JsonObject" && true) {
    let d = __x__.__args[0].__args[0];
    return (_P_ => (__x__ => ((__x__) => {
  if (__x__.__constructor === "Just" && true) {
    let value = __x__.__args[0];
    return Right(value);
  }
  else if (__x__.__constructor === "Nothing") {
    return Left(`Error parsing fieldname '` + fieldName + `'`);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__))(Dictionary.get(Comparable.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(fieldName)(_P_)))(d);
  }
  else if (true) {
    return Left(`Error parsing path: '` + Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(pathParts) + `' - value: '` + Inspect.Either_11f2a5613ec0faa56c0cc840a0dd5288.inspect()(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.JsonValue_84b71ee84616951011b6f654a88b372f)(val) + `'`);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(val)))(Right(input))(_P_)))(pathParts))));
export let chain1 = Monad.Parser_2379ca5c5920d41be717dbbacc2a7786.chain();
export let chain2 = (fn => parserA => parserB => Parser((input => ((__x__) => {
  if (__x__.length === 2 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true) {
    let [{ __args: [a]},{ __args: [b]}] = __x__;
    return getParserFn(fn(a)(b))(input);
  }
  else if (__x__.length === 2 && __x__[0].__constructor === "Left" && true && true) {
    let [{ __args: [e]},] = __x__;
    return Left(e);
  }
  else if (__x__.length === 2 && true && __x__[1].__constructor === "Left" && true) {
    let [,{ __args: [e]}] = __x__;
    return Left(e);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(([getParserFn(parserA)(input), getParserFn(parserB)(input)])))));
export let chain3 = (fn => parserA => parserB => parserC => Parser((input => ((__x__) => {
  if (__x__.length === 3 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true) {
    let [{ __args: [a]},{ __args: [b]},{ __args: [c]}] = __x__;
    return getParserFn(fn(a)(b)(c))(input);
  }
  else if (__x__.length === 3 && __x__[0].__constructor === "Left" && true && true && true) {
    let [{ __args: [e]},,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 3 && true && __x__[1].__constructor === "Left" && true && true) {
    let [,{ __args: [e]},] = __x__;
    return Left(e);
  }
  else if (__x__.length === 3 && true && true && __x__[2].__constructor === "Left" && true) {
    let [,,{ __args: [e]}] = __x__;
    return Left(e);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(([getParserFn(parserA)(input), getParserFn(parserB)(input), getParserFn(parserC)(input)])))));
export let chain4 = (fn => parserA => parserB => parserC => parserD => Parser((input => ((__x__) => {
  if (__x__.length === 4 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true && __x__[3].__constructor === "Right" && true) {
    let [{ __args: [a]},{ __args: [b]},{ __args: [c]},{ __args: [d]}] = __x__;
    return getParserFn(fn(a)(b)(c)(d))(input);
  }
  else if (__x__.length === 4 && __x__[0].__constructor === "Left" && true && true && true && true) {
    let [{ __args: [e]},,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 4 && true && __x__[1].__constructor === "Left" && true && true && true) {
    let [,{ __args: [e]},,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 4 && true && true && __x__[2].__constructor === "Left" && true && true) {
    let [,,{ __args: [e]},] = __x__;
    return Left(e);
  }
  else if (__x__.length === 4 && true && true && true && __x__[3].__constructor === "Left" && true) {
    let [,,,{ __args: [e]}] = __x__;
    return Left(e);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(([getParserFn(parserA)(input), getParserFn(parserB)(input), getParserFn(parserC)(input), getParserFn(parserD)(input)])))));
export let chain5 = (fn => parserA => parserB => parserC => parserD => parserE => Parser((input => ((__x__) => {
  if (__x__.length === 5 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true && __x__[3].__constructor === "Right" && true && __x__[4].__constructor === "Right" && true) {
    let [{ __args: [a]},{ __args: [b]},{ __args: [c]},{ __args: [d]},{ __args: [e]}] = __x__;
    return getParserFn(fn(a)(b)(c)(d)(e))(input);
  }
  else if (__x__.length === 5 && __x__[0].__constructor === "Left" && true && true && true && true && true) {
    let [{ __args: [e]},,,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 5 && true && __x__[1].__constructor === "Left" && true && true && true && true) {
    let [,{ __args: [e]},,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 5 && true && true && __x__[2].__constructor === "Left" && true && true && true) {
    let [,,{ __args: [e]},,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 5 && true && true && true && __x__[3].__constructor === "Left" && true && true) {
    let [,,,{ __args: [e]},] = __x__;
    return Left(e);
  }
  else if (__x__.length === 5 && true && true && true && true && __x__[4].__constructor === "Left" && true) {
    let [,,,,{ __args: [e]}] = __x__;
    return Left(e);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(([getParserFn(parserA)(input), getParserFn(parserB)(input), getParserFn(parserC)(input), getParserFn(parserD)(input), getParserFn(parserE)(input)])))));
export let chain6 = (fn => parserA => parserB => parserC => parserD => parserE => parserF => Parser((input => ((__x__) => {
  if (__x__.length === 6 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true && __x__[3].__constructor === "Right" && true && __x__[4].__constructor === "Right" && true && __x__[5].__constructor === "Right" && true) {
    let [{ __args: [a]},{ __args: [b]},{ __args: [c]},{ __args: [d]},{ __args: [e]},{ __args: [f]}] = __x__;
    return getParserFn(fn(a)(b)(c)(d)(e)(f))(input);
  }
  else if (__x__.length === 6 && __x__[0].__constructor === "Left" && true && true && true && true && true && true) {
    let [{ __args: [e]},,,,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 6 && true && __x__[1].__constructor === "Left" && true && true && true && true && true) {
    let [,{ __args: [e]},,,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 6 && true && true && __x__[2].__constructor === "Left" && true && true && true && true) {
    let [,,{ __args: [e]},,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 6 && true && true && true && __x__[3].__constructor === "Left" && true && true && true) {
    let [,,,{ __args: [e]},,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 6 && true && true && true && true && __x__[4].__constructor === "Left" && true && true) {
    let [,,,,{ __args: [e]},] = __x__;
    return Left(e);
  }
  else if (__x__.length === 6 && true && true && true && true && true && __x__[5].__constructor === "Left" && true) {
    let [,,,,,{ __args: [e]}] = __x__;
    return Left(e);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(([getParserFn(parserA)(input), getParserFn(parserB)(input), getParserFn(parserC)(input), getParserFn(parserD)(input), getParserFn(parserE)(input), getParserFn(parserF)(input)])))));
export let chain7 = (fn => parserA => parserB => parserC => parserD => parserE => parserF => parserG => Parser((input => ((__x__) => {
  if (__x__.length === 7 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true && __x__[3].__constructor === "Right" && true && __x__[4].__constructor === "Right" && true && __x__[5].__constructor === "Right" && true && __x__[6].__constructor === "Right" && true) {
    let [{ __args: [a]},{ __args: [b]},{ __args: [c]},{ __args: [d]},{ __args: [e]},{ __args: [f]},{ __args: [g]}] = __x__;
    return getParserFn(fn(a)(b)(c)(d)(e)(f)(g))(input);
  }
  else if (__x__.length === 7 && __x__[0].__constructor === "Left" && true && true && true && true && true && true && true) {
    let [{ __args: [e]},,,,,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 7 && true && __x__[1].__constructor === "Left" && true && true && true && true && true && true) {
    let [,{ __args: [e]},,,,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 7 && true && true && __x__[2].__constructor === "Left" && true && true && true && true && true) {
    let [,,{ __args: [e]},,,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 7 && true && true && true && __x__[3].__constructor === "Left" && true && true && true && true) {
    let [,,,{ __args: [e]},,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 7 && true && true && true && true && __x__[4].__constructor === "Left" && true && true && true) {
    let [,,,,{ __args: [e]},,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 7 && true && true && true && true && true && __x__[5].__constructor === "Left" && true && true) {
    let [,,,,,{ __args: [e]},] = __x__;
    return Left(e);
  }
  else if (__x__.length === 7 && true && true && true && true && true && true && __x__[6].__constructor === "Left" && true) {
    let [,,,,,,{ __args: [e]}] = __x__;
    return Left(e);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(([getParserFn(parserA)(input), getParserFn(parserB)(input), getParserFn(parserC)(input), getParserFn(parserD)(input), getParserFn(parserE)(input), getParserFn(parserF)(input), getParserFn(parserG)(input)])))));
export let chain8 = (fn => parserA => parserB => parserC => parserD => parserE => parserF => parserG => parserH => Parser((input => ((__x__) => {
  if (__x__.length === 8 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true && __x__[3].__constructor === "Right" && true && __x__[4].__constructor === "Right" && true && __x__[5].__constructor === "Right" && true && __x__[6].__constructor === "Right" && true && __x__[7].__constructor === "Right" && true) {
    let [{ __args: [a]},{ __args: [b]},{ __args: [c]},{ __args: [d]},{ __args: [e]},{ __args: [f]},{ __args: [g]},{ __args: [h]}] = __x__;
    return getParserFn(fn(a)(b)(c)(d)(e)(f)(g)(h))(input);
  }
  else if (__x__.length === 8 && __x__[0].__constructor === "Left" && true && true && true && true && true && true && true && true) {
    let [{ __args: [e]},,,,,,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 8 && true && __x__[1].__constructor === "Left" && true && true && true && true && true && true && true) {
    let [,{ __args: [e]},,,,,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 8 && true && true && __x__[2].__constructor === "Left" && true && true && true && true && true && true) {
    let [,,{ __args: [e]},,,,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 8 && true && true && true && __x__[3].__constructor === "Left" && true && true && true && true && true) {
    let [,,,{ __args: [e]},,,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 8 && true && true && true && true && __x__[4].__constructor === "Left" && true && true && true && true) {
    let [,,,,{ __args: [e]},,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 8 && true && true && true && true && true && __x__[5].__constructor === "Left" && true && true && true) {
    let [,,,,,{ __args: [e]},,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 8 && true && true && true && true && true && true && __x__[6].__constructor === "Left" && true && true) {
    let [,,,,,,{ __args: [e]},] = __x__;
    return Left(e);
  }
  else if (__x__.length === 8 && true && true && true && true && true && true && true && __x__[7].__constructor === "Left" && true) {
    let [,,,,,,,{ __args: [e]}] = __x__;
    return Left(e);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(([getParserFn(parserA)(input), getParserFn(parserB)(input), getParserFn(parserC)(input), getParserFn(parserD)(input), getParserFn(parserE)(input), getParserFn(parserF)(input), getParserFn(parserG)(input), getParserFn(parserH)(input)])))));
export let map1 = Functor.Parser_2379ca5c5920d41be717dbbacc2a7786.map();
export let map2 = (fn => parserA => parserB => Parser((input => ((__x__) => {
  if (__x__.length === 2 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true) {
    let [{ __args: [a]},{ __args: [b]}] = __x__;
    return Right(fn(a)(b));
  }
  else if (__x__.length === 2 && __x__[0].__constructor === "Left" && true && true) {
    let [{ __args: [e]},] = __x__;
    return Left(e);
  }
  else if (__x__.length === 2 && true && __x__[1].__constructor === "Left" && true) {
    let [,{ __args: [e]}] = __x__;
    return Left(e);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(([getParserFn(parserA)(input), getParserFn(parserB)(input)])))));
export let map3 = (fn => parserA => parserB => parserC => Parser((input => ((__x__) => {
  if (__x__.length === 3 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true) {
    let [{ __args: [a]},{ __args: [b]},{ __args: [c]}] = __x__;
    return Right(fn(a)(b)(c));
  }
  else if (__x__.length === 3 && __x__[0].__constructor === "Left" && true && true && true) {
    let [{ __args: [e]},,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 3 && true && __x__[1].__constructor === "Left" && true && true) {
    let [,{ __args: [e]},] = __x__;
    return Left(e);
  }
  else if (__x__.length === 3 && true && true && __x__[2].__constructor === "Left" && true) {
    let [,,{ __args: [e]}] = __x__;
    return Left(e);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(([getParserFn(parserA)(input), getParserFn(parserB)(input), getParserFn(parserC)(input)])))));
export let map4 = (fn => parserA => parserB => parserC => parserD => Parser((input => ((__x__) => {
  if (__x__.length === 4 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true && __x__[3].__constructor === "Right" && true) {
    let [{ __args: [a]},{ __args: [b]},{ __args: [c]},{ __args: [d]}] = __x__;
    return Right(fn(a)(b)(c)(d));
  }
  else if (__x__.length === 4 && __x__[0].__constructor === "Left" && true && true && true && true) {
    let [{ __args: [e]},,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 4 && true && __x__[1].__constructor === "Left" && true && true && true) {
    let [,{ __args: [e]},,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 4 && true && true && __x__[2].__constructor === "Left" && true && true) {
    let [,,{ __args: [e]},] = __x__;
    return Left(e);
  }
  else if (__x__.length === 4 && true && true && true && __x__[3].__constructor === "Left" && true) {
    let [,,,{ __args: [e]}] = __x__;
    return Left(e);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(([getParserFn(parserA)(input), getParserFn(parserB)(input), getParserFn(parserC)(input), getParserFn(parserD)(input)])))));
export let map5 = (fn => parserA => parserB => parserC => parserD => parserE => Parser((input => ((__x__) => {
  if (__x__.length === 5 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true && __x__[3].__constructor === "Right" && true && __x__[4].__constructor === "Right" && true) {
    let [{ __args: [a]},{ __args: [b]},{ __args: [c]},{ __args: [d]},{ __args: [e]}] = __x__;
    return Right(fn(a)(b)(c)(d)(e));
  }
  else if (__x__.length === 5 && __x__[0].__constructor === "Left" && true && true && true && true && true) {
    let [{ __args: [e]},,,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 5 && true && __x__[1].__constructor === "Left" && true && true && true && true) {
    let [,{ __args: [e]},,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 5 && true && true && __x__[2].__constructor === "Left" && true && true && true) {
    let [,,{ __args: [e]},,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 5 && true && true && true && __x__[3].__constructor === "Left" && true && true) {
    let [,,,{ __args: [e]},] = __x__;
    return Left(e);
  }
  else if (__x__.length === 5 && true && true && true && true && __x__[4].__constructor === "Left" && true) {
    let [,,,,{ __args: [e]}] = __x__;
    return Left(e);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(([getParserFn(parserA)(input), getParserFn(parserB)(input), getParserFn(parserC)(input), getParserFn(parserD)(input), getParserFn(parserE)(input)])))));
export let map6 = (fn => parserA => parserB => parserC => parserD => parserE => parserF => Parser((input => ((__x__) => {
  if (__x__.length === 6 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true && __x__[3].__constructor === "Right" && true && __x__[4].__constructor === "Right" && true && __x__[5].__constructor === "Right" && true) {
    let [{ __args: [a]},{ __args: [b]},{ __args: [c]},{ __args: [d]},{ __args: [e]},{ __args: [f]}] = __x__;
    return Right(fn(a)(b)(c)(d)(e)(f));
  }
  else if (__x__.length === 6 && __x__[0].__constructor === "Left" && true && true && true && true && true && true) {
    let [{ __args: [e]},,,,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 6 && true && __x__[1].__constructor === "Left" && true && true && true && true && true) {
    let [,{ __args: [e]},,,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 6 && true && true && __x__[2].__constructor === "Left" && true && true && true && true) {
    let [,,{ __args: [e]},,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 6 && true && true && true && __x__[3].__constructor === "Left" && true && true && true) {
    let [,,,{ __args: [e]},,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 6 && true && true && true && true && __x__[4].__constructor === "Left" && true && true) {
    let [,,,,{ __args: [e]},] = __x__;
    return Left(e);
  }
  else if (__x__.length === 6 && true && true && true && true && true && __x__[5].__constructor === "Left" && true) {
    let [,,,,,{ __args: [e]}] = __x__;
    return Left(e);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(([getParserFn(parserA)(input), getParserFn(parserB)(input), getParserFn(parserC)(input), getParserFn(parserD)(input), getParserFn(parserE)(input), getParserFn(parserF)(input)])))));
export let map7 = (fn => parserA => parserB => parserC => parserD => parserE => parserF => parserG => Parser((input => ((__x__) => {
  if (__x__.length === 7 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true && __x__[3].__constructor === "Right" && true && __x__[4].__constructor === "Right" && true && __x__[5].__constructor === "Right" && true && __x__[6].__constructor === "Right" && true) {
    let [{ __args: [a]},{ __args: [b]},{ __args: [c]},{ __args: [d]},{ __args: [e]},{ __args: [f]},{ __args: [g]}] = __x__;
    return Right(fn(a)(b)(c)(d)(e)(f)(g));
  }
  else if (__x__.length === 7 && __x__[0].__constructor === "Left" && true && true && true && true && true && true && true) {
    let [{ __args: [e]},,,,,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 7 && true && __x__[1].__constructor === "Left" && true && true && true && true && true && true) {
    let [,{ __args: [e]},,,,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 7 && true && true && __x__[2].__constructor === "Left" && true && true && true && true && true) {
    let [,,{ __args: [e]},,,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 7 && true && true && true && __x__[3].__constructor === "Left" && true && true && true && true) {
    let [,,,{ __args: [e]},,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 7 && true && true && true && true && __x__[4].__constructor === "Left" && true && true && true) {
    let [,,,,{ __args: [e]},,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 7 && true && true && true && true && true && __x__[5].__constructor === "Left" && true && true) {
    let [,,,,,{ __args: [e]},] = __x__;
    return Left(e);
  }
  else if (__x__.length === 7 && true && true && true && true && true && true && __x__[6].__constructor === "Left" && true) {
    let [,,,,,,{ __args: [e]}] = __x__;
    return Left(e);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(([getParserFn(parserA)(input), getParserFn(parserB)(input), getParserFn(parserC)(input), getParserFn(parserD)(input), getParserFn(parserE)(input), getParserFn(parserF)(input), getParserFn(parserG)(input)])))));
export let map8 = (fn => parserA => parserB => parserC => parserD => parserE => parserF => parserG => parserH => Parser((input => ((__x__) => {
  if (__x__.length === 8 && __x__[0].__constructor === "Right" && true && __x__[1].__constructor === "Right" && true && __x__[2].__constructor === "Right" && true && __x__[3].__constructor === "Right" && true && __x__[4].__constructor === "Right" && true && __x__[5].__constructor === "Right" && true && __x__[6].__constructor === "Right" && true && __x__[7].__constructor === "Right" && true) {
    let [{ __args: [a]},{ __args: [b]},{ __args: [c]},{ __args: [d]},{ __args: [e]},{ __args: [f]},{ __args: [g]},{ __args: [h]}] = __x__;
    return Right(fn(a)(b)(c)(d)(e)(f)(g)(h));
  }
  else if (__x__.length === 8 && __x__[0].__constructor === "Left" && true && true && true && true && true && true && true && true) {
    let [{ __args: [e]},,,,,,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 8 && true && __x__[1].__constructor === "Left" && true && true && true && true && true && true && true) {
    let [,{ __args: [e]},,,,,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 8 && true && true && __x__[2].__constructor === "Left" && true && true && true && true && true && true) {
    let [,,{ __args: [e]},,,,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 8 && true && true && true && __x__[3].__constructor === "Left" && true && true && true && true && true) {
    let [,,,{ __args: [e]},,,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 8 && true && true && true && true && __x__[4].__constructor === "Left" && true && true && true && true) {
    let [,,,,{ __args: [e]},,,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 8 && true && true && true && true && true && __x__[5].__constructor === "Left" && true && true && true) {
    let [,,,,,{ __args: [e]},,] = __x__;
    return Left(e);
  }
  else if (__x__.length === 8 && true && true && true && true && true && true && __x__[6].__constructor === "Left" && true && true) {
    let [,,,,,,{ __args: [e]},] = __x__;
    return Left(e);
  }
  else if (__x__.length === 8 && true && true && true && true && true && true && true && __x__[7].__constructor === "Left" && true) {
    let [,,,,,,,{ __args: [e]}] = __x__;
    return Left(e);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(([getParserFn(parserA)(input), getParserFn(parserB)(input), getParserFn(parserC)(input), getParserFn(parserD)(input), getParserFn(parserE)(input), getParserFn(parserF)(input), getParserFn(parserG)(input), getParserFn(parserH)(input)])))));
export default { parse, succeed, fail, string, integer, float, boolean, list, dict, maybe, lazy, field, path, chain1, chain2, chain3, chain4, chain5, chain6, chain7, chain8, map1, map2, map3, map4, map5, map6, map7, map8, Parser };
