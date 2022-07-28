// file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/bin/prelude/__internal__/String.mad
import {} from "./../__internals__.mjs"
import { Just, Nothing } from "./Maybe.mjs";
import { MORE, LESS, EQUAL } from "./Compare.mjs";
import {  } from "./Monoid.mjs";
import List from "./List.mjs";

Semigroup['String_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Semigroup['String_5b7ebeeaa5acfe1eeea5a9e9845b152d']['assoc'] = () => (a => b => a + b);
Monoid['String_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Monoid['String_5b7ebeeaa5acfe1eeea5a9e9845b152d']['mconcat'] = () => (a => b => a + b);
Monoid['String_5b7ebeeaa5acfe1eeea5a9e9845b152d']['mempty'] = () => ``;
Comparable['String_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Comparable['String_5b7ebeeaa5acfe1eeea5a9e9845b152d']['compare'] = () => (a => b =>  a > b ? MORE : a === b ? EQUAL : LESS );
export let toLower = (s =>  s.toLowerCase() );
export let toUpper = (s =>  s.toUpperCase() );
export let split = (separator => str =>  {
  const items = str.split(separator)

  if (items.length === 0) {
    return null
  }

  let current = {}
  let output = current
  items.forEach((item) => {
    current = current.n = {}
    current.v = item
  })
  current.n = null

  return output.n
} );
export let join = (a => xs => (_P_ => List.reduce(Monoid.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.mconcat())(``)(List.intersperse(a)(_P_)))(xs));
export let lines = split(`\n`);
export let unlines = join(`\n`);
export let words = split(` `);
export let unwords = join(` `);
export let toList = (str =>  {
  let result = {}
  let current = result
  str.split('').forEach(c => {
    current = current.n = { v: c, n: null }
  })
  return result.n
} );
export let fromList = (list =>  {
  let chars = []
  while (list !== null) {
    chars.push(list.v)
    list = list.n
  }
  return chars.join('')
} );
export let mapChars = (f => s =>  s.split("").map(f).join("") );
export let filterChars = (predicate => s => (_P_ => fromList(List.filter(predicate)(toList(_P_))))(s));
export let reduceChars = (f => initial => s => (_P_ => List.reduce(f)(initial)(toList(_P_)))(s));
export let slice = (start => end => s =>  s.slice(start, end === 0 ? s.length : end) );
export let isEmpty = (s => __eq__(s, ``));
export let drop = (n => s => slice(n)(0)(s));
export let dropLast = (n => s => slice(0)(-n)(s));
export let dropWhile = (predicate => s => (_P_ => fromList(List.dropWhile(predicate)(toList(_P_))))(s));
export let take = (n => s => slice(0)(n)(s));
export let takeLast = (n => s => slice(-n)(0)(s));
export let takeWhile = (predicate => s => (_P_ => fromList(List.takeWhile(predicate)(toList(_P_))))(s));
export let charAt = (n => s => {
  const c = s[n]
  return !!c ? Just(c) : Nothing
});
export let firstChar = charAt(0);
export let lastChar = (s => charAt(_$_length_$_(s) - 1)(s));
export let trim = (s =>  s.trim() );
export let trimStart = (s =>  s.trimStart() );
export let trimEnd = (s =>  s.trimEnd() );
export let _$_length_$_ = (s =>  s.length );
export let repeat = (c => n => (_P_ => fromList(List.repeat(c)(_P_)))(n));
export let match = (regex => input =>  input.match(regex) !== null );
export let replace = (regex => replacing => input => 
  input.replace(new RegExp(regex, "g"), replacing)
);
export let pushChar = (c => s =>  c + s );
export let appendChar = (c => s =>  s + c );
export let reverse = (s => (_P_ => fromList(List.reverse(toList(_P_))))(s));
export default { toLower, toUpper, split, join, lines, unlines, words, unwords, toList, fromList, mapChars, filterChars, reduceChars, slice, isEmpty, drop, dropLast, dropWhile, take, takeLast, takeWhile, charAt, firstChar, lastChar, trim, trimStart, trimEnd, _$_length_$_, repeat, match, replace, pushChar, appendChar, reverse };
