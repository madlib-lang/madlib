// file: /opt/hostedtoolcache/node/14.20.1/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Number.mad
import {} from "./../__internals__.mjs"
import { Just, Nothing } from "./Maybe.mjs";
import { EQUAL, LESS, MORE } from "./Compare.mjs";
import {  } from "./Scan.mjs";
import {  } from "./Show.mjs";

Comparable['Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Comparable['Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d']['compare'] = () => (a => b => (a > b ? MORE : (__eq__(a, b) ? EQUAL : LESS)));
Comparable['Byte_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Comparable['Byte_5b7ebeeaa5acfe1eeea5a9e9845b152d']['compare'] = () => (a => b => (a > b ? MORE : (__eq__(a, b) ? EQUAL : LESS)));
Comparable['Float_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Comparable['Float_5b7ebeeaa5acfe1eeea5a9e9845b152d']['compare'] = () => (a => b => (a > b ? MORE : (__eq__(a, b) ? EQUAL : LESS)));
Show['Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Show['Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d']['show'] = () => Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect();
Show['Byte_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Show['Byte_5b7ebeeaa5acfe1eeea5a9e9845b152d']['show'] = () => Inspect.Byte_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect();
Show['Float_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Show['Float_5b7ebeeaa5acfe1eeea5a9e9845b152d']['show'] = () => Inspect.Float_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect();
Scan['Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Scan['Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d']['scan'] = () => scanInteger;
Scan['Float_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Scan['Float_5b7ebeeaa5acfe1eeea5a9e9845b152d']['scan'] = () => scanFloat;
Scan['Byte_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Scan['Byte_5b7ebeeaa5acfe1eeea5a9e9845b152d']['scan'] = () => scanByte;
let scanInteger = (str =>  {
  const n = parseInt(str)
  return isNaN(n) ? Nothing : Just(n)
} );
let scanFloat = (str =>  {
  const n = parseFloat(str)
  return isNaN(n) ? Nothing : Just(n)
} );
let scanByte = (str =>  {
  const n = parseInt(str)
  return isNaN(n) ? Nothing : Just(n)
} );
export let integerToFloat = (a =>  a );
export let integerToByte = (a =>  Uint8Array.from([a])[0] );
export let byteToFloat = (a =>  a );
export let byteToInteger = (a =>  a );
export let floatToInteger =  Math.trunc ;
export let floatToByte = (a =>  Uint8Array.from([Math.trunc(a)])[0] );
export let formatDecimal = (a => x => {
  const n = x.toFixed(a)
  return isNaN(n) ? "0" : n
});
export default { integerToFloat, integerToByte, byteToFloat, byteToInteger, floatToInteger, floatToByte, formatDecimal };
