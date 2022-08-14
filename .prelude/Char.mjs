// file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/Char.mad
import {} from "./../__internals__.mjs"
import { MORE, LESS, EQUAL } from "./Compare.mjs";
import {  } from "./Show.mjs";
import String from "./String.mjs";

Comparable['Char_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Comparable['Char_5b7ebeeaa5acfe1eeea5a9e9845b152d']['compare'] = () => (a => b =>  a > b ? MORE : a === b ? EQUAL : LESS );
Show['Char_5b7ebeeaa5acfe1eeea5a9e9845b152d'] = {};
Show['Char_5b7ebeeaa5acfe1eeea5a9e9845b152d']['show'] = () => (x =>  x );
export let isDigit = (s => __eq__(s, __String.fromCharCode(48)) || __eq__(s, __String.fromCharCode(49)) || __eq__(s, __String.fromCharCode(50)) || __eq__(s, __String.fromCharCode(51)) || __eq__(s, __String.fromCharCode(52)) || __eq__(s, __String.fromCharCode(53)) || __eq__(s, __String.fromCharCode(54)) || __eq__(s, __String.fromCharCode(55)) || __eq__(s, __String.fromCharCode(56)) || __eq__(s, __String.fromCharCode(57)));
export let isLetter = (c => (_P_ => String.match(`[a-zA-Z]+`)(Show.Char_5b7ebeeaa5acfe1eeea5a9e9845b152d.show()(_P_)))(c));
export let toLower = (c =>  c.toLowerCase() );
export let toUpper = (c =>  c.toUpperCase() );
export default { isDigit, isLetter, toLower, toUpper };
