// file: /Users/arnaudboeglin/Code/madlib/prelude/__internal__/String.mad


import { _e6cdb_intersperse__40, _e6cdb_reduce__42 } from "./List.mjs";
import { _10be0_mappend__41 } from "./__BUILTINS__.mjs";


export function _64fbe_toList__25(str) {
  return (str =>
  {
    if (str.length === 0) {
     ; return null
    }

   ; let result = { v: null, n: null }
   ; let current = result
    str.split('').forEach(c => {
      current = current.n = { v: c, n: null }
    })
   ; return result.n
  })
  (
    str
  );
}
let _64fbe_join__39$$ = ((a, xs) =>
_e6cdb_reduce__42(_10be0_mappend__41())("")(_e6cdb_intersperse__40(a)(xs)));
export function _64fbe_join__39(a) {
  return (xs =>
  _64fbe_join__39$$(a, xs));
}
let _64fbe_byteStartsWith__24$$ = ((target, offset, s) =>
 s.startsWith(target, offset) );
export function _64fbe_byteStartsWith__24(target) {
  return (offset =>
  s =>
  _64fbe_byteStartsWith__24$$(target, offset, s));
}
export function _64fbe_byteLength__16(s) {
  return (s =>  s.length )(s);
}
let _64fbe_byteCharWidth__10$$ = ((offset, s) =>
 (() => {
  const code = s.charCodeAt(offset)
  return (code >= 0xD800 && code <= 0xDBFF) ? 2 : 1
})() );
export function _64fbe_byteCharWidth__10(offset) {
  return (s =>
  _64fbe_byteCharWidth__10$$(offset, s));
}
let _64fbe_byteCharAt__9$$ = ((offset, s) =>
 (() => {
  if (offset >= s.length) return {__constructor: "Nothing", __a: 0}
  const code = s.charCodeAt(offset)
  // High surrogate: decode the full codepoint from the pair
  if (code >= 0xD800 && code <= 0xDBFF) {
    const low = s.charCodeAt(offset + 1)
    return {__constructor: "Just", __a: 1, _0: String.fromCodePoint(((code - 0xD800) << 10) + (low - 0xDC00) + 0x10000)}
  }
  return {__constructor: "Just", __a: 1, _0: s[offset]}
})() );
export function _64fbe_byteCharAt__9(offset) {
  return (s =>
  _64fbe_byteCharAt__9$$(offset, s));
}
export function __moduleInit_64fbe() {}


export default {
  _64fbe_toList__25,
  _64fbe_join__39,
  _64fbe_byteStartsWith__24,
  _64fbe_byteLength__16,
  _64fbe_byteCharWidth__10,
  _64fbe_byteCharAt__9
};