// file: /Users/arnaudboeglin/Code/madlib/prelude/__internal__/StringBuilder.mad


export var _b3cd6_StringBuilder = ({__constructor: "StringBuilder",__a: 0});



export function _b3cd6_toString__7(sb) {
  return (sb =>
   (() => {
    const s = sb.parts.join('')
    sb.parts = [s]
    return s
  })() )
  (
    sb
  );
}
export function _b3cd6_new__3(_) {
  return (_ =>  ({ parts: [], len: 0 }) )(_);
}
let _b3cd6_append__4$$ = ((s, sb) =>
 (sb.parts.push(s), sb.len += s.length, sb) );
export function _b3cd6_append__4(s) {
  return (sb =>
  _b3cd6_append__4$$(s, sb));
}
export function __moduleInit_b3cd6() {}


export default {
  _b3cd6_toString__7,
  _b3cd6_new__3,
  _b3cd6_append__4,
  _b3cd6_StringBuilder
};