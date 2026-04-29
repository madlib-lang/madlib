// file: /Users/arnaudboeglin/Code/madlib/prelude/__internal__/IO.mad


import { _be211_show__7 } from "./Maybe.mjs";
import { _10be0_show__8 } from "./__BUILTINS__.mjs";







import prelude_readline from "readline";
export function _df1ab_putLine__9(a) {
  return (a => { console.log(a) })(a);
}
export function _df1ab_log__6(__P__2) {
  return (__P__2 => _df1ab_putLine__9(_be211_show__7(__P__2)))(__P__2);
}
export function _df1ab_log__12(__P__2) {
  return (__P__2 => _df1ab_putLine__9(_10be0_show__8(__P__2)))(__P__2);
}
export function __moduleInit_df1ab() {}


export default {_df1ab_putLine__9,_df1ab_log__6,_df1ab_log__12};