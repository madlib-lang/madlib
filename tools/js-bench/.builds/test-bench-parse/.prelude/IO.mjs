// file: /Users/arnaudboeglin/Code/madlib/prelude/__internal__/IO.mad


import { _10be0_show__20 } from "./__BUILTINS__.mjs";







import prelude_readline from "readline";
export function _df1ab_putLine__21(a) {
  return (a => { console.log(a) })(a);
}
export function _df1ab_log__19(__P__2) {
  return (__P__2 => _df1ab_putLine__21(_10be0_show__20(__P__2)))(__P__2);
}
export function __moduleInit_df1ab() {}


export default {_df1ab_putLine__21,_df1ab_log__19};