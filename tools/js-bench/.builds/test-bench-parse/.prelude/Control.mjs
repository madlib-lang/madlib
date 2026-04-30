// file: /Users/arnaudboeglin/Code/madlib/prelude/__internal__/Control.mad


import { _be211_Just, _be211_Nothing } from "./Maybe.mjs";


let _0c6b8_maybeLoop__36$$ = ((start, evaluate) =>
{
  let $_result_;
  let $_continue_ = true;
  let $$start = start;
  let $$evaluate = evaluate;
  while($_continue_) {
    let $start = $$start;
    let $evaluate = $$evaluate;
    $_continue_ = false;
    ((__x__) => {
      switch (__x__.__constructor) {
        case "Just": {
          let x = __x__._0;
          ($$start = x, $$evaluate = $evaluate, $_continue_ = true);
          break;
        }
        case "Nothing": {
          ($_result_ = $start);
          break;
        }
        default: {
          console.log('non exhaustive patterns for value: ', __x__.toString());
          console.trace();
          throw 'non exhaustive patterns!';
        }
      }
    })($evaluate($start))
  }
  return $_result_;
});
export function _0c6b8_maybeLoop__36(start) {
  return (evaluate =>
  _0c6b8_maybeLoop__36$$(start, evaluate));
}
export function __moduleInit_0c6b8() {}


export default {_0c6b8_maybeLoop__36};