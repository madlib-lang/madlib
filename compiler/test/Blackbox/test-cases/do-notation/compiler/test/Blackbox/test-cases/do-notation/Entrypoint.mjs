// file: /Users/arnaudboeglin/Code/madlib/compiler/test/Blackbox/test-cases/do-notation/Entrypoint.mad


import {} from "./../../../../../__internals__.mjs";


import { __moduleInit_10be0 } from "./../../../../../.prelude/__BUILTINS__.mjs";
import { __moduleInit_65513 } from "./../../../../../.prelude/Functor.mjs";
import { __moduleInit_5fb1f } from "./../../../../../.prelude/Applicative.mjs";
import { __moduleInit_450df } from "./../../../../../.prelude/Alternative.mjs";
import { __moduleInit_6c1bf } from "./../../../../../.prelude/Bifunctor.mjs";
import { __moduleInit_6fb04 } from "./../../../../../.prelude/Function.mjs";
import { __moduleInit_360b0 } from "./../../../../../.prelude/Monad.mjs";
import { __moduleInit_be211 } from "./../../../../../.prelude/Maybe.mjs";
import { __moduleInit_05e0f } from "./../../../../../.prelude/Scan.mjs";
import { __moduleInit_c0b1b } from "./../../../../../.prelude/Integer.mjs";
import { __moduleInit_57ec4 } from "./../../../../../.prelude/Compare.mjs";
import { __moduleInit_e6cdb } from "./../../../../../.prelude/List.mjs";
import { __moduleInit_ba969 } from "./../../../../../.prelude/MonadRec.mjs";
import { __moduleInit_a9179 } from "./../../../../../.prelude/Tuple.mjs";
import { __moduleInit_302cb } from "./../../../../../.prelude/Wish.mjs";
import { __moduleInit_13984 } from "./../../../../../.prelude/__IOError__.mjs";
import { __moduleInit_b6337 } from "./../../../../../.prelude/Either.mjs";
import { __moduleInit_64fbe } from "./../../../../../.prelude/String.mjs";
import { __moduleInit_a22c7 } from "./../../../../../.prelude/Char.mjs";
import { __moduleInit_0c6b8 } from "./../../../../../.prelude/Control.mjs";
import { __moduleInit_7d446 } from "./../../../../../.prelude/Parse.mjs";
import { __moduleInit_2bcb8 } from "./../../../../../.prelude/Dictionary.mjs";
import { __moduleInit_5a87a } from "./../../../../../.prelude/Math.mjs";
import { __moduleInit_2133b } from "./../../../../../.prelude/PrettyPrint.mjs";
import { __moduleInit_400fc } from "./../../../../../.prelude/Terminal.mjs";
import { __moduleInit_05f27 } from "./../../../../../.prelude/Show.mjs";
import { __moduleInit_df1ab } from "./../../../../../.prelude/IO.mjs";
import { _df1ab_log__12, _df1ab_log__6 } from "./../../../../../.prelude/IO.mjs";
import { _be211_Just, _be211_Nothing, _be211_chain__5, _be211_fromMaybe__11, _be211_of__3, _be211_chain__5$$, _be211_fromMaybe__11$$, _be211_of__3$$ } from "./../../../../../.prelude/Maybe.mjs";


let _4ed41_safeLookup__2$$ = ((i, list) =>
{
  let $_result_;
  let $_continue_ = true;
  let $$i = i;
  let $$list = list;
  while($_continue_) {
    let $i = $$i;
    let $list = $$list;
    $_continue_ = false;
    ((__x__) => {
      if (__x__ !== null && true && true) {
        let { v: x, n: rest } = __x__;
        (__eq__($i, 0) ? ($_result_ = ({
          __constructor: "Just",
          __args: [
            x
          ]
        })) : ($$i = ($i - 1), $$list = rest, $_continue_ = true));
      } else if (__x__ === null) {
        ($_result_ = ({__constructor: "Nothing",__args: []}));
      } else {
        console.log('non exhaustive patterns for value: ', __x__.toString());
        console.trace();
        throw 'non exhaustive patterns!';
      }
    })($list)
  }
  return $_result_;
});
function _4ed41_safeLookup__2(i) {
  return (list =>
  _4ed41_safeLookup__2$$(i, list));
}
function _4ed41_compute__1(list) {
  return (list =>
  (() => {
    return _be211_chain__5$$((x =>
    _be211_chain__5$$((y =>
    _be211_of__3()
    (
      (x + y)
    )), _4ed41_safeLookup__2$$(1, list))), _4ed41_safeLookup__2$$(0, list));
  })())
  (
    list
  );
}
var _4ed41_computeNested__10;
function _4ed41_main(_) {
  return (_ =>
  {
    _df1ab_log__6(_4ed41_compute__1(({v: 10,n: {v: 20,n: {v: 30,n: null}}})));
    _df1ab_log__6(_4ed41_compute__1(({v: 10,n: null})));
    _df1ab_log__6(_4ed41_compute__1((null)));
    _df1ab_log__6(_4ed41_computeNested__10);
    let result = (() => {
      return _be211_chain__5$$((x =>
      _be211_chain__5$$((y => _be211_of__3()((x + y))), ({
        __constructor: "Just",
        __args: [
          20
        ]
      }))), ({__constructor: "Just",__args: [10]}));
    })();
    _df1ab_log__6(result);
    let failed = (() => {
      return _be211_chain__5$$((x =>
      _be211_chain__5$$((_ => _be211_of__3()(x)), ({
        __constructor: "Nothing",
        __args: [

        ]
      }))), ({__constructor: "Just",__args: [5]}));
    })();
    _df1ab_log__6(failed);
    _df1ab_log__12
    (
      _be211_fromMaybe__11$$(0, _4ed41_compute__1
      (
        ({v: 10,n: {v: 20,n: {v: 30,n: null}}})
      ))
    );
    _df1ab_log__12(_be211_fromMaybe__11$$((-1), _4ed41_compute__1((null))));
    return undefined;
  })
  (
    _
  );
}
export function __moduleInit_4ed41() {
  _4ed41_computeNested__10 = (() => {
    return _be211_chain__5$$((a =>
    _be211_chain__5$$((b =>
    _be211_chain__5$$((c => _be211_of__3()((c * 2))), ({
      __constructor: "Just",
      __args: [
        (a + b)
      ]
    }))), ({__constructor: "Just",__args: [5]}))), ({
      __constructor: "Just",
      __args: [
        100
      ]
    }));
  })()
}


__moduleInit_10be0();
__moduleInit_65513();
__moduleInit_5fb1f();
__moduleInit_450df();
__moduleInit_6c1bf();
__moduleInit_6fb04();
__moduleInit_360b0();
__moduleInit_be211();
__moduleInit_05e0f();
__moduleInit_c0b1b();
__moduleInit_57ec4();
__moduleInit_e6cdb();
__moduleInit_ba969();
__moduleInit_a9179();
__moduleInit_302cb();
__moduleInit_13984();
__moduleInit_b6337();
__moduleInit_64fbe();
__moduleInit_a22c7();
__moduleInit_0c6b8();
__moduleInit_7d446();
__moduleInit_2bcb8();
__moduleInit_5a87a();
__moduleInit_2133b();
__moduleInit_400fc();
__moduleInit_05f27();
__moduleInit_df1ab();
__moduleInit_4ed41();
export default {};
const __makeArgs = () => {
  let list = {}
  let start = list
  Object.keys(process.argv.slice(0)).forEach((key) => {
    list = list.n = { v: process.argv[key], n: null }
  }, {})
  return {
    n: start.n.n.n,
    v: start.n.n.v
  }
}
_4ed41_main(__makeArgs())
