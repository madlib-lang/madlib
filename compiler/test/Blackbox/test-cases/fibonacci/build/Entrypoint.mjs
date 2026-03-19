// file: /Users/arnaudboeglin/Code/madlib/compiler/test/Blackbox/test-cases/fibonacci/Entrypoint.mad
import {} from "./__internals__.mjs"
export let _10be0_DictRBBlack = ({ __constructor: "DictRBBlack", __args: [  ] });
export let _10be0_DictRBRed = ({ __constructor: "DictRBRed", __args: [  ] });
export let _10be0_DictRBEmpty = ({ __constructor: "DictRBEmpty", __args: [  ] });
export let _10be0_DictRBNode = (a => b => c => d => e => ({ __constructor: "DictRBNode", __args: [ a, b, c, d, e ] }));
export let _10be0_LT = ({ __constructor: "LT", __args: [  ] });
export let _10be0_EQ = ({ __constructor: "EQ", __args: [  ] });
export let _10be0_GT = ({ __constructor: "GT", __args: [  ] });
export let _6fb04_Loop = (a => ({ __constructor: "Loop", __args: [ a ] }));
export let _6fb04_Done = (a => ({ __constructor: "Done", __args: [ a ] }));
export let _be211_Just = (a => ({ __constructor: "Just", __args: [ a ] }));
export let _be211_Nothing = ({ __constructor: "Nothing", __args: [  ] });
let _302cb___TimerId__ = ({ __constructor: "__TimerId__", __args: [  ] });
export let _302cb_Wish = (a => ({ __constructor: "Wish", __args: [ a ] }));
export let _13984_AddressAlreadyInUse = ({ __constructor: "AddressAlreadyInUse", __args: [  ] });
export let _13984_ArgumentListToLong = ({ __constructor: "ArgumentListToLong", __args: [  ] });
export let _13984_PermissionDenied = ({ __constructor: "PermissionDenied", __args: [  ] });
export let _13984_UnknownError = ({ __constructor: "UnknownError", __args: [  ] });
export let _13984_GeneralError = (a => ({ __constructor: "GeneralError", __args: [ a ] }));
export let _b6337_Left = (a => ({ __constructor: "Left", __args: [ a ] }));
export let _b6337_Right = (a => ({ __constructor: "Right", __args: [ a ] }));
export let _7d446_Loc = (a => b => c => ({ __constructor: "Loc", __args: [ a, b, c ] }));
export let _7d446_Parser = (a => ({ __constructor: "Parser", __args: [ a ] }));
export let _7d446_Error = (a => ({ __constructor: "Error", __args: [ a ] }));
export let _7d446_Config = (a => ({ __constructor: "Config", __args: [ a ] }));
export let _2133b_EmptyDoc = ({ __constructor: "EmptyDoc", __args: [  ] });
export let _2133b_CharDoc = (a => ({ __constructor: "CharDoc", __args: [ a ] }));
export let _2133b_TextDoc = (a => b => ({ __constructor: "TextDoc", __args: [ a, b ] }));
export let _2133b_LineDoc = (a => ({ __constructor: "LineDoc", __args: [ a ] }));
export let _2133b_CatDoc = (a => b => ({ __constructor: "CatDoc", __args: [ a, b ] }));
export let _2133b_NestDoc = (a => b => ({ __constructor: "NestDoc", __args: [ a, b ] }));
export let _2133b_UnionDoc = (a => b => ({ __constructor: "UnionDoc", __args: [ a, b ] }));
export let _2133b_ColumnDoc = (a => ({ __constructor: "ColumnDoc", __args: [ a ] }));
export let _2133b_NestingDoc = (a => ({ __constructor: "NestingDoc", __args: [ a ] }));
export let _2133b_SEmpty = ({ __constructor: "SEmpty", __args: [  ] });
export let _2133b_SChar = (a => b => ({ __constructor: "SChar", __args: [ a, b ] }));
export let _2133b_SText = (a => b => c => ({ __constructor: "SText", __args: [ a, b, c ] }));
export let _2133b_SLine = (a => b => ({ __constructor: "SLine", __args: [ a, b ] }));
let _400fc_HandlerId = ({ __constructor: "HandlerId", __args: [  ] });
let _400fc_NormalMode = ({ __constructor: "NormalMode", __args: [  ] });
let _400fc_RawMode = ({ __constructor: "RawMode", __args: [  ] });
let _05f27_Constructor = (a => b => ({ __constructor: "Constructor", __args: [ a, b ] }));
let _05f27_Unit = ({ __constructor: "Unit", __args: [  ] });
let _05f27_Record = (a => ({ __constructor: "Record", __args: [ a ] }));
let _05f27_Integer = (a => ({ __constructor: "Integer", __args: [ a ] }));
let _05f27_Float = (a => ({ __constructor: "Float", __args: [ a ] }));
let _05f27_Boolean = (a => ({ __constructor: "Boolean", __args: [ a ] }));
let _05f27_Char = (a => ({ __constructor: "Char", __args: [ a ] }));
let _05f27_Str = (a => ({ __constructor: "Str", __args: [ a ] }));
let _05f27_DictionaryConstructor = (a => ({ __constructor: "DictionaryConstructor", __args: [ a ] }));
let _05f27_ListConstructor = (a => ({ __constructor: "ListConstructor", __args: [ a ] }));
let _05f27_TupleConstructor = (a => ({ __constructor: "TupleConstructor", __args: [ a ] }));
let _05f27_Byte = (a => ({ __constructor: "Byte", __args: [ a ] }));
let _05f27_ByteArray = (a => ({ __constructor: "ByteArray", __args: [ a ] }));

const escapeChar = (c) => {
  if (c === '\\') {
    return `\\\\`
  } else if (c === '"') {
    return `\\"`
  } else if (c === '\n') {
    return `\\n`
  } else if (c === '\t') {
    return `\\t`
  } else if (c === '\r') {
    return `\\r`
  } else {
    return c
  }
}
;
;
;
;
;
;
;
;
;

import prelude_terminal_readline from "readline"
;
 let readLineInterface = null ;
;
;
;
;
;

import prelude_readline from "readline"
;
export let _df1ab_putLine__4 = (a =>  { console.log(a) } );
let _a1426_fib__1 = (n => {
    let helper__0
helper__0 = (a => b => counter => {
    let $_result_;
    let $_continue_ = true;
    let $$a = a;
    let $$b = b;
    let $$counter = counter;

    while($_continue_) {
      let $a = $$a;
      let $b = $$b;
      let $counter = $$counter;

        $_continue_ = false;
        ($counter > 0 ? ($$a = $b, $$b = ($a + $b), $$counter = ($counter - 1), $_continue_ = true) : ($_result_ = $a))
    }
    return $_result_;
});
    return helper__0(0)(1)(n);
});
export let _10be0_show__3 = (n =>  "" + n );
export let _df1ab_log__2 = (__P__2 => _df1ab_putLine__4(_10be0_show__3(__P__2)));
let _a1426_main = (_ => {
    _df1ab_log__2(_a1426_fib__1(30));
    return ({ __constructor: "Unit", __args: [] });
});
export default { _df1ab_putLine__4, _10be0_show__3, _df1ab_log__2, _10be0_DictRBBlack, _10be0_DictRBRed, _10be0_DictRBEmpty, _10be0_DictRBNode, _10be0_LT, _10be0_EQ, _10be0_GT, _6fb04_Loop, _6fb04_Done, _be211_Just, _be211_Nothing, _302cb_Wish, _13984_AddressAlreadyInUse, _13984_ArgumentListToLong, _13984_PermissionDenied, _13984_UnknownError, _13984_GeneralError, _b6337_Left, _b6337_Right, _7d446_Loc, _7d446_Parser, _7d446_Error, _7d446_Config, _2133b_EmptyDoc, _2133b_CharDoc, _2133b_TextDoc, _2133b_LineDoc, _2133b_CatDoc, _2133b_NestDoc, _2133b_UnionDoc, _2133b_ColumnDoc, _2133b_NestingDoc, _2133b_SEmpty, _2133b_SChar, _2133b_SText, _2133b_SLine };
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
_a1426_main(__makeArgs())
