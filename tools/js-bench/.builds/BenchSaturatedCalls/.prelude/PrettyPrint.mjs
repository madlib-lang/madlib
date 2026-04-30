// file: /Users/arnaudboeglin/Code/madlib/prelude/__internal__/PrettyPrint.mad


export var _2133b_EmptyDoc = ({__constructor: "EmptyDoc",__a: 0});
export var _2133b_CharDoc = (a =>
({__constructor: "CharDoc",__a: 1,_0: a}));
export var _2133b_TextDoc = (a =>
b =>
({__constructor: "TextDoc",__a: 2,_0: a,_1: b}));
export var _2133b_LineDoc = (a =>
({__constructor: "LineDoc",__a: 1,_0: a}));
export var _2133b_CatDoc = (a =>
b =>
({__constructor: "CatDoc",__a: 2,_0: a,_1: b}));
export var _2133b_NestDoc = (a =>
b =>
({__constructor: "NestDoc",__a: 2,_0: a,_1: b}));
export var _2133b_UnionDoc = (a =>
b =>
({__constructor: "UnionDoc",__a: 2,_0: a,_1: b}));
export var _2133b_ColumnDoc = (a =>
({__constructor: "ColumnDoc",__a: 1,_0: a}));
export var _2133b_NestingDoc = (a =>
({__constructor: "NestingDoc",__a: 1,_0: a}));

export var _2133b_SEmpty = ({__constructor: "SEmpty",__a: 0});
export var _2133b_SChar = (a =>
b =>
({__constructor: "SChar",__a: 2,_0: a,_1: b}));
export var _2133b_SText = (a =>
b =>
c =>
({__constructor: "SText",__a: 3,_0: a,_1: b,_2: c}));
export var _2133b_SLine = (a =>
b =>
({__constructor: "SLine",__a: 2,_0: a,_1: b}));



export function __moduleInit_2133b() {}


export default {
  _2133b_EmptyDoc,
  _2133b_CharDoc,
  _2133b_TextDoc,
  _2133b_LineDoc,
  _2133b_CatDoc,
  _2133b_NestDoc,
  _2133b_UnionDoc,
  _2133b_ColumnDoc,
  _2133b_NestingDoc,
  _2133b_SEmpty,
  _2133b_SChar,
  _2133b_SText,
  _2133b_SLine
};