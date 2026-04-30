// file: /Users/arnaudboeglin/Code/madlib/prelude/__internal__/__BUILTINS__.mad


export var _10be0_DictRBBlack = ({__constructor: "DictRBBlack",__a: 0});
export var _10be0_DictRBRed = ({__constructor: "DictRBRed",__a: 0});

export var _10be0_DictRBEmpty = ({__constructor: "DictRBEmpty",__a: 0});
export var _10be0_DictRBNode = (a =>
b =>
c =>
d =>
e =>
({__constructor: "DictRBNode",__a: 5,_0: a,_1: b,_2: c,_3: d,_4: e}));

export var _10be0_LT = ({__constructor: "LT",__a: 0});
export var _10be0_EQ = ({__constructor: "EQ",__a: 0});
export var _10be0_GT = ({__constructor: "GT",__a: 0});



export function _10be0_show__5(n) {
  return (n =>  "" + n )(n);
}
function escapeChar(c) {
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
export function __moduleInit_10be0() {}


export default {
  _10be0_show__5,
  _10be0_DictRBBlack,
  _10be0_DictRBRed,
  _10be0_DictRBEmpty,
  _10be0_DictRBNode,
  _10be0_LT,
  _10be0_EQ,
  _10be0_GT
};