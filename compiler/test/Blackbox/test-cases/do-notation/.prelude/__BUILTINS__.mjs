// file: /Users/arnaudboeglin/Code/madlib/prelude/__internal__/__BUILTINS__.mad


export var _10be0_DictRBBlack = ({__constructor: "DictRBBlack",__args: []});
export var _10be0_DictRBRed = ({__constructor: "DictRBRed",__args: []});

export var _10be0_DictRBEmpty = ({__constructor: "DictRBEmpty",__args: []});
export var _10be0_DictRBNode = (a =>
b =>
c =>
d =>
e =>
({__constructor: "DictRBNode",__args: [a,b,c,d,e]}));

export var _10be0_LT = ({__constructor: "LT",__args: []});
export var _10be0_EQ = ({__constructor: "EQ",__args: []});
export var _10be0_GT = ({__constructor: "GT",__args: []});



export function _10be0_show__8(n) {
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
  _10be0_show__8,
  _10be0_DictRBBlack,
  _10be0_DictRBRed,
  _10be0_DictRBEmpty,
  _10be0_DictRBNode,
  _10be0_LT,
  _10be0_EQ,
  _10be0_GT
};