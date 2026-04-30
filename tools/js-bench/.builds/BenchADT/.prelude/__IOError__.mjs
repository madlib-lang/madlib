// file: /Users/arnaudboeglin/Code/madlib/prelude/__internal__/__IOError__.mad


export var _13984_AddressAlreadyInUse = ({
  __constructor: "AddressAlreadyInUse",
  __a: 0
});
export var _13984_ArgumentListToLong = ({
  __constructor: "ArgumentListToLong",
  __a: 0
});
export var _13984_PermissionDenied = ({
  __constructor: "PermissionDenied",
  __a: 0
});
export var _13984_UnknownError = ({__constructor: "UnknownError",__a: 0});
export var _13984_GeneralError = (a =>
({__constructor: "GeneralError",__a: 1,_0: a}));



export function __moduleInit_13984() {}


export default {
  _13984_AddressAlreadyInUse,
  _13984_ArgumentListToLong,
  _13984_PermissionDenied,
  _13984_UnknownError,
  _13984_GeneralError
};