// file: /opt/hostedtoolcache/node/14.20.1/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/node_modules/.bin/prelude/__internal__/JsonValue.mad
import {} from "./../__internals__.mjs"
export let JsonString = (a => ({ __constructor: "JsonString", __args: [ a ] }));
export let JsonInteger = (a => ({ __constructor: "JsonInteger", __args: [ a ] }));
export let JsonFloat = (a => ({ __constructor: "JsonFloat", __args: [ a ] }));
export let JsonBoolean = (a => ({ __constructor: "JsonBoolean", __args: [ a ] }));
export let JsonNull = ({ __constructor: "JsonNull", __args: [  ] });
export let JsonObject = (a => ({ __constructor: "JsonObject", __args: [ a ] }));
export let JsonArray = (a => ({ __constructor: "JsonArray", __args: [ a ] }));
Inspect['JsonValue_42310d802c161b1897bd964d310db165'] = {};
Inspect['JsonValue_42310d802c161b1897bd964d310db165']['inspect'] = () => (__$a__ => ((__x__) => {
  if (__x__.__constructor === "JsonString" && true) {
    let a0 = __x__.__args[0];
    return `JsonString(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
  }
  else if (__x__.__constructor === "JsonInteger" && true) {
    let a0 = __x__.__args[0];
    return `JsonInteger(` + Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
  }
  else if (__x__.__constructor === "JsonFloat" && true) {
    let a0 = __x__.__args[0];
    return `JsonFloat(` + Inspect.Float_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
  }
  else if (__x__.__constructor === "JsonBoolean" && true) {
    let a0 = __x__.__args[0];
    return `JsonBoolean(` + Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
  }
  else if (__x__.__constructor === "JsonNull") {
    return `JsonNull`;
  }
  else if (__x__.__constructor === "JsonObject" && true) {
    let a0 = __x__.__args[0];
    return `JsonObject(` + Inspect.Dictionary_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(Inspect.JsonValue_42310d802c161b1897bd964d310db165)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(a0) + `)`;
  }
  else if (__x__.__constructor === "JsonArray" && true) {
    let a0 = __x__.__args[0];
    return `JsonArray(` + Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(Inspect.JsonValue_42310d802c161b1897bd964d310db165)(a0) + `)`;
  }
  else if (true) {
    return `Unknown`;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__$a__));
export default { JsonString, JsonInteger, JsonFloat, JsonBoolean, JsonNull, JsonObject, JsonArray };
