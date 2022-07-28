// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Parser/Documentation.mad
import {} from "./../../../../__internals__.mjs"
import Json from "./../../../../.prelude/JsonParser.mjs";
import { Just, Nothing } from "./../../../../.prelude/Maybe.mjs";

export let BothTargets = (a => b => ({ __constructor: "BothTargets", __args: [ a, b ] }));
export let JSTarget = (a => ({ __constructor: "JSTarget", __args: [ a ] }));
export let LLVMTarget = (a => ({ __constructor: "LLVMTarget", __args: [ a ] }));
Inspect['Record_constraintsf_0_declarationf_1_descriptionf_2_examplef_3_sincef_4'] = {};
Inspect['Record_constraintsf_0_declarationf_1_descriptionf_2_examplef_3_sincef_4']['inspect'] = () => (Inspect_e914) => (Inspect_x907) => (Inspect_q900) => (Inspect_j893) => (Inspect_c886) => (__$a__ => `{ ` + `constraints: ` + Inspect_c886.inspect()(__$a__.constraints) + `, ` + `declaration: ` + Inspect_j893.inspect()(__$a__.declaration) + `, ` + `description: ` + Inspect_q900.inspect()(__$a__.description) + `, ` + `example: ` + Inspect_x907.inspect()(__$a__.example) + `, ` + `since: ` + Inspect_e914.inspect()(__$a__.since) + ` }`);
Inspect['Record_constraintsf_0_descriptionf_1_examplef_2_methodsf_3_namef_4_sincef_5_varsf_6'] = {};
Inspect['Record_constraintsf_0_descriptionf_1_examplef_2_methodsf_3_namef_4_sincef_5_varsf_6']['inspect'] = () => (Inspect_i970) => (Inspect_b963) => (Inspect_u956) => (Inspect_n949) => (Inspect_g942) => (Inspect_z935) => (Inspect_s928) => (__$a__ => `{ ` + `constraints: ` + Inspect_s928.inspect()(__$a__.constraints) + `, ` + `description: ` + Inspect_z935.inspect()(__$a__.description) + `, ` + `example: ` + Inspect_g942.inspect()(__$a__.example) + `, ` + `methods: ` + Inspect_n949.inspect()(__$a__.methods) + `, ` + `name: ` + Inspect_u956.inspect()(__$a__.name) + `, ` + `since: ` + Inspect_b963.inspect()(__$a__.since) + `, ` + `vars: ` + Inspect_i970.inspect()(__$a__.vars) + ` }`);
Inspect['Record_aliasedTypef_0_descriptionf_1_examplef_2_namef_3_paramsf_4_sincef_5'] = {};
Inspect['Record_aliasedTypef_0_descriptionf_1_examplef_2_namef_3_paramsf_4_sincef_5']['inspect'] = () => (Inspect_h1021) => (Inspect_a1014) => (Inspect_t1007) => (Inspect_m1000) => (Inspect_f993) => (Inspect_y986) => (__$a__ => `{ ` + `aliasedType: ` + Inspect_y986.inspect()(__$a__.aliasedType) + `, ` + `description: ` + Inspect_f993.inspect()(__$a__.description) + `, ` + `example: ` + Inspect_m1000.inspect()(__$a__.example) + `, ` + `name: ` + Inspect_t1007.inspect()(__$a__.name) + `, ` + `params: ` + Inspect_a1014.inspect()(__$a__.params) + `, ` + `since: ` + Inspect_h1021.inspect()(__$a__.since) + ` }`);
Inspect['Record_constructorsf_0_descriptionf_1_examplef_2_namef_3_paramsf_4_sincef_5'] = {};
Inspect['Record_constructorsf_0_descriptionf_1_examplef_2_namef_3_paramsf_4_sincef_5']['inspect'] = () => (Inspect_f1071) => (Inspect_y1064) => (Inspect_r1057) => (Inspect_k1050) => (Inspect_d1043) => (Inspect_w1036) => (__$a__ => `{ ` + `constructors: ` + Inspect_w1036.inspect()(__$a__.constructors) + `, ` + `description: ` + Inspect_d1043.inspect()(__$a__.description) + `, ` + `example: ` + Inspect_k1050.inspect()(__$a__.example) + `, ` + `name: ` + Inspect_r1057.inspect()(__$a__.name) + `, ` + `params: ` + Inspect_y1064.inspect()(__$a__.params) + `, ` + `since: ` + Inspect_f1071.inspect()(__$a__.since) + ` }`);
Inspect['Record_descriptionf_0_examplef_1_namef_2_sincef_3_typingf_4'] = {};
Inspect['Record_descriptionf_0_examplef_1_namef_2_sincef_3_typingf_4']['inspect'] = () => (Inspect_w1114) => (Inspect_p1107) => (Inspect_i1100) => (Inspect_b1093) => (Inspect_u1086) => (__$a__ => `{ ` + `description: ` + Inspect_u1086.inspect()(__$a__.description) + `, ` + `example: ` + Inspect_b1093.inspect()(__$a__.example) + `, ` + `name: ` + Inspect_i1100.inspect()(__$a__.name) + `, ` + `since: ` + Inspect_p1107.inspect()(__$a__.since) + `, ` + `typing: ` + Inspect_w1114.inspect()(__$a__.typing) + ` }`);
Inspect['Targeted_059980c9d93cf2de3fe268b3d9d26775'] = {};
Inspect['Targeted_059980c9d93cf2de3fe268b3d9d26775']['inspect'] = () => (Inspect_i1126) => (__$a__ => ((__x__) => {
  if (__x__.__constructor === "BothTargets" && true && true) {
    let a0 = __x__.__args[0];
    let a1 = __x__.__args[1];
    return `BothTargets(` + Inspect_i1126.inspect()(a0) + `, ` + Inspect_i1126.inspect()(a1) + `)`;
  }
  else if (__x__.__constructor === "JSTarget" && true) {
    let a0 = __x__.__args[0];
    return `JSTarget(` + Inspect_i1126.inspect()(a0) + `)`;
  }
  else if (__x__.__constructor === "LLVMTarget" && true) {
    let a0 = __x__.__args[0];
    return `LLVMTarget(` + Inspect_i1126.inspect()(a0) + `)`;
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
Inspect['Record_aliasesf_0_descriptionf_1_expressionsf_2_instancesf_3_interfacesf_4_namef_5_pathf_6_typeDeclarationsf_7'] = {};
Inspect['Record_aliasesf_0_descriptionf_1_expressionsf_2_instancesf_3_interfacesf_4_namef_5_pathf_6_typeDeclarationsf_7']['inspect'] = () => (Inspect_e1200) => (Inspect_x1193) => (Inspect_q1186) => (Inspect_j1179) => (Inspect_c1172) => (Inspect_v1165) => (Inspect_o1158) => (Inspect_h1151) => (__$a__ => `{ ` + `aliases: ` + Inspect_h1151.inspect()(__$a__.aliases) + `, ` + `description: ` + Inspect_o1158.inspect()(__$a__.description) + `, ` + `expressions: ` + Inspect_v1165.inspect()(__$a__.expressions) + `, ` + `instances: ` + Inspect_c1172.inspect()(__$a__.instances) + `, ` + `interfaces: ` + Inspect_j1179.inspect()(__$a__.interfaces) + `, ` + `name: ` + Inspect_q1186.inspect()(__$a__.name) + `, ` + `path: ` + Inspect_x1193.inspect()(__$a__.path) + `, ` + `typeDeclarations: ` + Inspect_e1200.inspect()(__$a__.typeDeclarations) + ` }`);
export let getName = (targeted => ((__x__) => {
  if (__x__.__constructor === "BothTargets" && true && true) {
    let a = __x__.__args[0];
    return a.name;
  }
  else if (__x__.__constructor === "JSTarget" && true) {
    let a = __x__.__args[0];
    return a.name;
  }
  else if (__x__.__constructor === "LLVMTarget" && true) {
    let a = __x__.__args[0];
    return a.name;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(targeted));
export let hasJSTarget = (targeted => ((__x__) => {
  if (__x__.__constructor === "BothTargets" && true && true) {
    return true;
  }
  else if (__x__.__constructor === "JSTarget" && true) {
    return true;
  }
  else if (true) {
    return false;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(targeted));
export let forJS = (targeted => ((__x__) => {
  if (__x__.__constructor === "BothTargets" && true && true) {
    let a = __x__.__args[0];
    return a;
  }
  else if (__x__.__constructor === "JSTarget" && true) {
    let a = __x__.__args[0];
    return a;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(targeted));
let makeInstance = (declaration => constraints => description => since => example => ({ declaration: declaration, constraints: constraints, description: description, since: since, example: example }));
let makeInterface = (name => vars => constraints => methods => description => since => example => ({ name: name, vars: vars, constraints: constraints, methods: methods, description: description, since: since, example: example }));
let makeAlias = (name => params => aliasedType => description => since => example => ({ name: name, params: params, aliasedType: aliasedType, description: description, since: since, example: example }));
let makeType = (name => params => constructors => description => since => example => ({ name: name, params: params, constructors: constructors, description: description, since: since, example: example }));
let makeExpression = (name => description => typing => since => example => ({ name: name, description: description, typing: typing, since: since, example: example }));
let makeTargeted = (maybeJS => maybeLLVM => ((__x__) => {
  if (__x__.length === 2 && __x__[0].__constructor === "Just" && true && __x__[1].__constructor === "Just" && true) {
    let [{ __args: [js]},{ __args: [llvm]}] = __x__;
    return BothTargets(js)(llvm);
  }
  else if (__x__.length === 2 && __x__[0].__constructor === "Just" && true && __x__[1].__constructor === "Nothing") {
    let [{ __args: [js]},{ __args: []}] = __x__;
    return JSTarget(js);
  }
  else if (__x__.length === 2 && __x__[0].__constructor === "Nothing" && __x__[1].__constructor === "Just" && true) {
    let [{ __args: []},{ __args: [llvm]}] = __x__;
    return LLVMTarget(llvm);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(([maybeJS, maybeLLVM])));
let makeModule = (path => name => description => expressions => typeDeclarations => aliases => interfaces => instances => ({ path: path, name: name, description: description, expressions: expressions, typeDeclarations: typeDeclarations, aliases: aliases, interfaces: interfaces, instances: instances }));
let expressionParser = Json.map5(makeExpression)(Json.field(`name`)(Json.string))(Json.field(`description`)(Json.string))(Json.field(`type`)(Json.string))(Json.field(`since`)(Json.string))(Json.field(`example`)(Json.string));
export let parser = Json.field(`modules`)(Json.list(Json.map8(makeModule)(Json.field(`path`)(Json.string))(Json.field(`moduleName`)(Json.string))(Json.field(`description`)(Json.string))(Json.field(`expressions`)(Json.list(Json.map2(makeTargeted)(Json.maybe(Json.field(`js`)(expressionParser)))(Json.maybe(Json.field(`llvm`)(expressionParser))))))(Json.field(`typeDeclarations`)(Json.list(Json.map6(makeType)(Json.path(({ v: `js`, n: { v: `name`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `params`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `constructors`, n: null } }))(Json.list(Json.string)))(Json.path(({ v: `js`, n: { v: `description`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `since`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `example`, n: null } }))(Json.string)))))(Json.field(`aliases`)(Json.list(Json.map6(makeAlias)(Json.path(({ v: `js`, n: { v: `name`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `params`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `aliasedType`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `description`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `since`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `example`, n: null } }))(Json.string)))))(Json.field(`interfaces`)(Json.list(Json.map7(makeInterface)(Json.path(({ v: `js`, n: { v: `name`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `vars`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `constraints`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `methods`, n: null } }))(Json.list(Json.string)))(Json.path(({ v: `js`, n: { v: `description`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `since`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `example`, n: null } }))(Json.string)))))(Json.field(`instances`)(Json.list(Json.map5(makeInstance)(Json.path(({ v: `js`, n: { v: `declaration`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `constraints`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `description`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `since`, n: null } }))(Json.string))(Json.path(({ v: `js`, n: { v: `example`, n: null } }))(Json.string)))))));
export default { getName, hasJSTarget, forJS, parser, BothTargets, JSTarget, LLVMTarget };
