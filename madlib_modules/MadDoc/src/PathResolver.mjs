// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/PathResolver.mad
import {} from "./../../../__internals__.mjs"
import List from "./../../../.prelude/List.mjs";
import { Just, Nothing } from "./../../../.prelude/Maybe.mjs";
import { always, any, equals, ifElse } from "./../../../.prelude/Function.mjs";
import { drop, split, toLower, _$_length_$_ } from "./../../../.prelude/String.mjs";
import { isRootPathOf, canonicalizePath } from "./../../../.prelude/FilePath/Posix.mjs";
import {  } from "./../../../.prelude/Compare.mjs";
import {  } from "./State.mjs";
import { getName } from "./Parser/Documentation.mjs";

export let ModuleResult = (a => ({ __constructor: "ModuleResult", __args: [ a ] }));
export let ExpressionResult = (a => b => ({ __constructor: "ExpressionResult", __args: [ a, b ] }));
export let TypeResult = (a => b => ({ __constructor: "TypeResult", __args: [ a, b ] }));
export let AliasResult = (a => b => ({ __constructor: "AliasResult", __args: [ a, b ] }));
export let InterfaceResult = (a => b => ({ __constructor: "InterfaceResult", __args: [ a, b ] }));
export let InstanceResult = (a => b => ({ __constructor: "InstanceResult", __args: [ a, b ] }));
export let NotFound = ({ __constructor: "NotFound", __args: [  ] });
Inspect['PathResult_b34400a9a78f179ffe0f3c51f401cc20'] = {};
Inspect['PathResult_b34400a9a78f179ffe0f3c51f401cc20']['inspect'] = () => (__$a__ => ((__x__) => {
  if (__x__.__constructor === "ModuleResult" && true) {
    let a0 = __x__.__args[0];
    return `ModuleResult(` + Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(__apMtdDicts__(Inspect.Record_aliasesf_0059980c9d93cf2de3fe268b3d9d26775_descriptionf_1059980c9d93cf2de3fe268b3d9d26775_expressionsf_2059980c9d93cf2de3fe268b3d9d26775_instancesf_3059980c9d93cf2de3fe268b3d9d26775_interfacesf_4059980c9d93cf2de3fe268b3d9d26775_namef_5059980c9d93cf2de3fe268b3d9d26775_pathf_6059980c9d93cf2de3fe268b3d9d26775_typeDeclarationsf_7059980c9d93cf2de3fe268b3d9d26775, [__apMtdDicts__(Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d, [__apMtdDicts__(Inspect.Record_constructorsf_0059980c9d93cf2de3fe268b3d9d26775_descriptionf_1059980c9d93cf2de3fe268b3d9d26775_examplef_2059980c9d93cf2de3fe268b3d9d26775_namef_3059980c9d93cf2de3fe268b3d9d26775_paramsf_4059980c9d93cf2de3fe268b3d9d26775_sincef_5059980c9d93cf2de3fe268b3d9d26775, [Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, __apMtdDicts__(Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d, [Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d])])]), Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, __apMtdDicts__(Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d, [__apMtdDicts__(Inspect.Record_constraintsf_0059980c9d93cf2de3fe268b3d9d26775_descriptionf_1059980c9d93cf2de3fe268b3d9d26775_examplef_2059980c9d93cf2de3fe268b3d9d26775_methodsf_3059980c9d93cf2de3fe268b3d9d26775_namef_4059980c9d93cf2de3fe268b3d9d26775_sincef_5059980c9d93cf2de3fe268b3d9d26775_varsf_6059980c9d93cf2de3fe268b3d9d26775, [Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, __apMtdDicts__(Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d, [Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d]), Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d])]), __apMtdDicts__(Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d, [__apMtdDicts__(Inspect.Record_constraintsf_0059980c9d93cf2de3fe268b3d9d26775_declarationf_1059980c9d93cf2de3fe268b3d9d26775_descriptionf_2059980c9d93cf2de3fe268b3d9d26775_examplef_3059980c9d93cf2de3fe268b3d9d26775_sincef_4059980c9d93cf2de3fe268b3d9d26775, [Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d])]), __apMtdDicts__(Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d, [__apMtdDicts__(Inspect.Targeted_059980c9d93cf2de3fe268b3d9d26775, [__apMtdDicts__(Inspect.Record_descriptionf_0059980c9d93cf2de3fe268b3d9d26775_examplef_1059980c9d93cf2de3fe268b3d9d26775_namef_2059980c9d93cf2de3fe268b3d9d26775_sincef_3059980c9d93cf2de3fe268b3d9d26775_typingf_4059980c9d93cf2de3fe268b3d9d26775, [Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d])])]), Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, __apMtdDicts__(Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d, [__apMtdDicts__(Inspect.Record_aliasedTypef_0059980c9d93cf2de3fe268b3d9d26775_descriptionf_1059980c9d93cf2de3fe268b3d9d26775_examplef_2059980c9d93cf2de3fe268b3d9d26775_namef_3059980c9d93cf2de3fe268b3d9d26775_paramsf_4059980c9d93cf2de3fe268b3d9d26775_sincef_5059980c9d93cf2de3fe268b3d9d26775, [Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d])])]))(a0) + `)`;
  }
  else if (__x__.__constructor === "ExpressionResult" && true && true) {
    let a0 = __x__.__args[0];
    let a1 = __x__.__args[1];
    return `ExpressionResult(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `, ` + Inspect.Targeted_059980c9d93cf2de3fe268b3d9d26775.inspect()(__apMtdDicts__(Inspect.Record_descriptionf_0059980c9d93cf2de3fe268b3d9d26775_examplef_1059980c9d93cf2de3fe268b3d9d26775_namef_2059980c9d93cf2de3fe268b3d9d26775_sincef_3059980c9d93cf2de3fe268b3d9d26775_typingf_4059980c9d93cf2de3fe268b3d9d26775, [Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d, Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d]))(a1) + `)`;
  }
  else if (__x__.__constructor === "TypeResult" && true && true) {
    let a0 = __x__.__args[0];
    let a1 = __x__.__args[1];
    return `TypeResult(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `, ` + Inspect.Record_constructorsf_0059980c9d93cf2de3fe268b3d9d26775_descriptionf_1059980c9d93cf2de3fe268b3d9d26775_examplef_2059980c9d93cf2de3fe268b3d9d26775_namef_3059980c9d93cf2de3fe268b3d9d26775_paramsf_4059980c9d93cf2de3fe268b3d9d26775_sincef_5059980c9d93cf2de3fe268b3d9d26775.inspect()(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(__apMtdDicts__(Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d, [Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d]))(a1) + `)`;
  }
  else if (__x__.__constructor === "AliasResult" && true && true) {
    let a0 = __x__.__args[0];
    let a1 = __x__.__args[1];
    return `AliasResult(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `, ` + Inspect.Record_aliasedTypef_0059980c9d93cf2de3fe268b3d9d26775_descriptionf_1059980c9d93cf2de3fe268b3d9d26775_examplef_2059980c9d93cf2de3fe268b3d9d26775_namef_3059980c9d93cf2de3fe268b3d9d26775_paramsf_4059980c9d93cf2de3fe268b3d9d26775_sincef_5059980c9d93cf2de3fe268b3d9d26775.inspect()(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(a1) + `)`;
  }
  else if (__x__.__constructor === "InterfaceResult" && true && true) {
    let a0 = __x__.__args[0];
    let a1 = __x__.__args[1];
    return `InterfaceResult(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `, ` + Inspect.Record_constraintsf_0059980c9d93cf2de3fe268b3d9d26775_descriptionf_1059980c9d93cf2de3fe268b3d9d26775_examplef_2059980c9d93cf2de3fe268b3d9d26775_methodsf_3059980c9d93cf2de3fe268b3d9d26775_namef_4059980c9d93cf2de3fe268b3d9d26775_sincef_5059980c9d93cf2de3fe268b3d9d26775_varsf_6059980c9d93cf2de3fe268b3d9d26775.inspect()(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(__apMtdDicts__(Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d, [Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d]))(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(a1) + `)`;
  }
  else if (__x__.__constructor === "InstanceResult" && true && true) {
    let a0 = __x__.__args[0];
    let a1 = __x__.__args[1];
    return `InstanceResult(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `, ` + Inspect.Record_constraintsf_0059980c9d93cf2de3fe268b3d9d26775_declarationf_1059980c9d93cf2de3fe268b3d9d26775_descriptionf_2059980c9d93cf2de3fe268b3d9d26775_examplef_3059980c9d93cf2de3fe268b3d9d26775_sincef_4059980c9d93cf2de3fe268b3d9d26775.inspect()(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(a1) + `)`;
  }
  else if (__x__.__constructor === "NotFound") {
    return `NotFound`;
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
let filterByPath = (path => {
    let canPath = canonicalizePath(path);
    return List.filter((module => (_P_ => ifElse(isRootPathOf(drop(1)(toLower(canPath))))(always(!(List.isEmpty(module.expressions)) || !(List.isEmpty(module.typeDeclarations)) || !(List.isEmpty(module.aliases)) || !(List.isEmpty(module.interfaces)) || !(List.isEmpty(module.instances))))((_P_ => any(isRootPathOf(drop(1)(toLower(canPath))))(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((_P_ => toLower(Monoid.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.mconcat()(module.name + `/`)(_P_))))(always((__listCtorSpread__(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(getName)(module.expressions), __listCtorSpread__(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((__R__ => __R__.name))(module.typeDeclarations), __listCtorSpread__(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((__R__ => __R__.name))(module.aliases), __listCtorSpread__(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((__R__ => __R__.name))(module.interfaces), Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((__R__ => __R__.declaration))(module.instances)))))))(_P_)))))(toLower((__R__ => __R__.name)(_P_))))(module)));
});
export let getModulesToShow = (state => (_P_ => filterByPath(state.path)((__R__ => __R__.modules)(_P_)))(state));
let isItemView = (path => ifElse((_P_ => equals(1)(List._$_length_$_(_P_))))((_P_ => (__x__ => ((__x__) => {
  if (__x__.__constructor === "Just" && true) {
    let m = __x__.__args[0];
    return _$_length_$_(canonicalizePath(path)) > _$_length_$_(`/` + m.name);
  }
  else if (__x__.__constructor === "Nothing") {
    return false;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__))(List.first(_P_))))(always(false)));
let tryItemByKind = (constructor => retrieveName => items => path => module => (_P_ => (__x__ => ((__x__) => {
  if (__x__.__constructor === "Just" && true) {
    let found = __x__.__args[0];
    return constructor(module.name)(found);
  }
  else if (__x__.__constructor === "Nothing") {
    return NotFound;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__))(List.find((e => __eq__(Just(retrieveName(e)), List.last(split(`/`)(path)))))(_P_)))(items));
let _findItem = (finders => path => module => {
    let $_result_;
    let $_continue_ = true;
    let $$finders = finders;
    let $$path = path;
    let $$module = module;

    while($_continue_) {
      let $finders = $$finders;
      let $path = $$path;
      let $module = $$module;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ !== null && true && true) {
    let { v: _$_try_$_, n: others } = __x__;
    ((__x__) => {
  if (__x__.__constructor === "NotFound") {
    ($$finders = others, $$path = $path, $$module = $module, $_continue_ = true);
  }
  else if (true) {
    let found = __x__;
    ($_result_ = found);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(_$_try_$_($path)($module));
  }
  else if (__x__ === null) {
    ($_result_ = NotFound);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})($finders)
    }
    return $_result_;
});
let findItem = (path => module => _findItem(({ v: tryItemByKind(ExpressionResult)(getName)(module.expressions), n: { v: tryItemByKind(TypeResult)((__R__ => __R__.name))(module.typeDeclarations), n: { v: tryItemByKind(AliasResult)((__R__ => __R__.name))(module.aliases), n: { v: tryItemByKind(InterfaceResult)((__R__ => __R__.name))(module.interfaces), n: { v: tryItemByKind(InstanceResult)((__R__ => __R__.declaration))(module.instances), n: null } } } } }))(path)(module));
export let processPath = (state => (_P_ => ifElse(isItemView(state.path))((_P_ => (__x__ => ((__x__) => {
  if (__x__.__constructor === "Just" && true) {
    let m = __x__.__args[0];
    return findItem(state.path)(m);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__))(List.first(_P_))))(ModuleResult)(getModulesToShow(_P_)))(state));
export default { getModulesToShow, processPath, ModuleResult, ExpressionResult, TypeResult, AliasResult, InterfaceResult, InstanceResult, NotFound };
