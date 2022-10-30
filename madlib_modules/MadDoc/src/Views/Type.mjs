// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Type.mad
import {} from "./../../../../__internals__.mjs"
import {  } from "./../State.mjs";
import { _$_length_$_, first } from "./../../../../.prelude/List.mjs";
import { fromMaybe } from "./../../../../.prelude/Maybe.mjs";
import { div, span, className, text, li } from "./../../../MadUI/src/Main.mjs";
import { Etiquette } from "./Etiquette.mjs";
import { Title } from "./Title.mjs";
import { Since } from "./Since.mjs";
import { Description } from "./Description.mjs";
import { Example } from "./Example.mjs";
import {  } from "./../Parser/Documentation.mjs";

Inspect['Record_hasJSf_08676e862654b892c9c43f99085b0b684_hasLLVMf_18676e862654b892c9c43f99085b0b684_isAvailablef_28676e862654b892c9c43f99085b0b684'] = {};
Inspect['Record_hasJSf_08676e862654b892c9c43f99085b0b684_hasLLVMf_18676e862654b892c9c43f99085b0b684_isAvailablef_28676e862654b892c9c43f99085b0b684']['inspect'] = () => (Inspect_z259) => (Inspect_s252) => (Inspect_l245) => (__$a__ => `{ ` + `hasJS: ` + Inspect_l245.inspect()(__$a__.hasJS) + `, ` + `hasLLVM: ` + Inspect_s252.inspect()(__$a__.hasLLVM) + `, ` + `isAvailable: ` + Inspect_z259.inspect()(__$a__.isAvailable) + ` }`);
export let Type = (moduleName => typeDefinition => {
    let constructors = typeDefinition.constructors;
    let manyCtors = _$_length_$_(constructors) > 1;
    let renderedConstructors = (manyCtors ? ConstructorsView(`=`)(constructors) : ({ v: span(({ v: className(`definition__constructor`), n: null }))(({ v: span(({ v: className(`highlight`), n: null }))(({ v: ` = `, n: null })), n: { v: span((null))(({ v: fromMaybe(`???`)(first(constructors)), n: null })), n: null } })), n: null }));
    return li(({ v: className(`definition`), n: null }))((__listCtorSpread__(({ v: Etiquette(`Type`), n: { v: Title(typeDefinition.name)(({ hasJS: false, hasLLVM: false, isAvailable: false }))(moduleName), n: null } }), { v: div(({ v: className(`definition__adt`), n: null }))(({ v: span(({ v: className(`highlight`), n: null }))(({ v: `type`, n: null })), n: { v: span((null))(({ v: ` `, n: { v: typeDefinition.name, n: { v: ` `, n: { v: typeDefinition.params, n: null } } } })), n: { v: span(({ v: className(`definition__constructors`), n: null }))((renderedConstructors)), n: null } } })), n: ({ v: Since(typeDefinition), n: { v: Description(typeDefinition), n: { v: Example(typeDefinition), n: null } } }) })));
});
let ConstructorsView = (separator => items => {
    let $_result_;
    let $_continue_ = true;
    let $_start_ = {};
    let $_end_ = $_start_;
    let $$separator = separator;
    let $$items = items;

    while($_continue_) {
      let $separator = $$separator;
      let $items = $$items;

        $_continue_ = false;
        ((__x__) => {
  if (__x__ !== null && true && true) {
    let { v: ctor, n: more } = __x__;
    ($_end_ = $_end_.n = { v: ConstructorView($separator)(ctor) }, $$separator = `|`, $$items = more, $_continue_ = true);
  }
  else if (__x__ !== null && true && __x__.n === null) {
    let { v: ctor } = __x__;
    ($_end_.n = ({ v: ConstructorView($separator)(ctor), n: null }), $_result_ = $_start_.n);
  }
  else if (__x__ === null) {
    ($_end_.n = (null), $_result_ = $_start_.n);
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})($items)
    }
    return $_result_;
});
let ConstructorView = (separator => constructor => div(({ v: className(`definition__constructor`), n: null }))(({ v: span(({ v: className(`highlight`), n: null }))(({ v: `  `, n: { v: separator, n: null } })), n: { v: span((null))(({ v: ` `, n: { v: constructor, n: null } })), n: null } })));
export default { Type };
