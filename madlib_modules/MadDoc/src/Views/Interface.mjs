// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Interface.mad
import {} from "./../../../../__internals__.mjs"
import {  } from "./../State.mjs";
import { div, span, className, text, li } from "./../../../MadUI/src/Main.mjs";
import { Etiquette } from "./Etiquette.mjs";
import { Title } from "./Title.mjs";
import { Since } from "./Since.mjs";
import { Description } from "./Description.mjs";
import { Example } from "./Example.mjs";
import {  } from "./../Parser/Documentation.mjs";

Inspect['Record_hasJSf_08e01963e32f303d010c30bf965841bdf_hasLLVMf_18e01963e32f303d010c30bf965841bdf_isAvailablef_28e01963e32f303d010c30bf965841bdf'] = {};
Inspect['Record_hasJSf_08e01963e32f303d010c30bf965841bdf_hasLLVMf_18e01963e32f303d010c30bf965841bdf_isAvailablef_28e01963e32f303d010c30bf965841bdf']['inspect'] = () => (Inspect_h189) => (Inspect_a182) => (Inspect_t175) => (__$a__ => `{ ` + `hasJS: ` + Inspect_t175.inspect()(__$a__.hasJS) + `, ` + `hasLLVM: ` + Inspect_a182.inspect()(__$a__.hasLLVM) + `, ` + `isAvailable: ` + Inspect_h189.inspect()(__$a__.isAvailable) + ` }`);
export let Interface = (moduleName => interfaceDef => {
    let methods = interfaceDef.methods;
    let constraints = interfaceDef.constraints;
    let constraintElements = (!__eq__(constraints, ``) ? ({ v: span((null))(({ v: constraints, n: null })), n: { v: span(({ v: className(`highlight`), n: null }))(({ v: ` => `, n: null })), n: null } }) : (null));
    return li(({ v: className(`definition`), n: null }))((__listCtorSpread__(({ v: Etiquette(`Interface`), n: { v: Title(interfaceDef.name)(({ hasJS: false, hasLLVM: false, isAvailable: false }))(moduleName), n: null } }), { v: div(({ v: className(`definition__interface`), n: null }))(({ v: span(({ v: className(`highlight`), n: null }))(({ v: `interface `, n: null })), n: { v: span((null))((constraintElements)), n: { v: span((null))(({ v: interfaceDef.name, n: { v: ` `, n: { v: interfaceDef.vars, n: null } } })), n: { v: span(({ v: className(`highlight`), n: null }))(({ v: ` {`, n: null })), n: { v: div((null))((Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((method => div((null))(({ v: `  `, n: { v: method, n: null } }))))(methods))), n: { v: span(({ v: className(`highlight`), n: null }))(({ v: `}`, n: null })), n: null } } } } } })), n: ({ v: Since(interfaceDef), n: { v: Description(interfaceDef), n: { v: Example(interfaceDef), n: null } } }) })));
});
export default { Interface };
