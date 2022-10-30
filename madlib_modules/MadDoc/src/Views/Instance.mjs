// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Instance.mad
import {} from "./../../../../__internals__.mjs"
import { div, span, className, text, li } from "./../../../MadUI/src/Main.mjs";
import {  } from "./../State.mjs";
import { Etiquette } from "./Etiquette.mjs";
import { Title } from "./Title.mjs";
import { Since } from "./Since.mjs";
import { Description } from "./Description.mjs";
import { Example } from "./Example.mjs";
import {  } from "./../Parser/Documentation.mjs";

Inspect['Record_hasJSf_0e18b622886cb39510dabf7debf8c29eb_hasLLVMf_1e18b622886cb39510dabf7debf8c29eb_isAvailablef_2e18b622886cb39510dabf7debf8c29eb'] = {};
Inspect['Record_hasJSf_0e18b622886cb39510dabf7debf8c29eb_hasLLVMf_1e18b622886cb39510dabf7debf8c29eb_isAvailablef_2e18b622886cb39510dabf7debf8c29eb']['inspect'] = () => (Inspect_j165) => (Inspect_c158) => (Inspect_v151) => (__$a__ => `{ ` + `hasJS: ` + Inspect_v151.inspect()(__$a__.hasJS) + `, ` + `hasLLVM: ` + Inspect_c158.inspect()(__$a__.hasLLVM) + `, ` + `isAvailable: ` + Inspect_j165.inspect()(__$a__.isAvailable) + ` }`);
export let Instance = (moduleName => instanceDef => {
    let constraints = instanceDef.constraints;
    let constraintElements = (!__eq__(constraints, ``) ? ({ v: span((null))(({ v: constraints, n: null })), n: { v: span(({ v: className(`highlight`), n: null }))(({ v: ` => `, n: null })), n: null } }) : (null));
    return li(({ v: className(`definition`), n: null }))((__listCtorSpread__(({ v: Etiquette(`Instance`), n: { v: Title(instanceDef.declaration)(({ hasJS: false, hasLLVM: false, isAvailable: false }))(moduleName), n: null } }), { v: div(({ v: className(`definition__interface`), n: null }))(({ v: span(({ v: className(`highlight`), n: null }))(({ v: `instance `, n: null })), n: { v: span((null))((constraintElements)), n: { v: span((null))(({ v: instanceDef.declaration, n: null })), n: null } } })), n: ({ v: Since(instanceDef), n: { v: Description(instanceDef), n: { v: Example(instanceDef), n: null } } }) })));
});
export default { Instance };
