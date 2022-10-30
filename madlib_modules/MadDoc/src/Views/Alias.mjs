// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Alias.mad
import {} from "./../../../../__internals__.mjs"
import {  } from "./../State.mjs";
import { div, span, className, text, li } from "./../../../MadUI/src/Main.mjs";
import String from "./../../../../.prelude/String.mjs";
import { Etiquette } from "./Etiquette.mjs";
import { Title } from "./Title.mjs";
import { Since } from "./Since.mjs";
import { Description } from "./Description.mjs";
import { Example } from "./Example.mjs";
import {  } from "./../Parser/Documentation.mjs";

Inspect['Record_hasJSf_0282beae8e01da97cf064d349e642ce84_hasLLVMf_1282beae8e01da97cf064d349e642ce84_isAvailablef_2282beae8e01da97cf064d349e642ce84'] = {};
Inspect['Record_hasJSf_0282beae8e01da97cf064d349e642ce84_hasLLVMf_1282beae8e01da97cf064d349e642ce84_isAvailablef_2282beae8e01da97cf064d349e642ce84']['inspect'] = () => (Inspect_k166) => (Inspect_d159) => (Inspect_w152) => (__$a__ => `{ ` + `hasJS: ` + Inspect_w152.inspect()(__$a__.hasJS) + `, ` + `hasLLVM: ` + Inspect_d159.inspect()(__$a__.hasLLVM) + `, ` + `isAvailable: ` + Inspect_k166.inspect()(__$a__.isAvailable) + ` }`);
export let Alias = (moduleName => aliasDef => {
    let aliasedType = aliasDef.aliasedType;
    let params = (String.isEmpty(aliasDef.params) ? `` : ` ` + aliasDef.params);
    return li(({ v: className(`definition`), n: null }))((__listCtorSpread__(({ v: Etiquette(`Alias`), n: { v: Title(aliasDef.name)(({ hasJS: false, hasLLVM: false, isAvailable: false }))(moduleName), n: null } }), { v: div(({ v: className(`definition__adt`), n: null }))(({ v: span(({ v: className(`highlight`), n: null }))(({ v: `alias`, n: null })), n: { v: span((null))(({ v: ` `, n: { v: aliasDef.name, n: { v: params, n: null } } })), n: { v: span(({ v: className(`definition__constructors`), n: null }))(({ v: span(({ v: className(`definition__constructor`), n: null }))(({ v: span(({ v: className(`highlight`), n: null }))(({ v: ` = `, n: null })), n: { v: span((null))(({ v: aliasedType, n: null })), n: null } })), n: null })), n: null } } })), n: ({ v: Since(aliasDef), n: { v: Description(aliasDef), n: { v: Example(aliasDef), n: null } } }) })));
});
export default { Alias };
