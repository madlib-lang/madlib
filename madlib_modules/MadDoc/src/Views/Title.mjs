// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Title.mad
import {} from "./../../../../__internals__.mjs"
import {  } from "./../State.mjs";
import {  } from "./../TargetInfo.mjs";
import { className, empty, h2, span, text } from "./../../../MadUI/src/Main.mjs";

let makeTagClassName = (isWarning => (isWarning ? `definition__target-tag definition__target-tag--warning` : `definition__target-tag`));
let TargetTags = (targetInfo => ({ v: (targetInfo.hasJS ? span(({ v: className(makeTagClassName(!(targetInfo.isAvailable))), n: null }))(({ v: `JS`, n: null })) : empty((null))((null))), n: { v: (targetInfo.hasLLVM ? span(({ v: className(makeTagClassName(!(targetInfo.isAvailable))), n: null }))(({ v: `LLVM`, n: null })) : empty((null))((null))), n: null } }));
export let Title = (title => targetInfo => moduleName => h2(({ v: className(`definition__title`), n: null }))(({ v: span(({ v: className(`definition__title-span`), n: null }))(({ v: title, n: null })), n: __listCtorSpread__(TargetTags(targetInfo), { v: span(({ v: className(`definition__module`), n: null }))(({ v: moduleName, n: null })), n: null }) })));
export default { Title };
