// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Example.mad
import {} from "./../../../../__internals__.mjs"
import {  } from "./../State.mjs";
import { className, empty, text, p } from "./../../../MadUI/src/Main.mjs";
import { always, ifElse } from "./../../../../.prelude/Function.mjs";
import { isEmpty } from "./../../../../.prelude/String.mjs";

Inspect['Record_examplef_0ca66fb83c2b9edf51f0d0404a2b09e30'] = {};
Inspect['Record_examplef_0ca66fb83c2b9edf51f0d0404a2b09e30']['inspect'] = () => (Inspect_a52) => (__$a__ => `{ ` + `example: ` + Inspect_a52.inspect()(__$a__.example) + ` }`);
export let Example = (_P_ => ifElse(isEmpty)(always(empty((null))((null))))((example => p(({ v: className(`definition__example`), n: null }))(({ v: example, n: null }))))((__R__ => __R__.example)(_P_)));
export default { Example };
