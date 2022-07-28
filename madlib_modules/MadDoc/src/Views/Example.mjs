// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Example.mad
import {} from "./../../../../__internals__.mjs"
import {  } from "./../State.mjs";
import { className, empty, text, p } from "./../../../MadUI/src/Main.mjs";
import { always, ifElse } from "./../../../../.prelude/Function.mjs";
import { isEmpty } from "./../../../../.prelude/String.mjs";

export let Example = (_P_ => ifElse(isEmpty)(always(empty((null))((null))))((example => p(({ v: className(`definition__example`), n: null }))(({ v: example, n: null }))))((__R__ => __R__.example)(_P_)));
export default { Example };
