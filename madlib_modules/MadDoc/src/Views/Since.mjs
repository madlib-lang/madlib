// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Since.mad
import {} from "./../../../../__internals__.mjs"
import {  } from "./../State.mjs";
import { className, empty, text, p } from "./../../../MadUI/src/Main.mjs";
import { always, ifElse } from "./../../../../.prelude/Function.mjs";
import { isEmpty } from "./../../../../.prelude/String.mjs";

Inspect['Record_sincef_0965d90d8ef0dbde173c27481f32bc777'] = {};
Inspect['Record_sincef_0965d90d8ef0dbde173c27481f32bc777']['inspect'] = () => (Inspect_a52) => (__$a__ => `{ ` + `since: ` + Inspect_a52.inspect()(__$a__.since) + ` }`);
export let Since = (_P_ => ifElse(isEmpty)(always(empty((null))((null))))((since => p(({ v: className(`definition__since`), n: null }))(({ v: `since v`, n: { v: since, n: null } }))))((__R__ => __R__.since)(_P_)));
export default { Since };
