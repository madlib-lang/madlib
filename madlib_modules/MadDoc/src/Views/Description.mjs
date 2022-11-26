// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Description.mad
import {} from "./../../../../__internals__.mjs"
import {  } from "./../State.mjs";
import { div, className, text } from "./../../../MadUI/src/Main.mjs";
import { renderMarkdown } from "./../Markdown.mjs";

Inspect['Record_descriptionf_0d0c831dd5bf147d6ed2e462ed3fe182b'] = {};
Inspect['Record_descriptionf_0d0c831dd5bf147d6ed2e462ed3fe182b']['inspect'] = () => (Inspect_o40) => (__$a__ => `{ ` + `description: ` + Inspect_o40.inspect()(__$a__.description) + ` }`);
export let Description = (_P_ => (content => div(({ v: className(`definition__description`), n: null }))(({ v: content, n: null })))(renderMarkdown((__R__ => __R__.description)(_P_))));
export default { Description };
