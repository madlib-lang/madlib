// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Description.mad
import {} from "./../../../../__internals__.mjs"
import {  } from "./../State.mjs";
import { div, className, text } from "./../../../MadUI/src/Main.mjs";
import { renderMarkdown } from "./../Markdown.mjs";

export let Description = (_P_ => (content => div(({ v: className(`definition__description`), n: null }))(({ v: content, n: null })))(renderMarkdown((__R__ => __R__.description)(_P_))));
export default { Description };
