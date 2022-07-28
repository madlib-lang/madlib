// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Markdown.mad
import {} from "./../../../__internals__.mjs"
import {  } from "./State.mjs";
import { className, text, link, to } from "./../../MadUI/src/Main.mjs";
import { renderMarkdownWithConfig, defaultConfig, setLinkView } from "./../../MadMarkdownRenderer/src/Main.mjs";

let mdConfig = (_P_ => setLinkView((txt => url => link(({ v: className(`markdown__link`), n: { v: to(url), n: null } }))(({ v: txt, n: null }))))(_P_))(defaultConfig);
export let renderMarkdown = renderMarkdownWithConfig(mdConfig);
export default { renderMarkdown };
