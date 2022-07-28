// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadMarkdownRenderer/src/Config.mad
import {} from "./../../../__internals__.mjs"
import { a, href, text } from "./../../MadUI/src/Main.mjs";

Inspect['Record_linkViewf_0'] = {};
Inspect['Record_linkViewf_0']['inspect'] = () => (Inspect_o40) => (__$a__ => `{ ` + `linkView: ` + Inspect_o40.inspect()(__$a__.linkView) + ` }`);
export let defaultConfig = ({ linkView: (name => url => a(({ v: href(url), n: null }))(({ v: name, n: null }))) });
export let setLinkView = (linkView => config => ({ ...config, linkView: linkView }));
export default { defaultConfig, setLinkView };
