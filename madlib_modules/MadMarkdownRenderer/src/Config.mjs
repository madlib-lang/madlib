// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadMarkdownRenderer/src/Config.mad
import {} from "./../../../__internals__.mjs"
import { a, href, text } from "./../../MadUI/src/Main.mjs";

Inspect['Record_linkViewf_0ce723a3860371783fb1c5e99ad00afb9'] = {};
Inspect['Record_linkViewf_0ce723a3860371783fb1c5e99ad00afb9']['inspect'] = () => (Inspect_n39) => (__$a__ => `{ ` + `linkView: ` + Inspect_n39.inspect()(__$a__.linkView) + ` }`);
export let defaultConfig = ({ linkView: (name => url => a(({ v: href(url), n: null }))(({ v: name, n: null }))) });
export let setLinkView = (linkView => config => ({ ...config, linkView: linkView }));
export default { defaultConfig, setLinkView };
