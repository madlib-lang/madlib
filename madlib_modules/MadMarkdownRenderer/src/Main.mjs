// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadMarkdownRenderer/src/Main.mad
import {} from "./../../../__internals__.mjs"
import { Left, Right } from "./../../../.prelude/Either.mjs";
import { parseMarkdown, Bold, Blockquote, Code, H1, H2, H3, H4, H5, H6, Image, InlineCode, Italic, LineReturn, Link, Paragraph, UnorderedList, Text } from "./../../MadMarkdownParser/src/Main.mjs";
import { alt, blockquote, br, className, code, div, h1, h2, h3, h4, h5, h6, i, img, li, p, span, src, strong, ul, text } from "./../../MadUI/src/Main.mjs";
import { defaultConfig, setLinkView } from "./Config.mjs";

let doRender = (config => markdown => div(({ v: className(`markdown`), n: null }))((Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(renderBlock(config))(markdown))));
let renderBlock = (config => __x__ => ((__x__) => {
  if (__x__.__constructor === "H1" && true) {
    let content = __x__.__args[0];
    return h1((null))((renderContent(config)(content)));
  }
  else if (__x__.__constructor === "H2" && true) {
    let content = __x__.__args[0];
    return h2((null))((renderContent(config)(content)));
  }
  else if (__x__.__constructor === "H3" && true) {
    let content = __x__.__args[0];
    return h3((null))((renderContent(config)(content)));
  }
  else if (__x__.__constructor === "H4" && true) {
    let content = __x__.__args[0];
    return h4((null))((renderContent(config)(content)));
  }
  else if (__x__.__constructor === "H5" && true) {
    let content = __x__.__args[0];
    return h5((null))((renderContent(config)(content)));
  }
  else if (__x__.__constructor === "H6" && true) {
    let content = __x__.__args[0];
    return h6((null))((renderContent(config)(content)));
  }
  else if (__x__.__constructor === "Paragraph" && true) {
    let content = __x__.__args[0];
    return p((null))((renderContent(config)(content)));
  }
  else if (__x__.__constructor === "Blockquote" && true) {
    let content = __x__.__args[0];
    return blockquote((null))((renderContent(config)(content)));
  }
  else if (__x__.__constructor === "Code" && true && true) {
    let content = __x__.__args[1];
    return code((null))(({ v: content, n: null }));
  }
  else if (__x__.__constructor === "UnorderedList" && true) {
    let items = __x__.__args[0];
    return ul((null))((Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((item => li((null))((renderContent(config)(item)))))(items)));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__));
let renderContentPart = (config => __x__ => ((__x__) => {
  if (__x__.__constructor === "Text" && true) {
    let t = __x__.__args[0];
    return span(({ v: className(`markdown__text`), n: null }))(({ v: t, n: null }));
  }
  else if (__x__.__constructor === "Bold" && true) {
    let t = __x__.__args[0];
    return strong(({ v: className(`markdown__bold`), n: null }))(({ v: t, n: null }));
  }
  else if (__x__.__constructor === "Italic" && true) {
    let t = __x__.__args[0];
    return i(({ v: className(`markdown__italic`), n: null }))(({ v: t, n: null }));
  }
  else if (__x__.__constructor === "InlineCode" && true) {
    let t = __x__.__args[0];
    return span(({ v: className(`markdown__inline-code`), n: null }))(({ v: t, n: null }));
  }
  else if (__x__.__constructor === "Link" && true && true) {
    let t = __x__.__args[0];
    let l = __x__.__args[1];
    return config.linkView(t)(l);
  }
  else if (__x__.__constructor === "Image" && true && true) {
    let alt_ = __x__.__args[0];
    let s = __x__.__args[1];
    return img(({ v: className(`markdown__image`), n: { v: src(s), n: { v: alt(alt_), n: null } } }))((null));
  }
  else if (__x__.__constructor === "LineReturn") {
    return br((null))((null));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__));
let renderContent = (config => Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(renderContentPart(config)));
export let renderMarkdownWithConfig = (config => _P_ => (__x__ => ((__x__) => {
  if (__x__.__constructor === "Right" && true) {
    let ast = __x__.__args[0];
    return doRender(config)(ast);
  }
  else if (__x__.__constructor === "Left" && true) {
    return p((null))(({ v: `Error processing the given markdown`, n: null }));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__))(parseMarkdown(_P_)));
export let renderMarkdown = renderMarkdownWithConfig(defaultConfig);
export { defaultConfig };
export { setLinkView };
export default { renderMarkdownWithConfig, renderMarkdown };
