// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadUI/src/Element.mad
import {} from "./../../../__internals__.mjs"
import {  } from "./Attribute.mjs";
import { reduce } from "./../../../.prelude/List.mjs";
import { wrapEventHandler } from "./CoreUtils.mjs";
import { EventConstructors } from "./Event.mjs";

export let Element = ({ __constructor: "Element", __args: [  ] });
Inspect['Element_e57bb41ee76a8c1acf300f8ac2295260'] = {};
Inspect['Element_e57bb41ee76a8c1acf300f8ac2295260']['inspect'] = () => (Inspect_r823) => (__$a__ => ((__x__) => {
  if (__x__.__constructor === "Element") {
    return `Element`;
  }
  else if (true) {
    return `Unknown`;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__$a__));

import { h } from "snabbdom"
;

const getAttributeTuple = attr =>
  [attr.__constructor.substr(9).toLowerCase(), attr.__args[0]]


const PROP_NAMES = [
  "value"
]


const objectifyAttrs = (env, attrs) => reduce(obj => attr => {
  const [attrName, attrValue] = getAttributeTuple(attr)

  if (attr.__constructor == "AttributeStyle") {
    const items = attr.__args[0].__args[0]
    const styleObj = reduce(obj => ([key, value]) => ({
      ...obj,
      [key]: value,
    }))({})(items)
    return { ...obj, style: styleObj }
  } else if (attr.__constructor == "StringAttribute") {
    return { ...obj, attrs: { ...obj.attrs, [attr.__args[0][0]]: attr.__args[0][1] }}
  } else if (attrName === "key") {
    return { ...obj, key: attrValue }
  } else if (PROP_NAMES.includes(attrName)) {
    return { ...obj, props: { ...obj.props, [attrName]: attrValue }}
  } else if (attrName.substr(0, 2) === "on") {
    const eventName = attrName.substr(2)
    const ctor = EventConstructors[eventName]
    return { ...obj, on: { ...obj.on, [eventName]: wrapEventHandler(env, ctor, attrValue) }}
  } else {
    return { ...obj, attrs: { ...obj.attrs, [attrName]: attrValue }}
  }
})({})(attrs)


const arrayifyChildren = (madlistChildren) => {
  let jsChildren = []
  while (madlistChildren !== null) {
    jsChildren.push(madlistChildren.v)
    madlistChildren = madlistChildren.n
  }

  return jsChildren
}
;
export let tag = (tagName => attrs => children =>  {
  return h(tagName, objectifyAttrs(window.env, attrs), arrayifyChildren(children))
} );
export let a = tag(`a`);
export let abbr = tag(`abbr`);
export let address = tag(`address`);
export let area = tag(`area`);
export let article = tag(`article`);
export let aside = tag(`aside`);
export let audio = tag(`audio`);
export let b = tag(`b`);
export let bdi = tag(`bdi`);
export let bdo = tag(`bdo`);
export let blockquote = tag(`blockquote`);
export let br = tag(`br`);
export let button = tag(`button`);
export let canvas = tag(`canvas`);
export let caption = tag(`caption`);
export let cite = tag(`cite`);
export let code = tag(`code`);
export let col = tag(`col`);
export let colgroup = tag(`colgroup`);
export let content = tag(`content`);
export let data = tag(`data`);
export let datalist = tag(`datalist`);
export let dd = tag(`dd`);
export let del = tag(`del`);
export let details = tag(`details`);
export let dfn = tag(`dfn`);
export let dialog = tag(`dialog`);
export let div = tag(`div`);
export let dl = tag(`dl`);
export let dt = tag(`dt`);
export let em = tag(`em`);
export let embed = tag(`embed`);
export let fieldset = tag(`fieldset`);
export let figcaption = tag(`figcaption`);
export let figure = tag(`figure`);
export let footer = tag(`footer`);
export let form = tag(`form`);
export let h1 = tag(`h1`);
export let h2 = tag(`h2`);
export let h3 = tag(`h3`);
export let h4 = tag(`h4`);
export let h5 = tag(`h5`);
export let h6 = tag(`h6`);
export let header = tag(`header`);
export let hgroup = tag(`hgroup`);
export let hr = tag(`hr`);
export let i = tag(`i`);
export let iframe = tag(`iframe`);
export let img = tag(`img`);
export let input = tag(`input`);
export let ins = tag(`ins`);
export let kbd = tag(`kbd`);
export let label = tag(`label`);
export let legend = tag(`legend`);
export let li = tag(`li`);
export let main = tag(`main`);
export let mapElement = tag(`map`);
export let mark = tag(`mark`);
export let menu = tag(`menu`);
export let menuitem = tag(`menuitem`);
export let meta = tag(`meta`);
export let meter = tag(`meter`);
export let nav = tag(`nav`);
export let noscript = tag(`noscript`);
export let object = tag(`object`);
export let ol = tag(`ol`);
export let optgroup = tag(`optgroup`);
export let option = tag(`option`);
export let output = tag(`output`);
export let p = tag(`p`);
export let param = tag(`param`);
export let picture = tag(`picture`);
export let plaintext = tag(`plaintext`);
export let pre = tag(`pre`);
export let progress = tag(`progress`);
export let q = tag(`q`);
export let rp = tag(`rp`);
export let rt = tag(`rt`);
export let rtc = tag(`rtc`);
export let ruby = tag(`ruby`);
export let s = tag(`s`);
export let samp = tag(`samp`);
export let script = tag(`script`);
export let section = tag(`section`);
export let select = tag(`select`);
export let shadow = tag(`shadow`);
export let slot = tag(`slot`);
export let small = tag(`small`);
export let source = tag(`source`);
export let span = tag(`span`);
export let strong = tag(`strong`);
export let sub = tag(`sub`);
export let summary = tag(`summary`);
export let sup = tag(`sup`);
export let table = tag(`table`);
export let tbody = tag(`tbody`);
export let td = tag(`td`);
export let template = tag(`template`);
export let textarea = tag(`textarea`);
export let tfoot = tag(`tfoot`);
export let th = tag(`th`);
export let thead = tag(`thead`);
export let time = tag(`time`);
export let title = tag(`title`);
export let tr = tag(`tr`);
export let track = tag(`track`);
export let u = tag(`u`);
export let ul = tag(`ul`);
export let _$_var_$_ = tag(`var`);
export let video = tag(`video`);
export let wbr = tag(`wbr`);
export let text = (t =>  t );
export let empty = (attrs => children =>  null );
export let link = (attrs => children => {
  const objAttrs = objectifyAttrs(window.env, attrs);
  if (objAttrs.attrs.to) {
    if (!objAttrs.attrs) {
      objAttrs.attrs = {}
    }
    objAttrs.attrs.href = `\#${objAttrs.attrs.to}`
    delete objAttrs.attrs.to
  }

  return h("a", { ...objAttrs }, arrayifyChildren(children));
});
export default { tag, a, abbr, address, area, article, aside, audio, b, bdi, bdo, blockquote, br, button, canvas, caption, cite, code, col, colgroup, content, data, datalist, dd, del, details, dfn, dialog, div, dl, dt, em, embed, fieldset, figcaption, figure, footer, form, h1, h2, h3, h4, h5, h6, header, hgroup, hr, i, iframe, img, input, ins, kbd, label, legend, li, main, mapElement, mark, menu, menuitem, meta, meter, nav, noscript, object, ol, optgroup, option, output, p, param, picture, plaintext, pre, progress, q, rp, rt, rtc, ruby, s, samp, script, section, select, shadow, slot, small, source, span, strong, sub, summary, sup, table, tbody, td, template, textarea, tfoot, th, thead, time, title, tr, track, u, ul, _$_var_$_, video, wbr, text, empty, link, Element };
