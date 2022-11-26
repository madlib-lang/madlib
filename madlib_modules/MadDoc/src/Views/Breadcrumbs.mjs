// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Breadcrumbs.mad
import {} from "./../../../../__internals__.mjs"
import {  } from "./../State.mjs";
import { canonicalizePath, joinPath, splitPath } from "./../../../../.prelude/FilePath/Posix.mjs";
import { append, intersperseWithIndex, reduce } from "./../../../../.prelude/List.mjs";
import {  } from "./../../../../.prelude/Number.mjs";
import { snd } from "./../../../../.prelude/Tuple.mjs";
import { className, key, li, link, text, to, ul } from "./../../../MadUI/src/Main.mjs";

let Breadcrumb = (a => b => ({ __constructor: "Breadcrumb", __args: [ a, b ] }));
Inspect['Breadcrumb_4abd1cdd7b1c927d13bb10af893c40c1'] = {};
Inspect['Breadcrumb_4abd1cdd7b1c927d13bb10af893c40c1']['inspect'] = () => (__$a__ => ((__x__) => {
  if (__x__.__constructor === "Breadcrumb" && true && true) {
    let a0 = __x__.__args[0];
    let a1 = __x__.__args[1];
    return `Breadcrumb(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `, ` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a1) + `)`;
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
let getLink = (__x__ => ((__x__) => {
  if (__x__.__constructor === "Breadcrumb" && true && true) {
    let l = __x__.__args[1];
    return l;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__));
let getName = (__x__ => ((__x__) => {
  if (__x__.__constructor === "Breadcrumb" && true && true) {
    let l = __x__.__args[0];
    return l;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__));
let generateBreadcrumbName = (_P_ => (pathSegment => (__eq__(pathSegment, `/`) || __eq__(pathSegment, ``) ? `home` : pathSegment))(canonicalizePath(_P_)));
let computeBreadcrumbs = (_P_ => snd(reduce((acc => pathSegment => ((__x__) => {
  if (__x__.length === 2 && true && true) {
    let [prevPath,breadcrumbs] = __x__;
    return (_P_ => (path => ([path, append(Breadcrumb(generateBreadcrumbName(pathSegment))(path))(breadcrumbs)]))(joinPath(append(pathSegment)(_P_))))(({ v: prevPath, n: null }));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(acc)))(([``, (null)]))(splitPath((__R__ => __R__.path)(_P_)))));
let BreadcrumbItem = (breadcrumb => li(({ v: className(`breadcrumbs__item`), n: { v: key(getLink(breadcrumb)), n: null } }))(({ v: link(({ v: to(getLink(breadcrumb)), n: null }))(({ v: getName(breadcrumb), n: null })), n: null })));
export let Breadcrumbs = (_P_ => (breadcrumbs => ul(({ v: className(`breadcrumbs`), n: null }))((breadcrumbs)))(intersperseWithIndex((i => li(({ v: className(`breadcrumbs__separator`), n: { v: key(`sep-` + Show.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d.show()(i)), n: null } }))(({ v: `/`, n: null }))))(Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(BreadcrumbItem)(computeBreadcrumbs(_P_)))));
export default { Breadcrumbs };
