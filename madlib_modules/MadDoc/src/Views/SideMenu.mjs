// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/SideMenu.mad
import {} from "./../../../../__internals__.mjs"
import {  } from "./../State.mjs";
import List from "./../../../../.prelude/List.mjs";
import Tuple from "./../../../../.prelude/Tuple.mjs";
import String from "./../../../../.prelude/String.mjs";
import { all } from "./../../../../.prelude/Function.mjs";
import { div, h3, p, li, link, to, className, text, span, ul } from "./../../../MadUI/src/Main.mjs";
import {  } from "./../../../../.prelude/Compare.mjs";
import { getName } from "./../Parser/Documentation.mjs";

let ModuleLink = (module => li(({ v: className(`side-menu__link-item`), n: null }))(({ v: link(({ v: className(`side-menu__link`), n: { v: to(`/` + module.name), n: null } }))(({ v: span(({ v: className(`side-menu__link-name`), n: null }))(({ v: module.name, n: null })), n: null })), n: null })));
let MenuLink = (__x__ => ((__x__) => {
  if (__x__.length === 2 && true && true) {
    let [name,moduleName] = __x__;
    return li(({ v: className(`side-menu__link-item`), n: null }))(({ v: link(({ v: className(`side-menu__link`), n: { v: to(`/` + moduleName + `/` + name), n: null } }))(({ v: span(({ v: className(`side-menu__link-name`), n: null }))(({ v: name, n: null })), n: { v: span(({ v: className(`side-menu__link-extra`), n: null }))(({ v: moduleName, n: null })), n: null } })), n: null }));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__));
let LinksForType = (search => getItems => retrieveName => _P_ => itemsToLinks(Monad.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.chain()((module => (_P_ => Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((a => ([retrieveName(a), module.name])))(List.filter((_P_ => String.match(search)(String.toLower(retrieveName(_P_)))))(getItems(_P_))))(module)))(_P_)));
let itemsToLinks = (_P_ => Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(MenuLink)(List.sortBy((a => b => Comparable.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.compare()(Tuple.fst(a))(Tuple.fst(b))))(_P_)));
let sortAndFilterModules = (search => _P_ => List.sortBy((a => b => Comparable.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.compare()(a.name)(b.name)))(List.filter((_P_ => String.match(search)(String.toLower((__R__ => __R__.name)(_P_)))))(_P_)));
let MenuSection = (title => items => (List.isEmpty(items) ? (null) : ({ v: h3(({ v: className(`side-menu__title`), n: null }))(({ v: title, n: null })), n: { v: ul(({ v: className(`side-menu__link-list`), n: null }))((items)), n: null } })));
export let SideMenu = (search => modules => {
    let moduleLinks = Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()(ModuleLink)(sortAndFilterModules(search)(modules));
    let functionLinks = LinksForType(search)((__R__ => __R__.expressions))(getName)(modules);
    let typeLinks = LinksForType(search)((__R__ => __R__.typeDeclarations))((__R__ => __R__.name))(modules);
    let aliasLinks = LinksForType(search)((__R__ => __R__.aliases))((__R__ => __R__.name))(modules);
    let interfaceLinks = LinksForType(search)((__R__ => __R__.interfaces))((__R__ => __R__.name))(modules);
    let instanceLinks = LinksForType(search)((__R__ => __R__.instances))((__R__ => __R__.declaration))(modules);
    let notFound = all(List.isEmpty)(({ v: moduleLinks, n: { v: functionLinks, n: { v: typeLinks, n: { v: aliasLinks, n: { v: interfaceLinks, n: { v: instanceLinks, n: null } } } } } }));
    return (notFound ? div(({ v: className(`side-menu`), n: null }))(({ v: p(({ v: className(`side-menu__no-result`), n: null }))(({ v: `No result was found for `, n: { v: span(({ v: className(`side-menu__no-result-search`), n: null }))(({ v: search, n: null })), n: null } })), n: null })) : div(({ v: className(`side-menu`), n: null }))(({ v: div(({ v: className(`side-menu__scrollbar-container`), n: null }))((__listCtorSpread__(MenuSection(`MODULES`)(moduleLinks), __listCtorSpread__(MenuSection(`FUNCTIONS`)(functionLinks), __listCtorSpread__(MenuSection(`TYPES`)(typeLinks), __listCtorSpread__(MenuSection(`ALIASES`)(aliasLinks), __listCtorSpread__(MenuSection(`INTERFACES`)(interfaceLinks), MenuSection(`INSTANCES`)(instanceLinks)))))))), n: null })));
});
export default { SideMenu };
