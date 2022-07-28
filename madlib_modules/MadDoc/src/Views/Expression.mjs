// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadDoc/src/Views/Expression.mad
import {} from "./../../../../__internals__.mjs"
import {  } from "./../State.mjs";
import {  } from "./../TargetInfo.mjs";
import { className, key, li, text } from "./../../../MadUI/src/Main.mjs";
import { getName } from "./../Parser/Documentation.mjs";
import { Etiquette } from "./Etiquette.mjs";
import { Title } from "./Title.mjs";
import { Since } from "./Since.mjs";
import { Description } from "./Description.mjs";
import { Example } from "./Example.mjs";
import { Typing } from "./Typing.mjs";
import { TargetedItem } from "./TargetedItem.mjs";

let makeKey = (moduleName => targeted => moduleName + `-` + getName(targeted));
let ExpressionView = (moduleName => targetInfo => definition => li(({ v: className(`definition` + (targetInfo.isAvailable ? `` : ` definition--greyed-out`)), n: { v: key(moduleName + `-` + definition.name), n: null } }))(({ v: Etiquette(`Function`), n: { v: Title(definition.name)(targetInfo)(moduleName), n: { v: Typing(definition), n: { v: Since(definition), n: { v: Description(definition), n: { v: Example(definition), n: null } } } } } })));
export let Expression = (target => moduleName => definition => TargetedItem(target)(definition)(ExpressionView(moduleName)));
export default { Expression };
