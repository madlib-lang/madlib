// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadUI/src/Config.mad
import {} from "./../../../__internals__.mjs"
import { append } from "./../../../.prelude/List.mjs";
import {  } from "./Action.mjs";
import {  } from "./Subscription.mjs";

Inspect['Record_globalEventHandlersf_0e264b6be2a515e01e171e67e9c05fdef_subscriptionsf_1e264b6be2a515e01e171e67e9c05fdef'] = {};
Inspect['Record_globalEventHandlersf_0e264b6be2a515e01e171e67e9c05fdef_subscriptionsf_1e264b6be2a515e01e171e67e9c05fdef']['inspect'] = () => (Inspect_p67) => (Inspect_i60) => (__$a__ => `{ ` + `globalEventHandlers: ` + Inspect_i60.inspect()(__$a__.globalEventHandlers) + `, ` + `subscriptions: ` + Inspect_p67.inspect()(__$a__.subscriptions) + ` }`);
export let DEFAULT_CONFIG = ({ subscriptions: (null), globalEventHandlers: (null) });
export let addGlobalEventHandler = (action => config => ({ ...config, globalEventHandlers: append(action)(config.globalEventHandlers) }));
export let addSubscription = (action => config => ({ ...config, subscriptions: append(action)(config.subscriptions) }));
export default { DEFAULT_CONFIG, addGlobalEventHandler, addSubscription };
