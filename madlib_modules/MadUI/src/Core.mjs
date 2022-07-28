// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadUI/src/Core.mad
import {} from "./../../../__internals__.mjs"
import { get } from "./../../../.prelude/Dictionary.mjs";
import {  } from "./../../../.prelude/Number.mjs";
import {  } from "./Config.mjs";
import {  } from "./Element.mjs";
import { runAction, wrapEventHandler } from "./CoreUtils.mjs";
import { EventConstructors } from "./Event.mjs";


import {
  attributesModule,
  init,
  propsModule,
  eventListenersModule,
  styleModule
} from "snabbdom"
;
export let getUrl = (_ =>  document.location.hash.substr(1) || "/" );
export let render = (view => initialState => containerId => {
    
  window.env = {
    patch: null,
    currentElement: null,
    currentState: null,
    rootView: null,
  }
  ;
    let initialElement = view(initialState);
    
  const patch = init([attributesModule, propsModule, eventListenersModule, styleModule])
  patch(document.getElementById(containerId), initialElement)

  window.env.patch = patch
  window.env.currentElement = initialElement
  window.env.rootView = view
  window.env.currentState = initialState
  ;
    return ({ __constructor: "Unit", __args: [] });
});

const startGlobalEventHandlers = (env, globalActions) => {
  const keysForWindowEvents = [];
  while (globalActions !== null) {
    keysForWindowEvents.push(({
      eventName: globalActions.v.__args[0],
      eventHandler: globalActions.v.__args[1],
    }))
    globalActions = globalActions.n
  }

  keysForWindowEvents.forEach(ga => {
    const handler = wrapEventHandler(env, EventConstructors[ga.eventName], ga.eventHandler)
    window.addEventListener(ga.eventName, handler)

    if (ga.eventName === "popstate") {
      handler({})
    }
  })
}
;
export let renderWithConfig = (config => view => initialState => containerId => {
    render(view)(initialState)(containerId);
    
  const env = window.env
  startGlobalEventHandlers(env, config.globalEventHandlers)

  let subs = config.subscriptions
  while (subs !== null) {
    subs.v(runAction(env))
    subs = subs.n
  }
  ;
    return ({ __constructor: "Unit", __args: [] });
});
export default { getUrl, render, renderWithConfig };
