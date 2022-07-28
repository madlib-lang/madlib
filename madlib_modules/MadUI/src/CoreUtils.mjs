// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadUI/src/CoreUtils.mad
import {} from "./../../../__internals__.mjs"
import { fulfill } from "./../../../.prelude/Wish.mjs";

export let runAction =  env => updater => {
  env.currentState = updater(env.currentState)
  window.env = env
  const newElement = env.rootView(env.currentState)
  env.patch(env.currentElement, newElement)
  env.currentElement = newElement
} ;
export let wrapEventHandler =  (env, ctor, handler) => {
  return event => {
    event.eventType = event.type
    // Calling an event handler gives us a list of wishes
    let wishes = handler(env.currentState)(ctor(event))

    while (wishes !== null) {
      fulfill(runAction(env))(runAction(env))(wishes.v)
      wishes = wishes.n
    }
  }
}
;
export default { runAction, wrapEventHandler };
