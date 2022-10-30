// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadUI/src/Event.mad
import {} from "./../../../__internals__.mjs"
import { getKeyFromCode } from "./Key.mjs";

export let AbstractEvent = (a => ({ __constructor: "AbstractEvent", __args: [ a ] }));
export let MouseEvent = (a => ({ __constructor: "MouseEvent", __args: [ a ] }));
export let InputEvent = (a => ({ __constructor: "InputEvent", __args: [ a ] }));
export let KeyboardEvent = (a => ({ __constructor: "KeyboardEvent", __args: [ a ] }));
export let PopStateEvent = (a => ({ __constructor: "PopStateEvent", __args: [ a ] }));
Inspect['Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_defaultPreventedf_150730a644794cadc5c69c04e30ae3c86_eventTypef_250730a644794cadc5c69c04e30ae3c86_preventDefaultf_350730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_450730a644794cadc5c69c04e30ae3c86_stopPropagationf_550730a644794cadc5c69c04e30ae3c86_timeStampf_650730a644794cadc5c69c04e30ae3c86'] = {};
Inspect['Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_defaultPreventedf_150730a644794cadc5c69c04e30ae3c86_eventTypef_250730a644794cadc5c69c04e30ae3c86_preventDefaultf_350730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_450730a644794cadc5c69c04e30ae3c86_stopPropagationf_550730a644794cadc5c69c04e30ae3c86_timeStampf_650730a644794cadc5c69c04e30ae3c86']['inspect'] = () => (Inspect_h865) => (Inspect_a858) => (Inspect_t851) => (Inspect_m844) => (Inspect_f837) => (Inspect_y830) => (Inspect_r823) => (__$a__ => `{ ` + `bubbles: ` + Inspect_r823.inspect()(__$a__.bubbles) + `, ` + `defaultPrevented: ` + Inspect_y830.inspect()(__$a__.defaultPrevented) + `, ` + `eventType: ` + Inspect_f837.inspect()(__$a__.eventType) + `, ` + `preventDefault: ` + Inspect_m844.inspect()(__$a__.preventDefault) + `, ` + `stopImmediatePropagation: ` + Inspect_t851.inspect()(__$a__.stopImmediatePropagation) + `, ` + `stopPropagation: ` + Inspect_a858.inspect()(__$a__.stopPropagation) + `, ` + `timeStamp: ` + Inspect_h865.inspect()(__$a__.timeStamp) + ` }`);
Inspect['Record_valuef_050730a644794cadc5c69c04e30ae3c86'] = {};
Inspect['Record_valuef_050730a644794cadc5c69c04e30ae3c86']['inspect'] = () => (Inspect_x881) => (__$a__ => `{ ` + `value: ` + Inspect_x881.inspect()(__$a__.value) + ` }`);
Inspect['Event_50730a644794cadc5c69c04e30ae3c86'] = {};
Inspect['Event_50730a644794cadc5c69c04e30ae3c86']['inspect'] = () => (__$a__ => ((__x__) => {
  if (__x__.__constructor === "AbstractEvent" && true) {
    let a0 = __x__.__args[0];
    return `AbstractEvent(` + Inspect.Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_defaultPreventedf_150730a644794cadc5c69c04e30ae3c86_eventTypef_250730a644794cadc5c69c04e30ae3c86_preventDefaultf_350730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_450730a644794cadc5c69c04e30ae3c86_stopPropagationf_550730a644794cadc5c69c04e30ae3c86_timeStampf_650730a644794cadc5c69c04e30ae3c86.inspect()(Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(a0) + `)`;
  }
  else if (__x__.__constructor === "MouseEvent" && true) {
    let a0 = __x__.__args[0];
    return `MouseEvent(` + Inspect.Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_defaultPreventedf_150730a644794cadc5c69c04e30ae3c86_eventTypef_250730a644794cadc5c69c04e30ae3c86_preventDefaultf_350730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_450730a644794cadc5c69c04e30ae3c86_stopPropagationf_550730a644794cadc5c69c04e30ae3c86_timeStampf_650730a644794cadc5c69c04e30ae3c86.inspect()(Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(a0) + `)`;
  }
  else if (__x__.__constructor === "InputEvent" && true) {
    let a0 = __x__.__args[0];
    return `InputEvent(` + Inspect.Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_dataf_150730a644794cadc5c69c04e30ae3c86_defaultPreventedf_250730a644794cadc5c69c04e30ae3c86_eventTypef_350730a644794cadc5c69c04e30ae3c86_inputTypef_450730a644794cadc5c69c04e30ae3c86_preventDefaultf_550730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_650730a644794cadc5c69c04e30ae3c86_stopPropagationf_750730a644794cadc5c69c04e30ae3c86_targetf_850730a644794cadc5c69c04e30ae3c86_timeStampf_950730a644794cadc5c69c04e30ae3c86.inspect()(Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d)(__apMtdDicts__(Inspect.Record_valuef_050730a644794cadc5c69c04e30ae3c86, [Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d]))(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(a0) + `)`;
  }
  else if (__x__.__constructor === "KeyboardEvent" && true) {
    let a0 = __x__.__args[0];
    return `KeyboardEvent(` + Inspect.Record_altKeyf_050730a644794cadc5c69c04e30ae3c86_bubblesf_150730a644794cadc5c69c04e30ae3c86_ctrlKeyf_250730a644794cadc5c69c04e30ae3c86_defaultPreventedf_350730a644794cadc5c69c04e30ae3c86_eventTypef_450730a644794cadc5c69c04e30ae3c86_keyf_550730a644794cadc5c69c04e30ae3c86_preventDefaultf_650730a644794cadc5c69c04e30ae3c86_shiftKeyf_750730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_850730a644794cadc5c69c04e30ae3c86_stopPropagationf_950730a644794cadc5c69c04e30ae3c86_timeStampf_1050730a644794cadc5c69c04e30ae3c86.inspect()(Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.a_arr_b)(Inspect.Key_29c16bbb90cf3a28d74dcca5983dc9c2)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(a0) + `)`;
  }
  else if (__x__.__constructor === "PopStateEvent" && true) {
    let a0 = __x__.__args[0];
    return `PopStateEvent(` + Inspect.Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_defaultPreventedf_150730a644794cadc5c69c04e30ae3c86_eventTypef_250730a644794cadc5c69c04e30ae3c86_preventDefaultf_350730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_450730a644794cadc5c69c04e30ae3c86_stopPropagationf_550730a644794cadc5c69c04e30ae3c86_timeStampf_650730a644794cadc5c69c04e30ae3c86_urlf_750730a644794cadc5c69c04e30ae3c86.inspect()(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(a0) + `)`;
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
Inspect['Record_altKeyf_050730a644794cadc5c69c04e30ae3c86_bubblesf_150730a644794cadc5c69c04e30ae3c86_ctrlKeyf_250730a644794cadc5c69c04e30ae3c86_defaultPreventedf_350730a644794cadc5c69c04e30ae3c86_eventTypef_450730a644794cadc5c69c04e30ae3c86_keyf_550730a644794cadc5c69c04e30ae3c86_preventDefaultf_650730a644794cadc5c69c04e30ae3c86_shiftKeyf_750730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_850730a644794cadc5c69c04e30ae3c86_stopPropagationf_950730a644794cadc5c69c04e30ae3c86_timeStampf_1050730a644794cadc5c69c04e30ae3c86'] = {};
Inspect['Record_altKeyf_050730a644794cadc5c69c04e30ae3c86_bubblesf_150730a644794cadc5c69c04e30ae3c86_ctrlKeyf_250730a644794cadc5c69c04e30ae3c86_defaultPreventedf_350730a644794cadc5c69c04e30ae3c86_eventTypef_450730a644794cadc5c69c04e30ae3c86_keyf_550730a644794cadc5c69c04e30ae3c86_preventDefaultf_650730a644794cadc5c69c04e30ae3c86_shiftKeyf_750730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_850730a644794cadc5c69c04e30ae3c86_stopPropagationf_950730a644794cadc5c69c04e30ae3c86_timeStampf_1050730a644794cadc5c69c04e30ae3c86']['inspect'] = () => (Inspect_l999) => (Inspect_e992) => (Inspect_x985) => (Inspect_q978) => (Inspect_j971) => (Inspect_c964) => (Inspect_v957) => (Inspect_o950) => (Inspect_h943) => (Inspect_a936) => (Inspect_t929) => (__$a__ => `{ ` + `altKey: ` + Inspect_t929.inspect()(__$a__.altKey) + `, ` + `bubbles: ` + Inspect_a936.inspect()(__$a__.bubbles) + `, ` + `ctrlKey: ` + Inspect_h943.inspect()(__$a__.ctrlKey) + `, ` + `defaultPrevented: ` + Inspect_o950.inspect()(__$a__.defaultPrevented) + `, ` + `eventType: ` + Inspect_v957.inspect()(__$a__.eventType) + `, ` + `key: ` + Inspect_c964.inspect()(__$a__.key) + `, ` + `preventDefault: ` + Inspect_j971.inspect()(__$a__.preventDefault) + `, ` + `shiftKey: ` + Inspect_q978.inspect()(__$a__.shiftKey) + `, ` + `stopImmediatePropagation: ` + Inspect_x985.inspect()(__$a__.stopImmediatePropagation) + `, ` + `stopPropagation: ` + Inspect_e992.inspect()(__$a__.stopPropagation) + `, ` + `timeStamp: ` + Inspect_l999.inspect()(__$a__.timeStamp) + ` }`);
Inspect['Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_dataf_150730a644794cadc5c69c04e30ae3c86_defaultPreventedf_250730a644794cadc5c69c04e30ae3c86_eventTypef_350730a644794cadc5c69c04e30ae3c86_inputTypef_450730a644794cadc5c69c04e30ae3c86_preventDefaultf_550730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_650730a644794cadc5c69c04e30ae3c86_stopPropagationf_750730a644794cadc5c69c04e30ae3c86_targetf_850730a644794cadc5c69c04e30ae3c86_timeStampf_950730a644794cadc5c69c04e30ae3c86'] = {};
Inspect['Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_dataf_150730a644794cadc5c69c04e30ae3c86_defaultPreventedf_250730a644794cadc5c69c04e30ae3c86_eventTypef_350730a644794cadc5c69c04e30ae3c86_inputTypef_450730a644794cadc5c69c04e30ae3c86_preventDefaultf_550730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_650730a644794cadc5c69c04e30ae3c86_stopPropagationf_750730a644794cadc5c69c04e30ae3c86_targetf_850730a644794cadc5c69c04e30ae3c86_timeStampf_950730a644794cadc5c69c04e30ae3c86']['inspect'] = () => (Inspect_q1082) => (Inspect_j1075) => (Inspect_c1068) => (Inspect_v1061) => (Inspect_o1054) => (Inspect_h1047) => (Inspect_a1040) => (Inspect_t1033) => (Inspect_m1026) => (Inspect_f1019) => (__$a__ => `{ ` + `bubbles: ` + Inspect_f1019.inspect()(__$a__.bubbles) + `, ` + `data: ` + Inspect_m1026.inspect()(__$a__.data) + `, ` + `defaultPrevented: ` + Inspect_t1033.inspect()(__$a__.defaultPrevented) + `, ` + `eventType: ` + Inspect_a1040.inspect()(__$a__.eventType) + `, ` + `inputType: ` + Inspect_h1047.inspect()(__$a__.inputType) + `, ` + `preventDefault: ` + Inspect_o1054.inspect()(__$a__.preventDefault) + `, ` + `stopImmediatePropagation: ` + Inspect_v1061.inspect()(__$a__.stopImmediatePropagation) + `, ` + `stopPropagation: ` + Inspect_c1068.inspect()(__$a__.stopPropagation) + `, ` + `target: ` + Inspect_j1075.inspect()(__$a__.target) + `, ` + `timeStamp: ` + Inspect_q1082.inspect()(__$a__.timeStamp) + ` }`);
Inspect['Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_defaultPreventedf_150730a644794cadc5c69c04e30ae3c86_eventTypef_250730a644794cadc5c69c04e30ae3c86_preventDefaultf_350730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_450730a644794cadc5c69c04e30ae3c86_stopPropagationf_550730a644794cadc5c69c04e30ae3c86_timeStampf_650730a644794cadc5c69c04e30ae3c86_urlf_750730a644794cadc5c69c04e30ae3c86'] = {};
Inspect['Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_defaultPreventedf_150730a644794cadc5c69c04e30ae3c86_eventTypef_250730a644794cadc5c69c04e30ae3c86_preventDefaultf_350730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_450730a644794cadc5c69c04e30ae3c86_stopPropagationf_550730a644794cadc5c69c04e30ae3c86_timeStampf_650730a644794cadc5c69c04e30ae3c86_urlf_750730a644794cadc5c69c04e30ae3c86']['inspect'] = () => (Inspect_g1150) => (Inspect_z1143) => (Inspect_s1136) => (Inspect_l1129) => (Inspect_e1122) => (Inspect_x1115) => (Inspect_q1108) => (Inspect_j1101) => (__$a__ => `{ ` + `bubbles: ` + Inspect_j1101.inspect()(__$a__.bubbles) + `, ` + `defaultPrevented: ` + Inspect_q1108.inspect()(__$a__.defaultPrevented) + `, ` + `eventType: ` + Inspect_x1115.inspect()(__$a__.eventType) + `, ` + `preventDefault: ` + Inspect_e1122.inspect()(__$a__.preventDefault) + `, ` + `stopImmediatePropagation: ` + Inspect_l1129.inspect()(__$a__.stopImmediatePropagation) + `, ` + `stopPropagation: ` + Inspect_s1136.inspect()(__$a__.stopPropagation) + `, ` + `timeStamp: ` + Inspect_z1143.inspect()(__$a__.timeStamp) + `, ` + `url: ` + Inspect_g1150.inspect()(__$a__.url) + ` }`);
export let buildKeyboardEvent = (e => {
    let k = getKeyFromCode(e.keyCode);
    return KeyboardEvent(({ bubbles: e.bubbles, defaultPrevented: e.defaultPrevented, preventDefault: e.preventDefault, stopImmediatePropagation: e.stopImmediatePropagation, stopPropagation: e.stopPropagation, timeStamp: e.timeStamp, eventType: e.eventType, key: k, altKey: e.altKey, ctrlKey: e.ctrlKey, shiftKey: e.shiftKey }));
});
export let buildInputEvent = (e => InputEvent(({ bubbles: e.bubbles, defaultPrevented: e.defaultPrevented, preventDefault: e.preventDefault, stopImmediatePropagation: e.stopImmediatePropagation, stopPropagation: e.stopPropagation, timeStamp: e.timeStamp, eventType: e.eventType, target: e.target, data: e.data, inputType: e.inputType })));
export let buildMouseEvent = (e => MouseEvent(({ bubbles: e.bubbles, defaultPrevented: e.defaultPrevented, preventDefault: e.preventDefault, stopImmediatePropagation: e.stopImmediatePropagation, stopPropagation: e.stopPropagation, timeStamp: e.timeStamp, eventType: e.eventType })));
export let buildAbstractEvent = (e => AbstractEvent(({ bubbles: e.bubbles, defaultPrevented: e.defaultPrevented, preventDefault: e.preventDefault, stopImmediatePropagation: e.stopImmediatePropagation, stopPropagation: e.stopPropagation, timeStamp: e.timeStamp, eventType: e.eventType })));
export let buildPopStateEvent = (e => PopStateEvent(({ url:  document.location.hash.substr(1) || "/" , bubbles: e.bubbles, defaultPrevented: e.defaultPrevented, preventDefault: e.preventDefault, stopImmediatePropagation: e.stopImmediatePropagation, stopPropagation: e.stopPropagation, timeStamp: e.timeStamp, eventType: e.eventType })));
export let EventConstructors =  Object.freeze({
  abort: buildAbstractEvent,
  afterprint: buildAbstractEvent,
  beforeprint: buildAbstractEvent,
  beforeunload: buildAbstractEvent,
  blur: buildAbstractEvent,
  canplay: buildAbstractEvent,
  canplaythrough: buildAbstractEvent,
  change: buildAbstractEvent,
  click: buildMouseEvent,
  contextmenu: buildAbstractEvent,
  copy: buildAbstractEvent,
  cuechange: buildAbstractEvent,
  cut: buildAbstractEvent,
  dblclick: buildMouseEvent,
  drag: buildAbstractEvent,
  dragend: buildAbstractEvent,
  dragenter: buildAbstractEvent,
  dragleave: buildAbstractEvent,
  dragover: buildAbstractEvent,
  dragstart: buildAbstractEvent,
  drop: buildAbstractEvent,
  durationchange: buildAbstractEvent,
  emptied: buildAbstractEvent,
  ended: buildAbstractEvent,
  error: buildAbstractEvent,
  focus: buildAbstractEvent,
  input: buildInputEvent,
  invalid: buildAbstractEvent,
  keydown: buildKeyboardEvent,
  keypress: buildKeyboardEvent,
  keyup: buildKeyboardEvent,
  load: buildAbstractEvent,
  loadeddata: buildAbstractEvent,
  loadedmetadata: buildAbstractEvent,
  loadstart: buildAbstractEvent,
  mousedown: buildMouseEvent,
  mouseenter: buildMouseEvent,
  mouseleave: buildMouseEvent,
  mousemove: buildMouseEvent,
  mouseout: buildMouseEvent,
  mouseover: buildMouseEvent,
  mouseup: buildMouseEvent,
  mousewheel: buildMouseEvent,
  offline: buildAbstractEvent,
  online: buildAbstractEvent,
  pagehide: buildAbstractEvent,
  pageshow: buildAbstractEvent,
  paste: buildAbstractEvent,
  pause: buildAbstractEvent,
  play: buildAbstractEvent,
  playing: buildAbstractEvent,
  popstate: buildPopStateEvent,
  progress: buildAbstractEvent,
  ratechange: buildAbstractEvent,
  reset: buildAbstractEvent,
  resize: buildAbstractEvent,
  scroll: buildAbstractEvent,
  search: buildAbstractEvent,
  seeked: buildAbstractEvent,
  seeking: buildAbstractEvent,
  select: buildAbstractEvent,
  stalled: buildAbstractEvent,
  storage: buildAbstractEvent,
  submit: buildAbstractEvent,
  suspend: buildAbstractEvent,
  timeupdate: buildAbstractEvent,
  toggle: buildAbstractEvent,
  transitioncancel: buildAbstractEvent,
  transitionend: buildAbstractEvent,
  transitionrun: buildAbstractEvent,
  transitionstart: buildAbstractEvent,
  unload: buildAbstractEvent,
  volumechange: buildAbstractEvent,
  waiting: buildAbstractEvent,
  wheel: buildAbstractEvent,
})
;
export default { buildKeyboardEvent, buildInputEvent, buildMouseEvent, buildAbstractEvent, buildPopStateEvent, EventConstructors, AbstractEvent, MouseEvent, InputEvent, KeyboardEvent, PopStateEvent };
