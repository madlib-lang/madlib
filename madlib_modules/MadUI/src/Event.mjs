// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadUI/src/Event.mad
import {} from "./../../../__internals__.mjs"
import { getKeyFromCode } from "./Key.mjs";

export let AbstractEvent = (a => ({ __constructor: "AbstractEvent", __args: [ a ] }));
export let MouseEvent = (a => ({ __constructor: "MouseEvent", __args: [ a ] }));
export let InputEvent = (a => ({ __constructor: "InputEvent", __args: [ a ] }));
export let KeyboardEvent = (a => ({ __constructor: "KeyboardEvent", __args: [ a ] }));
export let PopStateEvent = (a => ({ __constructor: "PopStateEvent", __args: [ a ] }));
Inspect['Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_defaultPreventedf_150730a644794cadc5c69c04e30ae3c86_eventTypef_250730a644794cadc5c69c04e30ae3c86_preventDefaultf_350730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_450730a644794cadc5c69c04e30ae3c86_stopPropagationf_550730a644794cadc5c69c04e30ae3c86_timeStampf_650730a644794cadc5c69c04e30ae3c86'] = {};
Inspect['Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_defaultPreventedf_150730a644794cadc5c69c04e30ae3c86_eventTypef_250730a644794cadc5c69c04e30ae3c86_preventDefaultf_350730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_450730a644794cadc5c69c04e30ae3c86_stopPropagationf_550730a644794cadc5c69c04e30ae3c86_timeStampf_650730a644794cadc5c69c04e30ae3c86']['inspect'] = () => (Inspect_e1044) => (Inspect_x1037) => (Inspect_q1030) => (Inspect_j1023) => (Inspect_c1016) => (Inspect_v1009) => (Inspect_o1002) => (__$a__ => `{ ` + `bubbles: ` + Inspect_o1002.inspect()(__$a__.bubbles) + `, ` + `defaultPrevented: ` + Inspect_v1009.inspect()(__$a__.defaultPrevented) + `, ` + `eventType: ` + Inspect_c1016.inspect()(__$a__.eventType) + `, ` + `preventDefault: ` + Inspect_j1023.inspect()(__$a__.preventDefault) + `, ` + `stopImmediatePropagation: ` + Inspect_q1030.inspect()(__$a__.stopImmediatePropagation) + `, ` + `stopPropagation: ` + Inspect_x1037.inspect()(__$a__.stopPropagation) + `, ` + `timeStamp: ` + Inspect_e1044.inspect()(__$a__.timeStamp) + ` }`);
Inspect['Record_valuef_050730a644794cadc5c69c04e30ae3c86'] = {};
Inspect['Record_valuef_050730a644794cadc5c69c04e30ae3c86']['inspect'] = () => (Inspect_u1060) => (__$a__ => `{ ` + `value: ` + Inspect_u1060.inspect()(__$a__.value) + ` }`);
Inspect['Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_dataf_150730a644794cadc5c69c04e30ae3c86_defaultPreventedf_250730a644794cadc5c69c04e30ae3c86_eventTypef_350730a644794cadc5c69c04e30ae3c86_inputTypef_450730a644794cadc5c69c04e30ae3c86_preventDefaultf_550730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_650730a644794cadc5c69c04e30ae3c86_stopPropagationf_750730a644794cadc5c69c04e30ae3c86_targetf_850730a644794cadc5c69c04e30ae3c86_timeStampf_950730a644794cadc5c69c04e30ae3c86'] = {};
Inspect['Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_dataf_150730a644794cadc5c69c04e30ae3c86_defaultPreventedf_250730a644794cadc5c69c04e30ae3c86_eventTypef_350730a644794cadc5c69c04e30ae3c86_inputTypef_450730a644794cadc5c69c04e30ae3c86_preventDefaultf_550730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_650730a644794cadc5c69c04e30ae3c86_stopPropagationf_750730a644794cadc5c69c04e30ae3c86_targetf_850730a644794cadc5c69c04e30ae3c86_timeStampf_950730a644794cadc5c69c04e30ae3c86']['inspect'] = () => (Inspect_p1133) => (Inspect_i1126) => (Inspect_b1119) => (Inspect_u1112) => (Inspect_n1105) => (Inspect_g1098) => (Inspect_z1091) => (Inspect_s1084) => (Inspect_l1077) => (Inspect_e1070) => (__$a__ => `{ ` + `bubbles: ` + Inspect_e1070.inspect()(__$a__.bubbles) + `, ` + `data: ` + Inspect_l1077.inspect()(__$a__.data) + `, ` + `defaultPrevented: ` + Inspect_s1084.inspect()(__$a__.defaultPrevented) + `, ` + `eventType: ` + Inspect_z1091.inspect()(__$a__.eventType) + `, ` + `inputType: ` + Inspect_g1098.inspect()(__$a__.inputType) + `, ` + `preventDefault: ` + Inspect_n1105.inspect()(__$a__.preventDefault) + `, ` + `stopImmediatePropagation: ` + Inspect_u1112.inspect()(__$a__.stopImmediatePropagation) + `, ` + `stopPropagation: ` + Inspect_b1119.inspect()(__$a__.stopPropagation) + `, ` + `target: ` + Inspect_i1126.inspect()(__$a__.target) + `, ` + `timeStamp: ` + Inspect_p1133.inspect()(__$a__.timeStamp) + ` }`);
Inspect['Record_altKeyf_050730a644794cadc5c69c04e30ae3c86_bubblesf_150730a644794cadc5c69c04e30ae3c86_ctrlKeyf_250730a644794cadc5c69c04e30ae3c86_defaultPreventedf_350730a644794cadc5c69c04e30ae3c86_eventTypef_450730a644794cadc5c69c04e30ae3c86_keyf_550730a644794cadc5c69c04e30ae3c86_preventDefaultf_650730a644794cadc5c69c04e30ae3c86_shiftKeyf_750730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_850730a644794cadc5c69c04e30ae3c86_stopPropagationf_950730a644794cadc5c69c04e30ae3c86_timeStampf_1050730a644794cadc5c69c04e30ae3c86'] = {};
Inspect['Record_altKeyf_050730a644794cadc5c69c04e30ae3c86_bubblesf_150730a644794cadc5c69c04e30ae3c86_ctrlKeyf_250730a644794cadc5c69c04e30ae3c86_defaultPreventedf_350730a644794cadc5c69c04e30ae3c86_eventTypef_450730a644794cadc5c69c04e30ae3c86_keyf_550730a644794cadc5c69c04e30ae3c86_preventDefaultf_650730a644794cadc5c69c04e30ae3c86_shiftKeyf_750730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_850730a644794cadc5c69c04e30ae3c86_stopPropagationf_950730a644794cadc5c69c04e30ae3c86_timeStampf_1050730a644794cadc5c69c04e30ae3c86']['inspect'] = () => (Inspect_a1222) => (Inspect_t1215) => (Inspect_m1208) => (Inspect_f1201) => (Inspect_y1194) => (Inspect_r1187) => (Inspect_k1180) => (Inspect_d1173) => (Inspect_w1166) => (Inspect_p1159) => (Inspect_i1152) => (__$a__ => `{ ` + `altKey: ` + Inspect_i1152.inspect()(__$a__.altKey) + `, ` + `bubbles: ` + Inspect_p1159.inspect()(__$a__.bubbles) + `, ` + `ctrlKey: ` + Inspect_w1166.inspect()(__$a__.ctrlKey) + `, ` + `defaultPrevented: ` + Inspect_d1173.inspect()(__$a__.defaultPrevented) + `, ` + `eventType: ` + Inspect_k1180.inspect()(__$a__.eventType) + `, ` + `key: ` + Inspect_r1187.inspect()(__$a__.key) + `, ` + `preventDefault: ` + Inspect_y1194.inspect()(__$a__.preventDefault) + `, ` + `shiftKey: ` + Inspect_f1201.inspect()(__$a__.shiftKey) + `, ` + `stopImmediatePropagation: ` + Inspect_m1208.inspect()(__$a__.stopImmediatePropagation) + `, ` + `stopPropagation: ` + Inspect_t1215.inspect()(__$a__.stopPropagation) + `, ` + `timeStamp: ` + Inspect_a1222.inspect()(__$a__.timeStamp) + ` }`);
Inspect['Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_defaultPreventedf_150730a644794cadc5c69c04e30ae3c86_eventTypef_250730a644794cadc5c69c04e30ae3c86_preventDefaultf_350730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_450730a644794cadc5c69c04e30ae3c86_stopPropagationf_550730a644794cadc5c69c04e30ae3c86_timeStampf_650730a644794cadc5c69c04e30ae3c86_urlf_750730a644794cadc5c69c04e30ae3c86'] = {};
Inspect['Record_bubblesf_050730a644794cadc5c69c04e30ae3c86_defaultPreventedf_150730a644794cadc5c69c04e30ae3c86_eventTypef_250730a644794cadc5c69c04e30ae3c86_preventDefaultf_350730a644794cadc5c69c04e30ae3c86_stopImmediatePropagationf_450730a644794cadc5c69c04e30ae3c86_stopPropagationf_550730a644794cadc5c69c04e30ae3c86_timeStampf_650730a644794cadc5c69c04e30ae3c86_urlf_750730a644794cadc5c69c04e30ae3c86']['inspect'] = () => (Inspect_r1291) => (Inspect_k1284) => (Inspect_d1277) => (Inspect_w1270) => (Inspect_p1263) => (Inspect_i1256) => (Inspect_b1249) => (Inspect_u1242) => (__$a__ => `{ ` + `bubbles: ` + Inspect_u1242.inspect()(__$a__.bubbles) + `, ` + `defaultPrevented: ` + Inspect_b1249.inspect()(__$a__.defaultPrevented) + `, ` + `eventType: ` + Inspect_i1256.inspect()(__$a__.eventType) + `, ` + `preventDefault: ` + Inspect_p1263.inspect()(__$a__.preventDefault) + `, ` + `stopImmediatePropagation: ` + Inspect_w1270.inspect()(__$a__.stopImmediatePropagation) + `, ` + `stopPropagation: ` + Inspect_d1277.inspect()(__$a__.stopPropagation) + `, ` + `timeStamp: ` + Inspect_k1284.inspect()(__$a__.timeStamp) + `, ` + `url: ` + Inspect_r1291.inspect()(__$a__.url) + ` }`);
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
