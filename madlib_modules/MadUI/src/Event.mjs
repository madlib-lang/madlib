// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadUI/src/Event.mad
import {} from "./../../../__internals__.mjs"
import { getKeyFromCode } from "./Key.mjs";

export let AbstractEvent = (a => ({ __constructor: "AbstractEvent", __args: [ a ] }));
export let MouseEvent = (a => ({ __constructor: "MouseEvent", __args: [ a ] }));
export let InputEvent = (a => ({ __constructor: "InputEvent", __args: [ a ] }));
export let KeyboardEvent = (a => ({ __constructor: "KeyboardEvent", __args: [ a ] }));
export let PopStateEvent = (a => ({ __constructor: "PopStateEvent", __args: [ a ] }));
Inspect['Record_bubblesf_0_defaultPreventedf_1_eventTypef_2_preventDefaultf_3_stopImmediatePropagationf_4_stopPropagationf_5_timeStampf_6'] = {};
Inspect['Record_bubblesf_0_defaultPreventedf_1_eventTypef_2_preventDefaultf_3_stopImmediatePropagationf_4_stopPropagationf_5_timeStampf_6']['inspect'] = () => (Inspect_l869) => (Inspect_e862) => (Inspect_x855) => (Inspect_q848) => (Inspect_j841) => (Inspect_c834) => (Inspect_v827) => (__$a__ => `{ ` + `bubbles: ` + Inspect_v827.inspect()(__$a__.bubbles) + `, ` + `defaultPrevented: ` + Inspect_c834.inspect()(__$a__.defaultPrevented) + `, ` + `eventType: ` + Inspect_j841.inspect()(__$a__.eventType) + `, ` + `preventDefault: ` + Inspect_q848.inspect()(__$a__.preventDefault) + `, ` + `stopImmediatePropagation: ` + Inspect_x855.inspect()(__$a__.stopImmediatePropagation) + `, ` + `stopPropagation: ` + Inspect_e862.inspect()(__$a__.stopPropagation) + `, ` + `timeStamp: ` + Inspect_l869.inspect()(__$a__.timeStamp) + ` }`);
Inspect['Record_valuef_0'] = {};
Inspect['Record_valuef_0']['inspect'] = () => (Inspect_b885) => (__$a__ => `{ ` + `value: ` + Inspect_b885.inspect()(__$a__.value) + ` }`);
Inspect['Event_50730a644794cadc5c69c04e30ae3c86'] = {};
Inspect['Event_50730a644794cadc5c69c04e30ae3c86']['inspect'] = () => (__$a__ => ((__x__) => {
  if (__x__.__constructor === "AbstractEvent" && true) {
    let a0 = __x__.__args[0];
    return `AbstractEvent(` + Inspect.Record_bubblesf_0_defaultPreventedf_1_eventTypef_2_preventDefaultf_3_stopImmediatePropagationf_4_stopPropagationf_5_timeStampf_6.inspect()(Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(a0) + `)`;
  }
  else if (__x__.__constructor === "MouseEvent" && true) {
    let a0 = __x__.__args[0];
    return `MouseEvent(` + Inspect.Record_bubblesf_0_defaultPreventedf_1_eventTypef_2_preventDefaultf_3_stopImmediatePropagationf_4_stopPropagationf_5_timeStampf_6.inspect()(Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(a0) + `)`;
  }
  else if (__x__.__constructor === "InputEvent" && true) {
    let a0 = __x__.__args[0];
    return `InputEvent(` + Inspect.Record_bubblesf_0_dataf_1_defaultPreventedf_2_eventTypef_3_inputTypef_4_preventDefaultf_5_stopImmediatePropagationf_6_stopPropagationf_7_targetf_8_timeStampf_9.inspect()(Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d)(__apMtdDicts__(Inspect.Record_valuef_0, [Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d]))(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(a0) + `)`;
  }
  else if (__x__.__constructor === "KeyboardEvent" && true) {
    let a0 = __x__.__args[0];
    return `KeyboardEvent(` + Inspect.Record_altKeyf_0_bubblesf_1_ctrlKeyf_2_defaultPreventedf_3_eventTypef_4_keyf_5_preventDefaultf_6_shiftKeyf_7_stopImmediatePropagationf_8_stopPropagationf_9_timeStampf_10.inspect()(Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.a_arr_b)(Inspect.Key_29c16bbb90cf3a28d74dcca5983dc9c2)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(a0) + `)`;
  }
  else if (__x__.__constructor === "PopStateEvent" && true) {
    let a0 = __x__.__args[0];
    return `PopStateEvent(` + Inspect.Record_bubblesf_0_defaultPreventedf_1_eventTypef_2_preventDefaultf_3_stopImmediatePropagationf_4_stopPropagationf_5_timeStampf_6_urlf_7.inspect()(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.a_arr_b)(Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(Inspect.Boolean_5b7ebeeaa5acfe1eeea5a9e9845b152d)(a0) + `)`;
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
Inspect['Record_altKeyf_0_bubblesf_1_ctrlKeyf_2_defaultPreventedf_3_eventTypef_4_keyf_5_preventDefaultf_6_shiftKeyf_7_stopImmediatePropagationf_8_stopPropagationf_9_timeStampf_10'] = {};
Inspect['Record_altKeyf_0_bubblesf_1_ctrlKeyf_2_defaultPreventedf_3_eventTypef_4_keyf_5_preventDefaultf_6_shiftKeyf_7_stopImmediatePropagationf_8_stopPropagationf_9_timeStampf_10']['inspect'] = () => (Inspect_p1003) => (Inspect_i996) => (Inspect_b989) => (Inspect_u982) => (Inspect_n975) => (Inspect_g968) => (Inspect_z961) => (Inspect_s954) => (Inspect_l947) => (Inspect_e940) => (Inspect_x933) => (__$a__ => `{ ` + `altKey: ` + Inspect_x933.inspect()(__$a__.altKey) + `, ` + `bubbles: ` + Inspect_e940.inspect()(__$a__.bubbles) + `, ` + `ctrlKey: ` + Inspect_l947.inspect()(__$a__.ctrlKey) + `, ` + `defaultPrevented: ` + Inspect_s954.inspect()(__$a__.defaultPrevented) + `, ` + `eventType: ` + Inspect_z961.inspect()(__$a__.eventType) + `, ` + `key: ` + Inspect_g968.inspect()(__$a__.key) + `, ` + `preventDefault: ` + Inspect_n975.inspect()(__$a__.preventDefault) + `, ` + `shiftKey: ` + Inspect_u982.inspect()(__$a__.shiftKey) + `, ` + `stopImmediatePropagation: ` + Inspect_b989.inspect()(__$a__.stopImmediatePropagation) + `, ` + `stopPropagation: ` + Inspect_i996.inspect()(__$a__.stopPropagation) + `, ` + `timeStamp: ` + Inspect_p1003.inspect()(__$a__.timeStamp) + ` }`);
Inspect['Record_bubblesf_0_dataf_1_defaultPreventedf_2_eventTypef_3_inputTypef_4_preventDefaultf_5_stopImmediatePropagationf_6_stopPropagationf_7_targetf_8_timeStampf_9'] = {};
Inspect['Record_bubblesf_0_dataf_1_defaultPreventedf_2_eventTypef_3_inputTypef_4_preventDefaultf_5_stopImmediatePropagationf_6_stopPropagationf_7_targetf_8_timeStampf_9']['inspect'] = () => (Inspect_u1086) => (Inspect_n1079) => (Inspect_g1072) => (Inspect_z1065) => (Inspect_s1058) => (Inspect_l1051) => (Inspect_e1044) => (Inspect_x1037) => (Inspect_q1030) => (Inspect_j1023) => (__$a__ => `{ ` + `bubbles: ` + Inspect_j1023.inspect()(__$a__.bubbles) + `, ` + `data: ` + Inspect_q1030.inspect()(__$a__.data) + `, ` + `defaultPrevented: ` + Inspect_x1037.inspect()(__$a__.defaultPrevented) + `, ` + `eventType: ` + Inspect_e1044.inspect()(__$a__.eventType) + `, ` + `inputType: ` + Inspect_l1051.inspect()(__$a__.inputType) + `, ` + `preventDefault: ` + Inspect_s1058.inspect()(__$a__.preventDefault) + `, ` + `stopImmediatePropagation: ` + Inspect_z1065.inspect()(__$a__.stopImmediatePropagation) + `, ` + `stopPropagation: ` + Inspect_g1072.inspect()(__$a__.stopPropagation) + `, ` + `target: ` + Inspect_n1079.inspect()(__$a__.target) + `, ` + `timeStamp: ` + Inspect_u1086.inspect()(__$a__.timeStamp) + ` }`);
Inspect['Record_bubblesf_0_defaultPreventedf_1_eventTypef_2_preventDefaultf_3_stopImmediatePropagationf_4_stopPropagationf_5_timeStampf_6_urlf_7'] = {};
Inspect['Record_bubblesf_0_defaultPreventedf_1_eventTypef_2_preventDefaultf_3_stopImmediatePropagationf_4_stopPropagationf_5_timeStampf_6_urlf_7']['inspect'] = () => (Inspect_k1154) => (Inspect_d1147) => (Inspect_w1140) => (Inspect_p1133) => (Inspect_i1126) => (Inspect_b1119) => (Inspect_u1112) => (Inspect_n1105) => (__$a__ => `{ ` + `bubbles: ` + Inspect_n1105.inspect()(__$a__.bubbles) + `, ` + `defaultPrevented: ` + Inspect_u1112.inspect()(__$a__.defaultPrevented) + `, ` + `eventType: ` + Inspect_b1119.inspect()(__$a__.eventType) + `, ` + `preventDefault: ` + Inspect_i1126.inspect()(__$a__.preventDefault) + `, ` + `stopImmediatePropagation: ` + Inspect_p1133.inspect()(__$a__.stopImmediatePropagation) + `, ` + `stopPropagation: ` + Inspect_w1140.inspect()(__$a__.stopPropagation) + `, ` + `timeStamp: ` + Inspect_d1147.inspect()(__$a__.timeStamp) + `, ` + `url: ` + Inspect_k1154.inspect()(__$a__.url) + ` }`);
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
