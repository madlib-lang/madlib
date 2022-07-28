// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadUI/src/Key.mad
import {} from "./../../../__internals__.mjs"
import { fromMaybe } from "./../../../.prelude/Maybe.mjs";
import { fromList, get } from "./../../../.prelude/Dictionary.mjs";
import {  } from "./../../../.prelude/Number.mjs";

export let KEY_ANY = ({ __constructor: "KEY_ANY", __args: [  ] });
export let KEY_BREAK = ({ __constructor: "KEY_BREAK", __args: [  ] });
export let KEY_BACKSPACE = ({ __constructor: "KEY_BACKSPACE", __args: [  ] });
export let KEY_TAB = ({ __constructor: "KEY_TAB", __args: [  ] });
export let KEY_CLEAR = ({ __constructor: "KEY_CLEAR", __args: [  ] });
export let KEY_ENTER = ({ __constructor: "KEY_ENTER", __args: [  ] });
export let KEY_SHIFT = ({ __constructor: "KEY_SHIFT", __args: [  ] });
export let KEY_CTRL = ({ __constructor: "KEY_CTRL", __args: [  ] });
export let KEY_ALT = ({ __constructor: "KEY_ALT", __args: [  ] });
export let KEY_PAUSE = ({ __constructor: "KEY_PAUSE", __args: [  ] });
export let KEY_CAPS_LOCK = ({ __constructor: "KEY_CAPS_LOCK", __args: [  ] });
export let KEY_HANGUL = ({ __constructor: "KEY_HANGUL", __args: [  ] });
export let KEY_HANJA = ({ __constructor: "KEY_HANJA", __args: [  ] });
export let KEY_ESCAPE = ({ __constructor: "KEY_ESCAPE", __args: [  ] });
export let KEY_CONVERSION = ({ __constructor: "KEY_CONVERSION", __args: [  ] });
export let KEY_NON_CONVERSION = ({ __constructor: "KEY_NON_CONVERSION", __args: [  ] });
export let KEY_SPACE = ({ __constructor: "KEY_SPACE", __args: [  ] });
export let KEY_PAGE_UP = ({ __constructor: "KEY_PAGE_UP", __args: [  ] });
export let KEY_PAGE_DOWN = ({ __constructor: "KEY_PAGE_DOWN", __args: [  ] });
export let KEY_END = ({ __constructor: "KEY_END", __args: [  ] });
export let KEY_HOME = ({ __constructor: "KEY_HOME", __args: [  ] });
export let KEY_LEFT_ARROW = ({ __constructor: "KEY_LEFT_ARROW", __args: [  ] });
export let KEY_UP_ARROW = ({ __constructor: "KEY_UP_ARROW", __args: [  ] });
export let KEY_RIGHT_ARROW = ({ __constructor: "KEY_RIGHT_ARROW", __args: [  ] });
export let KEY_DOWN_ARROW = ({ __constructor: "KEY_DOWN_ARROW", __args: [  ] });
export let KEY_SELECT = ({ __constructor: "KEY_SELECT", __args: [  ] });
export let KEY_PRINT = ({ __constructor: "KEY_PRINT", __args: [  ] });
export let KEY_EXECUTE = ({ __constructor: "KEY_EXECUTE", __args: [  ] });
export let KEY_PRINT_SCREEN = ({ __constructor: "KEY_PRINT_SCREEN", __args: [  ] });
export let KEY_INSERT = ({ __constructor: "KEY_INSERT", __args: [  ] });
export let KEY_DELETE = ({ __constructor: "KEY_DELETE", __args: [  ] });
export let KEY_HELP = ({ __constructor: "KEY_HELP", __args: [  ] });
export let KEY_0 = ({ __constructor: "KEY_0", __args: [  ] });
export let KEY_1 = ({ __constructor: "KEY_1", __args: [  ] });
export let KEY_2 = ({ __constructor: "KEY_2", __args: [  ] });
export let KEY_3 = ({ __constructor: "KEY_3", __args: [  ] });
export let KEY_4 = ({ __constructor: "KEY_4", __args: [  ] });
export let KEY_5 = ({ __constructor: "KEY_5", __args: [  ] });
export let KEY_6 = ({ __constructor: "KEY_6", __args: [  ] });
export let KEY_7 = ({ __constructor: "KEY_7", __args: [  ] });
export let KEY_8 = ({ __constructor: "KEY_8", __args: [  ] });
export let KEY_9 = ({ __constructor: "KEY_9", __args: [  ] });
export let KEY_COLON = ({ __constructor: "KEY_COLON", __args: [  ] });
export let KEY_LEFT_CHEVRON = ({ __constructor: "KEY_LEFT_CHEVRON", __args: [  ] });
export let KEY_EQUAL = ({ __constructor: "KEY_EQUAL", __args: [  ] });
export let KEY_ESZETT = ({ __constructor: "KEY_ESZETT", __args: [  ] });
export let KEY_AT = ({ __constructor: "KEY_AT", __args: [  ] });
export let KEY_A = ({ __constructor: "KEY_A", __args: [  ] });
export let KEY_B = ({ __constructor: "KEY_B", __args: [  ] });
export let KEY_C = ({ __constructor: "KEY_C", __args: [  ] });
export let KEY_D = ({ __constructor: "KEY_D", __args: [  ] });
export let KEY_E = ({ __constructor: "KEY_E", __args: [  ] });
export let KEY_F = ({ __constructor: "KEY_F", __args: [  ] });
export let KEY_G = ({ __constructor: "KEY_G", __args: [  ] });
export let KEY_H = ({ __constructor: "KEY_H", __args: [  ] });
export let KEY_I = ({ __constructor: "KEY_I", __args: [  ] });
export let KEY_J = ({ __constructor: "KEY_J", __args: [  ] });
export let KEY_K = ({ __constructor: "KEY_K", __args: [  ] });
export let KEY_L = ({ __constructor: "KEY_L", __args: [  ] });
export let KEY_M = ({ __constructor: "KEY_M", __args: [  ] });
export let KEY_N = ({ __constructor: "KEY_N", __args: [  ] });
export let KEY_O = ({ __constructor: "KEY_O", __args: [  ] });
export let KEY_P = ({ __constructor: "KEY_P", __args: [  ] });
export let KEY_Q = ({ __constructor: "KEY_Q", __args: [  ] });
export let KEY_R = ({ __constructor: "KEY_R", __args: [  ] });
export let KEY_S = ({ __constructor: "KEY_S", __args: [  ] });
export let KEY_T = ({ __constructor: "KEY_T", __args: [  ] });
export let KEY_U = ({ __constructor: "KEY_U", __args: [  ] });
export let KEY_V = ({ __constructor: "KEY_V", __args: [  ] });
export let KEY_W = ({ __constructor: "KEY_W", __args: [  ] });
export let KEY_X = ({ __constructor: "KEY_X", __args: [  ] });
export let KEY_Y = ({ __constructor: "KEY_Y", __args: [  ] });
export let KEY_Z = ({ __constructor: "KEY_Z", __args: [  ] });
export let KEY_CMD_LEFT = ({ __constructor: "KEY_CMD_LEFT", __args: [  ] });
export let KEY_CMD_RIGHT = ({ __constructor: "KEY_CMD_RIGHT", __args: [  ] });
export let KEY_SLEEP = ({ __constructor: "KEY_SLEEP", __args: [  ] });
export let KEY_NUMPAD_0 = ({ __constructor: "KEY_NUMPAD_0", __args: [  ] });
export let KEY_NUMPAD_1 = ({ __constructor: "KEY_NUMPAD_1", __args: [  ] });
export let KEY_NUMPAD_2 = ({ __constructor: "KEY_NUMPAD_2", __args: [  ] });
export let KEY_NUMPAD_3 = ({ __constructor: "KEY_NUMPAD_3", __args: [  ] });
export let KEY_NUMPAD_4 = ({ __constructor: "KEY_NUMPAD_4", __args: [  ] });
export let KEY_NUMPAD_5 = ({ __constructor: "KEY_NUMPAD_5", __args: [  ] });
export let KEY_NUMPAD_6 = ({ __constructor: "KEY_NUMPAD_6", __args: [  ] });
export let KEY_NUMPAD_7 = ({ __constructor: "KEY_NUMPAD_7", __args: [  ] });
export let KEY_NUMPAD_8 = ({ __constructor: "KEY_NUMPAD_8", __args: [  ] });
export let KEY_NUMPAD_9 = ({ __constructor: "KEY_NUMPAD_9", __args: [  ] });
export let KEY_MULTIPLY = ({ __constructor: "KEY_MULTIPLY", __args: [  ] });
export let KEY_ADD = ({ __constructor: "KEY_ADD", __args: [  ] });
export let KEY_NUMPAD_PERIOD = ({ __constructor: "KEY_NUMPAD_PERIOD", __args: [  ] });
export let KEY_SUBSTRACT = ({ __constructor: "KEY_SUBSTRACT", __args: [  ] });
export let KEY_DECIMAL_POINT = ({ __constructor: "KEY_DECIMAL_POINT", __args: [  ] });
export let KEY_DIVIDE = ({ __constructor: "KEY_DIVIDE", __args: [  ] });
export let KEY_F1 = ({ __constructor: "KEY_F1", __args: [  ] });
export let KEY_F2 = ({ __constructor: "KEY_F2", __args: [  ] });
export let KEY_F3 = ({ __constructor: "KEY_F3", __args: [  ] });
export let KEY_F4 = ({ __constructor: "KEY_F4", __args: [  ] });
export let KEY_F5 = ({ __constructor: "KEY_F5", __args: [  ] });
export let KEY_F6 = ({ __constructor: "KEY_F6", __args: [  ] });
export let KEY_F7 = ({ __constructor: "KEY_F7", __args: [  ] });
export let KEY_F8 = ({ __constructor: "KEY_F8", __args: [  ] });
export let KEY_F9 = ({ __constructor: "KEY_F9", __args: [  ] });
export let KEY_F10 = ({ __constructor: "KEY_F10", __args: [  ] });
export let KEY_F11 = ({ __constructor: "KEY_F11", __args: [  ] });
export let KEY_F12 = ({ __constructor: "KEY_F12", __args: [  ] });
export let KEY_F13 = ({ __constructor: "KEY_F13", __args: [  ] });
export let KEY_F14 = ({ __constructor: "KEY_F14", __args: [  ] });
export let KEY_F15 = ({ __constructor: "KEY_F15", __args: [  ] });
export let KEY_F16 = ({ __constructor: "KEY_F16", __args: [  ] });
export let KEY_F17 = ({ __constructor: "KEY_F17", __args: [  ] });
export let KEY_F18 = ({ __constructor: "KEY_F18", __args: [  ] });
export let KEY_F19 = ({ __constructor: "KEY_F19", __args: [  ] });
export let KEY_F20 = ({ __constructor: "KEY_F20", __args: [  ] });
export let KEY_F21 = ({ __constructor: "KEY_F21", __args: [  ] });
export let KEY_F22 = ({ __constructor: "KEY_F22", __args: [  ] });
export let KEY_F23 = ({ __constructor: "KEY_F23", __args: [  ] });
export let KEY_F24 = ({ __constructor: "KEY_F24", __args: [  ] });
export let KEY_F25 = ({ __constructor: "KEY_F25", __args: [  ] });
export let KEY_F26 = ({ __constructor: "KEY_F26", __args: [  ] });
export let KEY_F27 = ({ __constructor: "KEY_F27", __args: [  ] });
export let KEY_F28 = ({ __constructor: "KEY_F28", __args: [  ] });
export let KEY_F29 = ({ __constructor: "KEY_F29", __args: [  ] });
export let KEY_F30 = ({ __constructor: "KEY_F30", __args: [  ] });
export let KEY_F31 = ({ __constructor: "KEY_F31", __args: [  ] });
export let KEY_F32 = ({ __constructor: "KEY_F32", __args: [  ] });
export let KEY_NUM_LOCK = ({ __constructor: "KEY_NUM_LOCK", __args: [  ] });
export let KEY_SCROLL_LOCK = ({ __constructor: "KEY_SCROLL_LOCK", __args: [  ] });
export let KEY_AIRPLANE_MODE = ({ __constructor: "KEY_AIRPLANE_MODE", __args: [  ] });
export let KEY_CIRCONFLEX = ({ __constructor: "KEY_CIRCONFLEX", __args: [  ] });
export let KEY_EXCLAMATION_MARK = ({ __constructor: "KEY_EXCLAMATION_MARK", __args: [  ] });
export let KEY_ARABIC_SEMI_COLON = ({ __constructor: "KEY_ARABIC_SEMI_COLON", __args: [  ] });
export let KEY_NUMBER_SIGN = ({ __constructor: "KEY_NUMBER_SIGN", __args: [  ] });
export let KEY_DOLLAR = ({ __constructor: "KEY_DOLLAR", __args: [  ] });
export let KEY_U_GRAVE_ACCENT = ({ __constructor: "KEY_U_GRAVE_ACCENT", __args: [  ] });
export let KEY_PAGE_BACKWARD = ({ __constructor: "KEY_PAGE_BACKWARD", __args: [  ] });
export let KEY_PAGE_FORWARD = ({ __constructor: "KEY_PAGE_FORWARD", __args: [  ] });
export let KEY_REFRESH = ({ __constructor: "KEY_REFRESH", __args: [  ] });
export let KEY_RIGHT_PAREN = ({ __constructor: "KEY_RIGHT_PAREN", __args: [  ] });
export let KEY_ASTERISK = ({ __constructor: "KEY_ASTERISK", __args: [  ] });
export let KEY_TILDE = ({ __constructor: "KEY_TILDE", __args: [  ] });
export let KEY_MUTE = ({ __constructor: "KEY_MUTE", __args: [  ] });
export let KEY_NEXT = ({ __constructor: "KEY_NEXT", __args: [  ] });
export let KEY_PREVIOUS = ({ __constructor: "KEY_PREVIOUS", __args: [  ] });
export let KEY_STOP = ({ __constructor: "KEY_STOP", __args: [  ] });
export let KEY_PLAY_PAUSE = ({ __constructor: "KEY_PLAY_PAUSE", __args: [  ] });
export let KEY_EMAIL = ({ __constructor: "KEY_EMAIL", __args: [  ] });
export let KEY_MUTE_UNMUTE = ({ __constructor: "KEY_MUTE_UNMUTE", __args: [  ] });
export let KEY_DECREASE_VOLUME = ({ __constructor: "KEY_DECREASE_VOLUME", __args: [  ] });
export let KEY_INCREASE_VOLUME = ({ __constructor: "KEY_INCREASE_VOLUME", __args: [  ] });
export let KEY_SEMI_COLON = ({ __constructor: "KEY_SEMI_COLON", __args: [  ] });
export let KEY_COMMA = ({ __constructor: "KEY_COMMA", __args: [  ] });
export let KEY_DASH = ({ __constructor: "KEY_DASH", __args: [  ] });
export let KEY_PERIOD = ({ __constructor: "KEY_PERIOD", __args: [  ] });
export let KEY_FORWARD_SLASH = ({ __constructor: "KEY_FORWARD_SLASH", __args: [  ] });
export let KEY_GRAVE_ACCENT = ({ __constructor: "KEY_GRAVE_ACCENT", __args: [  ] });
export let KEY_QUESTION_MARK = ({ __constructor: "KEY_QUESTION_MARK", __args: [  ] });
export let KEY_BRACKET_LEFT = ({ __constructor: "KEY_BRACKET_LEFT", __args: [  ] });
export let KEY_BACK_SLASH = ({ __constructor: "KEY_BACK_SLASH", __args: [  ] });
export let KEY_BRACKET_RIGHT = ({ __constructor: "KEY_BRACKET_RIGHT", __args: [  ] });
export let KEY_SINGLE_QUOTE = ({ __constructor: "KEY_SINGLE_QUOTE", __args: [  ] });
export let KEY_BACK_TICK = ({ __constructor: "KEY_BACK_TICK", __args: [  ] });
export let KEY_CMD = ({ __constructor: "KEY_CMD", __args: [  ] });
export let KEY_ALTGR = ({ __constructor: "KEY_ALTGR", __args: [  ] });
export let KEY_LEFT_BACK_SLASH = ({ __constructor: "KEY_LEFT_BACK_SLASH", __args: [  ] });
export let KEY_GNOME_COMPOSE = ({ __constructor: "KEY_GNOME_COMPOSE", __args: [  ] });
export let KEY_C_CEDILLA = ({ __constructor: "KEY_C_CEDILLA", __args: [  ] });
export let KEY_XF86_FORWARD = ({ __constructor: "KEY_XF86_FORWARD", __args: [  ] });
export let KEY_XF86_BACKWARD = ({ __constructor: "KEY_XF86_BACKWARD", __args: [  ] });
export let KEY_ALPHA_NUMERIC = ({ __constructor: "KEY_ALPHA_NUMERIC", __args: [  ] });
export let KEY_HIRAGANA_KATAKANA = ({ __constructor: "KEY_HIRAGANA_KATAKANA", __args: [  ] });
export let KEY_HALF_WIDTH_FULL_WIDTH = ({ __constructor: "KEY_HALF_WIDTH_FULL_WIDTH", __args: [  ] });
export let KEY_KANJI = ({ __constructor: "KEY_KANJI", __args: [  ] });
export let KEY_UNLOCK_TRACK_PAD = ({ __constructor: "KEY_UNLOCK_TRACK_PAD", __args: [  ] });
export let KEY_TOGGLE_TOUCH_PAD = ({ __constructor: "KEY_TOGGLE_TOUCH_PAD", __args: [  ] });
Inspect['Key_29c16bbb90cf3a28d74dcca5983dc9c2'] = {};
Inspect['Key_29c16bbb90cf3a28d74dcca5983dc9c2']['inspect'] = () => (__$a__ => ((__x__) => {
  if (__x__.__constructor === "KEY_ANY") {
    return `KEY_ANY`;
  }
  else if (__x__.__constructor === "KEY_BREAK") {
    return `KEY_BREAK`;
  }
  else if (__x__.__constructor === "KEY_BACKSPACE") {
    return `KEY_BACKSPACE`;
  }
  else if (__x__.__constructor === "KEY_TAB") {
    return `KEY_TAB`;
  }
  else if (__x__.__constructor === "KEY_CLEAR") {
    return `KEY_CLEAR`;
  }
  else if (__x__.__constructor === "KEY_ENTER") {
    return `KEY_ENTER`;
  }
  else if (__x__.__constructor === "KEY_SHIFT") {
    return `KEY_SHIFT`;
  }
  else if (__x__.__constructor === "KEY_CTRL") {
    return `KEY_CTRL`;
  }
  else if (__x__.__constructor === "KEY_ALT") {
    return `KEY_ALT`;
  }
  else if (__x__.__constructor === "KEY_PAUSE") {
    return `KEY_PAUSE`;
  }
  else if (__x__.__constructor === "KEY_CAPS_LOCK") {
    return `KEY_CAPS_LOCK`;
  }
  else if (__x__.__constructor === "KEY_HANGUL") {
    return `KEY_HANGUL`;
  }
  else if (__x__.__constructor === "KEY_HANJA") {
    return `KEY_HANJA`;
  }
  else if (__x__.__constructor === "KEY_ESCAPE") {
    return `KEY_ESCAPE`;
  }
  else if (__x__.__constructor === "KEY_CONVERSION") {
    return `KEY_CONVERSION`;
  }
  else if (__x__.__constructor === "KEY_NON_CONVERSION") {
    return `KEY_NON_CONVERSION`;
  }
  else if (__x__.__constructor === "KEY_SPACE") {
    return `KEY_SPACE`;
  }
  else if (__x__.__constructor === "KEY_PAGE_UP") {
    return `KEY_PAGE_UP`;
  }
  else if (__x__.__constructor === "KEY_PAGE_DOWN") {
    return `KEY_PAGE_DOWN`;
  }
  else if (__x__.__constructor === "KEY_END") {
    return `KEY_END`;
  }
  else if (__x__.__constructor === "KEY_HOME") {
    return `KEY_HOME`;
  }
  else if (__x__.__constructor === "KEY_LEFT_ARROW") {
    return `KEY_LEFT_ARROW`;
  }
  else if (__x__.__constructor === "KEY_UP_ARROW") {
    return `KEY_UP_ARROW`;
  }
  else if (__x__.__constructor === "KEY_RIGHT_ARROW") {
    return `KEY_RIGHT_ARROW`;
  }
  else if (__x__.__constructor === "KEY_DOWN_ARROW") {
    return `KEY_DOWN_ARROW`;
  }
  else if (__x__.__constructor === "KEY_SELECT") {
    return `KEY_SELECT`;
  }
  else if (__x__.__constructor === "KEY_PRINT") {
    return `KEY_PRINT`;
  }
  else if (__x__.__constructor === "KEY_EXECUTE") {
    return `KEY_EXECUTE`;
  }
  else if (__x__.__constructor === "KEY_PRINT_SCREEN") {
    return `KEY_PRINT_SCREEN`;
  }
  else if (__x__.__constructor === "KEY_INSERT") {
    return `KEY_INSERT`;
  }
  else if (__x__.__constructor === "KEY_DELETE") {
    return `KEY_DELETE`;
  }
  else if (__x__.__constructor === "KEY_HELP") {
    return `KEY_HELP`;
  }
  else if (__x__.__constructor === "KEY_0") {
    return `KEY_0`;
  }
  else if (__x__.__constructor === "KEY_1") {
    return `KEY_1`;
  }
  else if (__x__.__constructor === "KEY_2") {
    return `KEY_2`;
  }
  else if (__x__.__constructor === "KEY_3") {
    return `KEY_3`;
  }
  else if (__x__.__constructor === "KEY_4") {
    return `KEY_4`;
  }
  else if (__x__.__constructor === "KEY_5") {
    return `KEY_5`;
  }
  else if (__x__.__constructor === "KEY_6") {
    return `KEY_6`;
  }
  else if (__x__.__constructor === "KEY_7") {
    return `KEY_7`;
  }
  else if (__x__.__constructor === "KEY_8") {
    return `KEY_8`;
  }
  else if (__x__.__constructor === "KEY_9") {
    return `KEY_9`;
  }
  else if (__x__.__constructor === "KEY_COLON") {
    return `KEY_COLON`;
  }
  else if (__x__.__constructor === "KEY_LEFT_CHEVRON") {
    return `KEY_LEFT_CHEVRON`;
  }
  else if (__x__.__constructor === "KEY_EQUAL") {
    return `KEY_EQUAL`;
  }
  else if (__x__.__constructor === "KEY_ESZETT") {
    return `KEY_ESZETT`;
  }
  else if (__x__.__constructor === "KEY_AT") {
    return `KEY_AT`;
  }
  else if (__x__.__constructor === "KEY_A") {
    return `KEY_A`;
  }
  else if (__x__.__constructor === "KEY_B") {
    return `KEY_B`;
  }
  else if (__x__.__constructor === "KEY_C") {
    return `KEY_C`;
  }
  else if (__x__.__constructor === "KEY_D") {
    return `KEY_D`;
  }
  else if (__x__.__constructor === "KEY_E") {
    return `KEY_E`;
  }
  else if (__x__.__constructor === "KEY_F") {
    return `KEY_F`;
  }
  else if (__x__.__constructor === "KEY_G") {
    return `KEY_G`;
  }
  else if (__x__.__constructor === "KEY_H") {
    return `KEY_H`;
  }
  else if (__x__.__constructor === "KEY_I") {
    return `KEY_I`;
  }
  else if (__x__.__constructor === "KEY_J") {
    return `KEY_J`;
  }
  else if (__x__.__constructor === "KEY_K") {
    return `KEY_K`;
  }
  else if (__x__.__constructor === "KEY_L") {
    return `KEY_L`;
  }
  else if (__x__.__constructor === "KEY_M") {
    return `KEY_M`;
  }
  else if (__x__.__constructor === "KEY_N") {
    return `KEY_N`;
  }
  else if (__x__.__constructor === "KEY_O") {
    return `KEY_O`;
  }
  else if (__x__.__constructor === "KEY_P") {
    return `KEY_P`;
  }
  else if (__x__.__constructor === "KEY_Q") {
    return `KEY_Q`;
  }
  else if (__x__.__constructor === "KEY_R") {
    return `KEY_R`;
  }
  else if (__x__.__constructor === "KEY_S") {
    return `KEY_S`;
  }
  else if (__x__.__constructor === "KEY_T") {
    return `KEY_T`;
  }
  else if (__x__.__constructor === "KEY_U") {
    return `KEY_U`;
  }
  else if (__x__.__constructor === "KEY_V") {
    return `KEY_V`;
  }
  else if (__x__.__constructor === "KEY_W") {
    return `KEY_W`;
  }
  else if (__x__.__constructor === "KEY_X") {
    return `KEY_X`;
  }
  else if (__x__.__constructor === "KEY_Y") {
    return `KEY_Y`;
  }
  else if (__x__.__constructor === "KEY_Z") {
    return `KEY_Z`;
  }
  else if (__x__.__constructor === "KEY_CMD_LEFT") {
    return `KEY_CMD_LEFT`;
  }
  else if (__x__.__constructor === "KEY_CMD_RIGHT") {
    return `KEY_CMD_RIGHT`;
  }
  else if (__x__.__constructor === "KEY_SLEEP") {
    return `KEY_SLEEP`;
  }
  else if (__x__.__constructor === "KEY_NUMPAD_0") {
    return `KEY_NUMPAD_0`;
  }
  else if (__x__.__constructor === "KEY_NUMPAD_1") {
    return `KEY_NUMPAD_1`;
  }
  else if (__x__.__constructor === "KEY_NUMPAD_2") {
    return `KEY_NUMPAD_2`;
  }
  else if (__x__.__constructor === "KEY_NUMPAD_3") {
    return `KEY_NUMPAD_3`;
  }
  else if (__x__.__constructor === "KEY_NUMPAD_4") {
    return `KEY_NUMPAD_4`;
  }
  else if (__x__.__constructor === "KEY_NUMPAD_5") {
    return `KEY_NUMPAD_5`;
  }
  else if (__x__.__constructor === "KEY_NUMPAD_6") {
    return `KEY_NUMPAD_6`;
  }
  else if (__x__.__constructor === "KEY_NUMPAD_7") {
    return `KEY_NUMPAD_7`;
  }
  else if (__x__.__constructor === "KEY_NUMPAD_8") {
    return `KEY_NUMPAD_8`;
  }
  else if (__x__.__constructor === "KEY_NUMPAD_9") {
    return `KEY_NUMPAD_9`;
  }
  else if (__x__.__constructor === "KEY_MULTIPLY") {
    return `KEY_MULTIPLY`;
  }
  else if (__x__.__constructor === "KEY_ADD") {
    return `KEY_ADD`;
  }
  else if (__x__.__constructor === "KEY_NUMPAD_PERIOD") {
    return `KEY_NUMPAD_PERIOD`;
  }
  else if (__x__.__constructor === "KEY_SUBSTRACT") {
    return `KEY_SUBSTRACT`;
  }
  else if (__x__.__constructor === "KEY_DECIMAL_POINT") {
    return `KEY_DECIMAL_POINT`;
  }
  else if (__x__.__constructor === "KEY_DIVIDE") {
    return `KEY_DIVIDE`;
  }
  else if (__x__.__constructor === "KEY_F1") {
    return `KEY_F1`;
  }
  else if (__x__.__constructor === "KEY_F2") {
    return `KEY_F2`;
  }
  else if (__x__.__constructor === "KEY_F3") {
    return `KEY_F3`;
  }
  else if (__x__.__constructor === "KEY_F4") {
    return `KEY_F4`;
  }
  else if (__x__.__constructor === "KEY_F5") {
    return `KEY_F5`;
  }
  else if (__x__.__constructor === "KEY_F6") {
    return `KEY_F6`;
  }
  else if (__x__.__constructor === "KEY_F7") {
    return `KEY_F7`;
  }
  else if (__x__.__constructor === "KEY_F8") {
    return `KEY_F8`;
  }
  else if (__x__.__constructor === "KEY_F9") {
    return `KEY_F9`;
  }
  else if (__x__.__constructor === "KEY_F10") {
    return `KEY_F10`;
  }
  else if (__x__.__constructor === "KEY_F11") {
    return `KEY_F11`;
  }
  else if (__x__.__constructor === "KEY_F12") {
    return `KEY_F12`;
  }
  else if (__x__.__constructor === "KEY_F13") {
    return `KEY_F13`;
  }
  else if (__x__.__constructor === "KEY_F14") {
    return `KEY_F14`;
  }
  else if (__x__.__constructor === "KEY_F15") {
    return `KEY_F15`;
  }
  else if (__x__.__constructor === "KEY_F16") {
    return `KEY_F16`;
  }
  else if (__x__.__constructor === "KEY_F17") {
    return `KEY_F17`;
  }
  else if (__x__.__constructor === "KEY_F18") {
    return `KEY_F18`;
  }
  else if (__x__.__constructor === "KEY_F19") {
    return `KEY_F19`;
  }
  else if (__x__.__constructor === "KEY_F20") {
    return `KEY_F20`;
  }
  else if (__x__.__constructor === "KEY_F21") {
    return `KEY_F21`;
  }
  else if (__x__.__constructor === "KEY_F22") {
    return `KEY_F22`;
  }
  else if (__x__.__constructor === "KEY_F23") {
    return `KEY_F23`;
  }
  else if (__x__.__constructor === "KEY_F24") {
    return `KEY_F24`;
  }
  else if (__x__.__constructor === "KEY_F25") {
    return `KEY_F25`;
  }
  else if (__x__.__constructor === "KEY_F26") {
    return `KEY_F26`;
  }
  else if (__x__.__constructor === "KEY_F27") {
    return `KEY_F27`;
  }
  else if (__x__.__constructor === "KEY_F28") {
    return `KEY_F28`;
  }
  else if (__x__.__constructor === "KEY_F29") {
    return `KEY_F29`;
  }
  else if (__x__.__constructor === "KEY_F30") {
    return `KEY_F30`;
  }
  else if (__x__.__constructor === "KEY_F31") {
    return `KEY_F31`;
  }
  else if (__x__.__constructor === "KEY_F32") {
    return `KEY_F32`;
  }
  else if (__x__.__constructor === "KEY_NUM_LOCK") {
    return `KEY_NUM_LOCK`;
  }
  else if (__x__.__constructor === "KEY_SCROLL_LOCK") {
    return `KEY_SCROLL_LOCK`;
  }
  else if (__x__.__constructor === "KEY_AIRPLANE_MODE") {
    return `KEY_AIRPLANE_MODE`;
  }
  else if (__x__.__constructor === "KEY_CIRCONFLEX") {
    return `KEY_CIRCONFLEX`;
  }
  else if (__x__.__constructor === "KEY_EXCLAMATION_MARK") {
    return `KEY_EXCLAMATION_MARK`;
  }
  else if (__x__.__constructor === "KEY_ARABIC_SEMI_COLON") {
    return `KEY_ARABIC_SEMI_COLON`;
  }
  else if (__x__.__constructor === "KEY_NUMBER_SIGN") {
    return `KEY_NUMBER_SIGN`;
  }
  else if (__x__.__constructor === "KEY_DOLLAR") {
    return `KEY_DOLLAR`;
  }
  else if (__x__.__constructor === "KEY_U_GRAVE_ACCENT") {
    return `KEY_U_GRAVE_ACCENT`;
  }
  else if (__x__.__constructor === "KEY_PAGE_BACKWARD") {
    return `KEY_PAGE_BACKWARD`;
  }
  else if (__x__.__constructor === "KEY_PAGE_FORWARD") {
    return `KEY_PAGE_FORWARD`;
  }
  else if (__x__.__constructor === "KEY_REFRESH") {
    return `KEY_REFRESH`;
  }
  else if (__x__.__constructor === "KEY_RIGHT_PAREN") {
    return `KEY_RIGHT_PAREN`;
  }
  else if (__x__.__constructor === "KEY_ASTERISK") {
    return `KEY_ASTERISK`;
  }
  else if (__x__.__constructor === "KEY_TILDE") {
    return `KEY_TILDE`;
  }
  else if (__x__.__constructor === "KEY_MUTE") {
    return `KEY_MUTE`;
  }
  else if (__x__.__constructor === "KEY_NEXT") {
    return `KEY_NEXT`;
  }
  else if (__x__.__constructor === "KEY_PREVIOUS") {
    return `KEY_PREVIOUS`;
  }
  else if (__x__.__constructor === "KEY_STOP") {
    return `KEY_STOP`;
  }
  else if (__x__.__constructor === "KEY_PLAY_PAUSE") {
    return `KEY_PLAY_PAUSE`;
  }
  else if (__x__.__constructor === "KEY_EMAIL") {
    return `KEY_EMAIL`;
  }
  else if (__x__.__constructor === "KEY_MUTE_UNMUTE") {
    return `KEY_MUTE_UNMUTE`;
  }
  else if (__x__.__constructor === "KEY_DECREASE_VOLUME") {
    return `KEY_DECREASE_VOLUME`;
  }
  else if (__x__.__constructor === "KEY_INCREASE_VOLUME") {
    return `KEY_INCREASE_VOLUME`;
  }
  else if (__x__.__constructor === "KEY_SEMI_COLON") {
    return `KEY_SEMI_COLON`;
  }
  else if (__x__.__constructor === "KEY_COMMA") {
    return `KEY_COMMA`;
  }
  else if (__x__.__constructor === "KEY_DASH") {
    return `KEY_DASH`;
  }
  else if (__x__.__constructor === "KEY_PERIOD") {
    return `KEY_PERIOD`;
  }
  else if (__x__.__constructor === "KEY_FORWARD_SLASH") {
    return `KEY_FORWARD_SLASH`;
  }
  else if (__x__.__constructor === "KEY_GRAVE_ACCENT") {
    return `KEY_GRAVE_ACCENT`;
  }
  else if (__x__.__constructor === "KEY_QUESTION_MARK") {
    return `KEY_QUESTION_MARK`;
  }
  else if (__x__.__constructor === "KEY_BRACKET_LEFT") {
    return `KEY_BRACKET_LEFT`;
  }
  else if (__x__.__constructor === "KEY_BACK_SLASH") {
    return `KEY_BACK_SLASH`;
  }
  else if (__x__.__constructor === "KEY_BRACKET_RIGHT") {
    return `KEY_BRACKET_RIGHT`;
  }
  else if (__x__.__constructor === "KEY_SINGLE_QUOTE") {
    return `KEY_SINGLE_QUOTE`;
  }
  else if (__x__.__constructor === "KEY_BACK_TICK") {
    return `KEY_BACK_TICK`;
  }
  else if (__x__.__constructor === "KEY_CMD") {
    return `KEY_CMD`;
  }
  else if (__x__.__constructor === "KEY_ALTGR") {
    return `KEY_ALTGR`;
  }
  else if (__x__.__constructor === "KEY_LEFT_BACK_SLASH") {
    return `KEY_LEFT_BACK_SLASH`;
  }
  else if (__x__.__constructor === "KEY_GNOME_COMPOSE") {
    return `KEY_GNOME_COMPOSE`;
  }
  else if (__x__.__constructor === "KEY_C_CEDILLA") {
    return `KEY_C_CEDILLA`;
  }
  else if (__x__.__constructor === "KEY_XF86_FORWARD") {
    return `KEY_XF86_FORWARD`;
  }
  else if (__x__.__constructor === "KEY_XF86_BACKWARD") {
    return `KEY_XF86_BACKWARD`;
  }
  else if (__x__.__constructor === "KEY_ALPHA_NUMERIC") {
    return `KEY_ALPHA_NUMERIC`;
  }
  else if (__x__.__constructor === "KEY_HIRAGANA_KATAKANA") {
    return `KEY_HIRAGANA_KATAKANA`;
  }
  else if (__x__.__constructor === "KEY_HALF_WIDTH_FULL_WIDTH") {
    return `KEY_HALF_WIDTH_FULL_WIDTH`;
  }
  else if (__x__.__constructor === "KEY_KANJI") {
    return `KEY_KANJI`;
  }
  else if (__x__.__constructor === "KEY_UNLOCK_TRACK_PAD") {
    return `KEY_UNLOCK_TRACK_PAD`;
  }
  else if (__x__.__constructor === "KEY_TOGGLE_TOUCH_PAD") {
    return `KEY_TOGGLE_TOUCH_PAD`;
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
let KEY_CODE_MAPPINGS = fromList(Comparable.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d)(({ v: ([3, KEY_BREAK]), n: { v: ([8, KEY_BACKSPACE]), n: { v: ([9, KEY_TAB]), n: { v: ([12, KEY_CLEAR]), n: { v: ([13, KEY_ENTER]), n: { v: ([16, KEY_SHIFT]), n: { v: ([17, KEY_CTRL]), n: { v: ([18, KEY_ALT]), n: { v: ([19, KEY_PAUSE]), n: { v: ([20, KEY_CAPS_LOCK]), n: { v: ([21, KEY_HANGUL]), n: { v: ([25, KEY_HANJA]), n: { v: ([27, KEY_ESCAPE]), n: { v: ([28, KEY_CONVERSION]), n: { v: ([29, KEY_NON_CONVERSION]), n: { v: ([32, KEY_SPACE]), n: { v: ([33, KEY_PAGE_UP]), n: { v: ([34, KEY_PAGE_DOWN]), n: { v: ([35, KEY_END]), n: { v: ([36, KEY_HOME]), n: { v: ([37, KEY_LEFT_ARROW]), n: { v: ([38, KEY_UP_ARROW]), n: { v: ([39, KEY_RIGHT_ARROW]), n: { v: ([40, KEY_DOWN_ARROW]), n: { v: ([41, KEY_SELECT]), n: { v: ([42, KEY_PRINT]), n: { v: ([43, KEY_EXECUTE]), n: { v: ([44, KEY_PRINT_SCREEN]), n: { v: ([45, KEY_INSERT]), n: { v: ([46, KEY_DELETE]), n: { v: ([47, KEY_HELP]), n: { v: ([48, KEY_0]), n: { v: ([49, KEY_1]), n: { v: ([50, KEY_2]), n: { v: ([51, KEY_3]), n: { v: ([52, KEY_4]), n: { v: ([53, KEY_5]), n: { v: ([54, KEY_6]), n: { v: ([55, KEY_7]), n: { v: ([56, KEY_8]), n: { v: ([57, KEY_9]), n: { v: ([58, KEY_COLON]), n: { v: ([59, KEY_EQUAL]), n: { v: ([60, KEY_LEFT_CHEVRON]), n: { v: ([61, KEY_EQUAL]), n: { v: ([63, KEY_ESZETT]), n: { v: ([64, KEY_AT]), n: { v: ([65, KEY_A]), n: { v: ([66, KEY_B]), n: { v: ([67, KEY_C]), n: { v: ([68, KEY_D]), n: { v: ([69, KEY_E]), n: { v: ([70, KEY_F]), n: { v: ([71, KEY_G]), n: { v: ([72, KEY_H]), n: { v: ([73, KEY_I]), n: { v: ([74, KEY_J]), n: { v: ([75, KEY_K]), n: { v: ([76, KEY_L]), n: { v: ([77, KEY_M]), n: { v: ([78, KEY_N]), n: { v: ([79, KEY_O]), n: { v: ([80, KEY_P]), n: { v: ([81, KEY_Q]), n: { v: ([82, KEY_R]), n: { v: ([83, KEY_S]), n: { v: ([84, KEY_T]), n: { v: ([85, KEY_U]), n: { v: ([86, KEY_V]), n: { v: ([87, KEY_W]), n: { v: ([88, KEY_X]), n: { v: ([89, KEY_Y]), n: { v: ([90, KEY_Z]), n: { v: ([91, KEY_CMD_LEFT]), n: { v: ([92, KEY_CMD_RIGHT]), n: { v: ([93, KEY_CMD_RIGHT]), n: { v: ([95, KEY_SLEEP]), n: { v: ([96, KEY_NUMPAD_0]), n: { v: ([97, KEY_NUMPAD_1]), n: { v: ([98, KEY_NUMPAD_2]), n: { v: ([99, KEY_NUMPAD_3]), n: { v: ([100, KEY_NUMPAD_4]), n: { v: ([101, KEY_NUMPAD_5]), n: { v: ([102, KEY_NUMPAD_6]), n: { v: ([103, KEY_NUMPAD_7]), n: { v: ([104, KEY_NUMPAD_8]), n: { v: ([105, KEY_NUMPAD_9]), n: { v: ([106, KEY_MULTIPLY]), n: { v: ([107, KEY_ADD]), n: { v: ([108, KEY_NUMPAD_PERIOD]), n: { v: ([109, KEY_SUBSTRACT]), n: { v: ([110, KEY_DECIMAL_POINT]), n: { v: ([111, KEY_DIVIDE]), n: { v: ([112, KEY_F1]), n: { v: ([113, KEY_F2]), n: { v: ([114, KEY_F3]), n: { v: ([115, KEY_F4]), n: { v: ([116, KEY_F5]), n: { v: ([117, KEY_F6]), n: { v: ([118, KEY_F7]), n: { v: ([119, KEY_F8]), n: { v: ([120, KEY_F9]), n: { v: ([121, KEY_F10]), n: { v: ([122, KEY_F11]), n: { v: ([123, KEY_F12]), n: { v: ([124, KEY_F13]), n: { v: ([125, KEY_F14]), n: { v: ([126, KEY_F15]), n: { v: ([127, KEY_F16]), n: { v: ([128, KEY_F17]), n: { v: ([129, KEY_F18]), n: { v: ([130, KEY_F19]), n: { v: ([131, KEY_F20]), n: { v: ([132, KEY_F21]), n: { v: ([133, KEY_F22]), n: { v: ([134, KEY_F23]), n: { v: ([135, KEY_F24]), n: { v: ([136, KEY_F25]), n: { v: ([137, KEY_F26]), n: { v: ([138, KEY_F27]), n: { v: ([139, KEY_F28]), n: { v: ([140, KEY_F29]), n: { v: ([141, KEY_F30]), n: { v: ([142, KEY_F31]), n: { v: ([143, KEY_F32]), n: { v: ([144, KEY_NUM_LOCK]), n: { v: ([145, KEY_SCROLL_LOCK]), n: { v: ([151, KEY_AIRPLANE_MODE]), n: { v: ([160, KEY_CIRCONFLEX]), n: { v: ([161, KEY_EXCLAMATION_MARK]), n: { v: ([162, KEY_ARABIC_SEMI_COLON]), n: { v: ([163, KEY_NUMBER_SIGN]), n: { v: ([164, KEY_DOLLAR]), n: { v: ([165, KEY_U_GRAVE_ACCENT]), n: { v: ([166, KEY_PAGE_BACKWARD]), n: { v: ([167, KEY_PAGE_FORWARD]), n: { v: ([168, KEY_REFRESH]), n: { v: ([169, KEY_RIGHT_PAREN]), n: { v: ([170, KEY_ASTERISK]), n: { v: ([171, KEY_TILDE]), n: { v: ([172, KEY_HOME]), n: { v: ([173, KEY_MUTE]), n: { v: ([174, KEY_DECREASE_VOLUME]), n: { v: ([175, KEY_INCREASE_VOLUME]), n: { v: ([176, KEY_NEXT]), n: { v: ([177, KEY_PREVIOUS]), n: { v: ([178, KEY_STOP]), n: { v: ([179, KEY_PLAY_PAUSE]), n: { v: ([180, KEY_EMAIL]), n: { v: ([181, KEY_MUTE_UNMUTE]), n: { v: ([182, KEY_DECREASE_VOLUME]), n: { v: ([183, KEY_INCREASE_VOLUME]), n: { v: ([186, KEY_SEMI_COLON]), n: { v: ([187, KEY_EQUAL]), n: { v: ([188, KEY_COMMA]), n: { v: ([189, KEY_DASH]), n: { v: ([190, KEY_PERIOD]), n: { v: ([191, KEY_FORWARD_SLASH]), n: { v: ([192, KEY_GRAVE_ACCENT]), n: { v: ([193, KEY_QUESTION_MARK]), n: { v: ([194, KEY_NUMPAD_PERIOD]), n: { v: ([219, KEY_BRACKET_LEFT]), n: { v: ([220, KEY_BACK_SLASH]), n: { v: ([221, KEY_BRACKET_RIGHT]), n: { v: ([222, KEY_SINGLE_QUOTE]), n: { v: ([223, KEY_BACK_TICK]), n: { v: ([224, KEY_CMD]), n: { v: ([225, KEY_ALTGR]), n: { v: ([226, KEY_LEFT_BACK_SLASH]), n: { v: ([230, KEY_GNOME_COMPOSE]), n: { v: ([231, KEY_C_CEDILLA]), n: { v: ([233, KEY_XF86_FORWARD]), n: { v: ([234, KEY_XF86_BACKWARD]), n: { v: ([235, KEY_NON_CONVERSION]), n: { v: ([240, KEY_ALPHA_NUMERIC]), n: { v: ([242, KEY_HIRAGANA_KATAKANA]), n: { v: ([243, KEY_HALF_WIDTH_FULL_WIDTH]), n: { v: ([244, KEY_KANJI]), n: { v: ([251, KEY_UNLOCK_TRACK_PAD]), n: { v: ([255, KEY_TOGGLE_TOUCH_PAD]), n: null } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } } }));
export let getKeyFromCode = (keyCode => fromMaybe(KEY_ANY)(get(Comparable.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d)(keyCode)(KEY_CODE_MAPPINGS)));
export default { getKeyFromCode, KEY_ANY, KEY_BREAK, KEY_BACKSPACE, KEY_TAB, KEY_CLEAR, KEY_ENTER, KEY_SHIFT, KEY_CTRL, KEY_ALT, KEY_PAUSE, KEY_CAPS_LOCK, KEY_HANGUL, KEY_HANJA, KEY_ESCAPE, KEY_CONVERSION, KEY_NON_CONVERSION, KEY_SPACE, KEY_PAGE_UP, KEY_PAGE_DOWN, KEY_END, KEY_HOME, KEY_LEFT_ARROW, KEY_UP_ARROW, KEY_RIGHT_ARROW, KEY_DOWN_ARROW, KEY_SELECT, KEY_PRINT, KEY_EXECUTE, KEY_PRINT_SCREEN, KEY_INSERT, KEY_DELETE, KEY_HELP, KEY_0, KEY_1, KEY_2, KEY_3, KEY_4, KEY_5, KEY_6, KEY_7, KEY_8, KEY_9, KEY_COLON, KEY_LEFT_CHEVRON, KEY_EQUAL, KEY_ESZETT, KEY_AT, KEY_A, KEY_B, KEY_C, KEY_D, KEY_E, KEY_F, KEY_G, KEY_H, KEY_I, KEY_J, KEY_K, KEY_L, KEY_M, KEY_N, KEY_O, KEY_P, KEY_Q, KEY_R, KEY_S, KEY_T, KEY_U, KEY_V, KEY_W, KEY_X, KEY_Y, KEY_Z, KEY_CMD_LEFT, KEY_CMD_RIGHT, KEY_SLEEP, KEY_NUMPAD_0, KEY_NUMPAD_1, KEY_NUMPAD_2, KEY_NUMPAD_3, KEY_NUMPAD_4, KEY_NUMPAD_5, KEY_NUMPAD_6, KEY_NUMPAD_7, KEY_NUMPAD_8, KEY_NUMPAD_9, KEY_MULTIPLY, KEY_ADD, KEY_NUMPAD_PERIOD, KEY_SUBSTRACT, KEY_DECIMAL_POINT, KEY_DIVIDE, KEY_F1, KEY_F2, KEY_F3, KEY_F4, KEY_F5, KEY_F6, KEY_F7, KEY_F8, KEY_F9, KEY_F10, KEY_F11, KEY_F12, KEY_F13, KEY_F14, KEY_F15, KEY_F16, KEY_F17, KEY_F18, KEY_F19, KEY_F20, KEY_F21, KEY_F22, KEY_F23, KEY_F24, KEY_F25, KEY_F26, KEY_F27, KEY_F28, KEY_F29, KEY_F30, KEY_F31, KEY_F32, KEY_NUM_LOCK, KEY_SCROLL_LOCK, KEY_AIRPLANE_MODE, KEY_CIRCONFLEX, KEY_EXCLAMATION_MARK, KEY_ARABIC_SEMI_COLON, KEY_NUMBER_SIGN, KEY_DOLLAR, KEY_U_GRAVE_ACCENT, KEY_PAGE_BACKWARD, KEY_PAGE_FORWARD, KEY_REFRESH, KEY_RIGHT_PAREN, KEY_ASTERISK, KEY_TILDE, KEY_MUTE, KEY_NEXT, KEY_PREVIOUS, KEY_STOP, KEY_PLAY_PAUSE, KEY_EMAIL, KEY_MUTE_UNMUTE, KEY_DECREASE_VOLUME, KEY_INCREASE_VOLUME, KEY_SEMI_COLON, KEY_COMMA, KEY_DASH, KEY_PERIOD, KEY_FORWARD_SLASH, KEY_GRAVE_ACCENT, KEY_QUESTION_MARK, KEY_BRACKET_LEFT, KEY_BACK_SLASH, KEY_BRACKET_RIGHT, KEY_SINGLE_QUOTE, KEY_BACK_TICK, KEY_CMD, KEY_ALTGR, KEY_LEFT_BACK_SLASH, KEY_GNOME_COMPOSE, KEY_C_CEDILLA, KEY_XF86_FORWARD, KEY_XF86_BACKWARD, KEY_ALPHA_NUMERIC, KEY_HIRAGANA_KATAKANA, KEY_HALF_WIDTH_FULL_WIDTH, KEY_KANJI, KEY_UNLOCK_TRACK_PAD, KEY_TOGGLE_TOUCH_PAD };
