#ifndef STDIO_H
#define STDIO_H

#include <gc.h>
#include <stdlib.h>
#include <uv.h>

#include "list.hpp"
#include "event-loop.hpp"
#include "apply-pap.hpp"

#ifdef __cplusplus
extern "C" {
#endif

int STDIN = 0;
int STDOUT = 1;

typedef struct StdinData {
  void *callback;
  char *data;
  int64_t currentSize;
  uv_tty_t *stream;
  bool done;
} StdinData_t;

StdinData_t *madlib__stdio__getLine(PAP_t *callback);
StdinData_t *madlib__stdio__get(PAP_t *callback);

void madlib__stdio__setTTYRawMode();
void madlib__stdio__disableTTYRawMode();
int64_t madlib__stdio__getTTYMode();

void madlib__stdio__clearKeyPressHandler(uv_tty_t *handle);
uv_tty_t *madlib__stdio__onKeyPressed(PAP_t *callback);

uv_signal_t *madlib__stdio__onWindowResized(PAP_t *callback);
void madlib__stdio__clearWindowResizeHandler(uv_signal_t *handle);
madlib__list__Node_t *madlib__stdio__getWindowSize();

void madlib__stdio__cancelGet(StdinData_t *handle);

void madlib__stdio__put(char *str);
void madlib__stdio__putLine(char *str);
void madlib__stdio__err(char *str);

#ifdef __cplusplus
}
#endif

#endif // STDIO_H
