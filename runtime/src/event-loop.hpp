#ifndef EVENT_LOOP_H
#define EVENT_LOOP_H

#include <uv.h>

uv_loop_t *getLoop();

#ifdef __cplusplus
extern "C" {
#endif

void __initEventLoop__();
void __startEventLoop__();

#ifdef __cplusplus
}
#endif

#endif // EVENT_LOOP_H
