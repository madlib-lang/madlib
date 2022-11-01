#ifndef EVENT_LOOP_H
#define EVENT_LOOP_H

#include <stdint.h>
#include <uv.h>
#include "apply-pap.hpp"

uv_loop_t *getLoop();
uv_rwlock_t *getLock();
uv_mutex_t *getMutex();

#ifdef __cplusplus
extern "C" {
#endif

int libuvErrorToMadlibIOError(int libuvError);

void __initEventLoopOnly__();
void __initEventLoop__();
void __startEventLoop__();

void madlib__eventloop__onExit(PAP_t *pap);

void __setTimeout__(PAP_t *pap, int64_t millis);

#ifdef __cplusplus
}
#endif

#endif // EVENT_LOOP_H
