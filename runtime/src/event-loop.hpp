#ifndef EVENT_LOOP_H
#define EVENT_LOOP_H

#include <uv.h>
#include "apply-pap.hpp"

uv_loop_t *getLoop();

#ifdef __cplusplus
extern "C" {
#endif

int libuvErrorToMadlibIOError(int libuvError);

void __initEventLoop__();
void __startEventLoop__();

void __setTimeout__(PAP_t *pap, int64_t millis);

#ifdef __cplusplus
}
#endif

#endif // EVENT_LOOP_H
