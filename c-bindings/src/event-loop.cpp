#include <stdlib.h>
#include <uv.h>
#include <cmath>

#include "apply-pap.hpp"

static uv_loop_t *loop;

#ifdef __cplusplus
extern "C" {
#endif

void __initEventLoop__() {
  loop = (uv_loop_t *)malloc(sizeof(uv_loop_t));
  uv_loop_init(loop);
}

void __startEventLoop__() {
  uv_run(loop, UV_RUN_DEFAULT);

  uv_loop_close(loop);
  free(loop);
}

void forwardTimeoutCallback(uv_timer_t *handle) {
  PAP_t *pap = (PAP_t *)handle->data;
  __applyPAP__(pap, 1, NULL);
}

void __setTimeout__(PAP_t *pap, double *millis) {
  uv_timer_t *timer_req1 = (uv_timer_t *)malloc(sizeof(uv_timer_t));
  timer_req1->data = (void *)pap;
  uv_timer_init(loop, timer_req1);
  uv_timer_start(timer_req1, forwardTimeoutCallback, std::round(*millis), 0);
}

#ifdef __cplusplus
}
#endif
