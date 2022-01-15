#include <gc.h>
#include <stdlib.h>
#include <uv.h>
#include <curl/curl.h>

#include <cmath>

#include "apply-pap.hpp"

static uv_loop_t *loop;

uv_loop_t *getLoop() {
  return loop;
}


#ifdef __cplusplus
extern "C" {
#endif

void __initEventLoop__() {
  curl_global_init(CURL_GLOBAL_ALL);
  loop = (uv_loop_t *)GC_malloc_uncollectable(sizeof(uv_loop_t));
  uv_loop_init(loop);
}

void __startEventLoop__() {
  uv_run(loop, UV_RUN_DEFAULT);

  uv_loop_close(loop);
  GC_free(loop);
  curl_global_cleanup();
}

// set timeout
void forwardTimeoutCallback(uv_timer_t *handle) {
  __applyPAP__(handle->data, 1, NULL);
}

void __setTimeout__(PAP_t *pap, int64_t millis) {
  uv_timer_t *timer_req1 =
      (uv_timer_t *)GC_malloc_uncollectable(sizeof(uv_timer_t));
  timer_req1->data = (void *)pap;
  uv_timer_init(loop, timer_req1);
  uv_timer_start(timer_req1, forwardTimeoutCallback, millis, 0);
}

#ifdef __cplusplus
}
#endif
