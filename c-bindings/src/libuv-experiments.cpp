#include <stdio.h>
#include <stdlib.h>
#include <uv.h>

#include <chrono>
#include <cmath>
#include <future>
#include <thread>

uv_loop_t *loop;

void timerCB(uv_timer_t *handle) { printf("done - %s\n", handle->data); }

void out() {
    printf("timer");
}

void forwardTimeoutCallback(uv_timer_t *handle) {
  void(*cb)() = (void(*)()) handle->data;
  cb();
}

void setTimeout(void(*cb)(), int millis) {
  uv_timer_t *timer_req1 = (uv_timer_t *)malloc(sizeof(uv_timer_t));
  timer_req1->data = (void*) cb;
  uv_timer_init(loop, timer_req1);
  uv_timer_start(timer_req1, forwardTimeoutCallback, millis, 0);
}

int main() {
  loop = (uv_loop_t *)malloc(sizeof(uv_loop_t));
  uv_loop_init(loop);


  printf("Running.\n");
  setTimeout([]() { printf("timer1\n"); }, 2000);
  setTimeout([]() { printf("timer2\n"); }, 1000);
  setTimeout([]() { printf("timer3\n"); }, 500);


  uv_run(loop, UV_RUN_DEFAULT);

  uv_loop_close(loop);
  free(loop);
  return 0;
}
