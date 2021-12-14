#include <stdlib.h>
#include <uv.h>
#include <gc.h>

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

// set timeout
void forwardTimeoutCallback(uv_timer_t *handle) {
  PAP_t *pap = (PAP_t *)handle->data;
  __applyPAP__(pap, 1, NULL);
}

void __setTimeout__(PAP_t *pap, int64_t millis) {
  uv_timer_t *timer_req1 = (uv_timer_t *)malloc(sizeof(uv_timer_t));
  timer_req1->data = (void *)pap;
  uv_timer_init(loop, timer_req1);
  uv_timer_start(timer_req1, forwardTimeoutCallback, millis, 0);
}


// read file
typedef struct ReadData {
  void *callback;
  uv_fs_t *readRequest;
  uv_fs_t *openRequest;
  uv_buf_t uvBuffer;
  char *dataBuffer;
  char *fileContent;
  int currentSize;
} ReadData_t;


void onRead(uv_fs_t *req) {
  uv_fs_req_cleanup(req);

  if (req->result < 0) {
    fprintf(stderr, "Read error: %s\n", uv_strerror(req->result));
  } else if (req->result == 0) {
    uv_fs_t closeReq;

    uv_fs_close(loop, &closeReq, ((ReadData_t *)req->data)->openRequest->result, NULL);

    char **boxed = (char **)GC_malloc(sizeof(char*));
    *boxed = ((ReadData_t *)req->data)->fileContent;

    __applyPAP__(((ReadData_t *)req->data)->callback, 1, boxed);
  } else if (req->result > 0) {
    int currentSize = ((ReadData_t *) req->data)->currentSize;
    ((ReadData_t *) req->data)->currentSize = currentSize + req->result;
    char *nextContent = (char *) GC_malloc(currentSize + req->result);
    if (currentSize > 0) {
      memcpy(nextContent, ((ReadData_t *) req->data)->fileContent, currentSize);
    }
    memcpy(nextContent + currentSize, ((ReadData_t *) req->data)->dataBuffer, req->result);
    ((ReadData_t *) req->data)->fileContent = nextContent;
    uv_fs_read(loop, req, ((ReadData_t *)req->data)->openRequest->result, &((ReadData_t *)req->data)->uvBuffer, 1, -1, onRead);
  }
}


void onOpen(uv_fs_t *req) {
  if (req->result >= 0) {
    uv_buf_t uvBuffer = uv_buf_init(((ReadData_t *) req->data)->dataBuffer, 1024);
    ((ReadData_t *) ((ReadData_t *) req->data)->readRequest->data)->uvBuffer = uvBuffer;
    uv_fs_read(loop, ((ReadData_t *) req->data)->readRequest, req->result, &uvBuffer, 1, -1, onRead);
  } else {
    fprintf(stderr, "error opening file: %s\n", uv_strerror((int)req->result));
  }

  free(req->data);
  uv_fs_req_cleanup(req);
}


void readFile(char *filepath, PAP_t *callback) {
  uv_fs_t *openReq = (uv_fs_t *)malloc(sizeof(uv_fs_t));
  uv_fs_t *readReq = (uv_fs_t *)malloc(sizeof(uv_fs_t));
  char *dataBuffer = (char *)malloc(sizeof(char) * 1024);

  openReq->data = malloc(sizeof(ReadData));
  ((ReadData_t *) openReq->data)->callback = callback;
  ((ReadData_t *) openReq->data)->readRequest = readReq;
  ((ReadData_t *) openReq->data)->dataBuffer = dataBuffer;

  readReq->data = malloc(sizeof(ReadData));
  ((ReadData_t *) readReq->data)->callback = callback;
  ((ReadData_t *) readReq->data)->openRequest = openReq;
  ((ReadData_t *) readReq->data)->dataBuffer = dataBuffer;
  ((ReadData_t *) readReq->data)->currentSize = 0;

  uv_fs_open(loop, openReq, filepath, O_RDONLY, 0, onOpen);
}

#ifdef __cplusplus
}
#endif
