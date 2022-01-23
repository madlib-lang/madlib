#include "stdio.hpp"

#include <gc.h>
#include <stdlib.h>
#include <uv.h>

#include "event-loop.hpp"


#ifdef __cplusplus
extern "C" {
#endif

// read file
typedef struct StdinData {
  void *callback;
  char *data;
  int64_t currentSize;
} StdinData_t;

void onError(StdinData_t *stdinData, int libUvError) {
  char **boxedResult = (char **)GC_malloc(sizeof(char *));
  *boxedResult = (char*)"\0";

  int64_t *boxedError = (int64_t *)GC_malloc_uncollectable(sizeof(int));
  *boxedError = libuvErrorToMadlibIOError(libUvError);

  // free resources
  if (stdinData->data) {
    GC_free(stdinData->data);
  }
  GC_free(stdinData);

  __applyPAP__(((StdinData_t *)stdinData->data)->callback, 2, boxedError, boxedResult);
}

void onReadLine(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buffer) {
  StdinData_t *stdinData = (StdinData_t*)stream->data;

  if (nread < 0) {
    // if (nread == UV_EOF) {

    // error
    uv_read_stop(stream);
    onError(stdinData, nread);
  } else if (nread > 0) {
    int bytesToUse = 0;
    bool foundLineReturn = false;
    for (int i = 0; i < nread && !foundLineReturn; i++) {
      if (buffer->base[i] == '\n') {
        foundLineReturn = true;
      } else {
        bytesToUse += 1;
      }
    }

    char *nextData = (char*) GC_malloc(sizeof(char) * (stdinData->currentSize + bytesToUse + 1));
    if (stdinData->currentSize > 0) {
      memcpy(nextData, stdinData->data, stdinData->currentSize * sizeof(char));
    }
    memcpy(nextData + stdinData->currentSize, buffer->base, bytesToUse * sizeof(char));
    nextData[stdinData->currentSize + bytesToUse] = '\0';

    GC_free(stdinData->data);
    stdinData->data = nextData;
    stdinData->currentSize += bytesToUse;

    if (foundLineReturn) {
      uv_read_stop(stream);
      char **boxedResult = (char **)GC_malloc(sizeof(char *));
      *boxedResult = stdinData->data;
      void *cb = stdinData->callback;
      GC_free(stdinData);

      int64_t *boxedError = (int64_t *)GC_malloc(sizeof(int64_t));
      *boxedError = 0;

      __applyPAP__(cb, 2, boxedError, boxedResult);
    }
  }

  if (buffer->base) {
    GC_free(buffer->base);
  }
}

void onRead(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buffer) {
  StdinData_t *stdinData = (StdinData_t*)stream->data;

  if (nread < 0) {
    if (nread == UV_EOF) {
      uv_read_stop(stream);
      char **boxedResult = (char **)GC_malloc(sizeof(char *));
      *boxedResult = stdinData->data;
      void *cb = stdinData->callback;
      GC_free(stdinData);

      int64_t *boxedError = (int64_t *)GC_malloc(sizeof(int64_t));
      *boxedError = 0;

      __applyPAP__(cb, 2, boxedError, boxedResult);
    } else {
      // error
      uv_read_stop(stream);
      onError(stdinData, nread);
    }
  } else if (nread > 0) {
    char *nextData = (char*) GC_malloc(sizeof(char) * (stdinData->currentSize + nread + 1));
    if (stdinData->currentSize > 0) {
      memcpy(nextData, stdinData->data, stdinData->currentSize * sizeof(char));
    }
    memcpy(nextData + stdinData->currentSize, buffer->base, nread);
    nextData[stdinData->currentSize + nread] = '\0';

    GC_free(stdinData->data);
    stdinData->data = nextData;
    stdinData->currentSize += nread;
  }

  if (buffer->base) {
    GC_free(buffer->base);
  }
}

void allocBuffer(uv_handle_t *handle, size_t suggestedSize, uv_buf_t *buffer) {
  *buffer = uv_buf_init((char*) GC_malloc_uncollectable(suggestedSize), suggestedSize);
}

void madlib__stdio__getLine(PAP_t *callback) {
  // we allocate request objects and the buffer
  uv_tty_t *stdin = (uv_tty_t *)GC_malloc_uncollectable(sizeof(uv_tty_t));
  
  StdinData_t *data = (StdinData_t *)GC_malloc_uncollectable(sizeof(StdinData_t));
  data->callback = callback;
  data->currentSize = 0;

  stdin->data = data;

  uv_tty_init(getLoop(), stdin, STDIN, 1);
  uv_read_start((uv_stream_t*)stdin, allocBuffer, onReadLine);
}

void madlib__stdio__get(PAP_t *callback) {
  // we allocate request objects and the buffer
  uv_tty_t *stdin = (uv_tty_t *)GC_malloc_uncollectable(sizeof(uv_tty_t));
  
  StdinData_t *data = (StdinData_t *)GC_malloc_uncollectable(sizeof(StdinData_t));
  data->callback = callback;
  data->currentSize = 0;

  stdin->data = data;

  uv_tty_init(getLoop(), stdin, STDIN, 1);
  uv_read_start((uv_stream_t*)stdin, allocBuffer, onRead);
}

void madlib__stdio__put(char *str) {
  fputs(str, stdout);
  fflush(stdout);
} 

void madlib__stdio__err(char *str) {
  fputs(str, stderr);
  fflush(stderr);
} 

#ifdef __cplusplus
}
#endif
