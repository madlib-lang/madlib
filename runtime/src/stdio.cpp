
#include <gc.h>
#include "stdio.hpp"


#include <stdlib.h>
#include <uv.h>
#include <string.h>

#include "event-loop.hpp"


#ifdef __cplusplus
extern "C" {
#endif


typedef struct StdinData {
  void *callback;
  char *data;
  int64_t currentSize;
} StdinData_t;


void onError(StdinData_t *stdinData, int libUvError) {
  char *boxedResult = (char*)"\0";

  int64_t *boxedError = (int64_t *)libuvErrorToMadlibIOError(libUvError);

  void *callback = stdinData->callback;

  // free resources
  if (stdinData->data) {
    GC_FREE(stdinData->data);
  }
  GC_FREE(stdinData);

  __applyPAP__(callback, 2, boxedError, boxedResult);
}


void onStdInPipeClose(uv_handle_t *handle) {
  GC_FREE(handle);
}


void onStdinReadLine(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buffer) {
  StdinData_t *stdinData = (StdinData_t*)stream->data;

  if (nread < 0) {
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

    char *nextData = (char*)GC_MALLOC_ATOMIC(sizeof(char) * (stdinData->currentSize + bytesToUse + 1));
    if (stdinData->currentSize > 0) {
      memcpy(nextData, stdinData->data, stdinData->currentSize * sizeof(char));
    }
    memcpy(nextData + stdinData->currentSize, buffer->base, bytesToUse * sizeof(char));
    nextData[stdinData->currentSize + bytesToUse] = '\0';

    // GC_free(stdinData->data);
    stdinData->data = nextData;
    stdinData->currentSize += bytesToUse;

    if (foundLineReturn) {
      void *cb = stdinData->callback;
      void *result = stdinData->data;

      uv_read_stop(stream);
      uv_close((uv_handle_t*)stream, onStdInPipeClose);
      GC_FREE(stdinData);

      int64_t *boxedError = (int64_t *)0;

      __applyPAP__(cb, 2, boxedError, result);
    }
  }

  if (buffer->base) {
    GC_FREE(buffer->base);
  }
}


void onStdinRead(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buffer) {
  StdinData_t *stdinData = (StdinData_t*)stream->data;

  if (nread < 0) {
    if (nread == UV_EOF) {
      uv_read_stop(stream);
      char *result = NULL;
      if (stdinData->data == NULL) {
        result = (char*)"\0";
      } else {
        result = stdinData->data;
      }
      void *cb = stdinData->callback;

      GC_FREE(stdinData);
      uv_close((uv_handle_t*) stream, onStdInPipeClose);

      int64_t *boxedError = (int64_t *)0;
      __applyPAP__(cb, 2, boxedError, result);
    } else {
      // error
      uv_read_stop(stream);
      onError(stdinData, nread);
    }
  } else if (nread > 0) {
    char *nextData = (char*)GC_MALLOC_ATOMIC(sizeof(char) * (stdinData->currentSize + nread + 1));
    if (stdinData->currentSize > 0) {
      memcpy(nextData, stdinData->data, stdinData->currentSize * sizeof(char));
    }
    memcpy(nextData + stdinData->currentSize, buffer->base, nread);
    nextData[stdinData->currentSize + nread] = '\0';

    stdinData->data = nextData;
    stdinData->currentSize += nread;
  }

  if (buffer->base) {
    GC_FREE(buffer->base);
  }
}


void allocBuffer(uv_handle_t *handle, size_t suggestedSize, uv_buf_t *buffer) {
  *buffer = uv_buf_init((char*)GC_MALLOC_ATOMIC_UNCOLLECTABLE(suggestedSize), suggestedSize);
}


void madlib__stdio__getLine(PAP_t *callback) {
  // we allocate request objects and the buffer
  uv_tty_t *stdIn = (uv_tty_t *)GC_MALLOC_UNCOLLECTABLE(sizeof(uv_tty_t));
  
  StdinData_t *data = (StdinData_t *)GC_MALLOC_UNCOLLECTABLE(sizeof(StdinData_t));
  data->callback = callback;
  data->currentSize = 0;

  stdIn->data = data;

  uv_tty_init(getLoop(), stdIn, STDIN, 1);
  uv_read_start((uv_stream_t*)stdIn, allocBuffer, onStdinReadLine);
}

void madlib__stdio__get(PAP_t *callback) {
  // we allocate request objects and the buffer
  uv_tty_t *stdIn = (uv_tty_t *)GC_MALLOC_UNCOLLECTABLE(sizeof(uv_tty_t));
  
  StdinData_t *data = (StdinData_t *)GC_MALLOC_UNCOLLECTABLE(sizeof(StdinData_t));
  data->callback = callback;
  data->currentSize = 0;

  stdIn->data = data;

  uv_tty_init(getLoop(), stdIn, STDIN, 1);
  uv_read_start((uv_stream_t*)stdIn, allocBuffer, onStdinRead);
}


void madlib__stdio__put(char *str) {
  fputs(str, stdout);
  fflush(stdout);
}


void madlib__stdio__putLine(char *str) {
  puts(str);
  fflush(stdout);
}


void madlib__stdio__err(char *str) {
  fputs(str, stderr);
  fflush(stderr);
} 

#ifdef __cplusplus
}
#endif
