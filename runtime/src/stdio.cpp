
#include "stdio.hpp"
#include <string.h>
#include "event-loop.hpp"
#include <iostream>
#include <cstring>

#ifdef __cplusplus
extern "C" {
#endif


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
    stdinData->done = true;
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
      stdinData->done = true;
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
    stdinData->done = true;
    if (nread == UV_EOF) {
      uv_read_stop(stream);
      char *result = NULL;
      if (stdinData->data == NULL) {
        result = (char*)"";
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

static int64_t madlib__stdio__ttyMode = 0;


int64_t madlib__stdio__getTTYMode() {
  return madlib__stdio__ttyMode;
}


void madlib__stdio__enableTTYRawMode() {
  uv_tty_t *stdIn = (uv_tty_t *)GC_MALLOC(sizeof(uv_tty_t));
  uv_tty_init(getLoop(), stdIn, STDIN, 1);

  uv_tty_set_mode(stdIn, UV_TTY_MODE_RAW);
  madlib__stdio__ttyMode = 1;
}


void madlib__stdio__disableTTYRawMode() {
  uv_tty_reset_mode();
  madlib__stdio__ttyMode = 0;
}

void onStdinKeyPress(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buffer) {
  StdinData_t *stdinData = (StdinData_t*)stream->data;

  if (nread < 0) {
    stdinData->done = true;
    // error
    uv_read_stop(stream);
    onError(stdinData, nread);
  } else if (nread > 0) {
    int bytesToUse = 0;
    char *result = (char*)GC_MALLOC_ATOMIC(sizeof(char) * (nread + 1));
    memcpy(result, buffer->base, nread * sizeof(char));
    result[nread] = '\0';
    void *cb = stdinData->callback;
    __applyPAP__(cb, 1, result);
  }

  if (buffer->base) {
    GC_FREE(buffer->base);
  }
}

uv_tty_t *madlib__stdio__onKeyPressed(PAP_t *callback) {
  // we allocate request objects and the buffer
  uv_tty_t *stdIn = (uv_tty_t *)GC_MALLOC(sizeof(uv_tty_t));

  StdinData_t *data = (StdinData_t *)GC_MALLOC(sizeof(StdinData_t));
  data->callback = callback;
  data->currentSize = 0;
  data->done = false;
  data->stream = stdIn;

  stdIn->data = data;

  uv_tty_init(getLoop(), stdIn, STDIN, 1);
  uv_read_start((uv_stream_t*)stdIn, allocBuffer, onStdinKeyPress);

  return stdIn;
}


void madlib__stdio__clearKeyPressHandler(uv_tty_t *handle) {
  if (!uv_is_closing((uv_handle_t *) handle)) {
    uv_close((uv_handle_t *) handle, NULL);
  }
}


void onWindowResized(uv_signal_t *signalHandle, int signum) {
  __applyPAP__(signalHandle->data, 1, NULL);
}


uv_signal_t *madlib__stdio__onWindowResized(PAP_t *callback) {
  // we allocate request objects and the buffer
  uv_signal_t *signalHandle = (uv_signal_t *)GC_MALLOC(sizeof(uv_signal_t));
  uv_signal_init(getLoop(), signalHandle);
  signalHandle->data = (void *) callback;

  uv_signal_start(signalHandle, onWindowResized, SIGWINCH);

  return signalHandle;
}


void madlib__stdio__clearWindowResizeHandler(uv_signal_t *handle) {
  if (!uv_is_closing((uv_handle_t *) handle)) {
    uv_close((uv_handle_t *) handle, NULL);
  }
}


madlib__list__Node_t *madlib__stdio__getWindowSize() {
  madlib__list__Node_t *result = madlib__list__empty();

  uv_tty_t *stdIn = (uv_tty_t *)GC_MALLOC(sizeof(uv_tty_t));
  uv_tty_init(getLoop(), stdIn, STDIN, 1);

  int cols, rows = 0;
  uv_tty_get_winsize(stdIn, &cols, &rows);

  result = madlib__list__internal__push((void *) rows, result);
  result = madlib__list__internal__push((void *) cols, result);
  return result;
}


StdinData_t *madlib__stdio__getLine(PAP_t *callback) {
  // we allocate request objects and the buffer
  uv_tty_t *stdIn = (uv_tty_t *)GC_MALLOC(sizeof(uv_tty_t));

  StdinData_t *data = (StdinData_t *)GC_MALLOC(sizeof(StdinData_t));
  data->callback = callback;
  data->currentSize = 0;
  data->done = false;
  data->stream = stdIn;

  stdIn->data = data;

  uv_tty_init(getLoop(), stdIn, STDIN, 1);
  uv_read_start((uv_stream_t*)stdIn, allocBuffer, onStdinReadLine);

  return data;
}

StdinData_t *madlib__stdio__get(PAP_t *callback) {
  // we allocate request objects and the buffer
  uv_tty_t *stdIn = (uv_tty_t *)GC_MALLOC(sizeof(uv_tty_t));

  StdinData_t *data = (StdinData_t *)GC_MALLOC(sizeof(StdinData_t));
  data->callback = callback;
  data->currentSize = 0;
  data->done = false;
  data->stream = stdIn;

  stdIn->data = data;

  uv_tty_init(getLoop(), stdIn, STDIN, 1);
  uv_read_start((uv_stream_t*)stdIn, allocBuffer, onStdinRead);

  return data;
}


void madlib__stdio__cancelGet(StdinData_t *handle) {
  if (!handle->done) {
    uv_read_stop((uv_stream_t*) handle->stream);
    uv_close((uv_handle_t*) handle->stream, onStdInPipeClose);
  }
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
