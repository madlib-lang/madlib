
#include <gc.h>
#include "process.hpp"
#include <sys/mman.h>
#include <uv.h>
#include <cstring>
#include "apply-pap.hpp"
#include "event-loop.hpp"
#include "string.hpp"
#include "tuple.hpp"

#ifndef __MINGW32__
  #include <glob.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifndef GLOB_NOMAGIC
#define GLOB_NOMAGIC 0
#endif

#ifndef MAP_NORESERVE
#define MAP_NORESERVE 0
#endif

extern char **environ;

extern void __main__start__();
extern void madlib__stack__init(void *, void (*)());

static madlib__list__Node_t *args;

static int ARGC = 0;
static char **ARGV = NULL;

void __main__init__(int argc, char **argv) {
  GC_set_dont_precollect(1);
  size_t minAlloc = 50 * 1024 * 1024; // 50MB
  GC_set_min_bytes_allocd(minAlloc);
  GC_expand_hp(64 * 1024 * 1024); // 64MB
  GC_set_free_space_divisor(1);

  ARGC = argc;
  ARGV = argv;

  size_t size = 64 * 1024 * 1024;  // 64MB
  char *newStack = (char *)mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE, -1, 0);
  char *stackBottom = newStack + size;

  GC_stack_base stackBase = {
    .mem_base = (void*)stackBottom
  };
  GC_set_stackbottom(NULL, &stackBase);

  madlib__stack__init(stackBottom, __main__start__);
}

void madlib__process__internal__registerArgs() {
  GC_INIT();
  args = madlib__list__empty();

  for (int i = ARGC - 1; i >= 0; i--) {
    args = madlib__list__push(ARGV[i], args);
  }
}

madlib__list__Node_t *madlib__process__internal__getArgs() { return args; }

madlib__list__Node_t *madlib__process__internal__getEnv() {
  GC_INIT();
  madlib__list__Node_t *envItems = madlib__list__empty();
  char **env = environ;
  for (; *env != NULL; env++) {
    // *env has shape ENV_VAR=VALUE
    madlib__tuple__Tuple_2_t *item = (madlib__tuple__Tuple_2_t *)GC_MALLOC(sizeof(madlib__tuple__Tuple_2_t));
    size_t itemLength = strlen(*env);
    int keyLength = 0;

    for (; (*env)[keyLength] != '='; keyLength++) {
    }

    int valueLength = itemLength - keyLength - 1;

    char *key = (char *)GC_MALLOC_ATOMIC(sizeof(char) * (keyLength + 1));
    strncpy(key, *env, keyLength);
    key[keyLength] = '\0';

    char *value = (char *)GC_MALLOC_ATOMIC(sizeof(char) * (valueLength + 1));
    strncpy(value, *env + keyLength + 1, valueLength);
    value[valueLength] = '\0';

    item->first = key;
    item->second = value;

    envItems = madlib__list__push(item, envItems);
  }

  return envItems;
}



// exec
typedef struct ExecData {
  void *callback;
  uv_stream_t *stdoutPipe;
  uv_stream_t *stderrPipe;
  uv_process_options_t *options;

  char *stdoutOutput;
  char *stderrOutput;
  size_t stdoutSize;
  size_t stderrSize;
} ExecData_t;


void onChildClose(uv_handle_t *handle) {
  ExecData_t *data = (ExecData_t*)handle->data;
  GC_FREE(data->options);
  GC_FREE(data);
  GC_FREE(handle);
}


void onChildExit(uv_process_t *req, int64_t exitCode, int termSignal) {
  ExecData_t *data = (ExecData_t*)req->data;

  int64_t *boxedStatus = (int64_t*)exitCode;

  data->stdoutOutput[data->stdoutSize] = '\0';
  data->stderrOutput[data->stderrSize] = '\0';

  uv_close((uv_handle_t*) req, onChildClose);
  __applyPAP__(data->callback, 3, boxedStatus, data->stdoutOutput, data->stderrOutput);
}


void allocExecBuffer(uv_handle_t *handle, size_t suggestedSize, uv_buf_t *buffer) {
  *buffer = uv_buf_init((char*)GC_MALLOC_ATOMIC_UNCOLLECTABLE(suggestedSize), suggestedSize);
}


void onPipeClose(uv_handle_t *handle) {
  GC_FREE(handle);
}


void onExecStdoutRead(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
  if (nread > 0) {
    ExecData_t *data = (ExecData_t*)stream->data;
    size_t newSize = data->stdoutSize + nread;
    char *newOutput = (char*)GC_MALLOC_ATOMIC(newSize + 1);

    if (data->stdoutSize > 0) {
      memcpy(newOutput, data->stdoutOutput, data->stdoutSize);
    }
    memcpy(newOutput + data->stdoutSize, buf->base, nread);

    if (buf->base != NULL) {
      GC_FREE(buf->base);
    }

    data->stdoutOutput = newOutput;
    data->stdoutSize = newSize;
  } else {
    // TODO: handle error
    uv_close((uv_handle_t*) stream, onPipeClose);
  }
}

void onExecStderrRead(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
  if (nread > 0) {
    ExecData_t *data = (ExecData_t*)stream->data;
    size_t newSize = data->stderrSize + nread;
    char *newOutput = (char*)GC_MALLOC_ATOMIC(newSize + 1);

    if (data->stderrSize > 0) {
      memcpy(newOutput, data->stderrOutput, data->stderrSize);
    }
    memcpy(newOutput + data->stderrSize, buf->base, nread);

    if (buf->base != NULL) {
      GC_FREE(buf->base);
    }

    data->stderrOutput = newOutput;
    data->stderrSize = newSize;
  } else {
    // TODO: handle error
    uv_close((uv_handle_t*) stream, onPipeClose);
  }
}

void madlib__process__exec(char *command, madlib__list__Node_t *argList, PAP_t *callback) {
  uv_process_t *childReq = (uv_process_t*)GC_MALLOC_UNCOLLECTABLE(sizeof(uv_process_t));
  uv_process_options_t *options = (uv_process_options_t*)GC_MALLOC_UNCOLLECTABLE(sizeof(uv_process_options_t));
  uv_pipe_t *stdoutPipe = (uv_pipe_t*)GC_MALLOC_UNCOLLECTABLE(sizeof(uv_pipe_t));
  uv_pipe_t *stderrPipe = (uv_pipe_t*)GC_MALLOC_UNCOLLECTABLE(sizeof(uv_pipe_t));

  ExecData_t *data = (ExecData_t*)GC_MALLOC_UNCOLLECTABLE(sizeof(ExecData_t));
  data->callback = callback;
  data->options = options;
  data->stdoutSize = 0;
  data->stderrSize = 0;
  data->stdoutOutput = (char*)GC_MALLOC_ATOMIC(sizeof(char));
  *data->stdoutOutput = '\0';
  data->stderrOutput = (char*)GC_MALLOC_ATOMIC(sizeof(char));
  *data->stderrOutput = '\0';
  data->stdoutPipe = (uv_stream_t *)stdoutPipe;
  data->stderrPipe = (uv_stream_t *)stderrPipe;

  childReq->data = data;
  stdoutPipe->data = data;
  stderrPipe->data = data;


  char **args;

  int64_t argc = madlib__list__length(argList);

  #ifndef __MINGW32__
    glob_t globBuffer;
    globBuffer.gl_offs = 1;
    if (argc > 0) {
      for (int i = 0; i < argc; i++) {
        if (i == 0) {
          glob((const char*)argList->value, GLOB_DOOFFS | GLOB_NOMAGIC | GLOB_NOCHECK, NULL, &globBuffer);
        } else {
          glob((const char*)argList->value, GLOB_DOOFFS | GLOB_NOMAGIC | GLOB_NOCHECK | GLOB_APPEND, NULL, &globBuffer);
        }

        argList = argList->next;
      }
      globBuffer.gl_pathv[0] = command;
      args = globBuffer.gl_pathv;
    } else {
      args = (char**)GC_MALLOC(sizeof(char*));
      args[0] = command;
    }
  #else
    args = (char**)GC_MALLOC(sizeof(char*) * (argc + 1));
    args[0] = command;

    for (int i = 0; i < argc; i++) {
      args[i + 1] = *((char**) argList->value);
      argList = argList->next;
    }
  #endif // __MINGW32__

  uv_pipe_init(getLoop(), stdoutPipe, 0);
  uv_pipe_init(getLoop(), stderrPipe, 0);

  options->stdio_count = 3;
  uv_stdio_container_t child_stdio[3];
  child_stdio[0].flags = UV_IGNORE;
  child_stdio[1].flags = (uv_stdio_flags) (UV_CREATE_PIPE | UV_WRITABLE_PIPE);
  child_stdio[1].data.stream = (uv_stream_t *)stdoutPipe;
  child_stdio[2].flags = (uv_stdio_flags) (UV_CREATE_PIPE | UV_WRITABLE_PIPE);
  child_stdio[2].data.stream = (uv_stream_t *)stderrPipe;
  options->stdio = child_stdio;

  options->exit_cb = onChildExit;
  options->file = command;
  options->args = args;
  options->env = NULL;
  options->flags = 0;
  options->cwd = NULL;

  int spawnResult = uv_spawn(getLoop(), childReq, options);

  #ifndef __MINGW32__
    if (argc > 0) {
      globfree(&globBuffer);
    }
  #endif // __MINGW32__

  if (spawnResult) {
    int64_t *boxedStatus = (int64_t*)1;

    char *stdoutOutput = data->stdoutOutput;
    char *stderrOutput = data->stderrOutput;

    GC_FREE(data->stdoutPipe);
    GC_FREE(data->stderrPipe);
    GC_FREE(childReq);
    GC_FREE(options);
    GC_FREE(data);

    __applyPAP__(callback, 3, boxedStatus, stdoutOutput, stderrOutput);
  } else {
    int rout = uv_read_start((uv_stream_t *)stdoutPipe, allocExecBuffer, onExecStdoutRead);
    int rerr = uv_read_start((uv_stream_t *)stderrPipe, allocExecBuffer, onExecStderrRead);
  }

}

#ifdef __cplusplus
}
#endif
