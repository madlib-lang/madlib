#define GC_NO_THREAD_REDIRECTS
# ifndef GC_THREADS
#   define GC_THREADS
# endif
#include <uv.h>
#include "process.hpp"
#include <sys/mman.h>
#include <cstring>
#include "apply-pap.hpp"
#include "event-loop.hpp"
#include "string.hpp"
#include "tuple.hpp"
#include <unistd.h>
#include <limits.h>
#include <time.h>
#include <gc.h>

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

// #define UV_THREADPOOL_SIZE 128

extern char **environ;

extern void __main__start__();
extern void madlib__stack__init(void *, void (*)());

static madlib__list__Node_t *__args__;

static int ARGC = 0;
static char **ARGV = NULL;

void __main__init__(int argc, char **argv) {
  GC_set_dont_precollect(1);
  GC_allow_register_threads();

  // TODO: make min alloc and initial heap size available to compilation options
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

void madlib__process__internal__initExtra() {
  GC_INIT();
  setenv("UV_THREADPOOL_SIZE", "128", 1);
  __args__ = madlib__list__empty();

  srand(time(NULL));

  for (int i = ARGC - 1; i >= 0; i--) {
    __args__ = madlib__list__push(ARGV[i], __args__);
  }
}

madlib__list__Node_t *madlib__process__internal__getArgs() {
  return __args__;
}

madlib__list__Node_t *madlib__process__internal__getEnv() {
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

char *madlib__process__internal__getCurrentPath() {
  char cwd[PATH_MAX];

  if (getcwd(cwd, sizeof(cwd)) != NULL) {
    char *res = (char*) GC_MALLOC(strlen(cwd) + 1);
    memcpy(res, cwd, strlen(cwd));
    return res;
  } else {
    return (char*)"";
  }
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

void madlib__process__exec(char *command, madlib__list__Node_t *argList, madlib__record__Record_t *commandOptions, PAP_t *callback) {
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
      args[i + 1] = (char*) argList->value;
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

  madlib__dictionary__Dictionary_t *envFromOptions = (madlib__dictionary__Dictionary_t *) madlib__record__internal__selectField((char*)"env", commandOptions);
  madlib__list__Node_t *envItems = envFromOptions->items;
  int itemCount = madlib__list__length(envItems);
  char **env = (char**)GC_MALLOC_ATOMIC(sizeof(char*) * (itemCount + 1));
  int index = 0;
  while (envItems->next != NULL) {
    char *key = (char*)((madlib__tuple__Tuple_2_t*)envItems->value)->first;
    size_t keyLength = strlen(key);
    char *value = (char*)((madlib__tuple__Tuple_2_t*)envItems->value)->second;
    size_t valueLength = strlen(value);

    env[index] = (char*) GC_MALLOC_ATOMIC(keyLength +valueLength + 2);
    memcpy(env[index], key, keyLength);
    env[index][keyLength] = '=';
    memcpy(env[index] + keyLength + 1, value, valueLength);
    env[index][keyLength + 1 + valueLength] = '\0';
    envItems = envItems->next;
  }
  env[itemCount] = 0; // must be null terminated from libuv docs


  options->exit_cb = onChildExit;
  options->file = command;
  options->args = args;
  options->env = itemCount == 0 ? NULL : env;
  options->flags = 0;
  options->cwd = (const char*) madlib__record__internal__selectField((char*)"cwd", commandOptions);

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



// thread

typedef struct ThreadData {
  void *badCallback;
  void *goodCallback;
  void *threadFn;
  void *result;
  bool isBad;
} ThreadData_t;


void goodCallbackFn(uv_work_t *req, void *result) {
  ThreadData_t *data = (ThreadData_t*) req->data;
  data->result = result;
  data->isBad = false;
  uv_cancel((uv_req_t*)req);
}


void badCallbackFn(uv_work_t *req, void *result) {
  ThreadData_t *data = (ThreadData_t*) req->data;
  data->result = result;
  data->isBad = true;
  uv_cancel((uv_req_t*)req);
}


void onThreadLoopClose(uv_handle_t *handle) {}


void onThreadLoopWalk(uv_handle_t *handle, void *arg) {
  if (!uv_is_closing(handle)) {  // FALSE: handle is closing
    uv_close(handle, onThreadLoopClose);
  }
}


void runThread(uv_work_t *req) {
  struct GC_stack_base sb;
  GC_get_stack_base(&sb);
  GC_register_my_thread(&sb);
  GC_INIT();
  __initEventLoopOnly__();
  uv_loop_t *threadLoop = getLoop();
  ThreadData_t *data = (ThreadData_t*) req->data;

  PAP_t *goodCallbackArg = (PAP_t*) GC_MALLOC(sizeof(PAP_t));
  goodCallbackArg->fn = (void*)goodCallbackFn;
  goodCallbackArg->arity = 2;
  goodCallbackArg->missingArgCount = 2;

  PAP_t *badCallbackArg = (PAP_t*) GC_MALLOC(sizeof(PAP_t));
  badCallbackArg->fn = (void*)badCallbackFn;
  badCallbackArg->arity = 2;
  badCallbackArg->missingArgCount = 2;

  PAP_t *goodCallbackArgWithData = (PAP_t*) __applyPAP__(goodCallbackArg, 1, req);
  PAP_t *badCallbackArgWithData = (PAP_t*) __applyPAP__(badCallbackArg, 1, req);

  __applyPAP__(data->threadFn, 2, badCallbackArgWithData, goodCallbackArgWithData);

  uv_run(threadLoop, UV_RUN_DEFAULT);

  int r = uv_loop_close(threadLoop);
  if (r != 0) {
    // Close pending handles
    uv_walk(threadLoop, onThreadLoopWalk, NULL);

    // run the loop until there are no pending callbacks
    do {
      r = uv_run(threadLoop, UV_RUN_ONCE);
    } while (r != 0);
    // Now we're safe.
    r = uv_loop_close(threadLoop);
  }
  GC_FREE(threadLoop);
}


void afterThread(uv_work_t *req, int status) {
  ThreadData_t *data = (ThreadData_t*) req->data;
  void *badCallback = data->badCallback;
  void *goodCallback = data->goodCallback;
  void *result = data->result;
  bool isBad = data->isBad;
  GC_FREE(data);
  GC_FREE(req);

  if (isBad) {
    __applyPAP__(badCallback, 1, result);
  } else {
    __applyPAP__(goodCallback, 1, result);
  }
}

void madlib__process__thread(PAP_t *fn, PAP_t *badCallback, PAP_t *goodCallback) {
  uv_work_t *req = (uv_work_t*) GC_MALLOC_UNCOLLECTABLE(sizeof(uv_work_t));
  ThreadData_t *data = (ThreadData_t*) GC_MALLOC_UNCOLLECTABLE(sizeof(ThreadData_t));
  data->badCallback = badCallback;
  data->goodCallback = goodCallback;
  data->threadFn = fn;
  req->data = data;
  int r = uv_queue_work(getLoop(), req, runThread, afterThread);
}


uv_rwlock_t *madlib__process__makeLock(void *_) {
  uv_rwlock_t *lock = (uv_rwlock_t*) GC_MALLOC(sizeof(uv_rwlock_t));
  uv_rwlock_init(lock);
  return lock;
}

uv_mutex_t *madlib__process__makeMutex(void *_) {
  uv_mutex_t *mutex = (uv_mutex_t*) GC_MALLOC(sizeof(uv_mutex_t));
  uv_mutex_init(mutex);
  return mutex;
}

void madlib__process__writeLock(uv_rwlock_t *lock) {
  uv_rwlock_wrlock(lock);
}

void madlib__process__writeUnlock(uv_rwlock_t *lock) {
  uv_rwlock_wrunlock(lock);
}

void madlib__process__readLock(uv_rwlock_t *lock) {
  uv_rwlock_rdlock(lock);
}

void madlib__process__readUnlock(uv_rwlock_t *lock) {
  uv_rwlock_rdunlock(lock);
}

void madlib__process__mutexLock(uv_mutex_t *mutex) {
  uv_mutex_lock(mutex);
}

void madlib__process__mutexUnlock(uv_mutex_t *mutex) {
  uv_mutex_unlock(mutex);
}


#ifdef __cplusplus
}
#endif
