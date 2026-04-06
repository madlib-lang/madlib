#include "uv.h"
#include "process.hpp"
#if defined(_WIN32) || defined(__MINGW32__)
#include <windows.h>
#else
#include <sys/mman.h>
#endif
#include <cstring>
#include "apply-pap.hpp"
#include "event-loop.hpp"
#include "string.hpp"
#include "tuple.hpp"
#if !defined(_WIN32) && !defined(__MINGW32__)
#include <unistd.h>
#endif
#include <limits.h>
#include <time.h>
#include <thread>
#include <cstdio>
#include <cstdlib>
#include <cstdint>

#ifndef __MINGW32__
  #include <glob.h>
#endif

#ifdef __MINGW32__
int putenv(char *) {
  return 0;
}
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

static madlib__list__Node_t *__args__;

static int ARGC = 0;
static char **ARGV = NULL;

static bool isTruthyEnvValue(const char *value) {
  if (value == NULL || value[0] == '\0') {
    return false;
  }

  return
    strcmp(value, "1") == 0
    || strcmp(value, "true") == 0
    || strcmp(value, "TRUE") == 0
    || strcmp(value, "yes") == 0
    || strcmp(value, "YES") == 0;
}

static uint64_t getPhysicalMemoryBytes() {
#if defined(_WIN32) || defined(__MINGW32__)
  MEMORYSTATUSEX statex;
  statex.dwLength = sizeof(statex);
  if (GlobalMemoryStatusEx(&statex)) {
    return (uint64_t)statex.ullTotalPhys;
  }
  return 8ULL * 1024ULL * 1024ULL * 1024ULL;
#else
  long pages = sysconf(_SC_PHYS_PAGES);
  long pageSize = sysconf(_SC_PAGESIZE);
  if (pages <= 0 || pageSize <= 0) {
    return 8ULL * 1024ULL * 1024ULL * 1024ULL;
  }
  return (uint64_t)pages * (uint64_t)pageSize;
#endif
}

static size_t clampSize(size_t value, size_t minValue, size_t maxValue) {
  if (value < minValue) {
    return minValue;
  }
  if (value > maxValue) {
    return maxValue;
  }
  return value;
}

static size_t parseSizeEnvOrDefault(const char *rawValue, size_t fallbackValue) {
  if (rawValue == NULL || rawValue[0] == '\0') {
    return fallbackValue;
  }

  char *endPtr = NULL;
  unsigned long long parsed = strtoull(rawValue, &endPtr, 10);
  if (endPtr == rawValue || (endPtr != NULL && *endPtr != '\0') || parsed == 0) {
    return fallbackValue;
  }

  return (size_t)parsed;
}

static int parseIntEnvOrDefault(const char *rawValue, int fallbackValue) {
  if (rawValue == NULL || rawValue[0] == '\0') {
    return fallbackValue;
  }

  char *endPtr = NULL;
  long parsed = strtol(rawValue, &endPtr, 10);
  if (endPtr == rawValue || (endPtr != NULL && *endPtr != '\0') || parsed <= 0) {
    return fallbackValue;
  }

  return (int)parsed;
}

static void appendOutputChunk(char **output, size_t *size, size_t *capacity, const char *chunk, size_t chunkSize) {
  size_t required = *size + chunkSize + 1;
  if (*capacity < required) {
    size_t newCapacity = (*capacity == 0) ? 64 : *capacity;
    while (newCapacity < required) {
      newCapacity *= 2;
    }

    char *newOutput = (char *)GC_MALLOC_ATOMIC(newCapacity);
    if (*size > 0) {
      memcpy(newOutput, *output, *size);
    }

    *output = newOutput;
    *capacity = newCapacity;
  }

  memcpy(*output + *size, chunk, chunkSize);
  *size += chunkSize;
  (*output)[*size] = '\0';
}


void __main__init__(int argc, char **argv) {
  // GC_use_threads_discovery();
  GC_set_dont_precollect(1);

  const bool gcDiag = isTruthyEnvValue(getenv("MADLIB_GC_DIAG"));
  const uint64_t physicalMemory = getPhysicalMemoryBytes();
  const size_t defaultMinAlloc = clampSize((size_t)(physicalMemory / 128ULL), 32ULL * 1024ULL * 1024ULL, 512ULL * 1024ULL * 1024ULL);
  const size_t defaultInitialHeap = clampSize((size_t)(physicalMemory / 4ULL), 256ULL * 1024ULL * 1024ULL, 6ULL * 1024ULL * 1024ULL * 1024ULL);
  const int defaultFreeSpaceDivisor =
    physicalMemory >= (24ULL * 1024ULL * 1024ULL * 1024ULL) ? 4 :
    physicalMemory >= (12ULL * 1024ULL * 1024ULL * 1024ULL) ? 3 :
    2;

  // Runtime override for testing/tuning GC allocation cadence.
  const size_t minAlloc = parseSizeEnvOrDefault(getenv("MADLIB_GC_MIN_ALLOC_BYTES"), defaultMinAlloc);
  const char *gcInitialHeapEnv = getenv("GC_INITIAL_HEAP_SIZE");
  const int freeSpaceDivisor = parseIntEnvOrDefault(getenv("MADLIB_GC_FREE_SPACE_DIVISOR"), defaultFreeSpaceDivisor);

  GC_set_min_bytes_allocd(minAlloc);

  // Respect GC_INITIAL_HEAP_SIZE when provided; otherwise use adaptive defaults.
  if (gcInitialHeapEnv == NULL || gcInitialHeapEnv[0] == '\0') {
    GC_expand_hp(defaultInitialHeap);
  }
  GC_set_free_space_divisor(freeSpaceDivisor);

  if (gcDiag) {
    fprintf(
      stderr,
      "[madlib-gc] init: phys=%llu min_alloc=%zu initial_heap=%s free_space_divisor=%d\n",
      (unsigned long long)physicalMemory,
      minAlloc,
      (gcInitialHeapEnv == NULL || gcInitialHeapEnv[0] == '\0') ? "auto" : "env",
      freeSpaceDivisor
    );
  }

  ARGC = argc;
  ARGV = argv;

  size_t size = 64 * 1024 * 1024;  // 64MB
#if defined(_WIN32) || defined(__MINGW32__)
  char *newStack = (char *)VirtualAlloc(NULL, size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
#else
  char *newStack = (char *)mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE, -1, 0);
#endif
  char *stackBottom = newStack + size;

  GC_stack_base stackBase = {
    .mem_base = (void*)stackBottom
  };
  GC_set_stackbottom(NULL, &stackBase);

  GC_allow_register_threads();
  madlib__stack__init(stackBottom, __main__start__);
}

void madlib__gc__configureAfterInit();

void madlib__process__internal__initExtra() {
  GC_INIT();
  madlib__gc__configureAfterInit();
  if (getenv("UV_THREADPOOL_SIZE") == NULL) {
    unsigned int cpuCount = std::thread::hardware_concurrency();
    if (cpuCount == 0) {
      cpuCount = 4;
    }
    unsigned int poolSize = cpuCount * 2;
    if (poolSize < 4) {
      poolSize = 4;
    } else if (poolSize > 32) {
      poolSize = 32;
    }

    char *threadPoolSetting = (char *)GC_MALLOC_ATOMIC(32);
    snprintf(threadPoolSetting, 32, "UV_THREADPOOL_SIZE=%u", poolSize);
    putenv(threadPoolSetting);
  }
  __args__ = madlib__list__empty();

  srand(time(NULL));

  for (int i = ARGC - 1; i >= 0; i--) {
    __args__ = madlib__list__push(ARGV[i], __args__);
  }
}

void madlib__process__internal__typedHoleReached() {
  fprintf(stderr, "Typed hole reached, exiting.\n");
  exit(1);
}

void madlib__process__internal__arrayOutOfBounds(int64_t index, int64_t len) {
  fprintf(stderr, "Array out of bounds access\nYou accessed the index '%lld' but the array currently has length '%lld'.\n", index, len);
  exit(1);
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
    res[strlen(cwd)] = '\0';
    return res;
  } else {
    return (char*)"";
  }
}


char *madlib__process__internal__getExecutablePath() {
  char exePath[PATH_MAX];
  size_t size = PATH_MAX;

  int r = uv_exepath(exePath, &size);
  if (r == 0) {
    char *res = (char*) GC_MALLOC_ATOMIC(size + 1);
    memcpy(res, exePath, size);
    res[size] = '\0';
    return res;
  } else {
    return (char*)"";
  }
}


// exec


void onChildClose(uv_handle_t *handle) {}


void onChildExit(uv_process_t *req, int64_t exitCode, int termSignal) {
  ExecData_t *data = (ExecData_t*)req->data;
  data->stopped = true;

  int64_t *boxedStatus = (int64_t*)exitCode;

  data->stdoutOutput[data->stdoutSize] = '\0';
  data->stderrOutput[data->stderrSize] = '\0';

  uv_close((uv_handle_t*) req, onChildClose);
  __applyPAP__(data->callback, 3, boxedStatus, data->stdoutOutput, data->stderrOutput);
}


void allocExecBuffer(uv_handle_t *handle, size_t suggestedSize, uv_buf_t *buffer) {
  *buffer = uv_buf_init((char*)GC_MALLOC_ATOMIC(suggestedSize + 1), suggestedSize);
}


void onPipeClose(uv_handle_t *handle) {}


void onExecStdoutRead(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
  ExecData_t *data = (ExecData_t*)stream->data;

  if (nread > 0) {
    appendOutputChunk(&data->stdoutOutput, &data->stdoutSize, &data->stdoutCapacity, buf->base, (size_t)nread);
    if (buf->base != NULL) {
      GC_FREE(buf->base);
    }
  } else {
    if (buf->base != NULL) {
      GC_FREE(buf->base);
    }
    if (nread < 0 && nread != UV_EOF) {
      fprintf(stderr, "exec stdout read error: %s\n", uv_strerror(nread));
    }
    data->stopped = true;
    uv_close((uv_handle_t*) stream, onPipeClose);
  }
}

void onExecStderrRead(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
  ExecData_t *data = (ExecData_t*)stream->data;

  if (nread > 0) {
    appendOutputChunk(&data->stderrOutput, &data->stderrSize, &data->stderrCapacity, buf->base, (size_t)nread);
    if (buf->base != NULL) {
      GC_FREE(buf->base);
    }
  } else {
    if (buf->base != NULL) {
      GC_FREE(buf->base);
    }
    if (nread < 0 && nread != UV_EOF) {
      fprintf(stderr, "exec stderr read error: %s\n", uv_strerror(nread));
    }
    data->stopped = true;
    uv_close((uv_handle_t*) stream, onPipeClose);
  }
}

ExecData_t *madlib__process__exec(char *command, madlib__list__Node_t *argList, madlib__process__CommandOptions_t *commandOptions, PAP_t *callback) {
  uv_process_t *childReq = (uv_process_t*)GC_MALLOC(sizeof(uv_process_t));
  uv_process_options_t *options = (uv_process_options_t*)GC_MALLOC(sizeof(uv_process_options_t));
  uv_pipe_t *stdoutPipe = (uv_pipe_t*)GC_MALLOC(sizeof(uv_pipe_t));
  uv_pipe_t *stderrPipe = (uv_pipe_t*)GC_MALLOC(sizeof(uv_pipe_t));

  ExecData_t *data = (ExecData_t*)GC_MALLOC(sizeof(ExecData_t));
  data->callback = callback;
  data->req = childReq;
  data->options = options;
  data->stdoutSize = 0;
  data->stderrSize = 0;
  data->stdoutCapacity = 64;
  data->stderrCapacity = 64;
  data->stdoutOutput = (char*)GC_MALLOC_ATOMIC(data->stdoutCapacity);
  (data->stdoutOutput)[0] = '\0';
  data->stderrOutput = (char*)GC_MALLOC_ATOMIC(data->stderrCapacity);
  (data->stderrOutput)[0] = '\0';
  data->stdoutPipe = (uv_stream_t *)stdoutPipe;
  data->stderrPipe = (uv_stream_t *)stderrPipe;
  data->canceled = false;
  data->started = false;
  data->stopped = false;

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

  madlib__list__Node_t *envItems = (madlib__list__Node_t *)commandOptions->env;
  int itemCount = madlib__list__length(envItems);
  char **env = (char**)GC_MALLOC(sizeof(char*) * (itemCount + 1));
  int index = 0;

  while (envItems->next != NULL) {
    char *key = (char*)((madlib__tuple__Tuple_2_t*)envItems->value)->first;
    size_t keyLength = strlen(key);
    char *value = (char*)((madlib__tuple__Tuple_2_t*)envItems->value)->second;
    size_t valueLength = strlen(value);

    env[index] = (char*) GC_MALLOC_ATOMIC(keyLength + valueLength + 2);
    memcpy(env[index], key, keyLength);
    env[index][keyLength] = '=';
    memcpy(env[index] + keyLength + 1, value, valueLength);
    env[index][keyLength + 1 + valueLength] = '\0';

    envItems = envItems->next;
    index += 1;
  }
  env[itemCount] = 0; // must be null terminated from libuv docs

  options->exit_cb = onChildExit;
  options->file = command;
  options->args = args;
  options->env = itemCount == 0 ? NULL : env;
  options->flags = 0;
  options->cwd = (const char *)commandOptions->cwd;

  int spawnResult = uv_spawn(getLoop(), childReq, options);

  #ifndef __MINGW32__
    if (argc > 0) {
      globfree(&globBuffer);
    }
  #endif // __MINGW32__

  if (spawnResult) {
    int64_t *boxedStatus = (int64_t*)1;

    size_t stdoutLength = strlen(data->stdoutOutput);
    size_t stderrLength = strlen(data->stderrOutput);
    data->stopped = true;

    __applyPAP__(callback, 3, boxedStatus, data->stdoutOutput, data->stderrOutput);

    return NULL;
  } else {
    data->started = true;
    int rout = uv_read_start((uv_stream_t *)stdoutPipe, allocExecBuffer, onExecStdoutRead);
    int rerr = uv_read_start((uv_stream_t *)stderrPipe, allocExecBuffer, onExecStderrRead);

    return data;
  }
}

void madlib__process__cancelExec(ExecData_t *data) {
  if (!data || data->stopped) {
    return;
  }

  data->canceled = true;

  uv_close((uv_handle_t*) data->stdoutPipe, onPipeClose);
  uv_close((uv_handle_t*) data->stderrPipe, onPipeClose);
  uv_close((uv_handle_t*) data->req, onChildClose);
}


// buffered exec

void onBufferedChildExit(uv_process_t *req, int64_t exitCode, int termSignal) {
  BufferedExecData_t *data = (BufferedExecData_t*)req->data;
  data->stopped = true;

  int64_t *boxedStatus = (int64_t*)exitCode;

  uv_close((uv_handle_t*) req, onChildClose);
  __applyPAP__(data->doneCallback, 1, boxedStatus);
}

void onBufferedExecStdoutRead(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
  BufferedExecData_t *handle = (BufferedExecData_t*)stream->data;

  if (nread > 0) {
    buf->base[nread] = '\0';
    __applyPAP__(handle->callback, 2, buf->base, "");
  } else {
    if (buf->base != NULL) {
      GC_FREE(buf->base);
    }
    if (nread < 0 && nread != UV_EOF) {
      fprintf(stderr, "buffered exec stdout read error: %s\n", uv_strerror(nread));
    }
    handle->stopped = true;
    uv_close((uv_handle_t*) stream, onPipeClose);
  }
}

void onBufferedExecStderrRead(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
  BufferedExecData_t *handle = (BufferedExecData_t*)stream->data;

  if (nread > 0) {
    buf->base[nread] = '\0';
    __applyPAP__(handle->callback, 2, "", buf->base);
  } else {
    if (buf->base != NULL) {
      GC_FREE(buf->base);
    }
    if (nread < 0 && nread != UV_EOF) {
      fprintf(stderr, "buffered exec stderr read error: %s\n", uv_strerror(nread));
    }
    handle->stopped = true;
    uv_close((uv_handle_t*) stream, onPipeClose);
  }
}

BufferedExecData_t *madlib__process__bufferedExec(char *command, madlib__list__Node_t *argList, madlib__process__CommandOptions_t *commandOptions, PAP_t *callback, PAP_t *doneCallback) {
  uv_process_t *childReq = (uv_process_t*)GC_MALLOC(sizeof(uv_process_t));
  uv_process_options_t *options = (uv_process_options_t*)GC_MALLOC(sizeof(uv_process_options_t));
  uv_pipe_t *stdoutPipe = (uv_pipe_t*)GC_MALLOC(sizeof(uv_pipe_t));
  uv_pipe_t *stderrPipe = (uv_pipe_t*)GC_MALLOC(sizeof(uv_pipe_t));

  BufferedExecData_t *data = (BufferedExecData_t*)GC_MALLOC(sizeof(BufferedExecData_t));
  data->callback = callback;
  data->doneCallback = doneCallback;
  data->req = childReq;
  data->options = options;
  data->stdoutSize = 0;
  data->stderrSize = 0;
  data->stdoutCapacity = 0;
  data->stderrCapacity = 0;
  data->stdoutOutput = (char*)GC_MALLOC_ATOMIC(sizeof(char));
  (data->stdoutOutput)[0] = '\0';
  data->stderrOutput = (char*)GC_MALLOC_ATOMIC(sizeof(char));
  (data->stderrOutput)[0] = '\0';
  data->stdoutPipe = (uv_stream_t *)stdoutPipe;
  data->stderrPipe = (uv_stream_t *)stderrPipe;
  data->canceled = false;
  data->started = false;
  data->stopped = false;

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

  madlib__list__Node_t *envItems = (madlib__list__Node_t *)commandOptions->env;
  int itemCount = madlib__list__length(envItems);
  char **env = (char**)GC_MALLOC(sizeof(char*) * (itemCount + 1));
  int index = 0;

  while (envItems->next != NULL) {
    char *key = (char*)((madlib__tuple__Tuple_2_t*)envItems->value)->first;
    size_t keyLength = strlen(key);
    char *value = (char*)((madlib__tuple__Tuple_2_t*)envItems->value)->second;
    size_t valueLength = strlen(value);

    env[index] = (char*) GC_MALLOC_ATOMIC(keyLength + valueLength + 2);
    memcpy(env[index], key, keyLength);
    env[index][keyLength] = '=';
    memcpy(env[index] + keyLength + 1, value, valueLength);
    env[index][keyLength + 1 + valueLength] = '\0';

    envItems = envItems->next;
    index += 1;
  }
  env[itemCount] = 0; // must be null terminated from libuv docs

  options->exit_cb = onBufferedChildExit;
  options->file = command;
  options->args = args;
  options->env = itemCount == 0 ? NULL : env;
  options->flags = 0;
  options->cwd = (const char *)commandOptions->cwd;

  int spawnResult = uv_spawn(getLoop(), childReq, options);

  #ifndef __MINGW32__
    if (argc > 0) {
      globfree(&globBuffer);
    }
  #endif // __MINGW32__

  if (spawnResult) {
    int64_t *boxedStatus = (int64_t*)1;

    size_t stdoutLength = strlen(data->stdoutOutput);
    size_t stderrLength = strlen(data->stderrOutput);
    data->stopped = true;

    __applyPAP__(doneCallback, 1, boxedStatus);

    return NULL;
  } else {
    data->started = true;
    int rout = uv_read_start((uv_stream_t *)stdoutPipe, allocExecBuffer, onBufferedExecStdoutRead);
    int rerr = uv_read_start((uv_stream_t *)stderrPipe, allocExecBuffer, onBufferedExecStderrRead);

    return data;
  }
}

void madlib__process__cancelBufferedExec(BufferedExecData_t *data) {
  if (!data || data->stopped) {
    return;
  }

  data->canceled = true;

  uv_close((uv_handle_t*) data->stdoutPipe, onPipeClose);
  uv_close((uv_handle_t*) data->stderrPipe, onPipeClose);
  uv_close((uv_handle_t*) data->req, onChildClose);
}


// thread

typedef struct ThreadData {
  void *badCallback;
  void *goodCallback;
  void *threadFn;
  void *result;
  bool isBad;
  bool hasFinished;
} ThreadData_t;

void onThreadClose(uv_handle_t *handle) {}

void goodCallbackFn(uv_work_t *req, void *result) {
  ThreadData_t *data = (ThreadData_t*) req->data;
  data->result = result;
  data->isBad = false;
  data->hasFinished = true;
  uv_cancel((uv_req_t*)req);

  __applyPAP__(data->goodCallback, 1, result);
}


void badCallbackFn(uv_work_t *req, void *result) {
  ThreadData_t *data = (ThreadData_t*) req->data;
  data->result = result;
  data->isBad = true;
  data->hasFinished = true;
  uv_cancel((uv_req_t*)req);

  __applyPAP__(data->badCallback, 1, result);
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
  goodCallbackArg->env = NULL;
  goodCallbackArg->env_is_atomic = 0;

  PAP_t *badCallbackArg = (PAP_t*) GC_MALLOC(sizeof(PAP_t));
  badCallbackArg->fn = (void*)badCallbackFn;
  badCallbackArg->arity = 2;
  badCallbackArg->missingArgCount = 2;
  badCallbackArg->env = NULL;
  badCallbackArg->env_is_atomic = 0;

  PAP_t *goodCallbackArgWithData = (PAP_t*) __applyPAP__(goodCallbackArg, 1, req);
  PAP_t *badCallbackArgWithData = (PAP_t*) __applyPAP__(badCallbackArg, 1, req);

  __applyPAP__(data->threadFn, 2, badCallbackArgWithData, goodCallbackArgWithData);

  uv_run(threadLoop, UV_RUN_DEFAULT);

  GC_unregister_my_thread();

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
  bool hasFinished = data->hasFinished;
  GC_FREE(data);
  GC_FREE(req);

  if (!hasFinished) {
    __applyPAP__(badCallback, 1, result);
  }
}


uv_work_t * madlib__process__thread(PAP_t *fn, PAP_t *badCallback, PAP_t *goodCallback) {
  uv_work_t *req = (uv_work_t*) GC_MALLOC_UNCOLLECTABLE(sizeof(uv_work_t));
  ThreadData_t *data = (ThreadData_t*) GC_MALLOC_UNCOLLECTABLE(sizeof(ThreadData_t));
  data->badCallback = badCallback;
  data->goodCallback = goodCallback;
  data->threadFn = fn;
  data->hasFinished = false;
  req->data = data;
  int r = uv_queue_work(getLoop(), req, runThread, afterThread);

  return req;
}


void madlib__process__cancelThread(uv_work_t *req) {
  if (req != NULL) {
    uv_cancel((uv_req_t*)req);
  }
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
