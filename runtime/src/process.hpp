#ifndef PROCESS_H
#define PROCESS_H
#ifndef GC_THREADS
  #define GC_THREADS
#endif

#ifndef MADLIB_USE_RC
#include <gc.h>
#endif
#include <uv.h>
#include "list.hpp"
#include "dictionary.hpp"
#include "record.hpp"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct ExecData {
  void *callback;
  uv_stream_t *stdoutPipe;
  uv_stream_t *stderrPipe;
  uv_process_t *req;
  uv_process_options_t *options;

  char *stdoutOutput;
  char *stderrOutput;
  size_t stdoutSize;
  size_t stderrSize;
  size_t stdoutCapacity;
  size_t stderrCapacity;

  bool canceled;
  bool started;
  bool stopped;
} ExecData_t;

typedef struct BufferedExecData {
  void *callback;
  void *doneCallback;
  uv_stream_t *stdoutPipe;
  uv_stream_t *stderrPipe;
  uv_process_t *req;
  uv_process_options_t *options;

  char *stdoutOutput;
  char *stderrOutput;
  size_t stdoutSize;
  size_t stderrSize;
  size_t stdoutCapacity;
  size_t stderrCapacity;

  bool canceled;
  bool started;
  bool stopped;
} BufferedExecData_t;

// Flattened LLVM record for Process.FFICommandOptions
// Field order follows record-field alphabetical ordering in LLVM lowering:
//   { cwd, env }
typedef struct madlib__process__CommandOptions {
  void *cwd;
  void *env;
} madlib__process__CommandOptions_t;

void __main__init__(int argc, char **argv);

void madlib__process__internal__initExtra();

madlib__list__Node_t *madlib__process__internal__getArgs();

madlib__list__Node_t *madlib__process__internal__getEnv();

char *madlib__process__internal__getCurrentPath();

char *madlib__process__internal__getExecutablePath();

ExecData_t *madlib__process__exec(char *command, madlib__list__Node_t *argList, madlib__process__CommandOptions_t *options, PAP_t *callback);
void madlib__process__cancelExec(ExecData_t *data);

BufferedExecData_t *madlib__process__bufferedExec(char *command, madlib__list__Node_t *argList, madlib__process__CommandOptions_t *commandOptions, PAP_t *callback, PAP_t *doneCallback);
void madlib__process__cancelBufferedExec(BufferedExecData_t *data);

uv_work_t *madlib__process__thread(PAP_t *fn, PAP_t *badCallback, PAP_t *goodCallback);
void madlib__process__cancelThread(uv_work_t *req);

#ifdef __cplusplus
}
#endif

#endif // PROCESS_H
