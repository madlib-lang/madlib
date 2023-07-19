#ifndef PROCESS_H
#define PROCESS_H

#include <gc.h>
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

  bool canceled;
  bool started;
  bool stopped;
} ExecData_t;

void __main__init__(int argc, char **argv);

void madlib__process__internal__initExtra();

madlib__list__Node_t *madlib__process__internal__getArgs();

madlib__list__Node_t *madlib__process__internal__getEnv();

char *madlib__process__internal__getCurrentPath();

ExecData_t *madlib__process__exec(char *command, madlib__list__Node_t *argList, madlib__record__Record_t *options, PAP_t *callback);
void madlib__process__cancelExec(ExecData_t *data);

#ifdef __cplusplus
}
#endif

#endif // PROCESS_H
