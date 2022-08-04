#ifndef PROCESS_H
#define PROCESS_H

#include "list.hpp"
#include "dictionary.hpp"
#include "record.hpp"

#ifdef __cplusplus
extern "C" {
#endif

void __main__init__(int argc, char **argv);

void madlib__process__internal__initExtra();

madlib__list__Node_t *madlib__process__internal__getArgs();

madlib__list__Node_t *madlib__process__internal__getEnv();

char *madlib__process__internal__getCurrentPath();

void madlib__process__exec(char *command, madlib__list__Node_t *argList, madlib__record__Record_t *options, PAP_t *callback);

#ifdef __cplusplus
}
#endif

#endif // PROCESS_H
