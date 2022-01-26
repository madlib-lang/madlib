#ifndef PROCESS_H
#define PROCESS_H

#include "list.hpp"
#include "dictionary.hpp"

#ifdef __cplusplus
extern "C" {
#endif

void madlib__process__internal__registerArgs(int argc, char **argv);

madlib__list__Node_t *madlib__process__internal__getArgs();

madlib__dictionary__Dictionary_t *madlib__process__internal__getEnv();

#ifdef __cplusplus
}
#endif

#endif // PROCESS_H
