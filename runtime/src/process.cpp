#include <sys/mman.h>
#include <cstring>
#include <gc.h>
#include "process.hpp"
#include "tuple.hpp"
#include "apply-pap.hpp"
#include "string.hpp"

#ifdef __cplusplus
extern "C" {
#endif

extern char **environ;

extern void __main__start__();
extern void madlib__stack__init(void*, void(*)());

static madlib__list__Node_t *args;

static int ARGC = 0;
static char **ARGV = NULL;


void __main__init__(int argc, char **argv) {
  GC_set_dont_precollect(1);

  ARGC = argc;
  ARGV = argv;

  size_t size = 1l * 1024 * 1024 * 1024 * 1024; // 1TB
  char *newStack = (char*)mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE, -1, 0);
  char *stackBottom = newStack + size;
  GC_stackbottom = stackBottom;

  madlib__stack__init(stackBottom, __main__start__);
}


void madlib__process__internal__registerArgs() {
  args = madlib__list__empty();

  for (int i = 0; i < ARGC; i++) {
    char **boxed = (char**)GC_malloc(sizeof(char*));
    *boxed = ARGV[i];
    args = madlib__list__append(boxed, args);
  }
}

madlib__list__Node_t *madlib__process__internal__getArgs() {
  return args;
}

madlib__dictionary__Dictionary_t *madlib__process__internal__getEnv() {
  madlib__list__Node_t *envItems = madlib__list__empty();
  char **env = environ;
  for (;*env != NULL; env++) {
    // *env has shape ENV_VAR=VALUE
    madlib__tuple__Tuple_2_t *item = (madlib__tuple__Tuple_2_t*)GC_malloc(sizeof(madlib__tuple__Tuple_2_t));
    size_t itemLength = strlen(*env);
    int keyLength = 0;

    for (; (*env)[keyLength] != '='; keyLength++) {}

    int valueLength = itemLength - keyLength - 1;

    char **boxedKey = (char**)GC_malloc(sizeof(char*));
    *boxedKey = (char*)GC_malloc(sizeof(char) * (keyLength + 1));
    strncpy(*boxedKey, *env, keyLength);
    (*boxedKey)[keyLength] = '\0';

    char **boxedValue = (char**)GC_malloc(sizeof(char*));
    *boxedValue = (char*)GC_malloc(sizeof(char) * (valueLength + 1));
    strncpy(*boxedValue, *env + keyLength + 1, valueLength);
    (*boxedValue)[valueLength] = '\0';

    item->first = boxedKey;
    item->second = boxedValue;

    envItems = madlib__list__append(item, envItems);
  }

  PAP_t stringEqPAP = { .fn = (void*)madlib__string__internal__eq, .arity = 2, .missingArgCount = 2, .env = NULL };
  madlib__eq__eqDictionary_t *stringEqDictionary = (madlib__eq__eqDictionary_t*)GC_malloc(sizeof(madlib__eq__eqDictionary_t));
  stringEqDictionary->eq = stringEqPAP;

  return madlib__dictionary__fromList(stringEqDictionary, envItems);
}

#ifdef __cplusplus
}
#endif
