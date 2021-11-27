#include <gc.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#include <future>
#include <chrono>
#include <thread>
#include <cmath>
#include <iostream>

// String

#ifdef __cplusplus
extern "C" {
#endif

bool __streq__(char *s1, char *s2) {
  if (strcmp(s1, s2) == 0) {
    return true;
  } else {
    return false;
  }
}

double *__strLength__(char *s) {
  double *result = (double *)GC_malloc(sizeof(double));
  *result = strlen(s);
  return result;
}

char *__strConcat__(char *s1, char *s2) {
  char *result = (char*)GC_malloc(strlen(s1) + strlen(s2) + 1);
  strcpy(result, s1);
  strcat(result, s2);
  return result;
}

char *__stripTrailingZeros__(char *number) {
  int length = strlen(number);
  char *end = number + strlen(number) - 1;
  int charsToRemove = 0;

  while (*end == '0' && charsToRemove < length) {
    charsToRemove += 1;
    end -= 1;
  }

  if (*end == '.') {
    charsToRemove += 1;
  }

  char *result = (char *)GC_malloc(length - charsToRemove + 1);
  memcpy(result, number, length - charsToRemove);
  result[length - charsToRemove] = '\0';

  return result;
}

#ifdef __cplusplus
}
#endif

// Show
#ifdef __cplusplus
extern "C" {
#endif

char *__doubleToStr__(double *d) {
  char *str = (char *)GC_malloc(200);
  sprintf(str, "%.20f", *d);
  return __stripTrailingZeros__(str);
}

char *__booleanToStr__(bool *b) {
  if (*b) {
    char *str = (char *)GC_malloc(5);
    str[0] = 't';
    str[1] = 'r';
    str[2] = 'u';
    str[3] = 'e';
    str[4] = '\0';
    return str;
  } else {
    char *str = (char *)GC_malloc(6);
    str[0] = 'f';
    str[1] = 'a';
    str[2] = 'l';
    str[3] = 's';
    str[4] = 'e';
    str[5] = '\0';
    return str;
  }
}

#ifdef __cplusplus
}
#endif


// ClassRefPred

// #ifdef __cplusplus
// extern "C" {
// #endif
// void* __ApplyDicts__(void* dict, int32_t methodCount, int32_t dictCount, ...) {
//   va_list dicts;
//   va_start(dicts, dictCount);
// }
// #ifdef __cplusplus
// }
// #endif


// Partial application
#ifdef __cplusplus
extern "C" {
#endif

typedef struct PAPEnv_1 {
  void *arg0;
} PAPEnv_1_t;

typedef struct PAPEnv_2 {
  void *arg0;
  void *arg1;
} PAPEnv_2_t;

typedef struct PAPEnv_3 {
  void *arg0;
  void *arg1;
  void *arg2;
} PAPEnv_3_t;

typedef struct PAPEnv_4 {
  void *arg0;
  void *arg1;
  void *arg2;
  void *arg3;
} PAPEnv_4_t;

typedef struct PAPEnv_5 {
  void *arg0;
  void *arg1;
  void *arg2;
  void *arg3;
  void *arg4;
} PAPEnv_5_t;

typedef struct PAPEnv_6 {
  void *arg0;
  void *arg1;
  void *arg2;
  void *arg3;
  void *arg4;
  void *arg5;
} PAPEnv_6_t;

typedef struct PAPEnv_7 {
  void *arg0;
  void *arg1;
  void *arg2;
  void *arg3;
  void *arg4;
  void *arg5;
  void *arg6;
} PAPEnv_7_t;

typedef struct PAPEnv_8 {
  void *arg0;
  void *arg1;
  void *arg2;
  void *arg3;
  void *arg4;
  void *arg5;
  void *arg6;
  void *arg7;
} PAPEnv_8_t;

typedef struct PAPEnv_9 {
  void *arg0;
  void *arg1;
  void *arg2;
  void *arg3;
  void *arg4;
  void *arg5;
  void *arg6;
  void *arg7;
  void *arg8;
} PAPEnv_9_t;

typedef struct PAPEnv_10 {
  void *arg0;
  void *arg1;
  void *arg2;
  void *arg3;
  void *arg4;
  void *arg5;
  void *arg6;
  void *arg7;
  void *arg8;
  void *arg9;
} PAPEnv_10_t;

typedef struct PAP {
  void *fn;
  int32_t arity;
  int32_t missingArgCount;
  void *env;
} PAP_t;

void *__applyPAP__(void *pap, int32_t argc, ...) {
  va_list argv;
  va_start(argv, argc);

  PAP_t *unwrappedPAP = (PAP_t *)pap;
  int32_t ENV_SIZE = unwrappedPAP->arity - unwrappedPAP->missingArgCount;
  int32_t ARITY = unwrappedPAP->arity;


  // if (ENV_SIZE == 19) {
  //   unwrappedPAP = (PAP_t *)&pap;
  //   ENV_SIZE = unwrappedPAP->arity - unwrappedPAP->missingArgCount;
  //   ARITY = unwrappedPAP->arity;
  // }

  // printf("FN_POINTER: %d\n", unwrappedPAP->fn);
  // printf("FN_POINTER_: %d\n", &unwrappedPAP->fn);
  // printf("PAP: %d\n", pap);
  // printf("PAP_: %d\n",  *((int*)pap));
  // printf("ENV_SIZE: %d\n", ENV_SIZE);
  // printf("ARITY: %d\n", ARITY);
  // printf("argc: %d\n", argc);
  // printf("missing: %d\n", unwrappedPAP->missingArgCount);

  if (argc >= unwrappedPAP->missingArgCount) {
    // We need to do the call
    // printf("doing call - argc: %d, ENV_SIZE: %d, ARITY: %d\n", argc, ENV_SIZE, ARITY);
    switch (ARITY) {
      case 1: {
        void *(*fn)(void *) = (void *(*)(void *))unwrappedPAP->fn;
        void *result = fn(va_arg(argv, void *));
        if (argc > 1) {
          va_list *remainingArgs = va_arg(argv, va_list *);
          result = __applyPAP__(result, argc - 1, remainingArgs);
        }
        va_end(argv);
        return result;
      }
      case 2: {
        void *(*fn)(void *, void *) =
            (void *(*)(void *, void *))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *result = fn(va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 2) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 2, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 1: {
            PAPEnv_1_t *env = (PAPEnv_1_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, va_arg(argv, void *));
            if (argc > 1) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 1, remainingArgs);
            }
            va_end(argv);
            return result;
          }
        }
      }
      case 3: {
        void *(*fn)(void *, void *, void *) =
            (void *(*)(void *, void *, void *))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *result = fn(va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *));
            if (argc > 3) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 3, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 1: {
            PAPEnv_1_t *env = (PAPEnv_1_t *)unwrappedPAP->env;
            void *result =
                fn(env->arg0, va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 2) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 2, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 2: {
            PAPEnv_2_t *env = (PAPEnv_2_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, va_arg(argv, void *));
            if (argc > 1) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 1, remainingArgs);
            }
            va_end(argv);
            return result;
          }
        }
      }
      case 4: {
        void *(*fn)(void *, void *, void *, void*) =
            (void *(*)(void *, void *, void *, void*))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *result = fn(va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 4) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 4, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 1: {
            PAPEnv_1_t *env = (PAPEnv_1_t *)unwrappedPAP->env;
            void *result =
                fn(env->arg0, va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 3) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 3, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 2: {
            PAPEnv_2_t *env = (PAPEnv_2_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 2) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 2, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 3: {
            PAPEnv_3_t *env = (PAPEnv_3_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, va_arg(argv, void *));
            if (argc > 1) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 1, remainingArgs);
            }
            va_end(argv);
            return result;
          }
        }
      }
      case 5: {
        void *(*fn)(void *, void *, void *, void*, void*) =
            (void *(*)(void *, void *, void *, void*, void*))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *result = fn(va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 5) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 5, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 1: {
            PAPEnv_1_t *env = (PAPEnv_1_t *)unwrappedPAP->env;
            void *result =
                fn(env->arg0, va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 4) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 4, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 2: {
            PAPEnv_2_t *env = (PAPEnv_2_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 3) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 3, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 3: {
            PAPEnv_3_t *env = (PAPEnv_3_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 2) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 2, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 4: {
            PAPEnv_4_t *env = (PAPEnv_4_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, env->arg3, va_arg(argv, void *));
            if (argc > 1) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 1, remainingArgs);
            }
            va_end(argv);
            return result;
          }
        }
      }
      case 6: {
        void *(*fn)(void *, void *, void *, void*, void*, void*) =
            (void *(*)(void *, void *, void *, void*, void*, void*))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *result = fn(va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 6) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 6, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 1: {
            PAPEnv_1_t *env = (PAPEnv_1_t *)unwrappedPAP->env;
            void *result =
                fn(env->arg0, va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 5) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 5, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 2: {
            PAPEnv_2_t *env = (PAPEnv_2_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 4) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 4, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 3: {
            PAPEnv_3_t *env = (PAPEnv_3_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 3) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 3, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 4: {
            PAPEnv_4_t *env = (PAPEnv_4_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, env->arg3, va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 2) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 2, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 5: {
            PAPEnv_5_t *env = (PAPEnv_5_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, va_arg(argv, void *));
            if (argc > 1) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 1, remainingArgs);
            }
            va_end(argv);
            return result;
          }
        }
      }
      case 7: {
        void *(*fn)(void *, void *, void *, void*, void*, void*, void*) =
            (void *(*)(void *, void *, void *, void*, void*, void*, void*))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *result = fn(va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 7) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 7, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 1: {
            PAPEnv_1_t *env = (PAPEnv_1_t *)unwrappedPAP->env;
            void *result =
                fn(env->arg0, va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 6) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 6, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 2: {
            PAPEnv_2_t *env = (PAPEnv_2_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 5) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 5, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 3: {
            PAPEnv_3_t *env = (PAPEnv_3_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 4) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 4, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 4: {
            PAPEnv_4_t *env = (PAPEnv_4_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, env->arg3, va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 3) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 3, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 5: {
            PAPEnv_5_t *env = (PAPEnv_5_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 2) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 2, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 6: {
            PAPEnv_6_t *env = (PAPEnv_6_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, va_arg(argv, void *));
            if (argc > 1) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 1, remainingArgs);
            }
            va_end(argv);
            return result;
          }
        }
      }
      case 8: {
        void *(*fn)(void *, void *, void *, void*, void*, void*, void*, void*) =
            (void *(*)(void *, void *, void *, void*, void*, void*, void*, void*))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *result = fn(va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 8) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 8, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 1: {
            PAPEnv_1_t *env = (PAPEnv_1_t *)unwrappedPAP->env;
            void *result =
                fn(env->arg0, va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 7) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 7, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 2: {
            PAPEnv_2_t *env = (PAPEnv_2_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 6) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 6, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 3: {
            PAPEnv_3_t *env = (PAPEnv_3_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 5) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 5, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 4: {
            PAPEnv_4_t *env = (PAPEnv_4_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, env->arg3, va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 4) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 4, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 5: {
            PAPEnv_5_t *env = (PAPEnv_5_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 3) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 3, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 6: {
            PAPEnv_6_t *env = (PAPEnv_6_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 2) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 2, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 7: {
            PAPEnv_7_t *env = (PAPEnv_7_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, env->arg6, va_arg(argv, void *));
            if (argc > 1) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 1, remainingArgs);
            }
            va_end(argv);
            return result;
          }
        }
      }
      case 9: {
        void *(*fn)(void *, void *, void *, void*, void*, void*, void*, void*, void*) =
            (void *(*)(void *, void *, void *, void*, void*, void*, void*, void*, void*))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *result = fn(va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 9) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 9, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 1: {
            PAPEnv_1_t *env = (PAPEnv_1_t *)unwrappedPAP->env;
            void *result =
                fn(env->arg0, va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 8) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 8, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 2: {
            PAPEnv_2_t *env = (PAPEnv_2_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 7) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 7, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 3: {
            PAPEnv_3_t *env = (PAPEnv_3_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 6) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 6, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 4: {
            PAPEnv_4_t *env = (PAPEnv_4_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, env->arg3, va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 5) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 5, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 5: {
            PAPEnv_5_t *env = (PAPEnv_5_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 4) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 4, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 6: {
            PAPEnv_6_t *env = (PAPEnv_6_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 3) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 3, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 7: {
            PAPEnv_7_t *env = (PAPEnv_7_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, env->arg6, va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 2) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 2, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 8: {
            PAPEnv_8_t *env = (PAPEnv_8_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, env->arg6, env->arg7, va_arg(argv, void *));
            if (argc > 1) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 1, remainingArgs);
            }
            va_end(argv);
            return result;
          }
        }
      }
      case 10: {
        void *(*fn)(void *, void *, void *, void *, void*, void*, void*, void*, void*, void*) =
            (void *(*)(void *, void *, void *, void *, void*, void*, void*, void*, void*, void*))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *result = fn(va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 10) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 10, remainingArgs);
            }
            va_end(argv);
            return result;
          }
        }
      }
      case 11: {
        void *(*fn)(void *, void *, void *, void *, void *, void*, void*, void*, void*, void*, void*) =
            (void *(*)(void *, void *, void *, void *, void *, void*, void*, void*, void*, void*, void*))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *result = fn(va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 11) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 11, remainingArgs);
            }
            va_end(argv);
            return result;
          }
        }
      }
    }
  } else {
    // We push the args to a newly allocated PAP
    int32_t NEXT_ENV_SIZE = argc + ENV_SIZE;
    PAP_t *newPAP = (PAP_t *)GC_malloc(sizeof(PAP_t));
    newPAP->fn = unwrappedPAP->fn;
    newPAP->arity = unwrappedPAP->arity;
    newPAP->missingArgCount = unwrappedPAP->missingArgCount - argc;

    // printf("NEW PAP - NEXT_ENV_SIZE: %d, argc: %d, ENV_SIZE: %d, arity: %d, missing: %d\n", NEXT_ENV_SIZE, argc, ENV_SIZE, unwrappedPAP->arity, newPAP->missingArgCount);

    switch (ENV_SIZE) {
      case 0: {
        switch (NEXT_ENV_SIZE) {
          case 1: {
            PAPEnv_1_t *newEnv = (PAPEnv_1_t *)GC_malloc(sizeof(PAPEnv_1_t));
            newEnv->arg0 = va_arg(argv, void *);
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 2: {
            PAPEnv_2_t *newEnv = (PAPEnv_2_t *)GC_malloc(sizeof(PAPEnv_2_t));
            newEnv->arg0 = va_arg(argv, void *);
            newEnv->arg1 = va_arg(argv, void *);
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 3: {
            PAPEnv_3_t *newEnv = (PAPEnv_3_t *)GC_malloc(sizeof(PAPEnv_3_t));
            newEnv->arg0 = va_arg(argv, void *);
            newEnv->arg1 = va_arg(argv, void *);
            newEnv->arg2 = va_arg(argv, void *);
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 4: {
            PAPEnv_4_t *newEnv = (PAPEnv_4_t *)GC_malloc(sizeof(PAPEnv_4_t));
            newEnv->arg0 = va_arg(argv, void *);
            newEnv->arg1 = va_arg(argv, void *);
            newEnv->arg2 = va_arg(argv, void *);
            newEnv->arg3 = va_arg(argv, void *);
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 5: {
            PAPEnv_5_t *newEnv = (PAPEnv_5_t *)GC_malloc(sizeof(PAPEnv_5_t));
            newEnv->arg0 = va_arg(argv, void *);
            newEnv->arg1 = va_arg(argv, void *);
            newEnv->arg2 = va_arg(argv, void *);
            newEnv->arg3 = va_arg(argv, void *);
            newEnv->arg4 = va_arg(argv, void *);
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 6: {
            PAPEnv_6_t *newEnv = (PAPEnv_6_t *)GC_malloc(sizeof(PAPEnv_6_t));
            newEnv->arg0 = va_arg(argv, void *);
            newEnv->arg1 = va_arg(argv, void *);
            newEnv->arg2 = va_arg(argv, void *);
            newEnv->arg3 = va_arg(argv, void *);
            newEnv->arg4 = va_arg(argv, void *);
            newEnv->arg5 = va_arg(argv, void *);
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 7: {
            PAPEnv_7_t *newEnv = (PAPEnv_7_t *)GC_malloc(sizeof(PAPEnv_7_t));
            newEnv->arg0 = va_arg(argv, void *);
            newEnv->arg1 = va_arg(argv, void *);
            newEnv->arg2 = va_arg(argv, void *);
            newEnv->arg3 = va_arg(argv, void *);
            newEnv->arg4 = va_arg(argv, void *);
            newEnv->arg5 = va_arg(argv, void *);
            newEnv->arg6 = va_arg(argv, void *);
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 8: {
            PAPEnv_8_t *newEnv = (PAPEnv_8_t *)GC_malloc(sizeof(PAPEnv_8_t));
            newEnv->arg0 = va_arg(argv, void *);
            newEnv->arg1 = va_arg(argv, void *);
            newEnv->arg2 = va_arg(argv, void *);
            newEnv->arg3 = va_arg(argv, void *);
            newEnv->arg4 = va_arg(argv, void *);
            newEnv->arg5 = va_arg(argv, void *);
            newEnv->arg6 = va_arg(argv, void *);
            newEnv->arg7 = va_arg(argv, void *);
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 9: {
            PAPEnv_9_t *newEnv = (PAPEnv_9_t *)GC_malloc(sizeof(PAPEnv_9_t));
            newEnv->arg0 = va_arg(argv, void *);
            newEnv->arg1 = va_arg(argv, void *);
            newEnv->arg2 = va_arg(argv, void *);
            newEnv->arg3 = va_arg(argv, void *);
            newEnv->arg4 = va_arg(argv, void *);
            newEnv->arg5 = va_arg(argv, void *);
            newEnv->arg6 = va_arg(argv, void *);
            newEnv->arg7 = va_arg(argv, void *);
            newEnv->arg8 = va_arg(argv, void *);
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
        }
      }
      case 1: {
        PAPEnv_1_t *env = (PAPEnv_1_t *)unwrappedPAP->env;
        switch (NEXT_ENV_SIZE) {
          case 2: {
            PAPEnv_2_t *newEnv = (PAPEnv_2_t *)GC_malloc(sizeof(PAPEnv_2_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 3: {
            PAPEnv_3_t *newEnv = (PAPEnv_3_t *)GC_malloc(sizeof(PAPEnv_3_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = va_arg(argv, void *);
            newEnv->arg2 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 4: {
            PAPEnv_4_t *newEnv = (PAPEnv_4_t *)GC_malloc(sizeof(PAPEnv_4_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = va_arg(argv, void *);
            newEnv->arg2 = va_arg(argv, void *);
            newEnv->arg3 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 5: {
            PAPEnv_5_t *newEnv = (PAPEnv_5_t *)GC_malloc(sizeof(PAPEnv_5_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = va_arg(argv, void *);
            newEnv->arg2 = va_arg(argv, void *);
            newEnv->arg3 = va_arg(argv, void *);
            newEnv->arg4 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 6: {
            PAPEnv_6_t *newEnv = (PAPEnv_6_t *)GC_malloc(sizeof(PAPEnv_6_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = va_arg(argv, void *);
            newEnv->arg2 = va_arg(argv, void *);
            newEnv->arg3 = va_arg(argv, void *);
            newEnv->arg4 = va_arg(argv, void *);
            newEnv->arg5 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 7: {
            PAPEnv_7_t *newEnv = (PAPEnv_7_t *)GC_malloc(sizeof(PAPEnv_7_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = va_arg(argv, void *);
            newEnv->arg2 = va_arg(argv, void *);
            newEnv->arg3 = va_arg(argv, void *);
            newEnv->arg4 = va_arg(argv, void *);
            newEnv->arg5 = va_arg(argv, void *);
            newEnv->arg6 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 8: {
            PAPEnv_8_t *newEnv = (PAPEnv_8_t *)GC_malloc(sizeof(PAPEnv_8_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = va_arg(argv, void *);
            newEnv->arg2 = va_arg(argv, void *);
            newEnv->arg3 = va_arg(argv, void *);
            newEnv->arg4 = va_arg(argv, void *);
            newEnv->arg5 = va_arg(argv, void *);
            newEnv->arg6 = va_arg(argv, void *);
            newEnv->arg7 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 9: {
            PAPEnv_9_t *newEnv = (PAPEnv_9_t *)GC_malloc(sizeof(PAPEnv_9_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = va_arg(argv, void *);
            newEnv->arg2 = va_arg(argv, void *);
            newEnv->arg3 = va_arg(argv, void *);
            newEnv->arg4 = va_arg(argv, void *);
            newEnv->arg5 = va_arg(argv, void *);
            newEnv->arg6 = va_arg(argv, void *);
            newEnv->arg7 = va_arg(argv, void *);
            newEnv->arg8 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
        }
        break;
      }
      case 2: {
        PAPEnv_2_t *env = (PAPEnv_2_t *)unwrappedPAP->env;
        switch (NEXT_ENV_SIZE) {
          case 3: {
            PAPEnv_3_t *newEnv = (PAPEnv_3_t *)GC_malloc(sizeof(PAPEnv_3_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 4: {
            PAPEnv_4_t *newEnv = (PAPEnv_4_t *)GC_malloc(sizeof(PAPEnv_4_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = va_arg(argv, void *);
            newEnv->arg3 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 5: {
            PAPEnv_5_t *newEnv = (PAPEnv_5_t *)GC_malloc(sizeof(PAPEnv_5_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = va_arg(argv, void *);
            newEnv->arg3 = va_arg(argv, void *);
            newEnv->arg4 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 6: {
            PAPEnv_6_t *newEnv = (PAPEnv_6_t *)GC_malloc(sizeof(PAPEnv_6_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = va_arg(argv, void *);
            newEnv->arg3 = va_arg(argv, void *);
            newEnv->arg4 = va_arg(argv, void *);
            newEnv->arg5 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 7: {
            PAPEnv_7_t *newEnv = (PAPEnv_7_t *)GC_malloc(sizeof(PAPEnv_7_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = va_arg(argv, void *);
            newEnv->arg3 = va_arg(argv, void *);
            newEnv->arg4 = va_arg(argv, void *);
            newEnv->arg5 = va_arg(argv, void *);
            newEnv->arg6 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 8: {
            PAPEnv_8_t *newEnv = (PAPEnv_8_t *)GC_malloc(sizeof(PAPEnv_8_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = va_arg(argv, void *);
            newEnv->arg3 = va_arg(argv, void *);
            newEnv->arg4 = va_arg(argv, void *);
            newEnv->arg5 = va_arg(argv, void *);
            newEnv->arg6 = va_arg(argv, void *);
            newEnv->arg7 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 9: {
            PAPEnv_9_t *newEnv = (PAPEnv_9_t *)GC_malloc(sizeof(PAPEnv_9_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = va_arg(argv, void *);
            newEnv->arg3 = va_arg(argv, void *);
            newEnv->arg4 = va_arg(argv, void *);
            newEnv->arg5 = va_arg(argv, void *);
            newEnv->arg6 = va_arg(argv, void *);
            newEnv->arg7 = va_arg(argv, void *);
            newEnv->arg8 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
        }
      }
      case 3: {
        PAPEnv_3_t *env = (PAPEnv_3_t *)unwrappedPAP->env;
        switch (NEXT_ENV_SIZE) {
          case 4: {
            PAPEnv_4_t *newEnv = (PAPEnv_4_t *)GC_malloc(sizeof(PAPEnv_4_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 5: {
            PAPEnv_5_t *newEnv = (PAPEnv_5_t *)GC_malloc(sizeof(PAPEnv_5_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = va_arg(argv, void *);
            newEnv->arg4 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 6: {
            PAPEnv_6_t *newEnv = (PAPEnv_6_t *)GC_malloc(sizeof(PAPEnv_6_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = va_arg(argv, void *);
            newEnv->arg4 = va_arg(argv, void *);
            newEnv->arg5 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 7: {
            PAPEnv_7_t *newEnv = (PAPEnv_7_t *)GC_malloc(sizeof(PAPEnv_7_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = va_arg(argv, void *);
            newEnv->arg4 = va_arg(argv, void *);
            newEnv->arg5 = va_arg(argv, void *);
            newEnv->arg6 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 8: {
            PAPEnv_8_t *newEnv = (PAPEnv_8_t *)GC_malloc(sizeof(PAPEnv_8_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = va_arg(argv, void *);
            newEnv->arg4 = va_arg(argv, void *);
            newEnv->arg5 = va_arg(argv, void *);
            newEnv->arg6 = va_arg(argv, void *);
            newEnv->arg7 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 9: {
            PAPEnv_9_t *newEnv = (PAPEnv_9_t *)GC_malloc(sizeof(PAPEnv_9_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = va_arg(argv, void *);
            newEnv->arg4 = va_arg(argv, void *);
            newEnv->arg5 = va_arg(argv, void *);
            newEnv->arg6 = va_arg(argv, void *);
            newEnv->arg7 = va_arg(argv, void *);
            newEnv->arg8 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
        }
      }
      case 4: {
        PAPEnv_4_t *env = (PAPEnv_4_t *)unwrappedPAP->env;
        switch (NEXT_ENV_SIZE) {
          case 5: {
            PAPEnv_5_t *newEnv = (PAPEnv_5_t *)GC_malloc(sizeof(PAPEnv_5_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 6: {
            PAPEnv_6_t *newEnv = (PAPEnv_6_t *)GC_malloc(sizeof(PAPEnv_6_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = va_arg(argv, void *);
            newEnv->arg5 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 7: {
            PAPEnv_7_t *newEnv = (PAPEnv_7_t *)GC_malloc(sizeof(PAPEnv_7_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = va_arg(argv, void *);
            newEnv->arg5 = va_arg(argv, void *);
            newEnv->arg6 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 8: {
            PAPEnv_8_t *newEnv = (PAPEnv_8_t *)GC_malloc(sizeof(PAPEnv_8_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = va_arg(argv, void *);
            newEnv->arg5 = va_arg(argv, void *);
            newEnv->arg6 = va_arg(argv, void *);
            newEnv->arg7 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 9: {
            PAPEnv_9_t *newEnv = (PAPEnv_9_t *)GC_malloc(sizeof(PAPEnv_9_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = va_arg(argv, void *);
            newEnv->arg5 = va_arg(argv, void *);
            newEnv->arg6 = va_arg(argv, void *);
            newEnv->arg7 = va_arg(argv, void *);
            newEnv->arg8 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
        }
      }
      case 5: {
        PAPEnv_5_t *env = (PAPEnv_5_t *)unwrappedPAP->env;
        switch (NEXT_ENV_SIZE) {
          case 6: {
            PAPEnv_6_t *newEnv = (PAPEnv_6_t *)GC_malloc(sizeof(PAPEnv_6_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = env->arg4;
            newEnv->arg5 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 7: {
            PAPEnv_7_t *newEnv = (PAPEnv_7_t *)GC_malloc(sizeof(PAPEnv_7_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = env->arg4;
            newEnv->arg5 = va_arg(argv, void *);
            newEnv->arg6 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 8: {
            PAPEnv_8_t *newEnv = (PAPEnv_8_t *)GC_malloc(sizeof(PAPEnv_8_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = env->arg4;
            newEnv->arg5 = va_arg(argv, void *);
            newEnv->arg6 = va_arg(argv, void *);
            newEnv->arg7 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 9: {
            PAPEnv_9_t *newEnv = (PAPEnv_9_t *)GC_malloc(sizeof(PAPEnv_9_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = env->arg4;
            newEnv->arg5 = va_arg(argv, void *);
            newEnv->arg6 = va_arg(argv, void *);
            newEnv->arg7 = va_arg(argv, void *);
            newEnv->arg8 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
        }
      }
      case 6: {
        PAPEnv_6_t *env = (PAPEnv_6_t *)unwrappedPAP->env;
        switch (NEXT_ENV_SIZE) {
          case 7: {
            PAPEnv_7_t *newEnv = (PAPEnv_7_t *)GC_malloc(sizeof(PAPEnv_7_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = env->arg4;
            newEnv->arg5 = env->arg5;
            newEnv->arg6 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 8: {
            PAPEnv_8_t *newEnv = (PAPEnv_8_t *)GC_malloc(sizeof(PAPEnv_8_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = env->arg4;
            newEnv->arg5 = env->arg5;
            newEnv->arg6 = va_arg(argv, void *);
            newEnv->arg7 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 9: {
            PAPEnv_9_t *newEnv = (PAPEnv_9_t *)GC_malloc(sizeof(PAPEnv_9_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = env->arg4;
            newEnv->arg5 = env->arg5;
            newEnv->arg6 = va_arg(argv, void *);
            newEnv->arg7 = va_arg(argv, void *);
            newEnv->arg8 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
        }
      }
      case 7: {
        PAPEnv_7_t *env = (PAPEnv_7_t *)unwrappedPAP->env;
        switch (NEXT_ENV_SIZE) {
          case 8: {
            PAPEnv_8_t *newEnv = (PAPEnv_8_t *)GC_malloc(sizeof(PAPEnv_8_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = env->arg4;
            newEnv->arg5 = env->arg5;
            newEnv->arg6 = env->arg6;
            newEnv->arg7 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 9: {
            PAPEnv_9_t *newEnv = (PAPEnv_9_t *)GC_malloc(sizeof(PAPEnv_9_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = env->arg4;
            newEnv->arg5 = env->arg5;
            newEnv->arg6 = env->arg6;
            newEnv->arg7 = va_arg(argv, void *);
            newEnv->arg8 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
        }
      }
      case 8: {
        PAPEnv_8_t *env = (PAPEnv_8_t *)unwrappedPAP->env;
        switch (NEXT_ENV_SIZE) {
          case 9: {
            PAPEnv_9_t *newEnv = (PAPEnv_9_t *)GC_malloc(sizeof(PAPEnv_9_t));
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = env->arg4;
            newEnv->arg5 = env->arg5;
            newEnv->arg6 = env->arg6;
            newEnv->arg7 = env->arg7;
            newEnv->arg8 = va_arg(argv, void *);
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
        }
      }
    }
  }

  printf("__applyPAP__ case not handled, argc: %d, ENV_SIZE: %d, ARITY: %d\n", argc, ENV_SIZE, ARITY);

  return NULL;
}

#ifdef __cplusplus
}
#endif

// List

#ifdef __cplusplus
extern "C" {
#endif

typedef struct MadListNode {
  void *value;
  struct MadListNode *next;
} MadListNode_t;

MadListNode_t *MadList_singleton(void *item) {
  MadListNode_t *head = (MadListNode_t *)GC_malloc(sizeof(MadListNode_t));
  head->next = NULL;
  head->value = item;

  return head;
}

MadListNode_t *MadList_append(void *item, MadListNode_t *list) {
  if (list == NULL) {
    return MadList_singleton(item);
  }

  MadListNode_t *current = list;
  while (current->next != NULL) {
    current = current->next;
  }

  MadListNode_t *nextNode = (MadListNode_t *)GC_malloc(sizeof(MadListNode_t));
  nextNode->next = NULL;
  nextNode->value = item;

  current->next = nextNode;

  return list;
}

MadListNode_t *MadList_push(void *item, MadListNode_t *list) {
  if (list == NULL) {
    return MadList_singleton(item);
  }

  MadListNode_t *newHead = (MadListNode_t *)GC_malloc(sizeof(MadListNode_t));
  newHead->next = list;
  newHead->value = item;

  return newHead;
}

MadListNode_t *__MadList_push__(void *item, MadListNode_t *list) {
  return MadList_push(item, list);
}

MadListNode_t *MadList_map(PAP_t *pap, MadListNode_t *list) {
  MadListNode_t *newList = (MadListNode_t *)GC_malloc(sizeof(MadListNode_t));
  MadListNode_t *head = newList;
  MadListNode_t *current = list;

  newList->value = __applyPAP__(pap, 1, current->value);
  newList->next = NULL;
  current = current->next;

  while (current != NULL) {
    MadListNode_t *nextItem = (MadListNode_t *)GC_malloc(sizeof(MadListNode_t));
    nextItem->value = __applyPAP__(
        pap, 1, current->value);  // cls->fn(cls->env, current->value);
    nextItem->next = NULL;

    newList->next = nextItem;
    newList = newList->next;

    current = current->next;
  }

  return head;
}

void *MadList_nth(double index, MadListNode_t *list) {
  // empty list
  if (list == NULL) {
    return NULL;
  }

  int intIndex = floor(index);
  int currentIndex = 0;

  MadListNode_t *current = list;
  while (current->next != NULL && currentIndex < intIndex) {
    current = current->next;
  }

  if (current != NULL) {
    return current->value;
  } else {
    return NULL;
  }
}

void *MadList_length(MadListNode_t *list) {
  double *total = (double *)GC_malloc(sizeof(double));

  if (list == NULL) {
    *total = 0;
    return total;
  }

  *total = 1;

  while (list->next != NULL) {
    *total += 1;
    list = list->next;
  }

  return total;
}

bool MadList_hasMinLength(double l, MadListNode_t *list) {
  MadListNode_t *head = list;
  if (head == NULL) {
    return l == 0;
  }

  l -= 1;

  while (head->next != NULL && l > 0) {
    l -= 1;
    head = head->next;
  }

  return l == 0;
}

bool MadList_hasLength(double l, MadListNode_t *list) {
  MadListNode_t *head = list;
  if (head == NULL) {
    return l == 0;
  }

  l -= 1;

  while (head->next != NULL && l > 0) {
    l -= 1;
    head = head->next;
  }

  return l == 0 && head->next == NULL;
}


MadListNode_t *MadList_concat(MadListNode_t *a, MadListNode_t *b) {
  if (a == NULL) {
    return b;
  } else if (b == NULL) {
    return a;
  } else {
    MadListNode_t *newList = (MadListNode_t *)GC_malloc(sizeof(MadListNode_t));
    MadListNode_t *head = newList;
    MadListNode_t *current = a;

    newList->value = current->value;
    newList->next = NULL;
    current = current->next;

    while (current != NULL) {
      MadListNode_t *nextItem =
          (MadListNode_t *)GC_malloc(sizeof(MadListNode_t));
      nextItem->value = current->value;
      nextItem->next = NULL;

      newList->next = nextItem;
      newList = newList->next;

      current = current->next;
    }

    newList->next = b;
    return head;
  }
}

#ifdef __cplusplus
}
#endif



// Wish

#ifdef __cplusplus
extern "C" {
#endif

void *__after__(double *millis, void *value) {
  auto a1 = std::async(std::launch::async, [value]() { return value; });
  std::this_thread::sleep_for(std::chrono::milliseconds((long) std::round(*millis)));
  return a1.get();
}

#ifdef __cplusplus
}
#endif
