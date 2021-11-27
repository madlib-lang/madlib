#include <gc.h>
#include <iostream>

#include "apply-pap.hpp"

// Partial application

#ifdef __cplusplus
extern "C" {
#endif


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
    // printf("doing call - argc: %d, ENV_SIZE: %d, ARITY: %d\n", argc,
    // ENV_SIZE, ARITY);
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
        void *(*fn)(void *, void *, void *, void *) =
            (void *(*)(void *, void *, void *, void *))unwrappedPAP->fn;
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
            void *result = fn(env->arg0, va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 3) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 3, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 2: {
            PAPEnv_2_t *env = (PAPEnv_2_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, va_arg(argv, void *),
                              va_arg(argv, void *));
            if (argc > 2) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 2, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 3: {
            PAPEnv_3_t *env = (PAPEnv_3_t *)unwrappedPAP->env;
            void *result =
                fn(env->arg0, env->arg1, env->arg2, va_arg(argv, void *));
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
        void *(*fn)(void *, void *, void *, void *, void *) =
            (void *(*)(void *, void *, void *, void *, void *))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *result = fn(va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *));
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
                fn(env->arg0, va_arg(argv, void *), va_arg(argv, void *),
                   va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 4) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 4, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 2: {
            PAPEnv_2_t *env = (PAPEnv_2_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 3) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 3, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 3: {
            PAPEnv_3_t *env = (PAPEnv_3_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2,
                              va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 2) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 2, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 4: {
            PAPEnv_4_t *env = (PAPEnv_4_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, env->arg3,
                              va_arg(argv, void *));
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
        void *(*fn)(void *, void *, void *, void *, void *, void *) =
            (void *(*)(void *, void *, void *, void *, void *,
                       void *))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *result = fn(va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 6) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 6, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 1: {
            PAPEnv_1_t *env = (PAPEnv_1_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 5) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 5, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 2: {
            PAPEnv_2_t *env = (PAPEnv_2_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *));
            if (argc > 4) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 4, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 3: {
            PAPEnv_3_t *env = (PAPEnv_3_t *)unwrappedPAP->env;
            void *result =
                fn(env->arg0, env->arg1, env->arg2, va_arg(argv, void *),
                   va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 3) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 3, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 4: {
            PAPEnv_4_t *env = (PAPEnv_4_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, env->arg3,
                              va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 2) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 2, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 5: {
            PAPEnv_5_t *env = (PAPEnv_5_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, env->arg3,
                              env->arg4, va_arg(argv, void *));
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
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *) =
            (void *(*)(void *, void *, void *, void *, void *, void *,
                       void *))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *result = fn(va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *));
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
                fn(env->arg0, va_arg(argv, void *), va_arg(argv, void *),
                   va_arg(argv, void *), va_arg(argv, void *),
                   va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 6) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 6, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 2: {
            PAPEnv_2_t *env = (PAPEnv_2_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 5) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 5, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 3: {
            PAPEnv_3_t *env = (PAPEnv_3_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2,
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 4) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 4, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 4: {
            PAPEnv_4_t *env = (PAPEnv_4_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, env->arg3,
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *));
            if (argc > 3) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 3, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 5: {
            PAPEnv_5_t *env = (PAPEnv_5_t *)unwrappedPAP->env;
            void *result =
                fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4,
                   va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 2) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 2, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 6: {
            PAPEnv_6_t *env = (PAPEnv_6_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, env->arg3,
                              env->arg4, env->arg5, va_arg(argv, void *));
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
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *,
                    void *) =
            (void *(*)(void *, void *, void *, void *, void *, void *, void *,
                       void *))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *result = fn(va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 8) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 8, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 1: {
            PAPEnv_1_t *env = (PAPEnv_1_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 7) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 7, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 2: {
            PAPEnv_2_t *env = (PAPEnv_2_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *));
            if (argc > 6) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 6, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 3: {
            PAPEnv_3_t *env = (PAPEnv_3_t *)unwrappedPAP->env;
            void *result =
                fn(env->arg0, env->arg1, env->arg2, va_arg(argv, void *),
                   va_arg(argv, void *), va_arg(argv, void *),
                   va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 5) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 5, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 4: {
            PAPEnv_4_t *env = (PAPEnv_4_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, env->arg3,
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 4) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 4, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 5: {
            PAPEnv_5_t *env = (PAPEnv_5_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, env->arg3,
                              env->arg4, va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 3) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 3, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 6: {
            PAPEnv_6_t *env = (PAPEnv_6_t *)unwrappedPAP->env;
            void *result =
                fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4,
                   env->arg5, va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 2) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 2, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 7: {
            PAPEnv_7_t *env = (PAPEnv_7_t *)unwrappedPAP->env;
            void *result =
                fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4,
                   env->arg5, env->arg6, va_arg(argv, void *));
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
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *,
                    void *, void *) =
            (void *(*)(void *, void *, void *, void *, void *, void *, void *,
                       void *, void *))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *result = fn(va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *));
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
                fn(env->arg0, va_arg(argv, void *), va_arg(argv, void *),
                   va_arg(argv, void *), va_arg(argv, void *),
                   va_arg(argv, void *), va_arg(argv, void *),
                   va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 8) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 8, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 2: {
            PAPEnv_2_t *env = (PAPEnv_2_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 7) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 7, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 3: {
            PAPEnv_3_t *env = (PAPEnv_3_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2,
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 6) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 6, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 4: {
            PAPEnv_4_t *env = (PAPEnv_4_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, env->arg3,
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *));
            if (argc > 5) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 5, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 5: {
            PAPEnv_5_t *env = (PAPEnv_5_t *)unwrappedPAP->env;
            void *result =
                fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4,
                   va_arg(argv, void *), va_arg(argv, void *),
                   va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 4) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 4, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 6: {
            PAPEnv_6_t *env = (PAPEnv_6_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, env->arg3,
                              env->arg4, env->arg5, va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 3) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 3, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 7: {
            PAPEnv_7_t *env = (PAPEnv_7_t *)unwrappedPAP->env;
            void *result = fn(env->arg0, env->arg1, env->arg2, env->arg3,
                              env->arg4, env->arg5, env->arg6,
                              va_arg(argv, void *), va_arg(argv, void *));
            if (argc > 2) {
              va_list *remainingArgs = va_arg(argv, va_list *);
              result = __applyPAP__(result, argc - 2, remainingArgs);
            }
            va_end(argv);
            return result;
          }
          case 8: {
            PAPEnv_8_t *env = (PAPEnv_8_t *)unwrappedPAP->env;
            void *result =
                fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4,
                   env->arg5, env->arg6, env->arg7, va_arg(argv, void *));
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
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *,
                    void *, void *, void *) =
            (void *(*)(void *, void *, void *, void *, void *, void *, void *,
                       void *, void *, void *))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *result = fn(va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *));
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
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *,
                    void *, void *, void *, void *) =
            (void *(*)(void *, void *, void *, void *, void *, void *, void *,
                       void *, void *, void *, void *))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *result = fn(va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *), va_arg(argv, void *),
                              va_arg(argv, void *));
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

    // printf("NEW PAP - NEXT_ENV_SIZE: %d, argc: %d, ENV_SIZE: %d, arity: %d,
    // missing: %d\n", NEXT_ENV_SIZE, argc, ENV_SIZE, unwrappedPAP->arity,
    // newPAP->missingArgCount);

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

  printf("__applyPAP__ case not handled, argc: %d, ENV_SIZE: %d, ARITY: %d\n",
         argc, ENV_SIZE, ARITY);

  return NULL;
}

#ifdef __cplusplus
}
#endif
