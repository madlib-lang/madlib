#include <gc.h>
#include "apply-pap.hpp"
#include <cstdarg>
#include <iostream>


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

  // printf("fn ptr: %d, arity: %d, argc: %d, args left: %d\n", unwrappedPAP->fn, ARITY, argc, ARITY - ENV_SIZE);

  if (argc >= unwrappedPAP->missingArgCount) {
    void *result = (void *)NULL;
    switch (ARITY) {
      case 1: {
        void *(*fn)(void *) = (void *(*)(void *))unwrappedPAP->fn;
        void *arg1 = va_arg(argv, void *);
        result = fn(arg1);
        break;
      }
      case 2: {
        void *(*fn)(void *, void *) = (void *(*)(void *, void *))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            result = fn(arg1, arg2);
            break;
          }
          case 1: {
            PAPEnv_1_t *env = (PAPEnv_1_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            result = fn(env->arg0, arg1);
            break;
          }
        }
        break;
      }
      case 3: {
        void *(*fn)(void *, void *, void *) = (void *(*)(void *, void *, void *))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            result = fn(arg1, arg2, arg3);
            break;
          }
          case 1: {
            PAPEnv_1_t *env = (PAPEnv_1_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            result = fn(env->arg0, arg1, arg2);
            break;
          }
          case 2: {
            PAPEnv_2_t *env = (PAPEnv_2_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, arg1);
            break;
          }
        }
        break;
      }
      case 4: {
        void *(*fn)(void *, void *, void *, void *) = (void *(*)(void *, void *, void *, void *))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            result = fn(arg1, arg2, arg3, arg4);
            break;
          }
          case 1: {
            PAPEnv_1_t *env = (PAPEnv_1_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            result = fn(env->arg0, arg1, arg2, arg3);
            break;
          }
          case 2: {
            PAPEnv_2_t *env = (PAPEnv_2_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, arg1, arg2);
            break;
          }
          case 3: {
            PAPEnv_3_t *env = (PAPEnv_3_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, arg1);
            break;
          }
        }
        break;
      }
      case 5: {
        void *(*fn)(void *, void *, void *, void *, void *) =
            (void *(*)(void *, void *, void *, void *, void *))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            result = fn(arg1, arg2, arg3, arg4, arg5);
            break;
          }
          case 1: {
            PAPEnv_1_t *env = (PAPEnv_1_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            result = fn(env->arg0, arg1, arg2, arg3, arg4);
            break;
          }
          case 2: {
            PAPEnv_2_t *env = (PAPEnv_2_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, arg1, arg2, arg3);
            break;
          }
          case 3: {
            PAPEnv_3_t *env = (PAPEnv_3_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, arg1, arg2);
            break;
          }
          case 4: {
            PAPEnv_4_t *env = (PAPEnv_4_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, arg1);
            break;
          }
        }
        break;
      }
      case 6: {
        void *(*fn)(void *, void *, void *, void *, void *, void *) =
            (void *(*)(void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            result = fn(arg1, arg2, arg3, arg4, arg5, arg6);
            break;
          }
          case 1: {
            PAPEnv_1_t *env = (PAPEnv_1_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            result = fn(env->arg0, arg1, arg2, arg3, arg4, arg5);
            break;
          }
          case 2: {
            PAPEnv_2_t *env = (PAPEnv_2_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, arg1, arg2, arg3, arg4);
            break;
          }
          case 3: {
            PAPEnv_3_t *env = (PAPEnv_3_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, arg1, arg2, arg3);
            break;
          }
          case 4: {
            PAPEnv_4_t *env = (PAPEnv_4_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, arg1, arg2);
            break;
          }
          case 5: {
            PAPEnv_5_t *env = (PAPEnv_5_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, arg1);
            break;
          }
        }
        break;
      }
      case 7: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *) =
            (void *(*)(void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            result = fn(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
            break;
          }
          case 1: {
            PAPEnv_1_t *env = (PAPEnv_1_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            result = fn(env->arg0, arg1, arg2, arg3, arg4, arg5, arg6);
            break;
          }
          case 2: {
            PAPEnv_2_t *env = (PAPEnv_2_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, arg1, arg2, arg3, arg4, arg5);
            break;
          }
          case 3: {
            PAPEnv_3_t *env = (PAPEnv_3_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, arg1, arg2, arg3, arg4);
            break;
          }
          case 4: {
            PAPEnv_4_t *env = (PAPEnv_4_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, arg1, arg2, arg3);
            break;
          }
          case 5: {
            PAPEnv_5_t *env = (PAPEnv_5_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, arg1, arg2);
            break;
          }
          case 6: {
            PAPEnv_6_t *env = (PAPEnv_6_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, arg1);
            break;
          }
        }
        break;
      }
      case 8: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *) =
            (void *(*)(void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            result = fn(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
            break;
          }
          case 1: {
            PAPEnv_1_t *env = (PAPEnv_1_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            result = fn(env->arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
            break;
          }
          case 2: {
            PAPEnv_2_t *env = (PAPEnv_2_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, arg1, arg2, arg3, arg4, arg5, arg6);
            break;
          }
          case 3: {
            PAPEnv_3_t *env = (PAPEnv_3_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, arg1, arg2, arg3, arg4, arg5);
            break;
          }
          case 4: {
            PAPEnv_4_t *env = (PAPEnv_4_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, arg1, arg2, arg3, arg4);
            break;
          }
          case 5: {
            PAPEnv_5_t *env = (PAPEnv_5_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, arg1, arg2, arg3);
            break;
          }
          case 6: {
            PAPEnv_6_t *env = (PAPEnv_6_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, arg1, arg2);
            break;
          }
          case 7: {
            PAPEnv_7_t *env = (PAPEnv_7_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, env->arg6, arg1);
            break;
          }
        }
        break;
      }
      case 9: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *) =
            (void *(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            result = fn(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
            break;
          }
          case 1: {
            PAPEnv_1_t *env = (PAPEnv_1_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            result = fn(env->arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
            break;
          }
          case 2: {
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            PAPEnv_2_t *env = (PAPEnv_2_t *)unwrappedPAP->env;
            result = fn(env->arg0, env->arg1, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
            break;
          }
          case 3: {
            PAPEnv_3_t *env = (PAPEnv_3_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, arg1, arg2, arg3, arg4, arg5, arg6);
            break;
          }
          case 4: {
            PAPEnv_4_t *env = (PAPEnv_4_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, arg1, arg2, arg3, arg4, arg5);
            break;
          }
          case 5: {
            PAPEnv_5_t *env = (PAPEnv_5_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, arg1, arg2, arg3, arg4);
            break;
          }
          case 6: {
            PAPEnv_6_t *env = (PAPEnv_6_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, arg1, arg2, arg3);
            break;
          }
          case 7: {
            PAPEnv_7_t *env = (PAPEnv_7_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, env->arg6,
                        arg1, arg2);
            break;
          }
          case 8: {
            PAPEnv_8_t *env = (PAPEnv_8_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, env->arg6, env->arg7, arg1);
            break;
          }
        }
        break;
      }
      case 10: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *) =
            (void *(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            result = fn(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
            break;
          }
          case 1: {
            PAPEnv_1_t *env = (PAPEnv_1_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            result = fn(env->arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
            break;
          }
          case 2: {
            PAPEnv_2_t *env = (PAPEnv_2_t *)unwrappedPAP->env;
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
            break;
          }
          case 3: {
            PAPEnv_3_t *env = (PAPEnv_3_t *)unwrappedPAP->env;
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
            break;
          }
          case 4: {
            PAPEnv_4_t *env = (PAPEnv_4_t *)unwrappedPAP->env;
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, arg4, arg5, arg6, arg7, arg8, arg9);
            break;
          }
          case 5: {
            PAPEnv_5_t *env = (PAPEnv_5_t *)unwrappedPAP->env;
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, arg5, arg6, arg7, arg8, arg9);
            break;
          }
          case 6: {
            PAPEnv_6_t *env = (PAPEnv_6_t *)unwrappedPAP->env;
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, arg6, arg7, arg8, arg9);
            break;
          }
          case 7: {
            PAPEnv_7_t *env = (PAPEnv_7_t *)unwrappedPAP->env;
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, env->arg6, arg7, arg8, arg9);
            break;
          }
          case 8: {
            PAPEnv_8_t *env = (PAPEnv_8_t *)unwrappedPAP->env;
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, env->arg6, env->arg7, arg8, arg9);
            break;
          }
          case 9: {
            PAPEnv_9_t *env = (PAPEnv_9_t *)unwrappedPAP->env;
            void *arg9 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, env->arg6, env->arg7, env->arg8, arg9);
            break;
          }
        }
        break;
      }
      case 11: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *) =
            (void *(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *,
                       void *))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            result = fn(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
            break;
          }
          case 1: {
            PAPEnv_1_t *env = (PAPEnv_1_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            result = fn(env->arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
            break;
          }
          case 2: {
            PAPEnv_2_t *env = (PAPEnv_2_t *)unwrappedPAP->env;
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
            break;
          }
          case 3: {
            PAPEnv_3_t *env = (PAPEnv_3_t *)unwrappedPAP->env;
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
            break;
          }
          case 4: {
            PAPEnv_4_t *env = (PAPEnv_4_t *)unwrappedPAP->env;
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
            break;
          }
          case 5: {
            PAPEnv_5_t *env = (PAPEnv_5_t *)unwrappedPAP->env;
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, arg5, arg6, arg7, arg8, arg9, arg10);
            break;
          }
          case 6: {
            PAPEnv_6_t *env = (PAPEnv_6_t *)unwrappedPAP->env;
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, arg6, arg7, arg8, arg9, arg10);
            break;
          }
          case 7: {
            PAPEnv_7_t *env = (PAPEnv_7_t *)unwrappedPAP->env;
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, env->arg6, arg7, arg8, arg9, arg10);
            break;
          }
          case 8: {
            PAPEnv_8_t *env = (PAPEnv_8_t *)unwrappedPAP->env;
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, env->arg6, env->arg7, arg8, arg9, arg10);
            break;
          }
          case 9: {
            PAPEnv_9_t *env = (PAPEnv_9_t *)unwrappedPAP->env;
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, env->arg6, env->arg7, env->arg8, arg9, arg10);
            break;
          }
          case 10: {
            PAPEnv_10_t *env = (PAPEnv_10_t *)unwrappedPAP->env;
            void *arg10 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, env->arg6, env->arg7, env->arg8, env->arg9, arg10);
            break;
          }
        }
        break;
      }
      case 12: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *) =
            (void *(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *,
                       void *))unwrappedPAP->fn;
        switch (ENV_SIZE) {
          case 0: {
            void *arg0 = va_arg(argv, void *);
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            result = fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
            break;
          }
          case 1: {
            PAPEnv_1_t *env = (PAPEnv_1_t *)unwrappedPAP->env;
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            result = fn(env->arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
            break;
          }
          case 2: {
            PAPEnv_2_t *env = (PAPEnv_2_t *)unwrappedPAP->env;
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
            break;
          }
          case 3: {
            PAPEnv_3_t *env = (PAPEnv_3_t *)unwrappedPAP->env;
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
            break;
          }
          case 4: {
            PAPEnv_4_t *env = (PAPEnv_4_t *)unwrappedPAP->env;
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
            break;
          }
          case 5: {
            PAPEnv_5_t *env = (PAPEnv_5_t *)unwrappedPAP->env;
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
            break;
          }
          case 6: {
            PAPEnv_6_t *env = (PAPEnv_6_t *)unwrappedPAP->env;
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, arg6, arg7, arg8, arg9, arg10, arg11);
            break;
          }
          case 7: {
            PAPEnv_7_t *env = (PAPEnv_7_t *)unwrappedPAP->env;
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, env->arg6, arg7, arg8, arg9, arg10, arg11);
            break;
          }
          case 8: {
            PAPEnv_8_t *env = (PAPEnv_8_t *)unwrappedPAP->env;
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, env->arg6, env->arg7, arg8, arg9, arg10, arg11);
            break;
          }
          case 9: {
            PAPEnv_9_t *env = (PAPEnv_9_t *)unwrappedPAP->env;
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, env->arg6, env->arg7, env->arg8, arg9, arg10, arg11);
            break;
          }
          case 10: {
            PAPEnv_10_t *env = (PAPEnv_10_t *)unwrappedPAP->env;
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, env->arg6, env->arg7, env->arg8, env->arg9, arg10, arg11);
            break;
          }
          case 11: {
            PAPEnv_11_t *env = (PAPEnv_11_t *)unwrappedPAP->env;
            void *arg11 = va_arg(argv, void *);
            result = fn(env->arg0, env->arg1, env->arg2, env->arg3, env->arg4, env->arg5, env->arg6, env->arg7,
                        env->arg8, env->arg9, env->arg10, arg11);
            break;
          }
        }
        break;
      }
    }

    if (argc > unwrappedPAP->missingArgCount) {
      int argsLeft = argc - unwrappedPAP->missingArgCount;
      switch (argsLeft) {
        case 1: {
          void *arg1 = va_arg(argv, void *);
          result = __applyPAP__(result, argsLeft, arg1);
          break;
        }
        case 2: {
          void *arg1 = va_arg(argv, void *);
          void *arg2 = va_arg(argv, void *);
          result = __applyPAP__(result, argsLeft, arg1, arg2);
          break;
        }
        case 3: {
          void *arg1 = va_arg(argv, void *);
          void *arg2 = va_arg(argv, void *);
          void *arg3 = va_arg(argv, void *);
          result = __applyPAP__(result, argsLeft, arg1, arg2, arg3);
          break;
        }
        case 4: {
          void *arg1 = va_arg(argv, void *);
          void *arg2 = va_arg(argv, void *);
          void *arg3 = va_arg(argv, void *);
          void *arg4 = va_arg(argv, void *);
          result = __applyPAP__(result, argsLeft, arg1, arg2, arg3, arg4);
          break;
        }
        case 5: {
          void *arg1 = va_arg(argv, void *);
          void *arg2 = va_arg(argv, void *);
          void *arg3 = va_arg(argv, void *);
          void *arg4 = va_arg(argv, void *);
          void *arg5 = va_arg(argv, void *);
          result = __applyPAP__(result, argsLeft, arg1, arg2, arg3, arg4, arg5);
          break;
        }
        case 6: {
          void *arg1 = va_arg(argv, void *);
          void *arg2 = va_arg(argv, void *);
          void *arg3 = va_arg(argv, void *);
          void *arg4 = va_arg(argv, void *);
          void *arg5 = va_arg(argv, void *);
          void *arg6 = va_arg(argv, void *);
          result = __applyPAP__(result, argsLeft, arg1, arg2, arg3, arg4, arg5, arg6);
          break;
        }
        case 7: {
          void *arg1 = va_arg(argv, void *);
          void *arg2 = va_arg(argv, void *);
          void *arg3 = va_arg(argv, void *);
          void *arg4 = va_arg(argv, void *);
          void *arg5 = va_arg(argv, void *);
          void *arg6 = va_arg(argv, void *);
          void *arg7 = va_arg(argv, void *);
          result = __applyPAP__(result, argsLeft, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
          break;
        }
        case 8: {
          void *arg1 = va_arg(argv, void *);
          void *arg2 = va_arg(argv, void *);
          void *arg3 = va_arg(argv, void *);
          void *arg4 = va_arg(argv, void *);
          void *arg5 = va_arg(argv, void *);
          void *arg6 = va_arg(argv, void *);
          void *arg7 = va_arg(argv, void *);
          void *arg8 = va_arg(argv, void *);
          result = __applyPAP__(result, argsLeft, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
          break;
        }
        case 9: {
          void *arg1 = va_arg(argv, void *);
          void *arg2 = va_arg(argv, void *);
          void *arg3 = va_arg(argv, void *);
          void *arg4 = va_arg(argv, void *);
          void *arg5 = va_arg(argv, void *);
          void *arg6 = va_arg(argv, void *);
          void *arg7 = va_arg(argv, void *);
          void *arg8 = va_arg(argv, void *);
          void *arg9 = va_arg(argv, void *);
          result = __applyPAP__(result, argsLeft, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
          break;
        }
        case 10: {
          void *arg1 = va_arg(argv, void *);
          void *arg2 = va_arg(argv, void *);
          void *arg3 = va_arg(argv, void *);
          void *arg4 = va_arg(argv, void *);
          void *arg5 = va_arg(argv, void *);
          void *arg6 = va_arg(argv, void *);
          void *arg7 = va_arg(argv, void *);
          void *arg8 = va_arg(argv, void *);
          void *arg9 = va_arg(argv, void *);
          void *arg10 = va_arg(argv, void *);
          result = __applyPAP__(result, argsLeft, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
          break;
        }
        case 11: {
          void *arg1 = va_arg(argv, void *);
          void *arg2 = va_arg(argv, void *);
          void *arg3 = va_arg(argv, void *);
          void *arg4 = va_arg(argv, void *);
          void *arg5 = va_arg(argv, void *);
          void *arg6 = va_arg(argv, void *);
          void *arg7 = va_arg(argv, void *);
          void *arg8 = va_arg(argv, void *);
          void *arg9 = va_arg(argv, void *);
          void *arg10 = va_arg(argv, void *);
          void *arg11 = va_arg(argv, void *);
          result = __applyPAP__(result, argsLeft, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
          break;
        }
        case 12: {
          void *arg1 = va_arg(argv, void *);
          void *arg2 = va_arg(argv, void *);
          void *arg3 = va_arg(argv, void *);
          void *arg4 = va_arg(argv, void *);
          void *arg5 = va_arg(argv, void *);
          void *arg6 = va_arg(argv, void *);
          void *arg7 = va_arg(argv, void *);
          void *arg8 = va_arg(argv, void *);
          void *arg9 = va_arg(argv, void *);
          void *arg10 = va_arg(argv, void *);
          void *arg11 = va_arg(argv, void *);
          void *arg12 = va_arg(argv, void *);
          result = __applyPAP__(result, argsLeft, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
          break;
        }
        case 13: {
          void *arg1 = va_arg(argv, void *);
          void *arg2 = va_arg(argv, void *);
          void *arg3 = va_arg(argv, void *);
          void *arg4 = va_arg(argv, void *);
          void *arg5 = va_arg(argv, void *);
          void *arg6 = va_arg(argv, void *);
          void *arg7 = va_arg(argv, void *);
          void *arg8 = va_arg(argv, void *);
          void *arg9 = va_arg(argv, void *);
          void *arg10 = va_arg(argv, void *);
          void *arg11 = va_arg(argv, void *);
          void *arg12 = va_arg(argv, void *);
          void *arg13 = va_arg(argv, void *);
          result = __applyPAP__(result, argsLeft, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
          break;
        }
      }
    }

    // if (!result) {
    //   printf("fn ptr: %d, arity: %d, argc: %d, args left: %d\n", unwrappedPAP->fn, ARITY, argc, ARITY - ENV_SIZE);
    //   printf("%d", result); 
    // }

    va_end(argv);
    return result;
  } else {
    // We push the args to a newly allocated PAP
    int32_t NEXT_ENV_SIZE = argc + ENV_SIZE;
    PAP_t *newPAP = (PAP_t *)GC_MALLOC(sizeof(PAP_t));
    newPAP->fn = unwrappedPAP->fn;
    newPAP->arity = unwrappedPAP->arity;
    newPAP->missingArgCount = unwrappedPAP->missingArgCount - argc;

    // printf("NEW PAP - NEXT_ENV_SIZE: %d, argc: %d, ENV_SIZE: %d, arity: %d, missing: %d\n", NEXT_ENV_SIZE, argc,
    // ENV_SIZE, unwrappedPAP->arity, newPAP->missingArgCount);

    switch (ENV_SIZE) {
      case 0: {
        switch (NEXT_ENV_SIZE) {
          case 1: {
            PAPEnv_1_t *newEnv = (PAPEnv_1_t *)GC_MALLOC(sizeof(PAPEnv_1_t));
            void *arg1 = va_arg(argv, void *);
            newEnv->arg0 = arg1;
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 2: {
            PAPEnv_2_t *newEnv = (PAPEnv_2_t *)GC_MALLOC(sizeof(PAPEnv_2_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            newEnv->arg0 = arg1;
            newEnv->arg1 = arg2;
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 3: {
            PAPEnv_3_t *newEnv = (PAPEnv_3_t *)GC_MALLOC(sizeof(PAPEnv_3_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            newEnv->arg0 = arg1;
            newEnv->arg1 = arg2;
            newEnv->arg2 = arg3;
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 4: {
            PAPEnv_4_t *newEnv = (PAPEnv_4_t *)GC_MALLOC(sizeof(PAPEnv_4_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            newEnv->arg0 = arg1;
            newEnv->arg1 = arg2;
            newEnv->arg2 = arg3;
            newEnv->arg3 = arg4;
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 5: {
            PAPEnv_5_t *newEnv = (PAPEnv_5_t *)GC_MALLOC(sizeof(PAPEnv_5_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            newEnv->arg0 = arg1;
            newEnv->arg1 = arg2;
            newEnv->arg2 = arg3;
            newEnv->arg3 = arg4;
            newEnv->arg4 = arg5;
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 6: {
            PAPEnv_6_t *newEnv = (PAPEnv_6_t *)GC_MALLOC(sizeof(PAPEnv_6_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            newEnv->arg0 = arg1;
            newEnv->arg1 = arg2;
            newEnv->arg2 = arg3;
            newEnv->arg3 = arg4;
            newEnv->arg4 = arg5;
            newEnv->arg5 = arg6;
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 7: {
            PAPEnv_7_t *newEnv = (PAPEnv_7_t *)GC_MALLOC(sizeof(PAPEnv_7_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            newEnv->arg0 = arg1;
            newEnv->arg1 = arg2;
            newEnv->arg2 = arg3;
            newEnv->arg3 = arg4;
            newEnv->arg4 = arg5;
            newEnv->arg5 = arg6;
            newEnv->arg6 = arg7;
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 8: {
            PAPEnv_8_t *newEnv = (PAPEnv_8_t *)GC_MALLOC(sizeof(PAPEnv_8_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            newEnv->arg0 = arg1;
            newEnv->arg1 = arg2;
            newEnv->arg2 = arg3;
            newEnv->arg3 = arg4;
            newEnv->arg4 = arg5;
            newEnv->arg5 = arg6;
            newEnv->arg6 = arg7;
            newEnv->arg7 = arg8;
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 9: {
            PAPEnv_9_t *newEnv = (PAPEnv_9_t *)GC_MALLOC(sizeof(PAPEnv_9_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            newEnv->arg0 = arg1;
            newEnv->arg1 = arg2;
            newEnv->arg2 = arg3;
            newEnv->arg3 = arg4;
            newEnv->arg4 = arg5;
            newEnv->arg5 = arg6;
            newEnv->arg6 = arg7;
            newEnv->arg7 = arg8;
            newEnv->arg8 = arg9;
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 10: {
            PAPEnv_10_t *newEnv = (PAPEnv_10_t *)GC_MALLOC(sizeof(PAPEnv_10_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            newEnv->arg0 = arg1;
            newEnv->arg1 = arg2;
            newEnv->arg2 = arg3;
            newEnv->arg3 = arg4;
            newEnv->arg4 = arg5;
            newEnv->arg5 = arg6;
            newEnv->arg6 = arg7;
            newEnv->arg7 = arg8;
            newEnv->arg8 = arg9;
            newEnv->arg9 = arg10;
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 11: {
            PAPEnv_11_t *newEnv = (PAPEnv_11_t *)GC_MALLOC(sizeof(PAPEnv_11_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            newEnv->arg0 = arg1;
            newEnv->arg1 = arg2;
            newEnv->arg2 = arg3;
            newEnv->arg3 = arg4;
            newEnv->arg4 = arg5;
            newEnv->arg5 = arg6;
            newEnv->arg6 = arg7;
            newEnv->arg7 = arg8;
            newEnv->arg8 = arg9;
            newEnv->arg9 = arg10;
            newEnv->arg10 = arg11;
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 12: {
            PAPEnv_12_t *newEnv = (PAPEnv_12_t *)GC_MALLOC(sizeof(PAPEnv_12_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            void *arg12 = va_arg(argv, void *);
            newEnv->arg0 = arg1;
            newEnv->arg1 = arg2;
            newEnv->arg2 = arg3;
            newEnv->arg3 = arg4;
            newEnv->arg4 = arg5;
            newEnv->arg5 = arg6;
            newEnv->arg6 = arg7;
            newEnv->arg7 = arg8;
            newEnv->arg8 = arg9;
            newEnv->arg9 = arg10;
            newEnv->arg10 = arg11;
            newEnv->arg11 = arg12;
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 13: {
            PAPEnv_13_t *newEnv = (PAPEnv_13_t *)GC_MALLOC(sizeof(PAPEnv_13_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            void *arg12 = va_arg(argv, void *);
            void *arg13 = va_arg(argv, void *);
            newEnv->arg0 = arg1;
            newEnv->arg1 = arg2;
            newEnv->arg2 = arg3;
            newEnv->arg3 = arg4;
            newEnv->arg4 = arg5;
            newEnv->arg5 = arg6;
            newEnv->arg6 = arg7;
            newEnv->arg7 = arg8;
            newEnv->arg8 = arg9;
            newEnv->arg9 = arg10;
            newEnv->arg10 = arg11;
            newEnv->arg11 = arg12;
            newEnv->arg12 = arg13;
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 14: {
            PAPEnv_14_t *newEnv = (PAPEnv_14_t *)GC_MALLOC(sizeof(PAPEnv_14_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            void *arg12 = va_arg(argv, void *);
            void *arg13 = va_arg(argv, void *);
            void *arg14 = va_arg(argv, void *);
            newEnv->arg0 = arg1;
            newEnv->arg1 = arg2;
            newEnv->arg2 = arg3;
            newEnv->arg3 = arg4;
            newEnv->arg4 = arg5;
            newEnv->arg5 = arg6;
            newEnv->arg6 = arg7;
            newEnv->arg7 = arg8;
            newEnv->arg8 = arg9;
            newEnv->arg9 = arg10;
            newEnv->arg10 = arg11;
            newEnv->arg11 = arg12;
            newEnv->arg12 = arg13;
            newEnv->arg13 = arg14;
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 15: {
            PAPEnv_15_t *newEnv = (PAPEnv_15_t *)GC_MALLOC(sizeof(PAPEnv_15_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            void *arg12 = va_arg(argv, void *);
            void *arg13 = va_arg(argv, void *);
            void *arg14 = va_arg(argv, void *);
            void *arg15 = va_arg(argv, void *);
            newEnv->arg0 = arg1;
            newEnv->arg1 = arg2;
            newEnv->arg2 = arg3;
            newEnv->arg3 = arg4;
            newEnv->arg4 = arg5;
            newEnv->arg5 = arg6;
            newEnv->arg6 = arg7;
            newEnv->arg7 = arg8;
            newEnv->arg8 = arg9;
            newEnv->arg9 = arg10;
            newEnv->arg10 = arg11;
            newEnv->arg11 = arg12;
            newEnv->arg12 = arg13;
            newEnv->arg13 = arg14;
            newEnv->arg14 = arg15;
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 16: {
            PAPEnv_16_t *newEnv = (PAPEnv_16_t *)GC_MALLOC(sizeof(PAPEnv_16_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            void *arg12 = va_arg(argv, void *);
            void *arg13 = va_arg(argv, void *);
            void *arg14 = va_arg(argv, void *);
            void *arg15 = va_arg(argv, void *);
            void *arg16 = va_arg(argv, void *);
            newEnv->arg0 = arg1;
            newEnv->arg1 = arg2;
            newEnv->arg2 = arg3;
            newEnv->arg3 = arg4;
            newEnv->arg4 = arg5;
            newEnv->arg5 = arg6;
            newEnv->arg6 = arg7;
            newEnv->arg7 = arg8;
            newEnv->arg8 = arg9;
            newEnv->arg9 = arg10;
            newEnv->arg10 = arg11;
            newEnv->arg11 = arg12;
            newEnv->arg12 = arg13;
            newEnv->arg13 = arg14;
            newEnv->arg14 = arg15;
            newEnv->arg15 = arg16;
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 17: {
            PAPEnv_17_t *newEnv = (PAPEnv_17_t *)GC_MALLOC(sizeof(PAPEnv_17_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            void *arg12 = va_arg(argv, void *);
            void *arg13 = va_arg(argv, void *);
            void *arg14 = va_arg(argv, void *);
            void *arg15 = va_arg(argv, void *);
            void *arg16 = va_arg(argv, void *);
            void *arg17 = va_arg(argv, void *);
            newEnv->arg0 = arg1;
            newEnv->arg1 = arg2;
            newEnv->arg2 = arg3;
            newEnv->arg3 = arg4;
            newEnv->arg4 = arg5;
            newEnv->arg5 = arg6;
            newEnv->arg6 = arg7;
            newEnv->arg7 = arg8;
            newEnv->arg8 = arg9;
            newEnv->arg9 = arg10;
            newEnv->arg10 = arg11;
            newEnv->arg11 = arg12;
            newEnv->arg12 = arg13;
            newEnv->arg13 = arg14;
            newEnv->arg14 = arg15;
            newEnv->arg15 = arg16;
            newEnv->arg16 = arg17;
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 18: {
            PAPEnv_18_t *newEnv = (PAPEnv_18_t *)GC_MALLOC(sizeof(PAPEnv_18_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            void *arg12 = va_arg(argv, void *);
            void *arg13 = va_arg(argv, void *);
            void *arg14 = va_arg(argv, void *);
            void *arg15 = va_arg(argv, void *);
            void *arg16 = va_arg(argv, void *);
            void *arg17 = va_arg(argv, void *);
            void *arg18 = va_arg(argv, void *);
            newEnv->arg0 = arg1;
            newEnv->arg1 = arg2;
            newEnv->arg2 = arg3;
            newEnv->arg3 = arg4;
            newEnv->arg4 = arg5;
            newEnv->arg5 = arg6;
            newEnv->arg6 = arg7;
            newEnv->arg7 = arg8;
            newEnv->arg8 = arg9;
            newEnv->arg9 = arg10;
            newEnv->arg10 = arg11;
            newEnv->arg11 = arg12;
            newEnv->arg12 = arg13;
            newEnv->arg13 = arg14;
            newEnv->arg14 = arg15;
            newEnv->arg15 = arg16;
            newEnv->arg16 = arg17;
            newEnv->arg17 = arg18;
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 19: {
            PAPEnv_19_t *newEnv = (PAPEnv_19_t *)GC_MALLOC(sizeof(PAPEnv_19_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            void *arg12 = va_arg(argv, void *);
            void *arg13 = va_arg(argv, void *);
            void *arg14 = va_arg(argv, void *);
            void *arg15 = va_arg(argv, void *);
            void *arg16 = va_arg(argv, void *);
            void *arg17 = va_arg(argv, void *);
            void *arg18 = va_arg(argv, void *);
            void *arg19 = va_arg(argv, void *);
            newEnv->arg0 = arg1;
            newEnv->arg1 = arg2;
            newEnv->arg2 = arg3;
            newEnv->arg3 = arg4;
            newEnv->arg4 = arg5;
            newEnv->arg5 = arg6;
            newEnv->arg6 = arg7;
            newEnv->arg7 = arg8;
            newEnv->arg8 = arg9;
            newEnv->arg9 = arg10;
            newEnv->arg10 = arg11;
            newEnv->arg11 = arg12;
            newEnv->arg12 = arg13;
            newEnv->arg13 = arg14;
            newEnv->arg14 = arg15;
            newEnv->arg15 = arg16;
            newEnv->arg16 = arg17;
            newEnv->arg17 = arg18;
            newEnv->arg18 = arg19;
            va_end(argv);
            newPAP->env = newEnv;
            return newPAP;
          }
          case 20: {
            PAPEnv_20_t *newEnv = (PAPEnv_20_t *)GC_MALLOC(sizeof(PAPEnv_20_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            void *arg12 = va_arg(argv, void *);
            void *arg13 = va_arg(argv, void *);
            void *arg14 = va_arg(argv, void *);
            void *arg15 = va_arg(argv, void *);
            void *arg16 = va_arg(argv, void *);
            void *arg17 = va_arg(argv, void *);
            void *arg18 = va_arg(argv, void *);
            void *arg19 = va_arg(argv, void *);
            void *arg20 = va_arg(argv, void *);
            newEnv->arg0 = arg1;
            newEnv->arg1 = arg2;
            newEnv->arg2 = arg3;
            newEnv->arg3 = arg4;
            newEnv->arg4 = arg5;
            newEnv->arg5 = arg6;
            newEnv->arg6 = arg7;
            newEnv->arg7 = arg8;
            newEnv->arg8 = arg9;
            newEnv->arg9 = arg10;
            newEnv->arg10 = arg11;
            newEnv->arg11 = arg12;
            newEnv->arg12 = arg13;
            newEnv->arg13 = arg14;
            newEnv->arg14 = arg15;
            newEnv->arg15 = arg16;
            newEnv->arg16 = arg17;
            newEnv->arg17 = arg18;
            newEnv->arg18 = arg19;
            newEnv->arg19 = arg20;
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
            PAPEnv_2_t *newEnv = (PAPEnv_2_t *)GC_MALLOC(sizeof(PAPEnv_2_t));
            void *arg1 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = arg1;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 3: {
            PAPEnv_3_t *newEnv = (PAPEnv_3_t *)GC_MALLOC(sizeof(PAPEnv_3_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = arg1;
            newEnv->arg2 = arg2;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 4: {
            PAPEnv_4_t *newEnv = (PAPEnv_4_t *)GC_MALLOC(sizeof(PAPEnv_4_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = arg1;
            newEnv->arg2 = arg2;
            newEnv->arg3 = arg3;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 5: {
            PAPEnv_5_t *newEnv = (PAPEnv_5_t *)GC_MALLOC(sizeof(PAPEnv_5_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = arg1;
            newEnv->arg2 = arg2;
            newEnv->arg3 = arg3;
            newEnv->arg4 = arg4;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 6: {
            PAPEnv_6_t *newEnv = (PAPEnv_6_t *)GC_MALLOC(sizeof(PAPEnv_6_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = arg1;
            newEnv->arg2 = arg2;
            newEnv->arg3 = arg3;
            newEnv->arg4 = arg4;
            newEnv->arg5 = arg5;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 7: {
            PAPEnv_7_t *newEnv = (PAPEnv_7_t *)GC_MALLOC(sizeof(PAPEnv_7_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = arg1;
            newEnv->arg2 = arg2;
            newEnv->arg3 = arg3;
            newEnv->arg4 = arg4;
            newEnv->arg5 = arg5;
            newEnv->arg6 = arg6;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 8: {
            PAPEnv_8_t *newEnv = (PAPEnv_8_t *)GC_MALLOC(sizeof(PAPEnv_8_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = arg1;
            newEnv->arg2 = arg2;
            newEnv->arg3 = arg3;
            newEnv->arg4 = arg4;
            newEnv->arg5 = arg5;
            newEnv->arg6 = arg6;
            newEnv->arg7 = arg7;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 9: {
            PAPEnv_9_t *newEnv = (PAPEnv_9_t *)GC_MALLOC(sizeof(PAPEnv_9_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = arg1;
            newEnv->arg2 = arg2;
            newEnv->arg3 = arg3;
            newEnv->arg4 = arg4;
            newEnv->arg5 = arg5;
            newEnv->arg6 = arg6;
            newEnv->arg7 = arg7;
            newEnv->arg8 = arg8;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 10: {
            PAPEnv_10_t *newEnv = (PAPEnv_10_t *)GC_MALLOC(sizeof(PAPEnv_10_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = arg1;
            newEnv->arg2 = arg2;
            newEnv->arg3 = arg3;
            newEnv->arg4 = arg4;
            newEnv->arg5 = arg5;
            newEnv->arg6 = arg6;
            newEnv->arg7 = arg7;
            newEnv->arg8 = arg8;
            newEnv->arg9 = arg9;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 11: {
            PAPEnv_11_t *newEnv = (PAPEnv_11_t *)GC_MALLOC(sizeof(PAPEnv_11_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = arg1;
            newEnv->arg2 = arg2;
            newEnv->arg3 = arg3;
            newEnv->arg4 = arg4;
            newEnv->arg5 = arg5;
            newEnv->arg6 = arg6;
            newEnv->arg7 = arg7;
            newEnv->arg8 = arg8;
            newEnv->arg9 = arg9;
            newEnv->arg10 = arg10;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 12: {
            PAPEnv_12_t *newEnv = (PAPEnv_12_t *)GC_MALLOC(sizeof(PAPEnv_12_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = arg1;
            newEnv->arg2 = arg2;
            newEnv->arg3 = arg3;
            newEnv->arg4 = arg4;
            newEnv->arg5 = arg5;
            newEnv->arg6 = arg6;
            newEnv->arg7 = arg7;
            newEnv->arg8 = arg8;
            newEnv->arg9 = arg9;
            newEnv->arg10 = arg10;
            newEnv->arg11 = arg11;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 13: {
            PAPEnv_13_t *newEnv = (PAPEnv_13_t *)GC_MALLOC(sizeof(PAPEnv_13_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            void *arg12 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = arg1;
            newEnv->arg2 = arg2;
            newEnv->arg3 = arg3;
            newEnv->arg4 = arg4;
            newEnv->arg5 = arg5;
            newEnv->arg6 = arg6;
            newEnv->arg7 = arg7;
            newEnv->arg8 = arg8;
            newEnv->arg9 = arg9;
            newEnv->arg10 = arg10;
            newEnv->arg11 = arg11;
            newEnv->arg12 = arg12;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 14: {
            PAPEnv_14_t *newEnv = (PAPEnv_14_t *)GC_MALLOC(sizeof(PAPEnv_14_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            void *arg12 = va_arg(argv, void *);
            void *arg13 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = arg1;
            newEnv->arg2 = arg2;
            newEnv->arg3 = arg3;
            newEnv->arg4 = arg4;
            newEnv->arg5 = arg5;
            newEnv->arg6 = arg6;
            newEnv->arg7 = arg7;
            newEnv->arg8 = arg8;
            newEnv->arg9 = arg9;
            newEnv->arg10 = arg10;
            newEnv->arg11 = arg11;
            newEnv->arg12 = arg12;
            newEnv->arg13 = arg13;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 15: {
            PAPEnv_15_t *newEnv = (PAPEnv_15_t *)GC_MALLOC(sizeof(PAPEnv_15_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            void *arg12 = va_arg(argv, void *);
            void *arg13 = va_arg(argv, void *);
            void *arg14 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = arg1;
            newEnv->arg2 = arg2;
            newEnv->arg3 = arg3;
            newEnv->arg4 = arg4;
            newEnv->arg5 = arg5;
            newEnv->arg6 = arg6;
            newEnv->arg7 = arg7;
            newEnv->arg8 = arg8;
            newEnv->arg9 = arg9;
            newEnv->arg10 = arg10;
            newEnv->arg11 = arg11;
            newEnv->arg12 = arg12;
            newEnv->arg13 = arg13;
            newEnv->arg14 = arg14;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 16: {
            PAPEnv_16_t *newEnv = (PAPEnv_16_t *)GC_MALLOC(sizeof(PAPEnv_16_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            void *arg12 = va_arg(argv, void *);
            void *arg13 = va_arg(argv, void *);
            void *arg14 = va_arg(argv, void *);
            void *arg15 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = arg1;
            newEnv->arg2 = arg2;
            newEnv->arg3 = arg3;
            newEnv->arg4 = arg4;
            newEnv->arg5 = arg5;
            newEnv->arg6 = arg6;
            newEnv->arg7 = arg7;
            newEnv->arg8 = arg8;
            newEnv->arg9 = arg9;
            newEnv->arg10 = arg10;
            newEnv->arg11 = arg11;
            newEnv->arg12 = arg12;
            newEnv->arg13 = arg13;
            newEnv->arg14 = arg14;
            newEnv->arg15 = arg15;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 17: {
            PAPEnv_17_t *newEnv = (PAPEnv_17_t *)GC_MALLOC(sizeof(PAPEnv_17_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            void *arg12 = va_arg(argv, void *);
            void *arg13 = va_arg(argv, void *);
            void *arg14 = va_arg(argv, void *);
            void *arg15 = va_arg(argv, void *);
            void *arg16 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = arg1;
            newEnv->arg2 = arg2;
            newEnv->arg3 = arg3;
            newEnv->arg4 = arg4;
            newEnv->arg5 = arg5;
            newEnv->arg6 = arg6;
            newEnv->arg7 = arg7;
            newEnv->arg8 = arg8;
            newEnv->arg9 = arg9;
            newEnv->arg10 = arg10;
            newEnv->arg11 = arg11;
            newEnv->arg12 = arg12;
            newEnv->arg13 = arg13;
            newEnv->arg14 = arg14;
            newEnv->arg15 = arg15;
            newEnv->arg16 = arg16;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 18: {
            PAPEnv_18_t *newEnv = (PAPEnv_18_t *)GC_MALLOC(sizeof(PAPEnv_18_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            void *arg12 = va_arg(argv, void *);
            void *arg13 = va_arg(argv, void *);
            void *arg14 = va_arg(argv, void *);
            void *arg15 = va_arg(argv, void *);
            void *arg16 = va_arg(argv, void *);
            void *arg17 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = arg1;
            newEnv->arg2 = arg2;
            newEnv->arg3 = arg3;
            newEnv->arg4 = arg4;
            newEnv->arg5 = arg5;
            newEnv->arg6 = arg6;
            newEnv->arg7 = arg7;
            newEnv->arg8 = arg8;
            newEnv->arg9 = arg9;
            newEnv->arg10 = arg10;
            newEnv->arg11 = arg11;
            newEnv->arg12 = arg12;
            newEnv->arg13 = arg13;
            newEnv->arg14 = arg14;
            newEnv->arg15 = arg15;
            newEnv->arg16 = arg16;
            newEnv->arg17 = arg17;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 19: {
            PAPEnv_19_t *newEnv = (PAPEnv_19_t *)GC_MALLOC(sizeof(PAPEnv_19_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            void *arg12 = va_arg(argv, void *);
            void *arg13 = va_arg(argv, void *);
            void *arg14 = va_arg(argv, void *);
            void *arg15 = va_arg(argv, void *);
            void *arg16 = va_arg(argv, void *);
            void *arg17 = va_arg(argv, void *);
            void *arg18 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = arg1;
            newEnv->arg2 = arg2;
            newEnv->arg3 = arg3;
            newEnv->arg4 = arg4;
            newEnv->arg5 = arg5;
            newEnv->arg6 = arg6;
            newEnv->arg7 = arg7;
            newEnv->arg8 = arg8;
            newEnv->arg9 = arg9;
            newEnv->arg10 = arg10;
            newEnv->arg11 = arg11;
            newEnv->arg12 = arg12;
            newEnv->arg13 = arg13;
            newEnv->arg14 = arg14;
            newEnv->arg15 = arg15;
            newEnv->arg16 = arg16;
            newEnv->arg17 = arg17;
            newEnv->arg18 = arg18;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 20: {
            PAPEnv_20_t *newEnv = (PAPEnv_20_t *)GC_MALLOC(sizeof(PAPEnv_20_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            void *arg12 = va_arg(argv, void *);
            void *arg13 = va_arg(argv, void *);
            void *arg14 = va_arg(argv, void *);
            void *arg15 = va_arg(argv, void *);
            void *arg16 = va_arg(argv, void *);
            void *arg17 = va_arg(argv, void *);
            void *arg18 = va_arg(argv, void *);
            void *arg19 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = arg1;
            newEnv->arg2 = arg2;
            newEnv->arg3 = arg3;
            newEnv->arg4 = arg4;
            newEnv->arg5 = arg5;
            newEnv->arg6 = arg6;
            newEnv->arg7 = arg7;
            newEnv->arg8 = arg8;
            newEnv->arg9 = arg9;
            newEnv->arg10 = arg10;
            newEnv->arg11 = arg11;
            newEnv->arg12 = arg12;
            newEnv->arg13 = arg13;
            newEnv->arg14 = arg14;
            newEnv->arg15 = arg15;
            newEnv->arg16 = arg16;
            newEnv->arg17 = arg17;
            newEnv->arg18 = arg18;
            newEnv->arg19 = arg19;
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
            PAPEnv_3_t *newEnv = (PAPEnv_3_t *)GC_MALLOC(sizeof(PAPEnv_3_t));
            void *arg1 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = arg1;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 4: {
            PAPEnv_4_t *newEnv = (PAPEnv_4_t *)GC_MALLOC(sizeof(PAPEnv_4_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = arg1;
            newEnv->arg3 = arg2;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 5: {
            PAPEnv_5_t *newEnv = (PAPEnv_5_t *)GC_MALLOC(sizeof(PAPEnv_5_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = arg1;
            newEnv->arg3 = arg2;
            newEnv->arg4 = arg3;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 6: {
            PAPEnv_6_t *newEnv = (PAPEnv_6_t *)GC_MALLOC(sizeof(PAPEnv_6_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = arg1;
            newEnv->arg3 = arg2;
            newEnv->arg4 = arg3;
            newEnv->arg5 = arg4;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 7: {
            PAPEnv_7_t *newEnv = (PAPEnv_7_t *)GC_MALLOC(sizeof(PAPEnv_7_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = arg1;
            newEnv->arg3 = arg2;
            newEnv->arg4 = arg3;
            newEnv->arg5 = arg4;
            newEnv->arg6 = arg5;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 8: {
            PAPEnv_8_t *newEnv = (PAPEnv_8_t *)GC_MALLOC(sizeof(PAPEnv_8_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = arg1;
            newEnv->arg3 = arg2;
            newEnv->arg4 = arg3;
            newEnv->arg5 = arg4;
            newEnv->arg6 = arg5;
            newEnv->arg7 = arg6;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 9: {
            PAPEnv_9_t *newEnv = (PAPEnv_9_t *)GC_MALLOC(sizeof(PAPEnv_9_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = arg1;
            newEnv->arg3 = arg2;
            newEnv->arg4 = arg3;
            newEnv->arg5 = arg4;
            newEnv->arg6 = arg5;
            newEnv->arg7 = arg6;
            newEnv->arg8 = arg7;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 10: {
            PAPEnv_10_t *newEnv = (PAPEnv_10_t *)GC_MALLOC(sizeof(PAPEnv_10_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = arg1;
            newEnv->arg3 = arg2;
            newEnv->arg4 = arg3;
            newEnv->arg5 = arg4;
            newEnv->arg6 = arg5;
            newEnv->arg7 = arg6;
            newEnv->arg8 = arg7;
            newEnv->arg9 = arg8;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 11: {
            PAPEnv_11_t *newEnv = (PAPEnv_11_t *)GC_MALLOC(sizeof(PAPEnv_11_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = arg1;
            newEnv->arg3 = arg2;
            newEnv->arg4 = arg3;
            newEnv->arg5 = arg4;
            newEnv->arg6 = arg5;
            newEnv->arg7 = arg6;
            newEnv->arg8 = arg7;
            newEnv->arg9 = arg8;
            newEnv->arg10 = arg9;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 12: {
            PAPEnv_12_t *newEnv = (PAPEnv_12_t *)GC_MALLOC(sizeof(PAPEnv_12_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = arg1;
            newEnv->arg3 = arg2;
            newEnv->arg4 = arg3;
            newEnv->arg5 = arg4;
            newEnv->arg6 = arg5;
            newEnv->arg7 = arg6;
            newEnv->arg8 = arg7;
            newEnv->arg9 = arg8;
            newEnv->arg10 = arg9;
            newEnv->arg11 = arg10;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 13: {
            PAPEnv_13_t *newEnv = (PAPEnv_13_t *)GC_MALLOC(sizeof(PAPEnv_13_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = arg1;
            newEnv->arg3 = arg2;
            newEnv->arg4 = arg3;
            newEnv->arg5 = arg4;
            newEnv->arg6 = arg5;
            newEnv->arg7 = arg6;
            newEnv->arg8 = arg7;
            newEnv->arg9 = arg8;
            newEnv->arg10 = arg9;
            newEnv->arg11 = arg10;
            newEnv->arg12 = arg11;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 14: {
            PAPEnv_14_t *newEnv = (PAPEnv_14_t *)GC_MALLOC(sizeof(PAPEnv_14_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            void *arg12 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = arg1;
            newEnv->arg3 = arg2;
            newEnv->arg4 = arg3;
            newEnv->arg5 = arg4;
            newEnv->arg6 = arg5;
            newEnv->arg7 = arg6;
            newEnv->arg8 = arg7;
            newEnv->arg9 = arg8;
            newEnv->arg10 = arg9;
            newEnv->arg11 = arg10;
            newEnv->arg12 = arg11;
            newEnv->arg13 = arg12;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 15: {
            PAPEnv_15_t *newEnv = (PAPEnv_15_t *)GC_MALLOC(sizeof(PAPEnv_15_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            void *arg12 = va_arg(argv, void *);
            void *arg13 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = arg1;
            newEnv->arg3 = arg2;
            newEnv->arg4 = arg3;
            newEnv->arg5 = arg4;
            newEnv->arg6 = arg5;
            newEnv->arg7 = arg6;
            newEnv->arg8 = arg7;
            newEnv->arg9 = arg8;
            newEnv->arg10 = arg9;
            newEnv->arg11 = arg10;
            newEnv->arg12 = arg11;
            newEnv->arg13 = arg12;
            newEnv->arg14 = arg13;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 16: {
            PAPEnv_16_t *newEnv = (PAPEnv_16_t *)GC_MALLOC(sizeof(PAPEnv_16_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            void *arg12 = va_arg(argv, void *);
            void *arg13 = va_arg(argv, void *);
            void *arg14 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = arg1;
            newEnv->arg3 = arg2;
            newEnv->arg4 = arg3;
            newEnv->arg5 = arg4;
            newEnv->arg6 = arg5;
            newEnv->arg7 = arg6;
            newEnv->arg8 = arg7;
            newEnv->arg9 = arg8;
            newEnv->arg10 = arg9;
            newEnv->arg11 = arg10;
            newEnv->arg12 = arg11;
            newEnv->arg13 = arg12;
            newEnv->arg14 = arg13;
            newEnv->arg15 = arg14;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 17: {
            PAPEnv_17_t *newEnv = (PAPEnv_17_t *)GC_MALLOC(sizeof(PAPEnv_17_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            void *arg12 = va_arg(argv, void *);
            void *arg13 = va_arg(argv, void *);
            void *arg14 = va_arg(argv, void *);
            void *arg15 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = arg1;
            newEnv->arg3 = arg2;
            newEnv->arg4 = arg3;
            newEnv->arg5 = arg4;
            newEnv->arg6 = arg5;
            newEnv->arg7 = arg6;
            newEnv->arg8 = arg7;
            newEnv->arg9 = arg8;
            newEnv->arg10 = arg9;
            newEnv->arg11 = arg10;
            newEnv->arg12 = arg11;
            newEnv->arg13 = arg12;
            newEnv->arg14 = arg13;
            newEnv->arg15 = arg14;
            newEnv->arg16 = arg15;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 18: {
            PAPEnv_18_t *newEnv = (PAPEnv_18_t *)GC_MALLOC(sizeof(PAPEnv_18_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            void *arg12 = va_arg(argv, void *);
            void *arg13 = va_arg(argv, void *);
            void *arg14 = va_arg(argv, void *);
            void *arg15 = va_arg(argv, void *);
            void *arg16 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = arg1;
            newEnv->arg3 = arg2;
            newEnv->arg4 = arg3;
            newEnv->arg5 = arg4;
            newEnv->arg6 = arg5;
            newEnv->arg7 = arg6;
            newEnv->arg8 = arg7;
            newEnv->arg9 = arg8;
            newEnv->arg10 = arg9;
            newEnv->arg11 = arg10;
            newEnv->arg12 = arg11;
            newEnv->arg13 = arg12;
            newEnv->arg14 = arg13;
            newEnv->arg15 = arg14;
            newEnv->arg16 = arg15;
            newEnv->arg17 = arg16;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 19: {
            PAPEnv_19_t *newEnv = (PAPEnv_19_t *)GC_MALLOC(sizeof(PAPEnv_19_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            void *arg12 = va_arg(argv, void *);
            void *arg13 = va_arg(argv, void *);
            void *arg14 = va_arg(argv, void *);
            void *arg15 = va_arg(argv, void *);
            void *arg16 = va_arg(argv, void *);
            void *arg17 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = arg1;
            newEnv->arg3 = arg2;
            newEnv->arg4 = arg3;
            newEnv->arg5 = arg4;
            newEnv->arg6 = arg5;
            newEnv->arg7 = arg6;
            newEnv->arg8 = arg7;
            newEnv->arg9 = arg8;
            newEnv->arg10 = arg9;
            newEnv->arg11 = arg10;
            newEnv->arg12 = arg11;
            newEnv->arg13 = arg12;
            newEnv->arg14 = arg13;
            newEnv->arg15 = arg14;
            newEnv->arg16 = arg15;
            newEnv->arg17 = arg16;
            newEnv->arg18 = arg17;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 20: {
            PAPEnv_20_t *newEnv = (PAPEnv_20_t *)GC_MALLOC(sizeof(PAPEnv_20_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            void *arg7 = va_arg(argv, void *);
            void *arg8 = va_arg(argv, void *);
            void *arg9 = va_arg(argv, void *);
            void *arg10 = va_arg(argv, void *);
            void *arg11 = va_arg(argv, void *);
            void *arg12 = va_arg(argv, void *);
            void *arg13 = va_arg(argv, void *);
            void *arg14 = va_arg(argv, void *);
            void *arg15 = va_arg(argv, void *);
            void *arg16 = va_arg(argv, void *);
            void *arg17 = va_arg(argv, void *);
            void *arg18 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = arg1;
            newEnv->arg3 = arg2;
            newEnv->arg4 = arg3;
            newEnv->arg5 = arg4;
            newEnv->arg6 = arg5;
            newEnv->arg7 = arg6;
            newEnv->arg8 = arg7;
            newEnv->arg9 = arg8;
            newEnv->arg10 = arg9;
            newEnv->arg11 = arg10;
            newEnv->arg12 = arg11;
            newEnv->arg13 = arg12;
            newEnv->arg14 = arg13;
            newEnv->arg15 = arg14;
            newEnv->arg16 = arg15;
            newEnv->arg17 = arg16;
            newEnv->arg18 = arg17;
            newEnv->arg19 = arg18;
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
            PAPEnv_4_t *newEnv = (PAPEnv_4_t *)GC_MALLOC(sizeof(PAPEnv_4_t));
            void *arg1 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = arg1;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 5: {
            PAPEnv_5_t *newEnv = (PAPEnv_5_t *)GC_MALLOC(sizeof(PAPEnv_5_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = arg1;
            newEnv->arg4 = arg2;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 6: {
            PAPEnv_6_t *newEnv = (PAPEnv_6_t *)GC_MALLOC(sizeof(PAPEnv_6_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = arg1;
            newEnv->arg4 = arg2;
            newEnv->arg5 = arg3;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 7: {
            PAPEnv_7_t *newEnv = (PAPEnv_7_t *)GC_MALLOC(sizeof(PAPEnv_7_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = arg1;
            newEnv->arg4 = arg2;
            newEnv->arg5 = arg3;
            newEnv->arg6 = arg4;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 8: {
            PAPEnv_8_t *newEnv = (PAPEnv_8_t *)GC_MALLOC(sizeof(PAPEnv_8_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = arg1;
            newEnv->arg4 = arg2;
            newEnv->arg5 = arg3;
            newEnv->arg6 = arg4;
            newEnv->arg7 = arg5;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 9: {
            PAPEnv_9_t *newEnv = (PAPEnv_9_t *)GC_MALLOC(sizeof(PAPEnv_9_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            void *arg6 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = arg1;
            newEnv->arg4 = arg2;
            newEnv->arg5 = arg3;
            newEnv->arg6 = arg4;
            newEnv->arg7 = arg5;
            newEnv->arg8 = arg6;
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
            PAPEnv_5_t *newEnv = (PAPEnv_5_t *)GC_MALLOC(sizeof(PAPEnv_5_t));
            void *arg1 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = arg1;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 6: {
            PAPEnv_6_t *newEnv = (PAPEnv_6_t *)GC_MALLOC(sizeof(PAPEnv_6_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = arg1;
            newEnv->arg5 = arg2;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 7: {
            PAPEnv_7_t *newEnv = (PAPEnv_7_t *)GC_MALLOC(sizeof(PAPEnv_7_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = arg1;
            newEnv->arg5 = arg2;
            newEnv->arg6 = arg3;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 8: {
            PAPEnv_8_t *newEnv = (PAPEnv_8_t *)GC_MALLOC(sizeof(PAPEnv_8_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = arg1;
            newEnv->arg5 = arg2;
            newEnv->arg6 = arg3;
            newEnv->arg7 = arg4;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 9: {
            PAPEnv_9_t *newEnv = (PAPEnv_9_t *)GC_MALLOC(sizeof(PAPEnv_9_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            void *arg5 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = arg1;
            newEnv->arg5 = arg2;
            newEnv->arg6 = arg3;
            newEnv->arg7 = arg4;
            newEnv->arg8 = arg5;
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
            PAPEnv_6_t *newEnv = (PAPEnv_6_t *)GC_MALLOC(sizeof(PAPEnv_6_t));
            void *arg1 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = env->arg4;
            newEnv->arg5 = arg1;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 7: {
            PAPEnv_7_t *newEnv = (PAPEnv_7_t *)GC_MALLOC(sizeof(PAPEnv_7_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = env->arg4;
            newEnv->arg5 = arg1;
            newEnv->arg6 = arg2;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 8: {
            PAPEnv_8_t *newEnv = (PAPEnv_8_t *)GC_MALLOC(sizeof(PAPEnv_8_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = env->arg4;
            newEnv->arg5 = arg1;
            newEnv->arg6 = arg2;
            newEnv->arg7 = arg3;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 9: {
            PAPEnv_9_t *newEnv = (PAPEnv_9_t *)GC_MALLOC(sizeof(PAPEnv_9_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            void *arg4 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = env->arg4;
            newEnv->arg5 = arg1;
            newEnv->arg6 = arg2;
            newEnv->arg7 = arg3;
            newEnv->arg8 = arg4;
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
            PAPEnv_7_t *newEnv = (PAPEnv_7_t *)GC_MALLOC(sizeof(PAPEnv_7_t));
            void *arg1 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = env->arg4;
            newEnv->arg5 = env->arg5;
            newEnv->arg6 = arg1;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 8: {
            PAPEnv_8_t *newEnv = (PAPEnv_8_t *)GC_MALLOC(sizeof(PAPEnv_8_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = env->arg4;
            newEnv->arg5 = env->arg5;
            newEnv->arg6 = arg1;
            newEnv->arg7 = arg2;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 9: {
            PAPEnv_9_t *newEnv = (PAPEnv_9_t *)GC_MALLOC(sizeof(PAPEnv_9_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            void *arg3 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = env->arg4;
            newEnv->arg5 = env->arg5;
            newEnv->arg6 = arg1;
            newEnv->arg7 = arg2;
            newEnv->arg8 = arg3;
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
            PAPEnv_8_t *newEnv = (PAPEnv_8_t *)GC_MALLOC(sizeof(PAPEnv_8_t));
            void *arg1 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = env->arg4;
            newEnv->arg5 = env->arg5;
            newEnv->arg6 = env->arg6;
            newEnv->arg7 = arg1;
            va_end(argv);

            newPAP->env = newEnv;
            return newPAP;
          }
          case 9: {
            PAPEnv_9_t *newEnv = (PAPEnv_9_t *)GC_MALLOC(sizeof(PAPEnv_9_t));
            void *arg1 = va_arg(argv, void *);
            void *arg2 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = env->arg4;
            newEnv->arg5 = env->arg5;
            newEnv->arg6 = env->arg6;
            newEnv->arg7 = arg1;
            newEnv->arg8 = arg2;
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
            PAPEnv_9_t *newEnv = (PAPEnv_9_t *)GC_MALLOC(sizeof(PAPEnv_9_t));
            void *arg1 = va_arg(argv, void *);
            newEnv->arg0 = env->arg0;
            newEnv->arg1 = env->arg1;
            newEnv->arg2 = env->arg2;
            newEnv->arg3 = env->arg3;
            newEnv->arg4 = env->arg4;
            newEnv->arg5 = env->arg5;
            newEnv->arg6 = env->arg6;
            newEnv->arg7 = env->arg7;
            newEnv->arg8 = arg1;
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
