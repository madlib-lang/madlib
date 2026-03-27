// generated automatically on the 2026-03-27 at 15:19:09 UTC
#include <gc.h>
#include "apply-pap.hpp"
#include <cstdarg>
#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <cstring>


// Partial application

#ifdef __cplusplus
extern "C" {
#endif

static const int32_t MADLIB_MAX_SUPPORTED_PAP_ARITY = 30;

static void failUnsupportedPAPArity(int32_t arity, int32_t argc) {
  fprintf(
      stderr,
      "Unsupported PAP arity: %d (argc=%d). runtime/src/apply-pap.* currently supports up to %d arguments.\\n",
      arity,
      argc,
      MADLIB_MAX_SUPPORTED_PAP_ARITY);
  abort();
}

static bool papAtomicEnvEnabled() {
  static int8_t cachedEnabled = -1;
  if (cachedEnabled != -1) {
    return cachedEnabled == 1;
  }

  const char *value = getenv("MADLIB_PAP_ATOMIC_ENV");
  const bool enabled =
    value != NULL
    && value[0] != '\0'
    &&
    (
    strcmp(value, "1") == 0
    || strcmp(value, "true") == 0
    || strcmp(value, "TRUE") == 0
    || strcmp(value, "yes") == 0
    || strcmp(value, "YES") == 0
    );

  cachedEnabled = enabled ? 1 : 0;
  return enabled;
}

void *__applyPAP1__(PAP_t *pap, void *arg1) {
  int32_t arity = pap->arity;
  if (arity == 1) {
    void *(*fn)(void *) = (void*(*)(void *))pap->fn;
    return fn(arg1);
  }

  int32_t ENV_SIZE = arity - pap->missingArgCount;
  if (ENV_SIZE == 1 && arity == 2) {
    void *(*fn)(void *, void *) = (void*(*)(void *, void *))pap->fn;
    void **env = (void **)pap->env;
    return fn(env[0], arg1);
  }

  if (ENV_SIZE == 2 && arity == 3) {
    void *(*fn)(void *, void *, void *) = (void*(*)(void *, void *, void *))pap->fn;
    void **env = (void **)pap->env;
    return fn(env[0], env[1], arg1);
  }

  return __applyPAP__(pap, 1, arg1);
}

void *__applyPAP2__(PAP_t *pap, void *arg1, void *arg2) {
  int32_t arity = pap->arity;
  int32_t missingArgs = pap->missingArgCount;
  if (missingArgs <= 2) {
    if (arity == 1) {
        void *(*fn)(void *) = (void*(*)(void *))pap->fn;
        return __applyPAP1__((PAP_t*) fn(arg1), arg2);
    } 

    int32_t ENV_SIZE = arity - missingArgs;
    if (ENV_SIZE == 0 && arity == 2) {
      void *(*fn)(void *, void *) = (void*(*)(void *, void *))pap->fn;
      return fn(arg1, arg2);
    }

    if (ENV_SIZE == 1 && arity == 3) {
      void *(*fn)(void *, void *, void *) = (void*(*)(void *, void *, void *))pap->fn;
      void **env = (void **)pap->env;
      return fn(env[0], arg1, arg2);
    }

    if (ENV_SIZE == 2 && arity == 4) {
      void *(*fn)(void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *))pap->fn;
      void **env = (void **)pap->env;
      return fn(env[0], env[1], arg1, arg2);
    }
  }

  return __applyPAP__(pap, 2, arg1, arg2);
}

void *__applyPAP3__(PAP_t *pap, void *arg1, void *arg2, void *arg3) {
  int32_t arity = pap->arity;
  int32_t missingArgs = pap->missingArgCount;
  if (missingArgs <= 3) {
    if (arity == 1) {
      void *(*fn)(void *) = (void*(*)(void *))pap->fn;
      return __applyPAP2__((PAP_t*) fn(arg1), arg2, arg3);
    }

    int32_t ENV_SIZE = arity - missingArgs;
    if (ENV_SIZE == 0 && arity == 3) {
      void *(*fn)(void *, void *, void *) = (void*(*)(void *, void *, void *))pap->fn;
      return fn(arg1, arg2, arg3);
    }

    if (ENV_SIZE == 1 && arity == 4) {
      void *(*fn)(void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *))pap->fn;
      void **env = (void **)pap->env;
      return fn(env[0], arg1, arg2, arg3);
    }
  }

  return __applyPAP__(pap, 3, arg1, arg2, arg3);
}

void *__applyPAP4__(PAP_t *pap, void *arg1, void *arg2, void *arg3, void *arg4) {
  int32_t arity = pap->arity;
  int32_t missingArgs = pap->missingArgCount;
  if (missingArgs <= 4) {
    if (arity == 1) {
      void *(*fn)(void *) = (void*(*)(void *))pap->fn;
      return __applyPAP3__((PAP_t*) fn(arg1), arg2, arg3, arg4);
    }

    int32_t ENV_SIZE = arity - missingArgs;
    if (ENV_SIZE == 0 && arity == 4) {
      void *(*fn)(void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *))pap->fn;
      return fn(arg1, arg2, arg3, arg4);
    }

    if (ENV_SIZE == 1 && arity == 5) {
      void *(*fn)(void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *))pap->fn;
      void **env = (void **)pap->env;
      return fn(env[0], arg1, arg2, arg3, arg4);
    }
  }

  return __applyPAP__(pap, 4, arg1, arg2, arg3, arg4);
}

void *__applyPAP__(void *pap, int32_t argc, ...) {
  va_list argv;
  va_start(argv, argc);

  PAP_t *unwrappedPAP = (PAP_t *)pap;
  int32_t ENV_SIZE = unwrappedPAP->arity - unwrappedPAP->missingArgCount;
  int32_t ARITY = unwrappedPAP->arity;
  if (ARITY > MADLIB_MAX_SUPPORTED_PAP_ARITY) {
    failUnsupportedPAPArity(ARITY, argc);
  }

  if (argc >= unwrappedPAP->missingArgCount) {
    void *result = (void *)NULL;
    void **env = (void **)unwrappedPAP->env;
    switch (ARITY) {
      case 1: {
        void *(*fn)(void *) = (void*(*)(void *))unwrappedPAP->fn;
        void* args[1];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0]);
        break;
      }
      case 2: {
        void *(*fn)(void *, void *) = (void*(*)(void *, void *))unwrappedPAP->fn;
        void* args[2];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1]);
        break;
      }
      case 3: {
        void *(*fn)(void *, void *, void *) = (void*(*)(void *, void *, void *))unwrappedPAP->fn;
        void* args[3];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2]);
        break;
      }
      case 4: {
        void *(*fn)(void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[4];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3]);
        break;
      }
      case 5: {
        void *(*fn)(void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[5];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4]);
        break;
      }
      case 6: {
        void *(*fn)(void *, void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[6];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4], args[5]);
        break;
      }
      case 7: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[7];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4], args[5], args[6]);
        break;
      }
      case 8: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[8];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]);
        break;
      }
      case 9: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[9];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8]);
        break;
      }
      case 10: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[10];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9]);
        break;
      }
      case 11: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[11];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10]);
        break;
      }
      case 12: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[12];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11]);
        break;
      }
      case 13: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[13];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12]);
        break;
      }
      case 14: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[14];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13]);
        break;
      }
      case 15: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[15];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14]);
        break;
      }
      case 16: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[16];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15]);
        break;
      }
      case 17: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[17];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16]);
        break;
      }
      case 18: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[18];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17]);
        break;
      }
      case 19: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[19];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17], args[18]);
        break;
      }
      case 20: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[20];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17], args[18], args[19]);
        break;
      }
      case 21: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[21];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17], args[18], args[19], args[20]);
        break;
      }
      case 22: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[22];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17], args[18], args[19], args[20], args[21]);
        break;
      }
      case 23: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[23];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17], args[18], args[19], args[20], args[21], args[22]);
        break;
      }
      case 24: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[24];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17], args[18], args[19], args[20], args[21], args[22], args[23]);
        break;
      }
      case 25: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[25];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17], args[18], args[19], args[20], args[21], args[22], args[23], args[24]);
        break;
      }
      case 26: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[26];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17], args[18], args[19], args[20], args[21], args[22], args[23], args[24], args[25]);
        break;
      }
      case 27: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[27];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17], args[18], args[19], args[20], args[21], args[22], args[23], args[24], args[25], args[26]);
        break;
      }
      case 28: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[28];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17], args[18], args[19], args[20], args[21], args[22], args[23], args[24], args[25], args[26], args[27]);
        break;
      }
      case 29: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[29];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17], args[18], args[19], args[20], args[21], args[22], args[23], args[24], args[25], args[26], args[27], args[28]);
        break;
      }
      case 30: {
        void *(*fn)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *) = (void*(*)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *))unwrappedPAP->fn;
        void* args[30];
        int i = 0;
        if (ENV_SIZE > 0) {
          memcpy(args, env, sizeof(void*) * ENV_SIZE);
          i = ENV_SIZE;
        }
        for (; i < ARITY; i++) { args[i] = va_arg(argv, void*); }
        result = fn(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17], args[18], args[19], args[20], args[21], args[22], args[23], args[24], args[25], args[26], args[27], args[28], args[29]);
        break;
      }
      default: {
        failUnsupportedPAPArity(ARITY, argc);
        break;
      }
    }
    if (argc > unwrappedPAP->missingArgCount) {
      int argsLeft = argc - unwrappedPAP->missingArgCount;
      switch (argsLeft) {
        case 1: {
          void *args[1];
          for (int i = 0; i < 1; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0]);
          break;
        }
        case 2: {
          void *args[2];
          for (int i = 0; i < 2; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1]);
          break;
        }
        case 3: {
          void *args[3];
          for (int i = 0; i < 3; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2]);
          break;
        }
        case 4: {
          void *args[4];
          for (int i = 0; i < 4; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3]);
          break;
        }
        case 5: {
          void *args[5];
          for (int i = 0; i < 5; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4]);
          break;
        }
        case 6: {
          void *args[6];
          for (int i = 0; i < 6; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4], args[5]);
          break;
        }
        case 7: {
          void *args[7];
          for (int i = 0; i < 7; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4], args[5], args[6]);
          break;
        }
        case 8: {
          void *args[8];
          for (int i = 0; i < 8; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]);
          break;
        }
        case 9: {
          void *args[9];
          for (int i = 0; i < 9; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8]);
          break;
        }
        case 10: {
          void *args[10];
          for (int i = 0; i < 10; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9]);
          break;
        }
        case 11: {
          void *args[11];
          for (int i = 0; i < 11; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10]);
          break;
        }
        case 12: {
          void *args[12];
          for (int i = 0; i < 12; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11]);
          break;
        }
        case 13: {
          void *args[13];
          for (int i = 0; i < 13; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12]);
          break;
        }
        case 14: {
          void *args[14];
          for (int i = 0; i < 14; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13]);
          break;
        }
        case 15: {
          void *args[15];
          for (int i = 0; i < 15; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14]);
          break;
        }
        case 16: {
          void *args[16];
          for (int i = 0; i < 16; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15]);
          break;
        }
        case 17: {
          void *args[17];
          for (int i = 0; i < 17; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16]);
          break;
        }
        case 18: {
          void *args[18];
          for (int i = 0; i < 18; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17]);
          break;
        }
        case 19: {
          void *args[19];
          for (int i = 0; i < 19; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17], args[18]);
          break;
        }
        case 20: {
          void *args[20];
          for (int i = 0; i < 20; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17], args[18], args[19]);
          break;
        }
        case 21: {
          void *args[21];
          for (int i = 0; i < 21; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17], args[18], args[19], args[20]);
          break;
        }
        case 22: {
          void *args[22];
          for (int i = 0; i < 22; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17], args[18], args[19], args[20], args[21]);
          break;
        }
        case 23: {
          void *args[23];
          for (int i = 0; i < 23; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17], args[18], args[19], args[20], args[21], args[22]);
          break;
        }
        case 24: {
          void *args[24];
          for (int i = 0; i < 24; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17], args[18], args[19], args[20], args[21], args[22], args[23]);
          break;
        }
        case 25: {
          void *args[25];
          for (int i = 0; i < 25; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17], args[18], args[19], args[20], args[21], args[22], args[23], args[24]);
          break;
        }
        case 26: {
          void *args[26];
          for (int i = 0; i < 26; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17], args[18], args[19], args[20], args[21], args[22], args[23], args[24], args[25]);
          break;
        }
        case 27: {
          void *args[27];
          for (int i = 0; i < 27; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17], args[18], args[19], args[20], args[21], args[22], args[23], args[24], args[25], args[26]);
          break;
        }
        case 28: {
          void *args[28];
          for (int i = 0; i < 28; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17], args[18], args[19], args[20], args[21], args[22], args[23], args[24], args[25], args[26], args[27]);
          break;
        }
        case 29: {
          void *args[29];
          for (int i = 0; i < 29; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17], args[18], args[19], args[20], args[21], args[22], args[23], args[24], args[25], args[26], args[27], args[28]);
          break;
        }
        case 30: {
          void *args[30];
          for (int i = 0; i < 30; i++) { args[i] = va_arg(argv, void*); }
          result = __applyPAP__(result, argsLeft, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17], args[18], args[19], args[20], args[21], args[22], args[23], args[24], args[25], args[26], args[27], args[28], args[29]);
          break;
        }
        default: {
          failUnsupportedPAPArity(argsLeft, argc);
          break;
        }
      }
    }
    va_end(argv);
    return result;
  } else {
    // We push the args to a newly allocated PAP
    int NEXT_ENV_SIZE = argc + ENV_SIZE;
    PAP_t *newPAP = (PAP_t *)GC_MALLOC(sizeof(PAP_t));
    newPAP->fn = unwrappedPAP->fn;
    newPAP->arity = unwrappedPAP->arity;
    newPAP->missingArgCount = unwrappedPAP->missingArgCount - argc;
    newPAP->env_is_atomic = unwrappedPAP->env_is_atomic;

    void **env = (void **)unwrappedPAP->env;
    bool useAtomicEnv = papAtomicEnvEnabled() && unwrappedPAP->env_is_atomic;
    void **newEnv = useAtomicEnv
      ? (void**) GC_MALLOC_ATOMIC(sizeof(void*) * NEXT_ENV_SIZE)
      : (void**) GC_MALLOC(sizeof(void*) * NEXT_ENV_SIZE);
    if (ENV_SIZE > 0) {
      memcpy(newEnv, env, sizeof(void*) * ENV_SIZE);
    }
    for (int i = ENV_SIZE; i<NEXT_ENV_SIZE; i++) {
      void *arg = va_arg(argv, void*);
      newEnv[i] = arg;
    }

    va_end(argv);
    newPAP->env = newEnv;
    return newPAP;
  }
  printf("__applyPAP__ case not handled, argc: %d, ENV_SIZE: %d, ARITY: %d\n", argc, ENV_SIZE, ARITY);

  return NULL;
}

#ifdef __cplusplus
}
#endif
