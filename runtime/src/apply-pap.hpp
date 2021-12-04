#include <math.h>

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


#ifdef __cplusplus
extern "C" {
#endif

void *__applyPAP__(void *pap, int32_t argc, ...);

#ifdef __cplusplus
}
#endif