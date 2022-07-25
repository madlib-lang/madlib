#ifndef APPLY_PAP_H
#define APPLY_PAP_H

#include <math.h>
#include <stdint.h>

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

typedef struct PAPEnv_11 {
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
  void *arg10;
} PAPEnv_11_t;

typedef struct PAPEnv_12 {
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
  void *arg10;
  void *arg11;
} PAPEnv_12_t;

typedef struct PAPEnv_13 {
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
  void *arg10;
  void *arg11;
  void *arg12;
} PAPEnv_13_t;

typedef struct PAPEnv_14 {
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
  void *arg10;
  void *arg11;
  void *arg12;
  void *arg13;
} PAPEnv_14_t;

typedef struct PAPEnv_15 {
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
  void *arg10;
  void *arg11;
  void *arg12;
  void *arg13;
  void *arg14;
} PAPEnv_15_t;

typedef struct PAPEnv_16 {
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
  void *arg10;
  void *arg11;
  void *arg12;
  void *arg13;
  void *arg14;
  void *arg15;
} PAPEnv_16_t;

typedef struct PAPEnv_17 {
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
  void *arg10;
  void *arg11;
  void *arg12;
  void *arg13;
  void *arg14;
  void *arg15;
  void *arg16;
} PAPEnv_17_t;

typedef struct PAPEnv_18 {
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
  void *arg10;
  void *arg11;
  void *arg12;
  void *arg13;
  void *arg14;
  void *arg15;
  void *arg16;
  void *arg17;
} PAPEnv_18_t;

typedef struct PAPEnv_19 {
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
  void *arg10;
  void *arg11;
  void *arg12;
  void *arg13;
  void *arg14;
  void *arg15;
  void *arg16;
  void *arg17;
  void *arg18;
} PAPEnv_19_t;

typedef struct PAPEnv_20 {
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
  void *arg10;
  void *arg11;
  void *arg12;
  void *arg13;
  void *arg14;
  void *arg15;
  void *arg16;
  void *arg17;
  void *arg18;
  void *arg19;
} PAPEnv_20_t;

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

#endif // APPLY_PAP_H
