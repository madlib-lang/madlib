#include <iostream>
#include <gc.h>


#ifdef __cplusplus
extern "C" {
#endif


// #define FLOAT_TYPE 0

// typedef struct Number {
//   char type;
//   void* value;
// } Number_t;


// double *__addFloats__(Number_t *a, Number_t *b) {
//   if (a->type == FLOAT_TYPE && b->type == FLOAT_TYPE) {
//     double *aValue = (double*)a->value;
//     double *bValue = (double*)b->value;
//     ...
//   }

// }


double* __numberToFloat__(int64_t *a) {
  double *result = (double*) malloc(sizeof(double));
  *result = (double) *a;
  return result;
}

double *__addFloats__(double *a, double *b) {
  double *boxed = (double*) GC_malloc(sizeof(double));
  *boxed = *a + *b;
  return boxed;
}

double *__substractFloats__(double *a, double *b) {
  double *boxed = (double*) GC_malloc(sizeof(double));
  *boxed = *a - *b;
  return boxed;
}

double *__multiplyFloats__(double *a, double *b) {
  double *boxed = (double*) GC_malloc(sizeof(double));
  *boxed = *a * *b;
  return boxed;
}


int64_t *__addIntegers__(int64_t *a, int64_t *b) {
  int64_t *boxed = (int64_t*) GC_malloc(sizeof(int64_t));
  *boxed = *a + *b;
  return boxed;
}

int64_t *__substractIntegers__(int64_t *a, int64_t *b) {
  int64_t *boxed = (int64_t*) GC_malloc(sizeof(int64_t));
  *boxed = *a - *b;
  return boxed;
}

int64_t *__multiplyIntegers__(int64_t *a, int64_t *b) {
  int64_t *boxed = (int64_t*) GC_malloc(sizeof(int64_t));
  *boxed = *a * *b;
  return boxed;
}

#ifdef __cplusplus
}
#endif
