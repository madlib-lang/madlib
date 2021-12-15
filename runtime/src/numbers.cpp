#include <iostream>
#include <gc.h>


#ifdef __cplusplus
extern "C" {
#endif



// instance Number Byte

unsigned char* __numberToByte__(int64_t *a) {
  unsigned char *result = (unsigned char*) malloc(sizeof(unsigned char));
  *result = (unsigned char) *a;
  return result;
}

unsigned char *__addBytes__(unsigned char *a, unsigned char *b) {
  unsigned char *boxed = (unsigned char*) GC_malloc(sizeof(unsigned char));
  *boxed = *a + *b;
  return boxed;
}

unsigned char *__substractBytes__(unsigned char *a, unsigned char *b) {
  unsigned char *boxed = (unsigned char*) GC_malloc(sizeof(unsigned char));
  *boxed = *a - *b;
  return boxed;
}

unsigned char *__multiplyBytes__(unsigned char *a, unsigned char *b) {
  unsigned char *boxed = (unsigned char*) GC_malloc(sizeof(unsigned char));
  *boxed = *a - *b;
  return boxed;
}

bool *__gtBytes__(unsigned char *a, unsigned char *b) {
  bool *boxed = (bool*) GC_malloc(sizeof(bool));
  *boxed = *a > *b;
  return boxed;
}

bool *__ltBytes__(unsigned char *a, unsigned char *b) {
  bool *boxed = (bool*) GC_malloc(sizeof(bool));
  *boxed = *a < *b;
  return boxed;
}

bool *__gteBytes__(unsigned char *a, unsigned char *b) {
  bool *boxed = (bool*) GC_malloc(sizeof(bool));
  *boxed = *a >= *b;
  return boxed;
}

bool *__lteBytes__(unsigned char *a, unsigned char *b) {
  bool *boxed = (bool*) GC_malloc(sizeof(bool));
  *boxed = *a <= *b;
  return boxed;
}


// instance Number Float

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

bool *__gtFloats__(double *a, double *b) {
  bool *boxed = (bool*) GC_malloc(sizeof(bool));
  *boxed = *a > *b;
  return boxed;
}

bool *__ltFloats__(double *a, double *b) {
  bool *boxed = (bool*) GC_malloc(sizeof(bool));
  *boxed = *a < *b;
  return boxed;
}

bool *__gteFloats__(double *a, double *b) {
  bool *boxed = (bool*) GC_malloc(sizeof(bool));
  *boxed = *a >= *b;
  return boxed;
}

bool *__lteFloats__(double *a, double *b) {
  bool *boxed = (bool*) GC_malloc(sizeof(bool));
  *boxed = *a <= *b;
  return boxed;
}


// instance Number Integer

int64_t* __numberToInteger__(int64_t *a) {
  int64_t *result = (int64_t*) malloc(sizeof(int64_t));
  *result = (int64_t) *a;
  return result;
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

bool *__gtIntegers__(int64_t *a, int64_t *b) {
  bool *boxed = (bool*) GC_malloc(sizeof(bool));
  *boxed = *a > *b;
  return boxed;
}

bool *__ltIntegers__(int64_t *a, int64_t *b) {
  bool *boxed = (bool*) GC_malloc(sizeof(bool));
  *boxed = *a < *b;
  return boxed;
}

bool *__gteIntegers__(int64_t *a, int64_t *b) {
  bool *boxed = (bool*) GC_malloc(sizeof(bool));
  *boxed = *a >= *b;
  return boxed;
}

bool *__lteIntegers__(int64_t *a, int64_t *b) {
  bool *boxed = (bool*) GC_malloc(sizeof(bool));
  *boxed = *a <= *b;
  return boxed;
}


// instance Eq Integer

bool *__eqInteger__(int64_t *a, int64_t *b) {
  bool *boxed = (bool*) GC_malloc(sizeof(bool));
  *boxed = *a == *b;
  return boxed;
}


bool *__eqByte__(unsigned char *a, unsigned char *b) {
  bool *boxed = (bool*) GC_malloc(sizeof(bool));
  *boxed = *a == *b;
  return boxed;
}


bool *__eqFloat__(double *a, double *b) {
  bool *boxed = (bool*) GC_malloc(sizeof(bool));
  *boxed = *a == *b;
  return boxed;
}

#ifdef __cplusplus
}
#endif
