#include "number.hpp"

#include <gc.h>

#ifdef __cplusplus
extern "C" {
#endif

// Byte

unsigned char *madlib__number__internal__numberToByte(int64_t *a) {
  unsigned char *result = (unsigned char *)malloc(sizeof(unsigned char));
  *result = (unsigned char)*a;
  return result;
}

unsigned char *madlib__number__internal__addBytes(unsigned char *a,
                                                  unsigned char *b) {
  unsigned char *boxed = (unsigned char *)GC_malloc(sizeof(unsigned char));
  *boxed = *a + *b;
  return boxed;
}

unsigned char *madlib__number__internal__substractBytes(unsigned char *a,
                                                        unsigned char *b) {
  unsigned char *boxed = (unsigned char *)GC_malloc(sizeof(unsigned char));
  *boxed = *a - *b;
  return boxed;
}

unsigned char *madlib__number__internal__multiplyBytes(unsigned char *a,
                                                       unsigned char *b) {
  unsigned char *boxed = (unsigned char *)GC_malloc(sizeof(unsigned char));
  *boxed = *a - *b;
  return boxed;
}

bool *madlib__number__internal__gtBytes(unsigned char *a, unsigned char *b) {
  bool *boxed = (bool *)GC_malloc(sizeof(bool));
  *boxed = *a > *b;
  return boxed;
}

bool *madlib__number__internal__ltBytes(unsigned char *a, unsigned char *b) {
  bool *boxed = (bool *)GC_malloc(sizeof(bool));
  *boxed = *a < *b;
  return boxed;
}

bool *madlib__number__internal__gteBytes(unsigned char *a, unsigned char *b) {
  bool *boxed = (bool *)GC_malloc(sizeof(bool));
  *boxed = *a >= *b;
  return boxed;
}

bool *madlib__number__internal__lteBytes(unsigned char *a, unsigned char *b) {
  bool *boxed = (bool *)GC_malloc(sizeof(bool));
  *boxed = *a <= *b;
  return boxed;
}

bool *madlib__number__internal__eqByte(unsigned char *a, unsigned char *b) {
  bool *boxed = (bool *)GC_malloc(sizeof(bool));
  *boxed = *a == *b;
  return boxed;
}

// Float

double *madlib__number__internal__numberToFloat(int64_t *a) {
  double *result = (double *)malloc(sizeof(double));
  *result = (double)*a;
  return result;
}

double *madlib__number__internal__addFloats(double *a, double *b) {
  double *boxed = (double *)GC_malloc(sizeof(double));
  *boxed = *a + *b;
  return boxed;
}

double *madlib__number__internal__substractFloats(double *a, double *b) {
  double *boxed = (double *)GC_malloc(sizeof(double));
  *boxed = *a - *b;
  return boxed;
}

double *madlib__number__internal__multiplyFloats(double *a, double *b) {
  double *boxed = (double *)GC_malloc(sizeof(double));
  *boxed = *a * *b;
  return boxed;
}

bool *madlib__number__internal__gtFloats(double *a, double *b) {
  bool *boxed = (bool *)GC_malloc(sizeof(bool));
  *boxed = *a > *b;
  return boxed;
}

bool *madlib__number__internal__ltFloats(double *a, double *b) {
  bool *boxed = (bool *)GC_malloc(sizeof(bool));
  *boxed = *a < *b;
  return boxed;
}

bool *madlib__number__internal__gteFloats(double *a, double *b) {
  bool *boxed = (bool *)GC_malloc(sizeof(bool));
  *boxed = *a >= *b;
  return boxed;
}

bool *madlib__number__internal__lteFloats(double *a, double *b) {
  bool *boxed = (bool *)GC_malloc(sizeof(bool));
  *boxed = *a <= *b;
  return boxed;
}

bool *madlib__number__internal__eqFloat(double *a, double *b) {
  bool *boxed = (bool *)GC_malloc(sizeof(bool));
  *boxed = *a == *b;
  return boxed;
}

// Integer

int64_t *madlib__number__internal__numberToInteger(int64_t *a) {
  int64_t *result = (int64_t *)malloc(sizeof(int64_t));
  *result = (int64_t)*a;
  return result;
}

int64_t *madlib__number__internal__addIntegers(int64_t *a, int64_t *b) {
  int64_t *boxed = (int64_t *)GC_malloc(sizeof(int64_t));
  *boxed = *a + *b;
  return boxed;
}

int64_t *madlib__number__internal__substractIntegers(int64_t *a, int64_t *b) {
  int64_t *boxed = (int64_t *)GC_malloc(sizeof(int64_t));
  *boxed = *a - *b;
  return boxed;
}

int64_t *madlib__number__internal__multiplyIntegers(int64_t *a, int64_t *b) {
  int64_t *boxed = (int64_t *)GC_malloc(sizeof(int64_t));
  *boxed = *a * *b;
  return boxed;
}

bool *madlib__number__internal__gtIntegers(int64_t *a, int64_t *b) {
  bool *boxed = (bool *)GC_malloc(sizeof(bool));
  *boxed = *a > *b;
  return boxed;
}

bool *madlib__number__internal__ltIntegers(int64_t *a, int64_t *b) {
  bool *boxed = (bool *)GC_malloc(sizeof(bool));
  *boxed = *a < *b;
  return boxed;
}

bool *madlib__number__internal__gteIntegers(int64_t *a, int64_t *b) {
  bool *boxed = (bool *)GC_malloc(sizeof(bool));
  *boxed = *a >= *b;
  return boxed;
}

bool *madlib__number__internal__lteIntegers(int64_t *a, int64_t *b) {
  bool *boxed = (bool *)GC_malloc(sizeof(bool));
  *boxed = *a <= *b;
  return boxed;
}

bool *madlib__number__internal__eqInteger(int64_t *a, int64_t *b) {
  bool *boxed = (bool *)GC_malloc(sizeof(bool));
  *boxed = *a == *b;
  return boxed;
}

#ifdef __cplusplus
}
#endif
