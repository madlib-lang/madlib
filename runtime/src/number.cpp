#include "number.hpp"
#include "string.hpp"

#include <inttypes.h>
#include <gc.h>

#ifdef __cplusplus
extern "C" {
#endif

// Byte

char *madlib__number__internal__showByte(unsigned char i) {
  char *str = (char *)GC_malloc(4 * sizeof(char));
  snprintf(str, 4 * sizeof(char), "%02X", i);

  return str;
}

char **madlib__number__internal__inspectByte(unsigned char *i) {
  char *str = madlib__number__internal__showByte(*i);
  char **boxed = (char**)GC_malloc(sizeof(char*));
  *boxed = str;
  return boxed;
}

unsigned char *madlib__number__internal__numberToByte(int64_t *a) {
  unsigned char *result = (unsigned char *)GC_malloc(sizeof(unsigned char));
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

char *madlib__number__internal__showFloat(double d) {
  char *str = (char *)GC_malloc(200);
  sprintf(str, "%.20f", d);
  char *stripped = stripTrailingZeros(str);

  return stripped;
}

char **madlib__number__internal__inspectFloat(double *d) {
  char *str = madlib__number__internal__showFloat(*d);
  char **boxed = (char**)GC_malloc(sizeof(char*));
  *boxed = str;
  return boxed;
}

double *madlib__number__internal__numberToFloat(int64_t *a) {
  double *result = (double *)GC_malloc(sizeof(double));
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

char *madlib__number__internal__showInteger(int64_t i) {
  char *str = (char *)GC_malloc(200);
  sprintf(str, "%" PRId64, i);

  return str;
}

char **madlib__number__internal__inspectInteger(int64_t *i) {
  char *str = madlib__number__internal__showInteger(*i);
  char **boxed = (char**)GC_malloc(sizeof(char*));
  *boxed = str;
  return boxed;
}

int64_t *madlib__number__internal__numberToInteger(int64_t *a) {
  int64_t *result = (int64_t *)GC_malloc(sizeof(int64_t));
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
