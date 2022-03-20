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

unsigned char *madlib__number__internal__negateByte(unsigned char *a) {
  unsigned char *boxed = (unsigned char *)GC_malloc(sizeof(unsigned char));
  *boxed = -(*a);
  return boxed;
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

unsigned char *madlib__number__internal__andBytes(unsigned char *a,
                                                       unsigned char *b) {
  unsigned char *boxed = (unsigned char *)GC_malloc(sizeof(unsigned char));
  *boxed = *a & *b;
  return boxed;
}

unsigned char *madlib__number__internal__orBytes(unsigned char *a,
                                                       unsigned char *b) {
  unsigned char *boxed = (unsigned char *)GC_malloc(sizeof(unsigned char));
  *boxed = *a | *b;
  return boxed;
}

unsigned char *madlib__number__internal__xorBytes(unsigned char *a,
                                                       unsigned char *b) {
  unsigned char *boxed = (unsigned char *)GC_malloc(sizeof(unsigned char));
  *boxed = *a ^ *b;
  return boxed;
}

unsigned char *madlib__number__internal__complementBytes(unsigned char *a) {
  unsigned char *boxed = (unsigned char *)GC_malloc(sizeof(unsigned char));
  *boxed = ~(*a);
  return boxed;
}

unsigned char *madlib__number__internal__leftShiftBytes(unsigned char *a,
                                                       unsigned char *b) {
  unsigned char *boxed = (unsigned char *)GC_malloc(sizeof(unsigned char));
  *boxed = *a << *b;
  return boxed;
}

unsigned char *madlib__number__internal__rightShiftBytes(unsigned char *a,
                                                       unsigned char *b) {
  unsigned char *boxed = (unsigned char *)GC_malloc(sizeof(unsigned char));
  *boxed = *a >> *b;
  return boxed;
}

madlib__maybe__Maybe_t *madlib__number__scanByte(char *s) {
  madlib__maybe__Maybe_t *result = (madlib__maybe__Maybe_t*)GC_malloc(sizeof(madlib__maybe__Maybe_t));
  unsigned char parsed;
  int success = sscanf(s, "%c", &parsed);

  if (success == 1) {
    unsigned char *boxed = (unsigned char*)GC_malloc(sizeof(unsigned char));
    *boxed = parsed;
    result->index = 0;
    result->data = boxed;
  } else {
    result->index = 1;
    result->data = NULL;
  }

  return result;
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

double *madlib__number__internal__negateFloat(double *a) {
  double *boxed = (double *)GC_malloc(sizeof(double));
  *boxed = -(*a);
  return boxed;
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

madlib__maybe__Maybe_t *madlib__number__scanFloat(char *s) {
  madlib__maybe__Maybe_t *result = (madlib__maybe__Maybe_t*)GC_malloc(sizeof(madlib__maybe__Maybe_t));
  double parsed;
  int success = sscanf(s, "%lf", &parsed);

  if (success == 1) {
    double *boxed = (double*)GC_malloc(sizeof(double));
    *boxed = parsed;
    result->index = 0;
    result->data = boxed;
  } else {
    result->index = 1;
    result->data = NULL;
  }

  return result;
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

int64_t *madlib__number__internal__negateInteger(int64_t *a) {
  int64_t *boxed = (int64_t *)GC_malloc(sizeof(int64_t));
  *boxed = -(*a);
  return boxed;
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

int64_t *madlib__number__internal__andIntegers(int64_t *a, int64_t *b) {
  int64_t *boxed = (int64_t *)GC_malloc(sizeof(int64_t));
  *boxed = *a & *b;
  return boxed;
}

int64_t *madlib__number__internal__orIntegers(int64_t *a, int64_t *b) {
  int64_t *boxed = (int64_t *)GC_malloc(sizeof(int64_t));
  *boxed = *a | *b;
  return boxed;
}

int64_t *madlib__number__internal__xorIntegers(int64_t *a, int64_t *b) {
  int64_t *boxed = (int64_t *)GC_malloc(sizeof(int64_t));
  *boxed = *a ^ *b;
  return boxed;
}

int64_t *madlib__number__internal__complementIntegers(int64_t *a) {
  int64_t *boxed = (int64_t *)GC_malloc(sizeof(int64_t));
  *boxed = ~(*a);
  return boxed;
}

int64_t *madlib__number__internal__leftShiftIntegers(int64_t *a, int64_t *b) {
  int64_t *boxed = (int64_t *)GC_malloc(sizeof(int64_t));
  *boxed = *a << *b;
  return boxed;
}

int64_t *madlib__number__internal__rightShiftIntegers(int64_t *a, int64_t *b) {
  int64_t *boxed = (int64_t *)GC_malloc(sizeof(int64_t));
  *boxed = *a >> *b;
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

madlib__maybe__Maybe_t *madlib__number__scanInteger(char *s) {
  madlib__maybe__Maybe_t *result = (madlib__maybe__Maybe_t*)GC_malloc(sizeof(madlib__maybe__Maybe_t));
  int64_t parsed;
  int success = sscanf(s, "%" SCNd64, &parsed);

  if (success == 1) {
    int64_t *boxed = (int64_t*)GC_malloc(sizeof(int64_t));
    *boxed = parsed;
    result->index = 0;
    result->data = boxed;
  } else {
    result->index = 1;
    result->data = NULL;
  }

  return result;
}


// conversion

double madlib__number__intToFloat(int64_t x) {
  return x;
}

double madlib__number__byteToFloat(char x) {
  return x;
}

int64_t madlib__number__byteToInt(char x) {
  return x;
}


#ifdef __cplusplus
}
#endif
