
#include <gc.h>
#include "number.hpp"
#include "string.hpp"

#include <inttypes.h>


#ifdef __cplusplus
extern "C" {
#endif

// Byte

char *madlib__number__internal__showByte(unsigned char i) {
  char *str = (char *)GC_MALLOC_ATOMIC(3 * sizeof(char));
  snprintf(str, 3 * sizeof(char), "%02X", i);

  return str;
}

char *madlib__number__internal__inspectByte(unsigned char i) {
  char *str = (char *)GC_MALLOC_ATOMIC(3 * sizeof(char));
  snprintf(str, 3 * sizeof(char), "%02X", i);

  return str;
}

unsigned char madlib__number__internal__numberToByte(int64_t a) {
  return a;
}

unsigned char madlib__number__internal__negateByte(unsigned char a) {
  return -a;
}

unsigned char madlib__number__internal__addBytes(unsigned char a,
                                                  unsigned char b) {
  return a + b;
}

unsigned char madlib__number__internal__substractBytes(unsigned char a,
                                                        unsigned char b) {
  return a - b;
}

unsigned char madlib__number__internal__multiplyBytes(unsigned char a,
                                                       unsigned char b) {
  return a * b;
}

bool madlib__number__internal__gtBytes(unsigned char a, unsigned char b) {
  return a > b;
}

bool madlib__number__internal__ltBytes(unsigned char a, unsigned char b) {
  return a < b;
}

bool madlib__number__internal__gteBytes(unsigned char a, unsigned char b) {
  return a >= b;
}

bool madlib__number__internal__lteBytes(unsigned char a, unsigned char b) {
  return a <= b;
}

bool madlib__number__internal__eqByte(unsigned char a, unsigned char b) {
  return a == b;
}

unsigned char madlib__number__internal__andBytes(unsigned char a,
                                                       unsigned char b) {
  return a & b;
}

unsigned char madlib__number__internal__orBytes(unsigned char a,
                                                       unsigned char b) {
  return a | b;
}

unsigned char madlib__number__internal__xorBytes(unsigned char a,
                                                       unsigned char b) {
  return a ^ b;
}

unsigned char madlib__number__internal__complementBytes(unsigned char a) {
  return ~a;
}

unsigned char madlib__number__internal__leftShiftBytes(unsigned char a,
                                                       unsigned char b) {
  return a << b;
}

unsigned char madlib__number__internal__rightShiftBytes(unsigned char a,
                                                       unsigned char b) {
  return a >> b;
}

madlib__maybe__Maybe_t *madlib__number__scanByte(char *s) {
  madlib__maybe__Maybe_t *result = (madlib__maybe__Maybe_t*)GC_MALLOC(sizeof(madlib__maybe__Maybe_t));
  unsigned char parsed;
  int success = sscanf(s, "%c", &parsed);

  if (success == 1) {
    result->index = 0;
    result->data = (unsigned char*)parsed;
  } else {
    result->index = 1;
    result->data = NULL;
  }

  return result;
}

// Float

char *madlib__number__internal__showFloat(double d) {
  char *str = (char *)GC_MALLOC_ATOMIC(200);
  sprintf(str, "%.20f", d);
  char *stripped = stripTrailingZeros(str);

  return stripped;
}


double unboxDouble(double *x) {
  return *(double*)&x;
}


double *boxDouble(double x) {
  double **w = (double**) &x;
  return *w;
}


char *madlib__number__internal__inspectFloat(double *d) {
  char *str = (char *)GC_MALLOC_ATOMIC(200);
  sprintf(str, "%.20f", unboxDouble(d));
  char *stripped = stripTrailingZeros(str);

  return stripped;
}

double *madlib__number__internal__numberToFloat(int64_t a) {
  return boxDouble(a);
}

double *madlib__number__internal__negateFloat(double *a) {
  return boxDouble(-unboxDouble(a));
}

double *madlib__number__internal__addFloats(double *a, double *b) {
  return boxDouble(unboxDouble(a) + unboxDouble(b));
}

double *madlib__number__internal__substractFloats(double *a, double *b) {
  return boxDouble(unboxDouble(a) - unboxDouble(b));
}

double *madlib__number__internal__multiplyFloats(double *a, double *b) {
  return boxDouble(unboxDouble(a) * unboxDouble(b));
}

bool madlib__number__internal__gtFloats(double *a, double *b) {
  return unboxDouble(a) > unboxDouble(b);
}

bool madlib__number__internal__ltFloats(double *a, double *b) {
  return unboxDouble(a) < unboxDouble(b);
}

bool madlib__number__internal__gteFloats(double *a, double *b) {
  return unboxDouble(a) >= unboxDouble(b);
}

bool madlib__number__internal__lteFloats(double *a, double *b) {
  return unboxDouble(a) <= unboxDouble(b);
}

bool madlib__number__internal__eqFloat(double *a, double *b) {
  return unboxDouble(a) == unboxDouble(b);
}

madlib__maybe__Maybe_t *madlib__number__scanFloat(char *s) {
  madlib__maybe__Maybe_t *result = (madlib__maybe__Maybe_t*)GC_MALLOC(sizeof(madlib__maybe__Maybe_t));
  double parsed;
  int success = sscanf(s, "%lf", &parsed);

  if (success == 1) {
    result->index = 0;
    result->data = boxDouble(parsed);
  } else {
    result->index = 1;
    result->data = NULL;
  }

  return result;
}

// Integer

char *madlib__number__internal__showInteger(int64_t i) {
  char *str = (char *)GC_MALLOC_ATOMIC(30);
  sprintf(str, "%" PRId64, i);

  return str;
}

char *madlib__number__internal__inspectInteger(int64_t i) {
  char *str = (char *)GC_MALLOC_ATOMIC(30);
  sprintf(str, "%" PRId64, i);

  return str;
}

int64_t madlib__number__internal__numberToInteger(int64_t a) {
  return a;
}

int64_t madlib__number__internal__negateInteger(int64_t a) {
  return -a;
}

int64_t madlib__number__internal__addIntegers(int64_t a, int64_t b) {
  return a + b;
}

int64_t madlib__number__internal__substractIntegers(int64_t a, int64_t b) {
  return a - b;
}

int64_t madlib__number__internal__multiplyIntegers(int64_t a, int64_t b) {
  return a * b;
}

int64_t madlib__number__internal__andIntegers(int64_t a, int64_t b) {
  return a & b;
}

int64_t madlib__number__internal__orIntegers(int64_t a, int64_t b) {
  return a | b;
}

int64_t madlib__number__internal__xorIntegers(int64_t a, int64_t b) {
  return a ^ b;
}

int64_t madlib__number__internal__complementIntegers(int64_t a) {
  return ~a;
}

int64_t madlib__number__internal__leftShiftIntegers(int64_t a, int64_t b) {
  return a << b;
}

int64_t madlib__number__internal__rightShiftIntegers(int64_t a, int64_t b) {
  return a >> b;
}

bool madlib__number__internal__gtIntegers(int64_t a, int64_t b) {
  return a > b;
}

bool madlib__number__internal__ltIntegers(int64_t a, int64_t b) {
  return a < b;
}

bool madlib__number__internal__gteIntegers(int64_t a, int64_t b) {
  return a >= b;
}

bool madlib__number__internal__lteIntegers(int64_t a, int64_t b) {
  return a <= b;
}

bool madlib__number__internal__eqInteger(int64_t a, int64_t b) {
  return a == b;
}

madlib__maybe__Maybe_t *madlib__number__scanInteger(char *s) {
  madlib__maybe__Maybe_t *result = (madlib__maybe__Maybe_t*)GC_MALLOC(sizeof(madlib__maybe__Maybe_t));
  int64_t parsed;
  int success = sscanf(s, "%" SCNd64, &parsed);

  if (success == 1) {
    int64_t *boxed = (int64_t*)parsed;
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

unsigned char madlib__number__intToByte(int64_t x) {
  return x;
}

double madlib__number__byteToFloat(unsigned char x) {
  return x;
}

int64_t madlib__number__byteToInt(unsigned char x) {
  return x;
}

int64_t madlib__number__floatToInt(double x) {
  return trunc(x);
}

unsigned char madlib__number__floatToByte(double x) {
  return trunc(x);
}


#ifdef __cplusplus
}
#endif
