
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


double unboxDouble(void *x) {
  return *(double*)&x;
}


void *boxDouble(double x) {
  double **w = (double**) &x;
  return (void*) *w;
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


// Short

char *madlib__number__internal__showShort(int32_t i) {
  char *str = (char *)GC_MALLOC_ATOMIC(30);
  sprintf(str, "%" PRId32, i);

  return str;
}

madlib__maybe__Maybe_t *madlib__number__scanShort(char *s) {
  madlib__maybe__Maybe_t *result = (madlib__maybe__Maybe_t*)GC_MALLOC(sizeof(madlib__maybe__Maybe_t));
  int32_t parsed;
  int success = sscanf(s, "%" SCNd32, &parsed);

  if (success == 1) {
    result->index = 0;
    result->data = (int32_t*)parsed;
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

int32_t madlib__number__intToShort(int64_t x) {
  return x;
}

double madlib__number__byteToFloat(unsigned char x) {
  return x;
}

int64_t madlib__number__byteToInt(unsigned char x) {
  return x;
}

int32_t madlib__number__byteToShort(unsigned char x) {
  return x;
}

int64_t madlib__number__floatToInt(double x) {
  return trunc(x);
}

unsigned char madlib__number__floatToByte(double x) {
  return trunc(x);
}

int32_t madlib__number__floatToShort(double x) {
  return trunc(x);
}

int64_t madlib__number__shortToInt(int32_t x) {
  return x;
}

double madlib__number__shortToFloat(int32_t x) {
  return x;
}

unsigned char madlib__number__shortToByte(int32_t x) {
  return x;
}

int32_t madlib__number__shortToChar(int32_t x) {
  return x;
}

int32_t madlib__number__charToShort(int32_t x) {
  return x;
}


#ifdef __cplusplus
}
#endif
