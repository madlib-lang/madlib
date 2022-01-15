#ifndef ARRAY_H
#define ARRAY_H

#include <stdio.h>

#include "eq.hpp"
#include "list.hpp"

typedef struct madlib__bytearray__ByteArray {
  int64_t length;
  unsigned char *bytes;
} madlib__bytearray__ByteArray_t;

#ifdef __cplusplus
extern "C" {
#endif

int64_t madlib__bytearray__length(madlib__bytearray__ByteArray_t *array);

// bool *madlib__array__internal__eq(madlib__eq__eqDictionary_t *eqDict,
// madlib__array__Array_t *arr1, madlib__array__Array_t *arr2);

madlib__bytearray__ByteArray_t *madlib__bytearray__fromList(
    madlib__list__Node_t *list);

// madlib__list__Node_t *madlib__array__toList(madlib__array__Array_t *arr);

// madlib__array__Array_t *madlib__array__concat(madlib__array__Array_t *a,
// madlib__array__Array_t *b);

madlib__bytearray__ByteArray_t *madlib__bytearray__map(
    PAP_t *f, madlib__bytearray__ByteArray_t *arr);

// void *madlib__array__reduce(PAP_t *f, void *initialValue,
// madlib__array__Array_t *arr);

#ifdef __cplusplus
}
#endif

#endif  // ARRAY_H
