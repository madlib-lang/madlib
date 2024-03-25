#ifndef BYTEARRAY_H
#define BYTEARRAY_H

#include <stdio.h>
#include <stdint.h>

#include "eq.hpp"
#include "list.hpp"
#include "number.hpp"

typedef struct madlib__bytearray__ByteArray {
  int64_t length;
  int64_t capacity;
  unsigned char *bytes;
} madlib__bytearray__ByteArray_t;

#ifdef __cplusplus
extern "C" {
#endif

int64_t madlib__bytearray__length(madlib__bytearray__ByteArray_t *array);

madlib__bytearray__ByteArray_t *madlib__bytearray__initWithCapacity(int64_t capacity);

unsigned char madlib__bytearray__unsafeAt(int64_t index, madlib__bytearray__ByteArray_t *array);
madlib__bytearray__ByteArray_t *madlib__bytearray__unsafeSet(int64_t index, unsigned char byte, madlib__bytearray__ByteArray_t *array);

bool madlib__bytearray__internal__eq(madlib__bytearray__ByteArray_t *arr1, madlib__bytearray__ByteArray_t *arr2);
char *madlib__bytearray__internal__show(madlib__bytearray__ByteArray_t *bytearray);

madlib__bytearray__ByteArray_t *madlib__bytearray__fromString(char *string);
char *madlib__bytearray__toString(madlib__bytearray__ByteArray_t *arr);

madlib__bytearray__ByteArray_t *madlib__bytearray__fromList(madlib__list__Node_t *list);
madlib__list__Node_t *madlib__bytearray__toList(madlib__bytearray__ByteArray_t *arr);

madlib__bytearray__ByteArray_t *madlib__bytearray__concat(madlib__bytearray__ByteArray_t *a, madlib__bytearray__ByteArray_t *b);

madlib__bytearray__ByteArray_t *madlib__bytearray__concatWithMutation(madlib__bytearray__ByteArray_t *a, madlib__bytearray__ByteArray_t *b);

madlib__bytearray__ByteArray_t *madlib__bytearray__pushBackWithMutation(unsigned char byte, madlib__bytearray__ByteArray_t *a);

madlib__bytearray__ByteArray_t *madlib__bytearray__map(PAP_t *f, madlib__bytearray__ByteArray_t *arr);

void *madlib__bytearray__reduce(PAP_t *f, void *initialValue, madlib__bytearray__ByteArray_t *arr);

#ifdef __cplusplus
}
#endif

#endif // BYTEARRAY_H
