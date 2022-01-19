
#include "bytearray.hpp"

#include <gc.h>
#include <stdio.h>

#include <cstring>

#ifdef __cplusplus
extern "C" {
#endif

int64_t madlib__bytearray__length(madlib__bytearray__ByteArray_t *array) { return array->length; }

bool *madlib__bytearray__internal__eq(madlib__bytearray__ByteArray_t *arr1, madlib__bytearray__ByteArray_t *arr2) {
  bool *result = (bool *)GC_malloc(sizeof(bool));

  if (arr1->length != arr2->length) {
    *result = false;
  } else {
    *result = true;

    for (int i = 0; *result && i < arr1->length; i++) {
      *result = arr1->bytes[i] == arr2->bytes[i];
    }
  }

  return result;
}

char *madlib__bytearray__toString(madlib__bytearray__ByteArray_t *arr) {
  char *string = (char*) arr->bytes;

  if (arr->bytes[arr->length - 1] > 0) {
    // TODO: realloc should be ok here
    string = (char *)GC_malloc(sizeof(char) * (arr->length + 1));
    memcpy(string, arr->bytes, arr->length);
    string[arr->length] = '\0';
  }

  return string;
}

madlib__bytearray__ByteArray_t *madlib__bytearray__fromString(char *string) {
  size_t length = strlen(string);

  madlib__bytearray__ByteArray_t *result =
      (madlib__bytearray__ByteArray_t *)GC_malloc(sizeof(madlib__bytearray__ByteArray_t));

  result->bytes = (unsigned char*) string;
  result->length = length;

  return result;
}

madlib__bytearray__ByteArray_t *madlib__bytearray__fromList(madlib__list__Node_t *list) {
  int64_t itemCount = madlib__list__length(list);

  madlib__bytearray__ByteArray_t *result =
      (madlib__bytearray__ByteArray_t *)GC_malloc(sizeof(madlib__bytearray__ByteArray_t));
  result->bytes = (unsigned char *)GC_malloc(itemCount * sizeof(unsigned char));
  result->length = itemCount;

  for (int i = 0; i < itemCount; i++) {
    result->bytes[i] = *(unsigned char *)list->value;
    list = list->next;
  }

  return result;
}

madlib__list__Node_t *madlib__bytearray__toList(madlib__bytearray__ByteArray_t *arr) {
  int64_t itemCount = madlib__bytearray__length(arr);
  madlib__list__Node_t *result = madlib__list__empty();

  for (int i = itemCount - 1; i >= 0; i--) {
    result = madlib__list__push(&arr->bytes[i], result);
  }

  return result;
}

madlib__bytearray__ByteArray_t *madlib__bytearray__concat(madlib__bytearray__ByteArray_t *a,
                                                          madlib__bytearray__ByteArray_t *b) {
  madlib__bytearray__ByteArray_t *result =
      (madlib__bytearray__ByteArray_t *)GC_malloc(sizeof(madlib__bytearray__ByteArray_t));
  result->bytes = (unsigned char *)GC_malloc((a->length + b->length) * sizeof(unsigned char));

  memcpy(result->bytes, a->bytes, a->length * sizeof(unsigned char));
  memcpy(result->bytes + a->length, b->bytes, b->length * sizeof(unsigned char));

  result->length = a->length + b->length;

  return result;
}

madlib__bytearray__ByteArray_t *madlib__bytearray__map(PAP_t *f, madlib__bytearray__ByteArray_t *arr) {
  madlib__bytearray__ByteArray_t *result =
      (madlib__bytearray__ByteArray_t *)GC_malloc(sizeof(madlib__bytearray__ByteArray_t));
  result->length = arr->length;
  result->bytes = (unsigned char *)GC_malloc(arr->length * sizeof(unsigned char));

  for (int i = 0; i < arr->length; i++) {
    result->bytes[i] = *(unsigned char *)__applyPAP__(f, 1, &arr->bytes[i]);
  }

  return result;
}

void *madlib__bytearray__reduce(PAP_t *f, void *initialValue, madlib__bytearray__ByteArray_t *arr) {
  for (int i = 0; i < arr->length; i++) {
    initialValue = __applyPAP__(f, 2, initialValue, &arr->bytes[i]);
  }

  return initialValue;
}

#ifdef __cplusplus
}
#endif
