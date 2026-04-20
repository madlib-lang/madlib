
#include <gc.h>
#include "bytearray.hpp"
#include "string_header.hpp"
#include <stdio.h>
#include <cstring>

#ifdef __cplusplus
extern "C" {
#endif

int64_t madlib__bytearray__length(madlib__bytearray__ByteArray_t *array) { return array->length; }


madlib__bytearray__ByteArray_t *madlib__bytearray__initWithCapacity(int64_t capacity) {
  if (capacity <= 0) {
    capacity = 1;
  }

  madlib__bytearray__ByteArray_t *result = (madlib__bytearray__ByteArray_t *) GC_MALLOC(sizeof(madlib__bytearray__ByteArray_t));
  result->bytes = (unsigned char *)GC_MALLOC_ATOMIC(capacity * sizeof(unsigned char));
  result->capacity = capacity;
  result->length = 0;
  return result;
}


unsigned char madlib__bytearray__unsafeAt(int64_t index, madlib__bytearray__ByteArray_t *array) {
  if (index >= array->length) {
    fprintf(stderr, "Array out of bounds access\nYou accessed the index '%lld' but the array currently has length '%lld'.\n", index, array->length);
    exit(1);
  }
  return array->bytes[index];
}


madlib__bytearray__ByteArray_t *madlib__bytearray__unsafeSet(int64_t index, unsigned char byte, madlib__bytearray__ByteArray_t *array) {
  if (index >= array->length) {
    fprintf(stderr, "Array out of bounds access\nYou accessed the index '%lld' but the array currently has length '%lld'.\n", index, array->length);
    exit(1);
  }

  array->bytes[index] = byte;

  return array;
}


bool madlib__bytearray__internal__eq(madlib__bytearray__ByteArray_t *arr1, madlib__bytearray__ByteArray_t *arr2) {
  if (arr1->length != arr2->length) {
    return false;
  }
  return memcmp(arr1->bytes, arr2->bytes, arr1->length) == 0;
}


char *madlib__bytearray__internal__show(madlib__bytearray__ByteArray_t *bytearray) {
  int64_t length = bytearray->length;

  if (length == 0) {
    char *empty = madlib__string__alloc_bytes(13);
    memcpy(empty, "ByteArray([])", 13);
    empty[13] = '\0';
    return empty;
  }

  char **inspectedItems = (char **)GC_MALLOC(length * sizeof(char *));
  size_t sizeOfItems = 0;

  for (int i = 0; i < length; i++) {
    inspectedItems[i] = madlib__number__internal__showByte(bytearray->bytes[i]);
    sizeOfItems += madstr_byte_len(inspectedItems[i]);
  }

  // Count group-separator spaces: one space after every 8th item (index 7, 15, …), not after last
  size_t spacesCount = 0;
  for (int i = 7; i < length - 1; i += 8) spacesCount++;

  // "ByteArray(" (10) + items + spaces + ")" (1) = totalSize
  size_t totalSize = 10 + sizeOfItems + spacesCount + 1;
  char *result = madlib__string__alloc_bytes((uint32_t)totalSize);

  memcpy(result, "ByteArray(", 10);
  size_t currentPosition = 10;

  for (int i = 0; i < length - 1; i++) {
    size_t itemLen = madstr_byte_len(inspectedItems[i]);
    memcpy(result + currentPosition, inspectedItems[i], itemLen);
    currentPosition += itemLen;
    if ((i + 1) % 8 == 0 && i > 0) {
      result[currentPosition++] = ' ';
    }
  }

  size_t lastItemLen = madstr_byte_len(inspectedItems[length - 1]);
  memcpy(result + currentPosition, inspectedItems[length - 1], lastItemLen);
  currentPosition += lastItemLen;
  result[currentPosition] = ')';
  result[currentPosition + 1] = '\0';

  return result;
}


char *madlib__bytearray__toString(madlib__bytearray__ByteArray_t *arr) {
  char *string = madlib__string__alloc_bytes((uint32_t)arr->length);
  memcpy(string, arr->bytes, arr->length);
  string[arr->length] = '\0';
  return string;
}

madlib__bytearray__ByteArray_t *madlib__bytearray__fromString(char *string) {
  size_t length = strlen(string);

  madlib__bytearray__ByteArray_t *result =
      (madlib__bytearray__ByteArray_t *)GC_MALLOC(sizeof(madlib__bytearray__ByteArray_t));

  result->bytes = (unsigned char*) string;
  result->length = length;
  result->capacity = length;

  return result;
}


madlib__bytearray__ByteArray_t *madlib__bytearray__fromList(madlib__list__Node_t *list) {
  int64_t itemCount = madlib__list__length(list);

  madlib__bytearray__ByteArray_t *result =
      (madlib__bytearray__ByteArray_t *)GC_MALLOC(sizeof(madlib__bytearray__ByteArray_t));
  result->bytes = (unsigned char *)GC_MALLOC_ATOMIC(itemCount * sizeof(unsigned char));
  result->length = itemCount;
  result->capacity = itemCount;

  for (int i = 0; i < itemCount; i++) {
    result->bytes[i] = (unsigned char)(int64_t)list->value;
    list = list->next;
  }

  return result;
}


madlib__list__Node_t *madlib__bytearray__toList(madlib__bytearray__ByteArray_t *arr) {
  int64_t itemCount = madlib__bytearray__length(arr);
  madlib__list__Node_t *result = madlib__list__empty();

  for (int i = itemCount - 1; i >= 0; i--) {
    result = madlib__list__push((void*)(int64_t)arr->bytes[i], result);
  }

  return result;
}

madlib__bytearray__ByteArray_t *madlib__bytearray__concat(madlib__bytearray__ByteArray_t *a,
                                                          madlib__bytearray__ByteArray_t *b) {
  madlib__bytearray__ByteArray_t *result =
      (madlib__bytearray__ByteArray_t *)GC_MALLOC(sizeof(madlib__bytearray__ByteArray_t));
  result->bytes = (unsigned char *)GC_MALLOC_ATOMIC((a->length + b->length) * sizeof(unsigned char));

  memcpy(result->bytes, a->bytes, a->length * sizeof(unsigned char));
  memcpy(result->bytes + a->length, b->bytes, b->length * sizeof(unsigned char));

  result->length = a->length + b->length;
  result->capacity = a->length + b->length;

  return result;
}


madlib__bytearray__ByteArray_t *madlib__bytearray__concatWithMutation(madlib__bytearray__ByteArray_t *a, madlib__bytearray__ByteArray_t *b) {
  unsigned char *resultBytes = a->bytes;
  int64_t nextLength = a->length + b->length;

  if (a->capacity < nextLength) {
    resultBytes = (unsigned char *)GC_MALLOC_ATOMIC(nextLength * 2 * sizeof(unsigned char));
    memcpy(resultBytes, a->bytes, a->length * sizeof(unsigned char));
    a->bytes = resultBytes;
    a->capacity = nextLength * 2;
  }

  memcpy(resultBytes + a->length, b->bytes, b->length * sizeof(unsigned char));
  a->length = nextLength;

  return a;
}


madlib__bytearray__ByteArray_t *madlib__bytearray__pushBackWithMutation(unsigned char byte, madlib__bytearray__ByteArray_t *a) {
  unsigned char *resultBytes = a->bytes;
  int64_t nextLength = a->length + 1;

  if (a->capacity < nextLength) {
    resultBytes = (unsigned char *)GC_MALLOC_ATOMIC(nextLength * 2 * sizeof(unsigned char));
    memcpy(resultBytes, a->bytes, a->length * sizeof(unsigned char));
    a->bytes = resultBytes;
    a->capacity = nextLength * 2;
  }

  resultBytes[a->length] = byte;
  a->length = nextLength;

  return a;
}


madlib__bytearray__ByteArray_t *madlib__bytearray__map(PAP_t *f, madlib__bytearray__ByteArray_t *arr) {
  madlib__bytearray__ByteArray_t *result =
      (madlib__bytearray__ByteArray_t *)GC_MALLOC(sizeof(madlib__bytearray__ByteArray_t));
  result->length = arr->length;
  result->capacity = arr->length;
  result->bytes = (unsigned char *)GC_MALLOC_ATOMIC(arr->length * sizeof(unsigned char));

  for (int i = 0; i < arr->length; i++) {
    result->bytes[i] = (unsigned char)(int64_t)__applyPAP__(f, 1, (void*)arr->bytes[i]);
  }

  return result;
}


void *madlib__bytearray__reduce(PAP_t *f, void *initialValue, madlib__bytearray__ByteArray_t *arr) {
  for (int i = 0; i < arr->length; i++) {
    initialValue = __applyPAP__(f, 2, initialValue, (void*)arr->bytes[i]);
  }

  return initialValue;
}

#ifdef __cplusplus
}
#endif
