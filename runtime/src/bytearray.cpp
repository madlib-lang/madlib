
#include <gc.h>
#include "bytearray.hpp"
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
  result->bytes = (unsigned char *)GC_MALLOC(capacity * sizeof(unsigned char));
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

bool madlib__bytearray__internal__eq(madlib__bytearray__ByteArray_t *arr1, madlib__bytearray__ByteArray_t *arr2) {
  bool result = false;

  if (arr1->length != arr2->length) {
    result = false;
  } else {
    result = true;

    for (int i = 0; result && i < arr1->length; i++) {
      result = arr1->bytes[i] == arr2->bytes[i];
    }
  }

  return result;
}


char *madlib__bytearray__internal__show(madlib__bytearray__ByteArray_t *bytearray) {
  int64_t length = bytearray->length;

  if (length == 0) {
    return (char*)"ByteArray([])";
  }

  int currentIndex = 0;
  char *inspectedItems[length];
  size_t sizeOfItems = 0;

  for (int i = 0; i < length; i++) {
    inspectedItems[i] = madlib__number__internal__showByte(bytearray->bytes[i]);
    sizeOfItems += strlen(inspectedItems[i]);
  }

  size_t sizeOfSpacesAndCommas = (length - 1) * 2;
  char *result = (char*)GC_MALLOC_ATOMIC(sizeof(char) * (sizeOfItems + sizeOfSpacesAndCommas + 12));

  // Leading "ByteArray(["
  strncpy(result, "ByteArray(", sizeof(char) * 10);
  size_t currentPosition = 10;

  // Items
  for (int i = 0; i < length - 1; i++) {
    size_t lengthOfItem = strlen(inspectedItems[i]);
    strncpy(result + currentPosition, inspectedItems[i], lengthOfItem);
    if ((i + 1) % 8 == 0 && i > 0 && i < length - 1) {
      // add space separator
      strncpy(result + currentPosition + lengthOfItem, " ", sizeof(char));
      currentPosition += lengthOfItem + 1;
    } else {
      currentPosition += lengthOfItem;
    }
  }

  // Last item does not have ", " at the end
  size_t lengthOfItem = strlen(inspectedItems[length - 1]);
  strncpy(result + currentPosition, inspectedItems[length - 1], lengthOfItem);
  strncpy(result + currentPosition + lengthOfItem, ")\0", sizeof(char) * 2);

  return result;
}


char *madlib__bytearray__toString(madlib__bytearray__ByteArray_t *arr) {
  char *string = (char*) arr->bytes;

  if (arr->bytes[arr->length - 1] > 0) {
    string = (char *)GC_MALLOC_ATOMIC(sizeof(char) * (arr->length + 1));
    memcpy(string, arr->bytes, arr->length);
    string[arr->length] = '\0';
  }

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
    resultBytes = (unsigned char *) GC_MALLOC(nextLength * 2 * sizeof(unsigned char));
    memcpy(resultBytes, a->bytes, a->length * sizeof(unsigned char));
    a->bytes = resultBytes;
    a->capacity = nextLength * 2;
  }

  memcpy(resultBytes + a->length, b->bytes, b->length * sizeof(unsigned char));
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
