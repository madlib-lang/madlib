#include "array.hpp"

#include <gc.h>
#include <stdio.h>

#include <cstring>

#ifdef __cplusplus
extern "C" {
#endif

int64_t madlib__array__length(madlib__array__Array_t *array) { return array->length; }

bool madlib__array__internal__eq(PAP_t *eqFn, madlib__array__Array_t *arr1, madlib__array__Array_t *arr2) {
  bool result;

  if (arr1->length != arr2->length) {
    result = false;
  } else {
    result = true;

    for (int i = 0; result && i < arr1->length; i++) {
      result = __applyPAP__(eqFn, 2, arr1->items[i], arr2->items[i]);
    }
  }

  return result;
}

madlib__array__Array_t *madlib__array__fromList(madlib__list__Node_t *list) {
  int64_t itemCount = madlib__list__length(list);

  madlib__array__Array_t *result = (madlib__array__Array_t *)GC_MALLOC(sizeof(madlib__array__Array_t));
  result->items = (void **)GC_MALLOC(itemCount * 2 * sizeof(void *));
  result->length = itemCount;
  result->capacity = itemCount * 2;

  for (int i = 0; i < itemCount; i++) {
    result->items[i] = list->value;
    list = list->next;
  }

  return result;
}

madlib__list__Node_t *madlib__array__toList(madlib__array__Array_t *arr) {
  int64_t itemCount = madlib__array__length(arr);
  madlib__list__Node_t *result = madlib__list__empty();

  for (int i = itemCount - 1; i >= 0; i--) {
    result = madlib__list__push(arr->items[i], result);
  }

  return result;
}

madlib__array__Array_t *madlib__array__concat(madlib__array__Array_t *a, madlib__array__Array_t *b) {
  madlib__array__Array_t *result = (madlib__array__Array_t *)GC_MALLOC(sizeof(madlib__array__Array_t));
  result->length = a->length + b->length;
  result->capacity = result->length * 2;
  result->items = (void **)GC_MALLOC(result->capacity * 2 * sizeof(void *));

  memcpy(result->items, a->items, a->length * sizeof(void *));
  memcpy(result->items + a->length, b->items, b->length * sizeof(void *));

  return result;
}

madlib__array__Array_t *madlib__array__concatWithMutation(madlib__array__Array_t *a, madlib__array__Array_t *b) {
  void **resultItems = a->items;
  int64_t nextLength = a->length + b->length;

  if (a->capacity < nextLength) {
    resultItems = (void **)GC_MALLOC(nextLength * 2 * sizeof(void *));
    memcpy(resultItems, a->items, a->length);
    a->items = resultItems;
    a->capacity = nextLength * 2;
  }

  memcpy(resultItems + a->length, b->items, b->length);
  a->length = nextLength;

  return a;
}

madlib__array__Array_t *madlib__array__pushBackWithMutation(madlib__array__Array_t *a, void *item) {
  void **resultItems = a->items;
  int64_t nextLength = a->length + 1;

  if (a->capacity < nextLength) {
    resultItems = (void **)GC_MALLOC(nextLength * 2 * sizeof(void *));
    memcpy(resultItems, a->items, a->length);
    a->items = resultItems;
    a->capacity = nextLength * 2;
  }

  resultItems[a->length] = item;
  a->length = nextLength;

  return a;
}

madlib__array__Array_t *madlib__array__removeWithMutation(madlib__array__Array_t *a, int64_t index) {
  // TODO: verify that we don't need memmove here
  memcpy(a->items + index, a->items + index + 1, a->length - index - 1);
  return a;
}

madlib__array__Array_t *madlib__array__initWithCapacity(int64_t capacity) {
  madlib__array__Array_t *result = (madlib__array__Array_t *)GC_MALLOC(sizeof(madlib__array__Array_t));
  result->items = (void **)GC_MALLOC(capacity * sizeof(void *));
  result->capacity = capacity;
  result->length = 0;
  return result;
}

madlib__array__Array_t *madlib__array__map(PAP_t *f, madlib__array__Array_t *arr) {
  madlib__array__Array_t *result = (madlib__array__Array_t *)GC_MALLOC(sizeof(madlib__array__Array_t));
  result->length = arr->length;
  result->capacity = arr->capacity;
  result->items = (void **)GC_MALLOC(arr->capacity * sizeof(void *));

  for (int i = 0; i < arr->length; i++) {
    result->items[i] = __applyPAP__(f, 1, arr->items[i]);
  }

  return result;
}

void *madlib__array__reduce(PAP_t *f, void *initialValue, madlib__array__Array_t *arr) {
  for (int i = 0; i < arr->length; i++) {
    initialValue = __applyPAP__(f, 2, initialValue, arr->items[i]);
  }

  return initialValue;
}

#ifdef __cplusplus
}
#endif