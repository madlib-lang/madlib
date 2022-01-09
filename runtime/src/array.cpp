#include "array.hpp"

#include <gc.h>
#include <stdio.h>

#include <cstring>



#ifdef __cplusplus
extern "C" {
#endif

int64_t madlib__array__length(madlib__array__Array_t *array) { return array->length; }

bool *madlib__array__internal__eq(EqDictionary_t *eqDict, madlib__array__Array_t *arr1, madlib__array__Array_t *arr2) {
  bool *boxed = (bool *)GC_malloc(sizeof(bool));

  if (arr1->length != arr2->length) {
    *boxed = false;
  } else {
    bool result = true;

    for (int i = 0; result && i < arr1->length; i++) {
      result = *(bool *)__applyPAP__((void *)&eqDict->eq, 2, arr1->items[i],
                                     arr2->items[i]);
    }

    *boxed = result;
  }

  return boxed;
}

madlib__array__Array_t *madlib__array__fromList(madlib__list__Node_t *list) {
  int64_t itemCount = madlib__list__length(list);

  madlib__array__Array_t *result = (madlib__array__Array_t *)GC_malloc(sizeof(madlib__array__Array_t));
  result->items = (void **)GC_malloc(itemCount * sizeof(void *));
  result->length = itemCount;

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
  madlib__array__Array_t *result = (madlib__array__Array_t *)GC_malloc(sizeof(madlib__array__Array_t));
  result->items = (void **)GC_malloc((a->length + b->length) * sizeof(void *));

  memcpy(result->items, a->items, a->length * sizeof(void *));
  memcpy(result->items + a->length, b->items, b->length * sizeof(void *));

  result->length = a->length + b->length;

  return result;
}

madlib__array__Array_t *madlib__array__map(PAP_t *f, madlib__array__Array_t *arr) {
  madlib__array__Array_t *result = (madlib__array__Array_t *)GC_malloc(sizeof(madlib__array__Array_t));
  result->length = arr->length;
  result->items = (void **)GC_malloc(arr->length * sizeof(void *));

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
