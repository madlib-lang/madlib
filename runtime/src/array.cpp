#include "array.hpp"

#include <gc.h>
#include <stdio.h>

#include <cstring>

#include "list.hpp"

#ifdef __cplusplus
extern "C" {
#endif

int64_t MadArray_length(MadArray_t *array) { return array->length; }

bool *__eqArray__(EqDictionary_t *eqDict, MadArray_t *arr1, MadArray_t *arr2) {
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

MadArray_t *MadArray_fromList(MadListNode_t *list) {
  int64_t itemCount = MadList_length(list);

  MadArray_t *result = (MadArray_t *)GC_malloc(sizeof(MadArray_t));
  result->items = (void **)GC_malloc(itemCount * sizeof(void *));
  result->length = itemCount;

  for (int i = 0; i < itemCount; i++) {
    result->items[i] = list->value;
    list = list->next;
  }

  return result;
}

MadListNode_t *MadArray_toList(MadArray_t *arr) {
  int64_t itemCount = MadArray_length(arr);
  MadListNode_t *result = MadList_empty();

  for (int i = itemCount - 1; i >= 0; i--) {
    result = MadList_push(arr->items[i], result);
  }

  return result;
}

MadArray_t *MadArray_concat(MadArray_t *a, MadArray_t *b) {
  MadArray_t *result = (MadArray_t *)GC_malloc(sizeof(MadArray_t));
  result->items = (void **)GC_malloc((a->length + b->length) * sizeof(void *));

  memcpy(result->items, a->items, a->length * sizeof(void *));
  memcpy(result->items + a->length, b->items, b->length * sizeof(void *));

  result->length = a->length + b->length;

  return result;
}

MadArray_t *MadArray_map(PAP_t *f, MadArray_t *arr) {
  MadArray_t *result = (MadArray_t *)GC_malloc(sizeof(MadArray_t));
  result->length = arr->length;
  result->items = (void **)GC_malloc(arr->length * sizeof(void *));

  for (int i = 0; i < arr->length; i++) {
    result->items[i] = __applyPAP__(f, 1, arr->items[i]);
  }

  return result;
}

void *MadArray_reduce(PAP_t *f, void *initialValue, MadArray_t *arr) {
  for (int i = 0; i < arr->length; i++) {
    initialValue = __applyPAP__(f, 2, initialValue, arr->items[i]);
  }

  return initialValue;
}

#ifdef __cplusplus
}
#endif
