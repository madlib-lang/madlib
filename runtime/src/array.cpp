#include <gc.h>
#include <cstring>
#include <stdio.h>

#include "list.hpp"

typedef struct MadArray {
  int64_t length;
  void **items;  // an item is a void*
} MadArray_t;


#ifdef __cplusplus
extern "C" {
#endif


MadArray_t *MadArray_fromList(MadListNode_t *list) {
  int64_t itemCount = MadList_length(list);

  MadArray_t *result = (MadArray_t *)GC_malloc(sizeof(MadArray_t));
  result->items = (void **)GC_malloc(itemCount * sizeof(void*));
  result->length = itemCount;

  for (int i = 0; i < itemCount; i++) {
    result->items[i] = list->value;
    list = list->next;
  }

  return result;
}

int64_t MadArray_length(MadArray_t *array) { return array->length; }

MadArray_t *MadArray_concat(MadArray_t *a, MadArray_t *b) {
  MadArray_t *result = (MadArray_t *)GC_malloc(sizeof(MadArray_t));
  result->items = (void **)GC_malloc((a->length + b->length) * sizeof(void*));

  memcpy(result->items, a->items, a->length * sizeof(void*));
  memcpy(result->items + a->length, b->items, b->length * sizeof(void*));

  result->length = a->length + b->length;

  return result;
}

MadArray_t *MadArray_map(PAP_t *f, MadArray_t *arr) {
  MadArray_t *result = (MadArray_t *)GC_malloc(sizeof(MadArray_t));
  result->length = arr->length;
  result->items = (void **)GC_malloc(arr->length * sizeof(void*));

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
