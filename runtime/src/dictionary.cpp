#include <gc.h>
#include <iostream>

#include "list.hpp"


// Dictionary

#ifdef __cplusplus
extern "C" {
#endif

typedef struct Dictionary {
  int64_t constructorIndex;
  void *items;
} Dictionary_t;


Dictionary_t *__Dictionary_constructor__(MadListNode_t *items) {
  Dictionary_t *dictionary = (Dictionary_t*) GC_malloc(sizeof(Dictionary_t));
  MadListNode_t **boxedItems = (MadListNode_t**) GC_malloc(sizeof(MadListNode_t*));
  *boxedItems = items;

  dictionary->constructorIndex = 0;
  dictionary->items = boxedItems;

  return dictionary;
}


typedef struct Tuple {
  void *key;
  void *value;
} Tuple_t;

Dictionary_t *__dict_ctor__(EqDictionary_t* eqDict, MadListNode_t **boxedItems) {
  MadListNode_t *head = *boxedItems;
  // Result, starting from an empty list, we push items as we go through them
  // if there is no double
  MadListNode_t *withoutDoubles = Madlist_empty();

  while (head->value != NULL) {
    Tuple_t *tuple = (Tuple_t *) head->value;
    bool isAlreadyThere = false;
    MadListNode_t *withoutDoublesHead = withoutDoubles;

    while (withoutDoublesHead->value != NULL) {
      Tuple_t *tupleInner = (Tuple_t *) withoutDoublesHead->value;
      if (*(bool*)__applyPAP__((void*)&eqDict->eq, 2, tuple->key, tupleInner->key)) {
        isAlreadyThere = true;
      }

      withoutDoublesHead = withoutDoublesHead->next;
    }

    if (!isAlreadyThere) {
      withoutDoubles = __MadList_push__(tuple, withoutDoubles);
    }

    head = head->next;
  }

  MadListNode_t **boxed = (MadListNode_t **) GC_malloc(sizeof(MadListNode_t *));
  *boxed = withoutDoubles;

  Dictionary_t *dictionary = (Dictionary_t*) GC_malloc(sizeof(Dictionary_t));
  dictionary->constructorIndex = 0;
  dictionary->items = boxed;

  return dictionary;
}

#ifdef __cplusplus
}
#endif
