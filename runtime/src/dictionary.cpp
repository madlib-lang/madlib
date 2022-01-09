#include <gc.h>
#include <iostream>

#include "list.hpp"


// Dictionary

#ifdef __cplusplus
extern "C" {
#endif

typedef struct MadDictionary {
  int64_t constructorIndex;
  void *items;
} MadDictionary_t;


// TODO: implement
bool *__eqDictionary__(EqDictionary_t* eqDictA, EqDictionary_t* eqDictB, MadDictionary_t *d1, MadDictionary_t *d2) {
  bool *boxed = (bool*) GC_malloc(sizeof(bool));

  // int64_t l1Length = MadList_length(*l1);
  // int64_t l2Length = MadList_length(*l2);

  // if (l1Length != l2Length) {
  //   *boxed = false;
  // } else {
  //   MadListNode *unboxedL1 = *l1;
  //   MadListNode *unboxedL2 = *l2;

  //   bool result = true;

  //   for (int i=0; result && i < l1Length; i++) {
  //     result = *(bool*)__applyPAP__((void*)&eqDict->eq, 2, unboxedL1->value, unboxedL2->value);
  //     unboxedL1 = unboxedL1->next;
  //     unboxedL2 = unboxedL2->next;
  //   }

  //   *boxed = result;
  // }

  *boxed = false;

  return boxed;
}


MadDictionary_t *__Dictionary_constructor__(madlib__list__Node_t *items) {
  MadDictionary_t *dictionary = (MadDictionary_t*) GC_malloc(sizeof(MadDictionary_t));
  madlib__list__Node_t **boxedItems = (madlib__list__Node_t**) GC_malloc(sizeof(madlib__list__Node_t*));
  *boxedItems = items;

  dictionary->constructorIndex = 0;
  dictionary->items = boxedItems;

  return dictionary;
}


typedef struct Tuple {
  void *key;
  void *value;
} Tuple_t;

MadDictionary_t *__dict_ctor__(EqDictionary_t* eqDict, madlib__list__Node_t **boxedItems) {
  madlib__list__Node_t *head = *boxedItems;
  // Result, starting from an empty list, we push items as we go through them
  // if there is no double
  madlib__list__Node_t *withoutDoubles = madlib__list__empty();

  while (head->value != NULL) {
    Tuple_t *tuple = (Tuple_t *) head->value;
    bool isAlreadyThere = false;
    madlib__list__Node_t *withoutDoublesHead = withoutDoubles;

    while (withoutDoublesHead->value != NULL) {
      Tuple_t *tupleInner = (Tuple_t *) withoutDoublesHead->value;
      if (*(bool*)__applyPAP__((void*)&eqDict->eq, 2, tuple->key, tupleInner->key)) {
        isAlreadyThere = true;
      }

      withoutDoublesHead = withoutDoublesHead->next;
    }

    if (!isAlreadyThere) {
      withoutDoubles = madlib__list__internal__push(tuple, withoutDoubles);
    }

    head = head->next;
  }

  madlib__list__Node_t **boxed = (madlib__list__Node_t **) GC_malloc(sizeof(madlib__list__Node_t *));
  *boxed = withoutDoubles;

  MadDictionary_t *dictionary = (MadDictionary_t*) GC_malloc(sizeof(MadDictionary_t));
  dictionary->constructorIndex = 0;
  dictionary->items = boxed;

  return dictionary;
}

#ifdef __cplusplus
}
#endif
