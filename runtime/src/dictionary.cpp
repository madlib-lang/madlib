#include <gc.h>

#include "dictionary.hpp"
#include "tuple.hpp"


#ifdef __cplusplus
extern "C" {
#endif

// TODO: implement
bool *madlib__dictionary__internal__eq(madlib__eq__eqDictionary_t* eqDictA, madlib__eq__eqDictionary_t* eqDictB, madlib__dictionary__Dictionary_t *d1, madlib__dictionary__Dictionary_t *d2) {
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


madlib__dictionary__Dictionary_t *madlib__dictionary__typeConstructor(madlib__list__Node_t *items) {
  madlib__dictionary__Dictionary_t *dictionary = (madlib__dictionary__Dictionary_t*) GC_malloc(sizeof(madlib__dictionary__Dictionary_t));
  madlib__list__Node_t **boxedItems = (madlib__list__Node_t**) GC_malloc(sizeof(madlib__list__Node_t*));
  *boxedItems = items;

  dictionary->constructorIndex = 0;
  dictionary->items = boxedItems;

  return dictionary;
}

madlib__dictionary__Dictionary_t *madlib__dictionary__fromList(madlib__eq__eqDictionary_t* eqDict, madlib__list__Node_t **boxedItems) {
  madlib__list__Node_t *head = *boxedItems;
  // Result, starting from an empty list, we push items as we go through them
  // if there is no double
  madlib__list__Node_t *withoutDoubles = madlib__list__empty();

  while (head->value != NULL) {
    madlib__tuple__Tuple_2_t *tuple = (madlib__tuple__Tuple_2_t *) head->value;
    bool isAlreadyThere = false;
    madlib__list__Node_t *withoutDoublesHead = withoutDoubles;

    while (withoutDoublesHead->value != NULL) {
      madlib__tuple__Tuple_2_t *tupleInner = (madlib__tuple__Tuple_2_t *) withoutDoublesHead->value;
      if (*(bool*)__applyPAP__((void*)&eqDict->eq, 2, tuple->first, tupleInner->first)) {
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

  madlib__dictionary__Dictionary_t *dictionary = (madlib__dictionary__Dictionary_t*) GC_malloc(sizeof(madlib__dictionary__Dictionary_t));
  dictionary->constructorIndex = 0;
  dictionary->items = boxed;

  return dictionary;
}

// fromList
madlib__dictionary__Dictionary_t *__dict_ctor__(madlib__eq__eqDictionary_t* eqDict, madlib__list__Node_t **boxedItems) {
  return madlib__dictionary__fromList(eqDict, boxedItems);
}

#ifdef __cplusplus
}
#endif
