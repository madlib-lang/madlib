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

char **madlib__dictionary__internal__inspect(madlib__inspect__inspectDictionary_t* inspectDictA, madlib__inspect__inspectDictionary_t* inspectDictB, madlib__dictionary__Dictionary_t *dict) {
  int64_t length = madlib__list__length(*dict->items);

  if (length == 0) {
    char **boxed = (char **)GC_malloc(sizeof(char*));
    *boxed = (char*)"{{}}";
    return boxed;
  }

  madlib__list__Node_t *unboxedList = *dict->items;
  int currentIndex = 0;
  char *inspectedItems[length];
  size_t sizeOfItems = 0;

  for (int i = 0; i < length; i++) {
    madlib__tuple__Tuple_2_t *tuple = (madlib__tuple__Tuple_2_t *)unboxedList->value;
    char *inspectedKey = *(char **)__applyPAP__(&inspectDictA->inspect, 1, tuple->first);
    char *inspectedValue = *(char **)__applyPAP__(&inspectDictB->inspect, 1, tuple->second);

    size_t keyLength = strlen(inspectedKey);
    size_t valueLength = strlen(inspectedValue);
    size_t totalLength = keyLength + valueLength + 3;

    char *keyAndValue = (char*)GC_malloc(sizeof(char) * totalLength);
    strncpy(keyAndValue, inspectedKey, keyLength);
    strncpy(keyAndValue + keyLength, ": ", 2);
    strncpy(keyAndValue + keyLength + 2, inspectedValue, valueLength);
    strncpy(keyAndValue + totalLength - 1, "\0", 1);

    inspectedItems[i] = keyAndValue;
    sizeOfItems += totalLength;
    unboxedList = unboxedList->next;
  }

  size_t sizeOfSpacesAndCommas = (length - 1) * 2;
  char *result = (char*)GC_malloc(sizeof(char) * (sizeOfItems + sizeOfSpacesAndCommas + 7));

  // Leading "["
  strncpy(result, "{{ ", sizeof(char) * 3);
  size_t currentPosition = 3;

  // Items
  for (int i = 0; i < length - 1; i++) {
    size_t lengthOfItem = strlen(inspectedItems[i]);
    strncpy(result + currentPosition, inspectedItems[i], lengthOfItem);
    strncpy(result + currentPosition + lengthOfItem, ", ", sizeof(char) * 2);
    currentPosition += lengthOfItem + 2;
  }

  // Last item does not have ", " at the end
  size_t lengthOfItem = strlen(inspectedItems[length - 1]);
  strncpy(result + currentPosition, inspectedItems[length - 1], lengthOfItem);
  strncpy(result + currentPosition + lengthOfItem, " }}\0", sizeof(char) * 4);

  char **boxed = (char **)GC_malloc(sizeof(char*));
  *boxed = result;
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

    while (!isAlreadyThere && withoutDoublesHead->value != NULL) {
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
