#include <gc.h>

#include "dictionary.hpp"
#include "tuple.hpp"
#include "apply-pap.hpp"
#include "list.hpp"


#ifdef __cplusplus
extern "C" {
#endif

bool *madlib__dictionary__internal__eq(madlib__eq__eqDictionary_t* eqDictA, madlib__eq__eqDictionary_t* eqDictB, madlib__dictionary__Dictionary_t *d1, madlib__dictionary__Dictionary_t *d2) {
  PAPEnv_2_t env = {
    .arg0 = eqDictA,
    .arg1 = eqDictB
  };

  PAP_t tupleEqPAP = {
    .fn = (void*)madlib__internal__tuple__Tuple_2_eq,
    .arity = 4,
    .missingArgCount = 2,
    .env = &env
  };
  madlib__eq__eqDictionary_t dict = { .eq = tupleEqPAP };

  return madlib__list__internal__eq(&dict, d1->items, d2->items);
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

#ifdef __cplusplus
}
#endif
