#ifndef DICTIONARY_H
#define DICTIONARY_H

#include <iostream>
#include "eq.hpp"
#include "list.hpp"

typedef struct madlib__dictionary__Dictionary {
  int64_t constructorIndex;
  void *items;
} madlib__dictionary__Dictionary_t;

#ifdef __cplusplus
extern "C" {
#endif

bool *madlib__dictionary__internal__eq(madlib__eq__eqDictionary_t* eqDictA, madlib__eq__eqDictionary_t* eqDictB, madlib__dictionary__Dictionary_t *d1, madlib__dictionary__Dictionary_t *d2);

madlib__dictionary__Dictionary_t *madlib__dictionary__typeConstructor(madlib__list__Node_t *items);

madlib__dictionary__Dictionary_t *madlib__dictionary__fromList(madlib__eq__eqDictionary_t* eqDict, madlib__list__Node_t **boxedItems);

/**
 * Used for the syntax sugar constructor {{ key1: value1, key2: value2 }}
 */
madlib__dictionary__Dictionary_t *__dict_ctor__(madlib__eq__eqDictionary_t* eqDict, madlib__list__Node_t **boxedItems);

#ifdef __cplusplus
}
#endif

#endif // DICTIONARY_H