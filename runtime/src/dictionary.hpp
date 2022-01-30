#ifndef DICTIONARY_H
#define DICTIONARY_H

#include <iostream>
#include "eq.hpp"
#include "comparable.hpp"
#include "list.hpp"

typedef struct madlib__dictionary__Dictionary {
  int64_t constructorIndex;
  madlib__list__Node_t **items;
} madlib__dictionary__Dictionary_t;

#ifdef __cplusplus
extern "C" {
#endif

bool *madlib__dictionary__internal__eq(madlib__eq__eqDictionary_t* eqDictA, madlib__eq__eqDictionary_t* eqDictB, madlib__dictionary__Dictionary_t *d1, madlib__dictionary__Dictionary_t *d2);
char **madlib__dictionary__internal__inspect(madlib__inspect__inspectDictionary_t* inspectDictA, madlib__inspect__inspectDictionary_t* inspectDictB, madlib__dictionary__Dictionary_t *dict);

madlib__dictionary__Dictionary_t *madlib__dictionary__typeConstructor(madlib__list__Node_t *items);

madlib__dictionary__Dictionary_t *madlib__dictionary__fromList(madlib__eq__eqDictionary_t* eqDict, madlib__list__Node_t *boxedItems);
madlib__list__Node_t *madlib__dictionary__toList(madlib__dictionary__Dictionary_t *boxedItems);

madlib__dictionary__Dictionary_t *madlib__dictionary__insert(madlib__comparable__comparableDictionary_t* comparableDict, void *key, void *value, madlib__dictionary__Dictionary_t *dictionary);
madlib__dictionary__Dictionary_t *madlib__dictionary__empty(bool unit);

/**
 * Used for the syntax sugar constructor {{ key1: value1, key2: value2 }}
 */
madlib__dictionary__Dictionary_t *__dict_ctor__(madlib__eq__eqDictionary_t* eqDict, madlib__list__Node_t **boxedItems);

#ifdef __cplusplus
}
#endif

#endif // DICTIONARY_H