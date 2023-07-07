
#include <gc.h>
#include <string.h>

#include "dictionary.hpp"
#include "tuple.hpp"
#include "apply-pap.hpp"
#include "list.hpp"


#ifdef __cplusplus
extern "C" {
#endif

madlib__dictionary__Dictionary_t *madlib__dictionary__typeConstructor(madlib__list__Node_t *items) {
  madlib__dictionary__Dictionary_t *dictionary = (madlib__dictionary__Dictionary_t*) GC_MALLOC(sizeof(madlib__dictionary__Dictionary_t));

  dictionary->constructorIndex = 0;
  dictionary->items = items;

  return dictionary;
}

#ifdef __cplusplus
}
#endif
