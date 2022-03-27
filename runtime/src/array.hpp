#ifndef ARRAY_H
#define ARRAY_H

#include <stdio.h>
#include <stdint.h>

#include "list.hpp"
#include "eq.hpp"

typedef struct madlib__array__Array {
  int64_t length;
  void **items; // an item is a void*
} madlib__array__Array_t;



#ifdef __cplusplus
extern "C" {
#endif

int64_t madlib__array__length(madlib__array__Array_t *array);

bool *madlib__array__internal__eq(madlib__eq__eqDictionary_t *eqDict, madlib__array__Array_t *arr1, madlib__array__Array_t *arr2);
char **madlib__array__internal__inspect(madlib__inspect__inspectDictionary_t *inspectDict, madlib__array__Array_t *list);

madlib__array__Array_t *madlib__array__fromList(madlib__list__Node_t *list);

madlib__list__Node_t *madlib__array__toList(madlib__array__Array_t *arr);

madlib__array__Array_t *madlib__array__concat(madlib__array__Array_t *a, madlib__array__Array_t *b);

madlib__array__Array_t *madlib__array__map(PAP_t *f, madlib__array__Array_t *arr);

void *madlib__array__reduce(PAP_t *f, void *initialValue, madlib__array__Array_t *arr);

#ifdef __cplusplus
}
#endif


#endif // ARRAY_H
