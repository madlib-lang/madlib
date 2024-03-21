#ifndef ARRAY_H
#define ARRAY_H

#include <stdio.h>
#include <stdint.h>

#include "list.hpp"
#include "eq.hpp"
#include "apply-pap.hpp"

typedef struct madlib__array__Array
{
  int64_t length;
  int64_t capacity;
  void **items; // an item is a void*
} madlib__array__Array_t;

#ifdef __cplusplus
extern "C"
{
#endif

  bool madlib__array__internal__eq(PAP_t *eqFn, madlib__array__Array_t *arr1, madlib__array__Array_t *arr2);

  int64_t madlib__array__length(madlib__array__Array_t *array);

  madlib__array__Array_t *madlib__array__fromList(madlib__list__Node_t *list);

  madlib__list__Node_t *madlib__array__toList(madlib__array__Array_t *arr);

  madlib__array__Array_t *madlib__array__concat(madlib__array__Array_t *a, madlib__array__Array_t *b);

  madlib__array__Array_t *madlib__array__concatWithMutation(madlib__array__Array_t *a, madlib__array__Array_t *b);

  madlib__array__Array_t *madlib__array__pushBackWithMutation(madlib__array__Array_t *a, void *item);

  madlib__array__Array_t *madlib__array__removeWithMutation(madlib__array__Array_t *a, int64_t index);

  madlib__array__Array_t *madlib__array__initWithCapacity(int64_t capacity);

  madlib__array__Array_t *madlib__array__map(PAP_t *f, madlib__array__Array_t *arr);

  void *madlib__array__reduce(PAP_t *f, void *initialValue, madlib__array__Array_t *arr);

#ifdef __cplusplus
}
#endif

#endif
