#ifndef TUPLE_H
#define TUPLE_H

#include "eq.hpp"

typedef struct madlib__tuple__Tuple_2 {
  void *first;
  void *second;
} madlib__tuple__Tuple_2_t;


#ifdef __cplusplus
extern "C" {
#endif

bool *madlib__internal__tuple__Tuple_2_eq(madlib__eq__eqDictionary_t* eqDictA, madlib__eq__eqDictionary_t* eqDictB, madlib__tuple__Tuple_2 *tuple1, madlib__tuple__Tuple_2 *tuple2);

#ifdef __cplusplus
}
#endif

#endif // TUPLE_H
