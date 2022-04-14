#include "tuple.hpp"
#include "apply-pap.hpp"
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif


bool madlib__internal__tuple__Tuple_2_eq(madlib__eq__eqDictionary_t* eqDictA, madlib__eq__eqDictionary_t* eqDictB, madlib__tuple__Tuple_2_t *tuple1, madlib__tuple__Tuple_2_t *tuple2) {
  bool areFirstsEqual = (bool)__applyPAP__(&eqDictA->eq, 2, tuple1->first, tuple2->first);
  bool areSecondsEqual = (bool)__applyPAP__(&eqDictB->eq, 2, tuple1->second, tuple2->second);

  return areFirstsEqual && areSecondsEqual;
}

#ifdef __cplusplus
}
#endif
