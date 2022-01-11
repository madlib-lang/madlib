#include "boolean.hpp"

#ifdef __cplusplus
extern "C" {
#endif

bool *madlib__boolean__internal__eq(bool *a, bool *b) {
  bool *boxed = (bool*) GC_malloc(sizeof(bool));
  *boxed = *a == *b;
  return boxed;
}

#ifdef __cplusplus
}
#endif
