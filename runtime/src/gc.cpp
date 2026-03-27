
#include <gc.h>
#include "list.hpp"


#ifdef __cplusplus
extern "C" {
#endif

void __setFreeSpaceDivisor__() {
  GC_set_free_space_divisor(1);
}

void *madlib__gc__reserveList(int64_t count) {
  // Avoid paying reserve cost for small lists.
  if (count < 1000000) {
    return NULL;
  }

  size_t listBytes = (size_t)count * sizeof(madlib__list__Node_t);
  size_t reserveBytes = listBytes * 2;

  GC_expand_hp(reserveBytes);
  return NULL;
}

void *madlib__gc__reserveListFromList(madlib__list__Node_t *list) {
  int64_t count = madlib__list__length(list);
  if (count < 1000000) {
    return NULL;
  }

  size_t listBytes = (size_t)count * sizeof(madlib__list__Node_t);
  GC_expand_hp(listBytes);
  return NULL;
}

#ifdef __cplusplus
}
#endif
