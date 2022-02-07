#include <stdio.h>

#include "apply-pap.hpp"


#ifdef __cplusplus
extern "C" {
#endif

void *madlib__control__loop(void *a, void *predicate, void *fn) {
  int i = 0;
  void *value = a;
  bool keepOn = *(bool*)__applyPAP__(predicate, 1, value);
  while (keepOn) {
    value = __applyPAP__(fn, 1, value);
    keepOn = *(bool*)__applyPAP__(predicate, 1, value);
    i++;
  }

  return value;
}


#ifdef __cplusplus
}
#endif
