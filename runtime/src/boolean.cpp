#include <gc.h>


#ifdef __cplusplus
extern "C" {
#endif

bool *__eqBoolean__(bool *a, bool *b) {
  bool *boxed = (bool*) GC_malloc(sizeof(bool));
  *boxed = *a == *b;
  return boxed;
}

#ifdef __cplusplus
}
#endif
