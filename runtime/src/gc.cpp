
#include <gc.h>


#ifdef __cplusplus
extern "C" {
#endif

void __setFreeSpaceDivisor__() {
  GC_set_free_space_divisor(1);
}

#ifdef __cplusplus
}
#endif
