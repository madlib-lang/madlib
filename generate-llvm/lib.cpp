#include <string.h>

#ifdef __cplusplus
extern "C"
{
#endif
  bool __streq__(char *s1, char *s2) {
    if (strcmp(s1, s2) == 0) {
      return true;
    }
    else {
      return false;
    }
  }
#ifdef __cplusplus
}
#endif
