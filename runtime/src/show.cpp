#include <gc.h>
#include <iostream>
#include <inttypes.h>

#include "string.hpp"

// Show

#ifdef __cplusplus
extern "C" {
#endif

char *__floatToStr__(double d) {
  char *str = (char *)GC_malloc(200);
  sprintf(str, "%.20f", d);
  char *stripped = stripTrailingZeros(str);

  return stripped;
}

char *__integerToStr__(int64_t i) {
  char *str = (char *)GC_malloc(200);
  sprintf(str, "%" PRId64, i);

  return str;
}

char *__byteToStr__(unsigned char i) {
  char *str = (char *)GC_malloc(4 * sizeof(char));
  snprintf(str, 4 * sizeof(char), "%02X", i);

  return str;
}

char *__booleanToStr__(bool b) {
  if (b) {
    char *str = (char *)GC_malloc(5);
    str[0] = 't';
    str[1] = 'r';
    str[2] = 'u';
    str[3] = 'e';
    str[4] = '\0';
    return str;
  } else {
    char *str = (char *)GC_malloc(6);
    str[0] = 'f';
    str[1] = 'a';
    str[2] = 'l';
    str[3] = 's';
    str[4] = 'e';
    str[5] = '\0';
    return str;
  }
}

#ifdef __cplusplus
}
#endif
