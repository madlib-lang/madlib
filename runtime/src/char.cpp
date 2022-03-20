#include <gc.h>
#include "char.hpp"

#ifdef __cplusplus
extern "C" {
#endif

bool *madlib__char__internal__eq(int32_t *a, int32_t *b) {
  bool *boxed = (bool *)GC_malloc(sizeof(bool));
  *boxed = *a == *b;
  return boxed;
}

char **madlib__char__internal__inspect(int32_t *unicode) {
  char **boxed = (char **)GC_malloc(sizeof(char*));

  if (*unicode <= 0x7f) {
    // ASCII character
    char *str = (char *)GC_malloc(sizeof(char) * 2);
    str[0] = (char)*unicode;
    str[1] = '\0';
    *boxed = str;
  }
  else if (*unicode <= 0x07ff) {
    // 2-byte unicode
    char *str = (char *)GC_malloc(sizeof(char) * 3);
    str[0] = (char) (((*unicode >> 6) & 0x1f) | 0xc0);
    str[1] = (char) (((*unicode >> 0) & 0x3f) | 0x80);
    str[2] = '\0';
    *boxed = str;
  }
  else if (*unicode <= 0xffff) {
    // 3-byte unicode
    char *str = (char *)GC_malloc(sizeof(char) * 4);
    str[0] = (char) (((*unicode >> 12) & 0x0f) | 0xe0);
    str[1] = (char) (((*unicode >>  6) & 0x3f) | 0x80);
    str[2] = (char) (((*unicode >>  0) & 0x3f) | 0x80);
    str[3] = 0;
    *boxed = str;
  }
  else if (*unicode <= 0x10ffff) {
    // 4-byte unicode
    char *str = (char *)GC_malloc(sizeof(char) * 5);
    str[0] = (char) (((*unicode >> 18) & 0x07) | 0xf0);
    str[1] = (char) (((*unicode >> 12) & 0x3f) | 0x80);
    str[2] = (char) (((*unicode >>  6) & 0x3f) | 0x80);
    str[3] = (char) (((*unicode >>  0) & 0x3f) | 0x80);
    str[4] = '\0';
    *boxed = str;
  }
  else { 
    // or use replacement character
    char *str = (char *)GC_malloc(sizeof(char) * 4);
    str[0] = (char) 0xef;  
    str[1] = (char) 0xbf;
    str[2] = (char) 0xbf;
    str[3] = '\0';
    *boxed = str;
  }

  return boxed;
}

int64_t madlib__char__compare(int32_t c1, int32_t c2) {
  if (c1 > c2) {
    return 1;
  } else if (c1 < c2) {
    return -1;
  } else {
    return 0;
  }
}

#ifdef __cplusplus
}
#endif
