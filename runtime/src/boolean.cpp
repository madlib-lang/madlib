#include "boolean.hpp"

#ifdef __cplusplus
extern "C" {
#endif

char *madlib__boolean__internal__showBoolean(bool b) {
  if (b) {
    return (char *)"true";
  } else {
    return (char *)"false";
  }
}

char **madlib__boolean__internal__inspectBoolean(bool *b) {
  char *str = madlib__boolean__internal__showBoolean(*b);
  char **boxed = (char**)GC_malloc(sizeof(char*));
  *boxed = str;
  return boxed;
}

bool *madlib__boolean__internal__eq(bool *a, bool *b) {
  bool *boxed = (bool*) GC_malloc(sizeof(bool));
  *boxed = *a == *b;
  return boxed;
}

#ifdef __cplusplus
}
#endif
