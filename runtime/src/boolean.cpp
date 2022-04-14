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

char *madlib__boolean__internal__inspectBoolean(bool b) {
  if (b) {
    return (char *)"true";
  } else {
    return (char *)"false";
  }
}

bool madlib__boolean__internal__eq(bool a, bool b) {
  return a == b;
}

#ifdef __cplusplus
}
#endif
