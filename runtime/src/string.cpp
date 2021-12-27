#include <gc.h>
#include <iostream>
#include <string.h>

// String

#ifdef __cplusplus
extern "C" {
#endif

bool *__eqString__(char **s1, char **s2) {
  bool *boxed = (bool*) GC_malloc(sizeof(bool));
  if (strcmp(*s1, *s2) == 0) {
    *boxed = true;
  } else {
    *boxed = false;
  }

  return boxed;
}

bool __areStringsEqual__(char *s1, char *s2) {
  if (strcmp(s1, s2) == 0) {
    return true;
  } else {
    return false;
  }
}

bool __areStringsNotEqual__(char *s1, char *s2) {
  if (strcmp(s1, s2) == 0) {
    return false;
  } else {
    return true;
  }
}


// currently unused, types need adjustment. The param probably needs to be a char**
int64_t *__strLength__(char *s) {
  int64_t *result = (int64_t *)GC_malloc(sizeof(int64_t));
  *result = strlen(s);
  return result;
}

char *__strConcat__(char *s1, char *s2) {
  char *result = (char *)GC_malloc((strlen(s1) + strlen(s2) + 1) * sizeof(char));
  strcpy(result, s1);
  strcat(result, s2);
  return result;
}

char *__stripTrailingZeros__(char *number) {
  int length = strlen(number);
  char *end = number + strlen(number) - 1;
  int charsToRemove = 0;

  while (*end == '0' && charsToRemove < length) {
    charsToRemove += 1;
    end -= 1;
  }

  if (*end == '.') {
    charsToRemove += 1;
  }

  char *result = (char *)GC_malloc(length - charsToRemove + 1);
  memcpy(result, number, length - charsToRemove);
  result[length - charsToRemove] = '\0';

  return result;
}

#ifdef __cplusplus
}
#endif
