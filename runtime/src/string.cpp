#include <gc.h>
#include <iostream>
#include <string.h>

// String

#ifdef __cplusplus
extern "C" {
#endif

bool *madlib__string__internal__eq(char **s1, char **s2) {
  bool *boxed = (bool*) GC_malloc(sizeof(bool));
  if (strcmp(*s1, *s2) == 0) {
    *boxed = true;
  } else {
    *boxed = false;
  }

  return boxed;
}


int64_t madlib__string__compare(char *s1, char *s2) {
  int64_t result = strcmp(s1, s2);
  if (result < 0) {
    return -1;
  } else if (result > 0) {
    return 1;
  } else {
    return 0;
  }
}


bool madlib__string__internal__areStringsEqual(char *s1, char *s2) {
  if (strcmp(s1, s2) == 0) {
    return true;
  } else {
    return false;
  }
}


bool madlib__string__internal__areStringsNotEqual(char *s1, char *s2) {
  if (strcmp(s1, s2) == 0) {
    return false;
  } else {
    return true;
  }
}


// currently unused, types need adjustment. The param probably needs to be a char**
int64_t madlib__string__length(char *s) {
  return strlen(s);
}

char *madlib__string__internal__concat(char *s1, char *s2) {
  char *result = (char *)GC_malloc((strlen(s1) + strlen(s2) + 1) * sizeof(char));
  strcpy(result, s1);
  strcat(result, s2);
  return result;
}

char *stripTrailingZeros(char *number) {
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
