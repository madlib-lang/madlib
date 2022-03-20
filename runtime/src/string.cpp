#include <gc.h>
#include <iostream>
#include <string.h>
#include "string.hpp"
#include "char.hpp"

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


int64_t madlib__string__length(unsigned char *s) {
  int64_t length = 0;
  int skipCount = 0;

  while (*s != '\0' || skipCount != 0) {
    if (skipCount > 0) {
      skipCount--;
    } else {
      length++;

      if (*s >= 0xf0) {
        skipCount = 3;
      } else if (*s >= 0xe0) {
        skipCount = 2;
      } else if (*s >= 0xc0) {
        skipCount = 1;
      }
    }
    s++;
  }

  return length;
}


char *madlib__string__slice(int64_t start, int64_t end, unsigned char *s) {
  int skipCount = 0;

  if (start < 0 || end <= 0) {
    int64_t length = madlib__string__length(s);

    if (start < 0) {
      start = start + length;
    }
    if (end == 0) {
      end = length - 1;
    }
    if (end < 0) {
      end = end + length;
    }
  }

  int charsToTake = end - start + 1;

  while ((*s != '\0' || skipCount != 0) && start > 0) {
    if (skipCount > 0) {
      skipCount--;
    } else {
      start--;

      if (*s >= 0xf0) {
        skipCount = 3;
      } else if (*s >= 0xe0) {
        skipCount = 2;
      } else if (*s >= 0xc0) {
        skipCount = 1;
      }
    }
    s++;
  }

  unsigned char *startPtr = s;

  // the input string was shorter than the start so we return an empty string
  if (start > 0) {
    char *empty = (char*)GC_malloc(sizeof(char));
    *empty = '\0';
    return empty;
  }

  int bytesToCopy = 0;
  skipCount = 0;
  while ((*s != '\0' || skipCount != 0) && charsToTake > 0) {
    bytesToCopy++;
    if (skipCount > 0) {
      skipCount--;
    } else {
      charsToTake--;

      if (*s >= 0xf0) {
        skipCount = 3;
      } else if (*s >= 0xe0) {
        skipCount = 2;
      } else if (*s >= 0xc0) {
        skipCount = 1;
      }
    }
    s++;
  }

  char *result = (char*)GC_malloc(sizeof(char) * (bytesToCopy + 1));
  memcpy(result, startPtr, bytesToCopy);
  result[bytesToCopy] = '\0';

  return result;
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

char *madlib__string__mapChars(PAP_t *pap, char *str) {
  int32_t *chars = utf8Decode(str);

  int i = 0;
  while(chars[i] != 0) {
    chars[i] = *((int32_t*)__applyPAP__(pap, 1, &chars[i]));
    i++;
  }

  char **encodedChars = (char**)GC_malloc(sizeof(char*) * i);
  int j = 0;
  size_t fullLength = 0;
  for (int j = 0; j <= i; j++) {
    char *encoded = utf8Encode(chars[j]);
    fullLength += strlen(encoded);
    encodedChars[j] = encoded;
  }

  char *result = (char *)GC_malloc(sizeof(char) * (fullLength + 1));
  j = 0;
  size_t offset = 0;
  for (int j = 0; j <= i; j++) {
    size_t sizeOfChar = strlen(encodedChars[j]);
    memcpy(result + offset, encodedChars[j], sizeOfChar);
    offset += sizeOfChar;
  }

  result[offset] = '\0';

  return result;
}


madlib__list__Node_t *madlib__string__toList(char *str) {
  madlib__list__Node_t *result = madlib__list__empty();

  int32_t *chars = utf8Decode(str);

  int length = 0;
  while (chars[length] != 0) {
    length++;
  }

  for (int i = length - 1; i >= 0; i--) {
    result = madlib__list__push(&chars[i], result);
  }

  return result;
}

char *madlib__string__fromList(madlib__list__Node_t *list) {
  int64_t charCount = madlib__list__length(list);
  char **encodedChars = (char**)GC_malloc(sizeof(char*) * charCount);
  int j = 0;
  size_t fullLength = 0;

  while (list->value != NULL) {
    char *encoded = utf8Encode(*(int32_t*)list->value);
    fullLength += strlen(encoded);
    encodedChars[j] = encoded;
    j++;
    list = list->next;
  }

  char *result = (char *)GC_malloc(sizeof(char) * (fullLength + 1));
  size_t offset = 0;
  for (int i = 0; i < charCount; i++) {
    size_t sizeOfChar = strlen(encodedChars[i]);
    memcpy(result + offset, encodedChars[i], sizeOfChar);
    offset += sizeOfChar;
  }

  result[offset] = '\0';

  return result;
}


madlib__list__Node_t *madlib__string__split(char *separator, char *str) {
  size_t strLength = strlen(str);
  size_t separatorLength = strlen(separator);
  char *copy = (char*)GC_malloc(sizeof(char) * (strLength + 1));
  memcpy(copy, str, strLength + 1);

  madlib__list__Node_t *result = (madlib__list__Node_t*)GC_malloc(sizeof(madlib__list__Node_t));
  madlib__list__Node_t *current = result;

  while (copy != NULL) {
    char *found = strstr(copy, separator);
    size_t partLength = 0;
    if (found == NULL) {
      partLength = strlen(copy);
    } else {
      partLength = found - copy;
    }

    char *part = (char*)GC_malloc(sizeof(char) * (partLength + 1));
    memcpy(part, copy, partLength);
    part[partLength] = '\0';
    char **boxed = (char**)GC_malloc(sizeof(char*));
    *boxed = part;

    madlib__list__Node_t *node = (madlib__list__Node_t*)GC_malloc(sizeof(madlib__list__Node_t));
    node->value = boxed;
    node->next = NULL;
    current = current->next = node;

    copy = found;
    if (found != NULL) {
      copy = found + separatorLength;
    }
  }

  madlib__list__Node_t *last = (madlib__list__Node_t*)GC_malloc(sizeof(madlib__list__Node_t));
  last->value = NULL;
  last->next = NULL;

  current->next = last;

  return result->next;
}


#ifdef __cplusplus
}
#endif
