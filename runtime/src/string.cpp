#include <gc.h>
#include "string.hpp"
#include <string.h>

#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

#include <iostream>
#include <regex>

#include "char.hpp"

// String

#ifdef __cplusplus
extern "C" {
#endif

char *madlib__string__replace(char *regex, char *replace, char *str) {
  int errornumber;
  size_t inputLength = strlen(str);
  PCRE2_SIZE erroroffset;
  pcre2_code *re = pcre2_compile(
    (PCRE2_SPTR)regex,
    PCRE2_ZERO_TERMINATED,
    PCRE2_UTF,
    &errornumber,
    &erroroffset,
    NULL
  );

  if (re == NULL) {
    return str;
  }

  size_t bufferLengthToUse = inputLength * 1.25;
  size_t bufferLength = bufferLengthToUse;
  char *buffer = (char*)GC_MALLOC_ATOMIC(sizeof(char)*bufferLength);

  int result = pcre2_substitute(re, (PCRE2_SPTR)str, PCRE2_ZERO_TERMINATED, 0, PCRE2_SUBSTITUTE_GLOBAL, NULL, NULL, (PCRE2_SPTR)replace, PCRE2_ZERO_TERMINATED, (PCRE2_UCHAR*) buffer, &bufferLength);

  while (result == PCRE2_ERROR_NOMEMORY) {
    bufferLengthToUse = bufferLength = bufferLengthToUse * 2;
    buffer = (char*)GC_MALLOC_ATOMIC(sizeof(char)*bufferLength);
    result = pcre2_substitute(re, (PCRE2_SPTR)str, PCRE2_ZERO_TERMINATED, 0, PCRE2_SUBSTITUTE_GLOBAL, NULL, NULL, (PCRE2_SPTR)replace, PCRE2_ZERO_TERMINATED, (PCRE2_UCHAR*) buffer, &bufferLength);
  }

  return buffer;
}


bool madlib__string__match(char *regex, char *str) {
  int errornumber;
  size_t inputLength = strlen(str);
  PCRE2_SIZE erroroffset;
  pcre2_code *re = pcre2_compile(
    (PCRE2_SPTR)regex,
    PCRE2_ZERO_TERMINATED,
    PCRE2_UTF,
    &errornumber,
    &erroroffset,
    NULL
  );

  if (re == NULL) {
    return false;
  }

  pcre2_match_data *match_data = pcre2_match_data_create_from_pattern(re, NULL);

  int rc = pcre2_match(
    re,
    (PCRE2_SPTR)str,
    inputLength,
    0,
    0,
    match_data,
    NULL
  );

  return rc >= 0;
}


bool madlib__string__internal__eq(char *s1, char *s2) {
  if (strcmp(s1, s2) == 0) {
    return true;
  } else {
    return false;
  }
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

int makeNextSize(int oldSize) {
  int newSize = oldSize * 0.1;
  if (newSize - oldSize < 10) {
    return oldSize + 10;
  }

  return newSize;
}

char *madlib__string__internal__inspect(char *input) {
  int initialLength = strlen(input);
  int currentLength = initialLength + (initialLength + 3) * 0.1;
  char *result = (char *)GC_MALLOC_ATOMIC(sizeof(char) * (currentLength + 1));
  int currentIndex = 1;

  result[0] = '"';

  while (*input != '\0') {
    // if the size of the string isn't enough we resize it
    if (currentLength - currentIndex < 4) {
      char *resized = (char *)GC_MALLOC_ATOMIC(sizeof(char) * makeNextSize(currentLength));
      memcpy(resized, result, sizeof(char) * (currentIndex + 1));
      // GC_free(result);
      result = resized;
    }

    if (*input == '\n') {
      result[currentIndex] = '\\';
      result[currentIndex + 1] = 'n';

      currentIndex += 2;
    } else if (*input == '\t') {
      result[currentIndex] = '\\';
      result[currentIndex + 1] = 't';

      currentIndex += 2;
    } else if (*input == '\r') {
      result[currentIndex] = '\\';
      result[currentIndex + 1] = 'r';

      currentIndex += 2;
    } else {
      result[currentIndex] = *input;
      currentIndex++;
    }

    input++;
  }

  result[currentIndex] = '"';
  result[currentIndex + 1] = '\0';

  return result;
}


madlib__maybe__Maybe_t *madlib__string__charAt(int64_t n, unsigned char *s) {
  int64_t length = 0;
  int skipCount = 0;
  madlib__maybe__Maybe_t *result = (madlib__maybe__Maybe_t*)GC_MALLOC(sizeof(madlib__maybe__Maybe_t));

  while (*s != '\0' && (skipCount != 0 || length < n)) {
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

  if (*s) {
    int _;
    int32_t c = utf8DecodeChar((char*)s, &_);
    int32_t *boxed = (int32_t*)c;

    result->index = 0;
    result->data = boxed;
  } else {
    result->index = 1;
    result->data = NULL;
  }

  return result;
}


char *madlib__string__slice(int64_t start, int64_t end, unsigned char *s) {
  int skipCount = 0;
  int initialEnd = end;

  if (start < 0 || end < 0) {
    int64_t length = madlib__string__length(s);

    if (start < 0) {
      start = start + length;
    }
    if (end == 0) {
      end = length;
    }
    if (end < 0) {
      end = end + length;
    }
  }

  int charsToTake = end - start;

  while ((*s != '\0') && (skipCount != 0 || start > 0)) {
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

  if (initialEnd == 0) {
    return (char*)s;
  }

  unsigned char *startPtr = s;

  // the input string was shorter than the start so we return an empty string
  if (start > 0) {
    char *empty = (char *)GC_MALLOC_ATOMIC(sizeof(char));
    *empty = '\0';
    return empty;
  }

  int bytesToCopy = 0;
  skipCount = 0;
  while (*s != '\0' && (skipCount != 0 || charsToTake > 0)) {
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

  char *result = (char *)GC_MALLOC_ATOMIC(sizeof(char) * (bytesToCopy + 1));
  memcpy(result, startPtr, bytesToCopy);
  result[bytesToCopy] = '\0';

  return result;
}

char *madlib__string__pushChar(int32_t c, char* s) {
  char *encoded = utf8EncodeChar(c);
  size_t encodedLength = strlen(encoded);
  size_t stringLength = strlen(s);
  char *result = (char*)GC_MALLOC_ATOMIC(sizeof(char) * (stringLength + encodedLength + 1));
  memcpy(result, encoded, encodedLength);
  memcpy(result + encodedLength, s, stringLength);
  result[stringLength + encodedLength] = '\0';
  return result;
}

char *madlib__string__appendChar(int32_t c, char* s) {
  char *encoded = utf8EncodeChar(c);
  size_t encodedLength = strlen(encoded);
  size_t stringLength = strlen(s);
  char *result = (char*)GC_MALLOC_ATOMIC(sizeof(char) * (stringLength + encodedLength + 1));
  memcpy(result, s, stringLength);
  memcpy(result + stringLength, encoded, encodedLength);
  result[stringLength + encodedLength] = '\0';
  return result;
}

char *madlib__string__trim(char *s) {
  char *startPtr = s;
  size_t strLength = strlen(s);
  int removeFromStart = 0;
  int removeFromEnd = 0;

  while (*s != '\0') {
    if (*s == '\n' || *s == '\r' || *s == ' ' || *s == '\t') {
      removeFromStart++;
    } else {
      break;
    }

    s++;
  }

  char *end = s + strlen(s) - 1;

  while (end >= s) {
    if (*end == '\n' || *end == '\r' || *end == ' ' || *end == '\t') {
      removeFromEnd++;
    } else {
      break;
    }
    end--;
  }

  size_t newSize = strLength - removeFromStart - removeFromEnd;
  char *result = (char *)GC_MALLOC_ATOMIC(sizeof(char) * (newSize + 1));
  memcpy(result, startPtr + removeFromStart, newSize);
  result[newSize] = '\0';

  return result;
}

char *madlib__string__trimStart(char *s) {
  char *startPtr = s;
  size_t strLength = strlen(s);
  int removeFromStart = 0;

  while (*s != '\0') {
    if (*s == '\n' || *s == '\r' || *s == ' ' || *s == '\t') {
      removeFromStart++;
    } else {
      break;
    }

    s++;
  }

  size_t newSize = strLength - removeFromStart;
  char *result = (char *)GC_MALLOC_ATOMIC(sizeof(char) * (newSize + 1));
  memcpy(result, startPtr + removeFromStart, newSize);
  result[newSize] = '\0';

  return result;
}

char *madlib__string__trimEnd(char *s) {
  size_t strLength = strlen(s);
  int removeFromEnd = 0;
  char *end = s + strLength - 1;

  while (end >= s) {
    if (*end == '\n' || *end == '\r' || *end == ' ' || *end == '\t') {
      removeFromEnd++;
    } else {
      break;
    }
    end--;
  }

  size_t newSize = strLength - removeFromEnd;
  char *result = (char *)GC_MALLOC_ATOMIC(sizeof(char) * (newSize + 1));
  memcpy(result, s, newSize);
  result[newSize] = '\0';

  return result;
}

char *madlib__string__internal__concat(char *s1, char *s2) {
  char *result = (char *)GC_MALLOC_ATOMIC((strlen(s1) + strlen(s2) + 1) * sizeof(char));
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

  char *result = (char *)GC_MALLOC_ATOMIC(length - charsToRemove + 1);
  memcpy(result, number, length - charsToRemove);
  result[length - charsToRemove] = '\0';

  return result;
}

char *madlib__string__mapChars(PAP_t *pap, char *str) {
  int32_t *chars = utf8Decode(str);

  int i = 0;
  while (chars[i] != 0) {
    chars[i] = (int32_t)(int64_t)__applyPAP__(pap, 1, (void*)chars[i]);
    i++;
  }

  // TODO: Seems to be bad here
  char **encodedChars = (char **)GC_MALLOC(sizeof(char *) * i);
  int j = 0;
  size_t fullLength = 0;
  for (int j = 0; j < i; j++) {
    char *encoded = utf8EncodeChar(chars[j]);
    fullLength += strlen(encoded);
    encodedChars[j] = encoded;
  }

  char *result = (char *)GC_MALLOC_ATOMIC(sizeof(char) * (fullLength + 1));
  j = 0;
  size_t offset = 0;
  for (int j = 0; j < i; j++) {
    size_t sizeOfChar = strlen(encodedChars[j]);
    memcpy(&result[offset], encodedChars[j], sizeOfChar);
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
    result = madlib__list__push((void*)chars[i], result);
  }

  return result;
}

char *madlib__string__fromList(madlib__list__Node_t *list) {
  int64_t charCount = madlib__list__length(list);
  char **encodedChars = (char **)GC_MALLOC(sizeof(char *) * charCount);
  int j = 0;
  size_t fullLength = 0;

  while (list->value != NULL) {
    char *encoded = utf8EncodeChar((int32_t)(int64_t)list->value);
    fullLength += strlen(encoded);
    encodedChars[j] = encoded;
    j++;
    list = list->next;
  }

  char *result = (char *)GC_MALLOC_ATOMIC(sizeof(char) * (fullLength + 1));
  size_t offset = 0;
  for (int i = 0; i < charCount; i++) {
    if (encodedChars[i] != NULL) {
      size_t sizeOfChar = strlen(encodedChars[i]);
      memcpy(result + offset, encodedChars[i], sizeOfChar);
      offset += sizeOfChar;
    }
  }

  result[offset] = '\0';

  return result;
}

madlib__list__Node_t *madlib__string__split(char *separator, char *str) {
  size_t separatorLength = strlen(separator);
  if (separatorLength == 0) {
    separatorLength = 1;
  }
  
  madlib__list__Node_t *result = (madlib__list__Node_t *)GC_MALLOC(sizeof(madlib__list__Node_t));
  madlib__list__Node_t *current = result;

  while (str != NULL && *str != '\0') {
    char *found = strstr(str, separator);
    size_t partLength = 0;
    if (found == NULL) {
      partLength = strlen(str);
    } else {
      partLength = found - str;
    }

    if (partLength == 0) {
      partLength = 1;
    }

    char *part = (char *)GC_MALLOC_ATOMIC(sizeof(char) * (partLength + 1));
    memcpy(part, str, partLength);
    part[partLength] = '\0';

    madlib__list__Node_t *node = (madlib__list__Node_t *)GC_MALLOC(sizeof(madlib__list__Node_t));
    node->value = part;
    node->next = NULL;
    current = current->next = node;

    str = found;
    if (found != NULL) {
      str = found + separatorLength;
    }
  }

  madlib__list__Node_t *last = (madlib__list__Node_t *)GC_MALLOC(sizeof(madlib__list__Node_t));
  last->value = NULL;
  last->next = NULL;

  current->next = last;

  return result->next;
}

unsigned char *__strToUpper__(unsigned char *s);
unsigned char *madlib__string__toUpper(unsigned char *str) { return __strToUpper__(str); }

unsigned char *__strToLower__(unsigned char *s);
unsigned char *madlib__string__toLower(unsigned char *str) { return __strToLower__(str); }

#ifdef __cplusplus
}
#endif


unsigned char *__strToLower__(unsigned char *s) {
  size_t length = strlen((char*)s);
  unsigned char *copy = (unsigned char*)GC_MALLOC_ATOMIC(sizeof(unsigned char) * (length + 1));
  memcpy(copy, s, length + 1);
  unsigned char *p = copy;
  unsigned char *pExtChar = 0;

  if (copy && *copy) {
    while (*p) {
      if ((*p >= 0x41) && (*p <= 0x5a)) /* US ASCII */
        (*p) += 0x20;
      else if (*p > 0xc0) {
        pExtChar = p;
        p++;
        switch (*pExtChar) {
          case 0xc3:                                                        /* Latin 1 */
            if ((*p >= 0x80) && (*p <= 0x9e) && (*p != 0x97)) (*p) += 0x20; /* US ASCII shift */
            break;
          case 0xc4:                                                           /* Latin ext */
            if (((*p >= 0x80) && (*p <= 0xb7) && (*p != 0xb0)) && (!(*p % 2))) /* Even */
              (*p)++;                                                          /* Next char is lwr */
            else if ((*p >= 0xb9) && (*p <= 0xbe) && (*p % 2))                 /* Odd */
              (*p)++;                                                          /* Next char is lwr */
            else if (*p == 0xbf) {
              *pExtChar = 0xc5;
              (*p) = 0x80;
            }
            break;
          case 0xc5:                                              /* Latin ext */
            if ((*p >= 0x81) && (*p <= 0x88) && (*p % 2))         /* Odd */
              (*p)++;                                             /* Next char is lwr */
            else if ((*p >= 0x8a) && (*p <= 0xb7) && (!(*p % 2))) /* Even */
              (*p)++;                                             /* Next char is lwr */
            else if (*p == 0xb8) {
              *pExtChar = 0xc3;
              (*p) = 0xbf;
            } else if ((*p >= 0xb9) && (*p <= 0xbe) && (*p % 2)) /* Odd */
              (*p)++;                                            /* Next char is lwr */
            break;
          case 0xc6: /* Latin ext */
            switch (*p) {
              case 0x81:
                *pExtChar = 0xc9;
                (*p) = 0x93;
                break;
              case 0x86:
                *pExtChar = 0xc9;
                (*p) = 0x94;
                break;
              case 0x89:
                *pExtChar = 0xc9;
                (*p) = 0x96;
                break;
              case 0x8a:
                *pExtChar = 0xc9;
                (*p) = 0x97;
                break;
              case 0x8e:
                *pExtChar = 0xc9;
                (*p) = 0x98;
                break;
              case 0x8f:
                *pExtChar = 0xc9;
                (*p) = 0x99;
                break;
              case 0x90:
                *pExtChar = 0xc9;
                (*p) = 0x9b;
                break;
              case 0x93:
                *pExtChar = 0xc9;
                (*p) = 0xa0;
                break;
              case 0x94:
                *pExtChar = 0xc9;
                (*p) = 0xa3;
                break;
              case 0x96:
                *pExtChar = 0xc9;
                (*p) = 0xa9;
                break;
              case 0x97:
                *pExtChar = 0xc9;
                (*p) = 0xa8;
                break;
              case 0x9c:
                *pExtChar = 0xc9;
                (*p) = 0xaf;
                break;
              case 0x9d:
                *pExtChar = 0xc9;
                (*p) = 0xb2;
                break;
              case 0x9f:
                *pExtChar = 0xc9;
                (*p) = 0xb5;
                break;
              case 0xa9:
                *pExtChar = 0xca;
                (*p) = 0x83;
                break;
              case 0xae:
                *pExtChar = 0xca;
                (*p) = 0x88;
                break;
              case 0xb1:
                *pExtChar = 0xca;
                (*p) = 0x8a;
                break;
              case 0xb2:
                *pExtChar = 0xca;
                (*p) = 0x8b;
                break;
              case 0xb7:
                *pExtChar = 0xca;
                (*p) = 0x92;
                break;
              case 0x82:
              case 0x84:
              case 0x87:
              case 0x8b:
              case 0x91:
              case 0x98:
              case 0xa0:
              case 0xa2:
              case 0xa4:
              case 0xa7:
              case 0xac:
              case 0xaf:
              case 0xb3:
              case 0xb5:
              case 0xb8:
              case 0xbc:
                (*p)++; /* Next char is lwr */
                break;
              default:
                break;
            }
            break;
          case 0xc7: /* Latin ext */
            if (*p == 0x84)
              (*p) = 0x86;
            else if (*p == 0x85)
              (*p)++; /* Next char is lwr */
            else if (*p == 0x87)
              (*p) = 0x89;
            else if (*p == 0x88)
              (*p)++; /* Next char is lwr */
            else if (*p == 0x8a)
              (*p) = 0x8c;
            else if (*p == 0x8b)
              (*p)++;                                             /* Next char is lwr */
            else if ((*p >= 0x8d) && (*p <= 0x9c) && (*p % 2))    /* Odd */
              (*p)++;                                             /* Next char is lwr */
            else if ((*p >= 0x9e) && (*p <= 0xaf) && (!(*p % 2))) /* Even */
              (*p)++;                                             /* Next char is lwr */
            else if (*p == 0xb1)
              (*p) = 0xb3;
            else if (*p == 0xb2)
              (*p)++; /* Next char is lwr */
            else if (*p == 0xb4)
              (*p)++; /* Next char is lwr */
            else if (*p == 0xb6) {
              *pExtChar = 0xc6;
              (*p) = 0x95;
            } else if (*p == 0xb7) {
              *pExtChar = 0xc6;
              (*p) = 0xbf;
            } else if ((*p >= 0xb8) && (*p <= 0xbf) && (!(*p % 2))) /* Even */
              (*p)++;                                               /* Next char is lwr */
            break;
          case 0xc8:                                         /* Latin ext */
            if ((*p >= 0x80) && (*p <= 0x9f) && (!(*p % 2))) /* Even */
              (*p)++;                                        /* Next char is lwr */
            else if (*p == 0xa0) {
              *pExtChar = 0xc6;
              (*p) = 0x9e;
            } else if ((*p >= 0xa2) && (*p <= 0xb3) && (!(*p % 2))) /* Even */
              (*p)++;                                               /* Next char is lwr */
            else if (*p == 0xbb)
              (*p)++; /* Next char is lwr */
            else if (*p == 0xbd) {
              *pExtChar = 0xc6;
              (*p) = 0x9a;
            }
            /* 0xba three byte small 0xe2 0xb1 0xa5 */
            /* 0xbe three byte small 0xe2 0xb1 0xa6 */
            break;
          case 0xc9: /* Latin ext */
            if (*p == 0x81)
              (*p)++; /* Next char is lwr */
            else if (*p == 0x83) {
              *pExtChar = 0xc6;
              (*p) = 0x80;
            } else if (*p == 0x84) {
              *pExtChar = 0xca;
              (*p) = 0x89;
            } else if (*p == 0x85) {
              *pExtChar = 0xca;
              (*p) = 0x8c;
            } else if ((*p >= 0x86) && (*p <= 0x8f) && (!(*p % 2))) /* Even */
              (*p)++;                                               /* Next char is lwr */
            break;
          case 0xcd: /* Greek & Coptic */
            switch (*p) {
              case 0xb0:
              case 0xb2:
              case 0xb6:
                (*p)++; /* Next char is lwr */
                break;
              case 0xbf:
                *pExtChar = 0xcf;
                (*p) = 0xb3;
                break;
              default:
                break;
            }
            break;
          case 0xce: /* Greek & Coptic */
            if (*p == 0x86)
              (*p) = 0xac;
            else if (*p == 0x88)
              (*p) = 0xad;
            else if (*p == 0x89)
              (*p) = 0xae;
            else if (*p == 0x8a)
              (*p) = 0xaf;
            else if (*p == 0x8c) {
              *pExtChar = 0xcf;
              (*p) = 0x8c;
            } else if (*p == 0x8e) {
              *pExtChar = 0xcf;
              (*p) = 0x8d;
            } else if (*p == 0x8f) {
              *pExtChar = 0xcf;
              (*p) = 0x8e;
            } else if ((*p >= 0x91) && (*p <= 0x9f))
              (*p) += 0x20; /* US ASCII shift */
            else if ((*p >= 0xa0) && (*p <= 0xab) && (*p != 0xa2)) {
              *pExtChar = 0xcf;
              (*p) -= 0x20;
            }
            break;
          case 0xcf: /* Greek & Coptic */
            if (*p == 0x8f)
              (*p) = 0x97;
            else if ((*p >= 0x98) && (*p <= 0xaf) && (!(*p % 2))) /* Even */
              (*p)++;                                             /* Next char is lwr */
            else if (*p == 0xb4) {
              (*p) = 0x91;
            } else if (*p == 0xb7)
              (*p)++; /* Next char is lwr */
            else if (*p == 0xb9)
              (*p) = 0xb2;
            else if (*p == 0xba)
              (*p)++; /* Next char is lwr */
            else if (*p == 0xbd) {
              *pExtChar = 0xcd;
              (*p) = 0xbb;
            } else if (*p == 0xbe) {
              *pExtChar = 0xcd;
              (*p) = 0xbc;
            } else if (*p == 0xbf) {
              *pExtChar = 0xcd;
              (*p) = 0xbd;
            }
            break;
          case 0xd0: /* Cyrillic */
            if ((*p >= 0x80) && (*p <= 0x8f)) {
              *pExtChar = 0xd1;
              (*p) += 0x10;
            } else if ((*p >= 0x90) && (*p <= 0x9f))
              (*p) += 0x20; /* US ASCII shift */
            else if ((*p >= 0xa0) && (*p <= 0xaf)) {
              *pExtChar = 0xd1;
              (*p) -= 0x20;
            }
            break;
          case 0xd1:                                         /* Cyrillic supplement */
            if ((*p >= 0xa0) && (*p <= 0xbf) && (!(*p % 2))) /* Even */
              (*p)++;                                        /* Next char is lwr */
            break;
          case 0xd2: /* Cyrillic supplement */
            if (*p == 0x80)
              (*p)++;                                             /* Next char is lwr */
            else if ((*p >= 0x8a) && (*p <= 0xbf) && (!(*p % 2))) /* Even */
              (*p)++;                                             /* Next char is lwr */
            break;
          case 0xd3: /* Cyrillic supplement */
            if (*p == 0x80)
              (*p) = 0x8f;
            else if ((*p >= 0x81) && (*p <= 0x8e) && (*p % 2))    /* Odd */
              (*p)++;                                             /* Next char is lwr */
            else if ((*p >= 0x90) && (*p <= 0xbf) && (!(*p % 2))) /* Even */
              (*p)++;                                             /* Next char is lwr */
            break;
          case 0xd4:                                         /* Cyrillic supplement & Armenian */
            if ((*p >= 0x80) && (*p <= 0xaf) && (!(*p % 2))) /* Even */
              (*p)++;                                        /* Next char is lwr */
            else if ((*p >= 0xb1) && (*p <= 0xbf)) {
              *pExtChar = 0xd5;
              (*p) -= 0x10;
            }
            break;
          case 0xd5: /* Armenian */
            if ((*p >= 0x80) && (*p <= 0x8f)) {
              (*p) += 0x30;
            } else if ((*p >= 0x90) && (*p <= 0x96)) {
              *pExtChar = 0xd6;
              (*p) -= 0x10;
            }
            break;
          case 0xe1: /* Three byte code */
            pExtChar = p;
            p++;
            switch (*pExtChar) {
              case 0x82: /* Georgian asomtavruli */
                if ((*p >= 0xa0) && (*p <= 0xbf)) {
                  *pExtChar = 0x83;
                  (*p) -= 0x10;
                }
                break;
              case 0x83: /* Georgian asomtavruli */
                if (((*p >= 0x80) && (*p <= 0x85)) || (*p == 0x87) || (*p == 0x8d)) (*p) += 0x30;
                break;
              case 0x8e: /* Cherokee */
                if ((*p >= 0xa0) && (*p <= 0xaf)) {
                  *(p - 2) = 0xea;
                  *pExtChar = 0xad;
                  (*p) += 0x10;
                } else if ((*p >= 0xb0) && (*p <= 0xbf)) {
                  *(p - 2) = 0xea;
                  *pExtChar = 0xae;
                  (*p) -= 0x30;
                }
                break;
              case 0x8f: /* Cherokee */
                if ((*p >= 0x80) && (*p <= 0xaf)) {
                  *(p - 2) = 0xea;
                  *pExtChar = 0xae;
                  (*p) += 0x10;
                } else if ((*p >= 0xb0) && (*p <= 0xb5)) {
                  (*p) += 0x08;
                }
                /* 0xbe three byte small 0xe2 0xb1 0xa6 */
                break;
              case 0xb2: /* Georgian mtavruli */
                if (((*p >= 0x90) && (*p <= 0xba)) || (*p == 0xbd) || (*p == 0xbe) || (*p == 0xbf)) *pExtChar = 0x83;
                break;
              case 0xb8:                                         /* Latin ext */
                if ((*p >= 0x80) && (*p <= 0xbf) && (!(*p % 2))) /* Even */
                  (*p)++;                                        /* Next char is lwr */
                break;
              case 0xb9:                                         /* Latin ext */
                if ((*p >= 0x80) && (*p <= 0xbf) && (!(*p % 2))) /* Even */
                  (*p)++;                                        /* Next char is lwr */
                break;
              case 0xba:                                              /* Latin ext */
                if ((*p >= 0x80) && (*p <= 0x94) && (!(*p % 2)))      /* Even */
                  (*p)++;                                             /* Next char is lwr */
                else if ((*p >= 0xa0) && (*p <= 0xbf) && (!(*p % 2))) /* Even */
                  (*p)++;                                             /* Next char is lwr */
                /* 0x9e Two byte small 0xc3 0x9f */
                break;
              case 0xbb:                                         /* Latin ext */
                if ((*p >= 0x80) && (*p <= 0xbf) && (!(*p % 2))) /* Even */
                  (*p)++;                                        /* Next char is lwr */
                break;
              case 0xbc: /* Greek ex */
                if ((*p >= 0x88) && (*p <= 0x8f))
                  (*p) -= 0x08;
                else if ((*p >= 0x98) && (*p <= 0x9d))
                  (*p) -= 0x08;
                else if ((*p >= 0xa8) && (*p <= 0xaf))
                  (*p) -= 0x08;
                else if ((*p >= 0xb8) && (*p <= 0xbf))
                  (*p) -= 0x08;
                break;
              case 0xbd: /* Greek ex */
                if ((*p >= 0x88) && (*p <= 0x8d))
                  (*p) -= 0x08;
                else if ((*p == 0x99) || (*p == 0x9b) || (*p == 0x9d) || (*p == 0x9f))
                  (*p) -= 0x08;
                else if ((*p >= 0xa8) && (*p <= 0xaf))
                  (*p) -= 0x08;
                break;
              case 0xbe: /* Greek ex */
                if ((*p >= 0x88) && (*p <= 0x8f))
                  (*p) -= 0x08;
                else if ((*p >= 0x98) && (*p <= 0x9f))
                  (*p) -= 0x08;
                else if ((*p >= 0xa8) && (*p <= 0xaf))
                  (*p) -= 0x08;
                else if ((*p >= 0xb8) && (*p <= 0xb9))
                  (*p) -= 0x08;
                else if ((*p >= 0xba) && (*p <= 0xbb)) {
                  *(p - 1) = 0xbd;
                  (*p) -= 0x0a;
                } else if (*p == 0xbc)
                  (*p) -= 0x09;
                break;
              case 0xbf: /* Greek ex */
                if ((*p >= 0x88) && (*p <= 0x8b)) {
                  *(p - 1) = 0xbd;
                  (*p) += 0x2a;
                } else if (*p == 0x8c)
                  (*p) -= 0x09;
                else if ((*p >= 0x98) && (*p <= 0x99))
                  (*p) -= 0x08;
                else if ((*p >= 0x9a) && (*p <= 0x9b)) {
                  *(p - 1) = 0xbd;
                  (*p) += 0x1c;
                } else if ((*p >= 0xa8) && (*p <= 0xa9))
                  (*p) -= 0x08;
                else if ((*p >= 0xaa) && (*p <= 0xab)) {
                  *(p - 1) = 0xbd;
                  (*p) += 0x10;
                } else if (*p == 0xac)
                  (*p) -= 0x07;
                else if ((*p >= 0xb8) && (*p <= 0xb9)) {
                  *(p - 1) = 0xbd;
                } else if ((*p >= 0xba) && (*p <= 0xbb)) {
                  *(p - 1) = 0xbd;
                  (*p) += 0x02;
                } else if (*p == 0xbc)
                  (*p) -= 0x09;
                break;
              default:
                break;
            }
            break;
          case 0xe2: /* Three byte code */
            pExtChar = p;
            p++;
            switch (*pExtChar) {
              case 0xb0: /* Glagolitic */
                if ((*p >= 0x80) && (*p <= 0x8f)) {
                  (*p) += 0x30;
                } else if ((*p >= 0x90) && (*p <= 0xae)) {
                  *pExtChar = 0xb1;
                  (*p) -= 0x10;
                }
                break;
              case 0xb1: /* Latin ext */
                switch (*p) {
                  case 0xa0:
                  case 0xa7:
                  case 0xa9:
                  case 0xab:
                  case 0xb2:
                  case 0xb5:
                    (*p)++; /* Next char is lwr */
                    break;
                  case 0xa2: /* Two byte small 0xc9 0xab */
                  case 0xa4: /* Two byte small 0xc9 0xbd */
                  case 0xad: /* Two byte small 0xc9 0x91 */
                  case 0xae: /* Two byte small 0xc9 0xb1 */
                  case 0xaf: /* Two byte small 0xc9 0x90 */
                  case 0xb0: /* Two byte small 0xc9 0x92 */
                  case 0xbe: /* Two byte small 0xc8 0xbf */
                  case 0xbf: /* Two byte small 0xc9 0x80 */
                    break;
                  case 0xa3:
                    *(p - 2) = 0xe1;
                    *(p - 1) = 0xb5;
                    *(p) = 0xbd;
                    break;
                  default:
                    break;
                }
                break;
              case 0xb2:                                         /* Coptic */
                if ((*p >= 0x80) && (*p <= 0xbf) && (!(*p % 2))) /* Even */
                  (*p)++;                                        /* Next char is lwr */
                break;
              case 0xb3:                                          /* Coptic */
                if (((*p >= 0x80) && (*p <= 0xa3) && (!(*p % 2))) /* Even */
                    || (*p == 0xab) || (*p == 0xad) || (*p == 0xb2))
                  (*p)++; /* Next char is lwr */
                break;
              case 0xb4: /* Georgian nuskhuri */
                if (((*p >= 0x80) && (*p <= 0xa5)) || (*p == 0xa7) || (*p == 0xad)) {
                  *(p - 2) = 0xe1;
                  *(p - 1) = 0x83;
                  (*p) += 0x10;
                }
                break;
              default:
                break;
            }
            break;
          case 0xea: /* Three byte code */
            pExtChar = p;
            p++;
            switch (*pExtChar) {
              case 0x99:                                         /* Cyrillic */
                if ((*p >= 0x80) && (*p <= 0xad) && (!(*p % 2))) /* Even */
                  (*p)++;                                        /* Next char is lwr */
                break;
              case 0x9a:                                         /* Cyrillic */
                if ((*p >= 0x80) && (*p <= 0x9b) && (!(*p % 2))) /* Even */
                  (*p)++;                                        /* Next char is lwr */
                break;
              case 0x9c:                                                                               /* Latin ext */
                if ((((*p >= 0xa2) && (*p <= 0xaf)) || ((*p >= 0xb2) && (*p <= 0xbf))) && (!(*p % 2))) /* Even */
                  (*p)++; /* Next char is lwr */
                break;
              case 0x9d:                                            /* Latin ext */
                if ((((*p >= 0x80) && (*p <= 0xaf)) && (!(*p % 2))) /* Even */
                    || (*p == 0xb9) || (*p == 0xbb) || (*p == 0xbe))
                  (*p)++; /* Next char is lwr */
                else if (*p == 0xbd) {
                  *(p - 2) = 0xe1;
                  *(p - 1) = 0xb5;
                  *(p) = 0xb9;
                }
                break;
              case 0x9e: /* Latin ext */
                if (((((*p >= 0x80) && (*p <= 0x87)) || ((*p >= 0x96) && (*p <= 0xa9)) ||
                      ((*p >= 0xb4) && (*p <= 0xbf))) &&
                     (!(*p % 2))) /* Even */
                    || (*p == 0x8b) || (*p == 0x90) || (*p == 0x92))
                  (*p)++; /* Next char is lwr */
                else if (*p == 0xb3) {
                  *(p - 2) = 0xea;
                  *(p - 1) = 0xad;
                  *(p) = 0x93;
                }
                /* case 0x8d: // Two byte small 0xc9 0xa5 */
                /* case 0xaa: // Two byte small 0xc9 0xa6 */
                /* case 0xab: // Two byte small 0xc9 0x9c */
                /* case 0xac: // Two byte small 0xc9 0xa1 */
                /* case 0xad: // Two byte small 0xc9 0xac */
                /* case 0xae: // Two byte small 0xc9 0xaa */
                /* case 0xb0: // Two byte small 0xca 0x9e */
                /* case 0xb1: // Two byte small 0xca 0x87 */
                /* case 0xb2: // Two byte small 0xca 0x9d */
                break;
              case 0x9f: /* Latin ext */
                if ((*p == 0x82) || (*p == 0x87) || (*p == 0x89) || (*p == 0xb5))
                  (*p)++; /* Next char is lwr */
                else if (*p == 0x84) {
                  *(p - 2) = 0xea;
                  *(p - 1) = 0x9e;
                  *(p) = 0x94;
                } else if (*p == 0x86) {
                  *(p - 2) = 0xe1;
                  *(p - 1) = 0xb6;
                  *(p) = 0x8e;
                }
                /* case 0x85: // Two byte small 0xca 0x82 */
                break;
              default:
                break;
            }
            break;
          case 0xef: /* Three byte code */
            pExtChar = p;
            p++;
            switch (*pExtChar) {
              case 0xbc: /* Latin fullwidth */
                if ((*p >= 0xa1) && (*p <= 0xba)) {
                  *pExtChar = 0xbd;
                  (*p) -= 0x20;
                }
                break;
              default:
                break;
            }
            break;
          case 0xf0: /* Four byte code */
            pExtChar = p;
            p++;
            switch (*pExtChar) {
              case 0x90:
                pExtChar = p;
                p++;
                switch (*pExtChar) {
                  case 0x90: /* Deseret */
                    if ((*p >= 0x80) && (*p <= 0x97)) {
                      (*p) += 0x28;
                    } else if ((*p >= 0x98) && (*p <= 0xa7)) {
                      *pExtChar = 0x91;
                      (*p) -= 0x18;
                    }
                    break;
                  case 0x92: /* Osage  */
                    if ((*p >= 0xb0) && (*p <= 0xbf)) {
                      *pExtChar = 0x93;
                      (*p) -= 0x18;
                    }
                    break;
                  case 0x93: /* Osage  */
                    if ((*p >= 0x80) && (*p <= 0x93)) (*p) += 0x28;
                    break;
                  case 0xb2: /* Old hungarian */
                    if ((*p >= 0x80) && (*p <= 0xb2)) *pExtChar = 0xb3;
                    break;
                  default:
                    break;
                }
                break;
              case 0x91:
                pExtChar = p;
                p++;
                switch (*pExtChar) {
                  case 0xa2: /* Warang citi */
                    if ((*p >= 0xa0) && (*p <= 0xbf)) {
                      *pExtChar = 0xa3;
                      (*p) -= 0x20;
                    }
                    break;
                  default:
                    break;
                }
                break;
              case 0x96:
                pExtChar = p;
                p++;
                switch (*pExtChar) {
                  case 0xb9: /* Medefaidrin */
                    if ((*p >= 0x80) && (*p <= 0x9f)) {
                      (*p) += 0x20;
                    }
                    break;
                  default:
                    break;
                }
                break;
              case 0x9E:
                pExtChar = p;
                p++;
                switch (*pExtChar) {
                  case 0xA4: /* Adlam */
                    if ((*p >= 0x80) && (*p <= 0x9d))
                      (*p) += 0x22;
                    else if ((*p >= 0x9e) && (*p <= 0xa1)) {
                      *(pExtChar) = 0xa5;
                      (*p) -= 0x1e;
                    }
                    break;
                  default:
                    break;
                }
                break;
              default:
                break;
            }
            break;
          default:
            break;
        }
        pExtChar = 0;
      }
      p++;
    }
  }
  return copy;
}

unsigned char *__strToUpper__(unsigned char *s) {
  size_t length = strlen((char *)s);
  unsigned char *copy = (unsigned char *)GC_MALLOC_ATOMIC(sizeof(unsigned char) * (length + 1));
  memcpy(copy, s, length + 1);

  unsigned char *p = copy;
  unsigned char *pExtChar = 0;

  if (copy && *copy) {
    while (*p) {
      if ((*p >= 0x61) && (*p <= 0x7a)) /* US ASCII */
        (*p) -= 0x20;
      else if (*p > 0xc0) {
        pExtChar = p;
        p++;
        switch (*pExtChar) {
          case 0xc3: /* Latin 1 */
            /* 0x9f Three byte capital 0xe1 0xba 0x9e */
            if ((*p >= 0xa0) && (*p <= 0xbe) && (*p != 0xb7))
              (*p) -= 0x20; /* US ASCII shift */
            else if (*p == 0xbf) {
              *pExtChar = 0xc5;
              (*p) = 0xb8;
            }
            break;
          case 0xc4:                                                        /* Latin ext */
            if (((*p >= 0x80) && (*p <= 0xb7) && (*p != 0xb1)) && (*p % 2)) /* Odd */
              (*p)--;                                                       /* Prev char is upr */
            else if ((*p >= 0xb9) && (*p <= 0xbe) && (!(*p % 2)))           /* Even */
              (*p)--;                                                       /* Prev char is upr */
            break;
          case 0xc5: /* Latin ext */
            if (*p == 0x80) {
              *pExtChar = 0xc4;
              (*p) = 0xbf;
            } else if ((*p >= 0x81) && (*p <= 0x88) && (!(*p % 2))) /* Even */
              (*p)--;                                               /* Prev char is upr */
            else if ((*p >= 0x8a) && (*p <= 0xb7) && (*p % 2))      /* Odd */
              (*p)--;                                               /* Prev char is upr */
            else if (*p == 0xb8) {
              *pExtChar = 0xc5;
              (*p) = 0xb8;
            } else if ((*p >= 0xb9) && (*p <= 0xbe) && (!(*p % 2))) /* Even */
              (*p)--;                                               /* Prev char is upr */
            break;
          case 0xc6: /* Latin ext */
            switch (*p) {
              case 0x83:
              case 0x85:
              case 0x88:
              case 0x8c:
              case 0x92:
              case 0x99:
              case 0xa1:
              case 0xa3:
              case 0xa5:
              case 0xa8:
              case 0xad:
              case 0xb0:
              case 0xb4:
              case 0xb6:
              case 0xb9:
              case 0xbd:
                (*p)--; /* Prev char is upr */
                break;
              case 0x80:
                *pExtChar = 0xc9;
                (*p) = 0x83;
                break;
              case 0x95:
                *pExtChar = 0xc7;
                (*p) = 0xb6;
                break;
              case 0x9a:
                *pExtChar = 0xc8;
                (*p) = 0xbd;
                break;
              case 0x9e:
                *pExtChar = 0xc8;
                (*p) = 0xa0;
                break;
              case 0xbf:
                *pExtChar = 0xc7;
                (*p) = 0xb7;
                break;
              default:
                break;
            }
            break;
          case 0xc7: /* Latin ext */
            if (*p == 0x85)
              (*p)--; /* Prev char is upr */
            else if (*p == 0x86)
              (*p) = 0x84;
            else if (*p == 0x88)
              (*p)--; /* Prev char is upr */
            else if (*p == 0x89)
              (*p) = 0x87;
            else if (*p == 0x8b)
              (*p)--; /* Prev char is upr */
            else if (*p == 0x8c)
              (*p) = 0x8a;
            else if ((*p >= 0x8d) && (*p <= 0x9c) && (!(*p % 2))) /* Even */
              (*p)--;                                             /* Prev char is upr */
            else if ((*p >= 0x9e) && (*p <= 0xaf) && (*p % 2))    /* Odd */
              (*p)--;                                             /* Prev char is upr */
            else if (*p == 0xb2)
              (*p)--; /* Prev char is upr */
            else if (*p == 0xb3)
              (*p) = 0xb1;
            else if (*p == 0xb5)
              (*p)--;                                          /* Prev char is upr */
            else if ((*p >= 0xb9) && (*p <= 0xbf) && (*p % 2)) /* Odd */
              (*p)--;                                          /* Prev char is upr */
            break;
          case 0xc8:                                           /* Latin ext */
            if ((*p >= 0x80) && (*p <= 0x9f) && (*p % 2))      /* Odd */
              (*p)--;                                          /* Prev char is upr */
            else if ((*p >= 0xa2) && (*p <= 0xb3) && (*p % 2)) /* Odd */
              (*p)--;                                          /* Prev char is upr */
            else if (*p == 0xbc)
              (*p)--; /* Prev char is upr */
            /* 0xbf Three byte capital 0xe2 0xb1 0xbe */
            break;
          case 0xc9: /* Latin ext */
            switch (*p) {
              case 0x80: /* Three byte capital 0xe2 0xb1 0xbf */
              case 0x90: /* Three byte capital 0xe2 0xb1 0xaf */
              case 0x91: /* Three byte capital 0xe2 0xb1 0xad */
              case 0x92: /* Three byte capital 0xe2 0xb1 0xb0 */
              case 0x9c: /* Three byte capital 0xea 0x9e 0xab */
              case 0xa1: /* Three byte capital 0xea 0x9e 0xac */
              case 0xa5: /* Three byte capital 0xea 0x9e 0x8d */
              case 0xa6: /* Three byte capital 0xea 0x9e 0xaa */
              case 0xab: /* Three byte capital 0xe2 0xb1 0xa2 */
              case 0xac: /* Three byte capital 0xea 0x9e 0xad */
              case 0xb1: /* Three byte capital 0xe2 0xb1 0xae */
              case 0xbd: /* Three byte capital 0xe2 0xb1 0xa4 */
                break;
              case 0x82:
                (*p)--; /* Prev char is upr */
                break;
              case 0x93:
                *pExtChar = 0xc6;
                (*p) = 0x81;
                break;
              case 0x94:
                *pExtChar = 0xc6;
                (*p) = 0x86;
                break;
              case 0x96:
                *pExtChar = 0xc6;
                (*p) = 0x89;
                break;
              case 0x97:
                *pExtChar = 0xc6;
                (*p) = 0x8a;
                break;
              case 0x98:
                *pExtChar = 0xc6;
                (*p) = 0x8e;
                break;
              case 0x99:
                *pExtChar = 0xc6;
                (*p) = 0x8f;
                break;
              case 0x9b:
                *pExtChar = 0xc6;
                (*p) = 0x90;
                break;
              case 0xa0:
                *pExtChar = 0xc6;
                (*p) = 0x93;
                break;
              case 0xa3:
                *pExtChar = 0xc6;
                (*p) = 0x94;
                break;
              case 0xa8:
                *pExtChar = 0xc6;
                (*p) = 0x97;
                break;
              case 0xa9:
                *pExtChar = 0xc6;
                (*p) = 0x96;
                break;
              case 0xaf:
                *pExtChar = 0xc6;
                (*p) = 0x9c;
                break;
              case 0xb2:
                *pExtChar = 0xc6;
                (*p) = 0x9d;
                break;
              case 0xb5:
                *pExtChar = 0xc6;
                (*p) = 0x9f;
                break;
              default:
                if ((*p >= 0x87) && (*p <= 0x8f) && (*p % 2)) /* Odd */
                  (*p)--;                                     /* Prev char is upr */
                break;
            }
            break;

          case 0xca: /* Latin ext */
            switch (*p) {
              case 0x82: /* Three byte capital 0xea 0x9f 0x85 */
              case 0x87: /* Three byte capital 0xea 0x9e 0xb1 */
              case 0x9d: /* Three byte capital 0xea 0x9e 0xb2 */
              case 0x9e: /* Three byte capital 0xea 0x9e 0xb0 */
                break;
              case 0x83:
                *pExtChar = 0xc6;
                (*p) = 0xa9;
                break;
              case 0x88:
                *pExtChar = 0xc6;
                (*p) = 0xae;
                break;
              case 0x89:
                *pExtChar = 0xc9;
                (*p) = 0x84;
                break;
              case 0x8a:
                *pExtChar = 0xc6;
                (*p) = 0xb1;
                break;
              case 0x8b:
                *pExtChar = 0xc6;
                (*p) = 0xb2;
                break;
              case 0x8c:
                *pExtChar = 0xc9;
                (*p) = 0x85;
                break;
              case 0x92:
                *pExtChar = 0xc6;
                (*p) = 0xb7;
                break;
              default:
                break;
            }
            break;
          case 0xcd: /* Greek & Coptic */
            switch (*p) {
              case 0xb1:
              case 0xb3:
              case 0xb7:
                (*p)--; /* Prev char is upr */
                break;
              case 0xbb:
                *pExtChar = 0xcf;
                (*p) = 0xbd;
                break;
              case 0xbc:
                *pExtChar = 0xcf;
                (*p) = 0xbe;
                break;
              case 0xbd:
                *pExtChar = 0xcf;
                (*p) = 0xbf;
                break;
              default:
                break;
            }
            break;
          case 0xce: /* Greek & Coptic */
            if (*p == 0xac)
              (*p) = 0x86;
            else if (*p == 0xad)
              (*p) = 0x88;
            else if (*p == 0xae)
              (*p) = 0x89;
            else if (*p == 0xaf)
              (*p) = 0x8a;
            else if ((*p >= 0xb1) && (*p <= 0xbf))
              (*p) -= 0x20; /* US ASCII shift */
            break;
          case 0xcf: /* Greek & Coptic */
            if (*p == 0x82) {
              *pExtChar = 0xce;
              (*p) = 0xa3;
            } else if ((*p >= 0x80) && (*p <= 0x8b)) {
              *pExtChar = 0xce;
              (*p) += 0x20;
            } else if (*p == 0x8c) {
              *pExtChar = 0xce;
              (*p) = 0x8c;
            } else if (*p == 0x8d) {
              *pExtChar = 0xce;
              (*p) = 0x8e;
            } else if (*p == 0x8e) {
              *pExtChar = 0xce;
              (*p) = 0x8f;
            } else if (*p == 0x91)
              (*p) = 0xb4;
            else if (*p == 0x97)
              (*p) = 0x8f;
            else if ((*p >= 0x98) && (*p <= 0xaf) && (*p % 2)) /* Odd */
              (*p)--;                                          /* Prev char is upr */
            else if (*p == 0xb2)
              (*p) = 0xb9;
            else if (*p == 0xb3) {
              *pExtChar = 0xcd;
              (*p) = 0xbf;
            } else if (*p == 0xb8)
              (*p)--; /* Prev char is upr */
            else if (*p == 0xbb)
              (*p)--; /* Prev char is upr */
            break;
          case 0xd0:                                        /* Cyrillic */
            if ((*p >= 0xb0) && (*p <= 0xbf)) (*p) -= 0x20; /* US ASCII shift */
            break;
          case 0xd1: /* Cyrillic supplement */
            if ((*p >= 0x80) && (*p <= 0x8f)) {
              *pExtChar = 0xd0;
              (*p) += 0x20;
            } else if ((*p >= 0x90) && (*p <= 0x9f)) {
              *pExtChar = 0xd0;
              (*p) -= 0x10;
            } else if ((*p >= 0xa0) && (*p <= 0xbf) && (*p % 2)) /* Odd */
              (*p)--;                                            /* Prev char is upr */
            break;
          case 0xd2: /* Cyrillic supplement */
            if (*p == 0x81)
              (*p)--;                                          /* Prev char is upr */
            else if ((*p >= 0x8a) && (*p <= 0xbf) && (*p % 2)) /* Odd */
              (*p)--;                                          /* Prev char is upr */
            break;
          case 0xd3:                                         /* Cyrillic supplement */
            if ((*p >= 0x81) && (*p <= 0x8e) && (!(*p % 2))) /* Even */
              (*p)--;                                        /* Prev char is upr */
            else if (*p == 0x8f)
              (*p) = 0x80;
            else if ((*p >= 0x90) && (*p <= 0xbf) && (*p % 2)) /* Odd */
              (*p)--;                                          /* Prev char is upr */
            break;
          case 0xd4:                                      /* Cyrillic supplement & Armenian */
            if ((*p >= 0x80) && (*p <= 0xaf) && (*p % 2)) /* Odd */
              (*p)--;                                     /* Prev char is upr */
            break;
          case 0xd5: /* Armenian */
            if ((*p >= 0xa1) && (*p <= 0xaf)) {
              *pExtChar = 0xd4;
              (*p) += 0x10;
            } else if ((*p >= 0xb0) && (*p <= 0xbf)) {
              (*p) -= 0x30;
            }
            break;
          case 0xd6: /* Armenian */
            if ((*p >= 0x80) && (*p <= 0x86)) {
              *pExtChar = 0xd5;
              (*p) += 0x10;
            }
            break;
          case 0xe1: /* Three byte code */
            pExtChar = p;
            p++;
            switch (*pExtChar) {
              case 0x82: /* Georgian Asomtavruli  */
                if ((*p >= 0xa0) && (*p <= 0xbf)) {
                  *pExtChar = 0xb2;
                  (*p) -= 0x10;
                }
                break;
              case 0x83: /* Georgian */
                /* Georgian Asomtavruli  */
                if (((*p >= 0x80) && (*p <= 0x85)) || (*p == 0x87) || (*p == 0x8d)) {
                  *pExtChar = 0xb2;
                  (*p) += 0x30;
                }
                /* Georgian mkhedruli */
                else if (((*p >= 0x90) && (*p <= 0xba)) || (*p == 0xbd) || (*p == 0xbe) || (*p == 0xbf)) {
                  *pExtChar = 0xb2;
                }
                break;
              case 0x8f: /* Cherokee */
                if ((*p >= 0xb8) && (*p <= 0xbd)) {
                  (*p) -= 0x08;
                }
                break;
              case 0xb5: /* Latin ext */
                if (*p == 0xb9) {
                  *(p - 2) = 0xea;
                  *(p - 1) = 0x9d;
                  (*p) = 0xbd;
                } else if (*p == 0xbd) {
                  *(p - 2) = 0xe2;
                  *(p - 1) = 0xb1;
                  (*p) = 0xa3;
                }
                break;
              case 0xb6: /* Latin ext */
                if (*p == 0x8e) {
                  *(p - 2) = 0xea;
                  *(p - 1) = 0x9f;
                  (*p) = 0x86;
                }
                break;
              case 0xb8:                                      /* Latin ext */
                if ((*p >= 0x80) && (*p <= 0xbf) && (*p % 2)) /* Odd */
                  (*p)--;                                     /* Prev char is upr */
                break;
              case 0xb9:                                      /* Latin ext */
                if ((*p >= 0x80) && (*p <= 0xbf) && (*p % 2)) /* Odd */
                  (*p)--;                                     /* Prev char is upr */
                break;
              case 0xba:                                           /* Latin ext */
                if ((*p >= 0x80) && (*p <= 0x95) && (*p % 2))      /* Odd */
                  (*p)--;                                          /* Prev char is upr */
                else if ((*p >= 0xa0) && (*p <= 0xbf) && (*p % 2)) /* Odd */
                  (*p)--;                                          /* Prev char is upr */
                break;
              case 0xbb:                                      /* Latin ext */
                if ((*p >= 0x80) && (*p <= 0xbf) && (*p % 2)) /* Odd */
                  (*p)--;                                     /* Prev char is upr */
                break;
              case 0xbc: /* Greek ext */
                if ((*p >= 0x80) && (*p <= 0x87))
                  (*p) += 0x08;
                else if ((*p >= 0x90) && (*p <= 0x95))
                  (*p) += 0x08;
                else if ((*p >= 0xa0) && (*p <= 0xa7))
                  (*p) += 0x08;
                else if ((*p >= 0xb0) && (*p <= 0xb7))
                  (*p) += 0x08;
                break;
              case 0xbd: /* Greek ext */
                if ((*p >= 0x80) && (*p <= 0x85))
                  (*p) += 0x08;
                else if ((*p == 0x91) || (*p == 0x93) || (*p == 0x95) || (*p == 0x97))
                  (*p) += 0x08;
                else if ((*p >= 0xa0) && (*p <= 0xa7))
                  (*p) += 0x08;
                else if ((*p >= 0xb0) && (*p <= 0xb1)) {
                  *(p - 1) = 0xbe;
                  (*p) += 0x0a;
                } else if ((*p >= 0xb2) && (*p <= 0xb5)) {
                  *(p - 1) = 0xbf;
                  (*p) -= 0x2a;
                } else if ((*p >= 0xb6) && (*p <= 0xb7)) {
                  *(p - 1) = 0xbf;
                  (*p) -= 0x1c;
                } else if ((*p >= 0xb8) && (*p <= 0xb9)) {
                  *(p - 1) = 0xbf;
                } else if ((*p >= 0xba) && (*p <= 0xbb)) {
                  *(p - 1) = 0xbf;
                  (*p) -= 0x10;
                } else if ((*p >= 0xbc) && (*p <= 0xbd)) {
                  *(p - 1) = 0xbf;
                  (*p) -= 0x02;
                }
                break;
              case 0xbe: /* Greek ext */
                if ((*p >= 0x80) && (*p <= 0x87))
                  (*p) += 0x08;
                else if ((*p >= 0x90) && (*p <= 0x97))
                  (*p) += 0x08;
                else if ((*p >= 0xa0) && (*p <= 0xa7))
                  (*p) += 0x08;
                else if ((*p >= 0xb0) && (*p <= 0xb1))
                  (*p) += 0x08;
                else if (*p == 0xb3)
                  (*p) += 0x09;
                break;
              case 0xbf: /* Greek ext */
                if (*p == 0x83)
                  (*p) += 0x09;
                else if ((*p >= 0x90) && (*p <= 0x91))
                  *p += 0x08;
                else if ((*p >= 0xa0) && (*p <= 0xa1))
                  (*p) += 0x08;
                else if (*p == 0xa5)
                  (*p) += 0x07;
                else if (*p == 0xb3)
                  (*p) += 0x09;
                break;
              default:
                break;
            }
            break;
          case 0xe2: /* Three byte code */
            pExtChar = p;
            p++;
            switch (*pExtChar) {
              case 0xb0: /* Glagolitic  */
                if ((*p >= 0xb0) && (*p <= 0xbf)) {
                  (*p) -= 0x30;
                }
                break;
              case 0xb1: /* Glagolitic */
                if ((*p >= 0x80) && (*p <= 0x9e)) {
                  *pExtChar = 0xb0;
                  (*p) += 0x10;
                } else { /* Latin ext */
                  switch (*p) {
                    case 0xa1:
                    case 0xa8:
                    case 0xaa:
                    case 0xac:
                    case 0xb3:
                    case 0xb6:
                      (*p)--; /* Prev char is upr */
                      break;
                    case 0xa5: /* Two byte capital  0xc8 0xba */
                    case 0xa6: /* Two byte capital  0xc8 0xbe */
                      break;
                    default:
                      break;
                  }
                }
                break;
              case 0xb2:                                      /* Coptic */
                if ((*p >= 0x80) && (*p <= 0xbf) && (*p % 2)) /* Odd */
                  (*p)--;                                     /* Prev char is upr */
                break;
              case 0xb3:                                       /* Coptic */
                if (((*p >= 0x80) && (*p <= 0xa3) && (*p % 2)) /* Odd */
                    || (*p == 0xac) || (*p == 0xae) || (*p == 0xb3))
                  (*p)--; /* Prev char is upr */
                break;
              case 0xb4: /* Georgian */
                if (((*p >= 0x80) && (*p <= 0xa5)) || (*p == 0xa7) || (*p == 0xad)) {
                  *(p - 2) = 0xe1;
                  *(p - 1) = 0xb2;
                  *(p) += 0x10;
                }
                break;
              default:
                break;
            }
            break;
          case 0xea: /* Three byte code */
            pExtChar = p;
            p++;
            switch (*pExtChar) {
              case 0x99:                                      /* Cyrillic */
                if ((*p >= 0x80) && (*p <= 0xad) && (*p % 2)) /* Odd */
                  (*p)--;                                     /* Prev char is upr */
                break;
              case 0x9a:                                      /* Cyrillic */
                if ((*p >= 0x80) && (*p <= 0x9b) && (*p % 2)) /* Odd */
                  (*p)--;                                     /* Prev char is upr */
                break;
              case 0x9c:                                                                            /* Latin ext */
                if ((((*p >= 0xa2) && (*p <= 0xaf)) || ((*p >= 0xb2) && (*p <= 0xbf))) && (*p % 2)) /* Odd */
                  (*p)--; /* Prev char is upr */
                break;
              case 0x9d:                                       /* Latin ext */
                if (((*p >= 0x80) && (*p <= 0xaf) && (*p % 2)) /* Odd */
                    || (*p == 0xba) || (*p == 0xbc) || (*p == 0xbf))
                  (*p)--; /* Prev char is upr */
                break;
              case 0x9e: /* Latin ext */
                if (((((*p >= 0x80) && (*p <= 0x87)) || ((*p >= 0x96) && (*p <= 0xa9)) ||
                      ((*p >= 0xb4) && (*p <= 0xbf))) &&
                     (*p % 2)) /* Odd */
                    || (*p == 0x8c) || (*p == 0x91) || (*p == 0x93))
                  (*p)--; /* Prev char is upr */
                else if (*p == 0x94) {
                  *(p - 2) = 0xea;
                  *(p - 1) = 0x9f;
                  *(p) = 0x84;
                }
                break;
              case 0x9f:                                                                  /* Latin ext */
                if ((*p == 0x83) || (*p == 0x88) || (*p == 0x8a) || (*p == 0xb6)) (*p)--; /* Prev char is upr */
                break;
              case 0xad:
                /* Latin ext */
                if (*p == 0x93) {
                  *pExtChar = 0x9e;
                  (*p) = 0xb3;
                }
                /* Cherokee */
                else if ((*p >= 0xb0) && (*p <= 0xbf)) {
                  *(p - 2) = 0xe1;
                  *pExtChar = 0x8e;
                  (*p) -= 0x10;
                }
                break;
              case 0xae: /* Cherokee */
                if ((*p >= 0x80) && (*p <= 0x8f)) {
                  *(p - 2) = 0xe1;
                  *pExtChar = 0x8e;
                  (*p) += 0x30;
                } else if ((*p >= 0x90) && (*p <= 0xbf)) {
                  *(p - 2) = 0xe1;
                  *pExtChar = 0x8f;
                  (*p) -= 0x10;
                }
                break;
              default:
                break;
            }
            break;
          case 0xef: /* Three byte code */
            pExtChar = p;
            p++;
            switch (*pExtChar) {
              case 0xbd: /* Latin fullwidth */
                if ((*p >= 0x81) && (*p <= 0x9a)) {
                  *pExtChar = 0xbc;
                  (*p) += 0x20;
                }
                break;
              default:
                break;
            }
            break;
          case 0xf0: /* Four byte code */
            pExtChar = p;
            p++;
            switch (*pExtChar) {
              case 0x90:
                pExtChar = p;
                p++;
                switch (*pExtChar) {
                  case 0x90: /* Deseret */
                    if ((*p >= 0xa8) && (*p <= 0xbf)) {
                      (*p) -= 0x28;
                    }
                    break;
                  case 0x91: /* Deseret */
                    if ((*p >= 0x80) && (*p <= 0x8f)) {
                      *pExtChar = 0x90;
                      (*p) += 0x18;
                    }
                    break;
                  case 0x93: /* Osage  */
                    if ((*p >= 0x98) && (*p <= 0xa7)) {
                      *pExtChar = 0x92;
                      (*p) += 0x18;
                    } else if ((*p >= 0xa8) && (*p <= 0xbb))
                      (*p) -= 0x28;
                    break;
                  case 0xb3: /* Old hungarian */
                    if ((*p >= 0x80) && (*p <= 0xb2)) *pExtChar = 0xb2;
                    break;
                  default:
                    break;
                }
                break;
              case 0x91:
                pExtChar = p;
                p++;
                switch (*pExtChar) {
                  case 0xa3: /* Warang citi */
                    if ((*p >= 0x80) && (*p <= 0x9f)) {
                      *pExtChar = 0xa2;
                      (*p) += 0x20;
                    }
                    break;
                  default:
                    break;
                }
                break;
              case 0x96:
                pExtChar = p;
                p++;
                switch (*pExtChar) {
                  case 0xb9: /* Medefaidrin */
                    if ((*p >= 0xa0) && (*p <= 0xbf)) (*p) -= 0x20;
                    break;
                  default:
                    break;
                }
                break;
              case 0x9E:
                pExtChar = p;
                p++;
                switch (*pExtChar) {
                  case 0xA4: /* Adlam */
                    if ((*p >= 0xa2) && (*p <= 0xbf)) (*p) -= 0x22;
                    break;
                  case 0xA5: /* Adlam */
                    if ((*p >= 0x80) && (*p <= 0x83)) {
                      *(pExtChar) = 0xa4;
                      (*p) += 0x1e;
                    }
                    break;
                  default:
                    break;
                }
                break;
            }
            break;
          default:
            break;
        }
        pExtChar = 0;
      }
      p++;
    }
  }
  return copy;
}
