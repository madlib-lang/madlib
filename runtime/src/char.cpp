#include "char.hpp"

#include <gc.h>
#include <string.h>
#include "string.hpp"

#ifdef __cplusplus
extern "C" {
#endif

#define isunicode(c) (((c)&0xc0) == 0xc0)

char *utf8Encode(int32_t unicode) {
  if (unicode <= 0x7f) {
    // ASCII character
    char *str = (char *)GC_malloc_atomic(sizeof(char) * 2);
    str[0] = (char)unicode;
    str[1] = '\0';
    return str;
  } else if (unicode <= 0x07ff) {
    // 2-byte unicode
    char *str = (char *)GC_malloc_atomic(sizeof(char) * 3);
    str[0] = (char)(((unicode >> 6) & 0x1f) | 0xc0);
    str[1] = (char)(((unicode >> 0) & 0x3f) | 0x80);
    str[2] = '\0';
    return str;
  } else if (unicode <= 0xffff) {
    // 3-byte unicode
    char *str = (char *)GC_malloc_atomic(sizeof(char) * 4);
    str[0] = (char)(((unicode >> 12) & 0x0f) | 0xe0);
    str[1] = (char)(((unicode >> 6) & 0x3f) | 0x80);
    str[2] = (char)(((unicode >> 0) & 0x3f) | 0x80);
    str[3] = 0;
    return str;
  } else if (unicode <= 0x10ffff) {
    // 4-byte unicode
    char *str = (char *)GC_malloc_atomic(sizeof(char) * 5);
    str[0] = (char)(((unicode >> 18) & 0x07) | 0xf0);
    str[1] = (char)(((unicode >> 12) & 0x3f) | 0x80);
    str[2] = (char)(((unicode >> 6) & 0x3f) | 0x80);
    str[3] = (char)(((unicode >> 0) & 0x3f) | 0x80);
    str[4] = '\0';
    return str;
  } else {
    // or use replacement character
    char *str = (char *)GC_malloc_atomic(sizeof(char) * 4);
    str[0] = (char)0xef;
    str[1] = (char)0xbf;
    str[2] = (char)0xbf;
    str[3] = '\0';
    return str;
  }
}

int32_t utf8DecodeChar(const char *str, int *i) {
  const unsigned char *s = (const unsigned char *)str;
  int32_t u = *s;
  int l = 1;
  if (isunicode(u)) {
    int a = (u & 0x20)
                ? ((u & 0x10) ? ((u & 0x08) ? ((u & 0x04) ? 6 : 5) : 4) : 3)
                : 2;
    if (a < 6 || !(u & 0x02)) {
      int b, p = 0;
      u = ((u << (a + 1)) & 0xff) >> (a + 1);
      for (b = 1; b < a; ++b) u = (u << 6) | (s[l++] & 0x3f);
    }
  }
  if (i) *i += l;
  return u;
}

int32_t *utf8Decode(char *str) {
  int l;
  int i = 0;
  int outputIndex = 0;
  size_t strLength = strlen(str);
  int32_t *output = (int32_t*)GC_malloc_atomic(sizeof(int32_t) * (strLength + 1));
  while (str[i] != '\0') {
    if (!isunicode(str[i])) {
      output[outputIndex] = str[i];
      i++;
    } else {
      l = 0;
      output[outputIndex] = utf8DecodeChar(&str[i], &l);
      i += l;
    }
    outputIndex++;
  }
  output[outputIndex] = 0;

  return output;
}

bool *madlib__char__internal__eq(int32_t *a, int32_t *b) {
  bool *boxed = (bool *)GC_malloc_atomic(sizeof(bool));
  *boxed = *a == *b;
  return boxed;
}

char *madlib__char__internal__show(int32_t unicode) {
  return utf8Encode(unicode);
}

char **madlib__char__internal__inspect(int32_t *unicode) {
  char **boxed = (char **)GC_malloc(sizeof(char *));

  if (*unicode == '\n' || *unicode == '\t' || *unicode == '\r') {
    char *result = (char*)GC_malloc_atomic(sizeof(char) * 5);
    result[0] = '\'';
    result[1] = '\\';
    result[3] = '\'';
    result[4] = '\0';

    switch (*unicode) {
      case '\n':
        result[2] = 'n';
        break;
      case '\t':
        result[2] = 't';
        break;
      case '\r':
        result[2] = 'r';
        break;
    }

    *boxed = result;
  } else {
    char *encoded = utf8Encode(*unicode);
    size_t encodedLength = strlen(encoded);
    char *full = (char*)GC_malloc_atomic(sizeof(char) * (encodedLength + 3));
    full[0] = '\'';
    memcpy(full + 1, encoded, encodedLength);
    full[1 + encodedLength] = '\'';
    full[2 + encodedLength] = '\0';

    *boxed = full;
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

int32_t madlib__char__toLower(int32_t c) {
  unsigned char *encoded = (unsigned char*)utf8Encode(c);
  unsigned char *lowered = madlib__string__toLower(encoded);
  int l;
  return utf8DecodeChar((char*)lowered, &l);
}

int32_t madlib__char__toUpper(int32_t c) {
  unsigned char *encoded = (unsigned char*)utf8Encode(c);
  unsigned char *uppered = madlib__string__toUpper(encoded);
  int l;
  return utf8DecodeChar((char*)uppered, &l);
}

#ifdef __cplusplus
}
#endif
