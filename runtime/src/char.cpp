
#include <gc.h>
#include "char.hpp"


#include <string.h>
#include "string.hpp"

#ifdef __cplusplus
extern "C" {
#endif

#define isunicode(c) (((c)&0xc0) == 0xc0)

// char *utf8Encode(int32_t unicode) {
//   if (unicode <= 0x7f) {
//     // ASCII character
//     char *str = (char *)GC_MALLOC_ATOMIC(sizeof(char) * 2);
//     str[0] = (char)unicode;
//     str[1] = '\0';
//     return str;
//   } else if (unicode <= 0x07ff) {
//     // 2-byte unicode
//     char *str = (char *)GC_MALLOC_ATOMIC(sizeof(char) * 3);
//     str[0] = (char)(((unicode >> 6) & 0x1f) | 0xc0);
//     str[1] = (char)(((unicode >> 0) & 0x3f) | 0x80);
//     str[2] = '\0';
//     return str;
//   } else if (unicode <= 0xffff) {
//     // 3-byte unicode
//     char *str = (char *)GC_MALLOC_ATOMIC(sizeof(char) * 4);
//     str[0] = (char)(((unicode >> 12) & 0x0f) | 0xe0);
//     str[1] = (char)(((unicode >> 6) & 0x3f) | 0x80);
//     str[2] = (char)(((unicode >> 0) & 0x3f) | 0x80);
//     str[3] = '\0';
//     return str;
//   } else if (unicode <= 0x10ffff) {
//     // 4-byte unicode
//     char *str = (char *)GC_MALLOC_ATOMIC(sizeof(char) * 5);
//     str[0] = (char)(((unicode >> 18) & 0x07) | 0xf0);
//     str[1] = (char)(((unicode >> 12) & 0x3f) | 0x80);
//     str[2] = (char)(((unicode >> 6) & 0x3f) | 0x80);
//     str[3] = (char)(((unicode >> 0) & 0x3f) | 0x80);
//     str[4] = '\0';
//     return str;
//   } else {
//     // or use replacement character
//     char *str = (char *)GC_MALLOC_ATOMIC(sizeof(char) * 4);
//     str[0] = (char)0xef;
//     str[1] = (char)0xbf;
//     str[2] = (char)0xbf;
//     str[3] = '\0';
//     return str;
//   }
// }



// int32_t utf8DecodeChar(const char *str, int *i) {
//   const unsigned char *s = (const unsigned char *)str;
//   int32_t u = *s;
//   int l = 1;
//   if (isunicode(u)) {
//     int a = (u & 0x20)
//                 ? ((u & 0x10) ? ((u & 0x08) ? ((u & 0x04) ? 6 : 5) : 4) : 3)
//                 : 2;
//     if (a < 6 || !(u & 0x02)) {
//       int b, p = 0;
//       u = ((u << (a + 1)) & 0xff) >> (a + 1);
//       for (b = 1; b < a; ++b) u = (u << 6) | (s[l++] & 0x3f);
//     }
//   }
//   if (i) *i += l;
//   return u;
// }
typedef struct {
  uint8_t mask;    /* char data will be bitwise AND with this */
  uint8_t lead;    /* start bytes of current char in utf-8 encoded character */
  uint32_t beg; /* beginning of codepoint range */
  uint32_t end; /* end of codepoint range */
  int bits_stored; /* the number of bits from the codepoint that fits in char */
} utf_t;


/*                   mask        lead        beg      end       bits */
utf_t utf0 = (utf_t){0b00111111, 0b10000000, 0,       0,        6    };
utf_t utf1 = (utf_t){0b01111111, 0b00000000, 0000,    0177,     7    };
utf_t utf2 = (utf_t){0b00011111, 0b11000000, 0200,    03777,    5    };
utf_t utf3 = (utf_t){0b00001111, 0b11100000, 04000,   0177777,  4    };
utf_t utf4 = (utf_t){0b00000111, 0b11110000, 0200000, 04177777, 3    };
utf_t utf5 = (utf_t){0};

utf_t *utf[] = {
  [0] = &utf0,
  [1] = &utf1,
  [2] = &utf2,
  [3] = &utf3,
  [4] = &utf4,
        &utf5,
};

int utf8CharLength(uint8_t ch)
{
  int len = 0;
  for(utf_t **u = utf; *u; ++u) {
    if(len > 4) { /* Malformed leading byte */
      return 1;
    }
    if((ch & ~(*u)->mask) == (*u)->lead) {
      break;
    }
    ++len;
  }
  
  return len;
}

int utf8CodepointLength(const uint32_t cp)
{
  int len = 0;
  for(utf_t **u = utf; *u; ++u) {
    if(len > 4) { /* Out of bounds */
      return 1;
    }
    if((cp >= (*u)->beg) && (cp <= (*u)->end)) {
      break;
    }
    ++len;
  }

  return len;
}

char *utf8EncodeChar(int32_t cp)
{
  const int bytes = utf8CodepointLength(cp);
  uint8_t *ret = (uint8_t*) GC_MALLOC_ATOMIC(bytes + 1);

  int shift = utf[0]->bits_stored * (bytes - 1);
  ret[0] = (cp >> shift & utf[bytes]->mask) | utf[bytes]->lead;
  shift -= utf[0]->bits_stored;
  for(int i = 1; i < bytes; ++i) {
    ret[i] = (cp >> shift & utf[0]->mask) | utf[0]->lead;
    shift -= utf[0]->bits_stored;
  }
  ret[bytes] = 0;
  return (char*)ret;
}


int32_t utf8DecodeChar(const char *chr, int *inputIndex)
{
  int bytes = utf8CharLength(*chr);
  int shift = utf[0]->bits_stored * (bytes - 1);
  uint32_t codep = (*chr++ & utf[bytes]->mask) << shift;

  for(int i = 1; i < bytes; ++i, ++chr) {
    shift -= utf[0]->bits_stored;
    codep |= ((char)*chr & utf[0]->mask) << shift;
  }

  *inputIndex += bytes;
  return codep;
}

int32_t *utf8Decode(char *str) {
  int i = 0;
  int outputIndex = 0;
  size_t strLength = strlen(str);

  // Note: using GC_MALLOC_ATOMIC seems to cause to following warning:
  // "GC Warning: Repeated allocation of very large block (appr. size 100007936):
  // May lead to memory leak and poor performance"
  // which causes memory to be corrupted when the input string is big ( tested with 10MB string )
  int32_t *output = (int32_t*)GC_MALLOC(sizeof(int32_t) * (strLength + 1));
  while (str[i] != '\0') {
    output[outputIndex] = utf8DecodeChar(&str[i], &i);
    outputIndex++;
  }
  output[outputIndex] = 0;

  return output;
}

bool madlib__char__internal__eq(int32_t a, int32_t b) {
  return a == b;
}

char *madlib__char__internal__show(int32_t unicode) {
  return utf8EncodeChar(unicode);
}

char *madlib__char__internal__inspect(int32_t unicode) {
  char *inspected;

  if (unicode == '\n' || unicode == '\t' || unicode == '\r' || unicode == '\\' || unicode == '\'') {
    char *result = (char*)GC_MALLOC_ATOMIC(sizeof(char) * 5);
    result[0] = '\'';
    result[1] = '\\';
    result[3] = '\'';
    result[4] = '\0';

    switch (unicode) {
      case '\'':
        result[2] = '\'';
        break;
      case '\\':
        result[2] = '\\';
        break;
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

    inspected = result;
  } else {
    char *encoded = utf8EncodeChar(unicode);
    size_t encodedLength = strlen(encoded);
    char *full = (char*)GC_MALLOC_ATOMIC(sizeof(char) * (encodedLength + 3));
    full[0] = '\'';
    memcpy(full + 1, encoded, encodedLength);
    full[1 + encodedLength] = '\'';
    full[2 + encodedLength] = '\0';

    inspected = full;
  }

  return inspected;
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
  unsigned char *encoded = (unsigned char*)utf8EncodeChar(c);
  unsigned char *lowered = madlib__string__toLower(encoded);
  int l;
  return utf8DecodeChar((char*)lowered, &l);
}

int32_t madlib__char__toUpper(int32_t c) {
  unsigned char *encoded = (unsigned char*)utf8EncodeChar(c);
  unsigned char *uppered = madlib__string__toUpper(encoded);
  int l;
  return utf8DecodeChar((char*)uppered, &l);
}

#ifdef __cplusplus
}
#endif
