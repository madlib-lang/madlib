#ifndef CHAR_H
#define CHAR_H

#include <iostream>
#include <stdint.h>


#ifdef __cplusplus
extern "C" {
#endif

bool *madlib__char__internal__eq(int32_t *a, int32_t *b);

char *madlib__char__internal__show(int32_t unicode);
char **madlib__char__internal__inspect(int32_t *unicode);

int64_t madlib__char__compare(int32_t c1, int32_t c2);

int32_t madlib__char__toLower(int32_t c);
int32_t madlib__char__toUpper(int32_t c);

char *utf8Encode(int32_t unicode);

int32_t *utf8Decode(char *str);
int32_t utf8DecodeChar(const char *str, int *i);

#ifdef __cplusplus
}
#endif

#endif // CHAR_H
