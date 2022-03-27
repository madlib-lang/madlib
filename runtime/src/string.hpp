#ifndef STRING_H
#define STRING_H

#include <stdint.h>
#include "apply-pap.hpp"
#include "list.hpp"
#include "maybe.hpp"
#include "char.hpp"

#ifdef __cplusplus
extern "C" {
#endif


bool *madlib__string__internal__eq(char **s1, char **s2);

int64_t madlib__string__compare(char *s1, char *s2);

bool madlib__string__internal__areStringsEqual(char *s1, char *s2);
bool madlib__string__internal__areStringsNotEqual(char *s1, char *s2);

int64_t madlib__string__length(unsigned char *s);

char *madlib__string__slice(int64_t start, int64_t end, unsigned char *s);

char *madlib__string__pushChar(int32_t c, char* s);
char *madlib__string__appendChar(int32_t c, char* s);

char *madlib__string__trim(char *s);
char *madlib__string__trimStart(char *s);
char *madlib__string__trimEnd(char *s);

madlib__maybe__Maybe_t *madlib__string__charAt(int64_t n, unsigned char *s);

char **madlib__string__internal__inspect(char **s);

char *madlib__string__internal__concat(char *s1, char *s2);

char *stripTrailingZeros(char *number);

char *madlib__string__mapChars(PAP_t *pap, char *str);

madlib__list__Node_t *madlib__string__toList(char *str);

char *madlib__string__fromList(madlib__list__Node_t *list);

madlib__list__Node_t *madlib__string__split(char *str, char *separator);

char *madlib__string__replace(char *regex, char *replace, char *str);
bool madlib__string__match(char *regex, char *str);

unsigned char *madlib__string__toUpper(unsigned char *str);
unsigned char *madlib__string__toLower(unsigned char *str);

#ifdef __cplusplus
}
#endif

#endif // STRING_H
