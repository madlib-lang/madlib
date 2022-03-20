#ifndef STRING_H
#define STRING_H

#include "apply-pap.hpp"
#include "list.hpp"

#ifdef __cplusplus
extern "C" {
#endif


bool *madlib__string__internal__eq(char **s1, char **s2);

int64_t madlib__string__compare(char *s1, char *s2);

bool madlib__string__internal__areStringsEqual(char *s1, char *s2);
bool madlib__string__internal__areStringsNotEqual(char *s1, char *s2);

int64_t madlib__string__length(unsigned char *s);

char *madlib__string__internal__concat(char *s1, char *s2);

char *stripTrailingZeros(char *number);

char *madlib__string__mapChars(PAP_t *pap, char *str);

madlib__list__Node_t *madlib__string__toList(char *str);

char *madlib__string__fromList(madlib__list__Node_t *list);

madlib__list__Node_t *madlib__string__split(char *str, char *separator);

#ifdef __cplusplus
}
#endif

#endif // STRING_H
