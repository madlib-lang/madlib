#ifndef STRING_H
#define STRING_H

#ifdef __cplusplus
extern "C" {
#endif

bool *madlib__string__internal__eq(char **s1, char **s2);

bool madlib__string__internal__areStringsEqual(char *s1, char *s2);
bool madlib__string__internal__areStringsNotEqual(char *s1, char *s2);

int64_t madlib__string__length(char *s);

char *madlib__string__internal__concat(char *s1, char *s2);

char *stripTrailingZeros(char *number);

#ifdef __cplusplus
}
#endif

#endif // STRING_H
