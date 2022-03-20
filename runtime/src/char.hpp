#ifndef CHAR_H
#define CHAR_H

#include <iostream>


#ifdef __cplusplus
extern "C" {
#endif

bool *madlib__char__internal__eq(int32_t *a, int32_t *b);

char **madlib__char__internal__inspect(int32_t *c);

int64_t madlib__char__compare(int32_t c1, int32_t c2);

#ifdef __cplusplus
}
#endif

#endif // CHAR_H
