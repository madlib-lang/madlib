#ifndef URL_H
#define URL_H

#include "maybe.hpp"

#ifdef __cplusplus
extern "C" {
#endif

madlib__maybe__Maybe_t *madlib__url__encode(char *url);

madlib__maybe__Maybe_t *madlib__url__decode(char *url);

#ifdef __cplusplus
}
#endif

#endif // URL_H
