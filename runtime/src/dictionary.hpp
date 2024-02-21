#ifndef DICTIONARY_H
#define DICTIONARY_H

#include <iostream>
#include <stdint.h>
#include "eq.hpp"
#include "list.hpp"

typedef struct madlib__dictionary__Dictionary {
  int64_t constructorIndex;
  madlib__list__Node_t *items;
} madlib__dictionary__Dictionary_t;

#ifdef __cplusplus
extern "C" {
#endif

madlib__dictionary__Dictionary_t *madlib__dictionary__typeConstructor(madlib__list__Node_t *items);

#ifdef __cplusplus
}
#endif

#endif // DICTIONARY_H