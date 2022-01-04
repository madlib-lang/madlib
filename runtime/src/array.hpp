#ifndef ARRAY_H
#define ARRAY_H

#include <stdio.h>

typedef struct MadArray {
  int64_t length;
  void **items;  // an item is a void*
} MadArray_t;

#endif // ARRAY_H
