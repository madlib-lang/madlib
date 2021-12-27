#include <stdio.h>


typedef struct MadArray {
  int64_t length;
  void **items;  // an item is a void*
} MadArray_t;
