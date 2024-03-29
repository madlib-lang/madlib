#ifndef MAYBE_H
#define MAYBE_H

#include <stdint.h>

const int64_t madlib__maybe__Maybe_JUST_INDEX = 0;
const int64_t madlib__maybe__Maybe_NOTHING_INDEX = 1;

typedef struct madlib__maybe__Maybe {
  int64_t index;
  void *data;
} madlib__maybe__Maybe_t;

#endif // MAYBE_H
