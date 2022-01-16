#ifndef MAYBE_H
#define MAYBE_H

const int64_t madlib__maybe__Maybe_JUST_INDEX = 0;
const int64_t madlib__maybe__Maybe_NOTHING_INDEX = 1;

typedef struct madlib__maybe__Maybe_Just {
  int64_t index;
  void *data;
} madlib__maybe__Maybe_Just_t;

typedef struct madlib__maybe__Maybe_Nothing {
  int64_t index;
} madlib__maybe__Maybe_Nothing_t;

#endif // MAYBE_H
