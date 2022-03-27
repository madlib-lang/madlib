#ifndef RECORD_H
#define RECORD_H

#include <stdint.h>
#include <inttypes.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct madlib__record__Field {
  char *name;
  void *value;
} madlib__record__Field_t;

typedef struct madlib__record__Record {
  int32_t fieldCount;
  madlib__record__Field_t **fields;
} madlib__record__Record_t;

madlib__record__Record_t *madlib__record__internal__buildRecord(int32_t fieldCount, madlib__record__Record_t *base, ...);

void *madlib__record__internal__selectField(char *name, madlib__record__Record_t *record);

#ifdef __cplusplus
}
#endif

#endif // RECORD_H
