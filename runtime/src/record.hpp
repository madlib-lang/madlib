#ifndef RECORD_H
#define RECORD_H

#include <inttypes.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct RecordField {
  char *name;
  void *value;
} RecordField_t;

typedef struct Record {
  int32_t fieldCount;
  RecordField_t **fields;
} Record_t;

Record_t *__buildRecord__(int32_t fieldCount, Record_t *base, ...);

void *__selectField__(char *name, Record_t *record);

#ifdef __cplusplus
}
#endif

#endif // RECORD_H