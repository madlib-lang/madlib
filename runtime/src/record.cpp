#include <gc.h>
#include <iostream>
#include <inttypes.h>
#include <string.h>
#include <cstdarg>


// Records

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

RecordField_t *__findField__(char *name, Record_t *record) {
  for (int i = 0; i < record->fieldCount; i++) {
    RecordField_t *currentField = record->fields[i];
    if (strcmp(name, currentField->name) == 0) {
      return currentField;
    }
  }

  return NULL;
}

/**
 * low level function for { name: value }
 * TODO: need to also handle update eg. { ...base, updatedField: newValue }
 */
Record_t *__buildRecord__(int32_t fieldCount, Record_t *base, ...) {
  va_list fieldArgs;
  va_start(fieldArgs, base);

  int32_t actualFieldCount = base == NULL ? fieldCount : base->fieldCount;

  Record_t *record = (Record_t *)GC_malloc(sizeof(Record_t));

  RecordField_t **fields =
      (RecordField **)GC_malloc(sizeof(RecordField_t) * actualFieldCount);
  record->fieldCount = actualFieldCount;
  record->fields = fields;

  if (base == NULL) {
    // if there is no base then all fields are provided ( closed record ) and we
    // simply push all fields
    for (int i = 0; i < fieldCount; i++) {
      RecordField_t *currentField = va_arg(fieldArgs, RecordField_t *);
      fields[i] = currentField;
    }
  } else {
    // If it's an extension we need to overwrite the update fields
    // first we copy all fields
    for (int i = 0; i < base->fieldCount; i++) {
      record->fields[i] = (RecordField_t *)GC_malloc(sizeof(RecordField_t));
      record->fields[i]->name = base->fields[i]->name;
      record->fields[i]->value = base->fields[i]->value;
    }

    for (int i = 0; i < fieldCount; i++) {
      RecordField_t *currentField = va_arg(fieldArgs, RecordField_t *);
      RecordField_t *targetField = __findField__(currentField->name, record);
      targetField->value = currentField->value;
    }
  }
  va_end(fieldArgs);

  return record;
}

/**
 * low level function for record.field
 */
void *__selectField__(char *name, Record_t *record) {
  for (int i = 0; i < record->fieldCount; i++) {
    RecordField_t *currentField = record->fields[i];
    if (strcmp(name, currentField->name) == 0) {
      return currentField->value;
    }
  }

  printf("field not found: %s\n", name);

  return NULL;
}

#ifdef __cplusplus
}
#endif
