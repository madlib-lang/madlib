
#include <gc.h>
#include <iostream>
#include <string.h>
#include <cstdarg>
#include "record.hpp"

// Records

#ifdef __cplusplus
extern "C" {
#endif

madlib__record__Field_t *retrieveField(char *name, madlib__record__Record_t *record) {
  for (int i = 0; i < record->fieldCount; i++) {
    madlib__record__Field_t *currentField = record->fields[i];
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
madlib__record__Record_t *madlib__record__internal__buildRecord(int32_t fieldCount, madlib__record__Record_t *base, ...) {
  va_list fieldArgs;
  va_start(fieldArgs, base);

  int32_t actualFieldCount = base == NULL ? fieldCount : base->fieldCount;

  madlib__record__Record_t *record = (madlib__record__Record_t *)GC_MALLOC(sizeof(madlib__record__Record_t));

  madlib__record__Field_t **fields =
      (madlib__record__Field_t **)GC_MALLOC(sizeof(madlib__record__Field_t) * actualFieldCount);
  record->fieldCount = actualFieldCount;
  record->fields = fields;

  if (base == NULL) {
    // if there is no base then all fields are provided ( closed record ) and we
    // simply push all fields
    for (int i = 0; i < fieldCount; i++) {
      madlib__record__Field_t *currentField = va_arg(fieldArgs, madlib__record__Field_t *);
      fields[i] = currentField;
    }
  } else {
    // If it's an extension we need to overwrite the update fields
    // first we copy all fields
    for (int i = 0; i < base->fieldCount; i++) {
      record->fields[i] = (madlib__record__Field_t *)GC_MALLOC(sizeof(madlib__record__Field_t));
      record->fields[i]->name = base->fields[i]->name;
      record->fields[i]->value = base->fields[i]->value;
    }

    for (int i = 0; i < fieldCount; i++) {
      madlib__record__Field_t *currentField = va_arg(fieldArgs, madlib__record__Field_t *);
      madlib__record__Field_t *targetField = retrieveField(currentField->name, record);
      targetField->value = currentField->value;
    }
  }
  va_end(fieldArgs);

  return record;
}

/**
 * low level function for record.field
 */
void *madlib__record__internal__selectField(char *name, madlib__record__Record_t *record) {
  for (int i = 0; i < record->fieldCount; i++) {
    madlib__record__Field_t *currentField = record->fields[i];
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
