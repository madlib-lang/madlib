
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
 * low level function for { ...base, name: value }
 */
madlib__record__Record_t *madlib__record__internal__buildRecord(int32_t fieldCount, madlib__record__Record_t *base, ...) {
  if(fieldCount == 0) {
    return base;
  }

  va_list fieldArgs;
  va_start(fieldArgs, base);

  madlib__record__Record_t *record = (madlib__record__Record_t *)GC_MALLOC(sizeof(madlib__record__Record_t));

  if (base == NULL) {
    int32_t actualFieldCount = base == NULL ? fieldCount : base->fieldCount;
    madlib__record__Field_t **fields =
        (madlib__record__Field_t **)GC_MALLOC(sizeof(madlib__record__Field_t*) * actualFieldCount);
    record->fieldCount = actualFieldCount;
    record->fields = fields;
    // if there is no base then all fields are provided ( closed record ) and we
    // simply push all fields
    for (int i = 0; i < fieldCount; i++) {
      madlib__record__Field_t *currentField = va_arg(fieldArgs, madlib__record__Field_t *);
      fields[i] = currentField;
    }
  } else {
    int32_t maxFieldCount = base == NULL ? fieldCount : base->fieldCount + fieldCount;
    madlib__record__Field_t **fields =
        (madlib__record__Field_t **)GC_MALLOC(sizeof(madlib__record__Field_t*) * maxFieldCount);
    record->fieldCount = 0;
    record->fields = fields;
    int32_t baseIndex = 0;
    int32_t fieldsIndex = 0;

    madlib__record__Field_t *currentField = va_arg(fieldArgs, madlib__record__Field_t *);

    while (baseIndex < base->fieldCount || fieldsIndex < fieldCount) {
      int cmpResult = 1;
      if (fieldsIndex >= fieldCount) {
        cmpResult = -1;
      } else if (baseIndex < base->fieldCount) {
        cmpResult = strcmp(base->fields[baseIndex]->name, currentField->name);
      }

      if (cmpResult >= 0) {
        // keys are equal, we replace the value, or the base is higher than the current one
        record->fields[record->fieldCount] = currentField;
        fieldsIndex = fieldsIndex + 1;
        if (fieldsIndex < fieldCount) {
          currentField = va_arg(fieldArgs, madlib__record__Field_t *);
        }
        if (cmpResult == 0) {
          // if we overwrite a key we must increase the baseIndex
          baseIndex = baseIndex + 1;
        }
      } else {
        // base key is lower, so we just add it from the base
        record->fields[record->fieldCount] = base->fields[baseIndex];
        baseIndex = baseIndex + 1;
      }

      record->fieldCount = record->fieldCount + 1;
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
