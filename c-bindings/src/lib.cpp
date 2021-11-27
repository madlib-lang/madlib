#include <gc.h>
#include <iostream>
#include <inttypes.h>

#include "apply-pap.hpp"


// TODO: move applyPAP to its own file


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

// Dictionary

#ifdef __cplusplus
extern "C" {
#endif

// void* __buildDictionary__() {}

#ifdef __cplusplus
}
#endif

// String

#ifdef __cplusplus
extern "C" {
#endif

bool __streq__(char *s1, char *s2) {
  if (strcmp(s1, s2) == 0) {
    return true;
  } else {
    return false;
  }
}

double *__strLength__(char *s) {
  double *result = (double *)GC_malloc(sizeof(double));
  *result = strlen(s);
  return result;
}

char *__strConcat__(char *s1, char *s2) {
  char *result = (char *)GC_malloc(strlen(s1) + strlen(s2) + 1);
  strcpy(result, s1);
  strcat(result, s2);
  return result;
}

char *__stripTrailingZeros__(char *number) {
  int length = strlen(number);
  char *end = number + strlen(number) - 1;
  int charsToRemove = 0;

  while (*end == '0' && charsToRemove < length) {
    charsToRemove += 1;
    end -= 1;
  }

  if (*end == '.') {
    charsToRemove += 1;
  }

  char *result = (char *)GC_malloc(length - charsToRemove + 1);
  memcpy(result, number, length - charsToRemove);
  result[length - charsToRemove] = '\0';

  return result;
}

#ifdef __cplusplus
}
#endif


// Show

#ifdef __cplusplus
extern "C" {
#endif

char *__floatToStr__(double d) {
  char *str = (char *)GC_malloc(200);
  sprintf(str, "%.20f", d);
  char *stripped = __stripTrailingZeros__(str);

  return stripped;
}

char *__integerToStr__(int64_t i) {
  int64_t *ii = (int64_t *) i;
  char *str = (char *)GC_malloc(200);
  sprintf(str, "%" PRId64, i);

  return str;
}

char *__byteToStr__(unsigned char i) {
  char *str = (char *)GC_malloc(4);
  sprintf(str, "%d", i);

  return str;
}

char *__booleanToStr__(bool b) {
  if (b) {
    char *str = (char *)GC_malloc(5);
    str[0] = 't';
    str[1] = 'r';
    str[2] = 'u';
    str[3] = 'e';
    str[4] = '\0';
    return str;
  } else {
    char *str = (char *)GC_malloc(6);
    str[0] = 'f';
    str[1] = 'a';
    str[2] = 'l';
    str[3] = 's';
    str[4] = 'e';
    str[5] = '\0';
    return str;
  }
}

#ifdef __cplusplus
}
#endif


// List

#ifdef __cplusplus
extern "C" {
#endif

typedef struct MadListNode {
  void *value;
  struct MadListNode *next;
} MadListNode_t;

MadListNode_t *Madlist_empty() {
  MadListNode_t *head = (MadListNode_t *)GC_malloc(sizeof(MadListNode_t));
  head->next = NULL;
  head->value = NULL;
  return head;
}

int64_t MadList_length(MadListNode_t *list) {
  int64_t total = 0;

  if (list->value == NULL) {
    total = 0;
    return total;
  }

  total = 1;

  while (list->next->value != NULL) {
    total += 1;
    list = list->next;
  }

  return total;
}

MadListNode_t *MadList_singleton(void *item) {
  MadListNode_t *head = (MadListNode_t *)GC_malloc(sizeof(MadListNode_t));
  head->next = Madlist_empty();
  head->value = item;

  return head;
}

MadListNode_t *MadList_append(void *item, MadListNode_t *list) {
  if (list->value == NULL) {
    return MadList_singleton(item);
  }

  MadListNode_t *current = list;
  while (current->next != NULL) {
    current = current->next;
  }

  MadListNode_t *nextNode = (MadListNode_t *)GC_malloc(sizeof(MadListNode_t));
  nextNode->next = NULL;
  nextNode->value = item;

  current->next = nextNode;

  return list;
}

MadListNode_t *MadList_push(void *item, MadListNode_t *list) {
  MadListNode_t *newHead = (MadListNode_t *)GC_malloc(sizeof(MadListNode_t));
  newHead->next = list;
  newHead->value = item;

  return newHead;
}

MadListNode_t *__MadList_push__(void *item, MadListNode_t *list) {
  return MadList_push(item, list);
}

MadListNode_t *MadList_map(PAP_t *pap, MadListNode_t *list) {
  if (list->value == NULL) {
    return list;
  }

  MadListNode_t *newList = (MadListNode_t *)GC_malloc(sizeof(MadListNode_t));
  MadListNode_t *head = newList;
  MadListNode_t *current = list;

  while (current->value != NULL) {
    MadListNode_t *nextItem = (MadListNode_t *)GC_malloc(sizeof(MadListNode_t));
    nextItem->value = __applyPAP__(
        pap, 1, current->value);  // cls->fn(cls->env, current->value);
    nextItem->next = NULL;

    newList->next = nextItem;
    newList = newList->next;

    current = current->next;
  }

  return head;
}

void *MadList_nth(double index, MadListNode_t *list) {
  // empty list
  if (list->value == NULL) {
    return NULL;
  }

  int intIndex = floor(index);
  int currentIndex = 0;

  MadListNode_t *current = list;
  while (current->next != NULL && currentIndex < intIndex) {
    current = current->next;
  }

  if (current != NULL) {
    return current->value;
  } else {
    return NULL;
  }
}

bool MadList_hasMinLength(double l, MadListNode_t *list) {
  MadListNode_t *head = list;

  if (head->value == NULL) {
    return l == 0;
  }

  l -= 1;

  while (head->next->value != NULL && l > 0) {
    l -= 1;
    head = head->next;
  }

  return l == 0;
}

bool MadList_hasLength(double l, MadListNode_t *list) {
  MadListNode_t *head = list;

  if (head->value == NULL) {
    return l == 0;
  }

  l -= 1;

  while (head->next->value != NULL && l > 0) {
    l -= 1;
    head = head->next;
  }

  return l == 0 && head->next->value == NULL;
}

MadListNode_t *MadList_concat(MadListNode_t *a, MadListNode_t *b) {
  if (a->value == NULL) {
    return b;
  } else if (b->value == NULL) {
    return a;
  } else {
    MadListNode_t *newList = (MadListNode_t *)GC_malloc(sizeof(MadListNode_t));
    MadListNode_t *head = newList;
    MadListNode_t *current = a;

    newList->value = current->value;
    newList->next = NULL;
    current = current->next;

    while (current->value != NULL) {
      MadListNode_t *nextItem =
          (MadListNode_t *)GC_malloc(sizeof(MadListNode_t));
      nextItem->value = current->value;
      nextItem->next = NULL;

      newList->next = nextItem;
      newList = newList->next;

      current = current->next;
    }

    newList->next = b;
    return head;
  }
}

#ifdef __cplusplus
}
#endif
