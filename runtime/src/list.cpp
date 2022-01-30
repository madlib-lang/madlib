#include "list.hpp"

#include <gc.h>

#include <iostream>

// List

#ifdef __cplusplus
extern "C" {
#endif

madlib__list__Node_t *madlib__list__empty() {
  madlib__list__Node_t *head = (madlib__list__Node_t *)GC_malloc(sizeof(madlib__list__Node_t));
  head->next = NULL;
  head->value = NULL;
  return head;
}

int64_t madlib__list__length(madlib__list__Node_t *list) {
  int64_t total = 0;

  while (list->value != NULL) {
    total += 1;
    list = list->next;
  }

  return total;
}

bool *madlib__list__internal__eq(madlib__eq__eqDictionary_t *eqDict, madlib__list__Node_t **l1,
                                 madlib__list__Node_t **l2) {
  bool *boxed = (bool *)GC_malloc(sizeof(bool));

  int64_t l1Length = madlib__list__length(*l1);
  int64_t l2Length = madlib__list__length(*l2);

  if (l1Length != l2Length) {
    *boxed = false;
  } else {
    madlib__list__Node_t *unboxedL1 = *l1;
    madlib__list__Node_t *unboxedL2 = *l2;

    bool result = true;

    for (int i = 0; result && i < l1Length; i++) {
      result = *(bool *)__applyPAP__((void *)&eqDict->eq, 2, unboxedL1->value, unboxedL2->value);
      unboxedL1 = unboxedL1->next;
      unboxedL2 = unboxedL2->next;
    }

    *boxed = result;
  }

  return boxed;
}

char **madlib__list__internal__inspect(madlib__inspect__inspectDictionary_t *inspectDict, madlib__list__Node_t **list) {
  int64_t length = madlib__list__length(*list);

  if (length == 0) {
    char **boxed = (char **)GC_malloc(sizeof(char*));
    *boxed = (char*)"[]";
    return boxed;
  }

  madlib__list__Node_t *unboxedList = *list;
  int currentIndex = 0;
  char *inspectedItems[length];
  size_t sizeOfItems = 0;

  for (int i = 0; i < length; i++) {
    inspectedItems[i] = *(char **)__applyPAP__((void *)&inspectDict->inspect, 1, unboxedList->value);
    sizeOfItems += strlen(inspectedItems[i]);
    unboxedList = unboxedList->next;
  }

  size_t sizeOfSpacesAndCommas = (length - 1) * 2;
  char *result = (char*)GC_malloc(sizeof(char) * (sizeOfItems + sizeOfSpacesAndCommas + 3));

  // Leading "["
  strncpy(result, "[", sizeof(char));
  size_t currentPosition = 1;

  // Items
  for (int i = 0; i < length - 1; i++) {
    size_t lengthOfItem = strlen(inspectedItems[i]);
    strncpy(result + currentPosition, inspectedItems[i], lengthOfItem);
    strncpy(result + currentPosition + lengthOfItem, ", ", sizeof(char) * 2);
    currentPosition += lengthOfItem + 2;
  }

  // Last item does not have ", " at the end
  size_t lengthOfItem = strlen(inspectedItems[length - 1]);
  strncpy(result + currentPosition, inspectedItems[length - 1], lengthOfItem);
  strncpy(result + currentPosition + lengthOfItem, "]\0", sizeof(char) * 2);

  char **boxed = (char **)GC_malloc(sizeof(char*));
  *boxed = result;


  return boxed;
}

madlib__list__Node_t *madlib__list__singleton(void *item) {
  madlib__list__Node_t *head = (madlib__list__Node_t *)GC_malloc(sizeof(madlib__list__Node_t));
  head->next = madlib__list__empty();
  head->value = item;

  return head;
}

// TODO: we need to copy the list here
madlib__list__Node_t *madlib__list__append(void *item, madlib__list__Node_t *list) {
  if (list->value == NULL || list->next == NULL) {
    return madlib__list__singleton(item);
  }

  madlib__list__Node_t *current = list;
  while (current->next->value != NULL) {
    current = current->next;
  }

  madlib__list__Node_t *nextNode = madlib__list__singleton(item);
  current->next = nextNode;

  return list;
}

madlib__list__Node_t *madlib__list__push(void *item, madlib__list__Node_t *list) {
  madlib__list__Node_t *newHead = (madlib__list__Node_t *)GC_malloc(sizeof(madlib__list__Node_t));
  newHead->next = list;
  newHead->value = item;

  return newHead;
}

madlib__list__Node_t *madlib__list__internal__push(void *item, madlib__list__Node_t *list) {
  return madlib__list__push(item, list);
}


madlib__list__Node_t *madlib__list__map(PAP_t *pap, madlib__list__Node_t *list) {
  size_t itemCount = madlib__list__length(list);
  int nodesIndex = 1;
  madlib__list__Node_t *nodes = (madlib__list__Node_t*)GC_malloc(sizeof(madlib__list__Node_t) * (itemCount + 1));
  madlib__list__Node_t *newList = nodes;
  madlib__list__Node_t *head = newList;

  while (list->value != NULL) {
    madlib__list__Node_t *nextItem = nodes + nodesIndex;
    nextItem->value = NULL;
    nextItem->next = NULL;

    newList->value = __applyPAP__(pap, 1, list->value);
    newList->next = nextItem;

    newList = newList->next;
    list = list->next;
    nodesIndex += 1;
  }

  return head;
}


void *madlib__list__reduce(PAP_t *pap, void *initialValue, madlib__list__Node_t *list) {
  while (list->value != NULL) {
    initialValue = __applyPAP__(pap, 2, initialValue, list->value);
    list = list->next;
  }

  return initialValue;
}

void *madlib__list__nth(double index, madlib__list__Node_t *list) {
  // empty list
  if (list->value == NULL) {
    // TODO: make it return Nothing ( { 1 } )
    return NULL;
  }

  int intIndex = floor(index);
  int currentIndex = 0;

  while (list->value != NULL && currentIndex < intIndex) {
    list = list->next;
  }

  if (list->value != NULL) {
    return list->value;
  } else {
    // TODO: make it return Nothing ( { 1 } )
    return NULL;
  }
}

bool madlib__list__internal__hasMinLength(int64_t l, madlib__list__Node_t *list) {
  while (list->value != NULL && l > 0) {
    l -= 1;
    list = list->next;
  }

  return l == 0;
}

bool madlib__list__internal__hasLength(int64_t l, madlib__list__Node_t *list) {
  while (list->value != NULL && l > 0) {
    l -= 1;
    list = list->next;
  }

  return l == 0 && list->value == NULL;
}

madlib__list__Node_t *madlib__list__concat(madlib__list__Node_t *a, madlib__list__Node_t *b) {
  if (a->value == NULL) {
    return b;
  } else if (b->value == NULL) {
    return a;
  } else {
    madlib__list__Node_t *newList = (madlib__list__Node_t *)GC_malloc(sizeof(madlib__list__Node_t));
    madlib__list__Node_t *head = newList;
    madlib__list__Node_t *current = a;

    newList->value = current->value;
    newList->next = NULL;
    current = current->next;

    while (current->value != NULL) {
      madlib__list__Node_t *nextItem = (madlib__list__Node_t *)GC_malloc(sizeof(madlib__list__Node_t));
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

madlib__list__Node_t *madlib__list__internal__copy(madlib__list__Node_t *list) {
  if (list->value == NULL) {
    return madlib__list__empty();
  }

  madlib__list__Node_t *newList = (madlib__list__Node_t *)GC_malloc(sizeof(madlib__list__Node_t));
  madlib__list__Node_t *head = newList;
  madlib__list__Node_t *current = list;

  while (current->value != NULL) {
    madlib__list__Node_t *nextItem = madlib__list__empty();

    newList->value = current->value;
    newList->next = nextItem;

    newList = newList->next;
    current = current->next;
  }

  return head;
}

// Sort

/**
 * currently a bubble sort algorithm, needs to be improved at some point
 */
madlib__list__Node_t *madlib__list__sort(PAP_t *compare, madlib__list__Node_t *list) {
  madlib__list__Node_t *copy = madlib__list__internal__copy(list);
  bool hadPermutation = true;

  while (hadPermutation != false) {
    madlib__list__Node_t *head = copy;
    hadPermutation = false;

    while (head->value != NULL) {
      if (head->next->value != NULL) {
        int64_t comparisonResult = *(int64_t *)__applyPAP__((void *)compare, 2, head->value, head->next->value);

        // If the current item is bigger than the next one we need to swap them
        if (comparisonResult == 1) {
          void *value = head->value;
          head->value = head->next->value;
          head->next->value = value;
          hadPermutation = true;
        }
      }
      head = head->next;
    }
  }

  return copy;
}

#ifdef __cplusplus
}
#endif
