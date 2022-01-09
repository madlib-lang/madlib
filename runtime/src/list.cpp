#include <gc.h>
#include <iostream>

#include "list.hpp"


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


bool *madlib__internal__list__eq(EqDictionary_t* eqDict, madlib__list__Node_t **l1, madlib__list__Node_t **l2) {
  bool *boxed = (bool*) GC_malloc(sizeof(bool));

  int64_t l1Length = madlib__list__length(*l1);
  int64_t l2Length = madlib__list__length(*l2);

  if (l1Length != l2Length) {
    *boxed = false;
  } else {
    madlib__list__Node_t *unboxedL1 = *l1;
    madlib__list__Node_t *unboxedL2 = *l2;

    bool result = true;

    for (int i=0; result && i < l1Length; i++) {
      result = *(bool*)__applyPAP__((void*)&eqDict->eq, 2, unboxedL1->value, unboxedL2->value);
      unboxedL1 = unboxedL1->next;
      unboxedL2 = unboxedL2->next;
    }

    *boxed = result;
  }

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
  madlib__list__Node_t *newList = (madlib__list__Node_t *)GC_malloc(sizeof(madlib__list__Node_t));
  madlib__list__Node_t *head = newList;

  while (list->value != NULL) {
    madlib__list__Node_t *nextItem = madlib__list__empty();

    newList->value = __applyPAP__(pap, 1, list->value);
    newList->next = nextItem;

    newList = newList->next;
    list = list->next;
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

bool madlib__internal__list__hasMinLength(int64_t l, madlib__list__Node_t *list) {
  while (list->value != NULL && l > 0) {
    l -= 1;
    list = list->next;
  }

  return l == 0;
}

bool madlib__internal__list__hasLength(int64_t l, madlib__list__Node_t *list) {
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
      madlib__list__Node_t *nextItem =
          (madlib__list__Node_t *)GC_malloc(sizeof(madlib__list__Node_t));
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


madlib__list__Node_t *madlib__internal__list__copy(madlib__list__Node_t *list) {
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
  madlib__list__Node_t *copy = madlib__internal__list__copy(list);
  bool hadPermutation = true;

  while (hadPermutation != false) {
    madlib__list__Node_t *head = copy;
    hadPermutation = false;

    while (head->value != NULL) {
      if (head->next->value != NULL) {
        int64_t comparisonResult = *(int64_t*)__applyPAP__((void*)compare, 2, head->value, head->next->value);

        // If the current item is bigger than the next one we need to swap them
        if (comparisonResult == 1) {
          void* value = head->value;
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
