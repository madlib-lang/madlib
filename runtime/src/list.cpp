
#include <gc.h>
#include "list.hpp"

#include <string.h>
#include <iostream>


#ifdef __cplusplus
extern "C" {
#endif

madlib__list__Node_t *madlib__list__empty() {
  madlib__list__Node_t *head =
      (madlib__list__Node_t *)GC_MALLOC(sizeof(madlib__list__Node_t));
  head->next = NULL;
  head->value = NULL;
  return head;
}

int64_t madlib__list__length(madlib__list__Node_t *list) {
  int64_t total = 0;

  while (list->next != NULL) {
    total += 1;
    list = list->next;
  }

  return total;
}

bool madlib__list__internal__eq(madlib__eq__eqDictionary_t *eqDict,
                                madlib__list__Node_t *l1,
                                madlib__list__Node_t *l2) {
  int64_t l1Length = madlib__list__length(l1);
  int64_t l2Length = madlib__list__length(l2);

  if (l1Length != l2Length) {
    return false;
  }


  bool result = true;

  for (int i = 0; result && i < l1Length; i++) {
    result = (bool)__applyPAP__((void *)&eqDict->eq, 2, l1->value, l2->value);
    l1 = l1->next;
    l2 = l2->next;
  }

  return result;
}

char *madlib__list__internal__inspect(
    madlib__inspect__inspectDictionary_t *inspectDict,
    madlib__list__Node_t *list) {
  int64_t length = madlib__list__length(list);

  if (length == 0) {
    return (char *)"[]";
  }

  int currentIndex = 0;
  char **inspectedItems =
      (char **)GC_MALLOC(sizeof(char *) * length);
  size_t sizeOfItems = 0;
  void *inspect = &inspectDict->inspect;

  for (int i = 0; i < length; i++) {
    inspectedItems[i] = (char *)__applyPAP__(inspect, 1, list->value);
    sizeOfItems += strlen(inspectedItems[i]);
    list = list->next;
  }

  size_t sizeOfSpacesAndCommas = (length - 1) * 2;
  char *result = (char *)GC_MALLOC_ATOMIC(sizeof(char) * (sizeOfItems + sizeOfSpacesAndCommas + 3));

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

  // GC_free(inspectedItems);

  return result;
}

madlib__list__Node_t *madlib__list__singleton(void *item) {
  madlib__list__Node_t *head =
      (madlib__list__Node_t *)GC_MALLOC(sizeof(madlib__list__Node_t));
  head->next = madlib__list__empty();
  head->value = item;

  return head;
}

madlib__list__Node_t *madlib__list__push(void *item,
                                         madlib__list__Node_t *list) {
  madlib__list__Node_t *newHead =
      (madlib__list__Node_t *)GC_MALLOC(sizeof(madlib__list__Node_t));
  newHead->next = list;
  newHead->value = item;

  return newHead;
}

madlib__list__Node_t *madlib__list__internal__push(void *item,
                                                   madlib__list__Node_t *list) {
  madlib__list__Node_t *newHead =
      (madlib__list__Node_t *)GC_MALLOC(sizeof(madlib__list__Node_t));
  newHead->next = list;
  newHead->value = item;

  return newHead;
}

madlib__list__Node_t *madlib__list__map(PAP_t *pap,
                                        madlib__list__Node_t *list) {
  size_t itemCount = madlib__list__length(list);
  int nodesIndex = 1;
  madlib__list__Node_t *nodes = (madlib__list__Node_t *)GC_MALLOC(
      sizeof(madlib__list__Node_t) * (itemCount + 1));
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

void *madlib__list__reduce(PAP_t *pap, void *initialValue,
                           madlib__list__Node_t *list) {
  while (list->value != NULL) {
    initialValue = __applyPAP__(pap, 2, initialValue, list->value);
    list = list->next;
  }

  return initialValue;
}

bool madlib__list__internal__hasMinLength(int64_t l,
                                          madlib__list__Node_t *list) {
  while (list->next != NULL && l > 0) {
    l -= 1;
    list = list->next;
  }

  return l == 0;
}

bool madlib__list__internal__hasLength(int64_t l, madlib__list__Node_t *list) {
  while (list->next != NULL && l > 0) {
    l -= 1;
    list = list->next;
  }

  return l == 0 && list->next == NULL;
}

madlib__list__Node_t *madlib__list__concat(madlib__list__Node_t *a,
                                           madlib__list__Node_t *b) {
  if (a->value == NULL) {
    return b;
  } else if (b->value == NULL) {
    return a;
  } else {
    madlib__list__Node_t *newList =
        (madlib__list__Node_t *)GC_MALLOC(sizeof(madlib__list__Node_t));
    madlib__list__Node_t *head = newList;
    madlib__list__Node_t *current = a;

    newList->value = current->value;
    newList->next = NULL;
    current = current->next;

    while (current->next != NULL) {
      madlib__list__Node_t *nextItem =
          (madlib__list__Node_t *)GC_MALLOC(sizeof(madlib__list__Node_t));
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

madlib__list__Node_t *madlib__list__internal__append(
    void *item, madlib__list__Node_t *list) {
  madlib__list__Node_t *copy = madlib__list__internal__copy(list);

  if (copy->next == NULL || list->next == NULL) {
    return madlib__list__singleton(item);
  }

  madlib__list__Node_t *current = copy;
  while (current->next->value != NULL) {
    current = current->next;
  }

  madlib__list__Node_t *nextNode = madlib__list__singleton(item);
  current->next = nextNode;

  return copy;
}

madlib__list__Node_t *madlib__list__internal__copy(madlib__list__Node_t *list) {
  if (list->value == NULL) {
    return madlib__list__empty();
  }

  madlib__list__Node_t *newList =
      (madlib__list__Node_t *)GC_MALLOC(sizeof(madlib__list__Node_t));
  madlib__list__Node_t *head = newList;
  madlib__list__Node_t *current = list;

  while (current->next != NULL) {
    madlib__list__Node_t *nextItem = madlib__list__empty();

    newList->value = current->value;
    newList->next = nextItem;

    newList = newList->next;
    current = current->next;
  }

  return head;
}


#ifdef __cplusplus
}
#endif
