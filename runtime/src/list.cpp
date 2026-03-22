
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
  if (a->value == NULL && a->next == NULL) {
    return b;
  } else if (b->value == NULL && b->next == NULL) {
    return a;
  } else {
    size_t aLen = madlib__list__length(a);
    madlib__list__Node_t *nodes = (madlib__list__Node_t *)GC_MALLOC(
        sizeof(madlib__list__Node_t) * aLen);
    madlib__list__Node_t *current = a;

    for (size_t i = 0; i < aLen; i++) {
      nodes[i].value = current->value;
      nodes[i].next = (i + 1 < aLen) ? &nodes[i + 1] : b;
      current = current->next;
    }

    return nodes;
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

  size_t len = madlib__list__length(list);
  madlib__list__Node_t *nodes = (madlib__list__Node_t *)GC_MALLOC(
      sizeof(madlib__list__Node_t) * (len + 1));
  madlib__list__Node_t *current = list;

  for (size_t i = 0; i < len; i++) {
    nodes[i].value = current->value;
    nodes[i].next = &nodes[i + 1];
    current = current->next;
  }
  // sentinel empty node
  nodes[len].value = NULL;
  nodes[len].next = NULL;

  return nodes;
}


#ifdef __cplusplus
}
#endif
