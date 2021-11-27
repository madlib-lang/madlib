#include <gc.h>
#include <iostream>

#include "apply-pap.hpp"


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



typedef struct EqDictionary {
  PAP_t eq;
} EqDictionary_t;

bool *__eqList__(EqDictionary_t* eqDict, MadListNode **l1, MadListNode **l2) {
  bool *boxed = (bool*) GC_malloc(sizeof(bool));

  int64_t l1Length = MadList_length(*l1);
  int64_t l2Length = MadList_length(*l2);
  
  if (l1Length != l2Length) {
    *boxed = false;
  } else {
    MadListNode *unboxedL1 = *l1;
    MadListNode *unboxedL2 = *l2;

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
