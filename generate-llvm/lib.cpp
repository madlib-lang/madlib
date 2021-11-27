#include <stdlib.h>
#include <string.h>
#include <iostream>
#include <gc.h>

// __streq__
#ifdef __cplusplus
extern "C"
{
#endif

  bool __streq__(char *s1, char *s2)
  {
    if (strcmp(s1, s2) == 0)
    {
      return true;
    }
    else
    {
      return false;
    }
  }

#ifdef __cplusplus
}
#endif

// Show
#ifdef __cplusplus
extern "C"
{
#endif

  void* __doubleToStr__(double d) {
    char *str = (char*) GC_malloc(200);
    sprintf(str,"%lf", d);
    return str;
  }

#ifdef __cplusplus
}
#endif


// List

#ifdef __cplusplus
extern "C"
{
#endif

  typedef struct MadListNode
  {
    void *value;
    struct MadListNode *next;
  } MadListNode_t;

  MadListNode_t *MadList_singleton(void *item)
  {
    MadListNode_t *head = (MadListNode_t *)malloc(sizeof(MadListNode_t));
    head->next = NULL;
    head->value = item;

    return head;
  }

  MadListNode_t *MadList_append(void *item, MadListNode_t *list)
  {
    MadListNode_t *current = list;
    while (current->next != NULL)
    {
      current = current->next;
    }

    MadListNode_t *nextNode = (MadListNode_t *)malloc(sizeof(MadListNode_t));
    nextNode->next = NULL;
    nextNode->value = item;

    current->next = nextNode;

    return list;
  }

#ifdef __cplusplus
}
#endif
