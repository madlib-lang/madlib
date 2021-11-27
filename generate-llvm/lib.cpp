#include <stdlib.h>
#include <string.h>
#include <math.h>
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

// String

#ifdef __cplusplus
extern "C"
{
#endif

  char *__stripTrailingZeros__(char *number)
  {
    int length = strlen(number);
    char *end = number + strlen(number) - 1;
    int charsToRemove = 0;

    while (*end == '0' && charsToRemove < length)
    {
      charsToRemove += 1;
      end -= 1;
    }

    if (*end == '.')
    {
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
extern "C"
{
#endif

  char *__doubleToStr__(double d)
  {
    char *str = (char *)GC_malloc(200);
    sprintf(str, "%.20f", d);
    return __stripTrailingZeros__(str);
  }

  char *__booleanToStr__(bool b)
  {
    if (b)
    {
      char *str = (char *)GC_malloc(5);
      str[0] = 't';
      str[1] = 'r';
      str[2] = 'u';
      str[3] = 'e';
      str[4] = '\0';
      return str;
    }
    else
    {
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
    MadListNode_t *head = (MadListNode_t *)GC_malloc(sizeof(MadListNode_t));
    head->next = NULL;
    head->value = item;

    return head;
  }

  MadListNode_t *MadList_append(void *item, MadListNode_t *list)
  {
    if (list == NULL)
    {
      return MadList_singleton(item);
    }

    MadListNode_t *current = list;
    while (current->next != NULL)
    {
      current = current->next;
    }

    MadListNode_t *nextNode = (MadListNode_t *)GC_malloc(sizeof(MadListNode_t));
    nextNode->next = NULL;
    nextNode->value = item;

    current->next = nextNode;

    return list;
  }

  MadListNode_t *MadList_push(void *item, MadListNode_t *list)
  {
    if (list == NULL)
    {
      return MadList_singleton(item);
    }

    MadListNode_t *newHead = (MadListNode_t *)GC_malloc(sizeof(MadListNode_t));
    newHead->next = list;
    newHead->value = item;

    return newHead;
  }

  MadListNode_t *__MadList_push__(void *item, MadListNode_t *list)
  {
    return MadList_push(item, list);
  }

  typedef void* (*ClosureFn)(void*, void*);

  typedef struct closure
  {
    // void* fn(void*, void*);
    ClosureFn fn;
    void* env;
  } closure_t;
  

  MadListNode_t *MadList_map(closure_t* cls, MadListNode_t *list)
  {
      MadListNode_t *newList = (MadListNode_t *)GC_malloc(sizeof(MadListNode_t));
      MadListNode_t *head = newList;
      MadListNode_t *current = list;

      newList->value = cls->fn(cls->env, current->value);
      newList->next = NULL;
      current = current->next;

      while (current != NULL)
      {
        MadListNode_t *nextItem = (MadListNode_t *)GC_malloc(sizeof(MadListNode_t));
        nextItem->value = cls->fn(cls->env, current->value);
        nextItem->next = NULL;

        newList->next = nextItem;
        newList = newList->next;

        current = current->next;
      }

      return head;
  }

  void *MadList_nth(double index, MadListNode_t *list)
  {
    // empty list
    if (list == NULL)
    {
      return NULL;
    }

    int intIndex = floor(index);
    int currentIndex = 0;

    MadListNode_t *current = list;
    while (current->next != NULL && currentIndex < intIndex)
    {
      current = current->next;
    }

    if (current != NULL)
    {
      return current->value;
    }
    else
    {
      return NULL;
    }
  }

  void *MadList_length(MadListNode_t *list)
  {
    double *total = (double *)GC_malloc(sizeof(double));

    if (list == NULL)
    {
      *total = 0;
      return total;
    }

    *total = 1;

    while (list->next != NULL)
    {
      *total += 1;
      list = list->next;
    }

    return total;
  }

  bool MadList_hasMinLength(double l, MadListNode_t *list)
  {
    MadListNode_t *head = list;
    if (head == NULL)
    {
      return l == 0;
    }

    l -= 1;

    while (head->next != NULL && l > 0)
    {
      l -= 1;
      head = head->next;
    }

    return l == 0;
  }

  bool MadList_hasLength(double l, MadListNode_t *list)
  {
    double *computed = (double*) MadList_length(list);

    return *computed == l;
  }

  MadListNode_t *MadList_concat(MadListNode_t *a, MadListNode_t *b)
  {
    if (a == NULL)
    {
      return b;
    }
    else if (b == NULL)
    {
      return a;
    }
    else
    {
      MadListNode_t *newList = (MadListNode_t *)GC_malloc(sizeof(MadListNode_t));
      MadListNode_t *head = newList;
      MadListNode_t *current = a;

      newList->value = current->value;
      newList->next = NULL;
      current = current->next;

      while (current != NULL)
      {
        MadListNode_t *nextItem = (MadListNode_t *)GC_malloc(sizeof(MadListNode_t));
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
