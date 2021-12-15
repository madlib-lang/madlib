#include "apply-pap.hpp"

typedef struct MadListNode {
  void *value;
  struct MadListNode *next;
} MadListNode_t;

typedef struct EqDictionary {
  PAP_t eq;
} EqDictionary_t;

#ifdef __cplusplus
extern "C" {
#endif

MadListNode_t *MadList_empty();

int64_t MadList_length(MadListNode_t *list);

bool *__eqList__(EqDictionary_t* eqDict, MadListNode **l1, MadListNode **l2);

MadListNode_t *MadList_singleton(void *item);

MadListNode_t *MadList_append(void *item, MadListNode_t *list);

MadListNode_t *MadList_push(void *item, MadListNode_t *list);

MadListNode_t *__MadList_push__(void *item, MadListNode_t *list);

MadListNode_t *MadList_map(PAP_t *pap, MadListNode_t *list);

void *MadList_nth(double index, MadListNode_t *list);

bool MadList_hasMinLength(int64_t l, MadListNode_t *list);

bool MadList_hasLength(int64_t l, MadListNode_t *list);

MadListNode_t *MadList_concat(MadListNode_t *a, MadListNode_t *b);

void *MadList_reduce(PAP_t *pap, void *initialValue, MadListNode_t *list);

#ifdef __cplusplus
}
#endif
