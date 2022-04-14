#ifndef LIST_H
#define LIST_H

#include "eq.hpp"
#include "inspect.hpp"
#include "apply-pap.hpp"
#include <stdint.h>

typedef struct madlib__list__Node {
  void *value;
  struct madlib__list__Node *next;
} madlib__list__Node_t;


#ifdef __cplusplus
extern "C" {
#endif

madlib__list__Node_t *madlib__list__empty();

int64_t madlib__list__length(madlib__list__Node_t *list);

char *madlib__list__internal__inspect(madlib__inspect__inspectDictionary_t *inspectDict, madlib__list__Node_t *list);
bool madlib__list__internal__eq(madlib__eq__eqDictionary_t* eqDict, madlib__list__Node_t *l1, madlib__list__Node_t *l2);

madlib__list__Node_t *madlib__list__singleton(void *item);

madlib__list__Node_t *madlib__list__append(void *item, madlib__list__Node_t *list);

madlib__list__Node_t *madlib__list__push(void *item, madlib__list__Node_t *list);

madlib__list__Node_t *madlib__list__internal__push(void *item, madlib__list__Node_t *list);

madlib__list__Node_t *madlib__list__map(PAP_t *pap, madlib__list__Node_t *list);

bool madlib__list__internal__hasMinLength(int64_t l, madlib__list__Node_t *list);

bool madlib__list__internal__hasLength(int64_t l, madlib__list__Node_t *list);

madlib__list__Node_t *madlib__list__concat(madlib__list__Node_t *a, madlib__list__Node_t *b);

void *madlib__list__reduce(PAP_t *pap, void *initialValue, madlib__list__Node_t *list);

madlib__list__Node_t *madlib__list__internal__append(void *item, madlib__list__Node_t *list);

madlib__list__Node_t *madlib__list__internal__copy(madlib__list__Node_t *list);

madlib__list__Node_t *madlib__list__sort(PAP_t *compare, madlib__list__Node_t *list);

#ifdef __cplusplus
}
#endif

#endif // LIST_H
