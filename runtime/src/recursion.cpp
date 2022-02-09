#include <stdio.h>
#include <gc.h>

#include "apply-pap.hpp"

#ifdef __cplusplus
extern "C" {
#endif

static int NEXT = 0x00;
static int DONE = 0x01;
static int THUNK = 0x02;

typedef struct Trampoline {
  int tag;
  void *data;
} Trampoline_t;

void *id(void *x) {
  return x;
}

void *madlib__recursion__internal__trampoline__1(PAP_t *fn, void *arg) {
  printf("trampo_1\n");
  PAP_t idPAP = {
    .fn = (void*)id,
    .arity = 1,
    .missingArgCount = 1,
    .env = NULL
  };
  Trampoline_t *v = (Trampoline_t*)__applyPAP__(fn, 2, arg, &idPAP);
//   printf("trampo tag: %d\n", v->tag);
  while (v->tag == NEXT) {
    v = (Trampoline_t*)__applyPAP__(v->data, 1, NULL);
    // printf("trampo tag: %d\n", v->tag);
  }
  return NULL;
}

void *madlib__recursion__internal__Next(PAP_t *next) {
//   printf("next\n");

  Trampoline_t *trampoline = (Trampoline_t*)GC_malloc(sizeof(Trampoline_t));
  trampoline->data = next;
  trampoline->tag = NEXT;

  return trampoline;
}

#ifdef __cplusplus
}
#endif
