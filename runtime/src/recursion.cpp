#include <stdio.h>
#include <gc.h>

#include "apply-pap.hpp"

#ifdef __cplusplus
extern "C" {
#endif



static int NEXT = 0x01;
static int DONE = 0x02;
static int THUNK = 0x03;

typedef struct Trampoline {
  int tag;
  void *data;
} Trampoline_t;

void *id(void *x) {
  return x;
}

void *madlib__recursion__internal__trampoline__1(PAP_t *fn, void *arg) {
  int one = 1;
  PAP_t idPAP = {
    .fn = (void*)id,
    .arity = 1,
    .missingArgCount = 1,
    .env = NULL
  };

  Trampoline_t *v = (Trampoline_t*)__applyPAP__(fn, 2, arg, &idPAP);

  while (v->tag == NEXT) {
    v = (Trampoline_t*)__applyPAP__(v->data, 1, NULL);
  }

  v = (Trampoline_t*)v->data;

  while (v->tag == THUNK) {
    // printf("tag: %d\n", v->tag);
    // printf("data: %d\n", v->data);
    v = (Trampoline_t*)__applyPAP__(v->data, 1, &one);
  }

  return (void*)v;
}

void *madlib__recursion__internal__Next(PAP_t *next) {
  Trampoline_t *trampoline = (Trampoline_t*)GC_malloc(sizeof(Trampoline_t));
  trampoline->data = next;
  trampoline->tag = NEXT;

  return trampoline;
}

void *madlib__recursion__internal__Thunk(PAP_t *next) {
  Trampoline_t *trampoline = (Trampoline_t*)GC_malloc(sizeof(Trampoline_t));
  trampoline->data = next;
  trampoline->tag = THUNK;

  return trampoline;
}

void *madlib__recursion__internal__Done(void *result) {
  Trampoline_t *trampoline = (Trampoline_t*)GC_malloc(sizeof(Trampoline_t));
  trampoline->data = result;
  trampoline->tag = DONE;

  return trampoline;
}

#ifdef __cplusplus
}
#endif
