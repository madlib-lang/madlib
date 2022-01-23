#include <gc.h>
#include <stdlib.h>
#include <uv.h>

#include "event-loop.hpp"
#include "apply-pap.hpp"

#ifdef __cplusplus
extern "C" {
#endif

int STDIN = 0;

void madlib__stdio__getLine(PAP_t *callback);

void madlib__stdio__get(PAP_t *callback);

void madlib__stdio__put(char *str);
void madlib__stdio__err(char *str);

#ifdef __cplusplus
}
#endif
