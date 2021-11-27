#include <stdio.h>
#include <stdlib.h>

double *addFloats(double *a, double *b) {
  double *result = (double*) malloc(sizeof(double));
  *result = *a + *b;
  return result;
}

double* toFloat(long *l) {
  double *result = (double*) malloc(sizeof(double));
  *result = (double) *l;

  return result;
}

void *inc(void *(*add)(void *, void *), void *a) {
  long *one = (long*) malloc(sizeof(long));
  *one = 1L;
  return add(a, toFloat(one));
}

int main() {
  double x = 1.5;
  double *result = (double *)inc((void*(*)(void*,  void*))addFloats, (void *) &x);
  printf("%f", *result);

  return 0;
}
