#include <math.h>


#ifdef __cplusplus
extern "C" {
#endif

double madlib__math__round(double x) {
  return round(x);
}


double madlib__math__sqrt(double x) {
  return sqrt(x);
}

double madlib__math__ceil(double x) {
  return ceil(x);
}

double madlib__math__floor(double x) {
  return floor(x);
}

double madlib__math__pow(double a, double b) {
  return pow(a, b);
}


#ifdef __cplusplus
}
#endif
