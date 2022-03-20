#include <math.h>


#ifdef __cplusplus
extern "C" {
#endif

int64_t madlib__math__round(double x) {
  return round(x);
}


double madlib__math__sqrt(double x) {
  return sqrt(x);
}

int64_t madlib__math__ceil(double x) {
  return ceil(x);
}

int64_t madlib__math__floor(double x) {
  return floor(x);
}


#ifdef __cplusplus
}
#endif
