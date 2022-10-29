#include <math.h>
#include <random>

static const int SEED = time(0);


double madlib__math__random__cpp() {
  std::default_random_engine generator;
  generator.seed(SEED);
  std::uniform_real_distribution<double> unif(0, 1);
  return  unif(generator);
}

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

double madlib__math__random() {
  return madlib__math__random__cpp();
}


#ifdef __cplusplus
}
#endif
