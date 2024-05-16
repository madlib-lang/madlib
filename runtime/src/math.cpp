#include <math.h>
#include <random>
#include <ctime>


std::default_random_engine __makeGenerator__() {
  int SEED = time(0);
  std::default_random_engine generator;
  generator.seed(SEED);
  return generator;
}

static std::default_random_engine __generator__ = __makeGenerator__();

double madlib__math__random__cpp() {
  std::uniform_real_distribution<double> unif(0, 1);
  return  unif(__generator__);
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

double madlib__math__sin(double a) {
  return sin(a);
}

double madlib__math__cos(double a) {
  return cos(a);
}

double madlib__math__tan(double a) {
  return tan(a);
}

double madlib__math__atan(double a) {
  return atan(a);
}

double madlib__math__atan2(double a, double b) {
  return atan2(a, b);
}


#ifdef __cplusplus
}
#endif
