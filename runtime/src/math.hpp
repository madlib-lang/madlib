#ifndef MATH_H
#define MATH_H

#include <iostream>


#ifdef __cplusplus
extern "C" {
#endif

double madlib__math__round(double x);
double madlib__math__ceil(double x);
double madlib__math__floor(double x);

double madlib__math__sqrt(double x);

double madlib__math__pow(double a, double b);

double madlib__math__random();

double madlib__math__atan(double a);
double madlib__math__atan2(double a, double b);

#ifdef __cplusplus
}
#endif

#endif // MATH_H
