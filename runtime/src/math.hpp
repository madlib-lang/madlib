#ifndef MATH_H
#define MATH_H

#include <iostream>


#ifdef __cplusplus
extern "C" {
#endif

int64_t madlib__math__round(double x);
int64_t madlib__math__ceil(double x);
int64_t madlib__math__floor(double x);

double madlib__math__sqrt(double x);

#ifdef __cplusplus
}
#endif

#endif // MATH_H
