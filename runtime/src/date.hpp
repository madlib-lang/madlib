#ifndef DATE_H
#define DATE_H
#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif

int64_t madlib__date__now();

char *madlib__date__toISOString(int64_t epochMilliseconds);

// DateInfo is a flat struct of 7 boxed fields sorted alphabetically:
// [0]=day, [1]=hours, [2]=milliseconds, [3]=minutes, [4]=month, [5]=seconds, [6]=year
void **madlib__date__toDateInfo(int64_t epochMilliseconds);
int64_t madlib__date__fromDateInfo(void **dateInfo);

#ifdef __cplusplus
}
#endif

#endif // DATE_H
