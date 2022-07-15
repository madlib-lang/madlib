#ifndef DATE_H
#define DATE_H
#include <stdint.h>
#include "record.hpp"


#ifdef __cplusplus
extern "C" {
#endif

int64_t madlib__date__now();

char *madlib__date__toISOString(int64_t epochMilliseconds);

madlib__record__Record_t *madlib__date__toDateInfo(int64_t epochMilliseconds);
int64_t madlib__date__fromDateInfo(madlib__record__Record_t *dateInfo);

#ifdef __cplusplus
}
#endif

#endif // DATE_H
