#ifndef COMPARABLE_H
#define COMPARABLE_H

// int64_t LESS = -1;
// int64_t EQUAL = 0;
// int64_t MORE = 1;

#ifdef __cplusplus
extern "C" {
#endif

typedef struct madlib__comparable__comparableDictionary {
  PAP_t compare;
} madlib__comparable__comparableDictionary_t;

#ifdef __cplusplus
}
#endif

#endif // COMPARABLE_H
