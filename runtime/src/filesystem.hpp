#ifndef FILESYSTEM_H
#define FILESYSTEM_H

#include "apply-pap.hpp"
#include "array.hpp"

#ifdef __cplusplus
extern "C" {
#endif

void madlib__filesystem__readFile(char *filepath, PAP_t *callback);

void madlib__filesystem__readBinaryFile(char *filepath, PAP_t *callback);

void madlib__filesystem__writeFile(char *filepath, char *content, PAP_t *callback);

void madlib__filesystem__writeBinaryFile(char *filepath, madlib__array__Array_t *content, PAP_t *callback);

void madlib__filesystem__fileExists(char *filepath, PAP_t *callback);

#ifdef __cplusplus
}
#endif

#endif // FILESYSTEM_H
