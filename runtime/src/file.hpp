#ifndef FILESYSTEM_H
#define FILESYSTEM_H

#include "apply-pap.hpp"
#include "bytearray.hpp"

#ifdef __cplusplus
extern "C" {
#endif

void madlib__file__read(char *filepath, PAP_t *callback);

void madlib__file__readBytes(char *filepath, PAP_t *callback);

void madlib__file__write(char *filepath, char *content, PAP_t *callback);

void madlib__file__writeBytes(char *filepath, madlib__bytearray__ByteArray_t *content, PAP_t *callback);

void madlib__file__exists(char *filepath, PAP_t *callback);

#ifdef __cplusplus
}
#endif

#endif // FILESYSTEM_H
