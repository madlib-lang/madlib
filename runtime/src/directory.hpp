#ifndef DIRECTORY_H
#define DIRECTORY_H

#include <gc.h>
#include <uv.h>
#include "apply-pap.hpp"

#ifdef __cplusplus
extern "C" {
#endif

uv_fs_t *madlib__directory__read(char *filepath, PAP_t *callback);
void madlib__directory__cancelRead(uv_fs_t *req);

#ifdef __cplusplus
}
#endif

#endif // DIRECTORY_H
