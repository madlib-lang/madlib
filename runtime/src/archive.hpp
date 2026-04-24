#ifndef ARCHIVE_H
#define ARCHIVE_H

#include <uv.h>

#include "apply-pap.hpp"

#ifdef __cplusplus
extern "C" {
#endif

uv_work_t *madlib__archive__zip(char *sourceDirectoryPath, char *archivePath, PAP_t *callback);
void madlib__archive__cancelZip(uv_work_t *req);

uv_work_t *madlib__archive__unzip(char *archivePath, char *destinationDirectoryPath, PAP_t *callback);
void madlib__archive__cancelUnzip(uv_work_t *req);

#ifdef __cplusplus
}
#endif

#endif // ARCHIVE_H
