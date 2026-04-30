#ifndef DIRECTORY_H
#define DIRECTORY_H

#include <gc.h>
#include <uv.h>
#include "apply-pap.hpp"
#include "list.hpp"

#ifdef __cplusplus
extern "C" {
#endif

uv_fs_t *madlib__directory__read(char *filepath, PAP_t *callback);
void madlib__directory__cancelRead(uv_fs_t *req);

uv_work_t *madlib__directory__create(char *path, bool recursive, PAP_t *callback);
void madlib__directory__cancelCreate(uv_work_t *req);

uv_work_t *madlib__directory__remove(char *path, bool recursive, PAP_t *callback);
void madlib__directory__cancelRemove(uv_work_t *req);

uv_work_t *madlib__directory__copy(
  char *sourcePath,
  char *destinationPath,
  bool recursive,
  bool overwrite,
  madlib__list__Node_t *exclude,
  PAP_t *callback
);
void madlib__directory__cancelCopy(uv_work_t *req);

uv_work_t *madlib__directory__rename(char *sourcePath, char *destinationPath, PAP_t *callback);
void madlib__directory__cancelRename(uv_work_t *req);

#ifdef __cplusplus
}
#endif

#endif // DIRECTORY_H
