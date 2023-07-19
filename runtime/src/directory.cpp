#include "directory.hpp"
#include <string.h>
#include "event-loop.hpp"
#include "list.hpp"

#ifdef __cplusplus
extern "C" {
#endif


void onDirScan(uv_fs_t *req) {
  void *callback = req->data;
  madlib__list__Node_t *result = madlib__list__empty();

  if (req->result >= 0) {
    uv_dirent_t dirh;

    while (uv_fs_scandir_next(req, &dirh) != UV_EOF) {
        size_t itemLength = strlen(dirh.name);
        char *item = (char*)GC_MALLOC_ATOMIC(sizeof(char) * (itemLength + 1));

        strncpy(item, dirh.name, itemLength);
        item[itemLength] = '\0';

        result = madlib__list__internal__append(item, result);
    }

    int64_t *boxedError = (int64_t*)0;

    GC_FREE(req);

    __applyPAP__(callback, 2, boxedError, result);
  } else {
    int64_t *boxedError = (int64_t*)libuvErrorToMadlibIOError(req->result);

    GC_FREE(req);

    __applyPAP__(callback, 2, boxedError, result);
  }
}


uv_fs_t *madlib__directory__read(char *dir, PAP_t *callback) {
  uv_fs_t *req = (uv_fs_t *)GC_MALLOC(sizeof(uv_fs_t));
  req->data = callback;
  uv_fs_scandir(getLoop(), req, dir, 0, onDirScan);

  return req;
}

void madlib__directory__cancelRead(uv_fs_t *req) {
  uv_fs_t closeReq;
  uv_fs_close(getLoop(), &closeReq, req->result, NULL);
}

#ifdef __cplusplus
}
#endif
