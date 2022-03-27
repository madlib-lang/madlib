#include <gc.h>
#include <string.h>

#include "event-loop.hpp"
#include "list.hpp"
#include "apply-pap.hpp"

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
        char **boxedItem = (char**)GC_malloc(sizeof(char*));
        *boxedItem = (char*)GC_malloc(sizeof(char) * (itemLength + 1));

        strncpy(*boxedItem, dirh.name, itemLength);
        (*boxedItem)[itemLength] = '\0';

        result = madlib__list__internal__append(boxedItem, result);
    }

    madlib__list__Node_t** boxedList = (madlib__list__Node_t**)GC_malloc(sizeof(madlib__list__Node_t*));
    *boxedList = result;

    int64_t *boxedError = (int64_t*)GC_malloc(sizeof(int64_t));
    *boxedError = 0;

    uv_fs_req_cleanup(req);
    GC_free(req);

    __applyPAP__(callback, 2, boxedError, boxedList);
  } else {
    int64_t *boxedError = (int64_t*)GC_malloc(sizeof(int64_t));
    *boxedError = libuvErrorToMadlibIOError(req->result);

    madlib__list__Node_t** boxedList = (madlib__list__Node_t**)GC_malloc(sizeof(madlib__list__Node_t*));
    *boxedList = result;

    uv_fs_req_cleanup(req);
    GC_free(req);

    __applyPAP__(callback, 2, boxedError, boxedList);
  }
}


void madlib__directory__read(char *dir, PAP_t *callback) {
  uv_fs_t *req = (uv_fs_t *)GC_malloc_uncollectable(sizeof(uv_fs_t));
  req->data = callback;
  uv_fs_scandir(getLoop(), req, dir, 0, onDirScan);
}

#ifdef __cplusplus
}
#endif
