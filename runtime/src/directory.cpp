#include "directory.hpp"

#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <cctype>
#include <string>
#include <vector>

#ifdef _WIN32
#include <direct.h>
#endif

#include "event-loop.hpp"
#include "list.hpp"
#include "string_header.hpp"

typedef struct DirectoryCreateData {
  void *callback;
  char *path;
  bool recursive;
  int result;
  bool canceled;
} DirectoryCreateData_t;


static bool isPathSeparator(char c) {
  return c == '/' || c == '\\';
}


static bool isRootPath(const std::string &path) {
#ifdef _WIN32
  if (path.size() == 2 && std::isalpha((unsigned char)path[0]) && path[1] == ':') {
    return true;
  }

  if (path.size() == 3 && std::isalpha((unsigned char)path[0]) && path[1] == ':' && isPathSeparator(path[2])) {
    return true;
  }
#endif

  return path == "/";
}


static int translateErrnoToLibuv() {
  int uvErr = uv_translate_sys_error(errno);
  return uvErr == 0 ? UV_EIO : uvErr;
}


static int pathIsDirectory(const char *path, bool *isDir) {
  struct stat statBuffer;
  int statResult = stat(path, &statBuffer);
  if (statResult != 0) {
    return translateErrnoToLibuv();
  }

  *isDir = S_ISDIR(statBuffer.st_mode);
  return 0;
}


static int createSingleDirectory(const char *path) {
#ifdef _WIN32
  int result = _mkdir(path);
#else
  int result = mkdir(path, 0777);
#endif

  if (result == 0) {
    return 0;
  }

  if (errno == EEXIST) {
    bool isDir = false;
    int statResult = pathIsDirectory(path, &isDir);
    if (statResult != 0) {
      return statResult;
    }

    return isDir ? 0 : UV_EEXIST;
  }

  return translateErrnoToLibuv();
}


static int createDirectoryRecursively(const char *path) {
  if (path == NULL || path[0] == '\0') {
    return UV_EINVAL;
  }

  std::string normalizedPath(path);

  while (normalizedPath.size() > 1 && isPathSeparator(normalizedPath.back()) && !isRootPath(normalizedPath)) {
    normalizedPath.pop_back();
  }

  if (normalizedPath.empty()) {
    return UV_EINVAL;
  }

  std::vector<char> mutablePath(normalizedPath.begin(), normalizedPath.end());
  mutablePath.push_back('\0');

  size_t len = normalizedPath.size();
  size_t start = 0;

#ifdef _WIN32
  if (len >= 2 && std::isalpha((unsigned char)mutablePath[0]) && mutablePath[1] == ':') {
    start = 2;
    if (len >= 3 && isPathSeparator(mutablePath[2])) {
      start = 3;
    }
  } else if (len > 0 && isPathSeparator(mutablePath[0])) {
    start = 1;
    if (len > 1 && isPathSeparator(mutablePath[1])) {
      start = 2;
    }
  }
#else
  if (len > 0 && isPathSeparator(mutablePath[0])) {
    start = 1;
  }
#endif

  for (size_t i = start; i <= len; i++) {
    if (i == len || isPathSeparator(mutablePath[i])) {
      char saved = mutablePath[i];
      mutablePath[i] = '\0';

      if (mutablePath[0] != '\0') {
        int createResult = createSingleDirectory(mutablePath.data());
        if (createResult != 0) {
          return createResult;
        }
      }

      mutablePath[i] = saved;

      while (i + 1 < len && isPathSeparator(mutablePath[i + 1])) {
        i++;
      }
    }
  }

  return 0;
}

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
      char *item = madlib__string__alloc_bytes((uint32_t)itemLength);
      memcpy(item, dirh.name, itemLength);
      item[itemLength] = '\0';

      result = madlib__list__internal__append(item, result);
    }

    int64_t *boxedError = (int64_t *)0;

    GC_FREE(req);

    __applyPAP__(callback, 2, boxedError, result);
  } else {
    int64_t *boxedError = (int64_t *)libuvErrorToMadlibIOError(req->result);

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


void createDirectoryWork(uv_work_t *req) {
  DirectoryCreateData_t *data = (DirectoryCreateData_t *)req->data;
  if (data->canceled) {
    return;
  }

  data->result = data->recursive ? createDirectoryRecursively(data->path) : createSingleDirectory(data->path);
}


void afterCreateDirectoryWork(uv_work_t *req, int status) {
  DirectoryCreateData_t *data = (DirectoryCreateData_t *)req->data;
  void *callback = data->callback;
  bool canceled = data->canceled || status == UV_ECANCELED;
  int result = status < 0 ? status : data->result;

  GC_FREE(data->path);
  GC_FREE(data);
  GC_FREE(req);

  if (canceled) {
    return;
  }

  int64_t *boxedError = result == 0 ? (int64_t *)0 : (int64_t *)libuvErrorToMadlibIOError(result);
  __applyPAP__(callback, 2, boxedError, NULL);
}


uv_work_t *madlib__directory__create(char *path, bool recursive, PAP_t *callback) {
  uv_work_t *req = (uv_work_t *)GC_MALLOC_UNCOLLECTABLE(sizeof(uv_work_t));
  DirectoryCreateData_t *data = (DirectoryCreateData_t *)GC_MALLOC_UNCOLLECTABLE(sizeof(DirectoryCreateData_t));

  size_t pathLength = strlen(path);
  char *copiedPath = (char *)GC_MALLOC_ATOMIC(pathLength + 1);
  memcpy(copiedPath, path, pathLength);
  copiedPath[pathLength] = '\0';

  data->callback = callback;
  data->path = copiedPath;
  data->recursive = recursive;
  data->result = 0;
  data->canceled = false;

  req->data = data;

  int queueResult = uv_queue_work(getLoop(), req, createDirectoryWork, afterCreateDirectoryWork);
  if (queueResult < 0) {
    int64_t *boxedError = (int64_t *)libuvErrorToMadlibIOError(queueResult);

    GC_FREE(copiedPath);
    GC_FREE(data);
    GC_FREE(req);

    __applyPAP__(callback, 2, boxedError, NULL);
    return NULL;
  }

  return req;
}


void madlib__directory__cancelCreate(uv_work_t *req) {
  if (req == NULL) {
    return;
  }

  DirectoryCreateData_t *data = (DirectoryCreateData_t *)req->data;
  if (data != NULL) {
    data->canceled = true;
  }

  uv_cancel((uv_req_t *)req);
}

#ifdef __cplusplus
}
#endif
