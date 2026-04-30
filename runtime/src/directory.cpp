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

enum DirectoryOperationType {
  DIRECTORY_OPERATION_REMOVE,
  DIRECTORY_OPERATION_COPY,
  DIRECTORY_OPERATION_RENAME,
};

typedef struct DirectoryWorkData {
  void *callback;
  char *sourcePath;
  char *destinationPath;
  bool recursive;
  bool overwrite;
  std::vector<std::string> *exclude;
  DirectoryOperationType operation;
  int result;
  bool canceled;
} DirectoryWorkData_t;


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


static std::string joinPath(const std::string &base, const std::string &name) {
  if (base.empty()) {
    return name;
  }

  if (isPathSeparator(base.back())) {
    return base + name;
  }

  return base + "/" + name;
}


static std::string baseName(const std::string &path) {
  if (path.empty()) {
    return path;
  }

  size_t end = path.size();
  while (end > 0 && isPathSeparator(path[end - 1])) {
    end--;
  }

  if (end == 0) {
    return "";
  }

  size_t separatorPos = path.find_last_of("/\\", end - 1);
  if (separatorPos == std::string::npos) {
    return path.substr(0, end);
  }

  return path.substr(separatorPos + 1, end - separatorPos - 1);
}


static bool isExcluded(const std::vector<std::string> *exclude, const std::string &path) {
  if (exclude == NULL) {
    return false;
  }

  std::string name = baseName(path);
  for (size_t i = 0; i < exclude->size(); i++) {
    if ((*exclude)[i] == name) {
      return true;
    }
  }

  return false;
}


static int statPath(const std::string &path, uv_stat_t *statBuffer) {
  uv_fs_t req;
  int result = uv_fs_stat(NULL, &req, path.c_str(), NULL);
  if (result == 0) {
    *statBuffer = req.statbuf;
  }
  uv_fs_req_cleanup(&req);
  return result;
}


static bool pathExists(const std::string &path) {
  uv_stat_t statBuffer;
  return statPath(path, &statBuffer) == 0;
}


static int removeDirectoryRecursively(const std::string &path) {
  uv_stat_t statBuffer;
  int statResult = statPath(path, &statBuffer);
  if (statResult != 0) {
    return statResult;
  }

  if (!(statBuffer.st_mode & S_IFDIR)) {
    uv_fs_t unlinkReq;
    int unlinkResult = uv_fs_unlink(NULL, &unlinkReq, path.c_str(), NULL);
    uv_fs_req_cleanup(&unlinkReq);
    return unlinkResult;
  }

  uv_fs_t scandirReq;
  int scandirResult = uv_fs_scandir(NULL, &scandirReq, path.c_str(), 0, NULL);
  if (scandirResult < 0) {
    uv_fs_req_cleanup(&scandirReq);
    return scandirResult;
  }

  while (true) {
    uv_dirent_t entry;
    int nextResult = uv_fs_scandir_next(&scandirReq, &entry);

    if (nextResult == UV_EOF) {
      break;
    }

    if (nextResult < 0) {
      uv_fs_req_cleanup(&scandirReq);
      return nextResult;
    }

    if (strcmp(entry.name, ".") == 0 || strcmp(entry.name, "..") == 0) {
      continue;
    }

    int removeResult = removeDirectoryRecursively(joinPath(path, entry.name));
    if (removeResult != 0) {
      uv_fs_req_cleanup(&scandirReq);
      return removeResult;
    }
  }

  uv_fs_req_cleanup(&scandirReq);

  uv_fs_t rmdirReq;
  int rmdirResult = uv_fs_rmdir(NULL, &rmdirReq, path.c_str(), NULL);
  uv_fs_req_cleanup(&rmdirReq);
  return rmdirResult;
}


static int removePath(const std::string &path, bool recursive) {
  uv_stat_t statBuffer;
  int statResult = statPath(path, &statBuffer);
  if (statResult != 0) {
    return statResult;
  }

  if ((statBuffer.st_mode & S_IFDIR) && recursive) {
    return removeDirectoryRecursively(path);
  }

  if (statBuffer.st_mode & S_IFDIR) {
    uv_fs_t rmdirReq;
    int rmdirResult = uv_fs_rmdir(NULL, &rmdirReq, path.c_str(), NULL);
    uv_fs_req_cleanup(&rmdirReq);
    return rmdirResult;
  }

  uv_fs_t unlinkReq;
  int unlinkResult = uv_fs_unlink(NULL, &unlinkReq, path.c_str(), NULL);
  uv_fs_req_cleanup(&unlinkReq);
  return unlinkResult;
}


static int copyPath(const std::string &sourcePath, const std::string &destinationPath, bool recursive, bool overwrite, const std::vector<std::string> *exclude) {
  if (isExcluded(exclude, sourcePath)) {
    return 0;
  }

  uv_stat_t sourceStat;
  int statResult = statPath(sourcePath, &sourceStat);
  if (statResult != 0) {
    return statResult;
  }

  if (sourceStat.st_mode & S_IFDIR) {
    if (!recursive) {
      return UV_EISDIR;
    }

    int createResult = createDirectoryRecursively(destinationPath.c_str());
    if (createResult != 0) {
      return createResult;
    }

    uv_fs_t scandirReq;
    int scandirResult = uv_fs_scandir(NULL, &scandirReq, sourcePath.c_str(), 0, NULL);
    if (scandirResult < 0) {
      uv_fs_req_cleanup(&scandirReq);
      return scandirResult;
    }

    while (true) {
      uv_dirent_t entry;
      int nextResult = uv_fs_scandir_next(&scandirReq, &entry);

      if (nextResult == UV_EOF) {
        break;
      }

      if (nextResult < 0) {
        uv_fs_req_cleanup(&scandirReq);
        return nextResult;
      }

      if (strcmp(entry.name, ".") == 0 || strcmp(entry.name, "..") == 0) {
        continue;
      }

      int copyResult = copyPath(
        joinPath(sourcePath, entry.name),
        joinPath(destinationPath, entry.name),
        recursive,
        overwrite,
        exclude
      );
      if (copyResult != 0) {
        uv_fs_req_cleanup(&scandirReq);
        return copyResult;
      }
    }

    uv_fs_req_cleanup(&scandirReq);
    return 0;
  }

  if (pathExists(destinationPath)) {
    if (!overwrite) {
      return UV_EEXIST;
    }

    int removeResult = removePath(destinationPath, true);
    if (removeResult != 0) {
      return removeResult;
    }
  }

  uv_fs_t copyReq;
  int copyResult = uv_fs_copyfile(NULL, &copyReq, sourcePath.c_str(), destinationPath.c_str(), 0, NULL);
  uv_fs_req_cleanup(&copyReq);
  return copyResult;
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
  if (path == NULL) {
    int64_t *boxedError = (int64_t *)libuvErrorToMadlibIOError(UV_EINVAL);
    __applyPAP__(callback, 2, boxedError, NULL);
    return NULL;
  }

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


static void directoryWork(uv_work_t *req) {
  DirectoryWorkData_t *data = (DirectoryWorkData_t *)req->data;
  if (data->canceled) {
    return;
  }

  if (data->operation == DIRECTORY_OPERATION_REMOVE) {
    data->result = removePath(data->sourcePath, data->recursive);
  } else if (data->operation == DIRECTORY_OPERATION_COPY) {
    data->result = copyPath(data->sourcePath, data->destinationPath, data->recursive, data->overwrite, data->exclude);
  } else {
    uv_fs_t renameReq;
    data->result = uv_fs_rename(NULL, &renameReq, data->sourcePath, data->destinationPath, NULL);
    uv_fs_req_cleanup(&renameReq);
  }
}


static void afterDirectoryWork(uv_work_t *req, int status) {
  DirectoryWorkData_t *data = (DirectoryWorkData_t *)req->data;
  void *callback = data->callback;
  bool canceled = data->canceled || status == UV_ECANCELED;
  int result = status < 0 ? status : data->result;

  GC_FREE(data->sourcePath);
  if (data->destinationPath != NULL) {
    GC_FREE(data->destinationPath);
  }
  if (data->exclude != NULL) {
    delete data->exclude;
  }
  GC_FREE(data);
  GC_FREE(req);

  if (canceled) {
    return;
  }

  int64_t *boxedError = result == 0 ? (int64_t *)0 : (int64_t *)libuvErrorToMadlibIOError(result);
  __applyPAP__(callback, 2, boxedError, NULL);
}


static char *copyCString(char *value) {
  if (value == NULL) {
    return NULL;
  }

  size_t length = strlen(value);
  char *copy = (char *)GC_MALLOC_ATOMIC(length + 1);
  memcpy(copy, value, length);
  copy[length] = '\0';
  return copy;
}


static std::vector<std::string> *copyExcludeList(madlib__list__Node_t *exclude) {
  std::vector<std::string> *items = new std::vector<std::string>();
  madlib__list__Node_t *current = exclude;
  while (current != NULL && current->next != NULL) {
    if (current->value != NULL) {
      items->push_back(std::string((char *)current->value));
    }
    current = current->next;
  }
  return items;
}


static uv_work_t *startDirectoryWork(
  char *sourcePath,
  char *destinationPath,
  bool recursive,
  bool overwrite,
  madlib__list__Node_t *exclude,
  DirectoryOperationType operation,
  PAP_t *callback
) {
  if (sourcePath == NULL) {
    int64_t *boxedError = (int64_t *)libuvErrorToMadlibIOError(UV_EINVAL);
    __applyPAP__(callback, 2, boxedError, NULL);
    return NULL;
  }

  uv_work_t *req = (uv_work_t *)GC_MALLOC_UNCOLLECTABLE(sizeof(uv_work_t));
  DirectoryWorkData_t *data = (DirectoryWorkData_t *)GC_MALLOC_UNCOLLECTABLE(sizeof(DirectoryWorkData_t));

  data->callback = callback;
  data->sourcePath = copyCString(sourcePath);
  data->destinationPath = copyCString(destinationPath);
  data->recursive = recursive;
  data->overwrite = overwrite;
  data->exclude = copyExcludeList(exclude);
  data->operation = operation;
  data->result = 0;
  data->canceled = false;

  req->data = data;

  int queueResult = uv_queue_work(getLoop(), req, directoryWork, afterDirectoryWork);
  if (queueResult < 0) {
    int64_t *boxedError = (int64_t *)libuvErrorToMadlibIOError(queueResult);

    GC_FREE(data->sourcePath);
    if (data->destinationPath != NULL) {
      GC_FREE(data->destinationPath);
    }
    delete data->exclude;
    GC_FREE(data);
    GC_FREE(req);

    __applyPAP__(callback, 2, boxedError, NULL);
    return NULL;
  }

  return req;
}


uv_work_t *madlib__directory__remove(char *path, bool recursive, PAP_t *callback) {
  return startDirectoryWork(path, NULL, recursive, false, NULL, DIRECTORY_OPERATION_REMOVE, callback);
}


void madlib__directory__cancelRemove(uv_work_t *req) {
  if (req == NULL) {
    return;
  }

  DirectoryWorkData_t *data = (DirectoryWorkData_t *)req->data;
  if (data != NULL) {
    data->canceled = true;
  }

  uv_cancel((uv_req_t *)req);
}


uv_work_t *madlib__directory__copy(
  char *sourcePath,
  char *destinationPath,
  bool recursive,
  bool overwrite,
  madlib__list__Node_t *exclude,
  PAP_t *callback
) {
  return startDirectoryWork(sourcePath, destinationPath, recursive, overwrite, exclude, DIRECTORY_OPERATION_COPY, callback);
}


void madlib__directory__cancelCopy(uv_work_t *req) {
  madlib__directory__cancelRemove(req);
}


uv_work_t *madlib__directory__rename(char *sourcePath, char *destinationPath, PAP_t *callback) {
  return startDirectoryWork(sourcePath, destinationPath, false, false, NULL, DIRECTORY_OPERATION_RENAME, callback);
}


void madlib__directory__cancelRename(uv_work_t *req) {
  madlib__directory__cancelRemove(req);
}

#ifdef __cplusplus
}
#endif
