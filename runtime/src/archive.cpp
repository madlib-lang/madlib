#include "archive.hpp"

#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <algorithm>
#include <cctype>
#include <cstdlib>
#include <string>
#include <vector>

#ifdef _WIN32
#include <direct.h>
#endif

#include "event-loop.hpp"
#include "miniz.h"

enum ArchiveOperationType {
  ARCHIVE_OPERATION_ZIP,
  ARCHIVE_OPERATION_UNZIP,
};

typedef struct ArchiveWorkData {
  void *callback;
  char *sourcePath;
  char *destinationPath;
  ArchiveOperationType operation;
  int result;
  bool canceled;
} ArchiveWorkData_t;


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


static int pathIsDirectory(const std::string &path, bool *isDir) {
  struct stat statBuffer;
  int statResult = stat(path.c_str(), &statBuffer);
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
    struct stat statBuffer;
    if (stat(path, &statBuffer) != 0) {
      return translateErrnoToLibuv();
    }

    return S_ISDIR(statBuffer.st_mode) ? 0 : UV_EEXIST;
  }

  return translateErrnoToLibuv();
}


static int createDirectoryRecursively(const std::string &path) {
  if (path.empty()) {
    return UV_EINVAL;
  }

  std::string normalizedPath = path;

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


static std::string takeDirectory(const std::string &path) {
  size_t separatorPos = path.find_last_of("/\\");
  if (separatorPos == std::string::npos) {
    return "";
  }

  return path.substr(0, separatorPos);
}


static bool isUnsafeArchivePath(const std::string &path) {
  if (path.empty()) {
    return true;
  }

  if (path[0] == '/' || path[0] == '\\') {
    return true;
  }

  if (path.size() >= 2 && std::isalpha((unsigned char)path[0]) && path[1] == ':') {
    return true;
  }

  std::string segment;
  for (size_t i = 0; i <= path.size(); i++) {
    char current = i == path.size() ? '/' : path[i];

    if (current == '/' || current == '\\') {
      if (segment == "..") {
        return true;
      }
      segment.clear();
    } else {
      segment.push_back(current);
    }
  }

  return false;
}


static int addDirectoryContentsToZip(mz_zip_archive *archive, const std::string &sourceDirectoryPath, const std::string &relativePrefix) {
  uv_fs_t scandirReq;
  int scandirResult = uv_fs_scandir(NULL, &scandirReq, sourceDirectoryPath.c_str(), 0, NULL);
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

    std::string fullPath = joinPath(sourceDirectoryPath, entry.name);

    bool isDirectory = entry.type == UV_DIRENT_DIR;
    if (entry.type == UV_DIRENT_UNKNOWN) {
      int dirResult = pathIsDirectory(fullPath, &isDirectory);
      if (dirResult != 0) {
        uv_fs_req_cleanup(&scandirReq);
        return dirResult;
      }
    }

    if (isDirectory) {
      std::string relativeDirPath = relativePrefix + entry.name + "/";
      if (!mz_zip_writer_add_mem(archive, relativeDirPath.c_str(), NULL, 0, MZ_NO_COMPRESSION)) {
        uv_fs_req_cleanup(&scandirReq);
        return UV_EIO;
      }

      int recursiveResult = addDirectoryContentsToZip(archive, fullPath, relativePrefix + entry.name + "/");
      if (recursiveResult != 0) {
        uv_fs_req_cleanup(&scandirReq);
        return recursiveResult;
      }
    } else {
      std::string relativeFilePath = relativePrefix + entry.name;
      if (!mz_zip_writer_add_file(archive, relativeFilePath.c_str(), fullPath.c_str(), NULL, 0, MZ_BEST_COMPRESSION)) {
        uv_fs_req_cleanup(&scandirReq);
        return UV_EIO;
      }
    }
  }

  uv_fs_req_cleanup(&scandirReq);
  return 0;
}


static int zipDirectoryContents(const std::string &sourceDirectoryPath, const std::string &archivePath) {
  bool isDirectory = false;
  int pathResult = pathIsDirectory(sourceDirectoryPath, &isDirectory);
  if (pathResult != 0) {
    return pathResult;
  }

  if (!isDirectory) {
    return UV_ENOTDIR;
  }

  mz_zip_archive archive;
  memset(&archive, 0, sizeof(archive));

  if (!mz_zip_writer_init_file(&archive, archivePath.c_str(), 0)) {
    return UV_EIO;
  }

  int result = addDirectoryContentsToZip(&archive, sourceDirectoryPath, "");
  if (result == 0 && !mz_zip_writer_finalize_archive(&archive)) {
    result = UV_EIO;
  }

  if (!mz_zip_writer_end(&archive) && result == 0) {
    result = UV_EIO;
  }

  return result;
}


static int unzipArchiveToDirectory(const std::string &archivePath, const std::string &destinationDirectoryPath) {
  int createDestResult = createDirectoryRecursively(destinationDirectoryPath);
  if (createDestResult != 0) {
    return createDestResult;
  }

  mz_zip_archive archive;
  memset(&archive, 0, sizeof(archive));

  if (!mz_zip_reader_init_file(&archive, archivePath.c_str(), 0)) {
    return UV_EIO;
  }

  int result = 0;
  mz_uint fileCount = mz_zip_reader_get_num_files(&archive);

  for (mz_uint i = 0; i < fileCount; i++) {
    mz_zip_archive_file_stat fileStat;
    if (!mz_zip_reader_file_stat(&archive, i, &fileStat)) {
      result = UV_EIO;
      break;
    }

    std::string entryPath(fileStat.m_filename);
    std::replace(entryPath.begin(), entryPath.end(), '\\', '/');

    if (isUnsafeArchivePath(entryPath)) {
      result = UV_EINVAL;
      break;
    }

    std::string outputPath = joinPath(destinationDirectoryPath, entryPath);
    bool entryIsDirectory = mz_zip_reader_is_file_a_directory(&archive, i) != 0;

    if (entryIsDirectory) {
      result = createDirectoryRecursively(outputPath);
      if (result != 0) {
        break;
      }

      continue;
    }

    std::string outputParentDirectory = takeDirectory(outputPath);
    if (!outputParentDirectory.empty()) {
      result = createDirectoryRecursively(outputParentDirectory);
      if (result != 0) {
        break;
      }
    }

    if (!mz_zip_reader_extract_to_file(&archive, i, outputPath.c_str(), 0)) {
      result = UV_EIO;
      break;
    }
  }

  if (!mz_zip_reader_end(&archive) && result == 0) {
    result = UV_EIO;
  }

  return result;
}


static void archiveWork(uv_work_t *req) {
  ArchiveWorkData_t *data = (ArchiveWorkData_t *)req->data;
  if (data->canceled) {
    return;
  }

  if (data->operation == ARCHIVE_OPERATION_ZIP) {
    data->result = zipDirectoryContents(data->sourcePath, data->destinationPath);
  } else {
    data->result = unzipArchiveToDirectory(data->sourcePath, data->destinationPath);
  }
}


static void archiveAfterWork(uv_work_t *req, int status) {
  ArchiveWorkData_t *data = (ArchiveWorkData_t *)req->data;
  void *callback = data->callback;
  bool canceled = data->canceled || status == UV_ECANCELED;
  int result = status < 0 ? status : data->result;

  free(data->sourcePath);
  free(data->destinationPath);
  free(data);
  free(req);

  if (canceled) {
    return;
  }

  int64_t *boxedError = result == 0 ? (int64_t *)0 : (int64_t *)libuvErrorToMadlibIOError(result);
  __applyPAP__(callback, 2, boxedError, NULL);
}


static uv_work_t *startArchiveWork(char *sourcePath, char *destinationPath, ArchiveOperationType operation, PAP_t *callback) {
  uv_work_t *req = (uv_work_t *)malloc(sizeof(uv_work_t));
  ArchiveWorkData_t *data = (ArchiveWorkData_t *)malloc(sizeof(ArchiveWorkData_t));

  if (req == NULL || data == NULL) {
    free(req);
    free(data);
    int64_t *boxedError = (int64_t *)libuvErrorToMadlibIOError(UV_ENOMEM);
    __applyPAP__(callback, 2, boxedError, NULL);
    return NULL;
  }

  size_t sourcePathLength = strlen(sourcePath);
  char *sourcePathCopy = (char *)malloc(sourcePathLength + 1);
  size_t destinationPathLength = strlen(destinationPath);
  char *destinationPathCopy = (char *)malloc(destinationPathLength + 1);

  if (sourcePathCopy == NULL || destinationPathCopy == NULL) {
    free(sourcePathCopy);
    free(destinationPathCopy);
    free(data);
    free(req);
    int64_t *boxedError = (int64_t *)libuvErrorToMadlibIOError(UV_ENOMEM);
    __applyPAP__(callback, 2, boxedError, NULL);
    return NULL;
  }

  memcpy(sourcePathCopy, sourcePath, sourcePathLength);
  sourcePathCopy[sourcePathLength] = '\0';
  memcpy(destinationPathCopy, destinationPath, destinationPathLength);
  destinationPathCopy[destinationPathLength] = '\0';

  data->callback = callback;
  data->sourcePath = sourcePathCopy;
  data->destinationPath = destinationPathCopy;
  data->operation = operation;
  data->result = 0;
  data->canceled = false;

  req->data = data;

  int queueResult = uv_queue_work(getLoop(), req, archiveWork, archiveAfterWork);
  if (queueResult < 0) {
    int64_t *boxedError = (int64_t *)libuvErrorToMadlibIOError(queueResult);

    free(sourcePathCopy);
    free(destinationPathCopy);
    free(data);
    free(req);

    __applyPAP__(callback, 2, boxedError, NULL);
    return NULL;
  }

  return req;
}

#ifdef __cplusplus
extern "C" {
#endif


uv_work_t *madlib__archive__zip(char *sourceDirectoryPath, char *archivePath, PAP_t *callback) {
  return startArchiveWork(sourceDirectoryPath, archivePath, ARCHIVE_OPERATION_ZIP, callback);
}


void madlib__archive__cancelZip(uv_work_t *req) {
  if (req == NULL) {
    return;
  }

  ArchiveWorkData_t *data = (ArchiveWorkData_t *)req->data;
  if (data != NULL) {
    data->canceled = true;
  }

  uv_cancel((uv_req_t *)req);
}


uv_work_t *madlib__archive__unzip(char *archivePath, char *destinationDirectoryPath, PAP_t *callback) {
  return startArchiveWork(archivePath, destinationDirectoryPath, ARCHIVE_OPERATION_UNZIP, callback);
}


void madlib__archive__cancelUnzip(uv_work_t *req) {
  madlib__archive__cancelZip(req);
}

#ifdef __cplusplus
}
#endif
