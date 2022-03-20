#include "file.hpp"

#include <gc.h>
#include <stdlib.h>
#include <uv.h>

#include "event-loop.hpp"


#ifdef __cplusplus
extern "C" {
#endif

// read file
typedef struct ReadData {
  void *callback;
  uv_fs_t *readRequest;
  uv_fs_t *openRequest;
  uv_buf_t uvBuffer;
  char *dataBuffer;
  char *fileContent;
  int64_t currentSize;

  // if true returns an a ByteArray instead of String
  bool readBytes;
} ReadData_t;

void onReadError(uv_fs_t *req) {
  uv_fs_req_cleanup(req);
  char **boxedResult = (char **)GC_malloc(sizeof(char *));
  *boxedResult = (char*)"\0";

  int64_t *boxedError = (int64_t *)GC_malloc(sizeof(int));
  *boxedError = libuvErrorToMadlibIOError(req->result);

  void *callback = ((ReadData_t *)req->data)->callback;

  // free resources
  char *dataBuffer = ((ReadData_t *)req->data)->dataBuffer;
  uv_fs_t *openRequest = ((ReadData_t *)req->data)->openRequest;
  uv_fs_t *readRequest = ((ReadData_t *)req->data)->readRequest;
  void *data = (ReadData_t *)req->data;

  GC_free(dataBuffer);
  GC_free(data);
  GC_free(openRequest);
  GC_free(readRequest);

  __applyPAP__(callback, 2, boxedError, boxedResult);
}

void onRead(uv_fs_t *req) {
  if (req->result < 0) {
    onReadError(req);
  } else if (req->result == 0) {
    uv_fs_req_cleanup(req);

    // close file
    uv_fs_t closeReq;
    uv_fs_close(getLoop(), &closeReq, ((ReadData_t *)req->data)->openRequest->result, NULL);

    int64_t *boxedError = (int64_t *)GC_malloc(sizeof(int64_t));
    *boxedError = 0;

    // box the result
    if (((ReadData_t *)req->data)->readBytes) {
      madlib__bytearray__ByteArray_t *arr =
          (madlib__bytearray__ByteArray_t *)GC_malloc(sizeof(madlib__bytearray__ByteArray_t));
      arr->bytes = (unsigned char *)((ReadData_t *)req->data)->fileContent;
      arr->length = ((ReadData_t *)req->data)->currentSize;

      __applyPAP__(((ReadData_t *)req->data)->callback, 2, boxedError, (void *)arr);
    } else {
      char **boxedResult = (char **)GC_malloc(sizeof(char *));
      *boxedResult = ((ReadData_t *)req->data)->fileContent;

      // call the callback
      __applyPAP__(((ReadData_t *)req->data)->callback, 2, boxedError, boxedResult);
    }

    // free resources
    char *dataBuffer = ((ReadData_t *)req->data)->dataBuffer;
    uv_fs_t *openRequest = ((ReadData_t *)req->data)->openRequest;
    uv_fs_t *readRequest = ((ReadData_t *)req->data)->readRequest;
    void *data = (ReadData_t *)req->data;

    GC_free(dataBuffer);
    GC_free(data);
    GC_free(openRequest);
    GC_free(readRequest);
  } else {
    uv_fs_req_cleanup(req);

    // get the byte count already read
    int64_t currentSize = ((ReadData_t *)req->data)->currentSize;

    // increase the byte count for the next iteration
    ((ReadData_t *)req->data)->currentSize = currentSize + req->result;

    // allocate the next content to the old size + size of current buffer
    char *nextContent = (char *)GC_malloc(currentSize + req->result);

    // if the fileContent is not empty we copy what was in it in the newly
    // allocated one
    if (currentSize > 0) {
      memcpy(nextContent, ((ReadData_t *)req->data)->fileContent, currentSize);
    }

    // then we copy after the already existing content, all data from the buffer
    memcpy(nextContent + currentSize, ((ReadData_t *)req->data)->dataBuffer, req->result);

    GC_free(((ReadData_t *)req->data)->fileContent);

    // we assign the fileContent to the newly created structure
    ((ReadData_t *)req->data)->fileContent = nextContent;

    // we ask to be notified when the buffer has been filled again
    uv_fs_read(getLoop(), req, ((ReadData_t *)req->data)->openRequest->result, &((ReadData_t *)req->data)->uvBuffer, 1,
               -1, onRead);
  }
}

void onReadFileOpen(uv_fs_t *req) {
  uv_fs_req_cleanup(req);
  if (req->result >= 0) {
    uv_buf_t uvBuffer = uv_buf_init(((ReadData_t *)req->data)->dataBuffer, 1024);
    ((ReadData_t *)((ReadData_t *)req->data)->readRequest->data)->uvBuffer = uvBuffer;
    uv_fs_read(getLoop(), ((ReadData_t *)req->data)->readRequest, req->result, &uvBuffer, 1, -1, onRead);
  } else {
    onReadError(req);
  }
}

void madlib__file__read(char *filepath, PAP_t *callback) {
  // we allocate request objects and the buffer
  uv_fs_t *openReq = (uv_fs_t *)GC_malloc_uncollectable(sizeof(uv_fs_t));
  uv_fs_t *readReq = (uv_fs_t *)GC_malloc_uncollectable(sizeof(uv_fs_t));
  char *dataBuffer = (char *)GC_malloc_uncollectable(sizeof(char) * 1024);

  // we allocate and initialize the data of requests
  readReq->data = GC_malloc_uncollectable(sizeof(ReadData_t));
  ((ReadData_t *)readReq->data)->callback = callback;
  ((ReadData_t *)readReq->data)->readRequest = readReq;
  ((ReadData_t *)readReq->data)->openRequest = openReq;
  ((ReadData_t *)readReq->data)->dataBuffer = dataBuffer;
  ((ReadData_t *)readReq->data)->currentSize = 0;
  ((ReadData_t *)readReq->data)->readBytes = false;

  openReq->data = readReq->data;

  // we open the file
  uv_fs_open(getLoop(), openReq, filepath, O_RDONLY, 0, onReadFileOpen);
}

void madlib__file__readBytes(char *filepath, PAP_t *callback) {
  // we allocate request objects and the buffer
  uv_fs_t *openReq = (uv_fs_t *)GC_malloc_uncollectable(sizeof(uv_fs_t));
  uv_fs_t *readReq = (uv_fs_t *)GC_malloc_uncollectable(sizeof(uv_fs_t));
  char *dataBuffer = (char *)GC_malloc_uncollectable(sizeof(char) * 1024);

  // we allocate and initialize the data of requests
  readReq->data = GC_malloc_uncollectable(sizeof(ReadData_t));
  ((ReadData_t *)readReq->data)->callback = callback;
  ((ReadData_t *)readReq->data)->readRequest = readReq;
  ((ReadData_t *)readReq->data)->openRequest = openReq;
  ((ReadData_t *)readReq->data)->dataBuffer = dataBuffer;
  ((ReadData_t *)readReq->data)->currentSize = 0;
  ((ReadData_t *)readReq->data)->readBytes = true;

  openReq->data = readReq->data;

  // we open the file
  uv_fs_open(getLoop(), openReq, filepath, O_RDONLY, 0, onReadFileOpen);
}

// write file
typedef struct WriteData {
  void *callback;
  uv_fs_t *readRequest;
  uv_fs_t *openRequest;
  uv_buf_t contentBuffer;
} WriteData_t;

void onWriteError(uv_fs_t *req) {
  uv_fs_req_cleanup(req);
  char **boxedResult = (char **)GC_malloc_uncollectable(sizeof(char *));
  char *result = (char *)GC_malloc_uncollectable(sizeof(char));
  *result = 0;
  *boxedResult = result;

  int64_t *boxedError = (int64_t *)GC_malloc_uncollectable(sizeof(int));
  *boxedError = libuvErrorToMadlibIOError(req->result);

  void *callback = ((WriteData_t *)req->data)->callback;

  // free resources
  GC_free(((WriteData_t *)req->data)->openRequest);
  GC_free(req->data);
  GC_free(req);

  __applyPAP__(callback, 2, boxedError, boxedResult);
}

void onWrite(uv_fs_t *req) {
  uv_fs_req_cleanup(req);
  if (req->result < 0) {
    onWriteError(req);
  } else {
    // close file
    uv_fs_t closeReq;
    uv_fs_close(getLoop(), &closeReq, ((WriteData_t *)req->data)->openRequest->result, NULL);

    int64_t *boxedError = (int64_t *)GC_malloc_uncollectable(sizeof(int64_t));
    *boxedError = 0;

    // call the callback
    __applyPAP__(((WriteData_t *)req->data)->callback, 2, boxedError, NULL);

    // free resources
    GC_free(((WriteData_t *)req->data)->openRequest);
    GC_free(req->data);
    GC_free(req);
  }
}

void onWriteFileOpen(uv_fs_t *req) {
  uv_fs_req_cleanup(req);
  if (req->result >= 0) {
    uv_fs_write(getLoop(), ((WriteData_t *)req->data)->readRequest, req->result,
                &((WriteData_t *)req->data)->contentBuffer, 1, -1, onWrite);
  } else {
    onWriteError(req);
  }
}

// Callback receives unit in case of success
void madlib__file__write(char *filepath, char *content, PAP_t *callback) {
  // we allocate request objects and the buffer
  uv_fs_t *openReq = (uv_fs_t *)GC_malloc_uncollectable(sizeof(uv_fs_t));
  uv_fs_t *readReq = (uv_fs_t *)GC_malloc_uncollectable(sizeof(uv_fs_t));

  // we allocate and initialize the data of requests
  readReq->data = GC_malloc_uncollectable(sizeof(WriteData_t));
  ((WriteData_t *)readReq->data)->callback = callback;
  ((WriteData_t *)readReq->data)->readRequest = readReq;
  ((WriteData_t *)readReq->data)->openRequest = openReq;
  ((WriteData_t *)readReq->data)->contentBuffer.base = content;
  ((WriteData_t *)readReq->data)->contentBuffer.len = strlen(content);

  openReq->data = readReq->data;

  // we open the file
  uv_fs_open(getLoop(), openReq, filepath, O_WRONLY | O_CREAT, S_IRUSR | S_IWUSR, onWriteFileOpen);
}

// Callback receives unit in case of success
void madlib__file__writeBytes(char *filepath, madlib__bytearray__ByteArray_t *content, PAP_t *callback) {
  // we allocate request objects and the buffer
  uv_fs_t *openReq = (uv_fs_t *)GC_malloc_uncollectable(sizeof(uv_fs_t));
  uv_fs_t *readReq = (uv_fs_t *)GC_malloc_uncollectable(sizeof(uv_fs_t));

  // we allocate and initialize the data of requests
  readReq->data = GC_malloc_uncollectable(sizeof(WriteData_t));
  ((WriteData_t *)readReq->data)->callback = callback;
  ((WriteData_t *)readReq->data)->readRequest = readReq;
  ((WriteData_t *)readReq->data)->openRequest = openReq;
  ((WriteData_t *)readReq->data)->contentBuffer.base = (char *)content->bytes;
  ((WriteData_t *)readReq->data)->contentBuffer.len = content->length;

  openReq->data = readReq->data;

  // we open the file
  uv_fs_open(getLoop(), openReq, filepath, O_WRONLY | O_CREAT, S_IRUSR | S_IWUSR, onWriteFileOpen);
}

typedef struct FileExistData {
  void *callback;
} FileExistData_t;

void onFileExists(uv_fs_t *req) {
  uv_fs_req_cleanup(req);

  bool *result = (bool *)GC_malloc(sizeof(bool));
  if (req->result < 0) {
    *result = false;
  } else {
    *result = true;
  }

  __applyPAP__(((FileExistData_t *)req->data)->callback, 1, result);

  // free memory
  GC_free(req->data);
  GC_free(req);
}

void madlib__file__exists(char *filepath, PAP_t *callback) {
  uv_fs_t *accessReq = (uv_fs_t *)GC_malloc_uncollectable(sizeof(uv_fs_t));

  accessReq->data = GC_malloc_uncollectable(sizeof(FileExistData_t));
  ((FileExistData_t *)accessReq->data)->callback = callback;

  uv_fs_access(getLoop(), accessReq, filepath, UV_FS_O_NOATIME, onFileExists);
}

#ifdef __cplusplus
}
#endif
