#include "file.hpp"

#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "event-loop.hpp"


#ifdef __cplusplus
extern "C" {
#endif

const size_t BUFFER_SIZE = 32 * 1024; // 32KB



void onReadError(uv_fs_t *req) {
  ((ReadData_t *)req->data)->closed = true;

  if (((ReadData_t *)req->data)->canceled) {
    return;
  }

  char *result = (char*)"";

  int64_t *boxedError = (int64_t *)libuvErrorToMadlibIOError(req->result);

  void *callback = ((ReadData_t *)req->data)->callback;

  // free resources
  char *dataBuffer = ((ReadData_t *)req->data)->dataBuffer;
  uv_fs_t *openRequest = ((ReadData_t *)req->data)->openRequest;
  uv_fs_t *readRequest = ((ReadData_t *)req->data)->readRequest;
  void *data = (ReadData_t *)req->data;

  uv_fs_t closeReq;
  uv_fs_close(getLoop(), &closeReq, openRequest->result, NULL);

  GC_FREE(dataBuffer);
  GC_FREE(data);
  GC_FREE(openRequest);
  GC_FREE(readRequest);

  __applyPAP__(callback, 2, boxedError, result);
}

void onRead(uv_fs_t *req) {
  uv_fs_req_cleanup(req);

  if ((ReadData_t *)((ReadData_t *)req->data)->canceled) {
    return;
  }

  if (req->result < 0) {
    onReadError(req);
  } else if (req->result == 0) {
    ((ReadData_t *)req->data)->closed = true;
    // close file
    uv_fs_t closeReq;
    uv_fs_close(getLoop(), &closeReq, ((ReadData_t *)req->data)->openRequest->result, NULL);

    int64_t *boxedError = (int64_t *)0;

    // box the result
    if (((ReadData_t *)req->data)->readBytes) {
      madlib__bytearray__ByteArray_t *arr =
          (madlib__bytearray__ByteArray_t *)GC_MALLOC(sizeof(madlib__bytearray__ByteArray_t));
      arr->bytes = (unsigned char *)((ReadData_t *)req->data)->fileContent;
      arr->length = ((ReadData_t *)req->data)->currentSize;

      __applyPAP__(((ReadData_t *)req->data)->callback, 2, boxedError, (void *)arr);
    } else {
      // call the callback
      __applyPAP__(((ReadData_t *)req->data)->callback, 2, boxedError, ((ReadData_t *)req->data)->fileContent);
    }

    // free resources
    char *dataBuffer = ((ReadData_t *)req->data)->dataBuffer;
    uv_fs_t *openRequest = ((ReadData_t *)req->data)->openRequest;
    uv_fs_t *readRequest = ((ReadData_t *)req->data)->readRequest;
    void *data = (ReadData_t *)req->data;

    GC_FREE(dataBuffer);
    GC_FREE(data);
    GC_FREE(openRequest);
    GC_FREE(readRequest);
  } else {
    // get the byte count already read
    int64_t currentSize = ((ReadData_t *)req->data)->currentSize;

    // increase the byte count for the next iteration
    ((ReadData_t *)req->data)->currentSize = currentSize + req->result;

    // allocate the next content to the old size + size of current buffer
    char *nextContent = (char *)GC_MALLOC_ATOMIC(currentSize + req->result + 1);

    // if the fileContent is not empty we copy what was in it in the newly
    // allocated one
    if (currentSize > 0) {
      memcpy(nextContent, ((ReadData_t *)req->data)->fileContent, currentSize);
    }

    // then we copy after the already existing content, all data from the buffer
    memcpy(nextContent + currentSize, ((ReadData_t *)req->data)->dataBuffer, req->result);

    // we assign the fileContent to the newly created structure
    ((ReadData_t *)req->data)->fileContent = nextContent;
    ((ReadData_t *)req->data)->fileContent[currentSize + req->result] = '\0';

    // we ask to be notified when the buffer has been filled again
    uv_fs_read(getLoop(), req, ((ReadData_t *)req->data)->openRequest->result, &((ReadData_t *)req->data)->uvBuffer, 1,
               -1, onRead);
  }
}

void onReadFileOpen(uv_fs_t *req) {
  uv_fs_req_cleanup(req);
  if ((ReadData_t *)((ReadData_t *)req->data)->canceled) {
    return;
  }

  ((ReadData_t *)req->data)->opened = true;

  if (req->result >= 0) {
    uv_buf_t uvBuffer = uv_buf_init(((ReadData_t *)req->data)->dataBuffer, BUFFER_SIZE);
    ((ReadData_t *)((ReadData_t *)req->data)->readRequest->data)->uvBuffer = uvBuffer;
    int r = uv_fs_read(getLoop(), ((ReadData_t *)req->data)->readRequest, req->result, &uvBuffer, 1, -1, onRead);
    ((ReadData_t *)req->data)->reading = true;
  } else {
    onReadError(req);
  }
}

ReadData_t *madlib__file__read(char *filepath, PAP_t *callback) {
  // we allocate request objects and the buffer
  uv_fs_t *openReq = (uv_fs_t *)GC_MALLOC(sizeof(uv_fs_t));
  uv_fs_t *readReq = (uv_fs_t *)GC_MALLOC(sizeof(uv_fs_t));
  char *dataBuffer = (char *)GC_MALLOC(sizeof(char) * BUFFER_SIZE);

  // we allocate and initialize the data of requests
  readReq->data = GC_MALLOC(sizeof(ReadData_t));
  ((ReadData_t *)readReq->data)->callback = callback;
  ((ReadData_t *)readReq->data)->readRequest = readReq;
  ((ReadData_t *)readReq->data)->openRequest = openReq;
  ((ReadData_t *)readReq->data)->dataBuffer = dataBuffer;
  ((ReadData_t *)readReq->data)->fileContent = (char*)"";
  ((ReadData_t *)readReq->data)->currentSize = 0;
  ((ReadData_t *)readReq->data)->readBytes = false;
  ((ReadData_t *)readReq->data)->canceled = false;
  ((ReadData_t *)readReq->data)->opened = false;
  ((ReadData_t *)readReq->data)->reading = false;
  ((ReadData_t *)readReq->data)->closed = false;

  openReq->data = readReq->data;

  // we open the file
  uv_fs_open(getLoop(), openReq, filepath, O_RDONLY, 0, onReadFileOpen);

  return (ReadData_t*) readReq->data;
}

ReadData_t *madlib__file__readBytes(char *filepath, PAP_t *callback) {
  // we allocate request objects and the buffer
  uv_fs_t *openReq = (uv_fs_t *)GC_MALLOC(sizeof(uv_fs_t));
  uv_fs_t *readReq = (uv_fs_t *)GC_MALLOC(sizeof(uv_fs_t));
  char *dataBuffer = (char *)GC_MALLOC(sizeof(char) * BUFFER_SIZE);

  // we allocate and initialize the data of requests
  readReq->data = GC_MALLOC(sizeof(ReadData_t));
  ((ReadData_t *)readReq->data)->callback = callback;
  ((ReadData_t *)readReq->data)->readRequest = readReq;
  ((ReadData_t *)readReq->data)->openRequest = openReq;
  ((ReadData_t *)readReq->data)->dataBuffer = dataBuffer;
  ((ReadData_t *)readReq->data)->currentSize = 0;
  ((ReadData_t *)readReq->data)->readBytes = true;
  ((ReadData_t *)readReq->data)->canceled = false;
  ((ReadData_t *)readReq->data)->opened = false;
  ((ReadData_t *)readReq->data)->reading = false;
  ((ReadData_t *)readReq->data)->closed = false;

  openReq->data = readReq->data;

  // we open the file
  uv_fs_open(getLoop(), openReq, filepath, O_RDONLY, 0, onReadFileOpen);

  return (ReadData_t*) readReq->data;
}


void madlib__file__cancelRead(ReadData_t *req) {
  req->canceled = true;

  if (req->reading) {
    uv_fs_t closeReq;
    uv_close((uv_handle_t*) req->readRequest, NULL);
    uv_fs_close(getLoop(), &closeReq, req->openRequest->result, NULL);
  } else if (req->opened) {
    uv_fs_t closeReq;
    uv_fs_close(getLoop(), &closeReq, req->openRequest->result, NULL);
  }
}

// write file


void onWriteError(uv_fs_t *req) {
  ((WriteData_t *)req->data)->closed = true;
  if (((WriteData_t *)req->data)->canceled) {
    return;
  }

  char *result = (char *)"\0";

  int64_t *boxedError = (int64_t *)libuvErrorToMadlibIOError(req->result);

  void *callback = ((WriteData_t *)req->data)->callback;

  uv_fs_t closeReq;
  uv_fs_close(getLoop(), &closeReq, ((WriteData_t *)req->data)->openRequest->result, NULL);

  // free resources
  GC_FREE(((WriteData_t *)req->data)->openRequest);
  GC_FREE(req->data);
  GC_FREE(req);


  __applyPAP__(callback, 2, boxedError, result);
}

void onWrite(uv_fs_t *req) {
  uv_fs_req_cleanup(req);

  if ((WriteData_t *)((WriteData_t *)req->data)->canceled) {
    return;
  }

  if (req->result < 0) {
    onWriteError(req);
  } else {

    WriteData_t *data = (WriteData_t *)req->data;
    void *callback = data->callback;
    int openResult = data->openRequest->result;

    // free resources
    GC_FREE(data->openRequest);
    GC_FREE(data);
    GC_FREE(req);

    // close file
    uv_fs_t closeReq;
    uv_fs_close(getLoop(), &closeReq, openResult, NULL);

    int64_t *boxedError = (int64_t *)0;

    // call the callback
    __applyPAP__(callback, 2, boxedError, NULL);
  }
}

void onWriteFileOpen(uv_fs_t *req) {
  uv_fs_req_cleanup(req);
  if ((WriteData_t *)((WriteData_t *)req->data)->canceled) {
    return;
  }
  ((ReadData_t *)req->data)->opened = true;

  if (req->result >= 0) {
    uv_fs_write(getLoop(), ((WriteData_t *)req->data)->writeRequest, req->result,
                &((WriteData_t *)req->data)->contentBuffer, 1, -1, onWrite);
    ((WriteData_t *)req->data)->writing = true;
  } else {
    onWriteError(req);
  }
}

// Callback receives unit in case of success
WriteData_t *madlib__file__write(char *filepath, char *content, PAP_t *callback) {
  // we allocate request objects and the buffer
  uv_fs_t *openReq = (uv_fs_t *)GC_MALLOC(sizeof(uv_fs_t));
  uv_fs_t *writeReq = (uv_fs_t *)GC_MALLOC(sizeof(uv_fs_t));

  // we allocate and initialize the data of requests
  writeReq->data = GC_MALLOC(sizeof(WriteData_t));
  ((WriteData_t *)writeReq->data)->callback = callback;
  ((WriteData_t *)writeReq->data)->writeRequest = writeReq;
  ((WriteData_t *)writeReq->data)->openRequest = openReq;
  ((WriteData_t *)writeReq->data)->contentBuffer.base = content;
  ((WriteData_t *)writeReq->data)->contentBuffer.len = strlen(content);
  ((WriteData_t *)writeReq->data)->canceled = false;
  ((WriteData_t *)writeReq->data)->opened = false;
  ((WriteData_t *)writeReq->data)->writing = false;
  ((WriteData_t *)writeReq->data)->closed = false;

  openReq->data = writeReq->data;

  // we open the file
  uv_fs_open(getLoop(), openReq, filepath, UV_FS_O_TRUNC | O_WRONLY | O_CREAT, S_IRUSR | S_IWUSR, onWriteFileOpen);

  return (WriteData_t*) openReq->data;
}

// Callback receives unit in case of success
WriteData_t *madlib__file__writeBytes(char *filepath, madlib__bytearray__ByteArray_t *content, PAP_t *callback) {
  // we allocate request objects and the buffer
  uv_fs_t *openReq = (uv_fs_t *)GC_MALLOC(sizeof(uv_fs_t));
  uv_fs_t *writeReq = (uv_fs_t *)GC_MALLOC(sizeof(uv_fs_t));

  // we allocate and initialize the data of requests
  writeReq->data = GC_MALLOC(sizeof(WriteData_t));
  ((WriteData_t *)writeReq->data)->callback = callback;
  ((WriteData_t *)writeReq->data)->writeRequest = writeReq;
  ((WriteData_t *)writeReq->data)->openRequest = openReq;
  ((WriteData_t *)writeReq->data)->contentBuffer.base = (char *)content->bytes;
  ((WriteData_t *)writeReq->data)->contentBuffer.len = content->length;
  ((WriteData_t *)writeReq->data)->canceled = false;
  ((WriteData_t *)writeReq->data)->opened = false;
  ((WriteData_t *)writeReq->data)->writing = false;
  ((WriteData_t *)writeReq->data)->closed = false;

  openReq->data = writeReq->data;

  // we open the file
  uv_fs_open(getLoop(), openReq, filepath, O_WRONLY | O_CREAT, S_IRUSR | S_IWUSR, onWriteFileOpen);

  return (WriteData_t*) openReq->data;
}

void madlib__file__cancelWrite(WriteData_t *req) {
  req->canceled = true;

  if (req->writing) {
    uv_fs_t closeReq;
    uv_close((uv_handle_t*) req->writeRequest, NULL);
    uv_fs_close(getLoop(), &closeReq, req->openRequest->result, NULL);
  } else if (req->opened) {
    uv_fs_t closeReq;
    uv_fs_close(getLoop(), &closeReq, req->openRequest->result, NULL);
  }
}


typedef struct FileExistData {
  void *callback;
} FileExistData_t;

void onFileExists(uv_fs_t *req) {
  uv_fs_req_cleanup(req);

  bool result = true;
  if (req->result < 0) {
    result = false;
  }

  __applyPAP__(((FileExistData_t *)req->data)->callback, 1, (bool*)result);

  // free memory
  GC_FREE(req->data);
  GC_FREE(req);
}


void madlib__file__exists(char *filepath, PAP_t *callback) {
  uv_fs_t *accessReq = (uv_fs_t *)GC_MALLOC(sizeof(uv_fs_t));

  accessReq->data = GC_MALLOC(sizeof(FileExistData_t));
  ((FileExistData_t *)accessReq->data)->callback = callback;

  uv_fs_access(getLoop(), accessReq, filepath, F_OK, onFileExists);
}

#ifdef __cplusplus
}
#endif
