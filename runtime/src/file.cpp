#include "file.hpp"

#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "event-loop.hpp"


#ifdef __cplusplus
extern "C" {
#endif

const size_t BUFFER_SIZE = 256 * 1024; // 256KB

static void appendReadChunk(ReadData_t *data, const char *chunk, int64_t chunkSize) {
  int64_t required = data->currentSize + chunkSize + 1;

  if (data->currentCapacity < required) {
    int64_t newCapacity = data->currentCapacity > 0 ? data->currentCapacity : (int64_t)BUFFER_SIZE;
    while (newCapacity < required) {
      newCapacity *= 2;
    }

    char *nextContent = (char *)GC_MALLOC_ATOMIC((size_t)newCapacity);
    if (data->currentSize > 0) {
      memcpy(nextContent, data->fileContent, (size_t)data->currentSize);
    }

    data->fileContent = nextContent;
    data->currentCapacity = newCapacity;
  }

  memcpy(data->fileContent + data->currentSize, chunk, (size_t)chunkSize);
  data->currentSize += chunkSize;
  data->fileContent[data->currentSize] = '\0';
}



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

  if (openRequest->result >= 0) {
    uv_fs_t closeReq;
    uv_fs_close(getLoop(), &closeReq, openRequest->result, NULL);
  }

  GC_FREE(dataBuffer);
  GC_FREE(data);
  GC_FREE(openRequest);
  GC_FREE(readRequest);

  __applyPAP__(callback, 2, boxedError, result);
}

void onRead(uv_fs_t *req) {
  uv_fs_req_cleanup(req);

  if (((ReadData_t *)req->data)->canceled) {
    return;
  }

  if (req->result < 0) {
    onReadError(req);
  } else if (req->result == 0) {
    ((ReadData_t *)req->data)->closed = true;
    // close file
    if (((ReadData_t *)req->data)->openRequest->result >= 0) {
      uv_fs_t closeReq;
      uv_fs_close(getLoop(), &closeReq, ((ReadData_t *)req->data)->openRequest->result, NULL);
    }

    int64_t *boxedError = (int64_t *)0;

    // box the result
    if (((ReadData_t *)req->data)->readBytes) {
      madlib__bytearray__ByteArray_t *arr =
          (madlib__bytearray__ByteArray_t *)GC_MALLOC(sizeof(madlib__bytearray__ByteArray_t));
      arr->bytes = (unsigned char *)((ReadData_t *)req->data)->fileContent;
      arr->length = ((ReadData_t *)req->data)->currentSize;
      arr->capacity = ((ReadData_t *)req->data)->currentSize;

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
    appendReadChunk((ReadData_t *)req->data, ((ReadData_t *)req->data)->dataBuffer, req->result);

    // we ask to be notified when the buffer has been filled again
    uv_fs_read(getLoop(), req, ((ReadData_t *)req->data)->openRequest->result, &((ReadData_t *)req->data)->uvBuffer, 1,
               -1, onRead);
  }
}

void onReadFileOpen(uv_fs_t *req) {
  uv_fs_req_cleanup(req);
  if (((ReadData_t *)req->data)->canceled) {
    return;
  }

  if (req->result >= 0) {
    ((ReadData_t *)req->data)->opened = true;
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
  char *dataBuffer = (char *)GC_MALLOC_ATOMIC(sizeof(char) * BUFFER_SIZE);

  // we allocate and initialize the data of requests
  readReq->data = GC_MALLOC(sizeof(ReadData_t));
  ((ReadData_t *)readReq->data)->callback = callback;
  ((ReadData_t *)readReq->data)->readRequest = readReq;
  ((ReadData_t *)readReq->data)->openRequest = openReq;
  ((ReadData_t *)readReq->data)->dataBuffer = dataBuffer;
  ((ReadData_t *)readReq->data)->fileContent = (char *)GC_MALLOC_ATOMIC(BUFFER_SIZE);
  ((ReadData_t *)readReq->data)->fileContent[0] = '\0';
  ((ReadData_t *)readReq->data)->currentCapacity = BUFFER_SIZE;
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


void madlib__file__cancelRead(ReadData_t *req) {
  req->canceled = true;

  if (req->reading) {
    uv_cancel((uv_req_t *)req->readRequest);
  } else if (!req->opened) {
    uv_cancel((uv_req_t *)req->openRequest);
  }

  if (req->opened && !req->closed && req->openRequest->result >= 0) {
    uv_fs_t closeReq;
    uv_fs_close(getLoop(), &closeReq, req->openRequest->result, NULL);
    req->closed = true;
  }
}


void onBufferedReadError(uv_fs_t *req) {
  ((BufferedReadData_t *)req->data)->closed = true;

  if (((BufferedReadData_t *)req->data)->canceled) {
    return;
  }

  char *result = (char*)"";

  int64_t *boxedError = (int64_t *)libuvErrorToMadlibIOError(req->result);

  void *callback = ((BufferedReadData_t *)req->data)->errorCallback;

  // free resources
  char *dataBuffer = ((BufferedReadData_t *)req->data)->dataBuffer;
  uv_fs_t *openRequest = ((BufferedReadData_t *)req->data)->openRequest;
  uv_fs_t *readRequest = ((BufferedReadData_t *)req->data)->readRequest;
  void *data = (BufferedReadData_t *)req->data;

  if (openRequest->result >= 0) {
    uv_fs_t closeReq;
    uv_fs_close(getLoop(), &closeReq, openRequest->result, NULL);
  }

  GC_FREE(dataBuffer);
  GC_FREE(data);
  GC_FREE(openRequest);
  GC_FREE(readRequest);

  __applyPAP__(callback, 1, boxedError);
}


void onBufferedRead(uv_fs_t *req) {
  uv_fs_req_cleanup(req);
  BufferedReadData_t *data = (BufferedReadData_t *)req->data;
  data->reading = false;

  if (data->canceled) {
    return;
  }

  if (req->result < 0) {
    onBufferedReadError(req);
  } else if (req->result == 0) {
    ((BufferedReadData_t *)req->data)->closed = true;
    // close file
    if (((BufferedReadData_t *)req->data)->openRequest->result >= 0) {
      uv_fs_t closeReq;
      uv_fs_close(getLoop(), &closeReq, ((BufferedReadData_t *)req->data)->openRequest->result, NULL);
    }

    __applyPAP__(((BufferedReadData_t *)req->data)->doneCallback, 1, NULL);

    // free resources
    char *dataBuffer = ((BufferedReadData_t *)req->data)->dataBuffer;
    uv_fs_t *openRequest = ((BufferedReadData_t *)req->data)->openRequest;
    uv_fs_t *readRequest = ((BufferedReadData_t *)req->data)->readRequest;
    void *data = (BufferedReadData_t *)req->data;

    GC_FREE(dataBuffer);
    GC_FREE(data);
    GC_FREE(openRequest);
    GC_FREE(readRequest);
  } else {
      madlib__bytearray__ByteArray_t *arr =
          (madlib__bytearray__ByteArray_t *)GC_MALLOC(sizeof(madlib__bytearray__ByteArray_t));
      arr->bytes = (unsigned char *)GC_MALLOC_ATOMIC(req->result);
      arr->length = req->result;
      arr->capacity = req->result;

    memcpy(arr->bytes, ((BufferedReadData_t *)req->data)->uvBuffer.base, req->result);
    __applyPAP__(((BufferedReadData_t *)req->data)->dataCallback, 1, arr);
  }
}


void madlib__file__readChunkFromFile(BufferedReadData_t *handle, PAP_t *dataCallback, PAP_t *errorCallback, PAP_t *doneCallback) {
  handle->dataCallback = dataCallback;
  handle->errorCallback = errorCallback;
  handle->doneCallback = doneCallback;
  handle->reading = true;
  int r = uv_fs_read(getLoop(), handle->readRequest, handle->openRequest->result, &handle->uvBuffer, 1, -1, onBufferedRead);
}

void onBufferedReadFileOpen(uv_fs_t *req) {
  uv_fs_req_cleanup(req);
  BufferedReadData_t *handle = (BufferedReadData_t *)req->data;

  if (handle->canceled) {
    return;
  }

  if (req->result >= 0) {
    handle->opened = true;
    uv_buf_t uvBuffer = uv_buf_init(handle->dataBuffer, BUFFER_SIZE);
    handle->uvBuffer = uvBuffer;

    __applyPAP__(((BufferedReadData_t *)req->data)->openCallback, 1, NULL);
  } else {
    onBufferedReadError(req);
  }
}


BufferedReadData_t *madlib__file__openFileForBufferedRead(char *filepath, PAP_t *openCallback, PAP_t *errorCallback) {
  // we allocate request objects and the buffer
  uv_fs_t *openReq = (uv_fs_t *)GC_MALLOC(sizeof(uv_fs_t));
  uv_fs_t *readReq = (uv_fs_t *)GC_MALLOC(sizeof(uv_fs_t));
  char *dataBuffer = (char *)GC_MALLOC_ATOMIC(sizeof(char) * BUFFER_SIZE);

  // we allocate and initialize the data of requests
  readReq->data = GC_MALLOC(sizeof(BufferedReadData_t));
  ((BufferedReadData_t *)readReq->data)->openCallback = openCallback;
  ((BufferedReadData_t *)readReq->data)->errorCallback = errorCallback;
  ((BufferedReadData_t *)readReq->data)->readRequest = readReq;
  ((BufferedReadData_t *)readReq->data)->openRequest = openReq;
  ((BufferedReadData_t *)readReq->data)->dataBuffer = dataBuffer;
  ((BufferedReadData_t *)readReq->data)->canceled = false;
  ((BufferedReadData_t *)readReq->data)->opened = false;
  ((BufferedReadData_t *)readReq->data)->reading = false;
  ((BufferedReadData_t *)readReq->data)->closed = false;

  openReq->data = readReq->data;

  // we open the file
  int r = uv_fs_open(getLoop(), openReq, filepath, O_RDONLY, 0, onBufferedReadFileOpen);

  if (r < 0) {
    onBufferedReadError(openReq);
  }

  return (BufferedReadData_t*) readReq->data;
}


void madlib__file__cancelBufferedRead(BufferedReadData_t *req) {
  req->canceled = true;

  if (req->reading) {
    uv_cancel((uv_req_t *)req->readRequest);
  } else if (!req->opened) {
    uv_cancel((uv_req_t *)req->openRequest);
  }

  if (req->opened && !req->closed && req->openRequest->result >= 0) {
    uv_fs_t closeReq;
    uv_fs_close(getLoop(), &closeReq, req->openRequest->result, NULL);
    req->closed = true;
  }
}


ReadData_t *madlib__file__readBytes(char *filepath, PAP_t *callback) {
  // we allocate request objects and the buffer
  uv_fs_t *openReq = (uv_fs_t *)GC_MALLOC(sizeof(uv_fs_t));
  uv_fs_t *readReq = (uv_fs_t *)GC_MALLOC(sizeof(uv_fs_t));
  char *dataBuffer = (char *)GC_MALLOC_ATOMIC(sizeof(char) * BUFFER_SIZE);

  // we allocate and initialize the data of requests
  readReq->data = GC_MALLOC(sizeof(ReadData_t));
  ((ReadData_t *)readReq->data)->callback = callback;
  ((ReadData_t *)readReq->data)->readRequest = readReq;
  ((ReadData_t *)readReq->data)->openRequest = openReq;
  ((ReadData_t *)readReq->data)->dataBuffer = dataBuffer;
  ((ReadData_t *)readReq->data)->fileContent = (char *)GC_MALLOC_ATOMIC(BUFFER_SIZE);
  ((ReadData_t *)readReq->data)->fileContent[0] = '\0';
  ((ReadData_t *)readReq->data)->currentCapacity = BUFFER_SIZE;
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


// write file


void onWriteError(uv_fs_t *req) {
  ((WriteData_t *)req->data)->closed = true;
  if (((WriteData_t *)req->data)->canceled) {
    return;
  }

  char *result = (char *)"\0";

  int64_t *boxedError = (int64_t *)libuvErrorToMadlibIOError(req->result);

  void *callback = ((WriteData_t *)req->data)->callback;

  if (((WriteData_t *)req->data)->openRequest->result >= 0) {
    uv_fs_t closeReq;
    uv_fs_close(getLoop(), &closeReq, ((WriteData_t *)req->data)->openRequest->result, NULL);
  }

  // free resources
  GC_FREE(((WriteData_t *)req->data)->openRequest);
  GC_FREE(req->data);
  GC_FREE(req);


  __applyPAP__(callback, 2, boxedError, result);
}

void onWrite(uv_fs_t *req) {
  uv_fs_req_cleanup(req);

  if (((WriteData_t *)req->data)->canceled) {
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
    if (openResult >= 0) {
      uv_fs_t closeReq;
      uv_fs_close(getLoop(), &closeReq, openResult, NULL);
    }

    int64_t *boxedError = (int64_t *)0;

    // call the callback
    __applyPAP__(callback, 2, boxedError, NULL);
  }
}

void onWriteFileOpen(uv_fs_t *req) {
  uv_fs_req_cleanup(req);
  if (((WriteData_t *)req->data)->canceled) {
    return;
  }
  if (req->result >= 0) {
    ((WriteData_t *)req->data)->opened = true;
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
  uv_fs_open(getLoop(), openReq, filepath, O_WRONLY | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR, onWriteFileOpen);

  return (WriteData_t*) openReq->data;
}

void madlib__file__cancelWrite(WriteData_t *req) {
  req->canceled = true;

  if (req->writing) {
    uv_cancel((uv_req_t *)req->writeRequest);
  } else if (!req->opened) {
    uv_cancel((uv_req_t *)req->openRequest);
  }

  if (req->opened && !req->closed && req->openRequest->result >= 0) {
    uv_fs_t closeReq;
    uv_fs_close(getLoop(), &closeReq, req->openRequest->result, NULL);
    req->closed = true;
  }
}


void onBufferedWriteError(uv_fs_t *req) {
  BufferedWriteData_t *handle = (BufferedWriteData_t *) req->data;
  handle->closed = true;
  if (handle->canceled) {
    return;
  }

  int64_t *boxedError = (int64_t *)libuvErrorToMadlibIOError(req->result);

  if (handle->openRequest->result >= 0) {
    uv_fs_t closeReq;
    uv_fs_close(getLoop(), &closeReq, handle->openRequest->result, NULL);
  }

  // free resources
  GC_FREE(handle->openRequest);
  GC_FREE(req->data);
  GC_FREE(req);

  __applyPAP__(handle->errorCallback, 1, boxedError);
}


void onBufferedWrite(uv_fs_t *req) {
  uv_fs_req_cleanup(req);
  BufferedWriteData_t *handle = (BufferedWriteData_t *)req->data;
  handle->writing = false;

  if (handle->canceled) {
    return;
  }

  if (req->result < 0) {
    onBufferedWriteError(req);
  } else {
    // call the callback
    __applyPAP__(handle->chunkWrittenCallback, 1, NULL);
  }
}


void madlib__file__writeChunkToFile(BufferedWriteData_t *handle,  madlib__bytearray__ByteArray_t *chunk, PAP_t *cb, PAP_t *errorCallback) {
  handle->chunkWrittenCallback = cb;
  handle->errorCallback = errorCallback;
  handle->writing = true;
  uv_buf_t contentBuffer;
  contentBuffer.base = (char *) chunk->bytes;
  contentBuffer.len = chunk->length;
  uv_fs_write(getLoop(), handle->writeRequest, handle->openRequest->result, &contentBuffer, 1, -1, onBufferedWrite);
}


void onBufferedWriteFileOpen(uv_fs_t *req) {
  uv_fs_req_cleanup(req);
  BufferedWriteData_t *handle = (BufferedWriteData_t *) req->data;

  if (handle->canceled) {
    return;
  }

  if (req->result >= 0) {
    handle->opened = true;
    __applyPAP__(handle->openCallback, 1, NULL);
  } else {
    onBufferedWriteError(req);
  }
}


// Callback receives unit in case of success
BufferedWriteData_t *madlib__file__openBufferedFileForWrite(char *filepath, PAP_t *openCallback, PAP_t *errorCallback) {
  // we allocate request objects and the buffer
  uv_fs_t *openReq = (uv_fs_t *)GC_MALLOC(sizeof(uv_fs_t));
  uv_fs_t *writeReq = (uv_fs_t *)GC_MALLOC(sizeof(uv_fs_t));

  // we allocate and initialize the data of requests
  writeReq->data = GC_MALLOC(sizeof(BufferedWriteData_t));
  ((BufferedWriteData_t *)writeReq->data)->openCallback = openCallback;
  ((BufferedWriteData_t *)writeReq->data)->errorCallback = errorCallback;
  ((BufferedWriteData_t *)writeReq->data)->writeRequest = writeReq;
  ((BufferedWriteData_t *)writeReq->data)->openRequest = openReq;
  ((BufferedWriteData_t *)writeReq->data)->canceled = false;
  ((BufferedWriteData_t *)writeReq->data)->opened = false;
  ((BufferedWriteData_t *)writeReq->data)->writing = false;
  ((BufferedWriteData_t *)writeReq->data)->closed = false;

  openReq->data = writeReq->data;

  // we open the file
  uv_fs_open(getLoop(), openReq, filepath, O_WRONLY | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR, onBufferedWriteFileOpen);

  return (BufferedWriteData_t*) openReq->data;
}


void madlib__file__cancelBufferedWrite(BufferedWriteData_t *req) {
  req->canceled = true;

  if (req->writing) {
    uv_cancel((uv_req_t *)req->writeRequest);
  } else if (!req->opened) {
    uv_cancel((uv_req_t *)req->openRequest);
  }

  if (req->opened && !req->closed && req->openRequest->result >= 0) {
    uv_fs_t closeReq;
    uv_fs_close(getLoop(), &closeReq, req->openRequest->result, NULL);
    req->closed = true;
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

  __applyPAP__(((FileExistData_t *)req->data)->callback, 1, (void *)(int64_t)result);

  // free memory
  GC_FREE(req->data);
  GC_FREE(req);
}


uv_fs_t *madlib__file__exists(char *filepath, PAP_t *callback) {
  uv_fs_t *accessReq = (uv_fs_t *)GC_MALLOC(sizeof(uv_fs_t));

  accessReq->data = GC_MALLOC(sizeof(FileExistData_t));
  ((FileExistData_t *)accessReq->data)->callback = callback;

  uv_fs_access(getLoop(), accessReq, filepath, F_OK, onFileExists);
  return accessReq;
}


void madlib__file__cancelExists(uv_fs_t *req) {
  if (req != NULL) {
    uv_cancel((uv_req_t*)req);
  }
}

#ifdef __cplusplus
}
#endif
