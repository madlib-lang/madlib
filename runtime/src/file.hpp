#ifndef FILESYSTEM_H
#define FILESYSTEM_H

#include <gc.h>
#include <uv.h>

#include "apply-pap.hpp"
#include "bytearray.hpp"

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

  bool canceled;
  bool opened;
  bool reading;
  bool closed;
} ReadData_t;

// read file
typedef struct BufferedReadData {
  void *openCallback;
  void *dataCallback;
  void *errorCallback;
  void *doneCallback;
  uv_fs_t *readRequest;
  uv_fs_t *openRequest;
  uv_buf_t uvBuffer;
  char *dataBuffer;

  bool canceled;
  bool opened;
  bool reading;
  bool closed;
} BufferedReadData_t;

typedef struct WriteData {
  void *callback;
  uv_fs_t *writeRequest;
  uv_fs_t *openRequest;
  uv_buf_t contentBuffer;

  bool canceled;
  bool opened;
  bool writing;
  bool closed;
} WriteData_t;

typedef struct BufferedWriteData {
  void *openCallback;
  void *chunkWrittenCallback;
  void *errorCallback;
  uv_fs_t *writeRequest;
  uv_fs_t *openRequest;

  bool canceled;
  bool opened;
  bool writing;
  bool closed;
} BufferedWriteData_t;

ReadData_t *madlib__file__read(char *filepath, PAP_t *callback);
ReadData_t *madlib__file__readBytes(char *filepath, PAP_t *callback);
void madlib__file__cancelRead(ReadData_t *req);


BufferedReadData_t *madlib__file__openFileForBufferedRead(char *filepath, PAP_t *openCallback, PAP_t *errorCallback);
void madlib__file__readChunkFromFile(BufferedReadData_t *handle, PAP_t *dataCallback, PAP_t *errorCallback, PAP_t *doneCallback);
void madlib__file__cancelBufferedRead(BufferedReadData_t *req);


WriteData_t *madlib__file__write(char *filepath, char *content, PAP_t *callback);
WriteData_t *madlib__file__writeBytes(char *filepath, madlib__bytearray__ByteArray_t *content, PAP_t *callback);
void madlib__file__cancelWrite(WriteData_t *req);


BufferedWriteData_t *madlib__file__openBufferedFileForWrite(char *filepath, PAP_t *openCallback, PAP_t *errorCallback);
void madlib__file__cancelBufferedWrite(BufferedWriteData_t *req);
void madlib__file__writeChunkToFile(BufferedWriteData_t *handle,  madlib__bytearray__ByteArray_t *chunk, PAP_t *cb, PAP_t *errorCallback);

void madlib__file__exists(char *filepath, PAP_t *callback);

#ifdef __cplusplus
}
#endif

#endif // FILESYSTEM_H
