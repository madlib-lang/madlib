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

ReadData_t *madlib__file__read(char *filepath, PAP_t *callback);

ReadData_t *madlib__file__readBytes(char *filepath, PAP_t *callback);

void madlib__file__cancelRead(ReadData_t *req);
void madlib__file__cancelWrite(WriteData_t *req);

WriteData_t *madlib__file__write(char *filepath, char *content, PAP_t *callback);

WriteData_t *madlib__file__writeBytes(char *filepath, madlib__bytearray__ByteArray_t *content, PAP_t *callback);

void madlib__file__exists(char *filepath, PAP_t *callback);

#ifdef __cplusplus
}
#endif

#endif // FILESYSTEM_H
