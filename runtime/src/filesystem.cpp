#include <gc.h>
#include <stdlib.h>
#include <uv.h>

#include "filesystem.hpp"
#include "event-loop.hpp"


// libuv errors:

// UV_E2BIG
// argument list too long

// UV_EACCES
// permission denied

// UV_EADDRINUSE
// address already in use

// UV_EADDRNOTAVAIL
// address not available

// UV_EAFNOSUPPORT
// address family not supported

// UV_EAGAIN
// resource temporarily unavailable

// UV_EAI_ADDRFAMILY
// address family not supported

// UV_EAI_AGAIN
// temporary failure

// UV_EAI_BADFLAGS
// bad ai_flags value

// UV_EAI_BADHINTS
// invalid value for hints

// UV_EAI_CANCELED
// request canceled

// UV_EAI_FAIL
// permanent failure

// UV_EAI_FAMILY
// ai_family not supported

// UV_EAI_MEMORY
// out of memory

// UV_EAI_NODATA
// no address

// UV_EAI_NONAME
// unknown node or service

// UV_EAI_OVERFLOW
// argument buffer overflow

// UV_EAI_PROTOCOL
// resolved protocol is unknown

// UV_EAI_SERVICE
// service not available for socket type

// UV_EAI_SOCKTYPE
// socket type not supported

// UV_EALREADY
// connection already in progress

// UV_EBADF
// bad file descriptor

// UV_EBUSY
// resource busy or locked

// UV_ECANCELED
// operation canceled

// UV_ECHARSET
// invalid Unicode character

// UV_ECONNABORTED
// software caused connection abort

// UV_ECONNREFUSED
// connection refused

// UV_ECONNRESET
// connection reset by peer

// UV_EDESTADDRREQ
// destination address required

// UV_EEXIST
// file already exists

// UV_EFAULT
// bad address in system call argument

// UV_EFBIG
// file too large

// UV_EHOSTUNREACH
// host is unreachable

// UV_EINTR
// interrupted system call

// UV_EINVAL
// invalid argument

// UV_EIO
// i/o error

// UV_EISCONN
// socket is already connected

// UV_EISDIR
// illegal operation on a directory

// UV_ELOOP
// too many symbolic links encountered

// UV_EMFILE
// too many open files

// UV_EMSGSIZE
// message too long

// UV_ENAMETOOLONG
// name too long

// UV_ENETDOWN
// network is down

// UV_ENETUNREACH
// network is unreachable

// UV_ENFILE
// file table overflow

// UV_ENOBUFS
// no buffer space available

// UV_ENODEV
// no such device

// UV_ENOENT
// no such file or directory

// UV_ENOMEM
// not enough memory

// UV_ENONET
// machine is not on the network

// UV_ENOPROTOOPT
// protocol not available

// UV_ENOSPC
// no space left on device

// UV_ENOSYS
// function not implemented

// UV_ENOTCONN
// socket is not connected

// UV_ENOTDIR
// not a directory

// UV_ENOTEMPTY
// directory not empty

// UV_ENOTSOCK
// socket operation on non-socket

// UV_ENOTSUP
// operation not supported on socket

// UV_EOVERFLOW
// value too large for defined data type

// UV_EPERM
// operation not permitted

// UV_EPIPE
// broken pipe

// UV_EPROTO
// protocol error

// UV_EPROTONOSUPPORT
// protocol not supported

// UV_EPROTOTYPE
// protocol wrong type for socket

// UV_ERANGE
// result too large

// UV_EROFS
// read-only file system

// UV_ESHUTDOWN
// cannot send after transport endpoint shutdown

// UV_ESPIPE
// invalid seek

// UV_ESRCH
// no such process

// UV_ETIMEDOUT
// connection timed out

// UV_ETXTBSY
// text file is busy

// UV_EXDEV
// cross-device link not permitted

// UV_UNKNOWN
// unknown error

// UV_EOF
// end of file

// UV_ENXIO
// no such device or address

// UV_EMLINK
// too many links

// UV_ENOTTY
// inappropriate ioctl for device

// UV_EFTYPE
// inappropriate file type or format

// UV_EILSEQ
// illegal byte sequence

// UV_ESOCKTNOSUPPORT
// socket type not supported

// TODO: remove this and directly build an IOError out of a libuv error
int libuvErrorToMadlibIOError(int libuvError) {
  switch (libuvError) {
    case UV_E2BIG:
      return 1;
    case UV_EACCES:
      return 2;
    case UV_EADDRINUSE:
      return 3;
    case UV_EADDRNOTAVAIL:
      return 4;
    case UV_EAFNOSUPPORT:
      return 5;
    case UV_EAGAIN:
      return 6;
    case UV_EAI_ADDRFAMILY:
      return 7;
    case UV_EAI_AGAIN:
      return 8;
    case UV_EAI_BADFLAGS:
      return 9;
    case UV_EAI_BADHINTS:
      return 10;
    case UV_EAI_CANCELED:
      return 11;
    case UV_EAI_FAIL:
      return 12;
    case UV_EAI_FAMILY:
      return 13;
    case UV_EAI_MEMORY:
      return 14;
    case UV_EAI_NODATA:
      return 15;
    case UV_EAI_NONAME:
      return 16;
    case UV_EAI_OVERFLOW:
      return 17;
    case UV_EAI_PROTOCOL:
      return 18;
    case UV_EAI_SERVICE:
      return 19;
    case UV_EAI_SOCKTYPE:
      return 20;
    case UV_EALREADY:
      return 21;
    case UV_EBADF:
      return 22;
    case UV_EBUSY:
      return 23;
    case UV_ECANCELED:
      return 24;
    case UV_ECHARSET:
      return 25;
    case UV_ECONNABORTED:
      return 26;
    case UV_ECONNREFUSED:
      return 27;
    case UV_ECONNRESET:
      return 28;
    case UV_EDESTADDRREQ:
      return 29;
    case UV_EEXIST:
      return 30;
    case UV_EFAULT:
      return 31;
    case UV_EFBIG:
      return 32;
    case UV_EHOSTUNREACH:
      return 33;
    case UV_EINTR:
      return 34;
    case UV_EINVAL:
      return 35;
    case UV_EIO:
      return 36;
    case UV_EISCONN:
      return 37;
    case UV_EISDIR:
      return 38;
    case UV_ELOOP:
      return 39;
    case UV_EMFILE:
      return 40;
    case UV_EMSGSIZE:
      return 41;
    case UV_ENAMETOOLONG:
      return 42;
    case UV_ENETDOWN:
      return 43;
    case UV_ENETUNREACH:
      return 44;
    case UV_ENFILE:
      return 45;
    case UV_ENOBUFS:
      return 46;
    case UV_ENODEV:
      return 47;
    case UV_ENOENT:
      return 48;
    case UV_ENOMEM:
      return 49;
    case UV_ENONET:
      return 50;
    case UV_ENOPROTOOPT:
      return 51;
    case UV_ENOSPC:
      return 52;
    case UV_ENOSYS:
      return 53;
    case UV_ENOTCONN:
      return 54;
    case UV_ENOTDIR:
      return 55;
    case UV_ENOTEMPTY:
      return 56;
    case UV_ENOTSOCK:
      return 57;
    case UV_ENOTSUP:
      return 58;
    case UV_EPERM:
      return 59;
    case UV_EPIPE:
      return 60;
    case UV_EPROTO:
      return 61;
    case UV_EPROTONOSUPPORT:
      return 62;
    case UV_EPROTOTYPE:
      return 63;
    case UV_ERANGE:
      return 64;
    case UV_EROFS:
      return 65;
    case UV_ESHUTDOWN:
      return 66;
    case UV_ESPIPE:
      return 67;
    case UV_ESRCH:
      return 68;
    case UV_ETIMEDOUT:
      return 69;
    case UV_ETXTBSY:
      return 70;
    case UV_EXDEV:
      return 71;
    case UV_UNKNOWN:
      return 72;
    case UV_EOF:
      return 73;
    case UV_ENXIO:
      return 74;
    case UV_EMLINK:
      return 75;
    case UV_ENOTTY:
      return 76;
    case UV_EFTYPE:
      return 77;
    case UV_EILSEQ:
      return 78;
    default:
      return 200;
  }
}

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

  // if true returns an Array Byte instead of String
  bool readBytes;
} ReadData_t;

void onReadError(uv_fs_t *req) {
  char **boxedResult = (char **)GC_malloc_uncollectable(sizeof(char *));
  char *result = (char *)GC_malloc_uncollectable(sizeof(char));
  *result = 0;
  *boxedResult = result;

  int64_t *boxedError = (int64_t *)GC_malloc_uncollectable(sizeof(int));
  *boxedError = libuvErrorToMadlibIOError(req->result);

  // free resources
  GC_free(((ReadData_t *)req->data)->dataBuffer);
  GC_free(((ReadData_t *)req->data)->openRequest);
  GC_free(req->data);
  GC_free(req);

  __applyPAP__(((ReadData_t *)req->data)->callback, 2, boxedError, boxedResult);
}

void onRead(uv_fs_t *req) {
  uv_fs_req_cleanup(req);
  if (req->result < 0) {
    onReadError(req);
  } else if (req->result == 0) {
    // close file
    uv_fs_t closeReq;
    uv_fs_close(getLoop(), &closeReq, ((ReadData_t *)req->data)->openRequest->result,
                NULL);

    int64_t *boxedError = (int64_t *)GC_malloc_uncollectable(sizeof(int64_t));
    *boxedError = 0;

    // box the result
    if (((ReadData_t *)req->data)->readBytes) {
      madlib__array__Array_t *arr =
          (madlib__array__Array_t *)GC_malloc_uncollectable(sizeof(madlib__array__Array_t));
      int64_t contentLength = ((ReadData_t *)req->data)->currentSize;
      arr->items =
          (void **)GC_malloc_uncollectable(sizeof(void *) * contentLength);
      arr->length = contentLength;

      for (int64_t i = 0; i < contentLength; i++) {
        (arr->items)[i] = (char *)GC_malloc_uncollectable(sizeof(char));
        memcpy((char *)*(arr->items + i),
               ((ReadData_t *)req->data)->fileContent + i, sizeof(char));
      }

      GC_free(((ReadData_t *)req->data)->fileContent);

      __applyPAP__(((ReadData_t *)req->data)->callback, 2, boxedError,
                   (void *)arr);
    } else {
      char **boxedResult = (char **)GC_malloc_uncollectable(sizeof(char *));
      *boxedResult = ((ReadData_t *)req->data)->fileContent;

      // call the callback
      __applyPAP__(((ReadData_t *)req->data)->callback, 2, boxedError,
                   boxedResult);
    }

    // free resources
    GC_free(((ReadData_t *)req->data)->dataBuffer);
    GC_free(((ReadData_t *)req->data)->openRequest);
    GC_free(req->data);
    GC_free(req);
  } else if (req->result > 0) {
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
    memcpy(nextContent + currentSize, ((ReadData_t *)req->data)->dataBuffer,
           req->result);

    GC_free(((ReadData_t *)req->data)->fileContent);

    // we assign the fileContent to the newly created structure
    ((ReadData_t *)req->data)->fileContent = nextContent;

    // we ask to be notified when the buffer has been filled again
    uv_fs_read(getLoop(), req, ((ReadData_t *)req->data)->openRequest->result,
               &((ReadData_t *)req->data)->uvBuffer, 1, -1, onRead);
  }
}

void onReadFileOpen(uv_fs_t *req) {
  uv_fs_req_cleanup(req);
  if (req->result >= 0) {
    uv_buf_t uvBuffer =
        uv_buf_init(((ReadData_t *)req->data)->dataBuffer, 1024);
    ((ReadData_t *)((ReadData_t *)req->data)->readRequest->data)->uvBuffer =
        uvBuffer;
    uv_fs_read(getLoop(), ((ReadData_t *)req->data)->readRequest, req->result,
               &uvBuffer, 1, -1, onRead);
  } else {
    onReadError(req);
  }
}

void madlib__filesystem__readFile(char *filepath, PAP_t *callback) {
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

void madlib__filesystem__readBinaryFile(char *filepath, PAP_t *callback) {
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
  char **boxedResult = (char **)GC_malloc_uncollectable(sizeof(char *));
  char *result = (char *)GC_malloc_uncollectable(sizeof(char));
  *result = 0;
  *boxedResult = result;

  int64_t *boxedError = (int64_t *)GC_malloc_uncollectable(sizeof(int));
  *boxedError = libuvErrorToMadlibIOError(req->result);

  // free resources
  GC_free(((WriteData_t *)req->data)->openRequest);
  GC_free(req->data);
  GC_free(req);

  __applyPAP__(((WriteData_t *)req->data)->callback, 2, boxedError, boxedResult);
}

void onWrite(uv_fs_t *req) {
  uv_fs_req_cleanup(req);
  if (req->result < 0) {
    onWriteError(req);
  } else {
    // close file
    uv_fs_t closeReq;
    uv_fs_close(getLoop(), &closeReq,
                ((WriteData_t *)req->data)->openRequest->result, NULL);

    int64_t *boxedError = (int64_t *)GC_malloc_uncollectable(sizeof(int64_t));
    *boxedError = 0;

    // call the callback
    __applyPAP__(((WriteData_t *)req->data)->callback, 2, boxedError,
                 NULL);

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
void madlib__filesystem__writeFile(char *filepath, char *content, PAP_t *callback) {
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
  uv_fs_open(getLoop(), openReq, filepath, O_WRONLY | O_CREAT, S_IRUSR | S_IWUSR,
             onWriteFileOpen);
}

// Callback receives unit in case of success
void madlib__filesystem__writeBinaryFile(char *filepath, madlib__array__Array_t *content, PAP_t *callback) {
  // we allocate request objects and the buffer
  uv_fs_t *openReq = (uv_fs_t *)GC_malloc_uncollectable(sizeof(uv_fs_t));
  uv_fs_t *readReq = (uv_fs_t *)GC_malloc_uncollectable(sizeof(uv_fs_t));

  // unbox bytes from array
  char *bytes = (char *)GC_malloc_uncollectable(sizeof(char) * content->length);
  for (int i = 0; i < content->length; i++) {
    bytes[i] = *((char *)content->items[i]);
  }

  // we allocate and initialize the data of requests
  readReq->data = GC_malloc_uncollectable(sizeof(WriteData_t));
  ((WriteData_t *)readReq->data)->callback = callback;
  ((WriteData_t *)readReq->data)->readRequest = readReq;
  ((WriteData_t *)readReq->data)->openRequest = openReq;
  ((WriteData_t *)readReq->data)->contentBuffer.base = bytes;
  ((WriteData_t *)readReq->data)->contentBuffer.len = content->length;

  openReq->data = readReq->data;

  // we open the file
  uv_fs_open(getLoop(), openReq, filepath, O_WRONLY | O_CREAT, S_IRUSR | S_IWUSR,
             onWriteFileOpen);
}



typedef struct FileExistData {
  void *callback;
} FileExistData_t;

void onFileExists(uv_fs_t *req) {
  uv_fs_req_cleanup(req);

  bool *result = (bool*)GC_malloc(sizeof(bool));
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

void madlib__filesystem__fileExists(char *filepath, PAP_t *callback) {
  uv_fs_t *accessReq = (uv_fs_t *)GC_malloc_uncollectable(sizeof(uv_fs_t));

  accessReq->data = GC_malloc_uncollectable(sizeof(FileExistData_t));
  ((FileExistData_t *)accessReq->data)->callback = callback;

  uv_fs_access(getLoop(), accessReq, filepath, UV_FS_O_NOATIME, onFileExists);
}

#ifdef __cplusplus
}
#endif
