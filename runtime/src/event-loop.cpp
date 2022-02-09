#include <gc.h>
#include <stdlib.h>
#include <uv.h>
#include <curl/curl.h>

#include <cmath>

#include "apply-pap.hpp"

static uv_loop_t *loop;

uv_loop_t *getLoop() {
  return loop;
}


#ifdef __cplusplus
extern "C" {
#endif


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

void __initEventLoop__() {
  GC_INIT();
  curl_global_init(CURL_GLOBAL_ALL);
  loop = (uv_loop_t *)GC_malloc_uncollectable(sizeof(uv_loop_t));
  uv_loop_init(loop);
}

void __startEventLoop__() {
  uv_run(loop, UV_RUN_DEFAULT);

  uv_loop_close(loop);
  GC_free(loop);
  curl_global_cleanup();
}

// set timeout
void forwardTimeoutCallback(uv_timer_t *handle) {
  __applyPAP__(handle->data, 1, NULL);
}

void __setTimeout__(PAP_t *pap, int64_t millis) {
  uv_timer_t *timer_req1 =
      (uv_timer_t *)GC_malloc_uncollectable(sizeof(uv_timer_t));
  timer_req1->data = (void *)pap;
  uv_timer_init(loop, timer_req1);
  uv_timer_start(timer_req1, forwardTimeoutCallback, millis, 0);
}

#ifdef __cplusplus
}
#endif
