#ifndef HTTP_H
#define HTTP_H

#include "apply-pap.hpp"
#include "record.hpp"


/**
 * type ClientError
 *   // libcurl 9
 *   = AccessDenied
 *   // libcurl 6
 *   | AddressNotFound
 *   // libcurl 3
 *   | BadUrl
 *   // libcurl 7
 *   | ConnectionFailed
 *   // libcurl 16, 92
 *   | Http2FramingError
 *   // libcurl 18
 *   | IncompleteResponse
 *   // all libcurl errors not fitting the other constructors 
 *   | InternalError
 *   // libcurl 58
 *   | InvalidSSLCertificate
 *   // libcurl 8
 *   | MalformedResponse
 *   // libcurl 4
 *   | NotSupported
 *   // libcurl 35
 *   | SSLConnectionFailed
 *   // libcurl 53
 *   | SSLEngineNotFound
 *   // libcurl 66
 *   | SSLInitializationFailed
 *   // libcurl 28
 *   | Timeout
 *   // libcurl 47
 *   | TooManyRedirects
 *   // libcurl 61
 *   | BadTransferEncoding
 *   // libcurl 5
 *   | UnresolvedProxy
 *   // libcurl 1
 *   | UnsupportedProtocol
 */
// ClientError indices
const int64_t madlib__http__ClientError_ACCESS_DENIED_INDEX = 0;
const int64_t madlib__http__ClientError_ADDRESS_NOT_FOUND_INDEX = 1;
const int64_t madlib__http__ClientError_BAD_TRANSFER_ENCODING_INDEX = 2;
const int64_t madlib__http__ClientError_BAD_URL_INDEX = 3;
const int64_t madlib__http__ClientError_CONNECTION_FAILED_INDEX = 4;
const int64_t madlib__http__ClientError_HTTP_2_FRAMING_ERROR_INDEX = 5;
const int64_t madlib__http__ClientError_INCOMPLETE_RESPONSE_INDEX = 6;
const int64_t madlib__http__ClientError_INTERNAL_ERROR_INDEX = 7;
const int64_t madlib__http__ClientError_INVALID_SSL_CERTIFICATE_INDEX = 8;
const int64_t madlib__http__ClientError_MALFORMED_RESPONSE_INDEX = 9;
const int64_t madlib__http__ClientError_NOT_SUPPORTED_INDEX = 10;
const int64_t madlib__http__ClientError_SSL_CONNECTION_FAILED_INDEX = 11;
const int64_t madlib__http__ClientError_SSL_ENGINE_NOT_FOUND_INDEX = 12;
const int64_t madlib__http__ClientError_SSL_INITIALIZATION_FAILED_INDEX = 13;
const int64_t madlib__http__ClientError_TIMEOUT_INDEX = 14;
const int64_t madlib__http__ClientError_TOO_MANY_REDIRECTS_INDEX = 15;
const int64_t madlib__http__ClientError_UNRESOLVED_PROXY_INDEX = 16;
const int64_t madlib__http__ClientError_UNSUPPORTED_PROTOCOL_INDEX = 17;

// unary constructors
typedef struct madlib__http__ClientError_1 {
  int64_t index;
  void *arg0;
} madlib__http__ClientError_1_t;

/**
 * type Error
 *   = BadResponse(Response)
 *   | ClientError(ClientError, Maybe Response)
 */
// Error indices
const int64_t madlib__http__Error_BAD_RESPONSE_INDEX = 0;
const int64_t madlib__http__Error_CLIENT_ERROR_INDEX = 1;

typedef struct madlib__http__Error_BadResponse {
  int64_t index; // 0
  void *response;
} madlib__http__Error_BadResponse_t;

typedef struct madlib__http__Error_ClientError {
  int64_t index; // 1
  void *clientError;
} madlib__http__Error_ClientError_t;

/**
 * type Header = Header(String, String)
 */
typedef struct madlib__http__Header {
  int64_t index;
  char **name;
  char **value;
} madlib__http__Header_t;

/**
 * alias Body = Maybe String
 */
typedef struct madlib__http__Body {
  /**
   * 0: Just
   * 1: Nothing
   */
  int64_t index;
  void *bodyData;
} madlib__http__Body_t;

/**
 * type Method
 *   = CONNECT
 *   | DELETE
 *   | GET
 *   | HEAD
 *   | OPTIONS
 *   | PATCH
 *   | POST
 *   | PUT
 *   | TRACE
 */
typedef struct madlib__http__Method {
  int64_t methodIndex;
} madlib__http__Method_t;

#ifdef __cplusplus
extern "C" {
#endif

/**
 * madlib__http__request :: Request -> (Response -> ()) -> ()
 */
void madlib__http__request(madlib__record__Record_t *request, PAP_t *badCallback, PAP_t *goodCallback);

#ifdef __cplusplus
}
#endif

#endif // HTTP_H
