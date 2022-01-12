#ifndef HTTP_H
#define HTTP_H

#include "apply-pap.hpp"
#include "record.hpp"


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
void madlib__http__request(madlib__record__Record_t *request, PAP_t *callback);

#ifdef __cplusplus
}
#endif

#endif // HTTP_H
