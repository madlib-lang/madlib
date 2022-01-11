#ifndef HTTP_H
#define HTTP_H

#include "apply-pap.hpp"


/**
 * type Header = Header(String, String)
 */
typedef struct madlib__http__Header {
  int64_t index;
  char **name;
  char **value;
} madlib__http__Header_t;

#ifdef __cplusplus
extern "C" {
#endif

/**
 * madlib__http__request :: String -> (Response -> ()) -> ()
 */
void madlib__http__request(char *url, PAP_t *callback);

#ifdef __cplusplus
}
#endif

#endif // HTTP_H
