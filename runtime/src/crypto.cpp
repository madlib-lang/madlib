
#include <gc.h>
#include "md5.hpp"
#include "sha256.hpp"
#include "string_header.hpp"
#include <cstring>

#ifdef __cplusplus
extern "C" {
#endif


char *madlib__crypto__md5(char *input) {
  std::string computed = __md5__(std::string(input));
  uint32_t len = (uint32_t)computed.length();
  char *result = madlib__string__alloc_bytes(len);
  memcpy(result, computed.c_str(), len);
  result[len] = '\0';
  return result;
}

char *madlib__crypto__sha256(char *input) {
  std::string computed = __sha256__(std::string(input));
  uint32_t len = (uint32_t)computed.length();
  char *result = madlib__string__alloc_bytes(len);
  memcpy(result, computed.c_str(), len);
  result[len] = '\0';
  return result;
}


#ifdef __cplusplus
}
#endif
