
#include <gc.h>
#include "md5.hpp"
#include "sha256.hpp"
#include <cstring>

#ifdef __cplusplus
extern "C" {
#endif


char *madlib__crypto__md5(char *input) {
  std::string computed = __md5__(std::string(input));
  char *result = (char*)GC_MALLOC_ATOMIC(sizeof(char) * (computed.length() + 1));
  memcpy(result, computed.c_str(), computed.length() + 1);
  return result;
}

char *madlib__crypto__sha256(char *input) {
  std::string computed = __sha256__(std::string(input));
  char *result = (char*)GC_MALLOC_ATOMIC(sizeof(char) * (computed.length() + 1));
  memcpy(result, computed.c_str(), computed.length() + 1);
  return result;
}


#ifdef __cplusplus
}
#endif
