
#include <gc.h>
#include <curl/curl.h>
#include <string.h>

#include "url.hpp"
#include "string_header.hpp"

#ifdef __cplusplus
extern "C" {
#endif

madlib__maybe__Maybe_t *madlib__url__encode(char *url) {
  CURL *curl = curl_easy_init();

  madlib__maybe__Maybe_t *result = (madlib__maybe__Maybe_t*)GC_MALLOC(sizeof(madlib__maybe__Maybe_t));
  if (curl) {
    char *output = curl_easy_escape(curl, url, 0);
    if (output) {
      size_t outputLength = strlen(output);
      char *data = madlib__string__alloc_bytes((uint32_t)outputLength);
      memcpy(data, output, outputLength);
      data[outputLength] = '\0';
      curl_free(output);

      result->data = data;
      result->index = madlib__maybe__Maybe_JUST_INDEX;
    }
    curl_easy_cleanup(curl);
  } else {
    result->data = NULL;
    result->index = madlib__maybe__Maybe_NOTHING_INDEX;
  }

  return result;
}

madlib__maybe__Maybe_t *madlib__url__decode(char *url) {
  CURL *curl = curl_easy_init();

  madlib__maybe__Maybe_t *result = (madlib__maybe__Maybe_t*)GC_MALLOC(sizeof(madlib__maybe__Maybe_t));

  if (curl) {
    char *output = curl_easy_unescape(curl, url, 0, NULL);
    if (output) {
      size_t outputLength = strlen(output);
      char *data = madlib__string__alloc_bytes((uint32_t)outputLength);
      memcpy(data, output, outputLength);
      data[outputLength] = '\0';
      curl_free(output);

      result->data = data;
      result->index = madlib__maybe__Maybe_JUST_INDEX;
    }
    curl_easy_cleanup(curl);
  } else {
    result->data = NULL;
    result->index = madlib__maybe__Maybe_NOTHING_INDEX;
  }

  return result;
}

#ifdef __cplusplus
}
#endif
