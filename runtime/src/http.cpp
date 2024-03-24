#include "http.hpp"

#include <ctype.h>
#include <stdlib.h>
#include <uv.h>
#include <string.h>

#include "event-loop.hpp"
#include "bytearray.hpp"
#include "maybe.hpp"

#ifdef __cplusplus
extern "C" {
#endif




// utility
void toUpper(char *dest, char *src, size_t length) {
  for (int i = 0; i < length; i++) {
    dest[i] = toupper(src[i]);
  }
}

int64_t mapCurlErrorToClientErrorIndex(CURLcode errorCode) {
  int64_t index = madlib__http__ClientError_INTERNAL_ERROR_INDEX;
  switch (errorCode) {
    case 1:
      index = madlib__http__ClientError_ACCESS_DENIED_INDEX;
      break;
    case 3:
      index = madlib__http__ClientError_BAD_URL_INDEX;
      break;
    case 4:
      index = madlib__http__ClientError_NOT_SUPPORTED_INDEX;
      break;
    case 5:
      index = madlib__http__ClientError_UNRESOLVED_PROXY_INDEX;
      break;
    case 6:
      index = madlib__http__ClientError_ADDRESS_NOT_FOUND_INDEX;
      break;
    case 7:
      index = madlib__http__ClientError_CONNECTION_FAILED_INDEX;
      break;
    case 8:
      index = madlib__http__ClientError_MALFORMED_RESPONSE_INDEX;
      break;
    case 9:
      index = madlib__http__ClientError_ACCESS_DENIED_INDEX;
      break;
    case 16:
    case 92:
      index = madlib__http__ClientError_HTTP_2_FRAMING_ERROR_INDEX;
      break;
    case 28:
      index = madlib__http__ClientError_TIMEOUT_INDEX;
      break;
    case 35:
      index = madlib__http__ClientError_SSL_CONNECTION_FAILED_INDEX;
      break;
    case 47:
      index = madlib__http__ClientError_TOO_MANY_REDIRECTS_INDEX;
      break;
    case 53:
      index = madlib__http__ClientError_SSL_ENGINE_NOT_FOUND_INDEX;
      break;
    case 58:
      index = madlib__http__ClientError_INVALID_SSL_CERTIFICATE_INDEX;
      break;
    case 61:
      index = madlib__http__ClientError_BAD_TRANSFER_ENCODING_INDEX;
      break;
    case 66:
      index = madlib__http__ClientError_SSL_INITIALIZATION_FAILED_INDEX;
      break;
    default:
      index = madlib__http__ClientError_INTERNAL_ERROR_INDEX;
  }

  return index;
}

const char *methodToString(madlib__http__Method_t *method) {
  switch (method->methodIndex) {
    case 0:
      return "CONNECT";
      break;
    case 1:
      return "DELETE";
      break;
    case 2:
      return "GET";
      break;
    case 3:
      return "HEAD";
      break;
    case 4:
      return "OPTIONS";
      break;
    case 5:
      return "PATCH";
      break;
    case 6:
      return "POST";
      break;
    case 7:
      return "PUT";
      break;
    case 8:
      return "TRACE";
      break;
  }

  return "GET";
}


madlib__record__Record_t *buildResponse(void *boxedBody, madlib__list__Node_t *boxedHeaders, int64_t *boxedStatus) {
  madlib__record__Field_t *bodyField = (madlib__record__Field_t *)GC_MALLOC(sizeof(madlib__record__Field_t));
  madlib__record__Field_t *headerField = (madlib__record__Field_t *)GC_MALLOC(sizeof(madlib__record__Field_t));
  madlib__record__Field_t *statusField = (madlib__record__Field_t *)GC_MALLOC(sizeof(madlib__record__Field_t));

  bodyField->name = (char *)GC_MALLOC_ATOMIC(sizeof(char) * 5);
  strcpy(bodyField->name, "body");
  bodyField->value = boxedBody;

  headerField->name = (char *)GC_MALLOC_ATOMIC(sizeof(char) * 8);
  strcpy(headerField->name, "headers");
  headerField->value = boxedHeaders;

  statusField->name = (char *)GC_MALLOC_ATOMIC(sizeof(char) * 7);
  strcpy(statusField->name, "status");
  statusField->value = boxedStatus;

  return madlib__record__internal__buildRecord(3, NULL, bodyField, headerField, statusField);
}

void callCallback(RequestData_t *requestData, CURLcode curlCode) {
  curl_slist_free_all(requestData->requestHeaders);
  curl_multi_cleanup(requestData->curlHandle);
  curl_url_cleanup(requestData->curlURL);

  if (curlCode != CURLE_OK) {
    madlib__http__ClientError_1_t *clientError =
        (madlib__http__ClientError_1_t *)GC_MALLOC(sizeof(madlib__http__ClientError_1_t));
    clientError->index = mapCurlErrorToClientErrorIndex(curlCode);
    clientError->arg0 = (char*) curl_easy_strerror(curlCode);

    madlib__http__Error_ClientError_t *error =
        (madlib__http__Error_ClientError_t *)GC_MALLOC(sizeof(madlib__http__Error_ClientError_t));
    error->index = madlib__http__Error_CLIENT_ERROR_INDEX;
    error->clientError = clientError;

    void *badCallback = requestData->badCallback;

    __applyPAP__(badCallback, 1, error);
  } else {
    // box body
    void *boxedBody = NULL;
    if (requestData->asBytes) {
      boxedBody = GC_MALLOC(sizeof(madlib__bytearray__ByteArray_t));
      ((madlib__bytearray__ByteArray_t*)boxedBody)->bytes = (unsigned char*) requestData->body;
      ((madlib__bytearray__ByteArray_t*)boxedBody)->length = requestData->responseSize;
      ((madlib__bytearray__ByteArray_t*)boxedBody)->capacity = requestData->responseSize;
    } else {
      boxedBody = requestData->body;
    }

    // box headers
    madlib__list__Node_t *responseHeaders = requestData->headers;

    // box status
    int64_t *boxedStatus = (int64_t*)requestData->status;

    madlib__record__Record_t *response = buildResponse(boxedBody, responseHeaders, boxedStatus);

    void *goodCallback = requestData->goodCallback;
    void *badCallback = requestData->badCallback;
    long status = requestData->status;

    if (status < 400) {
      // call the good callback
      __applyPAP__(goodCallback, 1, response);
    } else {
      // call the bad callback
      madlib__http__ClientError_1_t *clientError =
        (madlib__http__ClientError_1_t *)GC_MALLOC(sizeof(madlib__http__ClientError_1_t));
      clientError->index = mapCurlErrorToClientErrorIndex(curlCode);
      clientError->arg0 = (char*) curl_easy_strerror(curlCode);

      madlib__http__Error_BadResponse_t *error =
          (madlib__http__Error_BadResponse_t *)GC_MALLOC(sizeof(madlib__http__Error_BadResponse_t));
      error->index = madlib__http__Error_BAD_RESPONSE_INDEX;
      error->response = response;

      __applyPAP__(badCallback, 1, error);
    }
  }
}


static void checkMultiInfo(RequestData_t *requestData) {
  CURLMsg *message;
  int pending;
  while ((message = curl_multi_info_read(requestData->curlHandle, &pending))) {
    if (message->msg == CURLMSG_DONE) {
      curl_easy_getinfo(message->easy_handle, CURLINFO_RESPONSE_CODE, &requestData->status);
      curl_multi_remove_handle(requestData->curlHandle, message->easy_handle);
      curl_easy_cleanup(message->easy_handle);

      callCallback(requestData, message->data.result);
      GC_FREE(requestData);
    }
  }
}


void onTimeout(uv_timer_t *req) {
  RequestData_t *requestData = (RequestData_t *)req->data;
  int runningHandles;
  CURLMcode actionResult;

  actionResult = curl_multi_socket_action(requestData->curlHandle, CURL_SOCKET_TIMEOUT, 0, &runningHandles);

  checkMultiInfo(requestData);
}


void onTimerClose(uv_handle_t *handle) {
  GC_FREE(handle);
}

int handleTimer(CURLM *multi, long timeoutMillis, void *userp) {
  uv_timer_t *handle = (uv_timer_t *)userp;
  if (timeoutMillis < 0) {
    uv_timer_stop(handle);
    if (!uv_is_closing((uv_handle_t *)handle)) {
      uv_close((uv_handle_t *)handle, onTimerClose);
    }
  } else {
    if (timeoutMillis == 0) {
      timeoutMillis = 1;
    }
    uv_timer_start(handle, onTimeout, timeoutMillis, 0);
  }
  
  return 0;
}


void handlePoll(uv_poll_t *req, int status, int events) {
  if (status < 0) {
    // TODO: handle?
  }

  RequestData_t *requestData = (RequestData_t *)req->data;
  int runningHandles;

  int flags = 0;
  if (events & UV_READABLE) flags |= CURL_CSELECT_IN;
  if (events & UV_WRITABLE) flags |= CURL_CSELECT_OUT;

  curl_multi_socket_action(requestData->curlHandle, requestData->curlSocket, flags, &runningHandles);

  checkMultiInfo(requestData);
}


void onHandleClose(uv_handle_t *handle) {
  GC_FREE(handle);
}


int handleSocket(CURL *easy, curl_socket_t socketfd, int action, void *userp, void *socketp) {
  RequestData_t *requestData = (RequestData_t *)userp;
  requestData->curlSocket = socketfd;

  uv_poll_t *handle = NULL;
  if (action == CURL_POLL_IN || action == CURL_POLL_OUT) {
    if (!socketp) {
      handle = (uv_poll_t*)GC_MALLOC_UNCOLLECTABLE(sizeof(uv_poll_t));
      handle->data = userp;
      uv_poll_init_socket(getLoop(), handle, socketfd);
    } else {
      handle = (uv_poll_t*) socketp;
    }
    curl_multi_assign(requestData->curlHandle, socketfd, (void *)handle);
  } else {
    handle = (uv_poll_t*) socketp;
  }

  switch (action) {
    case CURL_POLL_INOUT:
      uv_poll_start(handle, UV_WRITABLE | UV_READABLE, handlePoll);
      break;
    case CURL_POLL_IN:
      uv_poll_start(handle, UV_READABLE, handlePoll);
      break;
    case CURL_POLL_OUT:
      uv_poll_start(handle, UV_WRITABLE, handlePoll);
      break;
    case CURL_POLL_REMOVE:
      if (socketfd) {
        uv_poll_stop(handle);
        if (!uv_is_closing((uv_handle_t *)handle)) {
          uv_close((uv_handle_t*)handle, onHandleClose);
        }
        curl_multi_assign(requestData->curlHandle, socketfd, NULL);
      }
      break;
    default:
      abort();
      break;
  }

  return 0;
}


static size_t onDataWrite(void *data, size_t size, size_t nmemb, void *userp) {
  size_t realSize = size * nmemb;
  RequestData_t *requestData = (RequestData_t *)userp;

  char *ptr = (char *)GC_MALLOC_ATOMIC(requestData->responseSize + realSize + 1);
  memcpy(ptr, requestData->body, requestData->responseSize);

  requestData->body = ptr;
  memcpy(requestData->body + requestData->responseSize, data, realSize);
  requestData->responseSize += realSize;
  requestData->body[requestData->responseSize] = '\0';

  return realSize;
}


static size_t onHeaderWrite(void *data, size_t size, size_t nmemb, void *userp) {
  RequestData_t *requestData = (RequestData_t *)userp;
  char *strData = (char *)data;
  size_t realSize = size * nmemb;

  char *startOfValue = (char *)memchr(strData, ':', realSize);

  if (startOfValue == NULL) {
    return realSize;
  }

  madlib__http__Header_t *header = (madlib__http__Header_t *)GC_MALLOC(sizeof(madlib__http__Header_t));
  header->index = 0;

  size_t extraValueOffset = 0;
  size_t nameLength = startOfValue - strData;
  size_t valueLength = realSize - nameLength;

  while (strData[nameLength + extraValueOffset + 1] == ' ') {
    extraValueOffset += 1;
  }

  char *headerName = (char *)GC_MALLOC_ATOMIC(nameLength + 1);
  char *headerValue = (char *)GC_MALLOC_ATOMIC(valueLength + 1);

  strncpy(headerName, strData, nameLength);
  strncpy(headerValue, strData + nameLength + extraValueOffset + 1, valueLength - extraValueOffset - 3);

  headerName[nameLength] = '\0';
  headerValue[valueLength - extraValueOffset - 3] = '\0';

  header->name = headerName;
  header->value = headerValue;

  requestData->headers = madlib__list__push(header, requestData->headers);

  return realSize;
}


curl_slist *buildLibCurlHeaders(madlib__list__Node_t *headers) {
  curl_slist *lcurlHeaders = NULL;
  const char *headerTpl = "%s: %s";

  while (headers->next != NULL) {
    madlib__http__Header_t *boxedHeader = (madlib__http__Header_t *)headers->value;

    char uppercasedHeaderName[strlen(boxedHeader->name) + 1];
    toUpper(uppercasedHeaderName, boxedHeader->name, strlen(boxedHeader->name));

    if (strcmp(uppercasedHeaderName, "HOST") == 0) {
      // Host header is set automatically so we just skip it if the user
      // provided one
      headers = headers->next;
      continue;
    }

    char *headerStr =
        (char *)GC_MALLOC_ATOMIC_UNCOLLECTABLE(sizeof(char) * (3 + strlen(boxedHeader->name) + strlen(boxedHeader->value)));
    sprintf(headerStr, headerTpl, boxedHeader->name, boxedHeader->value);

    lcurlHeaders = curl_slist_append(lcurlHeaders, headerStr);
    GC_FREE(headerStr);
    headers = headers->next;
  }

  return lcurlHeaders;
}



RequestData_t *makeRequest(madlib__record__Record_t *request, PAP_t *badCallback, PAP_t *goodCallback, bool asBytes) {
  char *url = (char *)madlib__record__internal__selectField((char *)"url", request);

  madlib__http__Method_t *boxedMethod =
      (madlib__http__Method_t *)madlib__record__internal__selectField((char *)"method", request);
  const char *methodString = methodToString(boxedMethod);

  madlib__list__Node_t *requestHeaders =
      (madlib__list__Node_t *)madlib__record__internal__selectField((char *)"headers", request);
  curl_slist *lcurlHeaders = buildLibCurlHeaders(requestHeaders);

  madlib__maybe__Maybe_t *boxedBody =
      (madlib__maybe__Maybe_t *)madlib__record__internal__selectField((char *)"body", request);

  CURLM *multiHandle = curl_multi_init();

  RequestData_t *requestData = (RequestData_t *)GC_MALLOC_UNCOLLECTABLE(sizeof(RequestData_t));
  requestData->curlHandle = multiHandle;
  uv_timer_t *timerHandle = (uv_timer_t *)GC_MALLOC(sizeof(uv_timer_t));
  uv_timer_init(getLoop(), timerHandle);
  timerHandle->data = requestData;
  requestData->asBytes = asBytes;
  requestData->responseSize = 0;
  requestData->status = 0;
  requestData->headers = madlib__list__empty();
  requestData->badCallback = badCallback;
  requestData->goodCallback = goodCallback;
  requestData->requestHeaders = lcurlHeaders;
  requestData->body = (char *)GC_MALLOC_ATOMIC(sizeof(char) * 1);
  requestData->body[0] = '\0';

  curl_multi_setopt(multiHandle, CURLMOPT_SOCKETDATA, requestData);
  curl_multi_setopt(multiHandle, CURLMOPT_SOCKETFUNCTION, handleSocket);
  curl_multi_setopt(multiHandle, CURLMOPT_TIMERDATA, timerHandle);
  curl_multi_setopt(multiHandle, CURLMOPT_TIMERFUNCTION, handleTimer);

  CURL *handle = curl_easy_init();
  curl_easy_setopt(handle, CURLOPT_WRITEDATA, requestData);
  curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, onDataWrite);
  curl_easy_setopt(handle, CURLOPT_HEADERDATA, requestData);
  curl_easy_setopt(handle, CURLOPT_HEADERFUNCTION, onHeaderWrite);
  curl_easy_setopt(handle, CURLOPT_HTTPHEADER, lcurlHeaders);
  curl_easy_setopt(handle, CURLOPT_CUSTOMREQUEST, methodString);
  curl_easy_setopt(handle, CURLOPT_ACCEPT_ENCODING, "gzip,deflate,zstd");
  curl_easy_setopt(handle, CURLOPT_SSL_VERIFYPEER, 0);
  if (boxedBody->index == madlib__maybe__Maybe_JUST_INDEX) {
    if (asBytes) {
      curl_easy_setopt(handle, CURLOPT_POSTFIELDS, ((madlib__bytearray__ByteArray_t*)boxedBody->data)->bytes);
      curl_easy_setopt(handle, CURLOPT_POSTFIELDSIZE, ((madlib__bytearray__ByteArray_t*)boxedBody->data)->length);
    } else {
      curl_easy_setopt(handle, CURLOPT_POSTFIELDS, (char *)boxedBody->data);
      curl_easy_setopt(handle, CURLOPT_POSTFIELDSIZE, -1L);
    }
  }
  requestData->curlEasy = handle;

  CURLU *urlp = curl_url();
  CURLUcode urlParseResponse = curl_url_set(urlp, CURLUPART_URL, url, 0);

  requestData->curlURL = urlp;

  if (urlParseResponse == 0) {
    curl_easy_setopt(handle, CURLOPT_CURLU, urlp);
    curl_multi_add_handle(multiHandle, handle);
    return requestData;
  } else {
    // call bad callback
    madlib__http__ClientError_1_t *clientError =
        (madlib__http__ClientError_1_t *)GC_MALLOC(sizeof(madlib__http__ClientError_1_t));
    clientError->index = madlib__http__ClientError_BAD_URL_INDEX;
    clientError->arg0 = url;

    madlib__http__Error_ClientError_t *error =
        (madlib__http__Error_ClientError_t *)GC_MALLOC(sizeof(madlib__http__Error_ClientError_t));
    error->index = madlib__http__Error_CLIENT_ERROR_INDEX;
    error->clientError = clientError;

    __applyPAP__(requestData->badCallback, 1, error);

    // free resources
    curl_slist_free_all(requestData->requestHeaders);
    curl_multi_cleanup(requestData->curlHandle);
    curl_easy_cleanup(handle);
    curl_url_cleanup(urlp);
    GC_FREE(requestData);
    return NULL;
  }
}

// madlib__http__request :: Request String -> (Error String) -> (Response String -> ()) -> ()
RequestData_t *madlib__http__request(madlib__record__Record_t *request, PAP_t *badCallback, PAP_t *goodCallback) {
  return makeRequest(request, badCallback, goodCallback, false);
}

// madlib__http__requestBytes :: Request ByteArray -> (Error ByteArray) -> (Response ByteArray -> ()) -> ()
RequestData_t *madlib__http__requestBytes(madlib__record__Record_t *request, PAP_t *badCallback, PAP_t *goodCallback) {
  return makeRequest(request, badCallback, goodCallback, true);
}

void madlib__http__cancel(RequestData_t *requestData) {
  if (!requestData) {
    return;
  }

  curl_multi_remove_handle(requestData->curlHandle, requestData->curlEasy);
  curl_easy_cleanup(requestData->curlEasy);
  GC_FREE(requestData);
}

#ifdef __cplusplus
}
#endif
