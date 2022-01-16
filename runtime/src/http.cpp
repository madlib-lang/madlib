#include "http.hpp"

#include <ctype.h>
#include <curl/curl.h>
#include <gc.h>
#include <http_parser.h>
#include <stdlib.h>
#include <uv.h>

#include "event-loop.hpp"
#include "list.hpp"
#include "maybe.hpp"

#ifdef __cplusplus
extern "C" {
#endif

// static const int CURL_ID = curl_global_init(CURL_GLOBAL_ALL);
static const int64_t JUST = 0;

typedef struct RequestData {
  // Request
  curl_slist *requestHeaders;
  void *goodCallback;
  void *badCallback;

  // Internals
  uv_poll_t *pollHandle;
  uv_timer_t *timerHandle;
  CURLM *curlHandle;
  CURLU *curlUrl;
  curl_socket_t curlSocket;

  // Response
  size_t responseSize;
  char *body;
  long status;
  // List Header
  madlib__list__Node_t *headers;
} RequestData_t;

// utility
void toUpper(char *dest, char *src, size_t length) {
  for (int i = 0; i < length; i++) {
    dest[i] = toupper(src[i]);
  }
}

madlib__record__Record_t *buildResponse(char **boxedBody,
                                        madlib__list__Node_t **boxedHeaders,
                                        int64_t *boxedStatus) {
  madlib__record__Field_t *bodyField =
      (madlib__record__Field_t *)GC_malloc(sizeof(madlib__record__Field_t));
  madlib__record__Field_t *headerField =
      (madlib__record__Field_t *)GC_malloc(sizeof(madlib__record__Field_t));
  madlib__record__Field_t *statusField =
      (madlib__record__Field_t *)GC_malloc(sizeof(madlib__record__Field_t));

  bodyField->name = (char *)GC_malloc(sizeof(char) * 5);
  strcpy(bodyField->name, "body");
  bodyField->value = boxedBody;

  headerField->name = (char *)GC_malloc(sizeof(char) * 8);
  strcpy(headerField->name, "headers");
  headerField->value = boxedHeaders;

  statusField->name = (char *)GC_malloc(sizeof(char) * 7);
  strcpy(statusField->name, "status");
  statusField->value = boxedStatus;

  return madlib__record__internal__buildRecord(3, NULL, bodyField, headerField,
                                               statusField);
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

void onTimeout(uv_timer_t *req) {
  RequestData_t *requestData = (RequestData_t *)req->data;
  int runningHandles;

  curl_multi_socket_action(requestData->curlHandle, requestData->curlSocket, 0,
                           &runningHandles);

  if (runningHandles == 0) {
    curl_slist_free_all(requestData->requestHeaders);
    curl_multi_cleanup(requestData->curlHandle);
    curl_url_cleanup(requestData->curlUrl);
    GC_free(requestData->pollHandle);
    GC_free(requestData->timerHandle);
    GC_free(requestData);
  }
}

void handleTimer(CURLM *multi, long timeoutMillis, void *userp) {
  RequestData_t *requestData = (RequestData_t *)userp;
  if (timeoutMillis <= 0) {
    // 0 means directly call socket_action, but we'll do it in a bit
    timeoutMillis = 1;
  }
  if (uv_timer_get_due_in(requestData->timerHandle) == 0) {
    uv_timer_start(requestData->timerHandle, onTimeout, timeoutMillis, 0);
  }
}

void callCallback(RequestData_t *requestData) {
  // box body
  char **boxedBody = (char **)GC_malloc(sizeof(char *));
  *boxedBody = requestData->body;

  // box headers
  madlib__list__Node_t **boxedHeaders =
      (madlib__list__Node_t **)GC_malloc(sizeof(madlib__list__Node_t *));
  *boxedHeaders = requestData->headers;

  // box status
  int64_t *boxedStatus = (int64_t *)GC_malloc(sizeof(int64_t));
  *boxedStatus = requestData->status;

  // call the callback
  __applyPAP__(requestData->goodCallback, 1,
               buildResponse(boxedBody, boxedHeaders, boxedStatus));
}

void handlePoll(uv_poll_t *req, int status, int events) {
  RequestData_t *requestData = (RequestData_t *)req->data;
  uv_timer_stop(requestData->timerHandle);
  int runningHandles;

  int flags = 0;
  if (events & UV_READABLE) flags |= CURL_CSELECT_IN;
  if (events & UV_WRITABLE) flags |= CURL_CSELECT_OUT;

  curl_multi_socket_action(requestData->curlHandle, requestData->curlSocket,
                           flags, &runningHandles);

  CURLMsg *message;
  int pending;
  while ((message = curl_multi_info_read(requestData->curlHandle, &pending))) {
    if (message->msg == CURLMSG_DONE) {
      curl_easy_getinfo(message->easy_handle, CURLINFO_RESPONSE_CODE,
                        &requestData->status);
      curl_easy_cleanup(message->easy_handle);
      curl_multi_remove_handle(requestData->curlHandle, message->easy_handle);

      // message->data.result == CURLE_OK
      printf("result: %d\n", message->data.result);
      callCallback(requestData);
    }
  }
}

int handleSocket(CURL *easy, curl_socket_t socketfd, int action, void *userp,
                 void *socketp) {
  RequestData_t *requestData = (RequestData_t *)userp;
  requestData->curlSocket = socketfd;

  if (action == CURL_POLL_IN || action == CURL_POLL_OUT) {
    if (!socketp) {
      uv_poll_init_socket(getLoop(), requestData->pollHandle, socketfd);
      requestData->pollHandle->data = requestData;
    }
    curl_multi_assign(requestData->curlHandle, socketfd,
                      (void *)requestData->pollHandle);
  }

  switch (action) {
    case CURL_POLL_INOUT:
      uv_poll_start(requestData->pollHandle, UV_WRITABLE | UV_READABLE,
                    handlePoll);
      break;
    case CURL_POLL_IN:
      uv_poll_start(requestData->pollHandle, UV_READABLE, handlePoll);
      break;
    case CURL_POLL_OUT:
      uv_poll_start(requestData->pollHandle, UV_WRITABLE, handlePoll);
      break;
    case CURL_POLL_REMOVE:
      if (socketfd) {
        uv_poll_stop(requestData->pollHandle);
        curl_multi_assign(requestData->curlHandle, socketfd, NULL);
      }
      break;
    default:
      abort();
  }

  return 0;
}

static size_t onDataWrite(void *data, size_t size, size_t nmemb, void *userp) {
  size_t realSize = size * nmemb;
  RequestData_t *requestData = (RequestData_t *)userp;

  char *ptr = (char *)GC_realloc(requestData->body,
                                 requestData->responseSize + realSize + 1);
  // out of memory
  if (ptr == NULL) return 0;

  requestData->body = ptr;
  memcpy(&(requestData->body[requestData->responseSize]), data, realSize);
  requestData->responseSize += realSize;
  requestData->body[requestData->responseSize] = 0;

  return realSize;
}

static size_t onHeaderWrite(void *data, size_t size, size_t nmemb,
                            void *userp) {
  RequestData_t *requestData = (RequestData_t *)userp;
  char *strData = (char *)data;
  size_t realSize = size * nmemb;

  char *startOfValue = (char *)memchr(strData, ':', realSize);

  if (startOfValue == NULL) {
    return realSize;
  }

  madlib__http__Header_t *header =
      (madlib__http__Header_t *)GC_malloc(sizeof(madlib__http__Header_t));
  header->index = 0;
  header->name = (char **)GC_malloc(sizeof(char *));
  header->value = (char **)GC_malloc(sizeof(char *));

  size_t extraValueOffset = 0;
  size_t nameLength = startOfValue - strData;
  size_t valueLength = realSize - nameLength;

  while (strData[nameLength + extraValueOffset + 1] == ' ') {
    extraValueOffset += 1;
  }

  char *headerName = (char *)GC_malloc(nameLength + 1);
  char *headerValue = (char *)GC_malloc(valueLength + 1);

  strncpy(headerName, strData, nameLength);
  strncpy(headerValue, strData + nameLength + extraValueOffset + 1,
          valueLength - extraValueOffset - 3);

  *header->name = headerName;
  *header->value = headerValue;

  requestData->headers = madlib__list__push(header, requestData->headers);

  return realSize;
}

curl_slist *buildLibCurlHeaders(madlib__list__Node_t *headers) {
  curl_slist *lcurlHeaders = NULL;
  const char *headerTpl = "%s: %s";

  while (headers->value != NULL) {
    madlib__http__Header_t *boxedHeader =
        (madlib__http__Header_t *)headers->value;

    char uppercasedHeaderName[strlen(*boxedHeader->name) + 1];
    toUpper(uppercasedHeaderName, *boxedHeader->name,
            strlen(*boxedHeader->name));

    if (strcmp(uppercasedHeaderName, "HOST") == 0) {
      // Host header is set automatically so we just skip it if the user
      // provided one
      headers = headers->next;
      continue;
    }

    char *headerStr = (char *)GC_malloc_uncollectable(
        sizeof(char) *
        (3 + strlen(*boxedHeader->name) + strlen(*boxedHeader->value)));
    sprintf(headerStr, headerTpl, *boxedHeader->name, *boxedHeader->value);

    lcurlHeaders = curl_slist_append(lcurlHeaders, headerStr);
    GC_free(headerStr);
    headers = headers->next;
  }

  return lcurlHeaders;
}

// madlib__http__request :: Request -> (Error -> {}) -> (Response -> {}) -> {}
void madlib__http__request(madlib__record__Record_t *request, PAP_t *badCallback, PAP_t *goodCallback) {
  char **boxedUrl =
      (char **)madlib__record__internal__selectField((char *)"url", request);
  char *url = *boxedUrl;

  madlib__http__Method_t *boxedMethod =
      (madlib__http__Method_t *)madlib__record__internal__selectField(
          (char *)"method", request);
  const char *methodString = methodToString(boxedMethod);

  madlib__list__Node_t **boxedHeaders =
      (madlib__list__Node_t **)madlib__record__internal__selectField(
          (char *)"headers", request);
  curl_slist *lcurlHeaders = buildLibCurlHeaders(*boxedHeaders);

  madlib__maybe__Maybe_Just_t *boxedBody =
      (madlib__maybe__Maybe_Just_t *)madlib__record__internal__selectField(
          (char *)"body", request);

  CURLM *multiHandle = curl_multi_init();

  RequestData_t *requestData =
      (RequestData_t *)GC_malloc_uncollectable(sizeof(RequestData_t));
  requestData->curlHandle = multiHandle;
  requestData->timerHandle =
      (uv_timer_t *)GC_malloc_uncollectable(sizeof(uv_timer_t));
  requestData->pollHandle =
      (uv_poll_t *)GC_malloc_uncollectable(sizeof(uv_poll_t));
  uv_timer_init(getLoop(), requestData->timerHandle);
  requestData->timerHandle->data = requestData;
  requestData->headers = madlib__list__empty();
  requestData->badCallback = badCallback;
  requestData->goodCallback = goodCallback;
  requestData->requestHeaders = lcurlHeaders;
  requestData->body = (char *)GC_malloc(sizeof(char) * 1);
  requestData->body[0] = '\0';

  curl_multi_setopt(multiHandle, CURLMOPT_SOCKETDATA, requestData);
  curl_multi_setopt(multiHandle, CURLMOPT_SOCKETFUNCTION, handleSocket);
  curl_multi_setopt(multiHandle, CURLMOPT_TIMERDATA, requestData);
  curl_multi_setopt(multiHandle, CURLMOPT_TIMERFUNCTION, handleTimer);

  CURL *handle = curl_easy_init();
  curl_easy_setopt(handle, CURLOPT_WRITEDATA, requestData);
  curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, onDataWrite);
  curl_easy_setopt(handle, CURLOPT_HEADERDATA, requestData);
  curl_easy_setopt(handle, CURLOPT_HEADERFUNCTION, onHeaderWrite);
  curl_easy_setopt(handle, CURLOPT_HTTPHEADER, lcurlHeaders);
  curl_easy_setopt(handle, CURLOPT_CUSTOMREQUEST, methodString);
  curl_easy_setopt(handle, CURLOPT_ACCEPT_ENCODING, "gzip,deflate,zstd");
  if (boxedBody->index == madlib__maybe__Maybe_JUST_INDEX) {
    curl_easy_setopt(handle, CURLOPT_POSTFIELDS,
                     *((char **)boxedBody->data));
    curl_easy_setopt(handle, CURLOPT_POSTFIELDSIZE, -1L);
  }

  CURLU *urlp = curl_url();
  CURLUcode urlParseResponse = curl_url_set(urlp, CURLUPART_URL, url, 0);

  if (urlParseResponse == 0) {
    requestData->curlUrl = urlp;
    curl_easy_setopt(handle, CURLOPT_CURLU, urlp);
    curl_multi_add_handle(multiHandle, handle);
  } else {
    printf("bad url\n");


    // call bad callback
    madlib__maybe__Maybe_Nothing_t* nothing = (madlib__maybe__Maybe_Nothing_t*)GC_malloc(sizeof(madlib__maybe__Maybe_Nothing_t));
    nothing->index = madlib__maybe__Maybe_NOTHING_INDEX;

    madlib__http__ClientError_1_t *clientError = (madlib__http__ClientError_1_t*)GC_malloc(sizeof(madlib__http__ClientError_1_t));
    clientError->index = madlib__http__ClientError_BAD_URL_INDEX;
    clientError->arg0 = boxedUrl;

    madlib__http__Error_ClientError_t *error = (madlib__http__Error_ClientError_t*)GC_malloc(sizeof(madlib__http__Error_ClientError_t));
    error->index = madlib__http__Error_CLIENT_ERROR_INDEX;
    error->maybeResponse = nothing;
    error->clientError = clientError;

    __applyPAP__(requestData->badCallback, 1, error);

    // free resources
    curl_slist_free_all(requestData->requestHeaders);
    curl_multi_cleanup(requestData->curlHandle);
    curl_easy_cleanup(handle);
    curl_url_cleanup(urlp);
    GC_free(requestData->pollHandle);
    GC_free(requestData->timerHandle);
    GC_free(requestData);
  }
}

#ifdef __cplusplus
}
#endif
