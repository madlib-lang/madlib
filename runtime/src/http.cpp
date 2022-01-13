#include "http.hpp"

#include <ctype.h>
#include <curl/curl.h>
#include <gc.h>
#include <http_parser.h>
#include <stdlib.h>
#include <uv.h>

#include "event-loop.hpp"
#include "list.hpp"

#ifdef __cplusplus
extern "C" {
#endif

static const int CURL_ID = curl_global_init(CURL_GLOBAL_ALL);
static const int64_t JUST = 0;

typedef struct RequestData {
  // Request
  char *host;
  const char *method;
  char *path;
  char *requestHeaders;
  char *requestBody;
  int port;
  void *callback;

  // Internals
  http_parser *parser;
  http_parser_settings *parserSettings;
  uv_stream_t *tcpStream;

  // Response
  char *body;
  int currentBodySize;
  int64_t status;
  // List Header
  madlib__list__Node_t *headers;
  madlib__http__Header_t *currentHeader;
} RequestData_t;

// utility
void toUpper(char *dest, char *src, size_t length) {
  for (int i = 0; i < length; i++) {
    dest[i] = toupper(src[i]);
  }
}

static void allocCallback(uv_handle_t *handle, size_t size, uv_buf_t *buf) {
  *buf = uv_buf_init((char *)GC_malloc_uncollectable(size), size);
}

void onClose(uv_handle_t *handle) {
  // release memory?
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

int onMessageComplete(http_parser *parser) {
  uv_close((uv_handle_t *)((RequestData_t *)parser->data)->tcpStream, onClose);

  // box body
  char **boxedBody = (char **)GC_malloc_uncollectable(sizeof(char *));
  *boxedBody = ((RequestData_t *)parser->data)->body;

  // box headers
  madlib__list__Node_t **boxedHeaders =
      (madlib__list__Node_t **)GC_malloc(sizeof(madlib__list__Node_t *));
  *boxedHeaders = ((RequestData_t *)parser->data)->headers;

  // box status
  int64_t *boxedStatus = (int64_t *)GC_malloc(sizeof(int64_t));
  *boxedStatus = parser->status_code;

  // free resources

  // call the callback
  __applyPAP__(((RequestData_t *)parser->data)->callback, 1,
               buildResponse(boxedBody, boxedHeaders, boxedStatus));

  return 0;
}

int onBodyReceived(http_parser *parser, const char *bodyPart, size_t length) {
  char *body = ((RequestData_t *)parser->data)->body;
  int currentBodySize = ((RequestData_t *)parser->data)->currentBodySize;
  ((RequestData_t *)parser->data)->currentBodySize = currentBodySize + length;
  char *nextBody = (char *)GC_malloc(currentBodySize + length);

  if (currentBodySize > 0) {
    memcpy(nextBody, body, currentBodySize);
  }

  memcpy(nextBody + currentBodySize, bodyPart, length);

  if (strlen(body) > 0) {
    GC_free(body);
  }

  ((RequestData_t *)parser->data)->body = nextBody;

  return 0;
}

int onHeaderFieldReceived(http_parser *parser, const char *field,
                          size_t length) {
  ((RequestData_t *)parser->data)->currentHeader =
      (madlib__http__Header_t *)GC_malloc(sizeof(madlib__http__Header_t));

  char *headerField = (char *)GC_malloc(length + 1);
  strncpy(headerField, field, length);
  headerField[length] = '\0';

  char **boxed = (char **)GC_malloc(sizeof(char *));
  *boxed = headerField;

  ((RequestData_t *)parser->data)->currentHeader->index = 0;
  ((RequestData_t *)parser->data)->currentHeader->name = boxed;

  return 0;
}

int onHeaderValueReceived(http_parser *parser, const char *value,
                          size_t length) {
  char *headerValue = (char *)GC_malloc(length + 1);
  strncpy(headerValue, value, length);
  headerValue[length] = '\0';

  char **boxed = (char **)GC_malloc(sizeof(char *));
  *boxed = headerValue;

  ((RequestData_t *)parser->data)->currentHeader->value = boxed;

  ((RequestData_t *)parser->data)->headers =
      madlib__list__push(((RequestData_t *)parser->data)->currentHeader,
                         ((RequestData_t *)parser->data)->headers);

  return 0;
}

void onRead(uv_stream_t *tcp, ssize_t sizeRead, const uv_buf_t *buf) {
  if (sizeRead > 0) {
    http_parser_execute(((RequestData_t *)tcp->data)->parser,
                        ((RequestData_t *)tcp->data)->parserSettings, buf->base,
                        sizeRead);
  } else {
    // we got an EOF
    printf("EOF\n");
    uv_close((uv_handle_t *)tcp, onClose);
  }

  GC_free(buf->base);
}

void onWrite(uv_write_t *req, int status) {
  req->handle->data = req->data;
  ((RequestData_t *)req->handle->data)->tcpStream = req->handle;
  uv_read_start(req->handle, allocCallback, onRead);
  GC_free(req);
}

void onConnect(uv_connect_t *connection, int status) {
  if (status < 0) {
    printf("failed to connect\n");
    return;
  }
  char *host = ((RequestData_t *)connection->data)->host;
  char *path = ((RequestData_t *)connection->data)->path;
  const char *method = ((RequestData_t *)connection->data)->method;
  char *headers = ((RequestData_t *)connection->data)->requestHeaders;
  char *body = ((RequestData_t *)connection->data)->requestBody;

  int contentLength = 22 + strlen(host) + strlen(method) + strlen(path) +
                      strlen(headers) + strlen(body);
  char *httpMessage =
      (char *)GC_malloc_uncollectable(sizeof(char) * (contentLength + 1));

  const char *httpMessageTpl =
      "%s %s HTTP/1.1\r\n"
      "Host: %s\r\n"
      "%s"
      "\r\n"
      "%s";

  sprintf(httpMessage, httpMessageTpl, method, path, host, headers, body);

  printf("http message:\n%s\n", httpMessage);
  uv_buf_t buffer[] = {
      {.base = httpMessage, .len = contentLength * sizeof(char)}};

  uv_stream_t *stream = connection->handle;
  stream->data = connection->data;

  uv_write_t *req = (uv_write_t *)GC_malloc_uncollectable(sizeof(uv_write_t));
  req->data = stream->data;
  uv_write(req, stream, buffer, 1, onWrite);
}

void onDNSResolved(uv_getaddrinfo_t *dnsReq, int status, struct addrinfo *res) {
  // establish connection
  uv_tcp_t *stream = (uv_tcp_t *)GC_malloc_uncollectable(sizeof(uv_tcp_t));
  uv_tcp_init(getLoop(), stream);
  uv_tcp_keepalive(stream, 1, 60);

  char *ipAddr = (char *)GC_malloc_uncollectable(sizeof(char) * 16);
  uv_ip4_name((sockaddr_in *)res->ai_addr, ipAddr, sizeof(char) * 16);

  printf("IP: %s\n", ipAddr);

  struct sockaddr_in dest;
  uv_ip4_addr(ipAddr, ((RequestData_t *)dnsReq->data)->port, &dest);

  uv_connect_t *req =
      (uv_connect_t *)GC_malloc_uncollectable(sizeof(uv_connect_t));
  req->data = dnsReq->data;
  uv_tcp_connect(req, stream, (const struct sockaddr *)&dest, onConnect);
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

char *buildHeadersString(madlib__list__Node_t *headers) {
  char *headersString = (char *)"";
  const char *headerTpl = "%s: %s\r\n";

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

    size_t currentLength = strlen(headersString);

    char *next = (char *)GC_malloc_uncollectable(
        sizeof(char) * (currentLength + 5 + strlen(*boxedHeader->name) +
                        strlen(*boxedHeader->value)));
    strncpy(next, headersString, currentLength);
    sprintf(next + currentLength, headerTpl, *boxedHeader->name,
            *boxedHeader->value);

    if (currentLength > 0) {
      GC_free(headersString);
    }

    headersString = next;
    headers = headers->next;
  }

  return headersString;
}

// void madlib__http__request(madlib__record__Record_t *request, PAP_t
// *callback) {
//   char **boxedUrl =
//       (char **)madlib__record__internal__selectField((char *)"url", request);
//   char *url = *boxedUrl;

//   madlib__http__Method_t *boxedMethod =
//       (madlib__http__Method_t *)madlib__record__internal__selectField(
//           (char *)"method", request);
//   const char *methodString = methodToString(boxedMethod);

//   madlib__list__Node_t **boxedHeaders =
//       (madlib__list__Node_t **)madlib__record__internal__selectField(
//           (char *)"headers", request);
//   char *headersString = buildHeadersString(*boxedHeaders);

//   madlib__http__Body_t *boxedBody =
//       (madlib__http__Body_t *)madlib__record__internal__selectField(
//           (char *)"body", request);

//   char *bodyString = (char*) (boxedBody->index == JUST ?
//   *((char**)boxedBody->bodyData) : "");

//   // parse url
//   http_parser_url *parser =
//       (http_parser_url *)GC_malloc_uncollectable(sizeof(http_parser_url));
//   http_parser_url_init(parser);
//   http_parser_parse_url(url, strlen(url), 0, parser);

//   char *host = (char *)GC_malloc_uncollectable(
//       sizeof(char) * (parser->field_data[UF_HOST].len + 1));
//   strncpy(host, url + parser->field_data[UF_HOST].off,
//           parser->field_data[UF_HOST].len);

//   char *portStr = (char *)GC_malloc_uncollectable(
//       sizeof(char) * (parser->field_data[UF_PORT].len + 1));
//   strncpy(portStr, url + parser->field_data[UF_PORT].off,
//           parser->field_data[UF_PORT].len);
//   int port = strcmp(portStr, "") == 0 ? 80 : atoi(portStr);
//   GC_free(portStr);

//   char *path = (char *)GC_malloc_uncollectable(
//       sizeof(char) * (parser->field_data[UF_PATH].len + 1));
//   strncpy(path, url + parser->field_data[UF_PATH].off,
//           parser->field_data[UF_PATH].len);
//   if (parser->field_data[UF_PATH].len == 0) {
//     path = (char *)"/";
//   }

//   printf("url: %s, host: %s, port: %d, path: %s\n", url, host, port, path);

//   // set HttpRequest
//   http_parser *httpParser =
//       (http_parser *)GC_malloc_uncollectable(sizeof(http_parser));
//   http_parser_settings *parserSettings =
//       (http_parser_settings *)GC_malloc_uncollectable(
//           sizeof(http_parser_settings));
//   parserSettings->on_message_complete = onMessageComplete;
//   parserSettings->on_body = onBodyReceived;
//   parserSettings->on_header_field = onHeaderFieldReceived;
//   parserSettings->on_header_value = onHeaderValueReceived;
//   http_parser_init(httpParser, HTTP_RESPONSE);

//   RequestData_t *requestData =
//       (RequestData_t *)GC_malloc_uncollectable(sizeof(RequestData_t));
//   requestData->host = host;
//   requestData->path = path;
//   requestData->method = methodString;
//   requestData->requestHeaders = headersString;
//   requestData->requestBody = bodyString;
//   requestData->port = port;
//   requestData->callback = callback;
//   requestData->parser = httpParser;
//   requestData->parserSettings = parserSettings;
//   requestData->body = (char *)"";
//   requestData->currentBodySize = 0;
//   requestData->currentHeader = NULL;
//   requestData->headers = madlib__list__empty();

//   httpParser->data = requestData;

//   // DNS resolution:
//   struct addrinfo hints;
//   hints.ai_family = PF_INET;
//   hints.ai_socktype = SOCK_STREAM;
//   hints.ai_protocol = IPPROTO_TCP;
//   hints.ai_flags = 0;

//   uv_getaddrinfo_t *resolver =
//       (uv_getaddrinfo_t *)GC_malloc_uncollectable(sizeof(uv_getaddrinfo_t));
//   resolver->data = requestData;

//   int r =
//       uv_getaddrinfo(getLoop(), resolver, onDNSResolved, host, NULL, &hints);

//   // free resoures
//   GC_free(parser);
// }

typedef struct CRequestData {
  // Request
  char *host;
  const char *method;
  char *path;
  char *requestBody;
  int port;
  curl_slist *requestHeaders;
  void *callback;

  // Internals
  http_parser *parser;
  http_parser_settings *parserSettings;
  uv_poll_t *pollHandle;
  uv_timer_t *timerHandle;
  CURLM *curlHandle;
  curl_socket_t curlSocket;

  // Response
  size_t responseSize;
  char *body;
  long status;
  // List Header
  madlib__list__Node_t *headers;
} CRequestData_t;

void onTimeout(uv_timer_t *req) {
  CRequestData_t *requestData = (CRequestData_t *)req->data;
  int running_handles;
  curl_multi_socket_action(requestData->curlHandle, CURL_SOCKET_TIMEOUT, 0,
                           &running_handles);
}

void handleTimer(CURLM *multi, long timeoutMillis, void *userp) {
  CRequestData_t *requestData = (CRequestData_t *)userp;
  if (timeoutMillis <= 0)
    timeoutMillis =
        1; /* 0 means directly call socket_action, but we'll do it in a bit */
  uv_timer_start(requestData->timerHandle, onTimeout, timeoutMillis, 0);
}

void callCallback(CRequestData_t *requestData) {
  // box body
  char **boxedBody = (char **)GC_malloc_uncollectable(sizeof(char *));
  *boxedBody = requestData->body;

  // box headers
  madlib__list__Node_t **boxedHeaders =
      (madlib__list__Node_t **)GC_malloc(sizeof(madlib__list__Node_t *));
  *boxedHeaders = requestData->headers;

  // box status
  int64_t *boxedStatus = (int64_t *)GC_malloc(sizeof(int64_t));
  *boxedStatus = requestData->status;


  // call the callback
  __applyPAP__(requestData->callback, 1,
               buildResponse(boxedBody, boxedHeaders, boxedStatus));
}

void handlePoll(uv_poll_t *req, int status, int events) {
  CRequestData_t *requestData = (CRequestData_t *)req->data;
  uv_timer_stop(requestData->timerHandle);
  int running_handles;

  int flags = 0;
  if (events & UV_READABLE) flags |= CURL_CSELECT_IN;
  if (events & UV_WRITABLE) flags |= CURL_CSELECT_OUT;

  curl_multi_socket_action(requestData->curlHandle, requestData->curlSocket,
                           flags, &running_handles);

  char *done_url;

  CURLMsg *message;
  int pending;
  while ((message = curl_multi_info_read(requestData->curlHandle, &pending))) {
    switch (message->msg) {
      case CURLMSG_DONE:
        curl_easy_getinfo(message->easy_handle, CURLINFO_EFFECTIVE_URL,
                          &done_url);
        curl_multi_remove_handle(requestData->curlHandle, message->easy_handle);
        curl_easy_getinfo(message->easy_handle, CURLINFO_RESPONSE_CODE, &requestData->status);
        curl_easy_cleanup(message->easy_handle);

        curl_slist_free_all(requestData->requestHeaders);

        callCallback(requestData);
        break;
      default:
        fprintf(stderr, "CURLMSG default\n");
        abort();
    }
  }
}

int handleSocket(CURL *easy, curl_socket_t socketfd, int action, void *userp,
                 void *socketp) {
  printf("handle socket\n");
  CRequestData_t *requestData = (CRequestData_t *)userp;
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
      uv_poll_start(requestData->pollHandle, UV_WRITABLE | UV_READABLE, handlePoll);
      break;
    case CURL_POLL_IN:
      uv_poll_start(requestData->pollHandle, UV_READABLE, handlePoll);
      break;
    case CURL_POLL_OUT:
      uv_poll_start(requestData->pollHandle, UV_WRITABLE, handlePoll);
      break;
    case CURL_POLL_REMOVE:
      if (socketfd) {
        // printf("result: %s\n", requestData->body);
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
  // printf("on data write\n");
  size_t realSize = size * nmemb;
  // printf("chunk: %s.*\n", realSize, data);
  CRequestData_t *requestData = (CRequestData_t *)userp;

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
  CRequestData_t *requestData = (CRequestData_t*) userp;
  char *strData = (char*) data;
  size_t realSize = size * nmemb;

  char *startOfValue = (char*)memchr(strData, ':', realSize);

  if (startOfValue == NULL) {
    return realSize;
  }

  madlib__http__Header_t *header = (madlib__http__Header_t *)GC_malloc(sizeof(madlib__http__Header_t));
  header->index = 0;
  header->name = (char**)GC_malloc(sizeof(char*));
  header->value = (char**)GC_malloc(sizeof(char*));

  size_t extraValueOffset = 0;
  size_t nameLength = startOfValue - strData;
  size_t valueLength = realSize - nameLength;

  while (strData[nameLength + extraValueOffset + 1] == ' ') {
    extraValueOffset += 1;
  }

  char *headerName = (char*) GC_malloc(nameLength + 1);
  char *headerValue = (char*) GC_malloc(valueLength + 1);

  strncpy(headerName, strData, nameLength);
  strncpy(headerValue, strData + nameLength + extraValueOffset + 1, valueLength - extraValueOffset - 3);

  *header->name = headerName;
  *header->value = headerValue;

  requestData->headers = madlib__list__push(header, requestData->headers);

  return realSize;
}

curl_slist *buildLibCurlHeaders(madlib__list__Node_t *headers) {
  curl_slist *lcurlHeaders = NULL;
  const char *headerTpl = "%s: %s\r\n";

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
        sizeof(char) * (5 + strlen(*boxedHeader->name) + strlen(*boxedHeader->value)));
    sprintf(headerStr, headerTpl, *boxedHeader->name,
            *boxedHeader->value);

    curl_slist_append(lcurlHeaders, headerStr);
    GC_free(headerStr);
    headers = headers->next;
  }

  return lcurlHeaders;
}

void madlib__http__request(madlib__record__Record_t *request, PAP_t *callback) {
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

  madlib__http__Body_t *boxedBody =
      (madlib__http__Body_t *)madlib__record__internal__selectField(
          (char *)"body", request);

  CURLM *multiHandle = curl_multi_init();

  CRequestData_t *requestData =
      (CRequestData_t *)GC_malloc_uncollectable(sizeof(CRequestData_t));
  requestData->curlHandle = multiHandle;
  requestData->timerHandle =
      (uv_timer_t *)GC_malloc_uncollectable(sizeof(uv_timer_t));
  requestData->pollHandle =
      (uv_poll_t *)GC_malloc_uncollectable(sizeof(uv_poll_t));
  uv_timer_init(getLoop(), requestData->timerHandle);
  requestData->timerHandle->data = requestData;
  requestData->headers = madlib__list__empty();
  requestData->callback = callback;
  requestData->requestHeaders = lcurlHeaders;

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
  curl_easy_setopt(handle, CURLOPT_URL, url);

  curl_multi_add_handle(multiHandle, handle);
}

#ifdef __cplusplus
}
#endif
