#include <gc.h>
#include <http_parser.h>
#include <stdlib.h>
#include <ctype.h>
#include <uv.h>

#include "apply-pap.hpp"
#include "event-loop.hpp"
#include "list.hpp"

#ifdef __cplusplus
extern "C" {
#endif

// Integer
typedef enum HeaderName {
  CONTENT_LENGTH = 1,
  CONTENT_TYPE = 2,
  TRANSFER_ENCODING = 3,
} HeaderName_t;

// #[Integer, String]
typedef struct Header {
  int64_t index;
  char **name;
  char **value;
} Header_t;

typedef struct RequestData {
  char *path;
  int port;
  void *callback;
  http_parser *parser;
  http_parser_settings *parserSettings;
  uv_stream_t *tcpStream;
  char *body;
  int currentBodySize;

  int64_t status;

  // List #[String, String]
  madlib__list__Node_t *headers;
  Header_t *currentHeader;
} RequestData_t;


// utility
void toUpper(char *str, size_t length) {
  for(int i = 0; i<length; i++) {
    str[i] = toupper(str[i]);
  }
}

static void allocCallback(uv_handle_t *handle, size_t size, uv_buf_t *buf) {
  *buf = uv_buf_init((char *)GC_malloc_uncollectable(size), size);
}

void onClose(uv_handle_t *handle) {
  // release memory?
}

int onMessageComplete(http_parser *parser) {
  uv_close((uv_handle_t *)((RequestData_t *)parser->data)->tcpStream, onClose);

  // box body
  char **boxedBody = (char **)GC_malloc_uncollectable(sizeof(char *));
  *boxedBody = ((RequestData_t *)parser->data)->body;

  // box headers
  madlib__list__Node_t **boxedHeaders = (madlib__list__Node_t **)GC_malloc(sizeof(madlib__list__Node_t *));
  *boxedHeaders = ((RequestData_t *)parser->data)->headers;

  // box status
  int64_t *boxedStatus = (int64_t *)GC_malloc(sizeof(int64_t));
  *boxedStatus = parser->status_code;

  // free resources

  // call the callback
  __applyPAP__(((RequestData_t *)parser->data)->callback, 3, boxedBody, boxedHeaders, boxedStatus);

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

  GC_free(body);
  ((RequestData_t *)parser->data)->body = nextBody;

  return 0;
}

int onHeaderFieldReceived(http_parser *parser, const char *field, size_t length) {
  printf("header field: %.*s\n", length, field);

  ((RequestData_t*)parser->data)->currentHeader = (Header_t *)GC_malloc(sizeof(Header_t));

  char *headerField = (char *)GC_malloc(length + 1);
  strncpy(headerField, field, length);
  headerField[length] = '\0';

  char **boxed = (char **)GC_malloc(sizeof(char*));
  *boxed = headerField;

  ((RequestData_t*)parser->data)->currentHeader->index = 0;
  ((RequestData_t*)parser->data)->currentHeader->name = boxed;

  return 0;
}

int onHeaderValueReceived(http_parser *parser, const char *value, size_t length) {
  printf("header value: %.*s\n", length, value);
  char *headerValue = (char *)GC_malloc(length + 1);
  strncpy(headerValue, value, length);
  headerValue[length] = '\0';

  char **boxed = (char **)GC_malloc(sizeof(char*));
  *boxed = headerValue;

  ((RequestData_t*)parser->data)->currentHeader->value = boxed;

  ((RequestData_t*)parser->data)->headers = madlib__list__push(((RequestData_t*)parser->data)->currentHeader, ((RequestData_t*)parser->data)->headers);

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
  char *path = ((RequestData_t *)connection->data)->path;

  int contentLength = 17 + strlen(path);
  char *httpMessage =
      (char *)GC_malloc_uncollectable(sizeof(char) * (contentLength + 1));

  const char *httpMessageTpl =
      "GET %s HTTP/1.1\r\n"
      "\r\n";

  sprintf(httpMessage, httpMessageTpl, path);

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

// get :: String -> (String -> List Header -> Integer -> ()) -> ()
void madlib__http__request(char *url, PAP_t *callback) {
  // parse url
  http_parser_url *parser =
      (http_parser_url *)GC_malloc_uncollectable(sizeof(http_parser_url));
  http_parser_url_init(parser);
  http_parser_parse_url(url, strlen(url), 0, parser);

  char *host = (char *)GC_malloc_uncollectable(
      sizeof(char) * (parser->field_data[UF_HOST].len + 1));
  strncpy(host, url + parser->field_data[UF_HOST].off,
          parser->field_data[UF_HOST].len);

  char *portStr = (char *)GC_malloc_uncollectable(
      sizeof(char) * (parser->field_data[UF_PORT].len + 1));
  strncpy(portStr, url + parser->field_data[UF_PORT].off,
          parser->field_data[UF_PORT].len);
  int port = strcmp(portStr, "") == 0 ? 80 : atoi(portStr);
  GC_free(portStr);

  char *path = (char *)GC_malloc_uncollectable(
      sizeof(char) * (parser->field_data[UF_PATH].len + 1));
  strncpy(path, url + parser->field_data[UF_PATH].off,
          parser->field_data[UF_PATH].len);
  if (parser->field_data[UF_PATH].len == 0) {
    path = (char *)"/";
  }

  printf("url: %s, host: %s, port: %d, path: %s\n", url, host, port, path);

  // set HttpRequest
  http_parser *httpParser =
      (http_parser *)GC_malloc_uncollectable(sizeof(http_parser));
  http_parser_settings *parserSettings =
      (http_parser_settings *)GC_malloc_uncollectable(
          sizeof(http_parser_settings));
  parserSettings->on_message_complete = onMessageComplete;
  parserSettings->on_body = onBodyReceived;
  parserSettings->on_header_field = onHeaderFieldReceived;
  parserSettings->on_header_value = onHeaderValueReceived;
  http_parser_init(httpParser, HTTP_RESPONSE);

  RequestData_t *requestData =
      (RequestData_t *)GC_malloc_uncollectable(sizeof(RequestData_t));
  requestData->path = path;
  requestData->port = port;
  requestData->callback = callback;
  requestData->parser = httpParser;
  requestData->parserSettings = parserSettings;
  requestData->body = NULL;
  requestData->currentBodySize = 0;
  requestData->currentHeader = NULL;
  requestData->headers = madlib__list__empty();

  httpParser->data = requestData;

  // DNS resolution:
  struct addrinfo hints;
  hints.ai_family = PF_INET;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_protocol = IPPROTO_TCP;
  hints.ai_flags = 0;

  uv_getaddrinfo_t *resolver =
      (uv_getaddrinfo_t *)GC_malloc_uncollectable(sizeof(uv_getaddrinfo_t));
  resolver->data = requestData;

  int r =
      uv_getaddrinfo(getLoop(), resolver, onDNSResolved, host, NULL, &hints);

  // free resoures
  GC_free(parser);
}

#ifdef __cplusplus
}
#endif
