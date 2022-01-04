#include <gc.h>
#include <http_parser.h>
#include <stdlib.h>
#include <uv.h>

#include "apply-pap.hpp"
#include "event-loop.hpp"
#include "list.hpp"

#ifdef __cplusplus
extern "C" {
#endif

typedef enum HeaderName {
  contentLength = 1,
  contentType = 2,
  transferEncoding = 3,
} HeaderName_t;

typedef struct Header {
  HeaderName_t name;
  void *value;
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
  MadListNode_t *headers;
  Header_t *currentHeader;
} RequestData_t;

static void allocCallback(uv_handle_t *handle, size_t size, uv_buf_t *buf) {
  *buf = uv_buf_init((char *)GC_malloc_uncollectable(size), size);
}

void onClose(uv_handle_t *handle) {
  // release memory?
}

int onMessageComplete(http_parser *parser) {
  uv_close((uv_handle_t *)((RequestData_t *)parser->data)->tcpStream, onClose);

  char **boxedResult = (char **)GC_malloc_uncollectable(sizeof(char *));
  *boxedResult = ((RequestData_t *)parser->data)->body;
  __applyPAP__(((RequestData_t *)parser->data)->callback, 1, boxedResult);

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

int onHeaderFieldReceived(http_parser *parser, const char *bodyPart, size_t length) {

}

int onHeaderValueReceived(http_parser *parser, const char *bodyPart, size_t length) {

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

void get(char *url, PAP_t *callback) {
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

  RequestData_t *httpReq =
      (RequestData_t *)GC_malloc_uncollectable(sizeof(RequestData_t));
  httpReq->path = path;
  httpReq->port = port;
  httpReq->callback = callback;
  httpReq->parser = httpParser;
  httpReq->parserSettings = parserSettings;
  httpReq->body = NULL;
  httpReq->currentBodySize = 0;

  httpParser->data = httpReq;

  // DNS resolution:
  struct addrinfo hints;
  hints.ai_family = PF_INET;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_protocol = IPPROTO_TCP;
  hints.ai_flags = 0;

  uv_getaddrinfo_t *resolver =
      (uv_getaddrinfo_t *)GC_malloc_uncollectable(sizeof(uv_getaddrinfo_t));
  resolver->data = httpReq;

  int r =
      uv_getaddrinfo(getLoop(), resolver, onDNSResolved, host, NULL, &hints);

  // free resoures
  GC_free(parser);
}

#ifdef __cplusplus
}
#endif
