// file: /opt/hostedtoolcache/node/14.20.0/x64/lib/node_modules/@madlib-lang/madlib/node_modules/binary-install/bin/prelude/__internal__/Http.mad
import {} from "./../__internals__.mjs"
import { fulfill } from "./Wish.mjs";
import { isJust } from "./Maybe.mjs";
import { Wish } from "./Wish.mjs";
import String from "./String.mjs";
import { Nothing } from "./Maybe.mjs";
import {  } from "./ByteArray.mjs";
import List from "./List.mjs";
import {  } from "./Number.mjs";
import { Just } from "./Maybe.mjs";

export let Chunked = ({ __constructor: "Chunked", __args: [  ] });
export let Compress = ({ __constructor: "Compress", __args: [  ] });
export let Deflate = ({ __constructor: "Deflate", __args: [  ] });
export let Gzip = ({ __constructor: "Gzip", __args: [  ] });
export let Header = (a => b => ({ __constructor: "Header", __args: [ a, b ] }));
export let CONNECT = ({ __constructor: "CONNECT", __args: [  ] });
export let DELETE = ({ __constructor: "DELETE", __args: [  ] });
export let GET = ({ __constructor: "GET", __args: [  ] });
export let HEAD = ({ __constructor: "HEAD", __args: [  ] });
export let OPTIONS = ({ __constructor: "OPTIONS", __args: [  ] });
export let PATCH = ({ __constructor: "PATCH", __args: [  ] });
export let POST = ({ __constructor: "POST", __args: [  ] });
export let PUT = ({ __constructor: "PUT", __args: [  ] });
export let TRACE = ({ __constructor: "TRACE", __args: [  ] });
export let AccessDenied = ({ __constructor: "AccessDenied", __args: [  ] });
export let AddressNotFound = ({ __constructor: "AddressNotFound", __args: [  ] });
export let BadTransferEncoding = ({ __constructor: "BadTransferEncoding", __args: [  ] });
export let BadUrl = (a => ({ __constructor: "BadUrl", __args: [ a ] }));
export let ConnectionFailed = ({ __constructor: "ConnectionFailed", __args: [  ] });
export let Http2FramingError = ({ __constructor: "Http2FramingError", __args: [  ] });
export let IncompleteResponse = ({ __constructor: "IncompleteResponse", __args: [  ] });
export let InternalError = ({ __constructor: "InternalError", __args: [  ] });
export let InvalidSSLCertificate = ({ __constructor: "InvalidSSLCertificate", __args: [  ] });
export let MalformedResponse = ({ __constructor: "MalformedResponse", __args: [  ] });
export let NotSupported = ({ __constructor: "NotSupported", __args: [  ] });
export let SSLConnectionFailed = ({ __constructor: "SSLConnectionFailed", __args: [  ] });
export let SSLEngineNotFound = ({ __constructor: "SSLEngineNotFound", __args: [  ] });
export let SSLInitializationFailed = ({ __constructor: "SSLInitializationFailed", __args: [  ] });
export let Timeout = ({ __constructor: "Timeout", __args: [  ] });
export let TooManyRedirects = ({ __constructor: "TooManyRedirects", __args: [  ] });
export let UnresolvedProxy = ({ __constructor: "UnresolvedProxy", __args: [  ] });
export let UnsupportedProtocol = ({ __constructor: "UnsupportedProtocol", __args: [  ] });
export let BadResponse = (a => ({ __constructor: "BadResponse", __args: [ a ] }));
export let ClientError = (a => ({ __constructor: "ClientError", __args: [ a ] }));
Inspect['TransferEncoding_6a95b62bda9126c5a49dc9882714c897'] = {};
Inspect['TransferEncoding_6a95b62bda9126c5a49dc9882714c897']['inspect'] = () => (__$a__ => ((__x__) => {
  if (__x__.__constructor === "Chunked") {
    return `Chunked`;
  }
  else if (__x__.__constructor === "Compress") {
    return `Compress`;
  }
  else if (__x__.__constructor === "Deflate") {
    return `Deflate`;
  }
  else if (__x__.__constructor === "Gzip") {
    return `Gzip`;
  }
  else if (true) {
    return `Unknown`;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__$a__));
Inspect['Header_6a95b62bda9126c5a49dc9882714c897'] = {};
Inspect['Header_6a95b62bda9126c5a49dc9882714c897']['inspect'] = () => (__$a__ => ((__x__) => {
  if (__x__.__constructor === "Header" && true && true) {
    let a0 = __x__.__args[0];
    let a1 = __x__.__args[1];
    return `Header(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `, ` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a1) + `)`;
  }
  else if (true) {
    return `Unknown`;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__$a__));
Inspect['Method_6a95b62bda9126c5a49dc9882714c897'] = {};
Inspect['Method_6a95b62bda9126c5a49dc9882714c897']['inspect'] = () => (__$a__ => ((__x__) => {
  if (__x__.__constructor === "CONNECT") {
    return `CONNECT`;
  }
  else if (__x__.__constructor === "DELETE") {
    return `DELETE`;
  }
  else if (__x__.__constructor === "GET") {
    return `GET`;
  }
  else if (__x__.__constructor === "HEAD") {
    return `HEAD`;
  }
  else if (__x__.__constructor === "OPTIONS") {
    return `OPTIONS`;
  }
  else if (__x__.__constructor === "PATCH") {
    return `PATCH`;
  }
  else if (__x__.__constructor === "POST") {
    return `POST`;
  }
  else if (__x__.__constructor === "PUT") {
    return `PUT`;
  }
  else if (__x__.__constructor === "TRACE") {
    return `TRACE`;
  }
  else if (true) {
    return `Unknown`;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__$a__));
Inspect['Record_bodyf_0_headersf_1_statusf_2'] = {};
Inspect['Record_bodyf_0_headersf_1_statusf_2']['inspect'] = () => (Inspect_d445) => (Inspect_w438) => (Inspect_p431) => (__$a__ => `{ ` + `body: ` + Inspect_p431.inspect()(__$a__.body) + `, ` + `headers: ` + Inspect_w438.inspect()(__$a__.headers) + `, ` + `status: ` + Inspect_d445.inspect()(__$a__.status) + ` }`);
Inspect['ClientError_6a95b62bda9126c5a49dc9882714c897'] = {};
Inspect['ClientError_6a95b62bda9126c5a49dc9882714c897']['inspect'] = () => (__$a__ => ((__x__) => {
  if (__x__.__constructor === "AccessDenied") {
    return `AccessDenied`;
  }
  else if (__x__.__constructor === "AddressNotFound") {
    return `AddressNotFound`;
  }
  else if (__x__.__constructor === "BadTransferEncoding") {
    return `BadTransferEncoding`;
  }
  else if (__x__.__constructor === "BadUrl" && true) {
    let a0 = __x__.__args[0];
    return `BadUrl(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
  }
  else if (__x__.__constructor === "ConnectionFailed") {
    return `ConnectionFailed`;
  }
  else if (__x__.__constructor === "Http2FramingError") {
    return `Http2FramingError`;
  }
  else if (__x__.__constructor === "IncompleteResponse") {
    return `IncompleteResponse`;
  }
  else if (__x__.__constructor === "InternalError") {
    return `InternalError`;
  }
  else if (__x__.__constructor === "InvalidSSLCertificate") {
    return `InvalidSSLCertificate`;
  }
  else if (__x__.__constructor === "MalformedResponse") {
    return `MalformedResponse`;
  }
  else if (__x__.__constructor === "NotSupported") {
    return `NotSupported`;
  }
  else if (__x__.__constructor === "SSLConnectionFailed") {
    return `SSLConnectionFailed`;
  }
  else if (__x__.__constructor === "SSLEngineNotFound") {
    return `SSLEngineNotFound`;
  }
  else if (__x__.__constructor === "SSLInitializationFailed") {
    return `SSLInitializationFailed`;
  }
  else if (__x__.__constructor === "Timeout") {
    return `Timeout`;
  }
  else if (__x__.__constructor === "TooManyRedirects") {
    return `TooManyRedirects`;
  }
  else if (__x__.__constructor === "UnresolvedProxy") {
    return `UnresolvedProxy`;
  }
  else if (__x__.__constructor === "UnsupportedProtocol") {
    return `UnsupportedProtocol`;
  }
  else if (true) {
    return `Unknown`;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__$a__));
Inspect['Error_6a95b62bda9126c5a49dc9882714c897'] = {};
Inspect['Error_6a95b62bda9126c5a49dc9882714c897']['inspect'] = () => (Inspect_n481) => (__$a__ => ((__x__) => {
  if (__x__.__constructor === "BadResponse" && true) {
    let a0 = __x__.__args[0];
    return `BadResponse(` + Inspect.Record_bodyf_0_headersf_1_statusf_2.inspect()(Inspect.Integer_5b7ebeeaa5acfe1eeea5a9e9845b152d)(__apMtdDicts__(Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d, [Inspect.Header_6a95b62bda9126c5a49dc9882714c897]))(Inspect_n481)(a0) + `)`;
  }
  else if (__x__.__constructor === "ClientError" && true) {
    let a0 = __x__.__args[0];
    return `ClientError(` + Inspect.ClientError_6a95b62bda9126c5a49dc9882714c897.inspect()(a0) + `)`;
  }
  else if (true) {
    return `Unknown`;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__$a__));
Inspect['Record_bodyf_0_headersf_1_methodf_2_urlf_3'] = {};
Inspect['Record_bodyf_0_headersf_1_methodf_2_urlf_3']['inspect'] = () => (Inspect_b521) => (Inspect_u514) => (Inspect_n507) => (Inspect_g500) => (__$a__ => `{ ` + `body: ` + Inspect_g500.inspect()(__$a__.body) + `, ` + `headers: ` + Inspect_n507.inspect()(__$a__.headers) + `, ` + `method: ` + Inspect_u514.inspect()(__$a__.method) + `, ` + `url: ` + Inspect_b521.inspect()(__$a__.url) + ` }`);


;
export let BadRequest = 400;
export let Unauthorized = 401;
export let PaymentRequired = 402;
export let NotFound = 404;
export let OK = 200;
export let getHeader = (headerName => response => (_P_ => List.filter((__x__ => ((__x__) => {
  if (__x__.__constructor === "Header" && true && true) {
    let name = __x__.__args[0];
    return __eq__(String.toLower(name), String.toLower(headerName));
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__)))((__R__ => __R__.headers)(_P_)))(response));
export let methodStr = (__x__ => ((__x__) => {
  if (__x__.__constructor === "GET") {
    return `GET`;
  }
  else if (__x__.__constructor === "POST") {
    return `POST`;
  }
  else if (__x__.__constructor === "PUT") {
    return `PUT`;
  }
  else if (__x__.__constructor === "DELETE") {
    return `DELETE`;
  }
  else if (__x__.__constructor === "PATCH") {
    return `PATCH`;
  }
  else if (__x__.__constructor === "HEAD") {
    return `HEAD`;
  }
  else if (__x__.__constructor === "OPTIONS") {
    return `OPTIONS`;
  }
  else if (__x__.__constructor === "CONNECT") {
    return `CONNECT`;
  }
  else if (__x__.__constructor === "TRACE") {
    return `TRACE`;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__));
let headerKey = (__x__ => ((__x__) => {
  if (__x__.__constructor === "Header" && true && true) {
    let key = __x__.__args[0];
    return key;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__));
let headerValue = (__x__ => ((__x__) => {
  if (__x__.__constructor === "Header" && true && true) {
    let val = __x__.__args[1];
    return val;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__));

const buildHeaderObj = (headerItems) => {
  let headerObj = {}
  while (headerItems !== null) {
    const k = headerKey(headerItems.v)
    const v = headerValue(headerItems.v)

    headerObj[k] = v
    headerItems = headerItems.n
  }
  
  return headerObj
}

const gunzipResponse = (response) => {
  const gunzip = zlib.createGunzip()
  let output = ""

  gunzip.on('data', (data) => {
    output += data.toString()
  })

  response.pipe(gunzip)

  return response
}

;
export let request = (config => Wish((bad => good => {
      let headers = config.headers
    const xhr = new XMLHttpRequest();
    xhr.open(methodStr(config.method), config.url)
    xhr.responseType = "arraybuffer"

    while (headers !== null) {
      xhr.setRequestHeader(headerKey(headers.v), headerValue(headers.v))
      headers = headers.n
    }

    if (isJust(config.body)) {
      xhr.send(config.body.__args[0])
    } else {
      xhr.send()
    }

    xhr.onerror = (err) => {
      // TODO: need to check err and build a correct error here
      return bad(ClientError(InternalError))
    }

    xhr.onload = () => {
      const contentType = xhr.getResponseHeader('Content-Type')
        ? xhr.getResponseHeader('Content-Type').split(";")[0]
        : "text/plain"
      const headerString = xhr.getAllResponseHeaders()
      const headerLines = headerString.split("\r\n")
      const responseHeaders = headerLines
        .map((line) => line
          .split(":")
          .map((s) => s.trim())
        )
        .filter(s => s.length == 2)
        .map(([key, value]) => Header(key)(value))

      let current = {}
      let headersList = current
      if (responseHeaders.length === 0) {
        headerList = null
      } else {
        responseHeaders.forEach((item) => {
          current = current.n = { v: item, n: null }
        })
        headersList = headersList.n
      }

      const buffer = xhr.response
      const ui8 = new Uint8Array(buffer)

      if (xhr.status >= 400) {
        bad(BadResponse({ body: new TextDecoder().decode(ui8), status: xhr.status, headers: headersList }))
      } else {
        good({ body: new TextDecoder().decode(ui8), status: xhr.status, headers: headersList })
      }
    }
    
})));
export let requestBytes = (config => Wish((bad => good => {
      let headers = config.headers
    const xhr = new XMLHttpRequest();
    xhr.open(methodStr(config.method), config.url)
    xhr.responseType = "arraybuffer"

    while (headers !== null) {
      xhr.setRequestHeader(headerKey(headers.v), headerValue(headers.v))
      headers = headers.n
    }

    if (isJust(config.body)) {
      xhr.send(config.body.__args[0])
    } else {
      xhr.send()
    }

    xhr.onerror = (err) => {
      // TODO: need to check err and build a correct error here
      return bad(ClientError(InternalError))
    }

    xhr.onload = () => {
      const contentType = xhr.getResponseHeader('Content-Type')
        ? xhr.getResponseHeader('Content-Type').split(";")[0]
        : "text/plain"
      const headerString = xhr.getAllResponseHeaders()
      const headerLines = headerString.split("\n")
      const responseHeaders = headerLines
        .map((line) => line
          .split(":")
          .map((s) => s.trim())
        )
        .filter(s => s.length == 2)
        .map(([key, value]) => Header(key)(value))

      let current = {}
      let headersList = current
      if (responseHeaders.length === 0) {
        headerList = null
      } else {
        responseHeaders.forEach((item) => {
          current = current.n = { v: item, n: null }
        })
        headersList = headersList.n
      }

      const buffer = xhr.response
      const ui8 = new Uint8Array(buffer)

      if (xhr.status >= 400) {
        bad(BadResponse({ body: ui8, status: xhr.status, headers: headersList }))
      } else {
        good({ body: ui8, status: xhr.status, headers: headersList })
      }
    }
    
})));
export let get = (url => request(({ method: GET, url: url, headers: (null), body: Nothing })));
export let getBytes = (url => requestBytes(({ method: GET, url: url, headers: (null), body: Nothing })));
export let getWithHeaders = (url => headers => request(({ method: GET, url: url, headers: headers, body: Nothing })));
export let getBytesWithHeaders = (url => headers => requestBytes(({ method: GET, url: url, headers: headers, body: Nothing })));
export let post = (url => body => request(({ method: POST, url: url, headers: (null), body: Just(body) })));
export let postBytes = (url => body => requestBytes(({ method: POST, url: url, headers: (null), body: Just(body) })));
export let postWithHeaders = (url => body => headers => request(({ method: POST, url: url, headers: headers, body: Just(body) })));
export let postBytesWithHeaders = (url => body => headers => requestBytes(({ method: POST, url: url, headers: headers, body: Just(body) })));
export let put = (url => body => request(({ method: PUT, url: url, headers: (null), body: Just(body) })));
export let putBytes = (url => body => requestBytes(({ method: PUT, url: url, headers: (null), body: Just(body) })));
export let putWithHeaders = (url => body => headers => request(({ method: PUT, url: url, body: Just(body), headers: headers })));
export let putBytesWithHeaders = (url => body => headers => requestBytes(({ method: PUT, url: url, body: Just(body), headers: headers })));
export let deleteBytes = (url => body => requestBytes(({ method: DELETE, url: url, headers: (null), body: Just(body) })));
export let deleteWithHeaders = (url => body => headers => request(({ method: DELETE, url: url, body: Just(body), headers: headers })));
export let deleteBytesWithHeaders = (url => body => headers => requestBytes(({ method: DELETE, url: url, body: Just(body), headers: headers })));
export default { BadRequest, Unauthorized, PaymentRequired, NotFound, OK, getHeader, methodStr, request, requestBytes, get, getBytes, getWithHeaders, getBytesWithHeaders, post, postBytes, postWithHeaders, postBytesWithHeaders, put, putBytes, putWithHeaders, putBytesWithHeaders, deleteBytes, deleteWithHeaders, deleteBytesWithHeaders, Chunked, Compress, Deflate, Gzip, Header, CONNECT, DELETE, GET, HEAD, OPTIONS, PATCH, POST, PUT, TRACE, AccessDenied, AddressNotFound, BadTransferEncoding, BadUrl, ConnectionFailed, Http2FramingError, IncompleteResponse, InternalError, InvalidSSLCertificate, MalformedResponse, NotSupported, SSLConnectionFailed, SSLEngineNotFound, SSLInitializationFailed, Timeout, TooManyRedirects, UnresolvedProxy, UnsupportedProtocol, BadResponse, ClientError };
