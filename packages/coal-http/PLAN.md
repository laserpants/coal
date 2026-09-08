# Coal HTTP Library — First Version

## Goal

Implement a small, synchronous/buffered HTTP client library for Coal.

The first version should provide:

* HTTP request/response data types
* HTTP methods
* HTTP headers
* URL parsing/representation
* HTTP client creation
* Sending requests through `IO`
* Basic GET/POST convenience functions
* Transport and HTTP-level error handling
* Comprehensive unit/integration tests
* Documentation and examples

Keep the implementation deliberately small. Do **not** implement an HTTP server, streaming bodies, cookies, redirects, multipart forms, authentication helpers, HTTP/2, or JSON-specific functionality in this first version.

The design principle is:

> HTTP requests and responses are ordinary immutable data. Network communication happens only through `IO`.

---

## 1. Inspect the Existing Codebase First

Before implementing anything:

1. Find the existing standard-library/package structure.
2. Inspect how modules, packages, tests, documentation, and public APIs are organized.
3. Identify existing types that HTTP should reuse:

   * `String`
   * `Bytes` / byte arrays, if available
   * `Option`
   * `Result`
   * `IO`
   * collection types
4. Inspect existing networking/socket/IO functionality.
5. Inspect existing error-handling conventions.
6. Inspect existing naming conventions for:

   * constructors
   * record fields
   * modules
   * functions
   * enum/variant names
7. Follow existing conventions instead of introducing new ones.

Do not duplicate existing functionality.

---

# 2. Package/Module Structure

Create a package/module corresponding to the existing project conventions, conceptually:

```text
Network.HTTP
```

A possible internal structure is:

```text
Network.HTTP
Network.HTTP.URL
Network.HTTP.Headers
Network.HTTP.Client
```

However, keep the public API simple. If the project convention favors a single module initially, use a single module and split the implementation internally only where useful.

Do not create unnecessary abstraction layers.

---

# 3. Core Public Types

## HTTP Method

Provide:

```text
type Method
  = GET
  | POST
  | PUT
  | PATCH
  | DELETE
  | HEAD
  | OPTIONS
```

Use the project's existing conventions for constructors/variants.

If straightforward, also provide conversion to/from the wire representation:

```text
method_to_string : Method -> String
```

Parsing an arbitrary method is optional for v1 unless required by the implementation.

---

## HTTP Version

Support HTTP/1.0 and HTTP/1.1:

```text
type Version
  = HTTP10
  | HTTP11
```

The implementation should use HTTP/1.1 by default.

---

## URL

Expose an abstract URL type:

```text
type URL
```

Provide:

```text
parse_url : String -> Result<URL>
url_to_string : URL -> String
```

At minimum, support URLs of the form:

```text
http://host/path
https://host/path
```

If TLS/HTTPS is not yet supported by the existing runtime, do not fake HTTPS support. Either:

* implement HTTPS only if the existing networking stack supports it, or
* reject HTTPS explicitly with an appropriate error.

Support ports and query strings where practical.

Do not expose the internal URL representation as part of the initial public API.

---

# 4. Headers

Define an abstract header collection:

```text
type Headers
```

Provide at least:

```text
empty_headers : Headers

get_header
  : String -> Headers -> Option<String>

set_header
  : String -> String -> Headers -> Headers

remove_header
  : String -> Headers -> Headers

has_header
  : String -> Headers -> Bool
```

Header names must be treated case-insensitively.

For v1, choose a simple representation compatible with the existing collection facilities.

If duplicate headers are important for correctness, preserve them internally rather than silently collapsing all duplicates. Do not over-engineer the API unless the implementation requires it.

---

# 5. Request

Define:

```text
type Request =
  Request({
    method  : Method
    url     : URL
    headers : Headers
    body    : Bytes
  })
```

Use the repository's actual byte type. If there is no `Bytes` type yet, inspect existing array/vector facilities before introducing a new one.

Provide convenient construction helpers if consistent with the rest of the library:

```text
request
get_request
post_request
```

Do not make request construction perform network IO.

---

# 6. Response

Define:

```text
type Response =
  Response({
    version : Version
    status  : int32
    headers : Headers
    body    : Bytes
  })
```

The first implementation should buffer the complete response body.

Provide basic helpers:

```text
status : Response -> int32
headers : Response -> Headers
body : Response -> Bytes

is_success : Response -> Bool
is_redirect : Response -> Bool
is_client_error : Response -> Bool
is_server_error : Response -> Bool
```

Do not treat HTTP 4xx/5xx responses as transport errors.

For example:

```text
GET -> 404
```

should normally produce:

```text
Ok(Response(... status = 404 ...))
```

rather than:

```text
Err(...)
```

Only failures communicating with the server, parsing the response, etc. should become library errors.

---

# 7. Errors

Define an HTTP-specific error type following existing Coal error conventions.

Conceptually:

```text
type Error
  = InvalidURL(...)
  | ConnectionError(...)
  | Timeout
  | InvalidRequest(...)
  | InvalidResponse(...)
  | IOError(...)
```

Only include variants that are actually meaningful given the existing runtime.

Avoid duplicating an existing generic IO/network error hierarchy if one already exists.

The distinction should be:

```text
Result<Response>
```

represents whether the HTTP operation itself succeeded at the transport/protocol level.

An HTTP status such as 404 is still a successful HTTP exchange.

---

# 8. HTTP Client

Define an abstract client:

```text
type Client
```

Provide:

```text
create : IO<Client>
```

or an equivalent constructor matching existing project conventions.

The client should encapsulate whatever networking state is necessary.

Do not expose sockets or transport implementation details publicly.

---

# 9. Core Operation

The primary operation should be:

```text
send
  : Client -> Request -> IO<Result<Response>>
```

This is the central API.

It must:

1. Validate/prepare the request.
2. Establish or use a connection.
3. Serialize the HTTP request.
4. Send it over the network.
5. Read the HTTP response.
6. Parse the status line.
7. Parse response headers.
8. Read the response body.
9. Construct a `Response`.
10. Return it through `IO`.

Do not perform network IO during request construction.

---

# 10. Convenience Functions

Provide simple wrappers around `send`:

```text
get
  : Client -> URL -> IO<Result<Response>>

post
  : Client -> URL -> Bytes -> IO<Result<Response>>
```

If useful and consistent with the API:

```text
put
delete
```

can be added, but they are not required for the first implementation.

The convenience functions should not duplicate HTTP implementation logic. They should construct a `Request` and call `send`.

---

# 11. HTTP/1.1 Serialization

Implement enough HTTP/1.1 to support ordinary requests.

For example:

```text
GET /path HTTP/1.1
Host: example.com
Connection: close

```

For requests with bodies, correctly handle:

* `Content-Length`
* `Content-Type` when explicitly supplied

The library should automatically generate `Host` where required.

Avoid implementing chunked request bodies unless the existing runtime makes this necessary.

---

# 12. HTTP Response Parsing

Implement a robust but intentionally limited HTTP/1.x response parser.

Parse:

```text
HTTP/1.1 200 OK
Content-Length: 123
Content-Type: text/plain
```

Extract:

* HTTP version
* status code
* headers
* body

At minimum, support responses using `Content-Length`.

Also correctly handle responses where the body is delimited by connection close when practical.

Respect HTTP semantics for methods/statuses that do not contain a response body, especially:

* `HEAD`
* `1xx`
* `204`
* `304`

Do not silently misinterpret protocol framing.

---

# 13. Connection Handling

For v1, prioritize correctness and simplicity over connection pooling.

It is acceptable to use:

```text
one request -> one connection -> response -> close
```

If persistent connections are trivial to support with the existing runtime, they may be used, but connection pooling/reuse is explicitly out of scope.

Prefer `Connection: close` if that substantially simplifies correct framing.

Document this behavior.

---

# 14. HTTPS / TLS

Inspect the existing runtime before deciding how to implement HTTPS.

If TLS support already exists:

* integrate with it.

If it does not:

* do not implement an insecure workaround;
* make HTTPS requests fail clearly with an appropriate error;
* document that HTTPS is not yet supported.

Do not silently downgrade `https://` to plain HTTP.

---

# 15. Testing

Create tests covering at least:

## URL

* valid HTTP URL
* valid URL with path
* valid URL with query
* explicit port
* invalid URL
* unsupported scheme

## Headers

* set/get
* remove
* existence
* case-insensitive lookup
* empty headers

## Request construction

* GET
* POST
* headers
* body

## Response parsing

Test representative raw HTTP responses:

```text
HTTP/1.1 200 OK
Content-Length: 5

hello
```

Also test:

* multiple headers
* empty body
* 404
* 500
* HEAD
* 204
* malformed status line
* malformed headers
* invalid Content-Length
* truncated response

## Integration

Use a local test HTTP server if the repository's testing infrastructure allows it.

At minimum test:

```text
GET / -> 200
POST / -> request body received
GET /not-found -> 404
```

Do not depend on external internet services for tests.

Tests must be deterministic and runnable offline.

---

# 16. Example Usage

Add a small documented example along these lines:

```text
let client = Network.HTTP.create

let url =
  Network.HTTP.parse_url("http://localhost:8080/hello")

let response =
  Network.HTTP.get(client, url)

match response {
  | Ok(response) =>
      ...
  | Err(error) =>
      ...
}
```

Adapt syntax to the actual Coal language and existing library conventions.

The example should demonstrate the important design property:

```text
construct data -> perform IO explicitly
```

---

# 17. Documentation

Document:

1. The purpose of the package.
2. Supported HTTP features.
3. The distinction between HTTP errors and transport errors.
4. The buffered response-body model.
5. HTTPS support/status.
6. Connection behavior.
7. Basic usage.
8. Explicitly unsupported functionality.

Keep the documentation honest about limitations.

---

# 18. Scope Restrictions

Do NOT implement in this first version:

* HTTP server
* HTTP/2
* HTTP/3
* WebSockets
* connection pooling
* automatic retries
* automatic redirects
* cookies
* authentication frameworks
* multipart/form-data
* streaming request bodies
* streaming response bodies
* compression
* JSON integration
* form encoding
* proxy support
* advanced TLS configuration

These can be future layers/features.

---

# 19. Implementation Order

Implement in this order:

### Phase 1 — Repository integration

* Inspect existing APIs and conventions.
* Identify reusable byte/string/result/io/network types.
* Establish module/package structure.

### Phase 2 — Pure HTTP types

Implement:

* `Method`
* `Version`
* `URL`
* `Headers`
* `Request`
* `Response`
* status helpers

These components should be testable without network IO.

### Phase 3 — HTTP serialization/parsing

Implement pure functions for:

* request serialization
* status-line parsing
* header parsing
* response framing/parsing

Keep parsing/serialization separate from sockets.

### Phase 4 — Transport

Implement:

* `Client`
* connection handling
* request transmission
* response reading

Keep all network effects behind `IO`.

### Phase 5 — Public API

Implement:

```text
create
send
get
post
```

and any small helpers justified by the existing API conventions.

### Phase 6 — Integration tests

Use a local HTTP server/test fixture.

### Phase 7 — Documentation and cleanup

* examples
* API documentation
* error behavior
* limitations
* formatting
* remove unnecessary abstractions

---

# 20. Architectural Constraint

Maintain this separation throughout the implementation:

```text
                Pure
                 │
       ┌─────────┼─────────┐
       │         │         │
      URL     Headers   Request
       │         │         │
       └─────────┼─────────┘
                 │
          Serialization
                 │
                 ▼
              Client
                 │
                 │ IO
                 ▼
              Network
                 │
                 ▼
             Response
                 │
          Parsing / Pure
                 │
                 ▼
              Response
```

In particular:

* URL parsing should be pure.
* Header manipulation should be pure.
* Request construction should be pure.
* Request serialization should be pure.
* Response parsing should be pure.
* Only socket/network operations should require `IO`.

This separation should make the implementation easy to test and fit naturally with Coal's functional/effectful architecture.

---

# Definition of Done

The first version is complete when:

* [ ] The HTTP package follows existing repository conventions.
* [ ] `Method`, `Version`, `URL`, `Headers`, `Request`, and `Response` exist.
* [ ] Requests can be constructed without performing IO.
* [ ] Requests can be serialized correctly.
* [ ] HTTP/1.x responses can be parsed correctly.
* [ ] A client can perform a real local HTTP request.
* [ ] GET works.
* [ ] POST with a byte body works.
* [ ] Response headers and body are accessible.
* [ ] HTTP 4xx/5xx responses are returned as responses, not transport errors.
* [ ] Malformed protocol data produces appropriate errors.
* [ ] Tests do not depend on external internet access.
* [ ] Documentation contains a working basic example.
* [ ] HTTPS behavior is explicit and correct.
* [ ] No unnecessary future-facing abstractions have been introduced.
* [ ] The implementation preserves the pure-data / `IO` boundary described above.

