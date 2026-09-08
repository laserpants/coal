/*
 * network.c - local socket transport for the Coal HTTP library.
 *
 * This file implements the small set of C functions that the Coal module
 * `Network.HTTP` calls through the `#{name : type}` FFI mechanism. Only the
 * socket operations live here; all HTTP parsing and serialization is done by
 * pure Coal code in `Network/HTTP.coal`.
 *
 * The library performs exactly one request per connection: a socket is opened,
 * the serialized request is sent, the response is read (framed by
 * `Content-Length` or by connection close), and the connection is closed.
 */

#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <netdb.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/time.h>

/* --- Coal runtime value types (subset needed here) ------------------------ */

/* `rt_value_t` is an untyped boxed value: pointers for allocated objects and
 * the integer itself reinterpreted as a pointer for int32/int64/bool/char. */
typedef void *rt_value_t;

typedef struct rt_string {
    int64_t length;
    char data[];
} rt_string_t;

typedef struct rt_record {
    const char *field;
    rt_value_t value;
    struct rt_record *next;
} rt_record_t;

/* Opaque byte buffer value produced/consumed by the FFI functions below. */
typedef struct rt_http_bytes {
    int64_t len;
    unsigned char *data;
} rt_http_bytes_t;

extern void *rt_alloc(size_t size);
extern void *rt_alloc_atomic(size_t size);
extern rt_value_t rt_int32_box(int32_t n);
extern int32_t rt_int32_unbox(rt_value_t v);
extern rt_record_t *rt_record_empty(void);
extern rt_record_t *rt_record_extend(rt_record_t *base, const char *field,
                                     rt_value_t value);
extern void rt_panic(const char *msg);

/* --- small helpers -------------------------------------------------------- */

static void *xalloc(size_t size)
{
    void *p = rt_alloc(size);
    if (!p) {
        rt_panic("http: out of memory");
    }
    return p;
}

static void *xalloc_atomic(size_t size)
{
    void *p = rt_alloc_atomic(size);
    if (!p) {
        rt_panic("http: out of memory");
    }
    return p;
}

/* Build a Coal string from raw bytes with an explicit length. */
static rt_string_t *make_string(const unsigned char *data, int64_t len)
{
    if (len < 0) {
        len = 0;
    }
    size_t size = sizeof(rt_string_t) + (size_t) len + 1;
    rt_string_t *s = (rt_string_t *) xalloc_atomic(size);
    s->length = len;
    if (len > 0 && data) {
        memcpy(s->data, data, (size_t) len);
    }
    s->data[len] = '\0';
    return s;
}

/* Build an opaque byte buffer value. */
static rt_http_bytes_t *make_bytes(const unsigned char *data, int64_t len)
{
    rt_http_bytes_t *b = (rt_http_bytes_t *) xalloc(sizeof(rt_http_bytes_t));
    b->len = len < 0 ? 0 : len;
    b->data = NULL;
    if (b->len > 0) {
        b->data = (unsigned char *) xalloc_atomic((size_t) b->len);
        if (data) {
            memcpy(b->data, data, (size_t) b->len);
        }
    }
    return b;
}

/* A record value in the Coal kernel is `{ i32 tag; ptr recordList }` with the
 * single `$Record` constructor (tag = 0) holding the linked list of fields. */
typedef struct {
    int32_t tag;
    void *ptr;
} record_box;

static rt_value_t make_record_value(rt_record_t *list)
{
    record_box *box = (record_box *) xalloc(sizeof(record_box));
    box->tag = 0;
    box->ptr = (void *) list;
    return (rt_value_t) box;
}

/* --- Bytes FFI ------------------------------------------------------------ */

rt_value_t coal_http_bytes_from_string(rt_value_t vs)
{
    rt_string_t *s = (rt_string_t *) vs;
    return (rt_value_t) make_bytes((const unsigned char *) s->data, s->length);
}

rt_value_t coal_http_bytes_to_string(rt_value_t vb)
{
    rt_http_bytes_t *b = (rt_http_bytes_t *) vb;
    return (rt_value_t) make_string(b->data, b->len);
}

rt_value_t coal_http_bytes_length(rt_value_t vb)
{
    rt_http_bytes_t *b = (rt_http_bytes_t *) vb;
    return rt_int32_box((int32_t) b->len);
}

rt_value_t coal_http_bytes_empty(rt_value_t unit)
{
    (void) unit;
    return (rt_value_t) make_bytes(NULL, 0);
}

/* --- HTTP request/response transport -------------------------------------- */

static int memcasecmp(const char *a, const char *b, size_t n)
{
    for (size_t i = 0; i < n; i++) {
        int ca = (unsigned char) a[i];
        int cb = (unsigned char) b[i];
        if (ca >= 'A' && ca <= 'Z') {
            ca += 32;
        }
        if (cb >= 'A' && cb <= 'Z') {
            cb += 32;
        }
        if (ca != cb) {
            return ca - cb;
        }
    }
    return 0;
}

/* Locate the `\r\n\r\n` header terminator in `buf[0..len)`. */
static int64_t find_header_terminator(const unsigned char *buf, int64_t len)
{
    for (int64_t i = 0; i + 3 < len; i++) {
        if (buf[i] == '\r' && buf[i + 1] == '\n' && buf[i + 2] == '\r' &&
            buf[i + 3] == '\n') {
            return i;
        }
    }
    return -1;
}

/* Parse the status code out of a leading `HTTP/x.y NNN ...` line. */
static int parse_status_code(const unsigned char *head, int64_t len)
{
    if (len < 9 || memcmp(head, "HTTP/", 5) != 0) {
        return -1;
    }
    int64_t i = 5;
    while (i < len && head[i] != ' ') {
        i++;
    }
    if (i >= len) {
        return -1;
    }
    i++; /* skip the space */
    if (i + 3 > len) {
        return -1;
    }
    int code = 0;
    for (int k = 0; k < 3; k++) {
        unsigned char c = head[i + k];
        if (c < '0' || c > '9') {
            return -1;
        }
        code = code * 10 + (c - '0');
    }
    return code;
}

/* Parse the `Content-Length` header value from the head bytes.
 * Returns: the value, or -1 if the header is absent, or -2 if invalid. */
static int64_t parse_content_length(const unsigned char *head, int64_t len)
{
    int64_t i = 0;
    while (i < len) {
        int64_t line_start = i;
        while (i < len && head[i] != '\r' && head[i] != '\n') {
            i++;
        }
        int64_t line_end = i;
        while (i < len && (head[i] == '\r' || head[i] == '\n')) {
            i++;
        }
        while (line_end > line_start && head[line_end - 1] == '\r') {
            line_end--;
        }
        int64_t ci = -1;
        for (int64_t k = line_start; k < line_end; k++) {
            if (head[k] == ':') {
                ci = k;
                break;
            }
        }
        if (ci > line_start) {
            size_t name_len = (size_t) (ci - line_start);
            if (name_len == 14 &&
                memcasecmp((const char *) head + line_start, "content-length",
                           14) == 0) {
                int64_t v = ci + 1;
                while (v < line_end && (head[v] == ' ' || head[v] == '\t')) {
                    v++;
                }
                if (v >= line_end) {
                    return -2; /* no value after the colon */
                }
                int64_t value = 0;
                for (; v < line_end; v++) {
                    unsigned char c = head[v];
                    if (c < '0' || c > '9') {
                        return -2; /* non-digit in Content-Length */
                    }
                    if (value > (INT64_MAX - 9) / 10) {
                        return -2; /* overflow */
                    }
                    value = value * 10 + (c - '0');
                }
                return value;
            }
        }
        if (line_end == len) {
            break;
        }
    }
    return -1;
}

/* Write every byte of `data` to the socket. Returns 0 on success. */
static int send_all(int fd, const unsigned char *data, int64_t len)
{
    int64_t off = 0;
    while (off < len) {
        ssize_t n = send(fd, data + off, (size_t) (len - off), 0);
        if (n < 0) {
            if (errno == EINTR) {
                continue;
            }
            return -1;
        }
        off += n;
    }
    return 0;
}

/*
 * Perform one HTTP/1.x exchange:
 *
 *   `coal_http_execute : string -> int32 -> string -> Bytes ->
 *                         { code : int32, status : int32,
 *                           head : string, body : Bytes, error : string }`
 *
 * `code == 0` means the exchange completed: `status` is the HTTP status code,
 * `head` is the status line + header fields, and `body` is the response body.
 * Any transport/protocol failure produces a non-zero `code` with `error`
 * filled in and status/head/body empty.
 */
rt_value_t coal_http_execute(rt_value_t vhost, rt_value_t vport,
                             rt_value_t vhead, rt_value_t vbody)
{
    rt_string_t *host = (rt_string_t *) vhost;
    int32_t port = rt_int32_unbox(vport);
    rt_string_t *head = (rt_string_t *) vhead;
    rt_http_bytes_t *body = (rt_http_bytes_t *) vbody;

    int code = 0;
    int status = 0;
    rt_string_t *resp_head = NULL;
    rt_http_bytes_t *resp_body = NULL;
    char err_buf[512];
    err_buf[0] = '\0';

    char hostname[1024];
    int64_t hlen_copy = host->length < 1023 ? host->length : 1023;
    memcpy(hostname, host->data, (size_t) hlen_copy);
    hostname[hlen_copy] = '\0';

    char port_str[16];
    snprintf(port_str, sizeof port_str, "%d", (int) port);

    struct addrinfo hints;
    memset(&hints, 0, sizeof hints);
    hints.ai_family = AF_INET; /* v1: IPv4 only */
    hints.ai_socktype = SOCK_STREAM;

    struct addrinfo *ai = NULL;
    int gai = getaddrinfo(hostname, port_str, &hints, &ai);
    if (gai != 0) {
        snprintf(err_buf, sizeof err_buf, "cannot resolve host: %s",
                 gai_strerror(gai));
        code = 1;
        goto finish;
    }

    int fd = -1;
    for (struct addrinfo *rp = ai; rp != NULL; rp = rp->ai_next) {
        fd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
        if (fd < 0) {
            continue;
        }
        if (connect(fd, rp->ai_addr, (socklen_t) rp->ai_addrlen) == 0) {
            break;
        }
        close(fd);
        fd = -1;
    }
    freeaddrinfo(ai);
    ai = NULL;

    if (fd < 0) {
        snprintf(err_buf, sizeof err_buf, "connection failed");
        code = 1;
        goto finish;
    }

    struct timeval tv;
    tv.tv_sec = 10;
    tv.tv_usec = 0;
    setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof tv);
    setsockopt(fd, SOL_SOCKET, SO_SNDTIMEO, &tv, sizeof tv);

    if (send_all(fd, (const unsigned char *) head->data, head->length) != 0 ||
        send_all(fd, body->data ? body->data : (const unsigned char *) "",
                 body->len) != 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            code = 2;
            snprintf(err_buf, sizeof err_buf, "sending the request timed out");
        } else {
            code = 3;
            snprintf(err_buf, sizeof err_buf, "sending the request failed: %s",
                     strerror(errno));
        }
        close(fd);
        goto finish;
    }
    shutdown(fd, SHUT_WR);

    int candidate = (head->length >= 5 &&
                     memcasecmp((const char *) head->data, "HEAD ", 5) == 0);

    int64_t start = 0;
    int64_t end = 0;
    int64_t cap = 16384;
    unsigned char *buf = (unsigned char *) xalloc_atomic((size_t) cap);

    int64_t pending_cl = -1;
    int eof_seen = 0;
    int finished = 0;

    while (!finished && code == 0) {
        if (end + 1024 > cap) {
            if (cap > (INT64_MAX / 2)) {
                code = 5;
                snprintf(err_buf, sizeof err_buf,
                         "response too large to buffer");
                break;
            }
            cap *= 2;
            unsigned char *nb = (unsigned char *) xalloc_atomic((size_t) cap);
            memcpy(nb, buf, (size_t) end);
            buf = nb;
        }

        ssize_t n = recv(fd, buf + end, (size_t) (cap - end), 0);
        if (n > 0) {
            end += n;
        } else if (n == 0) {
            eof_seen = 1;
        } else {
            if (errno == EINTR) {
                continue;
            }
            if (errno == EAGAIN || errno == EWOULDBLOCK) {
                code = 2;
                snprintf(err_buf, sizeof err_buf,
                         "reading the response timed out");
            } else {
                code = 4;
                snprintf(err_buf, sizeof err_buf, "read failed: %s",
                         strerror(errno));
            }
            break;
        }

        /* Inspect complete responses in the buffer. */
        int progress = 1;
        while (progress) {
            progress = 0;
            int64_t t = find_header_terminator(buf + start, end - start);
            if (t < 0) {
                break;
            }
            int64_t hstart = start;
            int64_t hend = start + t;

            int sc = parse_status_code(buf + hstart, hend - hstart);
            if (sc >= 100 && sc < 200) {
                /* Informational 1xx: discard and look for the next response. */
                start = hend + 4;
                progress = 1;
                continue;
            }
            if (sc < 100 || sc > 599) {
                code = 4;
                snprintf(err_buf, sizeof err_buf,
                         "malformed status line in response");
                break;
            }

            status = sc;

            if (candidate || sc == 204 || sc == 304) {
                /* No response body for HEAD, 204 and 304. */
                resp_head = make_string(buf + hstart, hend - hstart);
                resp_body = make_bytes(NULL, 0);
                finished = 1;
                break;
            }

            int64_t cl = parse_content_length(buf + hstart, hend - hstart);
            if (cl == -2) {
                code = 4;
                snprintf(err_buf, sizeof err_buf, "invalid Content-Length");
                break;
            }
            if (cl == -1) {
                /* No Content-Length: body delimited by connection close. */
                pending_cl = -1;
                break;
            }
            if (cl > INT32_MAX) {
                code = 5;
                snprintf(err_buf, sizeof err_buf,
                         "response body too large for a byte buffer");
                break;
            }
            pending_cl = cl;
            int64_t need = hend + 4 + cl;
            if (end >= need) {
                resp_head = make_string(buf + hstart, hend - hstart);
                resp_body = make_bytes(buf + hend + 4, cl);
                finished = 1;
                break;
            }
        }

        if (eof_seen) {
            if (pending_cl >= 0) {
                code = 4;
                snprintf(err_buf, sizeof err_buf,
                         "connection closed before the full body arrived "
                         "(truncated response)");
            } else {
                int64_t t = find_header_terminator(buf + start, end - start);
                if (t < 0) {
                    code = 4;
                    snprintf(err_buf, sizeof err_buf,
                             "malformed response: missing header terminator");
                } else {
                    resp_head = make_string(buf + start, t);
                    resp_body = make_bytes(buf + start + t + 4,
                                           end - (start + t + 4));
                    finished = 1;
                }
            }
            break;
        }
    }

    close(fd);

    if (code == 0 && !finished && resp_head == NULL) {
        code = 5;
        snprintf(err_buf, sizeof err_buf, "internal transport error");
    }

finish:
    if (code == 0) {
        return make_record_value(
            rt_record_extend(
                rt_record_extend(
                    rt_record_extend(
                        rt_record_extend(
                            rt_record_extend(rt_record_empty(), "error",
                                             (rt_value_t) make_string(
                                                 (const unsigned char *) "",
                                                 0)),
                            "body", (rt_value_t) resp_body),
                        "head", (rt_value_t) resp_head),
                    "status", rt_int32_box(status)),
                "code", rt_int32_box(0)));
    }

    return make_record_value(
        rt_record_extend(
            rt_record_extend(
                rt_record_extend(
                    rt_record_extend(
                        rt_record_extend(
                            rt_record_empty(), "error",
                            (rt_value_t) make_string(
                                (const unsigned char *) err_buf,
                                (int64_t) strlen(err_buf))),
                        "body", (rt_value_t) make_bytes(NULL, 0)),
                    "head",
                    (rt_value_t) make_string((const unsigned char *) "", 0)),
                "status", rt_int32_box(0)),
            "code", rt_int32_box(code)));
}
