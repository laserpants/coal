/*
 * sqlite.c - SQLite 3 integration for the Coal `Sqlite` module.
 *
 * This file implements every foreign function declared with `#{name : type}`
 * in src/FFI/Sqlite.coal. It is intentionally thin: it calls the SQLite public C
 * API, manages the native handles, and transports outcomes back to Coal
 * through a small GC-allocated result envelope (see below).
 *
 * Database and statement handles are wrapped so that using a closed connection
 * or a finalized statement is reported as a `SQLITE_MISUSE` error instead of
 * dereferencing freed memory (see "Handle wrappers" below).
 *
 * Only the SQLite public API is used; no SQLite internals are accessed.
 *
 * The Coal compiler links this file together with the embedded runtime and
 * the system SQLite library (-lsqlite3). Because the build only adds `-I.` to
 * the include path, the runtime headers are not visible here: the handful of
 * runtime symbols and the `rt_string_t` layout are declared locally, exactly
 * as other Coal C libraries (e.g. coal-http) do.
 */

#include <sqlite3.h>

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

/* --- Coal runtime value representation (subset needed here) ---------------
 *
 * All Coal values cross the FFI boundary as a single `void *`. Small integers
 * (int32/int64/bool/char) are untagged immediates; strings are pointers to an
 * `rt_string_t`. See coal-docs/docs/ffi.md.
 * ------------------------------------------------------------------------ */

typedef void *rt_value_t;

typedef struct rt_string {
    int64_t length;
    char data[];
} rt_string_t;

extern void *rt_alloc(size_t size);
extern void *rt_alloc_atomic(size_t size);
extern void rt_panic(const char *msg);
extern int32_t rt_int32_unbox(rt_value_t v);
extern int64_t rt_int64_unbox(rt_value_t v);
extern double rt_double_unbox(rt_value_t v);

/* --- Result envelope ------------------------------------------------------
 *
 * Every fallible operation returns one of these. It mirrors the runtime's own
 * `rt_result_t` (status + value) and additionally carries a SQLite error code
 * and a copied error message. The envelope is allocated by the runtime's
 * garbage collector, so Coal owns it; the native handle it may carry is still
 * owned by the caller and must be released with the matching close/finalize.
 * ------------------------------------------------------------------------ */

typedef struct {
    int32_t status;       /* 0 = success, 1 = error */
    int32_t code;         /* SQLite result/error code (0 on success) */
    rt_value_t value;     /* Database/Statement handle, or NULL */
    rt_string_t *message; /* copied error text; empty on success */
} sqlite_result_t;

/* Build a Coal string from `len` bytes of `data` (a NULL `data` yields ""). */
static rt_string_t *make_string(const char *data, int64_t len)
{
    if (!data || len < 0) {
        data = "";
        len = 0;
    }
    size_t size = sizeof(rt_string_t) + (size_t) len + 1;
    rt_string_t *s = (rt_string_t *) rt_alloc_atomic(size);
    if (!s) {
        rt_panic("sqlite: out of memory");
    }
    s->length = len;
    if (len > 0) {
        memcpy(s->data, data, (size_t) len);
    }
    s->data[len] = '\0';
    return s;
}

/* Build a Coal string from a NUL-terminated C string. */
static rt_string_t *make_cstring(const char *s)
{
    return make_string(s, s ? (int64_t) strlen(s) : 0);
}

static sqlite_result_t *result_alloc(void)
{
    sqlite_result_t *r = (sqlite_result_t *) rt_alloc(sizeof(sqlite_result_t));
    if (!r) {
        rt_panic("sqlite: out of memory");
    }
    r->status = 0;
    r->code = 0;
    r->value = NULL;
    r->message = make_string("", 0);
    return r;
}

static rt_value_t result_ok(void)
{
    return (rt_value_t) result_alloc();
}

static rt_value_t result_ok_code(int32_t code)
{
    sqlite_result_t *r = result_alloc();
    r->code = code;
    return (rt_value_t) r;
}

static rt_value_t result_ok_value(rt_value_t value)
{
    sqlite_result_t *r = result_alloc();
    r->value = value;
    return (rt_value_t) r;
}

static rt_value_t result_error(int32_t code, const char *message)
{
    sqlite_result_t *r = result_alloc();
    r->status = 1;
    r->code = code;
    r->message = make_cstring(message ? message : "unknown SQLite error");
    return (rt_value_t) r;
}

/* --- Handle wrappers ------------------------------------------------------
 *
 * `Database` and `Statement` are opaque Coal values, i.e. raw pointers across
 * the FFI. Instead of exposing the native `sqlite3 *` / `sqlite3_stmt *`
 * directly, each handle points at one of the small GC-allocated wrappers
 * below. The wrapper keeps the native pointer together with a validity flag,
 * so using a closed connection or a finalized statement is reported as a
 * defined `SQLITE_MISUSE` error rather than dereferencing freed memory.
 *
 * A wrapper holds no GC-managed pointers, and it is reachable only through the
 * Coal handle, so the flag cannot go stale while the handle is live.
 * ------------------------------------------------------------------------ */

typedef struct {
    sqlite3 *db;   /* NULL once the connection has been closed */
    bool     open; /* true while the connection is usable */
} sqlite_db_handle_t;

typedef struct {
    sqlite3_stmt *stmt; /* NULL once the statement has been finalized */
    bool          live; /* true while the statement is usable */
} sqlite_stmt_handle_t;

/* The connection behind a `Database` handle, or NULL if it is not usable. */
static sqlite3 *open_db(sqlite_db_handle_t *handle)
{
    return (handle && handle->open) ? handle->db : NULL;
}

/* The statement behind a `Statement` handle, or NULL if it is not usable. */
static sqlite3_stmt *live_stmt(sqlite_stmt_handle_t *handle)
{
    return (handle && handle->live) ? handle->stmt : NULL;
}

/* --- Database operations -------------------------------------------------- */

/*
 * FFI: #{sqlite_open : string -> SqliteResult}
 *
 * Opens (creating if necessary) the database at `path` and wraps the
 * connection in a `sqlite_db_handle_t`. On failure the partially opened handle
 * is closed before returning, so no native resource is leaked.
 */
rt_value_t sqlite_open(rt_value_t vpath)
{
    rt_string_t *path = (rt_string_t *) vpath;
    sqlite3 *db = NULL;
    int rc = sqlite3_open_v2(path->data, &db,
                             SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE, NULL);
    if (rc != SQLITE_OK) {
        const char *message = db ? sqlite3_errmsg(db) : sqlite3_errstr(rc);
        rt_value_t result = result_error(rc, message);
        if (db) {
            sqlite3_close(db);
        }
        return result;
    }
    sqlite_db_handle_t *handle =
        (sqlite_db_handle_t *) rt_alloc(sizeof(sqlite_db_handle_t));
    if (!handle) {
        rt_panic("sqlite: out of memory");
    }
    handle->db = db;
    handle->open = true;
    return result_ok_value((rt_value_t) handle);
}

/*
 * FFI: #{sqlite_close : Database -> SqliteResult}
 *
 * Uses sqlite3_close, which reports SQLITE_BUSY (rather than invalidating the
 * statements) when outstanding prepared statements remain; on BUSY the handle
 * stays usable. Closing an already-closed handle is reported as SQLITE_MISUSE.
 */
rt_value_t sqlite_close(rt_value_t vdb)
{
    sqlite_db_handle_t *handle = (sqlite_db_handle_t *) vdb;
    if (!handle || !handle->open) {
        return result_error(SQLITE_MISUSE, "database is already closed");
    }
    sqlite3 *db = handle->db;
    int rc = sqlite3_close(db);
    if (rc != SQLITE_OK) {
        return result_error(rc, sqlite3_errmsg(db));
    }
    handle->open = false;
    handle->db = NULL;
    return result_ok();
}

/* --- Prepared statements -------------------------------------------------- */

/*
 * FFI: #{sqlite_prepare : Database -> string -> SqliteResult}
 *
 * Prepares the first statement in `sql` and wraps it in a
 * `sqlite_stmt_handle_t`. An empty (or comment-only) string prepares no
 * statement and is reported as an error. Preparing on a closed database is
 * reported as SQLITE_MISUSE.
 */
rt_value_t sqlite_prepare(rt_value_t vdb, rt_value_t vsql)
{
    sqlite3 *db = open_db((sqlite_db_handle_t *) vdb);
    if (!db) {
        return result_error(SQLITE_MISUSE, "database is closed");
    }
    rt_string_t *sql = (rt_string_t *) vsql;
    sqlite3_stmt *stmt = NULL;
    int rc = sqlite3_prepare_v2(db, sql->data, -1, &stmt, NULL);
    if (rc != SQLITE_OK) {
        return result_error(rc, sqlite3_errmsg(db));
    }
    if (stmt == NULL) {
        return result_error(SQLITE_ERROR, "no SQL statement to prepare");
    }
    sqlite_stmt_handle_t *handle =
        (sqlite_stmt_handle_t *) rt_alloc(sizeof(sqlite_stmt_handle_t));
    if (!handle) {
        rt_panic("sqlite: out of memory");
    }
    handle->stmt = stmt;
    handle->live = true;
    return result_ok_value((rt_value_t) handle);
}

/*
 * FFI: #{sqlite_finalize : Statement -> SqliteResult}
 *
 * The error message is read from the owning connection before the statement is
 * destroyed. sqlite3_finalize always destroys the statement, even when it
 * returns an error code, so the handle is marked dead in every case.
 * Finalizing an already-finalized statement is reported as SQLITE_MISUSE.
 */
rt_value_t sqlite_finalize(rt_value_t vstmt)
{
    sqlite_stmt_handle_t *handle = (sqlite_stmt_handle_t *) vstmt;
    sqlite3_stmt *stmt = live_stmt(handle);
    if (!stmt) {
        return result_error(SQLITE_MISUSE, "statement is already finalized");
    }
    sqlite3 *db = sqlite3_db_handle(stmt);
    int rc = sqlite3_finalize(stmt);
    handle->live = false;
    handle->stmt = NULL;
    if (rc != SQLITE_OK) {
        return result_error(rc, db ? sqlite3_errmsg(db) : "finalize failed");
    }
    return result_ok();
}

/* --- Parameter binding (1-based indices) ---------------------------------- */

static rt_value_t bind_result(sqlite3_stmt *stmt, int rc)
{
    if (rc != SQLITE_OK) {
        return result_error(rc, sqlite3_errmsg(sqlite3_db_handle(stmt)));
    }
    return result_ok();
}

/* FFI: #{sqlite_bind_int64 : Statement -> int32 -> int64 -> SqliteResult} */
rt_value_t sqlite_bind_int64(rt_value_t vstmt, rt_value_t vidx, rt_value_t vval)
{
    sqlite3_stmt *stmt = live_stmt((sqlite_stmt_handle_t *) vstmt);
    if (!stmt) {
        return result_error(SQLITE_MISUSE, "statement is already finalized");
    }
    int idx = (int) rt_int32_unbox(vidx);
    int64_t val = rt_int64_unbox(vval);
    return bind_result(stmt, sqlite3_bind_int64(stmt, idx, (sqlite3_int64) val));
}

/* FFI: #{sqlite_bind_double : Statement -> int32 -> double -> SqliteResult} */
rt_value_t sqlite_bind_double(rt_value_t vstmt, rt_value_t vidx, rt_value_t vval)
{
    sqlite3_stmt *stmt = live_stmt((sqlite_stmt_handle_t *) vstmt);
    if (!stmt) {
        return result_error(SQLITE_MISUSE, "statement is already finalized");
    }
    int idx = (int) rt_int32_unbox(vidx);
    double val = rt_double_unbox(vval);
    return bind_result(stmt, sqlite3_bind_double(stmt, idx, val));
}

/*
 * FFI: #{sqlite_bind_text : Statement -> int32 -> string -> SqliteResult}
 *
 * SQLITE_TRANSIENT makes SQLite copy the text, so it never retains a pointer
 * into Coal-managed storage.
 */
rt_value_t sqlite_bind_text(rt_value_t vstmt, rt_value_t vidx, rt_value_t vval)
{
    sqlite3_stmt *stmt = live_stmt((sqlite_stmt_handle_t *) vstmt);
    if (!stmt) {
        return result_error(SQLITE_MISUSE, "statement is already finalized");
    }
    int idx = (int) rt_int32_unbox(vidx);
    rt_string_t *text = (rt_string_t *) vval;
    int rc = sqlite3_bind_text(stmt, idx, text->data, (int) text->length,
                               SQLITE_TRANSIENT);
    return bind_result(stmt, rc);
}

/* FFI: #{sqlite_bind_null : Statement -> int32 -> SqliteResult} */
rt_value_t sqlite_bind_null(rt_value_t vstmt, rt_value_t vidx)
{
    sqlite3_stmt *stmt = live_stmt((sqlite_stmt_handle_t *) vstmt);
    if (!stmt) {
        return result_error(SQLITE_MISUSE, "statement is already finalized");
    }
    int idx = (int) rt_int32_unbox(vidx);
    return bind_result(stmt, sqlite3_bind_null(stmt, idx));
}

/* --- Statement execution -------------------------------------------------- */

/*
 * FFI: #{sqlite_step : Statement -> SqliteResult}
 *
 * Invariant: success (`status == 0`) is reported *only* for SQLITE_ROW and
 * SQLITE_DONE, and the envelope's `code` then holds that raw step result.
 * Every other code is reported as an error envelope carrying the connection's
 * current message. Coal's `decode_step` relies on this when mapping a
 * successful code to `Row`/`Done`.
 */
rt_value_t sqlite_step(rt_value_t vstmt)
{
    sqlite3_stmt *stmt = live_stmt((sqlite_stmt_handle_t *) vstmt);
    if (!stmt) {
        return result_error(SQLITE_MISUSE, "statement is already finalized");
    }
    int rc = sqlite3_step(stmt);
    if (rc == SQLITE_ROW || rc == SQLITE_DONE) {
        return result_ok_code((int32_t) rc);
    }
    return result_error(rc, sqlite3_errmsg(sqlite3_db_handle(stmt)));
}

/* --- Column inspection (0-based indices) ---------------------------------- */

/* True when `idx` is a valid column index for the statement's current row. */
static bool column_index_ok(sqlite3_stmt *stmt, int idx)
{
    return idx >= 0 && idx < sqlite3_column_count(stmt);
}

/*
 * Column accessors have no error channel, so a finalized statement yields the
 * same neutral values as an out-of-range index (0, 0.0, "", false, Null)
 * rather than reading freed memory.
 */

/* FFI: #{sqlite_column_count : Statement -> int32} */
int32_t sqlite_column_count(rt_value_t vstmt)
{
    sqlite3_stmt *stmt = live_stmt((sqlite_stmt_handle_t *) vstmt);
    if (!stmt) {
        return 0;
    }
    return (int32_t) sqlite3_column_count(stmt);
}

/* FFI: #{sqlite_column_name : Statement -> int32 -> string} */
rt_string_t *sqlite_column_name(rt_value_t vstmt, rt_value_t vidx)
{
    sqlite3_stmt *stmt = live_stmt((sqlite_stmt_handle_t *) vstmt);
    int idx = (int) rt_int32_unbox(vidx);
    if (!stmt || !column_index_ok(stmt, idx)) {
        return make_cstring("");
    }
    const char *name = sqlite3_column_name(stmt, idx);
    return make_cstring(name ? name : "");
}

/*
 * FFI: #{sqlite_column_type : Statement -> int32 -> int32}
 *
 * Invariant: returns exactly one of SQLite's five column-type constants
 * (SQLITE_INTEGER, SQLITE_FLOAT, SQLITE_TEXT, SQLITE_BLOB, SQLITE_NULL), or
 * SQLITE_NULL for an out-of-range index (the documented neutral value for an
 * invalid column access). Coal's `decode_column_type` relies on this.
 */
int32_t sqlite_column_type(rt_value_t vstmt, rt_value_t vidx)
{
    sqlite3_stmt *stmt = live_stmt((sqlite_stmt_handle_t *) vstmt);
    int idx = (int) rt_int32_unbox(vidx);
    if (!stmt || !column_index_ok(stmt, idx)) {
        return SQLITE_NULL;
    }
    return (int32_t) sqlite3_column_type(stmt, idx);
}

/* FFI: #{sqlite_column_int64 : Statement -> int32 -> int64} */
int64_t sqlite_column_int64(rt_value_t vstmt, rt_value_t vidx)
{
    sqlite3_stmt *stmt = live_stmt((sqlite_stmt_handle_t *) vstmt);
    int idx = (int) rt_int32_unbox(vidx);
    if (!stmt || !column_index_ok(stmt, idx)) {
        return 0;
    }
    return (int64_t) sqlite3_column_int64(stmt, idx);
}

/* FFI: #{sqlite_column_double : Statement -> int32 -> double} */
double sqlite_column_double(rt_value_t vstmt, rt_value_t vidx)
{
    sqlite3_stmt *stmt = live_stmt((sqlite_stmt_handle_t *) vstmt);
    int idx = (int) rt_int32_unbox(vidx);
    if (!stmt || !column_index_ok(stmt, idx)) {
        return 0.0;
    }
    return sqlite3_column_double(stmt, idx);
}

/*
 * FFI: #{sqlite_column_text : Statement -> int32 -> string}
 *
 * The bytes are copied into Coal-owned storage (with an explicit length, so
 * embedded NULs survive); no SQLite-owned pointer escapes the call.
 */
rt_string_t *sqlite_column_text(rt_value_t vstmt, rt_value_t vidx)
{
    sqlite3_stmt *stmt = live_stmt((sqlite_stmt_handle_t *) vstmt);
    int idx = (int) rt_int32_unbox(vidx);
    if (!stmt || !column_index_ok(stmt, idx)) {
        return make_string("", 0);
    }
    const unsigned char *text = sqlite3_column_text(stmt, idx);
    int bytes = sqlite3_column_bytes(stmt, idx);
    return make_string((const char *) text, (int64_t) bytes);
}

/* FFI: #{sqlite_column_is_null : Statement -> int32 -> bool} */
bool sqlite_column_is_null(rt_value_t vstmt, rt_value_t vidx)
{
    sqlite3_stmt *stmt = live_stmt((sqlite_stmt_handle_t *) vstmt);
    int idx = (int) rt_int32_unbox(vidx);
    if (!stmt || !column_index_ok(stmt, idx)) {
        return true;
    }
    return sqlite3_column_type(stmt, idx) == SQLITE_NULL;
}

/* --- Result-envelope accessors -------------------------------------------- */

/* FFI: #{sqlite_result_status : SqliteResult -> int32} */
int32_t sqlite_result_status(rt_value_t vres)
{
    sqlite_result_t *r = (sqlite_result_t *) vres;
    return r->status;
}

/* FFI: #{sqlite_result_code : SqliteResult -> int32} */
int32_t sqlite_result_code(rt_value_t vres)
{
    sqlite_result_t *r = (sqlite_result_t *) vres;
    return r->code;
}

/* FFI: #{sqlite_result_message : SqliteResult -> string} */
rt_string_t *sqlite_result_message(rt_value_t vres)
{
    sqlite_result_t *r = (sqlite_result_t *) vres;
    return r->message;
}

/*
 * FFI: #{sqlite_result_value : SqliteResult -> <opaque>}
 *
 * The same C symbol is annotated at each call site with the concrete opaque
 * type it yields (Database or Statement); both are represented as a pointer.
 */
void *sqlite_result_value(rt_value_t vres)
{
    sqlite_result_t *r = (sqlite_result_t *) vres;
    return r->value;
}

/* --- Test helper ---------------------------------------------------------- */

/*
 * FFI: #{sqlite_remove_file : string -> int32}
 *
 * Test-only helper. The Coal standard library has no file-removal primitive,
 * so the example/test program uses this to delete its temporary database file.
 * It is not part of the public `FFI.Sqlite` API and wraps the C library `remove`.
 */
int32_t sqlite_remove_file(rt_value_t vpath)
{
    rt_string_t *path = (rt_string_t *) vpath;
    return (int32_t) remove(path->data);
}

/* --- Test helper: callback-invocation counter ----------------------------- */

/*
 * FFI: #{sqlite_test_counter_reset : unit -> int32}
 * FFI: #{sqlite_test_counter_bump  : unit -> int32}
 * FFI: #{sqlite_test_counter_get   : unit -> int32}
 *
 * Test-only helpers, in the same spirit as `sqlite_remove_file`. They let the
 * test suite observe how many times a callback was invoked, which cannot be
 * observed through SQLite alone. They are not part of the public API.
 */
static int32_t g_sqlite_test_counter = 0;

int32_t sqlite_test_counter_reset(void)
{
    g_sqlite_test_counter = 0;
    return 0;
}

int32_t sqlite_test_counter_bump(void)
{
    g_sqlite_test_counter += 1;
    return g_sqlite_test_counter;
}

int32_t sqlite_test_counter_get(void)
{
    return g_sqlite_test_counter;
}

