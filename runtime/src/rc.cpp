/* Madlib reference counting runtime.
 *
 * This file provides the actual definitions of the RC functions declared in
 * rc.h.  They are non-inline extern C functions so that the LLVM codegen can
 * reference them as external symbols that are resolved at link time via
 * libruntime.a.
 *
 * When MADLIB_USE_RC is NOT defined (default: GC mode), these functions still
 * get compiled into libruntime.a and are available as dead code that the
 * linker can drop.  The LLVM modules always emit calls to rc_alloc / rc_inc /
 * rc_dec etc., so they must always be present in libruntime.a regardless of
 * the memory management mode.
 */

#include "rc.h"

/* Force inclusion of gc.h when in GC mode so that any indirect include of
 * rc.h from C++ files that already have gc.h in scope remains valid. */
#ifndef MADLIB_USE_RC
#  include <gc.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* ---------------------------------------------------------------------------
 * Allocation
 * --------------------------------------------------------------------------- */

void* rc_alloc(int64_t size) {
    int64_t total = size + RC_HEADER_SIZE;
#ifdef MADLIB_USE_RC
    /* Pure RC mode: use plain malloc; Boehm GC is not active. */
    madlib__rc__Header_t* h = (madlib__rc__Header_t*)malloc((size_t)total);
#else
    /* GC-compatibility mode: allocate via GC_MALLOC so that Boehm GC can
     * scan this memory for live pointers.  This prevents GC from collecting
     * objects that are only referenced by rc_alloc'd payloads, which would
     * happen if plain malloc were used (GC doesn't scan malloc memory). */
    madlib__rc__Header_t* h = (madlib__rc__Header_t*)GC_MALLOC((size_t)total);
#endif
    if (!h) { fprintf(stderr, "rc_alloc: out of memory\n"); abort(); }
    h->refcount  = 1;
    h->size_class = (int32_t)total;
    return (char*)h + RC_HEADER_SIZE;
}

void* rc_alloc_immortal(int64_t size) {
#ifdef MADLIB_USE_RC
    void* ptr = rc_alloc(size);
    rc_header(ptr)->refcount = RC_STICKY;
    return ptr;
#else
    /* In GC mode, use GC_MALLOC_UNCOLLECTABLE for immortal objects.
     * This tells Boehm GC never to collect this allocation. */
    int64_t total = size + RC_HEADER_SIZE;
    madlib__rc__Header_t* h = (madlib__rc__Header_t*)GC_MALLOC_UNCOLLECTABLE((size_t)total);
    if (!h) { fprintf(stderr, "rc_alloc_immortal: out of memory\n"); abort(); }
    h->refcount  = RC_STICKY;
    h->size_class = (int32_t)total;
    return (char*)h + RC_HEADER_SIZE;
#endif
}

/* ---------------------------------------------------------------------------
 * Reference counting
 * --------------------------------------------------------------------------- */

void rc_inc(void* ptr) {
    if (!ptr) return;
    madlib__rc__Header_t* h = rc_header(ptr);
    if (h->refcount < RC_STICKY) h->refcount++;
}

void rc_dec_with_drop(void* ptr, rc_drop_fn drop) {
    if (!ptr) return;
    madlib__rc__Header_t* h = rc_header(ptr);
    if (h->refcount >= RC_STICKY) return;
    if (--h->refcount == 0) {
        if (drop) drop(ptr);
#ifdef MADLIB_USE_RC
        free(h);
#else
        GC_FREE(h);
#endif
    }
}

void rc_dec(void* ptr) {
    rc_dec_with_drop(ptr, NULL);
}

/* ---------------------------------------------------------------------------
 * FBIP reuse
 * --------------------------------------------------------------------------- */

int rc_is_unique(void* ptr) {
    if (!ptr) return 0;
    return rc_header(ptr)->refcount == 1;
}

void* rc_reuse(void* ptr, int64_t new_size) {
    if (ptr && rc_is_unique(ptr)) {
        madlib__rc__Header_t* h = rc_header(ptr);
        if (h->size_class >= (int32_t)(new_size + RC_HEADER_SIZE)) {
            h->refcount = 1;
            return ptr;
        }
    }
    return rc_alloc(new_size);
}

/* ---------------------------------------------------------------------------
 * Debug leak tracking
 * --------------------------------------------------------------------------- */

#ifdef MADLIB_RC_DEBUG
#include <stdatomic.h>

static _Atomic int64_t rc_total_allocs = 0;
static _Atomic int64_t rc_total_frees  = 0;

void rc_report_leaks(void) {
    int64_t a = (int64_t)atomic_load(&rc_total_allocs);
    int64_t f = (int64_t)atomic_load(&rc_total_frees);
    fprintf(stderr, "[RC] allocs: %lld  frees: %lld  leaked: %lld\n",
            (long long)a, (long long)f, (long long)(a - f));
}

__attribute__((destructor))
static void rc_atexit(void) { rc_report_leaks(); }
#endif /* MADLIB_RC_DEBUG */

#ifdef __cplusplus
}
#endif
