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

/* Include C++ headers for drop functions BEFORE extern "C" — they contain
 * templates that require C++ linkage. */
#ifdef MADLIB_USE_RC
#include "list.hpp"
#include "array.hpp"
#include "bytearray.hpp"
#include "apply-pap.hpp"
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
    h->magic     = RC_MAGIC;
    h->refcount  = 1;
    h->size_class = (int32_t)total;
    h->_pad      = 0;
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
    h->magic     = RC_MAGIC;
    h->refcount  = RC_STICKY;
    h->size_class = (int32_t)total;
    h->_pad      = 0;
    return (char*)h + RC_HEADER_SIZE;
#endif
}

/* ---------------------------------------------------------------------------
 * Reference counting
 * --------------------------------------------------------------------------- */

void rc_inc(void* ptr) {
    if (!rc_is_managed(ptr)) return;
    madlib__rc__Header_t* h = rc_header(ptr);
    if (h->refcount < RC_STICKY) h->refcount++;
}

void rc_dec_with_drop(void* ptr, rc_drop_fn drop) {
    if (!rc_is_managed(ptr)) return;
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
    if (!rc_is_managed(ptr)) return 0;
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
 * GC stub functions — provide GC_malloc, GC_disable, GC_enable symbols
 * so that LLVM IR referencing them links without -lgc.
 * Under MADLIB_USE_RC these simply delegate to malloc or are no-ops.
 * --------------------------------------------------------------------------- */

#ifdef MADLIB_USE_RC

void* GC_malloc(size_t size) { return malloc(size); }
void* GC_malloc_atomic(size_t size) { return malloc(size); }
void GC_disable(void) {}
void GC_enable(void) {}

#endif

/* ---------------------------------------------------------------------------
 * Perceus drop specialization — built-in type drop functions
 *
 * These are called via rc_dec_with_drop(ptr, drop_fn) when a compound value
 * is freed and its children need to be decremented.  Each function receives
 * a pointer to the payload (past the RC header) and decrements all heap-
 * managed child pointers before the caller frees the struct itself.
 *
 * Only compiled under MADLIB_USE_RC — in GC mode the GC handles all
 * collection without explicit child traversal.
 * --------------------------------------------------------------------------- */

#ifdef MADLIB_USE_RC

/* drop_list: iteratively decrement the spine of a singly-linked list.
 *
 * Elements (node->value) are decremented with no drop function (elements
 * are treated as atomic leaves here; the LLVM codegen generates specialised
 * list drop functions per element type via drop_list_<elemDrop> wrappers).
 *
 * The spine is walked iteratively to avoid stack overflow on long lists.
 */
void rc_drop_list(void *ptr) {
    madlib__list__Node_t *node = (madlib__list__Node_t *)ptr;

    /* Decrement the value of this node (element type unknown — use rc_dec).
     * In RC mode, rc_dec on a primitive-encoded pointer is safe because
     * the pointer will have been sanity-checked by the LLVM codegen before
     * calling rc_dec_with_drop. */
    if (node->value != NULL) {
        rc_dec(node->value);
    }

    /* Walk and free the tail iteratively. */
    madlib__list__Node_t *cur = node->next;
    while (cur != NULL) {
        /* The static empty-list sentinel has NULL value and NULL next — stop. */
        if (cur->value == NULL && cur->next == NULL) break;

        madlib__rc__Header_t *h = rc_header(cur);
        if (h->refcount >= RC_STICKY) break;  /* immortal sentinel */
        if (h->refcount > 1) {
            h->refcount--;  /* shared tail — just decrement, stop walking */
            break;
        }
        /* Unique node: decrement its element, then free. */
        if (cur->value != NULL) {
            rc_dec(cur->value);
        }
        madlib__list__Node_t *nxt = cur->next;
        free(h);  /* free header+payload together */
        cur = nxt;
    }
}

/* rc_drop_array: decrement each element of a heap-allocated Array, then
 * free the items buffer.  Array elements are void* (untyped), so we call
 * rc_dec (no child drop — element types unknown here). */
void rc_drop_array(void *ptr) {
    madlib__array__Array_t *arr = (madlib__array__Array_t *)ptr;
    if (arr->items != NULL) {
        for (int64_t i = 0; i < arr->length; i++) {
            if (arr->items[i] != NULL) {
                rc_dec(arr->items[i]);
            }
        }
        /* Free the items buffer itself (has its own RC header). */
        madlib__rc__Header_t *h = rc_header(arr->items);
        if (h->refcount >= RC_STICKY) return;
        if (--h->refcount == 0) free(h);
    }
}

/* rc_drop_bytearray: free the bytes buffer.  Bytes are primitive (uint8) —
 * no child decrement needed, just free the backing buffer. */
void rc_drop_bytearray(void *ptr) {
    madlib__bytearray__ByteArray_t *ba = (madlib__bytearray__ByteArray_t *)ptr;
    if (ba->bytes != NULL) {
        madlib__rc__Header_t *h = rc_header(ba->bytes);
        if (h->refcount >= RC_STICKY) return;
        if (--h->refcount == 0) free(h);
    }
}

/* rc_drop_pap: decrement each captured value in the PAP environment.
 *
 * PAP env layout: PAPEnv_N is a struct of N void* captured values.
 * We know the count from arity - missingArgCount.
 * env_is_atomic == 1 means captured values are all primitives — skip. */
void rc_drop_pap(void *ptr) {
    PAP_t *pap = (PAP_t *)ptr;
    if (pap->env == NULL) return;
    if (pap->env_is_atomic) return;

    int32_t capturedCount = pap->arity - pap->missingArgCount;
    if (capturedCount <= 0) return;

    /* PAPEnv_N is a struct of void* pointers — treat as void** array. */
    void **env = (void **)pap->env;
    for (int32_t i = 0; i < capturedCount; i++) {
        if (env[i] != NULL) {
            rc_dec(env[i]);
        }
    }
    /* Free the environment struct itself. */
    madlib__rc__Header_t *h = rc_header(pap->env);
    if (h->refcount >= RC_STICKY) return;
    if (--h->refcount == 0) free(h);
}

#endif /* MADLIB_USE_RC */

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
