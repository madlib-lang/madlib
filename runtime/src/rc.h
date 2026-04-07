#ifndef MADLIB_RC_H
#define MADLIB_RC_H

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* -----------------------------------------------------------------------
 * Perceus-style reference counting for Madlib.
 *
 * Every RC-managed heap object has an 8-byte header prepended before the
 * user payload:
 *
 *   [ refcount: i32 | size_class: i32 | ---- payload ---- ]
 *   ^                                   ^
 *   header start                        pointer returned to caller
 *
 * refcount   – Reference count. RC_STICKY (INT32_MAX) = immortal (globals).
 * size_class – Total allocation size in bytes (header + payload). Used by
 *              FBIP reuse analysis to check if old memory fits a new value.
 *
 * Madlib's inductive types cannot form cycles, so RC is sound without a
 * cycle collector (same structural guarantee Koka relies on).
 * ----------------------------------------------------------------------- */

#define RC_HEADER_SIZE 8
#define RC_STICKY INT32_MAX

typedef struct {
    int32_t refcount;
    int32_t size_class;
} madlib__rc__Header_t;

/* Get the header of a pointer returned by rc_alloc. */
static inline madlib__rc__Header_t* rc_header(void* ptr) {
    return (madlib__rc__Header_t*)((char*)ptr - RC_HEADER_SIZE);
}

/* Drop function type: called when an object's refcount reaches 0 to
 * recursively decrement child pointers before freeing the object. */
typedef void (*rc_drop_fn)(void*);

/* -----------------------------------------------------------------------
 * Core RC functions — declared here, defined in rc.c as proper extern C
 * symbols so the LLVM codegen can call them as external functions.
 * ----------------------------------------------------------------------- */

#ifdef __cplusplus
extern "C" {
#endif

/* Allocate `size` bytes of payload. Returns pointer to payload (past header).
 * refcount is initialised to 1. */
void* rc_alloc(int64_t size);

/* Allocate with sticky (immortal) refcount – never freed. Used for globals. */
void* rc_alloc_immortal(int64_t size);

/* Increment refcount (safe for NULL and immortal). */
void rc_inc(void* ptr);

/* Decrement refcount.  If it reaches 0, call drop (if not NULL) to
 * recursively release children, then free the object. */
void rc_dec_with_drop(void* ptr, rc_drop_fn drop);

/* Decrement with no child cleanup (leaf / atomic types like strings). */
void rc_dec(void* ptr);

/* Return 1 if ptr is uniquely owned (refcount == 1). */
int rc_is_unique(void* ptr);

/* If ptr is unique AND its allocation is large enough to hold new_size bytes
 * of payload, reuse it (reset refcount to 1, return same pointer).
 * Otherwise allocate fresh.  The caller must overwrite all fields. */
void* rc_reuse(void* ptr, int64_t new_size);

#ifdef MADLIB_RC_DEBUG
void rc_report_leaks(void);
#endif

#ifdef __cplusplus
}
#endif

/* -----------------------------------------------------------------------
 * Conditional compilation: GC  vs  RC
 *
 * When MADLIB_USE_RC is defined the macros use the RC functions above.
 * Otherwise they fall back to Boehm GC so the existing codebase keeps
 * working unchanged during the transition.
 * ----------------------------------------------------------------------- */

#ifdef MADLIB_USE_RC
  #define MADLIB_ALLOC(size)          rc_alloc(size)
  #define MADLIB_ALLOC_ATOMIC(size)   rc_alloc(size)
  #define MADLIB_INC(ptr)             rc_inc(ptr)
  #define MADLIB_DEC(ptr)             rc_dec(ptr)
  #define MADLIB_DEC_DROP(ptr, drop)  rc_dec_with_drop((ptr), (drop))
  #define MADLIB_FREE(ptr)            ((void)0)   /* handled by DEC */
#else
  #include <gc.h>
  #define MADLIB_ALLOC(size)          GC_MALLOC(size)
  #define MADLIB_ALLOC_ATOMIC(size)   GC_MALLOC_ATOMIC(size)
  #define MADLIB_INC(ptr)             ((void)0)
  #define MADLIB_DEC(ptr)             ((void)0)
  #define MADLIB_DEC_DROP(ptr, drop)  ((void)0)
  #define MADLIB_FREE(ptr)            GC_FREE(ptr)
#endif

#endif /* MADLIB_RC_H */
