/*
 * test_rc.cpp — Stress and correctness tests for the Madlib RC runtime.
 *
 * Build and run (from runtime/):
 *   clang++ -std=c++14 -I./src -I./include -o test_rc test_rc.cpp \
 *           build/libruntime.a \
 *           $(pkg-config --libs bdw-gc) \
 *           -L./include/bdwgc -lgc \
 *           && ./test_rc
 *
 * Or with the same flags the Makefile uses:
 *   clang++ -std=c++14 -Iinclude -o test_rc test_rc.cpp \
 *           build/libruntime.a -lgc && ./test_rc
 */

#include <cassert>
#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <gc.h>        // GC_INIT — must init GC before any rc_alloc in GC mode
#include "src/rc.h"

// ──────────────────────────────────────────────────────────────────────────────
// Helpers
// ──────────────────────────────────────────────────────────────────────────────

static int tests_run    = 0;
static int tests_passed = 0;
static int tests_failed = 0;

#define ASSERT_TRUE(cond) do { \
    tests_run++; \
    if (cond) { \
        tests_passed++; \
    } else { \
        tests_failed++; \
        fprintf(stderr, "FAIL [%s:%d]: %s\n", __FILE__, __LINE__, #cond); \
    } \
} while(0)

#define ASSERT_EQ(a, b) do { \
    tests_run++; \
    if ((a) == (b)) { \
        tests_passed++; \
    } else { \
        tests_failed++; \
        fprintf(stderr, "FAIL [%s:%d]: %s == %s  (%lld != %lld)\n", \
                __FILE__, __LINE__, #a, #b, (long long)(a), (long long)(b)); \
    } \
} while(0)

// ──────────────────────────────────────────────────────────────────────────────
// Test 1: Basic allocation — header fields are initialised correctly
// ──────────────────────────────────────────────────────────────────────────────
static void test_alloc_basic() {
    printf("[test_alloc_basic]\n");

    void* p = rc_alloc(64);
    ASSERT_TRUE(p != NULL);

    madlib__rc__Header_t* h = rc_header(p);
    ASSERT_EQ(h->refcount, 1);
    // size_class must be at least payload + header
    ASSERT_TRUE(h->size_class >= (int32_t)(64 + RC_HEADER_SIZE));
}

// ──────────────────────────────────────────────────────────────────────────────
// Test 2: Immortal allocation — refcount is RC_STICKY
// ──────────────────────────────────────────────────────────────────────────────
static void test_alloc_immortal() {
    printf("[test_alloc_immortal]\n");

    void* p = rc_alloc_immortal(32);
    ASSERT_TRUE(p != NULL);

    madlib__rc__Header_t* h = rc_header(p);
    ASSERT_EQ(h->refcount, RC_STICKY);

    // rc_inc on an immortal must not overflow
    rc_inc(p);
    ASSERT_EQ(h->refcount, RC_STICKY);

    // rc_dec on an immortal must not free it
    rc_dec(p);
    ASSERT_EQ(h->refcount, RC_STICKY);
}

// ──────────────────────────────────────────────────────────────────────────────
// Test 3: rc_inc / rc_dec — refcount tracks correctly
// ──────────────────────────────────────────────────────────────────────────────
static void test_inc_dec() {
    printf("[test_inc_dec]\n");

    void* p = rc_alloc(16);
    madlib__rc__Header_t* h = rc_header(p);

    ASSERT_EQ(h->refcount, 1);

    rc_inc(p);
    ASSERT_EQ(h->refcount, 2);

    rc_inc(p);
    ASSERT_EQ(h->refcount, 3);

    rc_dec(p);
    ASSERT_EQ(h->refcount, 2);

    rc_dec(p);
    ASSERT_EQ(h->refcount, 1);

    // After one more dec the object is freed; don't read h after this.
    // In GC mode GC_FREE is called, in RC mode free() is called.
    rc_dec(p);
    // We can't safely assert h->refcount == 0 here because the memory was freed.
    // Just verify we didn't crash.
    ASSERT_TRUE(true);
}

// ──────────────────────────────────────────────────────────────────────────────
// Test 4: NULL safety — rc_inc / rc_dec on NULL must not crash
// ──────────────────────────────────────────────────────────────────────────────
static void test_null_safety() {
    printf("[test_null_safety]\n");

    rc_inc(NULL);  // must not crash
    rc_dec(NULL);  // must not crash
    rc_dec_with_drop(NULL, NULL);  // must not crash
    int u = rc_is_unique(NULL);
    ASSERT_EQ(u, 0);

    ASSERT_TRUE(true);
}

// ──────────────────────────────────────────────────────────────────────────────
// Test 5: rc_is_unique — returns 1 only when refcount == 1
// ──────────────────────────────────────────────────────────────────────────────
static void test_is_unique() {
    printf("[test_is_unique]\n");

    void* p = rc_alloc(8);
    ASSERT_EQ(rc_is_unique(p), 1);

    rc_inc(p);
    ASSERT_EQ(rc_is_unique(p), 0);

    rc_dec(p);
    ASSERT_EQ(rc_is_unique(p), 1);

    // Immortal objects are NOT unique (refcount == INT32_MAX)
    void* q = rc_alloc_immortal(8);
    ASSERT_EQ(rc_is_unique(q), 0);

    // Clean up (rc_dec on immortal is a no-op, GC will reclaim)
    rc_dec(p);
}

// ──────────────────────────────────────────────────────────────────────────────
// Test 6: rc_reuse — unique, fits → same pointer returned; shared → fresh alloc
// ──────────────────────────────────────────────────────────────────────────────
static void test_reuse() {
    printf("[test_reuse]\n");

    void* p = rc_alloc(128);   // allocate a large block
    ASSERT_TRUE(rc_is_unique(p));

    // Reuse for something smaller — should return same pointer
    void* r = rc_reuse(p, 64);
    ASSERT_TRUE(r == p);       // in-place reuse
    ASSERT_EQ(rc_header(r)->refcount, 1);

    // Now share p (simulate two references)
    void* p2 = rc_alloc(128);
    rc_inc(p2);                // refcount = 2
    ASSERT_TRUE(!rc_is_unique(p2));

    void* r2 = rc_reuse(p2, 64);
    ASSERT_TRUE(r2 != p2);    // shared → fresh allocation
    ASSERT_EQ(rc_header(r2)->refcount, 1);

    // Clean up
    rc_dec(p2);   // refcount → 1
    rc_dec(p2);   // freed
    rc_dec(r2);   // freed
}

// ──────────────────────────────────────────────────────────────────────────────
// Test 7: rc_reuse — block too small → fresh alloc even if unique
// ──────────────────────────────────────────────────────────────────────────────
static void test_reuse_too_small() {
    printf("[test_reuse_too_small]\n");

    void* p = rc_alloc(8);     // small block
    ASSERT_TRUE(rc_is_unique(p));

    // Request more memory than the block has
    void* r = rc_reuse(p, 256);
    // Because p is too small, rc_reuse allocates fresh
    ASSERT_TRUE(r != p);
    ASSERT_EQ(rc_header(r)->refcount, 1);

    rc_dec(r);
    // p was not consumed by rc_reuse (it just wasn't reused).
    // In a real program the caller would own p and dec it separately.
    rc_dec(p);
}

// ──────────────────────────────────────────────────────────────────────────────
// Test 8: Drop function called exactly once when refcount hits 0
// ──────────────────────────────────────────────────────────────────────────────
static int drop_count = 0;
static void my_drop(void* ptr) {
    (void)ptr;
    drop_count++;
}

static void test_drop_called() {
    printf("[test_drop_called]\n");

    drop_count = 0;

    void* p = rc_alloc(32);
    rc_inc(p);                   // refcount = 2

    rc_dec_with_drop(p, my_drop);  // refcount → 1, drop NOT called
    ASSERT_EQ(drop_count, 0);

    rc_dec_with_drop(p, my_drop);  // refcount → 0, drop IS called
    ASSERT_EQ(drop_count, 1);
}

// ──────────────────────────────────────────────────────────────────────────────
// Test 9: Stress — allocate and free 100k objects, verify no crash
// ──────────────────────────────────────────────────────────────────────────────
static void test_stress_alloc_free() {
    printf("[test_stress_alloc_free]\n");

#define N 100000
    void* ptrs[N];
    for (int i = 0; i < N; i++) {
        ptrs[i] = rc_alloc(32 + (i % 128));
        ASSERT_TRUE(ptrs[i] != NULL);
        ASSERT_EQ(rc_header(ptrs[i])->refcount, 1);
    }
    for (int i = 0; i < N; i++) {
        rc_dec(ptrs[i]);
    }
    ASSERT_TRUE(true);
#undef N
}

// ──────────────────────────────────────────────────────────────────────────────
// Test 10: Stress — chain of rc_inc/rc_dec to simulate shared ownership
// ──────────────────────────────────────────────────────────────────────────────
static void test_stress_shared() {
    printf("[test_stress_shared]\n");

#define DEPTH 1000
    void* p = rc_alloc(64);

    // Inc DEPTH times (simulate DEPTH references)
    for (int i = 0; i < DEPTH; i++) {
        rc_inc(p);
    }
    ASSERT_EQ(rc_header(p)->refcount, DEPTH + 1);

    // Dec DEPTH times — object must still be alive
    for (int i = 0; i < DEPTH; i++) {
        rc_dec(p);
    }
    ASSERT_EQ(rc_header(p)->refcount, 1);
    ASSERT_EQ(rc_is_unique(p), 1);

    // Final dec — object freed, no crash
    rc_dec(p);
    ASSERT_TRUE(true);
#undef DEPTH
}

// ──────────────────────────────────────────────────────────────────────────────
// Test 11: Sticky saturation — rc_inc near INT32_MAX must not overflow
// ──────────────────────────────────────────────────────────────────────────────
static void test_sticky_saturation() {
    printf("[test_sticky_saturation]\n");

    void* p = rc_alloc(16);
    // Manually set refcount to RC_STICKY - 1
    rc_header(p)->refcount = RC_STICKY - 1;
    // One more inc should clamp at RC_STICKY
    rc_inc(p);
    ASSERT_EQ(rc_header(p)->refcount, RC_STICKY);
    // Another inc must not go past RC_STICKY
    rc_inc(p);
    ASSERT_EQ(rc_header(p)->refcount, RC_STICKY);
    // rc_dec on a sticky object must not free it
    rc_dec(p);
    ASSERT_EQ(rc_header(p)->refcount, RC_STICKY);
    // GC will reclaim it eventually
}

// ──────────────────────────────────────────────────────────────────────────────
// Test 12: rc_reuse returns NULL-safe result when ptr is NULL
// ──────────────────────────────────────────────────────────────────────────────
static void test_reuse_null_ptr() {
    printf("[test_reuse_null_ptr]\n");

    // rc_reuse(NULL, size) must always return a fresh allocation
    void* r = rc_reuse(NULL, 64);
    ASSERT_TRUE(r != NULL);
    ASSERT_EQ(rc_header(r)->refcount, 1);
    rc_dec(r);
}

// ──────────────────────────────────────────────────────────────────────────────
// Test 13: Writing into payload and reading it back after inc/dec
// ──────────────────────────────────────────────────────────────────────────────
static void test_payload_integrity() {
    printf("[test_payload_integrity]\n");

    const char* src = "Hello, Madlib RC!";
    size_t len = strlen(src) + 1;
    void* p = rc_alloc((int64_t)len);
    memcpy(p, src, len);

    rc_inc(p);
    // Compare payload after inc
    ASSERT_TRUE(memcmp(p, src, len) == 0);

    rc_dec(p);
    // Compare payload after dec (refcount now 1, still alive)
    ASSERT_TRUE(memcmp(p, src, len) == 0);

    rc_dec(p);  // freed
    ASSERT_TRUE(true);
}

// ──────────────────────────────────────────────────────────────────────────────
// Test 14: rc_reuse preserves memory content for smaller request
// ──────────────────────────────────────────────────────────────────────────────
static void test_reuse_content() {
    printf("[test_reuse_content]\n");

    void* p = rc_alloc(128);
    memset(p, 0xAB, 128);

    void* r = rc_reuse(p, 64);  // r == p (same block, unique and large enough)
    ASSERT_TRUE(r == p);
    // The first 64 bytes should still be 0xAB (we didn't wipe them)
    unsigned char* bytes = (unsigned char*)r;
    int all_ok = 1;
    for (int i = 0; i < 64; i++) {
        if (bytes[i] != 0xAB) { all_ok = 0; break; }
    }
    ASSERT_TRUE(all_ok);
    rc_dec(r);
}

// ──────────────────────────────────────────────────────────────────────────────
// Test 15: Alignment — payload pointer is at least 8-byte aligned
// ──────────────────────────────────────────────────────────────────────────────
static void test_alignment() {
    printf("[test_alignment]\n");

    for (int sz = 1; sz <= 512; sz *= 2) {
        void* p = rc_alloc(sz);
        uintptr_t addr = (uintptr_t)p;
        ASSERT_TRUE((addr % 8) == 0);
        rc_dec(p);
    }
}

// ──────────────────────────────────────────────────────────────────────────────
// main
// ──────────────────────────────────────────────────────────────────────────────
int main(void) {
    GC_INIT();   // Required: initialise Boehm GC before any rc_alloc (GC mode)

    printf("=== RC Runtime Tests ===\n\n");

    test_alloc_basic();
    test_alloc_immortal();
    test_inc_dec();
    test_null_safety();
    test_is_unique();
    test_reuse();
    test_reuse_too_small();
    test_drop_called();
    test_stress_alloc_free();
    test_stress_shared();
    test_sticky_saturation();
    test_reuse_null_ptr();
    test_payload_integrity();
    test_reuse_content();
    test_alignment();

    printf("\n=== Results: %d/%d passed", tests_passed, tests_run);
    if (tests_failed > 0) {
        printf(" (%d FAILED)", tests_failed);
    }
    printf(" ===\n");

    return tests_failed > 0 ? 1 : 0;
}
