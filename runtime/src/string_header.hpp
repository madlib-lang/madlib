#pragma once
#include <stdint.h>
#include <string.h>
#include <gc.h>

// Flags stored in MadlibStringHeader::flags
#define MADSTR_HAS_HEADER   0x01u  // always set for madlib-owned strings
#define MADSTR_SSO          0x02u  // string fits inline in the 24-byte SSO object
#define MADSTR_PRECOMPUTED  0x04u  // char_len pre-computed at compile time (literals); never write back

// Sentinel: char_len field holds this value when not yet computed
#define MADSTR_CHAR_LEN_UNKNOWN 0x00FFFFFFu

//
// MadlibStringHeader  (8 bytes, sits immediately BEFORE the char* data pointer)
//
// Layout on a 64-bit host:
//   [0..3]  byte_len   u32  — byte length of the UTF-8 data (NOT including NUL terminator)
//   [4]     char_len_lo u8  — Unicode char count, bits 0–7
//   [5]     char_len_mid u8 — Unicode char count, bits 8–15
//   [6]     char_len_hi u8  — Unicode char count, bits 16–23  (24-bit field, max ~16M chars)
//   [7]     flags       u8  — see MADSTR_* constants above
//
// Access pattern from a char* p:
//   MadlibStringHeader *h = ((MadlibStringHeader*)p) - 1;
//   if (h->flags & MADSTR_HAS_HEADER) { use h->byte_len, ... }
//
// Invariant:
//   Every string produced by a madlib runtime function carries a valid header.
//   Strings arriving from external C code (PCRE2, libuv, curl, FFI) do NOT;
//   madstr_byte_len() falls back to strlen() for them.
//
typedef struct {
    uint32_t byte_len;      // UTF-8 byte count (NOT including NUL terminator)
    uint8_t  char_len_lo;   // Unicode char count, bits  0–7
    uint8_t  char_len_mid;  // Unicode char count, bits  8–15
    uint8_t  char_len_hi;   // Unicode char count, bits 16–23
    uint8_t  flags;
} MadlibStringHeader;

static_assert(sizeof(MadlibStringHeader) == 8, "MadlibStringHeader must be exactly 8 bytes");
static_assert(alignof(MadlibStringHeader) == 4, "MadlibStringHeader must be 4-byte aligned");

// ---- Inline helpers ---------------------------------------------------------

static inline uint32_t madstr_char_len(const MadlibStringHeader *h) {
    return (uint32_t)h->char_len_lo
         | ((uint32_t)h->char_len_mid << 8)
         | ((uint32_t)h->char_len_hi  << 16);
}

static inline void madstr_set_char_len(MadlibStringHeader *h, uint32_t n) {
    h->char_len_lo  = (uint8_t)(n & 0xFF);
    h->char_len_mid = (uint8_t)((n >>  8) & 0xFF);
    h->char_len_hi  = (uint8_t)((n >> 16) & 0xFF);
}

// Returns 1 if ptr was allocated by madlib (has a valid header), 0 for raw FFI strings.
static inline int madstr_has_header(const char *p) {
    const MadlibStringHeader *h = ((const MadlibStringHeader*)p) - 1;
    return (h->flags & MADSTR_HAS_HEADER) != 0;
}

// Returns a pointer to the header of a madlib-owned string.
// Only call when madstr_has_header(p) is true.
static inline MadlibStringHeader *madstr_header(char *p) {
    return ((MadlibStringHeader*)p) - 1;
}

// O(1) byte_len for owned strings, fallback strlen() for FFI/PCRE strings.
static inline size_t madstr_byte_len(const char *p) {
    const MadlibStringHeader *h = ((const MadlibStringHeader*)p) - 1;
    if (h->flags & MADSTR_HAS_HEADER) return (size_t)h->byte_len;
    return strlen(p);
}

// ---- String allocation (implementation in string.cpp) -----------------------

#ifdef __cplusplus
extern "C" {
#endif

// Allocate a madlib string with a valid header.
// byte_len: number of UTF-8 bytes (NOT including the NUL terminator).
// char_len: Unicode character count, or MADSTR_CHAR_LEN_UNKNOWN if not known.
// The NUL terminator at data[byte_len] is NOT written — callers must do it.
char *madlib__string__alloc(uint32_t byte_len, uint32_t char_len);

#ifdef __cplusplus
}
#endif

// Convenience: allocate with unknown char_len (lazy computation on first String.length call).
static inline char *madlib__string__alloc_bytes(uint32_t byte_len) {
    return madlib__string__alloc(byte_len, MADSTR_CHAR_LEN_UNKNOWN);
}
