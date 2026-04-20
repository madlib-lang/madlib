#pragma once
#include <stdint.h>
#include <stdbool.h>
#include "string_header.hpp"
#include "list.hpp"
#include "char.hpp"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * MadlibStringBuilder — a mutable, growable string buffer.
 *
 * Exposed to Madlib as an opaque Builder type.
 * All mutating functions return the same pointer so chaining works naturally.
 */
typedef struct {
    char     *data;   // raw byte buffer (no MadlibStringHeader — internal only)
    uint32_t  len;    // bytes written so far
    uint32_t  cap;    // allocated capacity
} MadlibStringBuilder;

MadlibStringBuilder *madlib__string__builder__new(void);
MadlibStringBuilder *madlib__string__builder__ofString(char *s);
MadlibStringBuilder *madlib__string__builder__withCapacity(int64_t cap);
MadlibStringBuilder *madlib__string__builder__append(char *s, MadlibStringBuilder *sb);
MadlibStringBuilder *madlib__string__builder__appendChar(int32_t c, MadlibStringBuilder *sb);
MadlibStringBuilder *madlib__string__builder__appendLine(char *s, MadlibStringBuilder *sb);
MadlibStringBuilder *madlib__string__builder__appendMany(madlib__list__Node_t *xs, MadlibStringBuilder *sb);
MadlibStringBuilder *madlib__string__builder__prepend(char *s, MadlibStringBuilder *sb);
MadlibStringBuilder *madlib__string__builder__prependChar(int32_t c, MadlibStringBuilder *sb);
MadlibStringBuilder *madlib__string__builder__insert(int64_t idx, char *s, MadlibStringBuilder *sb);
MadlibStringBuilder *madlib__string__builder__deleteRange(int64_t start, int64_t end, MadlibStringBuilder *sb);
MadlibStringBuilder *madlib__string__builder__replace(int64_t start, int64_t end, char *s, MadlibStringBuilder *sb);
MadlibStringBuilder *madlib__string__builder__reverse(MadlibStringBuilder *sb);
MadlibStringBuilder *madlib__string__builder__clear(MadlibStringBuilder *sb);
char                *madlib__string__builder__toString(MadlibStringBuilder *sb);
int64_t              madlib__string__builder__length(MadlibStringBuilder *sb);
bool                 madlib__string__builder__isEmpty(MadlibStringBuilder *sb);

#ifdef __cplusplus
}
#endif
