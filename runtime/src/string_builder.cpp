#include <gc.h>
#include <string.h>
#include "string_builder.hpp"
#include "string_header.hpp"
#include "list.hpp"
#include "char.hpp"

#ifdef __cplusplus
extern "C" {
#endif

#define BUILDER_INITIAL_CAP 64u

static void builder_grow(MadlibStringBuilder *sb, uint32_t extra) {
    if (sb->len + extra <= sb->cap) return;
    uint32_t newCap = sb->cap * 2;
    if (newCap < sb->len + extra) newCap = sb->len + extra;
    char *buf = (char*)GC_MALLOC_ATOMIC(newCap);
    if (sb->len > 0) memcpy(buf, sb->data, sb->len);
    sb->data = buf;
    sb->cap  = newCap;
}

MadlibStringBuilder *madlib__string__builder__new(void) {
    MadlibStringBuilder *sb = (MadlibStringBuilder*)GC_MALLOC(sizeof(MadlibStringBuilder));
    sb->data = (char*)GC_MALLOC_ATOMIC(BUILDER_INITIAL_CAP);
    sb->len  = 0;
    sb->cap  = BUILDER_INITIAL_CAP;
    return sb;
}

MadlibStringBuilder *madlib__string__builder__ofString(char *s) {
    MadlibStringBuilder *sb = madlib__string__builder__new();
    return madlib__string__builder__append(s, sb);
}

MadlibStringBuilder *madlib__string__builder__withCapacity(int64_t cap) {
    uint32_t c = (uint32_t)(cap > 0 ? cap : BUILDER_INITIAL_CAP);
    MadlibStringBuilder *sb = (MadlibStringBuilder*)GC_MALLOC(sizeof(MadlibStringBuilder));
    sb->data = (char*)GC_MALLOC_ATOMIC(c);
    sb->len  = 0;
    sb->cap  = c;
    return sb;
}

MadlibStringBuilder *madlib__string__builder__append(char *s, MadlibStringBuilder *sb) {
    uint32_t len = (uint32_t)madstr_byte_len(s);
    builder_grow(sb, len);
    memcpy(sb->data + sb->len, s, len);
    sb->len += len;
    return sb;
}

MadlibStringBuilder *madlib__string__builder__appendChar(int32_t c, MadlibStringBuilder *sb) {
    char *encoded = utf8EncodeChar(c);
    return madlib__string__builder__append(encoded, sb);
}

MadlibStringBuilder *madlib__string__builder__appendLine(char *s, MadlibStringBuilder *sb) {
    uint32_t len = (uint32_t)madstr_byte_len(s);
    builder_grow(sb, len + 1);
    memcpy(sb->data + sb->len, s, len);
    sb->len += len;
    sb->data[sb->len++] = '\n';
    return sb;
}

MadlibStringBuilder *madlib__string__builder__appendMany(madlib__list__Node_t *xs, MadlibStringBuilder *sb) {
    while (xs != NULL && xs->value != NULL) {
        madlib__string__builder__append((char*)xs->value, sb);
        xs = xs->next;
    }
    return sb;
}

MadlibStringBuilder *madlib__string__builder__prepend(char *s, MadlibStringBuilder *sb) {
    uint32_t len = (uint32_t)madstr_byte_len(s);
    builder_grow(sb, len);
    memmove(sb->data + len, sb->data, sb->len);
    memcpy(sb->data, s, len);
    sb->len += len;
    return sb;
}

MadlibStringBuilder *madlib__string__builder__prependChar(int32_t c, MadlibStringBuilder *sb) {
    char *encoded = utf8EncodeChar(c);
    return madlib__string__builder__prepend(encoded, sb);
}

MadlibStringBuilder *madlib__string__builder__insert(int64_t idx, char *s, MadlibStringBuilder *sb) {
    if (idx <= 0) return madlib__string__builder__prepend(s, sb);
    if (idx >= (int64_t)sb->len) return madlib__string__builder__append(s, sb);
    uint32_t len = (uint32_t)madstr_byte_len(s);
    builder_grow(sb, len);
    memmove(sb->data + idx + len, sb->data + idx, sb->len - (uint32_t)idx);
    memcpy(sb->data + idx, s, len);
    sb->len += len;
    return sb;
}

MadlibStringBuilder *madlib__string__builder__deleteRange(int64_t start, int64_t end, MadlibStringBuilder *sb) {
    if (start < 0) start = 0;
    if (end > (int64_t)sb->len) end = (int64_t)sb->len;
    if (start >= end) return sb;
    uint32_t removedLen = (uint32_t)(end - start);
    memmove(sb->data + start, sb->data + end, sb->len - (uint32_t)end);
    sb->len -= removedLen;
    return sb;
}

MadlibStringBuilder *madlib__string__builder__replace(int64_t start, int64_t end, char *s, MadlibStringBuilder *sb) {
    madlib__string__builder__deleteRange(start, end, sb);
    return madlib__string__builder__insert(start, s, sb);
}

MadlibStringBuilder *madlib__string__builder__reverse(MadlibStringBuilder *sb) {
    if (sb->len == 0) return sb;
    // NUL-terminate so utf8Decode can scan it
    builder_grow(sb, 1);
    sb->data[sb->len] = '\0';
    int32_t *chars = utf8Decode(sb->data);
    // Count codepoints
    int count = 0;
    while (chars[count] != 0) count++;
    // Rebuild in reverse
    sb->len = 0;
    for (int i = count - 1; i >= 0; i--) {
        madlib__string__builder__appendChar(chars[i], sb);
    }
    return sb;
}

MadlibStringBuilder *madlib__string__builder__clear(MadlibStringBuilder *sb) {
    sb->len = 0;
    return sb;
}

char *madlib__string__builder__toString(MadlibStringBuilder *sb) {
    char *result = madlib__string__alloc_bytes(sb->len);
    memcpy(result, sb->data, sb->len);
    result[sb->len] = '\0';
    return result;
}

int64_t madlib__string__builder__length(MadlibStringBuilder *sb) {
    return (int64_t)sb->len;
}

bool madlib__string__builder__isEmpty(MadlibStringBuilder *sb) {
    return sb->len == 0;
}

#ifdef __cplusplus
}
#endif
