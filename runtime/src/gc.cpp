
#include <gc.h>
#include "list.hpp"
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>


#ifdef __cplusplus
extern "C" {
#endif

static bool isTruthyEnvValue(const char *value) {
  if (value == NULL || value[0] == '\0') {
    return false;
  }

  return
    strcmp(value, "1") == 0
    || strcmp(value, "true") == 0
    || strcmp(value, "TRUE") == 0
    || strcmp(value, "yes") == 0
    || strcmp(value, "YES") == 0;
}

static size_t clampSize(size_t value, size_t minValue, size_t maxValue) {
  if (value < minValue) {
    return minValue;
  }
  if (value > maxValue) {
    return maxValue;
  }
  return value;
}

static uint64_t getPhysicalMemoryBytes() {
  long pages = sysconf(_SC_PHYS_PAGES);
  long pageSize = sysconf(_SC_PAGESIZE);
  if (pages <= 0 || pageSize <= 0) {
    return 8ULL * 1024ULL * 1024ULL * 1024ULL;
  }

  return (uint64_t)pages * (uint64_t)pageSize;
}

static size_t saturatingMul(size_t a, size_t b) {
  if (a == 0 || b == 0) {
    return 0;
  }
  if (a > ((size_t)-1) / b) {
    return (size_t)-1;
  }
  return a * b;
}

static void reserveHeapForListCount(int64_t count, size_t multiplier) {
  if (count < 1000000) {
    return;
  }

  const bool gcDiag = isTruthyEnvValue(getenv("MADLIB_GC_DIAG"));
  const uint64_t physicalMemory = getPhysicalMemoryBytes();
  const size_t listBytes = saturatingMul((size_t)count, sizeof(madlib__list__Node_t));
  const size_t requestedReserve = saturatingMul(listBytes, multiplier);
  const size_t maxReserve = clampSize((size_t)(physicalMemory / 2ULL), 512ULL * 1024ULL * 1024ULL, 8ULL * 1024ULL * 1024ULL * 1024ULL);
  const size_t reserveBytes = clampSize(requestedReserve, 256ULL * 1024ULL * 1024ULL, maxReserve);

  GC_expand_hp(reserveBytes);

  if (gcDiag) {
    fprintf(
      stderr,
      "[madlib-gc] reserve-list: count=%lld list_bytes=%zu multiplier=%zu reserve=%zu cap=%zu\n",
      (long long)count,
      listBytes,
      multiplier,
      reserveBytes,
      maxReserve
    );
  }
}

static GC_warn_proc originalWarnProc = NULL;

static void madlib__gc__warnProc(char *msg, GC_word arg) {
  if (strstr(msg, "very large block") != NULL) {
    return;
  }
  if (originalWarnProc != NULL) {
    originalWarnProc(msg, arg);
  }
}

void madlib__gc__configureAfterInit() {
  originalWarnProc = GC_get_warn_proc();
  GC_set_warn_proc(madlib__gc__warnProc);
}

void __setFreeSpaceDivisor__() {
  GC_set_free_space_divisor(1);
}

void *madlib__gc__reserveList(int64_t count) {
  reserveHeapForListCount(count, 2);
  return NULL;
}

void *madlib__gc__reserveListFromList(madlib__list__Node_t *list) {
  int64_t count = madlib__list__length(list);
  reserveHeapForListCount(count, 1);
  return NULL;
}

void madlib__gc__forceCollect() {
  GC_gcollect();
}

#ifdef __cplusplus
}
#endif
