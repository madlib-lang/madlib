#include "date.hpp"

#include <gc.h>
#include <math.h>
#include <stdio.h>
#include <sys/time.h>
#include <time.h>

#include <ctime>

#ifdef __MINGW32__
#define timegm _mkgmtime
#endif

#ifdef __cplusplus
extern "C" {
#endif

int64_t madlib__date__now() {
  struct timeval time_now {};
  gettimeofday(&time_now, nullptr);
  time_t msecs_time = (time_now.tv_sec * 1000) + (time_now.tv_usec / 1000);
  return msecs_time;
}

// YYYY-MM-DDTHH:mm:ss.sssZ
char *madlib__date__toISOString(int64_t epochMilliseconds) {
  time_t epochSeconds = trunc(epochMilliseconds / 1000);
  struct tm timeInfo = *gmtime(&epochSeconds);

  int milliseconds = epochMilliseconds - epochSeconds * 1000;
  int seconds = timeInfo.tm_sec;
  int minutes = timeInfo.tm_min;
  int hours = timeInfo.tm_hour;
  int day = timeInfo.tm_mday;
  int month = timeInfo.tm_mon + 1;
  int year = timeInfo.tm_year + 1900;

  char *result = (char *)GC_MALLOC(25);
  sprintf(result, "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ", year, month, day, hours, minutes, seconds, milliseconds);

  return result;
}

// DateInfo flat struct field indices (sorted alphabetically):
// [0]=day, [1]=hours, [2]=milliseconds, [3]=minutes, [4]=month, [5]=seconds, [6]=year
#define DATEINFO_DAY          0
#define DATEINFO_HOURS        1
#define DATEINFO_MILLISECONDS 2
#define DATEINFO_MINUTES      3
#define DATEINFO_MONTH        4
#define DATEINFO_SECONDS      5
#define DATEINFO_YEAR         6
#define DATEINFO_FIELD_COUNT  7

void **madlib__date__toDateInfo(int64_t epochMilliseconds) {
  time_t epochSeconds = trunc(epochMilliseconds / 1000);
  struct tm timeInfo = *gmtime(&epochSeconds);

  void **result = (void **)GC_MALLOC(sizeof(void *) * DATEINFO_FIELD_COUNT);
  result[DATEINFO_DAY]          = (void *)(int64_t)timeInfo.tm_mday;
  result[DATEINFO_HOURS]        = (void *)(int64_t)timeInfo.tm_hour;
  result[DATEINFO_MILLISECONDS] = (void *)(epochMilliseconds - epochSeconds * 1000);
  result[DATEINFO_MINUTES]      = (void *)(int64_t)timeInfo.tm_min;
  result[DATEINFO_MONTH]        = (void *)(int64_t)(timeInfo.tm_mon + 1);
  result[DATEINFO_SECONDS]      = (void *)(int64_t)timeInfo.tm_sec;
  result[DATEINFO_YEAR]         = (void *)(int64_t)(timeInfo.tm_year + 1900);

  return result;
}

int64_t madlib__date__fromDateInfo(void **dateInfo) {
  int64_t year         = (int64_t)dateInfo[DATEINFO_YEAR];
  int64_t month        = (int64_t)dateInfo[DATEINFO_MONTH];
  int64_t day          = (int64_t)dateInfo[DATEINFO_DAY];
  int64_t hours        = (int64_t)dateInfo[DATEINFO_HOURS];
  int64_t minutes      = (int64_t)dateInfo[DATEINFO_MINUTES];
  int64_t seconds      = (int64_t)dateInfo[DATEINFO_SECONDS];
  int64_t milliseconds = (int64_t)dateInfo[DATEINFO_MILLISECONDS];

  struct tm timeInfo = {(int)seconds, (int)minutes, (int)hours, (int)day, (int)month - 1, (int)year - 1900};

  time_t rawTime = timegm(&timeInfo);
  return rawTime * 1000 + milliseconds;
}

#ifdef __cplusplus
}
#endif
