#include "date.hpp"
#include <gc.h>
#include <sys/time.h>
#include <ctime>
#include <time.h>
#include <math.h>
#include <stdio.h>


#ifdef __MINGW32__
  #define timegm _mkgmtime
#endif


#ifdef __cplusplus
extern "C" {
#endif

int64_t madlib__date__now() {
  struct timeval time_now{};
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

  char *result = (char*) GC_MALLOC(25);
  sprintf(result, "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ", year, month, day, hours, minutes, seconds, milliseconds);

  return result;
}


madlib__record__Record_t *madlib__date__toDateInfo(int64_t epochMilliseconds) {
  time_t epochSeconds = trunc(epochMilliseconds / 1000);
  struct tm timeInfo = *gmtime(&epochSeconds);

  int64_t milliseconds = epochMilliseconds - epochSeconds * 1000;
  int64_t seconds = timeInfo.tm_sec;
  int64_t minutes = timeInfo.tm_min;
  int64_t hours = timeInfo.tm_hour;
  int64_t day = timeInfo.tm_mday;
  int64_t month = timeInfo.tm_mon + 1;
  int64_t year = timeInfo.tm_year + 1900;

  madlib__record__Field_t *millisecondField = (madlib__record__Field_t *) GC_MALLOC(sizeof(madlib__record__Field_t));
  millisecondField->name = (char*) "milliseconds";
  millisecondField->value = (int64_t*) milliseconds;

  madlib__record__Field_t *secondsField = (madlib__record__Field_t *) GC_MALLOC(sizeof(madlib__record__Field_t));
  secondsField->name = (char*) "seconds";
  secondsField->value = (int64_t*) seconds;

  madlib__record__Field_t *minutesField = (madlib__record__Field_t *) GC_MALLOC(sizeof(madlib__record__Field_t));
  minutesField->name = (char*) "minutes";
  minutesField->value = (int64_t*) minutes;

  madlib__record__Field_t *hoursField = (madlib__record__Field_t *) GC_MALLOC(sizeof(madlib__record__Field_t));
  hoursField->name = (char*) "hours";
  hoursField->value = (int64_t*) hours;

  madlib__record__Field_t *dayField = (madlib__record__Field_t *) GC_MALLOC(sizeof(madlib__record__Field_t));
  dayField->name = (char*) "day";
  dayField->value = (int64_t*) day;

  madlib__record__Field_t *monthField = (madlib__record__Field_t *) GC_MALLOC(sizeof(madlib__record__Field_t));
  monthField->name = (char*) "month";
  monthField->value = (int64_t*) month;

  madlib__record__Field_t *yearField = (madlib__record__Field_t *) GC_MALLOC(sizeof(madlib__record__Field_t));
  yearField->name = (char*) "year";
  yearField->value = (int64_t*) year;

  madlib__record__Record_t *result = (madlib__record__Record_t*) GC_MALLOC(sizeof(madlib__record__Record_t));
  result->fieldCount = 7;
  result->fields = (madlib__record__Field_t**) GC_MALLOC(sizeof(madlib__record__Field_t*) * 7);
  result->fields[0] = millisecondField;
  result->fields[1] = secondsField;
  result->fields[2] = minutesField;
  result->fields[3] = hoursField;
  result->fields[4] = dayField;
  result->fields[5] = monthField;
  result->fields[6] = yearField;

  return result;
}

int64_t madlib__date__fromDateInfo(madlib__record__Record_t *dateInfo) {
  int64_t year = (int64_t) madlib__record__internal__selectField((char*)"year", dateInfo);
  int64_t month = (int64_t) madlib__record__internal__selectField((char*)"month", dateInfo);
  int64_t day = (int64_t) madlib__record__internal__selectField((char*)"day", dateInfo);
  int64_t hours = (int64_t) madlib__record__internal__selectField((char*)"hours", dateInfo);
  int64_t minutes = (int64_t) madlib__record__internal__selectField((char*)"minutes", dateInfo);
  int64_t seconds = (int64_t) madlib__record__internal__selectField((char*)"seconds", dateInfo);
  int64_t milliseconds = (int64_t) madlib__record__internal__selectField((char*)"milliseconds", dateInfo);

  struct tm timeInfo = { (int) seconds, (int) minutes, (int) hours, (int) day, (int) month - 1, (int) year - 1900 };

  time_t rawTime = timegm(&timeInfo);
  return rawTime * 1000 + milliseconds;
}

#ifdef __cplusplus
}
#endif


// madlib__record__Record_t *madlib__date__now() {
//   struct timeval currentTime{};
//   gettimeofday(&currentTime, nullptr);
//   time_t epochMilliseconds = (currentTime.tv_sec * 1000) + (currentTime.tv_usec / 1000);
//   time_t epochSeconds = currentTime.tv_sec;
//   struct tm timeInfo = *gmtime(&epochSeconds);

//   int64_t milliseconds = epochMilliseconds - epochSeconds * 1000;
//   int64_t seconds = timeInfo.tm_sec;
//   int64_t minutes = timeInfo.tm_min;
//   int64_t hours = timeInfo.tm_hour;
//   int64_t day = timeInfo.tm_mday;
//   int64_t month = timeInfo.tm_mon + 1;
//   int64_t year = timeInfo.tm_year;

//   madlib__record__Record_t *result = (madlib__record__Record_t*) GC_MALLOC(sizeof(madlib__record__Record_t));

//   madlib__record__Field_t *millisecondField = (madlib__record__Field_t *) GC_MALLOC(sizeof(madlib__record__Field_t));
//   millisecondField->name = (char*) "ms";
//   millisecondField->value = (int64_t*) milliseconds;


//   return result;
// }
