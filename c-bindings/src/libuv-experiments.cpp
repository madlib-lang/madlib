#include <stdio.h>
#include <iostream>

int main() {
  // char *s = "\u2321";
  // printf("char code1: %u\n", s[0]);
  // puts((char*) s);

  char *s = (char*)malloc(10);
  s[0] = '\\';
  s[1] = 'x';
  s[2] = '2';
  s[3] = '7';
  s[4] = '1';
  s[5] = '3';
  s[6] = 0;
  puts(s);


  // wchar_t *s2 = (wchar_t*) malloc(sizeof(wchar_t)*2);
  wchar_t *s2 = L"\u2713";
  // s2[1] = 0;

  wprintf(s2);
  puts("");
  // puts((char*) s2);
  return 0;
}
