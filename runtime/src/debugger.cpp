#include <gc.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

char *madlib__debugger__sgets(char *line, size_t size)
{
   size_t i;
   for ( i = 0; i < size - 1; ++i )
   {
      int ch = fgetc(stdin);
      if ( ch == '\n' || ch == EOF )
      {
         break;
      }
      line[i] = ch;
   }
   line[i] = '\0';
   return line;
}

char *madlib__debugger__prompt() {
  char *line = (char *) GC_MALLOC_ATOMIC(256);

  return madlib__debugger__sgets(line, 256);
}

#ifdef __cplusplus
}
#endif
