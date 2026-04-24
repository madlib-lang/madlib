#ifndef MINIZ_EXPORT_H
#define MINIZ_EXPORT_H

#if defined(_WIN32) && defined(MINIZ_DLL)
  #if defined(miniz_EXPORTS)
    #define MINIZ_EXPORT __declspec(dllexport)
  #else
    #define MINIZ_EXPORT __declspec(dllimport)
  #endif
#else
  #define MINIZ_EXPORT
#endif

#endif // MINIZ_EXPORT_H
