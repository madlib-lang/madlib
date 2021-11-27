#ifdef __cplusplus
extern "C" {
#endif

bool *__eqString__(char **s1, char **s2);

bool __areStringsEqual__(char *s1, char *s2);

double *__strLength__(char *s);

char *__strConcat__(char *s1, char *s2);

char *__stripTrailingZeros__(char *number);

#ifdef __cplusplus
}
#endif
