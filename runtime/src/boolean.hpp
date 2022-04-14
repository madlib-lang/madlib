#ifndef BOOLEAN_H
#define BOOLEAN_H



#ifdef __cplusplus
extern "C" {
#endif

char *madlib__boolean__internal__showBoolean(bool b);
char *madlib__boolean__internal__inspectBoolean(bool b);

bool madlib__boolean__internal__eq(bool a, bool b);

#ifdef __cplusplus
}
#endif

#endif // BOOLEAN_H