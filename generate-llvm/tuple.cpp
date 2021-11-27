#include <iostream>

struct Tuple2
{
  void *a;
  void *b;
};

#ifdef __cplusplus
extern "C"
{
#endif
  Tuple2 *createTuple2(void *a, void *b)
  {
    Tuple2 *t = new Tuple2();

    t->a = a;
    t->b = b;

    return t;
  }
#ifdef __cplusplus
}
#endif
