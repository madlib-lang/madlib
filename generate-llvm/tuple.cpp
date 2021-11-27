#include <iostream>

struct Tuple2
{
  void *a;
  void *b;
};


char* v() {
  char* s = (char*) malloc(sizeof(char) * 20);
  return s;
}


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

  void *fst(Tuple2 *tuple)
  {
    return tuple->a;
  }

  void *snd(Tuple2 *tuple)
  {
    return tuple->b;
  }
#ifdef __cplusplus
}
#endif

int main() {
  puts("before\nafter");
  return 0;
}
