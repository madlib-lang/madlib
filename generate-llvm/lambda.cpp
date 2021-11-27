
struct X {
  int x;
};

auto f() {
  X *r = new X();

  r->x = 3;

  return r;
}
