int main(void) {
  int a, *p, **pp;
  p = &a;
  pp = &p;
  **pp = 17;
  return a;
}
