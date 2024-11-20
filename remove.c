void print_i32(int n);
void print_nl(void);

int main(void) {
  int n;
  n = 10;
  while (n > 0) {
    print_i32(n);
    print_nl();
    n = n - 1;
  }
  return 0;
}
