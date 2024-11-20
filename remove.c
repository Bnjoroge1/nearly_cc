void print_i64(long n);
void print_nl(void);

int main(void) {
  long a, b, c;
  a = 14642L;
  a = a * 341491L;
  b = 29L;
  c = a * b;
  print_i64(c);
  print_nl();
  return 0;
}
