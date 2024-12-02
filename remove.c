// test for an integer promotion

void print_i32(int n);
void print_nl(void);

int main(void) {
  char c;
  c = -67;
  print_i32(c);
  print_nl();
  return 0;
}
