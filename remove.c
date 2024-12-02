void print_i32(int n);
void print_space(void);
void print_nl(void);

int main(void) {
  int i, j, n;

  int m[10][10];

  for (i = 0; i <= 9; i = i + 1) {
    n = 0;

    for (j = 0; j <= 9; j = j + 1) {
      m[i][j] = n;
      n = n + i;
    }
  }

  for (i = 0; i <= 9; i = i + 1) {
    for (j = 0; j <= 9; j = j + 1) {
      print_i32(m[i][j]);
      print_space();
    }
    print_nl();
  }

  return 0;
}
