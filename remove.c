struct Point {
  int x, y;
};

int main(void) {
  struct Point points[10];
  int i;

  for (i = 0; i < 10; i = i + 1) {
    points[i].x = i;
    points[i].y = i;
  }

  return 0;
}
