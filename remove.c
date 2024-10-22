struct Point {
  int x, y;
};

struct Player {
  struct Point location;
  char symbol;
  short health;
};

int main(void) {
  struct Player p;

  p.location.x = 3;
  p.location.y = 4;
  p.symbol = '@';
  p.health = 100;

  return 0;
}
