void strcpy(char *dst, const char *src);

struct Item {
  char name[32];
  int price;
  int quantity;
};

int main(void) {
  struct Item item;

  // item.name is an array of char elements, and used without
  // a subscript it "decays" to become a pointer to char
  strcpy(item.name, "apples");
  item.price = 120;
  item.quantity = 10;

  return 0;
}
