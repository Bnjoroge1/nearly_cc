struct Foo {
  int x;
};

int main(void) {
  struct Foo s;
  s.x = 0;

  // error: returned value is incompatible with int
  // Note that your SemanticAnalysis class will need to override
  // the visit_return_expression_statement member function in order
  // to type check return statements. You will also need to implement
  // some way of getting the return type of the current functtion.
  return s;
}
