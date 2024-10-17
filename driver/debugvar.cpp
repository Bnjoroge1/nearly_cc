#include <cstring>
#include <cstdlib>
#include <string>
#include "debugvar.h"

DebugVar::DebugVar(const char *name, bool &var) {
  const char *val = getenv(name);
  var = (val && strcmp(val, "yes") == 0);
}

DebugVar::DebugVar(const char *name, int &var) {
  const char *val = getenv(name);
  if (val != nullptr)
    var = std::stoi(val);
  else
    var = 0;
}

DebugVar::~DebugVar() {
}
