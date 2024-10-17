#ifndef DEBUGVAR_H
#define DEBUGVAR_H

class DebugVar {
public:
  DebugVar(const char *name, bool &var);
  DebugVar(const char *name, int &var);
  ~DebugVar();
};

#endif // DEBUGVAR_H
