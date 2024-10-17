// Copyright (c) 2021-2023, David H. Hovemeyer <david.hovemeyer@gmail.com>
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
// OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
// ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.

#ifndef GLOBAL_VARIABLE_H
#define GLOBAL_VARIABLE_H

#include <memory>
#include "type.h"

//! A GlobalVariable represents a global variable, and specifies its
//! name and data type. Each global variable in the program should
//! be added to the Module (and thus made available when target code
//! is printed.) Note that this type has value semantics (copying
//! and assignment are allowed.)
class GlobalVariable {
private:
  std::string m_name;
  std::shared_ptr<Type> m_type;

public:
  //! Constructor.
  //! @param name the name of the global variable
  //! @param type shared pointer to the Type of the global variable
  GlobalVariable(const std::string &name, std::shared_ptr<Type> type);
  ~GlobalVariable();

  //! Get the name of the global variable.
  //! @return the name of the global variable
  std::string get_name() const { return m_name; }

  //! Get the Type of the global variable.
  //! @return shared pointer to the Type of the global variable
  std::shared_ptr<Type> get_type() const { return m_type; }
};


#endif // GLOBAL_VARIABLE_H
