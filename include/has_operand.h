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

#ifndef HAS_OPERAND_H
#define HAS_OPERAND_H

#include "operand.h"

//! HasOperand is a base class for NodeBase, which in turn is a base class
//! for Node. The idea is that each Node in an AST has an Operand which
//! represents the location (register or memory) where the results of
//! expression evaluation are stored. By default, the Operand is "invalid".
//! The `set_operand()` member function must be called to set the Operand.
class HasOperand {
private:
  Operand m_operand;

public:
  //! Set the Node's Operand.
  //! @param op the Operand to set
  void set_operand(const Operand &op) { m_operand = op; }

  //! Check whether this Node has a valid Operand.
  //! @return true if the Node has a valid Operand, false otherwise
  bool has_operand() const { return m_operand.get_kind() != Operand::NONE; }

  //! Get this Node's Operand.
  //! @return this Node's Operand
  const Operand get_operand() const { return m_operand; }
};

#endif // HAS_OPERAND_H
