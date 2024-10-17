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

#ifndef FORMATTER_H
#define FORMATTER_H

#include "operand.h"
class Instruction;

//! A Formatter turns Operand and Instruction objects into
//! strings, which in turn allows high-level and low-level code
//! to be printed.
class Formatter {
public:
  Formatter();
  virtual ~Formatter();

  //! Convert an Operand to a formatted string.
  //! The default implementation of format_operand is useful
  //! for immediate values and labels. Subclasses should override
  //! to implement more specific kinds of operands (registers,
  //! memory references, etc.)
  //!
  //! @param operand the Operand to format
  //! @return the string formatted from the Operand
  virtual std::string format_operand(const Operand &operand) const;

  //! Convert an Instruction to a formatted string.
  //!
  //! @param ins the Instruction to format
  //! @return the string formatted from the Instruction
  virtual std::string format_instruction(const Instruction *ins) const = 0;
};

#endif // FORMATTER_H
