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

#ifndef LOWLEVEL_FORMATTER_H
#define LOWLEVEL_FORMATTER_H

#include "formatter.h"

//! Implementation of Formatter for low-level code.
class LowLevelFormatter : public Formatter {
public:
  LowLevelFormatter();
  virtual ~LowLevelFormatter();

  //! Convert a low-level Operand to a formatted string.
  //!
  //! @param operand the low-level Operand to format
  //! @return the string formatted from the Operand
  virtual std::string format_operand(const Operand &operand) const;

  //! Convert a low-level Instruction to a formatted string.
  //!
  //! @param ins the low-level Instruction to format
  //! @return the string formatted from the Instruction
  virtual std::string format_instruction(const Instruction *ins) const;
};

#endif // LOWLEVEL_FORMATTER_H
