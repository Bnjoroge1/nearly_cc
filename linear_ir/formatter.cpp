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

#include <stdexcept>
#include "cpputil.h"
#include "formatter.h"

Formatter::Formatter() {
}

Formatter::~Formatter() {
}

std::string Formatter::format_operand(const Operand &operand) const {
  switch (operand.get_kind()) {
  case Operand::IMM_IVAL:
    return cpputil::format("$%ld", operand.get_imm_ival());

  case Operand::LABEL:
    return operand.get_label();

  case Operand::IMM_LABEL:
    return cpputil::format("$%s", operand.get_label().c_str());

  default:
    {
      std::string exmsg = cpputil::format("Operand kind %d not handled", operand.get_kind());
      throw std::runtime_error(exmsg);
    }
  }
}
