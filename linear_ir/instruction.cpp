// Copyright (c) 2021-2024, David H. Hovemeyer <david.hovemeyer@gmail.com>
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

#include <cassert>
#include "instruction.h"

Instruction::Instruction(int opcode)
  : Instruction(opcode, Operand(), Operand(), Operand()) {
}

Instruction::Instruction(int opcode, const Operand &op1)
  : Instruction(opcode, op1, Operand(), Operand()) {
}

Instruction::Instruction(int opcode, const Operand &op1, const Operand &op2)
  : Instruction(opcode, op1, op2, Operand()) {
}

Instruction::Instruction(int opcode, const Operand &op1, const Operand &op2, const Operand &op3)
  : m_opcode(opcode)
  , m_symbol(nullptr) {
  // Don't allow a "real" Operand to follow an Operand marked
  // as kind Operand::NONE

  if (op1.get_kind() != Operand::NONE) {
    m_operands.push_back(op1);
  }
  if (op2.get_kind() != Operand::NONE) {
    assert(op1.get_kind() != Operand::NONE);
    m_operands.push_back(op2);
  }
  if (op3.get_kind() != Operand::NONE) {
    assert(op2.get_kind() != Operand::NONE);
    m_operands.push_back(op3);
  }
}

Instruction::~Instruction() {
}

int Instruction::get_opcode() const {
  return m_opcode;
}

unsigned Instruction::get_num_operands() const {
  return unsigned(m_operands.size());
}

const Operand &Instruction::get_operand(unsigned index) const {
  assert(index < get_num_operands());
  return m_operands[index];
}


void Instruction::set_operand(unsigned index, const Operand &operand) {
  assert(index < get_num_operands());
  m_operands[index] = operand;
}

Operand Instruction::get_last_operand() const {
  assert(get_num_operands() > 0);
  return m_operands[get_num_operands() - 1];
}
