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
#include <set>
#include "instruction.h"
#include "operand.h"
#include "highlevel.h"
#include "highlevel_defuse.h"

namespace {

// High-level opcodes that do NOT have a destination operand
const std::set<HighLevelOpcode> NO_DEST = {
  HINS_nop,
  HINS_ret,
  HINS_jmp,
  HINS_enter,
  HINS_leave,
  HINS_cjmp_t,
  HINS_cjmp_f,
};

// Does the instruction have a destination operand?
bool has_dest_operand(HighLevelOpcode hl_opcode) {
  return NO_DEST.count(hl_opcode) == 0;
}

}

namespace HighLevel {

// A high-level instruction is a def if it has a destination operand,
// and the destination operand is a vreg.
bool is_def(Instruction *ins) {
  // HINS_call is a special case: it implicitly is a def of vr0
  if (ins->get_opcode() == HINS_call)
    return true;

  if (!has_dest_operand(HighLevelOpcode(ins->get_opcode())))
    return false;

  assert(ins->get_num_operands() > 0);
  Operand dest = ins->get_operand(0);

  return dest.get_kind() == Operand::VREG;
}

int get_def_vreg(Instruction *ins) {
  assert(is_def(ins));

  // a HINS_call instruction is a def of vr0:
  // otherwise, the assigned-to vreg is the base register
  // of the first Operand
  return (ins->get_opcode() == HINS_call)
         ? 0
         : ins->get_operand(0).get_base_reg();
}

bool is_use(Instruction *ins, unsigned operand_index) {
  Operand operand = ins->get_operand(operand_index);

  if (operand_index == 0 && has_dest_operand(HighLevelOpcode(ins->get_opcode()))) {
    // special case: if the instruction has a destination operand, but the operand
    // is a memory reference, then the base register and index register (if either
    // are present) are refs
    return operand.is_memref() && (operand.has_base_reg() || operand.has_index_reg());
  }

  // in general, an operand is a ref if it mentions a virtual register
  return operand.has_base_reg() || operand.has_index_reg();
}

}
