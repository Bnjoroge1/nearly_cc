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

#include <cassert>
#include <set>
#include <algorithm>
#include <iterator>
#include "operand.h"
#include "instruction.h"
#include "symtab.h"
#include "lowlevel.h"
#include "lowlevel_defuse.h"

namespace {

const MachineReg ARG_REGS[] = { MREG_RDI, MREG_RSI, MREG_RDX, MREG_R8, MREG_R9 };

// "Normal" instructions which have no implicit defs or uses,
// and for which the last operand is a destination operand
const std::set<LowLevelOpcode> NORMAL_OPCODES = {
  MINS_MOVB,
  MINS_MOVW,
  MINS_MOVL,
  MINS_MOVQ,
  MINS_ADDB,
  MINS_ADDW,
  MINS_ADDL,
  MINS_ADDQ,
  MINS_SUBB,
  MINS_SUBW,
  MINS_SUBL,
  MINS_SUBQ,
  MINS_LEAQ,
  MINS_IMULL,
  MINS_IMULQ,
  MINS_POPQ,
  MINS_MOVSBW,
  MINS_MOVSBL,
  MINS_MOVSBQ,
  MINS_MOVSWL,
  MINS_MOVSWQ,
  MINS_MOVSLQ,
  MINS_MOVZBW,
  MINS_MOVZBL,
  MINS_MOVZBQ,
  MINS_MOVZWL,
  MINS_MOVZWQ,
  MINS_MOVZLQ,
  MINS_SETL,
  MINS_SETLE,
  MINS_SETG,
  MINS_SETGE,
  MINS_SETE,
  MINS_SETNE,
};

// Subset of NORMAL_OPCODES where the destination is not a use
// (basically, just the move instructions, set instructions,
// leaq, and popq)
const std::set<LowLevelOpcode> MOVE_OPCODES = {
  MINS_MOVB,
  MINS_MOVW,
  MINS_MOVL,
  MINS_MOVQ,
  MINS_MOVSBW,
  MINS_MOVSBL,
  MINS_MOVSBQ,
  MINS_MOVSWL,
  MINS_MOVSWQ,
  MINS_MOVSLQ,
  MINS_MOVZBW,
  MINS_MOVZBL,
  MINS_MOVZBQ,
  MINS_MOVZWL,
  MINS_MOVZWQ,
  MINS_MOVZLQ,
  MINS_SETL,
  MINS_SETLE,
  MINS_SETG,
  MINS_SETGE,
  MINS_SETE,
  MINS_SETNE,
  MINS_LEAQ,
  MINS_POPQ,
};

// Opcodes that are defs, but have implicit operands
const std::set<LowLevelOpcode> IMPLICIT_DEF_OPCODES = {
  MINS_IDIVL,
  MINS_IDIVQ,
  MINS_CDQ,
  MINS_CQTO,
  MINS_CALL,
};

// Opcodes that are never defs, and in which explicit operands
// are always uses
const std::set<LowLevelOpcode> NON_DEF_OPCODES = {
  MINS_RET,
  MINS_JMP,
  MINS_JE,
  MINS_JNE,
  MINS_JL,
  MINS_JLE,
  MINS_JG,
  MINS_JGE,
  MINS_JB,
  MINS_JBE,
  MINS_JA,
  MINS_JAE,
  MINS_CMPB,
  MINS_CMPW,
  MINS_CMPL,
  MINS_CMPQ,
};

// Opcodes that are never uses
const std::set<LowLevelOpcode> NON_USE_OPCODES = {
  MINS_JMP,
  MINS_JE,
  MINS_JNE,
  MINS_JL,
  MINS_JLE,
  MINS_JG,
  MINS_JGE,
  MINS_JB,
  MINS_JBE,
  MINS_JA,
  MINS_JAE,
};

// opcodes which must be handled specially
// MINS_CALL: def of %rax, use of whichever arg regs are used
// MINS_IDIVL, MINS_IDIVQ: implicit def and use of %rax and %rdx
// MINS_CDQ, MINS_CQTO: implicit use of %rax, implicit def of %rdx
// MINS_RET: implicit use of %rax?

}

bool LowLevel::is_def(Instruction *ins) {
  LowLevelOpcode ll_opcode = LowLevelOpcode(ins->get_opcode());

  // "Normal" opcodes: instruction is a def IFF the last
  // operand is not a memory reference
  if (NORMAL_OPCODES.count(ll_opcode) > 0)
    return !ins->get_last_operand().is_memref();

  // The instruction is a def IFF it has implicit defs
  return IMPLICIT_DEF_OPCODES.count(ll_opcode) > 0;
}

std::vector<MachineReg> LowLevel::get_def_mregs(Instruction *ins) {
  assert(is_def(ins));

  LowLevelOpcode ll_opcode = LowLevelOpcode(ins->get_opcode());

  // special cases

  if (ll_opcode == MINS_CALL) {
    // Per x86-64 calling conventions, a caller must assume that a
    // called function will modify every caller-saved register
    std::vector<MachineReg> defs;
    std::copy(ARG_REGS, ARG_REGS + 6, std::back_inserter(defs));
    defs.push_back(MREG_RAX);
    return defs;
  }

  if (ll_opcode == MINS_IDIVL || ll_opcode == MINS_IDIVQ)
    return std::vector<MachineReg>({ MREG_RAX, MREG_RDX });

  if (ll_opcode == MINS_CDQ || ll_opcode == MINS_CQTO)
    return std::vector<MachineReg>({ MREG_RDX });

  if (ll_opcode == MINS_RET)
    return std::vector<MachineReg>();

  // For all "normal" instructions, the last operand is the one
  // being defined
  if (NORMAL_OPCODES.count(ll_opcode) > 0) {
    Operand last = ins->get_last_operand();
    assert(!last.is_memref());
    assert(last.has_base_reg());
    return std::vector<MachineReg>({ MachineReg(last.get_base_reg()) });
  }

  // Assume no defs
  assert(NON_DEF_OPCODES.count(ll_opcode) > 0);
  return std::vector<MachineReg>();
}

std::vector<MachineReg> LowLevel::get_use_mregs(Instruction *ins) {
  LowLevelOpcode ll_opcode = LowLevelOpcode(ins->get_opcode());
  unsigned num_operands = ins->get_num_operands();

  if (ll_opcode == MINS_CALL) {
    // Determine which argument registers are used.
    // Note that this will require that the Instruction
    // has the pointer to the Symbol representing the
    // called functions, so we can check how many arguments
    // it has.

    std::vector<MachineReg> uses;

    // Determine the number of arguments being passed
    // (this assumes that MINS_CALL instructions contain a pointer
    // to the Symbol with the information about the called function)
    Symbol *fn_sym = ins->get_symbol();
    assert(fn_sym != nullptr);
    std::shared_ptr<Type> fn_type = fn_sym->get_type();
    assert(fn_type->is_function());
    unsigned num_args = fn_type->get_num_members();

    // Add argument registers to the uses
    for (unsigned i = 0; i < num_args; ++i)
      uses.push_back(ARG_REGS[i]);

    return uses;
  }

  if (ll_opcode == MINS_IDIVL || ll_opcode == MINS_IDIVQ)
    return std::vector<MachineReg>({ MREG_RAX, MREG_RDX });

  if (ll_opcode == MINS_CDQ || ll_opcode == MINS_CQTO)
    return std::vector<MachineReg>({ MREG_RAX });

  if (ll_opcode == MINS_RET)
    return std::vector<MachineReg>({ MREG_RAX });

  if (ll_opcode == MINS_PUSHQ)
    return std::vector<MachineReg>({ MachineReg(ins->get_operand(0).get_base_reg()) });

  // popq instructions are not a use
  if (ll_opcode == MINS_POPQ)
    return std::vector<MachineReg>();


  // For "normal" opcodes and "non-def" opcodes: any mreg mentioned that isn't
  // the destination operand is a use
  bool is_non_def = NON_DEF_OPCODES.count(ll_opcode) > 0;
  if (is_non_def || NORMAL_OPCODES.count(ll_opcode) > 0) {
    // For moves, the last operand is *not* a use
    bool is_move = MOVE_OPCODES.count(ll_opcode) > 0;

    // Collect used mregs in a set, since the same mreg could
    // be mentioned more than once in the instruction
    std::set<MachineReg> uses;

    for (unsigned i = 0; i < num_operands; ++i) {
      Operand operand = ins->get_operand(i);
      bool is_last = (i == num_operands - 1);

      // If the operand is not a memory reference,
      // then it is a use if either
      //   - it's not the last operand, or
      //   - it's not a move instruction
      //   - it is a non-def instruction
      //
      // Note that if the operand *is* a memory reference, then
      // any machine registers mentioned *are* uses.

      bool is_use = operand.is_memref() || !is_last || !is_move || is_non_def;

      if (is_use) {
        // any mreg mentioned is a use
        if (operand.has_base_reg())
          uses.insert(MachineReg(operand.get_base_reg()));
        if (operand.has_index_reg())
          uses.insert(MachineReg(operand.get_index_reg()));
      }
    }

    return std::vector<MachineReg>(uses.begin(), uses.end());
  }

  // Instruction is not a use
  assert(NON_USE_OPCODES.count(ll_opcode) > 0);
  return std::vector<MachineReg>();
}
