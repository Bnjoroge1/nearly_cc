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

#ifndef LOWLEVEL_H
#define LOWLEVEL_H

#include <cassert>
#include "operand.h"
#include "instruction.h"

//! @file
//! Machine registers, machine instructions, and low-level code formatting
//! support for x86-64.

//! Enumeration for naming machine registers.
//! Note that these names correspond to the full 64-bit name
//! of each register. However, when used as the register number
//! in an Operand, the Operand kind will select whether the
//! 64-bit (Operand::MREG64), 32-bit (Operand::MREG32),
//! 16-bit (Operand::MREG16), or 8-bit (Operand::MREG8)
//! variant of the register is being accessed.
enum MachineReg {
  MREG_RAX,
  MREG_RBX,
  MREG_RCX,
  MREG_RDX,
  MREG_RSI,
  MREG_RDI,
  MREG_RSP,
  MREG_RBP,
  MREG_R8,
  MREG_R9,
  MREG_R10,
  MREG_R11,
  MREG_R12,
  MREG_R13,
  MREG_R14,
  MREG_R15,

  // This is not an actual machine register,
  // it is just here to have a value 1 greater than the
  // last actual machine register
  MREG_END,
};

//! x86-64 assembly language instruction mnemonics.
//! You may add other instructions here. If you do, you will need
//! to modify the lowlevel_opcode_to_str() function to add
//! support for translating the instruction's enum value to a string.
//! For instructions which allow an operand size suffix, there should
//! be 4 enum values, in the order "b", "w", "l", and "q".
//! (The same way that the high-level instructions with operand size
//! suffixes use this order.)
enum LowLevelOpcode {
  MINS_NOP,
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
  MINS_LEAQ, // only one variant, pointers are always 64-bit
  MINS_JMP,
  MINS_JL,   // these are in the same order as the corresponding SETx instructions
  MINS_JLE,
  MINS_JG,
  MINS_JGE,
  MINS_JE,
  MINS_JNE,
  MINS_JB,
  MINS_JBE,
  MINS_JA,
  MINS_JAE,
  MINS_CMPB,
  MINS_CMPW,
  MINS_CMPL,
  MINS_CMPQ,
  MINS_CALL,
  MINS_IMULL,
  MINS_IMULQ,
  MINS_IDIVL,
  MINS_IDIVQ,
  MINS_CDQ,
  MINS_CQTO,
  MINS_PUSHQ,
  MINS_POPQ,
  MINS_RET,
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
  MINS_SETL,  // these are in the same order as the corresponding Jx instructions
  MINS_SETLE,
  MINS_SETG,
  MINS_SETGE,
  MINS_SETE,
  MINS_SETNE,
  MINS_XORB,
  MINS_XORW,
  MINS_XORL,
  MINS_XORQ,
  MINS_INCB,
  MINS_INCW,
  MINS_INCL,
  MINS_INCQ,
  MINS_DECB,
  MINS_DECW,
  MINS_DECL,
  MINS_DECQ,
};

//! Convert a LowLevelOpcode to a string containing its assembler mnemonic.
//! @param opcode a LowLevelOpcode
//! @return a string containing the opcode's assembler mnemonic
const char *lowlevel_opcode_to_str(LowLevelOpcode opcode);

//! LowLevelInstructionProperties class:
//! this is used by ControlFlowGraphBuilder to make sense of
//! control flow involving low-level instructions.
class LowLevelInstructionProperties {
public:
  //! Determine whether an Instruction is a function call.
  //! @param ins an Instruction
  //! @return true if the instuction is a function call, false otherwise
  bool is_function_call(Instruction *ins) const {
    return ins->get_opcode() == MINS_CALL;
  }

  //! Determine whether it is possible for an Instruction to fall through
  //! to the next sequential instruction in program order.
  //! @param ins an Instruction
  //! @return true if the instuction can fall through to the next sequential
  //!         instruction in program order, false otherwise
  bool falls_through(Instruction *ins) const {
    // only an unconditional jump instruction does not fall through
    return ins->get_opcode() != MINS_JMP;
  }
};

#endif // LOWLEVEL_H
