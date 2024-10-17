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

#ifndef LOWLEVEL_CODEGEN_H
#define LOWLEVEL_CODEGEN_H

#include <memory>
#include "options.h"
#include "operand.h"
#include "instruction.h"
#include "instruction_seq.h"
#include "function.h"

//! @file
//! Translation of high-level IR code to Low-level (x86-64) IR code.

//! A LowLevelCodeGen object transforms an InstructionSequence containing
//! high-level instructions into an InstructionSequence containing
//! low-level (x86-64) instructions.
class LowLevelCodeGen {
private:
  const Options &m_options;
  std::shared_ptr<Function> m_function;
  int m_total_memory_storage;

public:
  LowLevelCodeGen(const Options &options);
  virtual ~LowLevelCodeGen();

  //! Generate low-level InstructionSequence based on the high-level InstructionSequence.
  //! This function should
  //!   - use the high-level InstructionSequence in the Function object
  //!   - store the generated low-level InstructionSequence in the Function object
  void generate(std::shared_ptr<Function> function);

private:
  std::shared_ptr<InstructionSequence> translate_hl_to_ll(std::shared_ptr<InstructionSequence> hl_iseq);
  void translate_instruction(Instruction *hl_ins, std::shared_ptr<InstructionSequence> ll_iseq);
  Operand get_ll_operand(Operand hl_opcode, int size, std::shared_ptr<InstructionSequence> ll_iseq);
};

#endif // LOWLEVEL_CODEGEN_H
