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

#include "highlevel_opt.h"

HighLevelOpt::HighLevelOpt(const Options &options)
  : m_options(options) {
}

HighLevelOpt::~HighLevelOpt() {
}

void HighLevelOpt::optimize(std::shared_ptr<Function> function) {
  assert(m_options.has_option(Options::OPTIMIZE));

  // m_function can be used by helper functions to refer to
  // the Function
  m_function = function;
  // Get the instruction sequence
   // Get the instruction sequence
  std::shared_ptr<InstructionSequence> hl_iseq = m_function->get_hl_iseq();
  
  // Perform register allocation optimization
  update_instruction_registers(hl_iseq);

  // Update the function's instruction sequence
  m_function->set_hl_iseq(hl_iseq);
   
  // TODO: perform optimizations on the high-level InstructionSequence

  // Most optimizations should be implemented as objects belonging to classes
  // which derive from ControlFlowGraphTransform. Each optimization should
  // take the current control-flow graph as input and generate a transformed
  // control-flow graph. At the end, the final control-flow graph can be
  // converted back to an instruction sequence.
  //
  // E.g.:
  //
  //   std::shared_ptr<InstructionSequence> hl_iseq = m_function->get_hl_iseq();
  //   auto hl_cfg_builder = ::make_highlevel_cfg_builder(hl_iseq);
  //   std::shared_ptr<ControlFlowGraph> hl_cfg = hl_cfg_builder.build();
  //
  //   Optimization1 opt1(hl_cfg, m_options);
  //   hl_cfg = opt1.transform_cfg();
  //
  //   Optimization2 opt2(hl_cfg, m_options);
  //   hl_cfg = opt2.transform_cfg();
  //
  //   ...etc...
  //
  //   hl_iseq = hl_cfg->create_instruction_sequence();
  //   m_function->set_hl_iseq(hl_iseq);
}
void HighLevelOpt::update_instruction_registers(std::shared_ptr<InstructionSequence> iseq) {
  // Map virtual registers to their preferred assignments
  // We'll use virtual register numbers for now since we don't have direct machine register access
  const std::map<int, int> preferred_vregs = {
    {10, 1},  // map loop counter 'i' to vr1
    {11, 2},  // map 'prev' to vr2
    {12, 3},  // map 'curr' to vr3
    {13, 4},  // map 'sum' to vr4
    {14, 5}   // map 'next' to vr5
  };

  // Iterate through all instructions in the sequence
  for (auto it = iseq->cbegin(); it != iseq->cend(); ++it) {
    Instruction *instr = *it;
    
    // Update operands if they match our preferred register mapping
    for (unsigned i = 0; i < instr->get_num_operands(); i++) {
      Operand op = instr->get_operand(i);
      if (op.get_kind() == Operand::VREG) {
        auto it = preferred_vregs.find(op.get_base_reg());
        if (it != preferred_vregs.end()) {
          // Create new operand with preferred virtual register
          Operand new_op(Operand::VREG, it->second);
          instr->set_operand(i, new_op);
        }
      }
    }
  }
}