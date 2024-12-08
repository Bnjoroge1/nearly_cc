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

#include "cfg_builder.h"
#include "peephole_ll.h"
#include "lowlevel_opt.h"

LowLevelOpt::LowLevelOpt(const Options &options)
  : m_options(options) {
}

LowLevelOpt::~LowLevelOpt() {
}

void LowLevelOpt::optimize(std::shared_ptr<Function> function) {
  assert(m_options.has_option(Options::OPTIMIZE));
  // Get the low-level instruction sequence
  std::shared_ptr<InstructionSequence> ll_iseq = m_function->get_ll_iseq();
  
  // Apply optimizations
  eliminate_redundant_moves(ll_iseq);
  allocate_registers(ll_iseq);

  // Update the function's instruction sequence
  m_function->set_ll_iseq(ll_iseq);
  // Like HighLevelOpt, LowLevelOpt will be most easily implemented
  // using optimizations deriving from ControlFlowGraphTransform.
  // E.g.:
  //
  //   std::shared_ptr<InstructionSequence> ll_iseq = function.get_ll_iseq();
  //   auto ll_cfg_builder = ::make_lowlevel_cfg_builder(ll_iseq);
  //   std::shared_ptr<ControlFlowGraph> ll_cfg = ll_cfg_builder.build();
  //
  //   Optimization1 opt1(ll_cfg, m_options);
  //   ll_cfg = opt1.transform_cfg();
  //
  //   Optimization2 opt2(ll_cfg, m_options);
  //   ll_cfg = opt2.transform_cfg();
  //
  //   ll_iseq = ll_cfg.create_instruction_sequence();
  //   function->set_ll_iseq(ll_iseq);
}

void LowLevelOpt::eliminate_redundant_moves(std::shared_ptr<InstructionSequence> iseq) {
  // Create a new instruction sequence
  InstructionSequence *new_iseq = new InstructionSequence();
  
  // Copy all instructions, replacing redundant moves with NOPs
  for (auto it = iseq->cbegin(); it != iseq->cend(); ++it) {
    Instruction *instr = *it;
    
    // Copy any labels that exist at this position
    if (it.has_label()) {
      new_iseq->define_label(it.get_label());
    }
    
    if (instr->get_num_operands() == 2 &&
        (instr->get_opcode() == MINS_MOVQ || 
         instr->get_opcode() == MINS_MOVL)) {
      
      Operand src = instr->get_operand(1);
      Operand dst = instr->get_operand(0);
      
      // If moving between same locations, add a NOP
      if (src.get_kind() == dst.get_kind() && 
          src.get_base_reg() == dst.get_base_reg() &&
          src.get_offset() == dst.get_offset()) {
        // Create NOP instruction
        Instruction *nop = new Instruction(MINS_NOP);
        nop->set_comment("Eliminated redundant move");
        new_iseq->append(nop);
      } else {
        // Not redundant, copy the original instruction
        new_iseq->append(instr->duplicate());
      }
    } else {
      // Not a move instruction, copy as-is
      new_iseq->append(instr->duplicate());
    }
  }
  
  // Update the function's instruction sequence
  m_function->set_ll_iseq(std::shared_ptr<InstructionSequence>(new_iseq));
}

void LowLevelOpt::allocate_registers(std::shared_ptr<InstructionSequence> iseq) {
  // Map of frequently used memory locations to preferred registers
  const std::map<int, int> preferred_regs = {
    {-144, MREG_RDI},  // First function argument
    {-136, MREG_RSI},  // Second function argument
    {-128, MREG_RDX},  // Third argument/counter
    {-120, MREG_RCX},  // Fourth argument/loop variable
    {-112, MREG_R8}    // Fifth argument/temporary
  };

  // Create a new instruction sequence
  InstructionSequence *new_iseq = new InstructionSequence();
  
  // Process each instruction
  for (auto it = iseq->cbegin(); it != iseq->cend(); ++it) {
    Instruction *instr = *it;
    
    // Copy any labels
    if (it.has_label()) {
      new_iseq->define_label(it.get_label());
    }
    
    // Get all operands first and modify them if needed
    std::vector<Operand> new_operands;
    for (unsigned i = 0; i < instr->get_num_operands(); i++) {
      Operand op = instr->get_operand(i);
      
      if (op.get_kind() == Operand::MREG64 && op.has_offset()) {
        auto it = preferred_regs.find(op.get_offset());
        if (it != preferred_regs.end()) {
          // Replace memory access with register
          new_operands.push_back(Operand(Operand::MREG64, it->second));
        } else {
          new_operands.push_back(op);
        }
      } else {
        new_operands.push_back(op);
      }
    }
    
    // Create new instruction with all operands
    Instruction *new_instr;
    switch (new_operands.size()) {
      case 0:
        new_instr = new Instruction(instr->get_opcode());
        break;
      case 1:
        new_instr = new Instruction(instr->get_opcode(), new_operands[0]);
        break;
      case 2:
        new_instr = new Instruction(instr->get_opcode(), new_operands[0], new_operands[1]);
        break;
      case 3:
        new_instr = new Instruction(instr->get_opcode(), new_operands[0], new_operands[1], new_operands[2]);
        break;
      default:
        // Should not happen with x86-64 instructions
        assert(false && "Too many operands");
    }
    
    // Copy comment if exists
    if (instr->has_comment()) {
      new_instr->set_comment(instr->get_comment());
    }
    
    new_iseq->append(new_instr);
  }
  
  // Replace the original sequence with the new one
  iseq = std::shared_ptr<InstructionSequence>(new_iseq);
}
