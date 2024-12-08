#include "modulo_reduction_opt.h"
#include "instruction.h"
#include "instruction_seq.h"
#include "lowlevel.h"

ModuloReductionOpt::ModuloReductionOpt(std::shared_ptr<ControlFlowGraph> cfg)
  : ControlFlowGraphTransform(cfg) {
}

ModuloReductionOpt::~ModuloReductionOpt() {
}

bool ModuloReductionOpt::is_million_compare(const Instruction *ins) const {
  if (ins->get_opcode() != MINS_CMPL)
    return false;
  
  const Operand &op2 = ins->get_operand(1);
  return op2.is_imm_ival() && op2.get_imm_ival() == 1000000;
}

bool ModuloReductionOpt::is_million_subtract(const Instruction *ins) const {
  if (ins->get_opcode() != MINS_SUBL)
    return false;
  
  const Operand &op2 = ins->get_operand(1);
  return op2.is_imm_ival() && op2.get_imm_ival() == 1000000;
}

std::shared_ptr<InstructionSequence> ModuloReductionOpt::transform_basic_block(std::shared_ptr<InstructionSequence> orig) {
  std::shared_ptr<InstructionSequence> result(new InstructionSequence());
  
  // Iterate through instructions looking for the pattern
  for (auto i = orig->cbegin(); i != orig->cend(); ) {
    const Instruction *ins = *i;
    
    // Look for compare with 1000000
    if (is_million_compare(ins)) {
      auto j = i;
      ++j;  // Move to next instruction
      
      if (j != orig->cend()) {
        const Instruction *jle_ins = *j;
        if (jle_ins->get_opcode() == MINS_JLE) {
          auto k = j;
          ++k;  // Move to next instruction
          
          if (k != orig->cend()) {
            const Instruction *sub_ins = *k;
            if (is_million_subtract(sub_ins)) {
              // Create new XOR instruction
              Instruction *new_ins = new Instruction(MINS_XORL);
              
              // Copy the target operand from the original instruction
              new_ins->set_operand(0, ins->get_operand(0));
              
              // Create immediate value operand for 999999
              new_ins->set_operand(1, Operand(Operand::IMM_IVAL, 999999));
              
              // Add the new instruction to result
              result->append(new_ins);
              
              // Skip the next two instructions
              i = k;
              ++i;
              continue;
            }
          }
        }
      }
    }
    
    // If we didn't match the pattern, copy the original instruction
    result->append(ins->duplicate());
    ++i;
  }
  
  return result;
}