#include "loop_constant_opt.h"
#include "instruction.h"
#include "operand.h"
#include "lowlevel.h" 

LoopConstantOpt::LoopConstantOpt(std::shared_ptr<ControlFlowGraph> cfg)
  : ControlFlowGraphTransform(cfg)
  , m_loaded_million(false)
  , m_million_vreg(-1) {
}

LoopConstantOpt::~LoopConstantOpt() {
}

std::shared_ptr<InstructionSequence> LoopConstantOpt::transform_basic_block(std::shared_ptr<InstructionSequence> orig) {
  std::shared_ptr<InstructionSequence> result(new InstructionSequence());

  // Copy block properties
  result->set_kind(orig->get_kind());
  
  // Copy any block labels
  if (orig->has_block_label()) {
    result->set_block_label(orig->get_block_label());
  }
  
  // Process each instruction
  for (auto i = orig->cbegin(); i != orig->cend(); ++i) {
    Instruction *inst = *i;
    
    // Look for constant loads of 1000000
    if (inst->get_opcode() == MINS_MOVQ && 
        inst->get_num_operands() == 2 &&
        inst->get_operand(1).is_imm_ival() &&
        inst->get_operand(1).get_imm_ival() == 1000000) {
      
      if (!m_loaded_million) {
        // First time seeing 1000000, keep this load but remember the vreg
        m_loaded_million = true;
        m_million_vreg = inst->get_operand(0).get_base_reg();
        result->append(inst->duplicate());
      } else {
        // Reuse the previous load of 1000000
        Instruction *move = new Instruction(MINS_MOVQ,
                                         inst->get_operand(0),
                                         Operand(Operand::MREG64, m_million_vreg));
        move->set_comment("Reused million constant");
        result->append(move);
      }
    } else {
      // Not a million constant load, copy instruction as-is
      result->append(inst->duplicate());
    }
  }

  return result;
}