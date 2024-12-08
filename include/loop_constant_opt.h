#ifndef LOOP_CONSTANT_OPT_H
#define LOOP_CONSTANT_OPT_H

#include <memory>
#include "cfg_transform.h"
#include "instruction.h"
#include "instruction_seq.h"
#include "lowlevel.h"

class LoopConstantOpt : public ControlFlowGraphTransform {
public:
  LoopConstantOpt(std::shared_ptr<ControlFlowGraph> cfg);
  virtual ~LoopConstantOpt();

  virtual std::shared_ptr<InstructionSequence> transform_basic_block(std::shared_ptr<InstructionSequence> orig) override;

private:
  bool m_loaded_million;
  int m_million_vreg;
};

#endif