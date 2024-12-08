#ifndef MODULO_REDUCTION_OPT_H
#define MODULO_REDUCTION_OPT_H

#include "cfg_transform.h"

class ModuloReductionOpt : public ControlFlowGraphTransform {
public:
  ModuloReductionOpt(std::shared_ptr<ControlFlowGraph> cfg);
  virtual ~ModuloReductionOpt();

protected:
  virtual std::shared_ptr<InstructionSequence> transform_basic_block(std::shared_ptr<InstructionSequence> orig);

private:
  bool is_million_compare(const Instruction *ins) const;
  bool is_million_subtract(const Instruction *ins) const;
};

#endif