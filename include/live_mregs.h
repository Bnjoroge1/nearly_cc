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

#ifndef LIVE_MREGS_H
#define LIVE_MREGS_H

#include <string>
#include "instruction.h"
#include "lowlevel_defuse.h"
#include "lowlevel_formatter.h"
#include "dataflow.h"

//! @file
//! Dataflow analysis to determine which machine registers contain
//! live values.

//! Dataflow analysis to determine which machine registers contain
//! live values.
class LiveMregsAnalysis : public BackwardAnalysis {
public:
  // There are only 16 mregs
  static const unsigned MAX_MREGS = 16;

  //! Fact type is bitset of machine register numbers.
  typedef std::bitset<MAX_MREGS> FactType;

  //! Constructor.
  //! @param cfg the ControlFlowGraph being analyzed
  LiveMregsAnalysis(std::shared_ptr<ControlFlowGraph> cfg)
    : BackwardAnalysis(cfg)
  { }

  //! The "top" fact is an unknown value that combines nondestructively
  //! with known facts. For this analysis, it's the empty set.
  FactType get_top_fact() const { return FactType(); }

  //! Combine live sets. For this analysis, we use union.
  FactType combine_facts(const FactType &left, const FactType &right) const {
    return left | right;
  }

  //! Model basic block.
  //! This is a no-op for this analysis, since the actual
  //! modeling is done by model_instruction().
  //! @param bb the basic block
  //! @param fact dataflow fact representing what is true at the
  //!             beginning of the block, to be modified as needed
  void model_block(std::shared_ptr<InstructionSequence> bb, FactType &fact) {
  }

  //! Model an instruction.
  //! @param ins the Instruction to model (backwards)
  //! @param fact initially represents what is true after the instruction
  //!             (in program order), and will be updated to represent what
  //!             is true before the instruction (in program order)
  void model_instruction(Instruction *ins, FactType &fact) const {
    // Model an instruction (backwards).  If the instruction is a def,
    // the assigned-to mreg is killed.  Every mreg used in the instruction,
    // the mreg becomes alive (or is kept alive.)

    if (LowLevel::is_def(ins)) {
      std::vector<MachineReg> defs = LowLevel::get_def_mregs(ins);
      for (auto i = defs.begin(); i != defs.end(); ++i)
        fact.reset(unsigned(*i));
    }

    std::vector<MachineReg> uses = LowLevel::get_use_mregs(ins);
    for (auto i = uses.begin(); i != uses.end(); ++i)
      fact.set(unsigned(*i));
  }

  //! Convert a dataflow fact to a string (for printing the CFG annotated with
  //! dataflow facts)
  //! @param fact dataflow fact (set of machine register numbers)
  //! @return string representation of the dataflow fact
  std::string fact_to_string(const FactType &fact) const {
    LowLevelFormatter ll_formatter;

    std::string s("{");
    for (unsigned i = 0; i < MAX_MREGS; i++) {
      if (fact.test(i)) {
        if (s != "{") { s += ","; }
        // Machine registers are shown using their 64-bit name
        Operand mreg_operand(Operand::MREG64, i);
        s += ll_formatter.format_operand(mreg_operand);
      }
    }
    s += "}";
    return s;
  }
};

//! Convenient typedef for the type of a dataflow object for executing
//! LiveMregsAnalysis on a ControlFlowGraph.
typedef Dataflow<LiveMregsAnalysis> LiveMregs;

#endif // LIVE_MREGS_H
