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

#ifndef CFG_BUILDER_H
#define CFG_BUILDER_H

#include "cfg.h"
#include "highlevel.h"
#include "lowlevel.h"

//! @file
//! Support for building a ControlFlowGraph from an InstructionSequence.

////////////////////////////////////////////////////////////////////////
// ControlFlowGraph builder class
////////////////////////////////////////////////////////////////////////

//! ControlFlowGraphBuilder builds a ControlFlowGraph from an InstructionSequence.
//! It is templated by InstructionProperties, which should be either
//! HighLevelInstructionProperties or LowLevelInstructionProperties,
//! depending on whether you're building a high-level or low-level
//! control-flow graph.
//! @tparam InstructionProperties either HighLevelInstructionProperties or
//!         LowLevelInstructionProperties, depending on whether the
//!         InstructionSequence has high-level or low-level instructions
template<typename InstructionProperties>
class ControlFlowGraphBuilder {
private:
  InstructionProperties m_ins_props;
  std::shared_ptr<InstructionSequence> m_iseq;
  std::shared_ptr<ControlFlowGraph> m_cfg;
  // map of instruction index to pointer to InstructionSequence starting at that instruction
  std::map<unsigned, std::shared_ptr<InstructionSequence> > m_basic_blocks;

  struct WorkItem {
    unsigned ins_index;
    std::shared_ptr<InstructionSequence> pred;
    EdgeKind edge_kind;
    std::string label;
  };

public:
  //! Constuctor.
  //! @param iseq the InstructionSequence containing the instructions to build
  //!        a ControlFlowGraph from
  ControlFlowGraphBuilder(std::shared_ptr<InstructionSequence> iseq);
  ~ControlFlowGraphBuilder();

  //! Build a ControlFlowGraph from the original InstructionSequence.
  //! @return a shared pointer to the ControlFlowGraph of the original InstructionSequence
  std::shared_ptr<ControlFlowGraph> build();

  //! Subclasses may override this method to specify which Instructions are
  //! branches (i.e., have a control successor other than the next instruction
  //! in the original InstructionSequence).  The default implementation assumes
  //! that any Instruction with an operand of type Operand::LABEL
  //! is a branch.
  //! @param ins the Instruction to check
  //! @return true if the instruction is a branch, false otherwise
  virtual bool is_branch(Instruction *ins);

private:
  std::shared_ptr<InstructionSequence> scan_basic_block(const WorkItem &item, const std::string &label);
  bool ends_in_branch(std::shared_ptr<InstructionSequence> bb);
  unsigned get_branch_target_index(std::shared_ptr<InstructionSequence> bb);
  bool falls_through(std::shared_ptr<InstructionSequence> bb);
};

// Convenience functions for creating high-level and low-level
// ControlFlowGraphBuilders. Example usage:
//
//   auto hl_cfg_builder = ::make_highlevel_cfg_builder(hl_iseq);
//
//   auto ll_cfg_builder = ::make_lowlevel_cfg_builder(ll_iseq);

//! Return a ControlFlowGraphBuilder for building a high-level
//! ControlFlowGraph.
//!
//! Example usage:
//! ```
//! auto hl_cfg_builder = ::make_highlevel_cfg_builder(hl_iseq);
//! ```
//!
//! @param the InstructionSequence containing the high-level instructions
//! @return a shared pointer to the resulting ControlFlowGraph
inline ControlFlowGraphBuilder<HighLevelInstructionProperties>
make_highlevel_cfg_builder(std::shared_ptr<InstructionSequence> iseq) {
  return ControlFlowGraphBuilder<HighLevelInstructionProperties>(iseq);
}

//! Return a ControlFlowGraphBuilder for building a low-level
//! ControlFlowGraph.
//!
//! Example usage:
//! ```
//! auto ll_cfg_builder = ::make_lowlevel_cfg_builder(ll_iseq);
//! ```
//!
//! @param the InstructionSequence containing the low-level instructions
//! @return a shared pointer to the resulting ControlFlowGraph
inline ControlFlowGraphBuilder<LowLevelInstructionProperties>
make_lowlevel_cfg_builder(std::shared_ptr<InstructionSequence> iseq) {
  return ControlFlowGraphBuilder<LowLevelInstructionProperties>(iseq);
}

////////////////////////////////////////////////////////////////////////
// ControlFlowGraphBuilder implementation
////////////////////////////////////////////////////////////////////////

template<typename InstructionProperties>
ControlFlowGraphBuilder<InstructionProperties>::ControlFlowGraphBuilder(std::shared_ptr<InstructionSequence> iseq)
  : m_ins_props(InstructionProperties())
  , m_iseq(iseq)
  , m_cfg(new ControlFlowGraph()) {
}

template<typename InstructionProperties>
ControlFlowGraphBuilder<InstructionProperties>::~ControlFlowGraphBuilder() {
}

template<typename InstructionProperties>
std::shared_ptr<ControlFlowGraph> ControlFlowGraphBuilder<InstructionProperties>::build() {
  unsigned num_instructions = m_iseq->get_length();

  std::shared_ptr<InstructionSequence> entry = m_cfg->create_basic_block(BASICBLOCK_ENTRY, -1);
  std::shared_ptr<InstructionSequence> exit = m_cfg->create_basic_block(BASICBLOCK_EXIT, 2000000);

  // exit block is reached by any branch that targets the end of the
  // InstructionSequence
  m_basic_blocks[num_instructions] = exit;

  std::deque<WorkItem> work_list;
  work_list.push_back({ ins_index: 0, pred: entry, edge_kind: EDGE_FALLTHROUGH });

  std::shared_ptr<InstructionSequence> last = nullptr;
  while (!work_list.empty()) {
    WorkItem item = work_list.front();
    work_list.pop_front();

    assert(item.ins_index <= m_iseq->get_length());

    // if item target is end of InstructionSequence, then it targets the exit block
    if (item.ins_index == m_iseq->get_length()) {
      m_cfg->create_edge(item.pred, exit, item.edge_kind);
      continue;
    }

    std::shared_ptr<InstructionSequence> bb;
    bool is_new_block;
    std::map<unsigned, std::shared_ptr<InstructionSequence> >::iterator i = m_basic_blocks.find(item.ins_index);
    if (i != m_basic_blocks.end()) {
      // a block starting at this instruction already exists
      bb = i->second;
      is_new_block = false;

      // Special case: if this block was originally discovered via a fall-through
      // edge, but is also reachable via a branch, then it might not be labeled
      // yet.  Set the label if necessary.
      if (item.edge_kind == EDGE_BRANCH && !bb->has_block_label()) {
        bb->set_block_label(item.label);
      }
    } else {
      // no block starting at this instruction currently exists:
      // scan the basic block and add it to the map of known basic blocks
      // (indexed by instruction index)
      bb = scan_basic_block(item, item.label);
      is_new_block = true;
      m_basic_blocks[item.ins_index] = bb;
    }

    // if the edge is a branch, make sure the work item's label matches
    // the InstructionSequence's label (if it doesn't, then somehow this block
    // is reachable via two different labels, which shouldn't be possible)
    assert(item.edge_kind != EDGE_BRANCH || bb->get_block_label() == item.label);

    // connect to predecessor
    m_cfg->create_edge(item.pred, bb, item.edge_kind);

    if (!is_new_block) {
      // we've seen this block already
      continue;
    }

    // if this basic block ends in a branch, prepare to create an edge
    // to the InstructionSequence for the target (creating the InstructionSequence if it
    // doesn't exist yet)
    if (ends_in_branch(bb)) {
      unsigned target_index = get_branch_target_index(bb);
      // Note: we assume that branch instructions have the target label
      // as the last operand
      Instruction *branch = bb->get_last_instruction();
      unsigned num_operands = branch->get_num_operands();
      assert(num_operands > 0);
      Operand operand = branch->get_operand(num_operands - 1);
      assert(operand.get_kind() == Operand::LABEL);
      std::string target_label = operand.get_label();
      work_list.push_back({ ins_index: target_index, pred: bb, edge_kind: EDGE_BRANCH, label: target_label });
    }

    // if this basic block falls through, prepare to create an edge
    // to the InstructionSequence for successor instruction (creating it if it doesn't
    // exist yet)
    if (falls_through(bb)) {
      unsigned target_index = item.ins_index + bb->get_length();
      assert(target_index <= m_iseq->get_length());
      if (target_index == num_instructions) {
        // this is the basic block at the end of the instruction sequence,
        // its fall-through successor should be the exit block
        last = bb;
      } else {
        // fall through to basic block starting at successor instruction
        work_list.push_back({ ins_index: target_index, pred: bb, edge_kind: EDGE_FALLTHROUGH });
      }
    }
  }

  assert(last != nullptr);
  m_cfg->create_edge(last, exit, EDGE_FALLTHROUGH);

  return m_cfg;
}

template<typename InstructionProperties>
bool ControlFlowGraphBuilder<InstructionProperties>::is_branch(Instruction *ins) {
  // assume that if an instruction's last operand is a label,
  // it's a branch instruction
  unsigned num_operands = ins->get_num_operands();
  return num_operands > 0 && ins->get_operand(num_operands - 1).get_kind() == Operand::LABEL;
}

template<typename InstructionProperties>
std::shared_ptr<InstructionSequence> ControlFlowGraphBuilder<InstructionProperties>::scan_basic_block(const WorkItem &item, const std::string &label) {
  unsigned index = item.ins_index;

  std::shared_ptr<InstructionSequence> bb = m_cfg->create_basic_block(BASICBLOCK_INTERIOR, int(index), label);

  // keep adding instructions until we
  // - reach an instruction that is a branch
  // - reach an instruction that is a target of a branch
  // - reach the end of the overall instruction sequence
  while (index < m_iseq->get_length()) {
    Instruction *ins = m_iseq->get_instruction(index);

    // this instruction is part of the basic block
    ins = ins->duplicate();
    bb->append(ins);
    index++;

    if (index >= m_iseq->get_length()) {
      // At end of overall instruction sequence
      break;
    }

    if (m_ins_props.is_function_call(ins)) {
      // The instruction we just added is a function call
      break;
    }

    if (is_branch(ins)) {
      // The instruction we just added is a branch instruction
      break;
    }

    if (m_iseq->has_label(index)) {
      // Next instruction has a label, so assume it is a branch target
      // (and thus the beginning of a different basic block)
      break;
    }
  }

  assert(bb->get_length() > 0);

  return bb;
}

template<typename InstructionProperties>
bool ControlFlowGraphBuilder<InstructionProperties>::ends_in_branch(std::shared_ptr<InstructionSequence> bb) {
  return !m_ins_props.is_function_call(bb->get_last_instruction()) && is_branch(bb->get_last_instruction());
}

template<typename InstructionProperties>
unsigned ControlFlowGraphBuilder<InstructionProperties>::get_branch_target_index(std::shared_ptr<InstructionSequence> bb) {
  assert(ends_in_branch(bb));
  Instruction *last = bb->get_last_instruction();

  // assume that the label is the last operand
  unsigned num_operands = last->get_num_operands();
  assert(num_operands > 0);
  Operand label = last->get_operand(num_operands - 1);
  assert(label.get_kind() == Operand::LABEL);

  // look up the index of the instruction targeted by this label
  unsigned target_index = m_iseq->get_index_of_labeled_instruction(label.get_label());
  return target_index;
}

template<typename InstructionProperties>
bool ControlFlowGraphBuilder<InstructionProperties>::falls_through(std::shared_ptr<InstructionSequence> bb) {
  return m_ins_props.falls_through(bb->get_last_instruction());
}


#endif // CFG_BUILDER_H
