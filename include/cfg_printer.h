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

#ifndef CFG_PRINTER_H
#define CFG_PRINTER_H

#include "cfg.h"
#include "print_instruction_seq.h"
#include "highlevel_formatter.h"
#include "lowlevel_formatter.h"

//! @file
//! Support for printing a textual representation of a ControlFlowGraph.

//! Default annotator for printing a control flow graph.
//! Uses DefaultInstructionAnnotator for instruction annotations,
//! and returns empty begin and end annotations for basic blocks.
class DefaultBlockAnnotator : public DefaultInstructionAnnotator {
public:
  //! Get a begin annotation for a basic block.
  //! For example, this could be overridden to return a textual representation
  //! of the dataflow fact at the beginning of the basic block.
  //! @param bb the basic block
  //! @return the annotation for the beginning of the basic block
  virtual std::string get_block_begin_annotation(std::shared_ptr<InstructionSequence> bb) const {
    return "";
  }

  //! Get an end annotation for a basic block.
  //! For example, this could be overridden to return a textual representation
  //! of the dataflow fact at the end of the basic block.
  //! @param bb the basic block
  //! @return the annotation for the end of the basic block
  virtual std::string get_block_end_annotation(std::shared_ptr<InstructionSequence> bb) const {
    return "";
  }
};

//! Print a textual representation of a ControlFlowGraph.
//! @tparam Formatter the Formatter to use for converting an Instruction
//!                   to its textual representation (either HighLevelFormatter
//!                   or LowLevelFormatter)
//! @tparam BlockAnnotator the annotator to use to annotate instructions
//                         and basic blocks
template<typename Formatter, typename BlockAnnotator = DefaultBlockAnnotator>
class ControlFlowGraphPrinter {
private:
  Formatter m_formatter;
  BlockAnnotator m_annotator;
  std::shared_ptr<ControlFlowGraph> m_cfg;

public:
  //! Constructor.
  //! @param cfg shared pointer to the ControlFlowGraph to print
  //! @param formatter the Formatter to use
  //! @param annotator the annotator to use to annotate basic blocks
  ControlFlowGraphPrinter(std::shared_ptr<ControlFlowGraph> cfg,
                          Formatter formatter = Formatter(),
                          BlockAnnotator annotator = BlockAnnotator());
  ~ControlFlowGraphPrinter();

  //! Print the ControlFlowGraph to the standard output.
  void print();
};

// Convenience functions for instantiating a ControlFlowGraphPrinter
// for high-level or low-level code

//! Create a ControlFlowGraphPrinter for printing a high-level ControlFlowGraph.
//!
//! Example usage:
//!
//! ```
//! auto hl_cfg_printer = ::make_highlevel_cfg_printer(hl_cfg);
//! ```
//!
//! @tparam BlockAnnotator the type of block annotator to use to annotate basic blocks
//! @param hl_cfg the high-level ControlFlowGraph to print
//! @param annotator the basic block annotator object to use
//! @return the initialized ControlFlowGraphPrinter instance
template<typename BlockAnnotator = DefaultBlockAnnotator>
ControlFlowGraphPrinter<HighLevelFormatter, BlockAnnotator>
make_highlevel_cfg_printer(std::shared_ptr<ControlFlowGraph> &hl_cfg, BlockAnnotator annotator = BlockAnnotator()) {
  return ControlFlowGraphPrinter<HighLevelFormatter, BlockAnnotator>(hl_cfg, HighLevelFormatter(), annotator);
}

//! Create a ControlFlowGraphPrinter for printing a low-level ControlFlowGraph.
//!
//! Example usage:
//!
//! ```
//! auto ll_cfg_printer = ::make_lowlevel_cfg_printer(ll_cfg);
//! ```
//!
//! @tparam BlockAnnotator the type of block annotator to use to annotate basic blocks
//! @param ll_cfg the low-level ControlFlowGraph to print
//! @param annotator the basic block annotator object to use
//! @return the initialized ControlFlowGraphPrinter instance
template<typename BlockAnnotator = DefaultBlockAnnotator>
ControlFlowGraphPrinter<LowLevelFormatter, BlockAnnotator>
make_lowlevel_cfg_printer(std::shared_ptr<ControlFlowGraph> &ll_cfg, BlockAnnotator annotator = BlockAnnotator()) {
  return ControlFlowGraphPrinter<LowLevelFormatter, BlockAnnotator>(ll_cfg, LowLevelFormatter(), annotator);
}

////////////////////////////////////////////////////////////////////////
// ControlFlowGraphPrinter implementation
////////////////////////////////////////////////////////////////////////

template<typename Formatter, typename BlockAnnotator>
ControlFlowGraphPrinter<Formatter, BlockAnnotator>::ControlFlowGraphPrinter(std::shared_ptr<ControlFlowGraph> cfg,
                                                                       Formatter formatter,
                                                                       BlockAnnotator annotator)
  : m_formatter(formatter)
  , m_annotator(annotator)
  , m_cfg(cfg) {
}

template<typename Formatter, typename BlockAnnotator>
ControlFlowGraphPrinter<Formatter, BlockAnnotator>::~ControlFlowGraphPrinter() {
}

template<typename Formatter, typename BlockAnnotator>
void ControlFlowGraphPrinter<Formatter, BlockAnnotator>::print() {
  for (auto i = m_cfg->bb_begin(); i != m_cfg->bb_end(); i++) {
    std::shared_ptr<InstructionSequence> bb = *i;

    // Print information about the beginning of the basic block
    std::string begin_str;
    begin_str += "BASIC BLOCK ";
    begin_str += std::to_string(bb->get_block_id());
    BasicBlockKind bb_kind = bb->get_kind();
    if (bb_kind != BASICBLOCK_INTERIOR)
      begin_str += (bb_kind == BASICBLOCK_ENTRY ? " [entry]" : " [exit]");
    if (bb->has_block_label()) {
      begin_str += " (label ";
      begin_str += bb->get_block_label();
      begin_str += ")";
    }
    std::string begin_annotation = m_annotator.get_block_begin_annotation(bb);
    if (!begin_annotation.empty()) {
      if (begin_str.size() < 37)
        begin_str += ("                                     " + begin_str.size());
      begin_str += "/* ";
      begin_str += begin_annotation;
      begin_str += " */";
    }
    printf("%s\n", begin_str.c_str());

    // Print the instructions in the basic block
    PrintInstructionSequence<Formatter, BlockAnnotator> print_ins_seq(m_formatter, m_annotator);
    print_ins_seq.print(bb);

    // Print information about the outgoing edges
    const ControlFlowGraph::EdgeList &outgoing_edges = m_cfg->get_outgoing_edges(bb);
    for (auto j = outgoing_edges.cbegin(); j != outgoing_edges.cend(); j++) {
      const Edge *e = *j;
      assert(e->get_kind() == EDGE_BRANCH || e->get_kind() == EDGE_FALLTHROUGH);
      printf("  %s EDGE to BASIC BLOCK %u\n", e->get_kind() == EDGE_FALLTHROUGH ? "fall-through" : "branch", e->get_target()->get_block_id());
    }

    // If there is an end annotation, print it
    std::string end_annotation = m_annotator.get_block_end_annotation(bb);
    if (!end_annotation.empty())
      printf("                    At end of block: /* %s */\n", end_annotation.c_str());

    printf("\n");
  }
}

#endif // CFG_PRINTER_H
