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

#ifndef DATAFLOW_H
#define DATAFLOW_H

#include <cassert>
#include <algorithm>
#include <memory>
#include <vector>
#include <bitset>
#include "cfg.h"

//! @file
//! Support for global (procedure-scope) dataflow analysis.
//! Both forward and backward analyses are supported.

//! Dataflow analysis direction.
enum class DataflowDirection {
  FORWARD,
  BACKWARD,
};

//! Forward navigation in the control-flow graph (from predecessors
//! towards successors). This is used for forward analyses.
class ForwardNavigation {
public:
  //! Get the "start" block in the ControlFlowGraph,
  //! which for a forward analysis is the entry block.
  //! @param cfg the ControlFlowGraph
  //! @return a shared pointer to the start block
  std::shared_ptr<InstructionSequence> get_start_block(std::shared_ptr<ControlFlowGraph> cfg) const {
    return cfg->get_entry_block();
  }

  //! Get the control edges for a given block which lead to the
  //! "logical successors" of the block. For a forward analysis,
  //! these are the outgoing edges leading to the actual successors.
  //! @param cfg the ControlFlowGraph
  //! @param bb the basic block
  //! @return the edges leading to successors
  const ControlFlowGraph::EdgeList &get_edges(std::shared_ptr<ControlFlowGraph> cfg, std::shared_ptr<InstructionSequence> bb) const {
    return cfg->get_outgoing_edges(bb);
  }

  //! Get the basic block that is a "logical" successor from an Edge
  //! returned from the `get_edges` member function. For forward analysis,
  //! the returned block is the target block.
  //! @param edge an Edge returned from `get_edges()`
  //! @return the Edge's target block
  std::shared_ptr<InstructionSequence> get_block(const Edge *edge) const {
    return edge->get_target();
  }
};

//! Backward navigation in the control-flow graph (from successors
//! back to predecessors). This is used for backward analyses.
class BackwardNavigation {
public:
  //! Get the "start" block in the ControlFlowGraph,
  //! which for a backward analysis is the exit block.
  //! @param cfg the ControlFlowGraph
  //! @return a shared pointer to the exit block
  std::shared_ptr<InstructionSequence> get_start_block(std::shared_ptr<ControlFlowGraph> cfg) const {
    return cfg->get_exit_block();
  }

  //! Get the control edges for a given block which lead to the
  //! "logical successors" of the block. For a backward analysis,
  //! these are the incoming edges leading to the actual predecessors.
  //! @param cfg the ControlFlowGraph
  //! @param bb the basic block
  //! @return the edges coming from predecessors
  const ControlFlowGraph::EdgeList &get_edges(std::shared_ptr<ControlFlowGraph> cfg, std::shared_ptr<InstructionSequence> bb) const {
    return cfg->get_incoming_edges(bb);
  }

  //! Get the basic block that is a "logical" successor from an Edge
  //! returned from the `get_edges` member function. For backward analysis,
  //! the returned block is the source block.
  //! @param edge an Edge returned from `get_edges()`
  //! @return the Edge's source block
  std::shared_ptr<InstructionSequence> get_block(const Edge *edge) const {
    return edge->get_source();
  }
};

//! ForwardAnalysis and BackwardAnalysis inherit from this class,
//! which provides a reference to the ControlFlowGraph being
//! analyzed. This allows any analysis to access the ControlFlowGraph
//! by calling get_cfg().
class HasCFG {
private:
  std::shared_ptr<ControlFlowGraph> m_cfg;

public:
  HasCFG(std::shared_ptr<ControlFlowGraph> cfg)
    : m_cfg(cfg)
  { }

  //! Return a reference to the ControlFlowGraph being analyzed.
  std::shared_ptr<ControlFlowGraph> get_cfg() const { return m_cfg; }
};

//! Base class for forward analyses.
class ForwardAnalysis : public HasCFG {
public:
  ForwardAnalysis(std::shared_ptr<ControlFlowGraph> cfg)
    : HasCFG(cfg)
  { }

  //! Analysis direction is forward.
  const static DataflowDirection DIRECTION = DataflowDirection::FORWARD;

  //! Iterator type for iterating over instructions in a basic block
  //! in "analysis" order.
  typedef InstructionSequence::const_iterator InstructionIterator;

  //! Get a begin iterator over the instructions in a basic block
  //! in "analysis" order.
  //! @param bb a basic block
  //! @return begin iterator over the instructions in the basic block
  InstructionIterator begin(std::shared_ptr<InstructionSequence> bb) const { return bb->cbegin(); }

  //! Get an end iterator over the instructions in a basic block
  //! in "analysis" order.
  //! @param bb a basic block
  //! @return end iterator over the instructions in the basic block
  InstructionIterator end(std::shared_ptr<InstructionSequence> bb) const { return bb->cend(); }

  // Logical navigation forward and backward (program order)

  //! Logical forward navigation through the ControlFlowGraph
  //! in analysis order.
  ForwardNavigation LOGICAL_FORWARD;

  //! Logical backward navigation through the ControlFlowGraph
  //! in analysis order.
  BackwardNavigation LOGICAL_BACKWARD;
};

//! Base class for backward analyses.
class BackwardAnalysis : public HasCFG {
public:
  BackwardAnalysis(std::shared_ptr<ControlFlowGraph> cfg)
    : HasCFG(cfg)
  { }

  //! Analysis direction is backward.
  const static DataflowDirection DIRECTION = DataflowDirection::BACKWARD;

  //! Iterator type for iterating over instructions in a basic block
  //! in "analysis" order.
  typedef InstructionSequence::const_reverse_iterator InstructionIterator;

  //! Get a begin iterator over the instructions in a basic block
  //! in "analysis" order.
  //! @param bb a basic block
  //! @return begin iterator over the instructions in the basic block
  InstructionIterator begin(std::shared_ptr<InstructionSequence> bb) const { return bb->crbegin(); }

  //! Get an end iterator over the instructions in a basic block
  //! in "analysis" order.
  //! @param bb a basic block
  //! @return end iterator over the instructions in the basic block
  InstructionIterator end(std::shared_ptr<InstructionSequence> bb) const { return bb->crend(); }

  // Logical navigation forward and backward (reverse of program order)

  //! Logical forward navigation through the ControlFlowGraph
  //! in analysis order.
  BackwardNavigation LOGICAL_FORWARD;

  //! Logical backward navigation through the ControlFlowGraph
  //! in analysis order.
  ForwardNavigation LOGICAL_BACKWARD;
};

//! Annotator to return a stringified dataflow fact for a
//! specific instruction in a basic block. This can be used
//! with ControlFlowGraphPrinter to annotate the printed
//! control-flow graph with dataflow facts.
//! @tparam the type of the Dataflow object containing the analysis results
template<typename DataflowType>
class DataflowAnnotator {
public:
  DataflowType &m_dataflow;

  //! Constructor.
  //! @param reference to the Dataflow object containing the analysis results
  DataflowAnnotator(DataflowType &dataflow)
    : m_dataflow(dataflow) {
  }

  std::string get_instruction_annotation(std::shared_ptr<InstructionSequence> bb, Instruction *ins) const {
    // get dataflow fact just before the instruction
    typename DataflowType::FactType fact = m_dataflow.get_fact_before_instruction(bb, ins);
    std::string s = m_dataflow.fact_to_string(fact);
    //printf("annotation: %s\n", s.c_str());
    return s;
  }

  //! Get a begin annotation for a basic block.
  //! This will return a textual representation of the dataflow fact
  //! at the beginning of the basic block.
  //!
  //! @param bb the basic block
  //! @return stringified dataflow fact at the beginning of the basic block
  virtual std::string get_block_begin_annotation(std::shared_ptr<InstructionSequence> bb) const {
    typename DataflowType::FactType fact = m_dataflow.get_fact_at_beginning_of_block(bb);
    return m_dataflow.fact_to_string(fact);
  }

  //! Get an end annotation for a basic block.
  //! This will return a textual representation of the dataflow fact
  //! at the end of the basic block.
  //!
  //! @param bb the basic block
  //! @return stringified dataflow fact at the end of the basic block
  virtual std::string get_block_end_annotation(std::shared_ptr<InstructionSequence> bb) const {
    typename DataflowType::FactType fact = m_dataflow.get_fact_at_end_of_block(bb);
    return m_dataflow.fact_to_string(fact);
  }
};

//! An instance of Dataflow performs a dataflow analysis on the basic blocks
//! of a control flow graph and provides an interface for querying
//! dataflow facts at arbitrary points.  The Analysis object (instance of the
//! Analysis type parameter) provides the concrete details about how the
//! analysis is performed:
//!
//!   - the fact type
//!   - how dataflow facts are combined
//!   - how instructions are modeled
//!
//! etc.
//!
//! @tparam Analysis the analysis class, derived from either ForwardAnalysis
//!                  or BackwardAnalysis
template<typename Analysis>
class Dataflow {
public:
  //! Data type representing a dataflow fact.
  //! Inferred from the Analysis type.
  typedef typename Analysis::FactType FactType;

  //! We assume there won't be more than this many basic blocks.
  static const unsigned MAX_BLOCKS = 1024;

private:
  // The Analysis object encapsulates all of the details about the
  // analysis to be performed: direction (forward or backward),
  // the fact type, how to model an instruction, how to combine
  // dataflow facts, etc.
  Analysis m_analysis;

  // The control flow graph to analyze
  std::shared_ptr<ControlFlowGraph> m_cfg;

  // facts at end and beginning of each basic block
  std::vector<FactType> m_endfacts, m_beginfacts;

  // block iteration order
  std::vector<unsigned> m_iter_order;

public:
  //! Constructor.
  //! @param the ControlFlowGraph to analyze
  Dataflow(std::shared_ptr<ControlFlowGraph> cfg);
  ~Dataflow();

  //! Execute the analysis.
  void execute();

  //! Get dataflow fact at end of specified block.
  //! @param bb the basic block
  //! @return the dataflow fact at the end of the basic block
  const FactType &get_fact_at_end_of_block(std::shared_ptr<InstructionSequence> bb) const;

  //! Get dataflow fact at beginning of specific block.
  //! @param bb the basic block
  //! @return the dataflow fact at the beginning of the basic block
  const FactType &get_fact_at_beginning_of_block(std::shared_ptr<InstructionSequence> bb) const;

  //! Get dataflow fact at the location immediately after the specified instruction
  //! (in program order).
  //! @param bb the basic block
  //! @param ins pointer to an Instruction in the basic block
  //! @return the dataflow fact at the location immediately after the specified Instruction
  FactType get_fact_after_instruction(std::shared_ptr<InstructionSequence> bb, Instruction *ins) const;

  //! Get dataflow fact at the location immediately before the specified instruction
  //! (in program order).
  //! @param bb the basic block
  //! @param ins pointer to an Instruction in the basic block
  //! @return the dataflow fact at the location immediately before the specified Instruction
  FactType get_fact_before_instruction(std::shared_ptr<InstructionSequence> bb, Instruction *ins) const;

  //! Convert dataflow fact to a string.
  //! @param fact a dataflow fact
  //! @return a stringified representation of the dataflow fact
  std::string fact_to_string(const FactType &fact);

private:
  // Helpers to get the vector containing the facts known at "logical"
  // beginning and end of each basic block.  In this context, "beginning" means
  // where analysis of the block should start (beginning for forward analyses,
  // end for backward analyses), "end" means where analysis ends.

  std::vector<typename Analysis::FactType> &get_logical_begin_facts() {
    return Analysis::DIRECTION == DataflowDirection::FORWARD
      ? m_beginfacts
      : m_endfacts;
  }

  std::vector<typename Analysis::FactType> &get_logical_end_facts() {
    return Analysis::DIRECTION == DataflowDirection::FORWARD
      ? m_endfacts
      : m_beginfacts;
  }

  const std::vector<typename Analysis::FactType> &get_logical_begin_facts() const {
    return Analysis::DIRECTION == DataflowDirection::FORWARD
      ? m_beginfacts
      : m_endfacts;
  }

  const std::vector<typename Analysis::FactType> &get_logical_end_facts() const {
    return Analysis::DIRECTION == DataflowDirection::FORWARD
      ? m_endfacts
      : m_beginfacts;
  }

  // Helper for get_fact_after_instruction() and get_fact_before_instruction()
  FactType get_instruction_fact(std::shared_ptr<InstructionSequence> bb, Instruction *ins, bool after_in_logical_order) const;

  // Compute the iteration order
  void compute_iter_order();

  // Postorder traversal on the CFG (or reversed CFG, depending on
  // analysis direction)
  void postorder_on_cfg(std::bitset<MAX_BLOCKS> &visited, std::shared_ptr<InstructionSequence> bb);
};

template<typename Analysis>
Dataflow<Analysis>::Dataflow(std::shared_ptr<ControlFlowGraph> cfg)
  : m_analysis(cfg)
  , m_cfg(cfg) {
  for (unsigned i = 0; i < cfg->get_num_blocks(); ++i) {
    m_beginfacts.push_back(m_analysis.get_top_fact());
    m_endfacts.push_back(m_analysis.get_top_fact());
  }
}

template<typename Analysis>
Dataflow<Analysis>::~Dataflow() {
}

template<typename Analysis>
void Dataflow<Analysis>::execute() {
  compute_iter_order();

  std::vector<FactType> &logical_begin_facts = get_logical_begin_facts(),
                        &logical_end_facts = get_logical_end_facts();

  const auto &to_logical_predecessors = m_analysis.LOGICAL_BACKWARD;

  bool done = false;

  unsigned num_iters = 0;
  while (!done) {
    num_iters++;
    bool change = false;

    // For each block (according to the iteration order)...
    for (auto i = m_iter_order.begin(); i != m_iter_order.end(); i++) {
      unsigned id = *i;
      std::shared_ptr<InstructionSequence> bb = m_cfg->get_block(id);

      // Combine facts known from control edges from the "logical" predecessors
      // (which are the successors for backward analyses)
      FactType fact = m_analysis.get_top_fact();
      const ControlFlowGraph::EdgeList &logical_predecessor_edges = to_logical_predecessors.get_edges(m_cfg, bb);
      for (auto j = logical_predecessor_edges.cbegin(); j != logical_predecessor_edges.cend(); j++) {
        const Edge *e = *j;
        std::shared_ptr<InstructionSequence> logical_predecessor = to_logical_predecessors.get_block(e);
        fact = m_analysis.combine_facts(fact, logical_end_facts[logical_predecessor->get_block_id()]);
      }

      // Update (currently-known) fact at the "beginning" of this basic block
      // (which will actually be the end of the basic block for backward analyses)
      logical_begin_facts[id] = fact;

      // Call model_block: this is useful for analyses which don't
      // model individual instructions
      m_analysis.model_block(bb, fact);

      // Model each instruction in the basic block
      for (auto j = m_analysis.begin(bb); j != m_analysis.end(bb); ++j) {
        Instruction *ins = *j;

        // model the instruction
        m_analysis.model_instruction(ins, fact);
      }

      // Did the fact at the logical "end" of the block change?
      // If so, at least one more round will be needed.
      if (fact != logical_end_facts[id]) {
        change = true;
        logical_end_facts[id] = fact;
      }
    }

    // If there was no change in the computed dataflow fact at the beginning
    // of any block, then the analysis has terminated
    if (!change) {
      done = true;
    }
  }
}

template<typename Analysis>
const typename Dataflow<Analysis>::FactType &Dataflow<Analysis>::get_fact_at_end_of_block(std::shared_ptr<InstructionSequence> bb) const {
  return m_endfacts.at(bb->get_block_id());
}

template<typename Analysis>
const typename Dataflow<Analysis>::FactType &Dataflow<Analysis>::get_fact_at_beginning_of_block(std::shared_ptr<InstructionSequence> bb) const {
  return m_beginfacts.at(bb->get_block_id());
}

template<typename Analysis>
typename Dataflow<Analysis>::FactType Dataflow<Analysis>::get_fact_after_instruction(std::shared_ptr<InstructionSequence> bb, Instruction *ins) const {
  // For a backward analysis, we want the fact that is true "before" (logically)
  // the specified instruction.
  bool after_in_logical_order = (Analysis::DIRECTION == DataflowDirection::FORWARD);
  return get_instruction_fact(bb, ins, after_in_logical_order);
}

template<typename Analysis>
typename Dataflow<Analysis>::FactType Dataflow<Analysis>::get_fact_before_instruction(std::shared_ptr<InstructionSequence> bb, Instruction *ins) const {
  // For a backward analysis, we want the fact that is true "after" (logically)
  // the specified instruction.
  bool after_in_logical_order = (Analysis::DIRECTION == DataflowDirection::BACKWARD);
  return get_instruction_fact(bb, ins, after_in_logical_order);
}

template<typename Analysis>
std::string Dataflow<Analysis>::fact_to_string(const FactType &fact) {
  return m_analysis.fact_to_string(fact);
}

// Compute the dataflow fact immediately before or after the specified instruction,
// in the "logical" order (corresponding to the analysis direction) rather than
// program order.
template<typename Analysis>
typename Analysis::FactType Dataflow<Analysis>::get_instruction_fact(std::shared_ptr<InstructionSequence> bb,
                                                                     Instruction *ins,
                                                                     bool after_in_logical_order) const {
  const std::vector<FactType> &logical_begin_facts = get_logical_begin_facts();

  FactType fact = logical_begin_facts[bb->get_block_id()];

  for (auto i = m_analysis.begin(bb); i != m_analysis.end(bb); ++i) {
    Instruction *bb_ins = *i;
    bool at_instruction = (bb_ins == ins);

    if (at_instruction && !after_in_logical_order) break;

    m_analysis.model_instruction(bb_ins, fact);

    if (at_instruction) {
      assert(after_in_logical_order);
      break;
    }
  }

  return fact;
}

template<typename Analysis>
void Dataflow<Analysis>::compute_iter_order() {
  std::bitset<MAX_BLOCKS> visited;

  const auto &to_logical_successors = m_analysis.LOGICAL_FORWARD;

  std::shared_ptr<InstructionSequence> logical_entry_block = to_logical_successors.get_start_block(m_cfg);

  postorder_on_cfg(visited, logical_entry_block);

  std::reverse(m_iter_order.begin(), m_iter_order.end());
}

template<typename Analysis>
void Dataflow<Analysis>::postorder_on_cfg(std::bitset<MAX_BLOCKS> &visited, std::shared_ptr<InstructionSequence> bb) {
  const auto &to_logical_successors = m_analysis.LOGICAL_FORWARD;

  // already arrived at this block?
  if (visited.test(bb->get_block_id())) {
    return;
  }

  // this block is now guaranteed to be visited
  visited.set(bb->get_block_id());

  // recursively visit (logical) successors
  const ControlFlowGraph::EdgeList &logical_successor_edges = to_logical_successors.get_edges(m_cfg, bb);
  for (auto i = logical_successor_edges.begin(); i != logical_successor_edges.end(); i++) {
    const Edge *e = *i;
    std::shared_ptr<InstructionSequence> next_bb = to_logical_successors.get_block(e);
    postorder_on_cfg(visited, next_bb);
  }

  // add this block to the order
  m_iter_order.push_back(bb->get_block_id());
}

#endif // DATAFLOW_H
