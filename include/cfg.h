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

#ifndef CFG_H
#define CFG_H

#include <cassert>
#include <vector>
#include <map>
#include <deque>
#include <string>
#include <memory>
#include "instruction.h"
#include "instruction_seq.h"
#include "operand.h"

//! @file
//! ControlFlowGraph and associated types.
//! A control-flow graph is a graph of basic blocks.
//! Each basic block is an InstructionSequence containing
//! the instructions of that basic block.

//! Control-flow graph edge kinds.
//! Edges can be
//!   - "fall through", meaning the target block's first instruction
//!     follows the source block's last instruction in the original
//!     InstructionSequence
//!   - "branch", meaning that the target block's first instruction
//!     is *not* the immediate successor of the source block's last
//!     instruction
enum EdgeKind {
  EDGE_FALLTHROUGH,
  EDGE_BRANCH,
};

//! Control-flow graph edge data type.
//! An Edge is a predecessor/successor connection between a source basic block
//! and a target basic block.
class Edge {
private:
  EdgeKind m_kind;
  std::shared_ptr<InstructionSequence> m_source, m_target;

public:
  //! Constructor.
  //! @param source the source basic block
  //! @param target the target basic block
  //! @param EdgeKind the kind of control edge
  Edge(std::shared_ptr<InstructionSequence> source, std::shared_ptr<InstructionSequence> target, EdgeKind kind);
  ~Edge();

  //! Get the EdgeKind of this Edge.
  //! @return the EdgeKind of this Edge
  EdgeKind get_kind() const { return m_kind; }

  //! Get the source basic block.
  //! @return the source basic block
  std::shared_ptr<InstructionSequence> get_source() const { return m_source; }

  //! Get the target basic block.
  //! @return the target basic block
  std::shared_ptr<InstructionSequence> get_target() const { return m_target; }
};

//! ControlFlowGraph: graph of basic blocks connected by control edges.
//! There are dedicated empty entry and exit blocks.
//! An InstructionSequence can be converted to a ControlFlowGraph
//! by a ControlFlowGraphBuilder object.
class ControlFlowGraph {
public:
  //! Data type for a vector of basic blocks.
  typedef std::vector<std::shared_ptr<InstructionSequence> > BlockList;

  //! Data type for a vector of edges.
  typedef std::vector<Edge *> EdgeList;

  //! Data type for a map of basic blocks to an EdgeList.
  typedef std::map<std::shared_ptr<InstructionSequence> , EdgeList> EdgeMap;

private:
  BlockList m_basic_blocks;
  std::shared_ptr<InstructionSequence> m_entry, m_exit;
  EdgeMap m_incoming_edges;
  EdgeMap m_outgoing_edges;
  EdgeList m_empty_edge_list;

  // A "Chunk" is a collection of InstructionSequences
  // connected by fall-through edges.  All of the blocks
  // in a Chunk must be emitted contiguously in the
  // resulting InstructionSequence when the CFG is flattened.
  struct Chunk {
    ControlFlowGraph::BlockList blocks;
    bool is_exit;

    void append(std::shared_ptr<InstructionSequence> bb) {
      blocks.push_back(bb);
      if (bb->get_kind() == BASICBLOCK_EXIT) { is_exit = true; }
    }
    void prepend(std::shared_ptr<InstructionSequence> bb) {
      blocks.insert(blocks.begin(), bb);
      if (bb->get_kind() == BASICBLOCK_EXIT) { is_exit = true; }
    }

    Chunk *merge_with(Chunk *other) {
      Chunk *merged = new Chunk();
      for (auto i = blocks.begin(); i != blocks.end(); i++) {
        merged->append(*i);
      }
      for (auto i = other->blocks.begin(); i != other->blocks.end(); i++) {
        merged->append(*i);
      }
      return merged;
    }

    bool is_first(std::shared_ptr<InstructionSequence> bb) const {
      return !blocks.empty() && blocks.front() == bb;
    }

    bool is_last(std::shared_ptr<InstructionSequence> bb) const {
      return !blocks.empty() && blocks.back() == bb;
    }

    bool contains_exit_block() const { return is_exit; }
  };

public:
  ControlFlowGraph();
  ~ControlFlowGraph();

  //! Get total number of basic blocks (including entry and exit).
  //! @return total number of basic blocks
  unsigned get_num_blocks() const { return unsigned(m_basic_blocks.size()); }

  //! Get pointer to the dedicated empty entry block.
  //! @return the entry block
  std::shared_ptr<InstructionSequence> get_entry_block() const;

  //! Get pointer to the dedicated empty exit block.
  //! @return the exit block
  std::shared_ptr<InstructionSequence> get_exit_block() const;

  //! Get block with specified id
  //! @param id the id value of a basic block
  //! @return the basic block with the specified id
  std::shared_ptr<InstructionSequence> get_block(unsigned id) const {
    assert(id < m_basic_blocks.size());
    return m_basic_blocks[id];
  }

  // iterator over pointers to InstructionSequences

  //! Get a begin iterator for the list of all basic blocks
  //! in the ControlFlowGraph.
  //! @return begin iterator to list of basic blocks
  BlockList::const_iterator bb_begin() const { return m_basic_blocks.cbegin(); }

  //! Get an end iterator for the list of all basic blocks
  //! in the ControlFlowGraph.
  //! @return end iterator to list of basic blocks
  BlockList::const_iterator bb_end() const   { return m_basic_blocks.cend(); }

  //! Create a new InstructionSequence. Use `BASICBLOCK_INTERIOR` for all blocks
  //! except for entry and exit. The label parameter should be a non-empty
  //! string if the InstructionSequence is reached via one or more branch instructions
  //! (which should have this label as their Operand.)
  //!
  //! @param kind the BasicBlockKind for the block to be created
  //! @param code_order relative order of this basic block in the original InstructionSequence
  //!                   (so that when the ControlFlowGraph is converted back to an InstructionSequence,
  //!                   the instructions are in the "correct" order)
  //! @param label label of the first instruction (if this basic block is
  //!              reached by any branch edges)
  //! @return a shared pointer to the new basic block
  std::shared_ptr<InstructionSequence> create_basic_block(BasicBlockKind kind, int code_order, const std::string &label = "");

  //! Adopt a new basic block.
  //! (Used by ControlFlowGraphTransform to add transformed blocks
  //! to the transformed ControlFlowGraph.)
  //! @param bb the basic block to adopt
  void adopt_basic_block(std::shared_ptr<InstructionSequence> &bb);

  //! Create Edge of given kind from source basic block to target basic block.
  //! Note that Edge objects are owned by the ControlFlowGraph.
  //!
  //! @param source the source basic block
  //! @param target the target basic block
  //! @param the EdgeKind of the Edge to create
  //! @return the created Edge
  Edge *create_edge(std::shared_ptr<InstructionSequence> source, std::shared_ptr<InstructionSequence> target, EdgeKind kind);

  //! Look up edge from specified source block to target block.
  //! Returns a null pointer if no such block exists
  //! @param source the source basic block
  //! @param target the target basic block
  //! @return the Edge connecting the source block to the target block,
  //!         or a null pointer if there is no such control edge
  Edge *lookup_edge(std::shared_ptr<InstructionSequence> source, std::shared_ptr<InstructionSequence> target) const;

  //! Get vector of all outgoing edges from given block.
  //! @param bb the basic block
  //! @return EdgeList of all outgoing edges from the basic block
  const EdgeList &get_outgoing_edges(std::shared_ptr<InstructionSequence> bb) const;

  //! Get vector of all incoming edges to given block.
  //! @param bb the basic block
  //! @return EdgeList of all incoming edges to the basic block
  const EdgeList &get_incoming_edges(std::shared_ptr<InstructionSequence> bb) const;

  //! Return a "flat" InstructionSequence created from this ControlFlowGraph.
  //! This is useful for optimization passes which create a transformed ControlFlowGraph.
  //! @return an InstructionSequence containing all instructions in all
  //          basic blocks
  std::shared_ptr<InstructionSequence> create_instruction_sequence() const;

private:
  std::vector<std::shared_ptr<InstructionSequence> > get_blocks_in_code_order() const;
  bool can_use_original_block_order() const;
  std::shared_ptr<InstructionSequence> rebuild_instruction_sequence() const;
  std::shared_ptr<InstructionSequence> reconstruct_instruction_sequence() const;
  void append_basic_block(std::shared_ptr<InstructionSequence> &iseq, std::shared_ptr<InstructionSequence> bb, std::vector<bool> &finished_blocks) const;
  void append_chunk(std::shared_ptr<InstructionSequence> &iseq, Chunk *chunk, std::vector<bool> &finished_blocks) const;
  void visit_successors(std::shared_ptr<InstructionSequence> bb, std::deque<std::shared_ptr<InstructionSequence> > &work_list) const;
  void delete_edges(EdgeMap &edge_map);
};

#endif // CFG_H
