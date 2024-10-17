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

#include <cassert>
#include <cstdio>
#include <algorithm>
#include <iterator>
#include "cpputil.h"
#include "exceptions.h"
#include "highlevel.h"
#include "lowlevel.h"
#include "print_instruction_seq.h"
#include "highlevel_formatter.h"
#include "lowlevel_formatter.h"
#include "cfg.h"

////////////////////////////////////////////////////////////////////////
// Edge implementation
////////////////////////////////////////////////////////////////////////

Edge::Edge(std::shared_ptr<InstructionSequence> source, std::shared_ptr<InstructionSequence> target, EdgeKind kind)
  : m_kind(kind)
  , m_source(source)
  , m_target(target) {
}

Edge::~Edge() {
}

////////////////////////////////////////////////////////////////////////
// ControlFlowGraph implementation
////////////////////////////////////////////////////////////////////////

ControlFlowGraph::ControlFlowGraph()
  : m_entry(nullptr)
  , m_exit(nullptr) {
}

ControlFlowGraph::~ControlFlowGraph() {
  // Blocks don't need to be deleted because they are managed by
  // std::shared_ptr

  // Note that because edges are added to both m_incoming_edges and
  // m_outgoing_edges, it is sufficient to just iterate through
  // one of them to find all edges to delete.
  delete_edges(m_incoming_edges);
}

std::shared_ptr<InstructionSequence> ControlFlowGraph::get_entry_block() const {
  return m_entry;
}

std::shared_ptr<InstructionSequence> ControlFlowGraph::get_exit_block() const {
  return m_exit;
}

std::shared_ptr<InstructionSequence> ControlFlowGraph::create_basic_block(BasicBlockKind kind, int code_order, const std::string &label) {
  std::shared_ptr<InstructionSequence> bb(new InstructionSequence(kind, code_order, label));
  adopt_basic_block(bb);
  return bb;
}

void ControlFlowGraph::adopt_basic_block(std::shared_ptr<InstructionSequence> &bb) {
  // Assign a basic block id
  bb->set_block_id(unsigned(m_basic_blocks.size()));

  // Add to collection of blocks
  m_basic_blocks.push_back(bb);

  // If appropriate, update m_entry or m_exit
  if (bb->get_kind() == BASICBLOCK_ENTRY) {
    assert(m_entry == nullptr);
    m_entry = bb;
  }
  if (bb->get_kind() == BASICBLOCK_EXIT) {
    assert(m_exit == nullptr);
    m_exit = bb;
  }
}

Edge *ControlFlowGraph::create_edge(std::shared_ptr<InstructionSequence> source, std::shared_ptr<InstructionSequence> target, EdgeKind kind) {
  // make sure InstructionSequences belong to this ControlFlowGraph
  assert(std::find(m_basic_blocks.begin(), m_basic_blocks.end(), source) != m_basic_blocks.end());
  assert(std::find(m_basic_blocks.begin(), m_basic_blocks.end(), target) != m_basic_blocks.end());

  // make sure this Edge doesn't already exist
  assert(lookup_edge(source, target) == nullptr);

  // create the edge, add it to outgoing/incoming edge maps
  Edge *e = new Edge(source, target, kind);
  m_outgoing_edges[source].push_back(e);
  m_incoming_edges[target].push_back(e);

  return e;
}

Edge *ControlFlowGraph::lookup_edge(std::shared_ptr<InstructionSequence> source, std::shared_ptr<InstructionSequence> target) const {
  auto i = m_outgoing_edges.find(source);
  if (i == m_outgoing_edges.cend()) {
    return nullptr;
  }
  const EdgeList &outgoing = i->second;
  for (auto j = outgoing.cbegin(); j != outgoing.cend(); j++) {
    Edge *e = *j;
    assert(e->get_source() == source);
    if (e->get_target() == target) {
      return e;
    }
  }
  return nullptr;
}

const ControlFlowGraph::EdgeList &ControlFlowGraph::get_outgoing_edges(std::shared_ptr<InstructionSequence> bb) const {
  EdgeMap::const_iterator i = m_outgoing_edges.find(bb);
  return i == m_outgoing_edges.end() ? m_empty_edge_list : i->second;
}

const ControlFlowGraph::EdgeList &ControlFlowGraph::get_incoming_edges(std::shared_ptr<InstructionSequence> bb) const {
  EdgeMap::const_iterator i = m_incoming_edges.find(bb);
  return i == m_incoming_edges.end() ? m_empty_edge_list : i->second;
}

std::shared_ptr<InstructionSequence> ControlFlowGraph::create_instruction_sequence() const {
  // There are two algorithms for creating the result InstructionSequence.
  // The ideal one is rebuild_instruction_sequence(), which uses the original
  // "code order" to sequence the basic blocks. This is desirable, because
  // it means that CFG transformations don't introduce unnecessary
  // resequencing of blocks, which makes the results of transformations easier
  // to compare with the original code. However, if control flow is changed,
  // reconstruct_instruction_sequence() will do a more "invasive" reconstruction
  // which guarantees that blocks connected by fall-through edges are placed
  // together, but has the disadvantage that it could arbitrarily reorder
  // blocks compared to their original order.

  if (can_use_original_block_order())
    return rebuild_instruction_sequence();
  else
    return reconstruct_instruction_sequence();
}

std::vector<std::shared_ptr<InstructionSequence> > ControlFlowGraph::get_blocks_in_code_order() const {
  std::vector<std::shared_ptr<InstructionSequence> > blocks_in_code_order;
  std::copy(m_basic_blocks.begin(), m_basic_blocks.end(), std::back_inserter(blocks_in_code_order));

  // Sort blocks by their code order
  std::sort(blocks_in_code_order.begin(), blocks_in_code_order.end(),
            [](std::shared_ptr<InstructionSequence> left, std::shared_ptr<InstructionSequence> right) {
              assert(left->get_code_order() != right->get_code_order());
              return left->get_code_order() < right->get_code_order();
            });

  return blocks_in_code_order;
}

bool ControlFlowGraph::can_use_original_block_order() const {
  // The result InstructionSequence can use the original block order
  // (the "code order") as long as every fall-through branch connects
  // to the block that is the successor in code order.

  std::vector<std::shared_ptr<InstructionSequence> > blocks_in_code_order = get_blocks_in_code_order();

  // Check whether each fall-through edge leads to the next block
  // in code order
  bool good = true;
  auto i = blocks_in_code_order.begin();
  assert(i != blocks_in_code_order.end());
  std::shared_ptr<InstructionSequence> cur = *i;
  ++i;
  while (good && i != blocks_in_code_order.end()) {
    // Get the next block in code order
    std::shared_ptr<InstructionSequence> next = *i;
    ++i;

    // Check the edges of the current block to see if there is
    // a fall-through edge
    const EdgeList &outgoing_edges = get_outgoing_edges(cur);
    for (auto j = outgoing_edges.begin(); j != outgoing_edges.end(); ++j) {
      const Edge *edge = *j;
      if (edge->get_kind() == EDGE_FALLTHROUGH) {
        // If the fall-through edge doesn't lead to the next block
        // in code order, then we can't use the original block order
        if (edge->get_target() != next)
          good = false;
      }
    }

    // Continue to next pair of blocks
    cur = next;
  }

  return good;
}

std::shared_ptr<InstructionSequence> ControlFlowGraph::rebuild_instruction_sequence() const {
  // Rebuild an InstructionSequence in which the basic blocks are
  // placed in their original code order.  This should only be
  // done if can_use_original_block_order() returns true.

  std::shared_ptr<InstructionSequence> result(new InstructionSequence());

  std::vector<std::shared_ptr<InstructionSequence> > blocks_in_code_order = get_blocks_in_code_order();
  std::vector<bool> finished_blocks(get_num_blocks(), false);

  for (auto i = blocks_in_code_order.begin(); i != blocks_in_code_order.end(); ++i) {
    std::shared_ptr<InstructionSequence> bb = *i;
    append_basic_block(result, bb, finished_blocks);
  }

  return result;
}

std::shared_ptr<InstructionSequence> ControlFlowGraph::reconstruct_instruction_sequence() const {
  // Reconstruct an InstructionSequence when it's the case that the blocks
  // can not be laid out in the original code order because the control
  // flow has changed. This code does work (it was used in Fall 2020 and
  // Fall 2021 without issues.) However, it has the disadvantage of
  // sometimes making arbitrary changes to the order of the basic blocks,
  // which makes the transformed code harder to understand.

  // In theory, transformations that don't change control flow edges
  // should always permit the InstructionSequence to be laid out following
  // the code order of the original basic blocks (which is what
  // rebuild_instruction_sequence() does.)

  assert(m_entry != nullptr);
  assert(m_exit != nullptr);
  assert(m_outgoing_edges.size() == m_incoming_edges.size());

  // Find all Chunks (groups of basic blocks connected via fall-through)
  typedef std::map<std::shared_ptr<InstructionSequence> , Chunk *> ChunkMap;
  ChunkMap chunk_map;
  for (auto i = m_outgoing_edges.cbegin(); i != m_outgoing_edges.cend(); i++) {
    const EdgeList &outgoing_edges = i->second;
    for (auto j = outgoing_edges.cbegin(); j != outgoing_edges.cend(); j++) {
      Edge *e = *j;

      if (e->get_kind() != EDGE_FALLTHROUGH) {
        continue;
      }

      std::shared_ptr<InstructionSequence> pred = e->get_source();
      std::shared_ptr<InstructionSequence> succ = e->get_target();

      Chunk *pred_chunk = (chunk_map.find(pred) == chunk_map.end()) ? nullptr : chunk_map[pred];
      Chunk *succ_chunk = (chunk_map.find(succ) == chunk_map.end()) ? nullptr : chunk_map[succ];

      if (pred_chunk == nullptr && succ_chunk == nullptr) {
        // create a new chunk
        Chunk *chunk = new Chunk();
        chunk->append(pred);
        chunk->append(succ);
        chunk_map[pred] = chunk;
        chunk_map[succ] = chunk;
      } else if (pred_chunk == nullptr) {
        // prepend predecessor to successor's chunk (successor should be the first block)
        assert(succ_chunk->is_first(succ));
        succ_chunk->prepend(pred);
        chunk_map[pred] = succ_chunk;
      } else if (succ_chunk == nullptr) {
        // append successor to predecessor's chunk (predecessor should be the last block)
        assert(pred_chunk->is_last(pred));
        pred_chunk->append(succ);
        chunk_map[succ] = pred_chunk;
      } else {
        // merge the chunks
        Chunk *merged = pred_chunk->merge_with(succ_chunk);
        // update every basic block to point to the merged chunk
        for (auto i = merged->blocks.begin(); i != merged->blocks.end(); i++) {
          std::shared_ptr<InstructionSequence> bb = *i;
          chunk_map[bb] = merged;
        }
        // delete old Chunks
        delete pred_chunk;
        delete succ_chunk;
      }
    }
  }

  std::shared_ptr<InstructionSequence> result(new InstructionSequence());
  std::vector<bool> finished_blocks(get_num_blocks(), false);
  Chunk *exit_chunk = nullptr;

  // Traverse the CFG, appending basic blocks to the generated InstructionSequence.
  // If we find a block that is part of a Chunk, the entire Chunk is emitted.
  // (This allows fall through to work.)
  std::deque<std::shared_ptr<InstructionSequence> > work_list;
  work_list.push_back(m_entry);

  while (!work_list.empty()) {
    std::shared_ptr<InstructionSequence> bb = work_list.front();
    work_list.pop_front();
    unsigned block_id = bb->get_block_id();

    if (finished_blocks[block_id]) {
      // already added this block
      continue;
    }

    ChunkMap::iterator i = chunk_map.find(bb);
    if (i != chunk_map.end()) {
      // This basic block is part of a Chunk: append all of its blocks
      Chunk *chunk = i->second;

      // If this chunk contains the exit block, it needs to be at the end
      // of the generated InstructionSequence, so defer appending any of
      // its blocks.  (But, *do* find its control successors.)
      bool is_exit_chunk = false;
      if (chunk->contains_exit_block()) {
        exit_chunk = chunk;
        is_exit_chunk = true;
      }

      for (auto j = chunk->blocks.begin(); j != chunk->blocks.end(); j++) {
        std::shared_ptr<InstructionSequence> b = *j;
        if (is_exit_chunk) {
          // mark the block as finished, but don't append its instructions yet
          finished_blocks[b->get_block_id()] = true;
        } else {
          append_basic_block(result, b, finished_blocks);
        }

        // Visit control successors
        visit_successors(b, work_list);
      }
    } else {
      // This basic block is not part of a Chunk
      append_basic_block(result, bb, finished_blocks);

      // Visit control successors
      visit_successors(bb, work_list);
    }
  }

  // append exit chunk
  if (exit_chunk != nullptr) {
    append_chunk(result, exit_chunk, finished_blocks);
  }

  return result;
}

void ControlFlowGraph::append_basic_block(std::shared_ptr<InstructionSequence> &iseq, std::shared_ptr<InstructionSequence> bb, std::vector<bool> &finished_blocks) const {
  if (bb->has_block_label()) {
    iseq->define_label(bb->get_block_label());
  }
  for (auto i = bb->cbegin(); i != bb->cend(); i++) {
    iseq->append((*i)->duplicate());
  }
  finished_blocks[bb->get_block_id()] = true;
}

void ControlFlowGraph::append_chunk(std::shared_ptr<InstructionSequence> &iseq, Chunk *chunk, std::vector<bool> &finished_blocks) const {
  for (auto i = chunk->blocks.begin(); i != chunk->blocks.end(); i++) {
    append_basic_block(iseq, *i, finished_blocks);
  }
}

void ControlFlowGraph::visit_successors(std::shared_ptr<InstructionSequence> bb, std::deque<std::shared_ptr<InstructionSequence> > &work_list) const {
  const EdgeList &outgoing_edges = get_outgoing_edges(bb);
  for (auto k = outgoing_edges.cbegin(); k != outgoing_edges.cend(); k++) {
    Edge *e = *k;
    work_list.push_back(e->get_target());
  }
}

void ControlFlowGraph::delete_edges(EdgeMap &edge_map) {
  for (auto i = edge_map.begin(); i != edge_map.end(); ++i) {
    for (auto j = i->second.begin(); j != i->second.end(); ++j) {
      delete *j;
    }
  }
}
