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

#include <cassert>
#include "cfg.h"
#include "cfg_transform.h"

ControlFlowGraphTransform::ControlFlowGraphTransform(std::shared_ptr<ControlFlowGraph> cfg)
  : m_cfg(cfg) {
}

ControlFlowGraphTransform::~ControlFlowGraphTransform() {
}

std::shared_ptr<ControlFlowGraph> ControlFlowGraphTransform::get_orig_cfg() {
  return m_cfg;
}

std::shared_ptr<ControlFlowGraph> ControlFlowGraphTransform::transform_cfg() {
  std::shared_ptr<ControlFlowGraph> result(new ControlFlowGraph());

  // map of basic blocks of original CFG to basic blocks in transformed CFG
  std::map<std::shared_ptr<InstructionSequence>, std::shared_ptr<InstructionSequence>> block_map;

  // iterate over all basic blocks, transforming each one
  for (auto i = m_cfg->bb_begin(); i != m_cfg->bb_end(); i++) {
    std::shared_ptr<InstructionSequence> orig = *i;

    // Transform the instructions
    std::shared_ptr<InstructionSequence> result_bb = transform_basic_block(orig);

    // Set basic block properties (code order, block label, etc.) of result
    // basic block to be the same as the original
    result_bb->set_kind(orig->get_kind());
    result_bb->set_code_order(orig->get_code_order());
    result_bb->set_block_label(orig->get_block_label());

    // Map original block to transformed block
    // (so that we can reconstruct edges)
    block_map[orig] = result_bb;

    // Have CFG formally adopt the basic block
    result->adopt_basic_block(result_bb);
  }

  // add edges to transformed CFG
  for (auto i = m_cfg->bb_begin(); i != m_cfg->bb_end(); i++) {
    std::shared_ptr<InstructionSequence> orig = *i;
    const ControlFlowGraph::EdgeList &outgoing_edges = m_cfg->get_outgoing_edges(orig);
    for (auto j = outgoing_edges.cbegin(); j != outgoing_edges.cend(); j++) {
      Edge *orig_edge = *j;

      std::shared_ptr<InstructionSequence> transformed_source = block_map[orig_edge->get_source()];
      std::shared_ptr<InstructionSequence> transformed_target = block_map[orig_edge->get_target()];

      result->create_edge(transformed_source, transformed_target, orig_edge->get_kind());
    }
  }

  return result;
}
