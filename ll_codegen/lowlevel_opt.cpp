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
#include "cfg_builder.h"
#include "peephole_ll.h"
#include "lowlevel_opt.h"
#include "loop_constant_opt.h"
#include "cfg.h"
#include "lowlevel.h" 

LowLevelOpt::LowLevelOpt(const Options &options)
  : m_options(options) {
}

LowLevelOpt::~LowLevelOpt() {
}

void LowLevelOpt::optimize(std::shared_ptr<Function> function) {
  assert(m_options.has_option(Options::OPTIMIZE));

  // Get the low-level instruction sequence
  std::shared_ptr<InstructionSequence> ll_iseq = function->get_ll_iseq();
  
  // Build CFG
  auto ll_cfg_builder = make_lowlevel_cfg_builder(ll_iseq);
  std::shared_ptr<ControlFlowGraph> ll_cfg = ll_cfg_builder.build();

  // Apply loop constant optimization
  LoopConstantOpt loop_opt(ll_cfg);
  ll_cfg = loop_opt.transform_cfg();

  // Convert back to instruction sequence
  ll_iseq = ll_cfg->create_instruction_sequence();
  
  // Copy over any block labels and properties
  if (ll_iseq->has_block_label()) {
    ll_iseq->set_block_label(function->get_name());
  }
  
  // Set the instruction sequence back in the function
  function->set_ll_iseq(ll_iseq);
}

