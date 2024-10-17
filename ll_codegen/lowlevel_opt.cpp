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

LowLevelOpt::LowLevelOpt(const Options &options)
  : m_options(options) {
}

LowLevelOpt::~LowLevelOpt() {
}

void LowLevelOpt::optimize(std::shared_ptr<Function> function) {
  assert(m_options.has_option(Options::OPTIMIZE));

  // Like HighLevelOpt, LowLevelOpt will be most easily implemented
  // using optimizations deriving from ControlFlowGraphTransform.
  // E.g.:
  //
  //   std::shared_ptr<InstructionSequence> ll_iseq = function.get_ll_iseq();
  //   auto ll_cfg_builder = ::make_lowlevel_cfg_builder(ll_iseq);
  //   std::shared_ptr<ControlFlowGraph> ll_cfg = ll_cfg_builder.build();
  //
  //   Optimization1 opt1(ll_cfg, m_options);
  //   ll_cfg = opt1.transform_cfg();
  //
  //   Optimization2 opt2(ll_cfg, m_options);
  //   ll_cfg = opt2.transform_cfg();
  //
  //   ll_iseq = ll_cfg.create_instruction_sequence();
  //   function->set_ll_iseq(ll_iseq);
}

