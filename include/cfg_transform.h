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

#ifndef CFG_TRANSFORM_H
#define CFG_TRANSFORM_H

#include <memory>
#include "cfg.h"

//! Base class for ControlFlowGraphTransform transformations.
//! This is the preferred way to implement local optimizations.
//! In general, implementations of ControlFlowGraphTransform are
//! intended to be nondestructive, meaning that they return a
//! new result ControlFlowGraph without modifying the original one.
class ControlFlowGraphTransform {
private:
  std::shared_ptr<ControlFlowGraph> m_cfg;

public:
  //! Constructor.
  //! @param cfg the ControlFlowGraph to transform
  ControlFlowGraphTransform(std::shared_ptr<ControlFlowGraph> cfg);
  virtual ~ControlFlowGraphTransform();

  //! Get a shared pointer to the original ControlFlowGraph.
  //! @return the original ControlFlowGraph
  std::shared_ptr<ControlFlowGraph> get_orig_cfg();

  //! Transform the original ControlFlowGraph.
  //! @return shared pointer to the transformed ControlFlowGraph
  virtual std::shared_ptr<ControlFlowGraph> transform_cfg();

  //! Create a transformed version of the instructions in a basic block.
  //! Note that an InstructionSequence "owns" the Instruction objects it contains,
  //! and is responsible for deleting them. Therefore, be careful to avoid
  //! having two InstructionSequences contain pointers to the same Instruction.
  //! If you need to make an exact copy of an Instruction object, you can
  //! do so using the `duplicate()` member function, as follows:
  //!
  //! ```
  //! Instruction *orig_ins = // ...an Instruction object...
  //! Instruction *dup_ins = orig_ins->duplicate();
  //! ```
  //!
  //! @param orig_bb the original basic block
  //! @return shared pointer to the new basic block InstructionSequence
  virtual std::shared_ptr<InstructionSequence> transform_basic_block(std::shared_ptr<InstructionSequence> orig_bb) = 0;
};

#endif // CFG_TRANSFORM_H
