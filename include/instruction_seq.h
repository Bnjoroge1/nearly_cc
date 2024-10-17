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

#ifndef INSTRUCTION_SEQ_H
#define INSTRUCTION_SEQ_H

#include <vector>
#include <string>
#include <map>
#include "instruction_seq_iter.h"

//! @file
//! InstructionSequence and friends.
//! This is the "linear" IR representing a sequence of Instruction
//! objects.

class Instruction;

//! Kinds of basic blocks.
//! An InstructionSequence can be used to represent a basic block
//! in a ControlFlowGraph.
enum BasicBlockKind {
  BASICBLOCK_ENTRY,     //!< special "entry" block
  BASICBLOCK_EXIT,      //!< special "exit" block
  BASICBLOCK_INTERIOR,  //!< normal basic block in the "interior" of the CFG
};

//! InstructionSequence class, which represents a linear sequence of
//! Instruction objects. This can be used for both a "flat" IR for a
//! function (with control flow), or a basic block in a ControlFlowGraph
//! (where a branch or function call can only be the last instruction
//! in the block.)
class InstructionSequence {
private:
  struct Slot {
    std::string label;
    Instruction *ins;
  };

  std::vector<Slot> m_instructions;
  std::map<std::string, unsigned> m_label_map; // label to instruction index map
  std::string m_next_label;

  // These fields are only used when the InstructionSequence
  // is used as a basic block in a ControlFlowGraph
  BasicBlockKind m_kind;
  unsigned m_block_id;
  int m_code_order;

  // copy constructor and assignment operator are not allowed
  InstructionSequence(const InstructionSequence &);
  InstructionSequence &operator=(const InstructionSequence &);

public:
  // Iterator types, providing a pointer to an Instruction object when
  // dereferenced. These are random access.

  //! Iterator over instructions in forward order.
  typedef ISeqIterator<std::vector<Slot>::const_iterator> const_iterator;

  //! Iterator over instructions in reverse order.
  typedef ISeqIterator<std::vector<Slot>::const_reverse_iterator> const_reverse_iterator;

  //! Default constructor.
  InstructionSequence();

  //! Constructor from BasicBlockKind, code order value, and block label.
  //! This is useful for creating an InstructionSequence to be used
  //! as a basic block in a ControlFlowGraph.
  //! @param kind the BasicBlockKind
  //! @param code_order the relative order of this basic block
  //! @param block_label label of first Instruction in the basic block
  InstructionSequence(BasicBlockKind kind, int code_order, const std::string &block_label);

  virtual ~InstructionSequence();

  //! Return a dynamically-allocated duplicate of this InstructionSequence.
  //! @return duplicate of this InstructionSequence
  InstructionSequence *duplicate() const;

  // get begin and end const_iterators

  //! Get begin forward iterator over the instructions.
  //! @return begin forward iterator over the instructions
  const_iterator cbegin() const { return const_iterator(m_instructions.cbegin()); }

  //! Get end forward iterator over the instructions.
  //! @return end forward iterator over the instructions
  const_iterator cend() const { return const_iterator(m_instructions.cend()); }

  // get begin and end const_reverse_iterators

  //! Get begin reverse iterator over the instructions.
  //! @return begin reverse iterator over the instructions
  const_reverse_iterator crbegin() const { return const_reverse_iterator(m_instructions.crbegin()); }

  //! Get end reverse iterator over the instructions.
  //! @return end reverse iterator over the instructions
  const_reverse_iterator crend() const { return const_reverse_iterator(m_instructions.crend()); }

  // Apply a function to each Instruction in order.
  //! @tparam Fn the type of the function to apply
  //! @param fn the function to apply to each instruction
  template<typename Fn>
  void apply_to_all(Fn f) {
    for (auto i = cbegin(); i != cend(); ++i)
      f(*i);
  }

  //! Append an Instruction.
  //! The InstructionSequence will assume responsibility for deleting the
  //! Instruction object.
  //! @param ins pointer to the Instruction to append (and adopt)
  void append(Instruction *ins);

  //! Get number of instructions.
  //! @return the number of instrtuctions
  unsigned get_length() const;

  //! Get Instruction at specified index.
  //! @param index index of instruction to get (0 for first, etc.)
  //! @return pointer to the Instruction at that index
  Instruction *get_instruction(unsigned index) const;

  //! Get the last Instruction.
  //! @return pointer to last Instruction
  Instruction *get_last_instruction() const;

  //! Define a label. The next Instruction appended will be labeled with
  //! this label. This is useful for labeling an instruction as a control-flow
  //! target.
  //!
  //! @param label the label to apply to use for the next appended Instruction
  void define_label(const std::string &label);

  //! Determine if Instruction at given index has a label.
  //! @param index index of instruction to get (0 for first, etc.)
  //! @return true if the instruction at this index has a label, false if not
  bool has_label(unsigned index) const { return !m_instructions.at(index).label.empty(); }

  //! Get label of the Instruction at the specified index.
  //! @param index index of instruction to get (0 for first, etc.)
  //! @return label of the instructino at this index (empty string if
  //!         there is no label)
  std::string get_label_at_index(unsigned index) const { return m_instructions.at(index).label; }

  //! Determine if Instruction referred to by specified iterator has a label.
  //! @param iter a forward iterator
  //! @return true if the instruction pointed-to by the iterator has a label
  bool has_label(const_iterator iter) const { return iter.has_label(); }

  //! Determine if the InstructionSequence has a label at the end
  //! This can happen in code generation for control constructs, when
  //! a label is generated as a target for the code that comes after
  //! the construct.
  //! @return true if there is a label at the end of the InstructionSequence,
  //!         false if not
  bool has_label_at_end() const;

  //! Return a forward const iterator positioned at the instruction with
  //! the specified label, or the end iterator if there is no instruction
  //! with the specified label
  //! @param label a label
  //! @return the forward iterator pointing to the Instruction with the label,
  //!         or the end iterator if there is no instruction with this label
  const_iterator get_iterator_at_labeled_position(const std::string &label) const;

  //! Find Instruction labeled with specified label.
  //! Returns null pointer if no Instruction has the specified label.
  //! @param label a label
  //! @return pointer to the Instruction with the label, or a null pointer
  //!         if there is no Instruction with this label
  Instruction *find_labeled_instruction(const std::string &label) const;

  //! Return the index of instruction labeled with the specified label.
  //! @param label a label
  //! @param the index of the Instruction with this label
  //! @throws RuntimeError if there is no instruction with the label
  unsigned get_index_of_labeled_instruction(const std::string &label) const;

  // Member functions used for basic blocks

  //! Get the BasicBlockKind.
  //! This is useful for checking a basic block to see if it is the
  //! entry or exit block.
  //! @return the BasicBlockKind
  BasicBlockKind get_kind() const { return m_kind; }

  //! Set the BasicBlockKind.
  //! @param kind the BasicBlockKind to set
  void set_kind(BasicBlockKind kind) { m_kind = kind; }

  //! Check whether the first Instruction is labeled.
  //! This is useful for identifying whether a basic block
  //! is targeted by a branch instruction.
  //! @return true if the first Instruction in the block is
  //!         labeled, false if not
  bool has_block_label() const;

  //! Get the label of the first Instruction.
  //! @return true if the first Instruction is labeled, false if not
  std::string get_block_label() const;

  //! Set the label on the first Instruction.
  //! This is used by ControlFlowGraphBuilder when constructing a
  //! ControlFlowGraph from a "flat" InstructionSequence.
  //! @param block_label the label to set on the first Instruction
  void set_block_label(const std::string &block_label);

  //! Get the basic block id. This is an arbitrary integer
  //! whose only significance is that it is different than the
  //! id of any other basic block.
  //!
  //! @return the basic block id
  unsigned get_block_id() const { return m_block_id; }

  //! Set the basic block id.
  //! @param block_id the basic block id to set
  void set_block_id(unsigned block_id) { m_block_id = block_id; }

  //! Get the code order value of this block.
  //! This represents the relative position of the basic block's
  //! instructions in the original "flat" InstructionSequence.
  //! @return the basic block's code order value
  int get_code_order() const { return m_code_order; }

  //! Set the code order value of this block.
  //! @param code_order the code order value to set
  void set_code_order(int code_order) { m_code_order = code_order; }
};

#endif // INSTRUCTION_SEQ_H
