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
#include "instruction.h"
#include "exceptions.h"
#include "instruction_seq.h"

InstructionSequence::InstructionSequence()
  : m_kind(BASICBLOCK_INTERIOR)
  , m_block_id(unsigned(-1))
  , m_code_order(-1) {
}

InstructionSequence::InstructionSequence(BasicBlockKind kind, int code_order, const std::string &block_label)
  : m_next_label(block_label)
  , m_kind(kind)
  //, m_block_label(block_label)
  , m_block_id(unsigned(-1))
  , m_code_order(code_order) {

}

InstructionSequence::~InstructionSequence() {
  // delete the Instructions
  for (auto i = m_instructions.begin(); i != m_instructions.end(); ++i)
    delete i->ins;
}

InstructionSequence *InstructionSequence::duplicate() const {
  InstructionSequence *dup = new InstructionSequence();
  dup->m_next_label = m_next_label;

  for (auto i = m_instructions.begin(); i != m_instructions.end(); ++i) {
    const Slot &slot = *i;
    if (!slot.label.empty())
      dup->define_label(slot.label);
    dup->append(slot.ins->duplicate());
  }

  return dup;
}

void InstructionSequence::append(Instruction *ins) {
  if (!m_next_label.empty()) {
    unsigned index = unsigned(m_instructions.size());
    m_label_map[m_next_label] = index;
  }
  m_instructions.push_back({ label: m_next_label, ins: ins });
  m_next_label = "";
}

unsigned InstructionSequence::get_length() const {
  return unsigned(m_instructions.size());
}

Instruction *InstructionSequence::get_instruction(unsigned index) const {
  return m_instructions.at(index).ins;
}

Instruction *InstructionSequence::get_last_instruction() const {
  assert(!m_instructions.empty());
  return m_instructions.back().ins;
}

void InstructionSequence::define_label(const std::string &label) {
  assert(m_next_label.empty());
  m_next_label = label;
}

bool InstructionSequence::has_label_at_end() const {
  return !m_next_label.empty();
}

InstructionSequence::const_iterator InstructionSequence::get_iterator_at_labeled_position(const std::string &label) const {
  auto i = m_label_map.find(label);
  if (i == m_label_map.end())
    return const_iterator(m_instructions.end());
  else
    return const_iterator(m_instructions.begin() + i->second);
}

Instruction *InstructionSequence::find_labeled_instruction(const std::string &label) const {
  const_iterator i = get_iterator_at_labeled_position(label);
  return (i != cend()) ? *i : nullptr;
}

unsigned InstructionSequence::get_index_of_labeled_instruction(const std::string &label) const {
  auto i = get_iterator_at_labeled_position(label);
  if (i == cend())
    RuntimeError::raise("no instruction has label '%s'", label.c_str());
  return unsigned(i - cbegin());
}

bool InstructionSequence::has_block_label() const {
  if (m_instructions.empty())
    return has_label_at_end();
  else
    return has_label(0);
}

std::string InstructionSequence::get_block_label() const {
  if (m_instructions.empty())
    return m_next_label;
  else
    return get_label_at_index(0);
}

void InstructionSequence::set_block_label(const std::string &block_label) {
  if (m_instructions.empty())
    define_label(block_label);
  else
    m_instructions.at(0).label = block_label;
}
