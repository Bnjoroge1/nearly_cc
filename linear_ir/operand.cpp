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

#include <map>
#include <cassert>
#include "operand.h"

namespace {

// flags for operand properties, based on operand kind
enum {
  INVALID    = (1 << 0),  // operand is invalid (not used)
  HL         = (1 << 1),  // high-level operand
  LL         = (1 << 2),  // low-level operand
  IMM_IVAL   = (1 << 3),  // immediate integer value
  LABEL      = (1 << 4),  // label
  IMM_LABEL  = (1 << 5),  // immediate label
  MEMREF     = (1 << 6),  // memory reference
  HAS_INDEX  = (1 << 7),  // has index register
  HAS_OFFSET = (1 << 8),  // memory reference with imm offset
  HAS_SCALE  = (1 << 9),  // memory reference with index scaling factor
};

struct OperandProperties {
  int flags;

  bool is_imm_ival() const     { return (flags & IMM_IVAL) != 0; }
  bool is_label() const        { return (flags & LABEL) != 0; }
  bool is_imm_label() const    { return (flags & IMM_LABEL) != 0; }
  bool has_index_reg() const   { return (flags & HAS_INDEX) != 0; }
  bool has_offset() const      { return (flags & HAS_OFFSET) != 0; }
  bool has_scale() const       { return (flags & HAS_SCALE) != 0; }
  bool is_non_reg() const      { return is_imm_ival() || is_label() || is_imm_label(); }
  bool is_memref() const       { return (flags & MEMREF) != 0; }
  bool has_imm_ival() const    { return is_imm_ival() || has_offset(); }
  bool has_label() const       { return is_label() || is_imm_label(); }
};

const std::map<Operand::Kind, OperandProperties> s_operand_props = {
  { Operand::NONE,             { .flags = INVALID } },
  { Operand::VREG,             { .flags = HL } },
  { Operand::VREG,             { .flags = HL } },
  { Operand::VREG,             { .flags = HL } },
  { Operand::VREG,             { .flags = HL } },
  { Operand::VREG_MEM,         { .flags = HL|MEMREF } },
  { Operand::VREG_MEM_OFF,     { .flags = HL|MEMREF|HAS_OFFSET } },
  { Operand::MREG8,            { .flags = LL } },
  { Operand::MREG16,           { .flags = LL } },
  { Operand::MREG32,           { .flags = LL } },
  { Operand::MREG64,           { .flags = LL } },
  { Operand::MREG64_MEM,       { .flags = LL|MEMREF } },
  { Operand::MREG64_MEM_IDX,   { .flags = LL|MEMREF|HAS_INDEX } },
  { Operand::MREG64_MEM_OFF,   { .flags = LL|MEMREF|HAS_OFFSET } },
  { Operand::MREG64_MEM_IDX_SCALE, { .flags = LL|MEMREF|HAS_INDEX|HAS_SCALE } },
  { Operand::IMM_IVAL,         { .flags = HL|LL|IMM_IVAL } },
  { Operand::LABEL,            { .flags = HL|LL|LABEL } },
  { Operand::IMM_LABEL,        { .flags = HL|LL|IMM_LABEL } },
};

const OperandProperties &oprops(Operand::Kind opkind) {
  auto i = s_operand_props.find(opkind);
  assert(i != s_operand_props.cend());
  return *((i != s_operand_props.end()) ? &i->second : nullptr);
}

}

// All constructors should delegate to this one, to ensure
// that all member variables are initialized.
Operand::Operand(Kind kind)
  : m_kind(kind)
  , m_basereg(-1)
  , m_index_reg(-1)
  , m_imm_ival(-1) {
}

// ival1 is either basereg or imm_ival (depending on operand Kind)
Operand::Operand(Kind kind, long ival1)
  : Operand(kind) {
  const OperandProperties &props = oprops(kind);
  if (!props.is_non_reg()) {
    m_basereg = int(ival1);
  } else if (props.is_imm_ival()) {
    m_imm_ival = ival1;
  } else {
    assert(false);
  }
}

// ival2 is either index_reg or imm_ival/offset (depending on operand kind)
Operand::Operand(Kind kind, int basereg, long ival2)
  : Operand(kind) {
  m_basereg = basereg;

  const OperandProperties &props = oprops(kind);
  if (props.has_index_reg()) {
    m_index_reg = int(ival2);
  } else if (props.has_imm_ival() || props.has_offset()) {
    m_imm_ival = ival2;
  } else {
    assert(false);
  }
}

Operand::Operand(Kind kind, int basereg, int indexreg, int scale)
  : Operand(kind) {
  m_basereg = basereg;
  m_index_reg = indexreg;
  m_imm_ival = scale;
  assert(kind == Operand::MREG64_MEM_IDX_SCALE);
}

// for label or immediate label operands
Operand::Operand(Kind kind, const std::string &label)
  : Operand(kind) {
  const OperandProperties &props = oprops(kind);
  assert(props.is_label() || props.is_imm_label());
  m_label = label;
}

Operand::~Operand() {
}

bool Operand::operator==(const Operand &rhs) const {
  const Operand &lhs = *this;

  if (lhs.get_kind() != rhs.get_kind())
    return false;

  switch (lhs.get_kind()) {
  case Operand::VREG:
  case Operand::VREG_MEM:
  case Operand::MREG8:
  case Operand::MREG16:
  case Operand::MREG32:
  case Operand::MREG64:
  case Operand::MREG64_MEM:
    return lhs.get_base_reg() == rhs.get_base_reg();

  case Operand::VREG_MEM_OFF:
  case Operand::MREG64_MEM_OFF:
    return lhs.get_base_reg() == rhs.get_base_reg() &&
           lhs.get_offset() == rhs.get_offset();

  case Operand::MREG64_MEM_IDX:
    return lhs.get_base_reg() == rhs.get_base_reg() &&
           lhs.get_index_reg() == rhs.get_index_reg();

  case Operand::MREG64_MEM_IDX_SCALE:
    return lhs.get_base_reg() == rhs.get_base_reg() &&
           lhs.get_index_reg() == rhs.get_index_reg() &&
           lhs.get_scale() == rhs.get_scale();

  case Operand::LABEL:
  case Operand::IMM_LABEL:
    return lhs.get_label() == rhs.get_label();

  case Operand::IMM_IVAL:
    return lhs.get_imm_ival() == rhs.get_imm_ival();

  default:
    assert(false);
    return false;
  }
}

Operand::Kind Operand::get_kind() const {
  return m_kind;
}

bool Operand::is_imm_ival() const {
  return oprops(m_kind).is_imm_ival();
}

bool Operand::is_label() const {
  return oprops(m_kind).is_label();
}

bool Operand::is_imm_label() const {
  return oprops(m_kind).is_imm_label();
}

bool Operand::has_index_reg() const {
  return oprops(m_kind).has_index_reg();
}

bool Operand::has_offset() const {
  return oprops(m_kind).has_offset();
}

bool Operand::has_scale() const {
  return oprops(m_kind).has_scale();
}

bool Operand::is_non_reg() const {
  return oprops(m_kind).is_non_reg();
}

bool Operand::is_memref() const {
  return oprops(m_kind).is_memref();
}

bool Operand::has_imm_ival() const {
  return oprops(m_kind).has_imm_ival();
}

bool Operand::has_label() const {
  return oprops(m_kind).has_label();
}

int Operand::get_base_reg() const {
  assert(!oprops(m_kind).is_non_reg());
  return m_basereg;
}

int Operand::get_index_reg() const {
  assert(oprops(m_kind).has_index_reg());
  return m_index_reg;
}

long Operand::get_imm_ival() const {
  assert(oprops(m_kind).has_imm_ival());
  return m_imm_ival;
}

long Operand::get_offset() const {
  assert(oprops(m_kind).has_offset());
  return m_imm_ival;
}

long Operand::get_scale() const {
  assert(oprops(m_kind).has_scale());
  return m_imm_ival;
}

void Operand::set_base_reg(int regnum) {
  assert(!oprops(m_kind).is_non_reg());
  m_basereg = regnum;
}

void Operand::set_index_reg(int regnum) {
  assert(oprops(m_kind).has_index_reg());
  m_index_reg = regnum;
}

void Operand::set_imm_ival(long ival) {
  assert(oprops(m_kind).has_imm_ival());
  m_imm_ival = ival;
}

void Operand::set_offset(long offset) {
  assert(oprops(m_kind).has_offset());
  m_imm_ival = offset;
}

Operand Operand::to_memref() const {
  assert(m_kind == Operand::VREG || m_kind == Operand::MREG64);
  Operand dup = *this;
  dup.m_kind = (m_kind == Operand::VREG) ? Operand::VREG_MEM : Operand::MREG64_MEM;
  return dup;
}

std::string Operand::get_label() const {
  assert(m_kind == Operand::LABEL || m_kind == Operand::IMM_LABEL);
  return m_label;
}
