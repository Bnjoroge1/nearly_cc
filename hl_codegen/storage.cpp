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
#include "storage.h"

namespace {

unsigned pad(unsigned offset, unsigned align) {
  // alignments must be powers of 2
  assert((align & (align - 1U)) == 0U);

  // Determine by how many bytes (if any) the offset is misaligned
  unsigned misalignment = offset & (align - 1U);

  // Determine how many bytes of padding should be added to the offset
  // to restore correct alignment
  assert(misalignment < align);
  return misalignment == 0U ? 0U : (align - misalignment);
}

}

StorageCalculator::StorageCalculator(Mode mode, unsigned start_offset)
  : m_mode(mode)
  , m_size(0U)
  , m_align(start_offset)
  , m_finished(false) {
  assert(mode == STRUCT || mode == UNION);
}

StorageCalculator::~StorageCalculator() {
}

unsigned StorageCalculator::add_field(std::shared_ptr<Type> type) {
  unsigned size = type->get_storage_size();
  unsigned align = type->get_alignment();

  // Keep track of largest alignment requirement, because
  // that will become the overall struct or union's
  // required alignment
  if (align > m_align) {
    m_align = align;
  }

  unsigned field_offset;

  if (m_mode == STRUCT) {
    // Determine amount of padding needed
    unsigned padding = pad(m_size, align);
    m_size += padding;

    // Now we know the offset of this field
    field_offset = m_size;

    // Next offset will, at a minimum, need to place the next
    // field beyond the storage for this one
    m_size += size;
  } else {
    // For a union, all field offsets are 0, and the union's
    // overall size is just the size of the largest member
    field_offset = 0U;
    if (size > m_size) {
      m_size = size;
    }
  }

  return field_offset;
}

void StorageCalculator::finish() {
  if (m_align == 0U) {
    // special case: if the struct or union has no fields,
    // its size is 0 and its alignment is 1
    assert(m_size == 0U);
    m_align = 1U;
  } else {
    if (m_mode == STRUCT) {
      // pad so that the overall size of the struct is a multiple
      // of the maximum field alignment
      m_size += pad(m_size, m_align);
    }
  }

  assert((m_align & (m_align - 1U)) == 0U);
  assert((m_size & (m_align - 1U)) == 0U);
  m_finished = true;
}

unsigned StorageCalculator::get_size() const {
  assert(m_finished);
  return m_size;
}

unsigned StorageCalculator::get_align() const {
  assert(m_finished);
  return m_align;
}
