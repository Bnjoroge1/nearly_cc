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

#ifndef STORAGE_H
#define STORAGE_H

#include "type.h"

// Compute storage size and field offsets for
// struct and union types. This can *also* be used to
// compute offsets and total storage size for local variables.
// Laying out fields in a struct and laying out storage for variables
// in a stack frame is essentially the same problem.
class StorageCalculator {
public:
  enum Mode { STRUCT, UNION };

private:
  Mode m_mode;
  unsigned m_size;
  unsigned m_align;
  bool m_finished;

public:
  // Note that the start_offset parameter is useful if you are
  // computing storage for local variables in a nested scope.
  // (You can set it to be the total amount of storage used
  // by the outer scopes.)
  StorageCalculator(Mode mode = STRUCT, unsigned start_offset = 0);
  ~StorageCalculator();

  // Add a field of given type.
  // Returns the field's storage offset.
  unsigned add_field(std::shared_ptr<Type> type);

  // Call this after all fields have been added.
  // Adds padding at end (if necessary).
  void finish();

  // Get storage size of overall struct or union
  unsigned get_size() const;

  // Get storage alignment of overall struct or union
  unsigned get_align() const;
};

#endif // STORAGE_H
