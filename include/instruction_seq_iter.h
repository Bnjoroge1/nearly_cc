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

#ifndef INSTRUCTION_SEQ_ITER_H
#define INSTRUCTION_SEQ_ITER_H

#include <cstddef> // for ptrdiff_t

class Instruction;

//! Generic `const_iterator` type for InstructionSequence.
//! When dereferenced, provides a pointer to the referenced Instruction
//! object.  The @ref has_label() and @ref get_label() member functions can
//! be used (respectively) to determine if the referenced Instruction
//! has a label and to access the label.  It is parametized with the
//! underlying vector const iterator type, to allow forward and reverse
//! versions to be defined easily.  It supports random access
//! (adding or subtracting a signed integer offset.)
//!
//! @tparam It an iterator over elements of a vector of Instruction::Slot objects
//!            containing instructions and (possibly) labels
template<typename It>
class ISeqIterator {
private:
  It slot_iter;

public:
  //! Default constructor.
  ISeqIterator() { }

  //! Constructor from a slot vector iteraor.
  //! @param i a slot vector iterator
  ISeqIterator(It i) : slot_iter(i) { }

  //! Copy constructor.
  //! @param other ISeqIterator object to copy from
  ISeqIterator(const ISeqIterator<It> &other) : slot_iter(other.slot_iter) { }

  //! Assignment operator.
  //! @param rhs ISeqIterator object to copy from
  ISeqIterator<It> &operator=(const ISeqIterator<It> &rhs) {
    if (this != &rhs) { slot_iter = rhs.slot_iter; }
    return *this;
  }

  // Equality and inequality comparisons

  //! Equality comparison.
  //! @param rhs ISeqIterator object to compare this object to
  //! @return true if this object is equal to (points to the same Instruction as) rhs,
  //!         false otherwise
  bool operator==(const ISeqIterator<It> &rhs) const {
    return slot_iter == rhs.slot_iter;
  }

  //! Inequality comparison.
  //! @param rhs ISeqIterator object to compare this object to
  //! @return true if this object is not equal to (points to a different Instruction as) rhs,
  //!         false otherwise
  bool operator!=(const ISeqIterator<It> &rhs) const {
    return slot_iter != rhs.slot_iter;
  }

  //! Dereference operator.
  //! @return pointer to the Instruction this iterator points to
  Instruction* operator*() const { return slot_iter->ins; }

  // Access to the referenced Instruction's label

  //! Check whether the iterator points to an Instruction that has a label.
  //! @return true if the pointed-to Instruction has a label, false if not
  bool has_label() const { return !slot_iter->label.empty(); }

  //! Get the label of the Instruction this iterator points to.
  //! @return the label of the Increment this iterator points to
  std::string get_label() const { return slot_iter->label; }

  // Increment and decrement

  //! Pre-increment. Advances the iterator to the next Instruction.
  //! @return iterator pointing to the next Instruction
  ISeqIterator<It> &operator++() {
    ++slot_iter;
    return *this;
  }

  //! Post-increment. Advances the iterator to the next Instruction,
  //! but returns an iterator pointing to the original instruction.
  //! @return iterator pointing to the original Instruction
  ISeqIterator<It> operator++(int) {
    ISeqIterator<It> copy(*this);
    ++slot_iter;
    return copy;
  }

  //! Pre-decrement. Moves the iterator back to the previous Instruction.
  //! @return iterator pointing to the previous Instruction
  ISeqIterator<It> &operator--() {
    --slot_iter;
    return *this;
  }

  //! Post-decrement. Moves the iterator back to the previous Instruction,
  //! but returns an iterator pointing to the original instruction.
  //! @return iterator pointing to the original Instruction
  ISeqIterator<It> operator--(int) {
    ISeqIterator<It> copy(*this);
    --slot_iter;
    return copy;
  }

  // Support
  //   - adding and subtracting integer values
  //   - computing pointer difference between iterators
  //   - relational operators
  // so that instruction sequence iterators are random access

  //! "Pointer arithmetic": add integer offset to this iterator.
  //! @param i integer offset
  //! @return iterator pointing to element i positions from the one this
  //!         iterator points to
  ISeqIterator<It> operator+(ptrdiff_t i) const {
    return { slot_iter + i };
  }

  //! "Pointer arithmetic": subtract integer offset from this iterator.
  //! @param i integer offset
  //! @return iterator pointing to element -i positions from the one this
  //!         iterator points to
  ISeqIterator<It> operator-(ptrdiff_t i) const {
    return { slot_iter - i };
  }

  //! Advance the iterator a number of positions specified by an integer offset.
  //! @param i integer offset (number of positions to advance)
  //! @return reference to this iterator
  ISeqIterator<It> &operator+=(ptrdiff_t i) {
    slot_iter += i;
    return *this;
  }

  //! Move the iterator back a number of positions specified by an integer offset.
  //! @param i integer offset (number of positions to move back)
  //! @return reference to this iterator
  ISeqIterator<It> &operator-=(ptrdiff_t i) {
    slot_iter -= i;
    return *this;
  }

  //! "Pointer difference": return the number of positions necessary to
  //! move forward from the given one in order to reach the Instruction
  //! this iterator points to.
  //! @param rhs another iterator
  //! @return number of positions needed to move forward from the Instruction
  //!         rhs points to to reach the Instruction this iterator points to
  ptrdiff_t operator-(ISeqIterator<It> rhs) {
    return slot_iter - rhs.slot_iter;
  }

  //! Less than operator.
  //! @param rhs another iterator
  //! @return true if this iterator points to an Instruction which is
  //!         earlier than the one this iterator points to, false otherwise
  bool operator<(ISeqIterator<It> rhs) const {
    return slot_iter < rhs.slot_iter;
  }

  //! Less than or equal operator.
  //! @param rhs another iterator
  //! @return true if this iterator points to an Instruction which is
  //!         earlier or the same as the one this iterator points to,
  //!         false otherwise
  bool operator<=(ISeqIterator<It> rhs) const {
    return slot_iter <= rhs.slot_iter;
  }

  //! Greater than operator.
  //! @param rhs another iterator
  //! @return true if this iterator points to an Instruction which is
  //!         later than the one this iterator points to, false otherwise
  bool operator>(ISeqIterator<It> rhs) const {
    return slot_iter > rhs.slot_iter;
  }

  //! Greater than or equal operator.
  //! @param rhs another iterator
  //! @return true if this iterator points to an Instruction which is
  //!         later or the same as the one this iterator points to,
  //!         false otherwise
  bool operator>=(ISeqIterator<It> rhs) const {
    return slot_iter >= rhs.slot_iter;
  }
};

#endif // INSTRUCTION_SEQ_ITER_H
