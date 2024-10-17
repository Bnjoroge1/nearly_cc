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

#ifndef LOCAL_STORAGE_ALLOCATION_H
#define LOCAL_STORAGE_ALLOCATION_H

#include "storage.h"
#include "ast_visitor.h"
#include "function.h"

//! A LocalStorageAllocation object is responsible for allocating local
//! storage for variables in exactly one function. The `allocate_storage()`
//! member function will be called with a shared pointer to the
//! Function containing the information about the function
//! (including its AST). The Function object is also intended to
//! the place where storage allocation decisions for the function
//! are recorded.
class LocalStorageAllocation : public ASTVisitor {
public:
  // vr0 is the return value vreg
  static const int VREG_RETVAL = 0;

  // vr1 is 1st argument vreg
  static const int VREG_FIRST_ARG = 1;

  // local variable allocation starts at vr10
  static const int VREG_FIRST_LOCAL = 10;

private:
  std::shared_ptr<Function> m_function;
  StorageCalculator m_storage_calc;
  unsigned m_total_local_storage;
  int m_next_vreg;

public:
  LocalStorageAllocation();
  virtual ~LocalStorageAllocation();

  //! Allocate storage for all variables defined in the specified Function.
  //!
  //! Suggested approach:
  //! - find all variable declarations in the AST
  //! - for all non-struct/non-array variables whose addresses aren't taken,
  //!   allocate a virtual register as the storage location for the variable
  //! - for all struct variables, array variables, and variables whose address
  //!   is taken, allocate storage in memory in the function's stack frame
  //!
  //! The symbol table entry (Symbol) of a variable is probably the best place
  //! to record the storage allocation decision for that variable.
  //!
  //! An instance of StorageCalculator could be useful for allocating storage
  //! for local variables requiring memory storage.
  //!
  //! @param function the Function containing the AST representaion of the function
  //!                 to allocate local storage for
  void allocate_storage(std::shared_ptr<Function> function);

  virtual void visit_function_definition(Node *n);
  virtual void visit_statement_list(Node *n);
  // TODO: override any other AST visitor member functions you need to

private:
  // TODO: add private member functions
};

#endif // LOCAL_STORAGE_ALLOCATION_H
