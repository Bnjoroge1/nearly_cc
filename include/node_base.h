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

#ifndef NODE_BASE_H
#define NODE_BASE_H

#include <memory>
#include "type.h"
#include "symtab.h"
#include "literal_value.h"
#include "has_operand.h"
class Function;
//! The Node class will inherit from this type, so you can use it
//! to define any attributes and methods that Node objects should have
//! (constant value, results of semantic analysis, code generation info,
//! etc.)
//!
//! Because NodeBase inherits from HasOperand, each Node automatically
//! stores an Operand. This is useful for code generation: when
//! generating code to evaluate an expression, HighLevelCodegen
//! can set the Node's Operation to indicate the location where the
//! result of the evaluation is stored.
class NodeBase : public HasOperand {
private:
  // TODO: fields (pointer to Type, pointer to Symbol, etc.)
  std::shared_ptr<Type> m_type;
  Symbol *m_symbol;
  bool m_is_lvalue;
  LiteralValue m_literal_value;  
  Function *m_function;

  // copy ctor and assignment operator not supported
  NodeBase(const NodeBase &);
  NodeBase &operator=(const NodeBase &);

public:
  NodeBase();
  virtual ~NodeBase();
  void set_type(const std::shared_ptr<Type> &type);
  std::shared_ptr<Type> get_type() const;
  void set_symbol(Symbol *symbol);
  Symbol *get_symbol() const;

  void set_literal_value(const LiteralValue &val) { m_literal_value = val; }
  LiteralValue get_literal_value() const { return m_literal_value; }
  void set_function(Function *function) { m_function = function; }
  Function *get_function() const { return m_function; }
  void set_is_lvalue(bool is_lvalue);
  bool is_lvalue() const;
  // TODO: add member functions
};

#endif // NODE_BASE_H
