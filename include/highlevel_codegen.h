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

#include <string>
#include <memory>
#include "highlevel.h"
#include "instruction_seq.h"
#include "ast_visitor.h"
#include "options.h"
#include "function.h"

//! A HighLevelCodegen visitor generates high-level IR code for
//! a single function definition AST. Code generation is initiated by
//! calling the `generate()` member function, which in turn
//! visits the function definition AST node.
class HighLevelCodegen : public ASTVisitor {
private:
  const Options &m_options;
  std::shared_ptr<Function> m_function;
  int m_next_label_num;
  std::string m_return_label_name; // name of the label that return instructions should target

public:
  //! Constructor.
  //! @param options the command-line Options
  //! @param next_label_num the next value that should be used when generating
  //!                       a control-flow label (this is used to ensure that
  //!                       control-flow labels aren't reused between functions)
  HighLevelCodegen(const Options &options, int next_label_num);

  virtual ~HighLevelCodegen();

  //! Create a high-level InstructionSequence from a function definition AST.
  //! The resulting InstructionSequence should be stored in the Function object.
  //!
  //! @param function shared pointer to the Function object
  void generate(std::shared_ptr<Function> function);

  //! Get a shared pointer to the high-level InstructionSequence containing
  //! the generated code.
  //! @return shared pointer to the high-level InstructionSequence
  std::shared_ptr<InstructionSequence> get_hl_iseq() { return m_function->get_hl_iseq(); }

  //! Get the next unused control-flow label number.
  //! @return the next unused control-flow label number
  int get_next_label_num() const { return m_next_label_num; }

  virtual void visit_function_definition(Node *n);
  virtual void visit_statement_list(Node *n);
  virtual void visit_expression_statement(Node *n);
  virtual void visit_return_statement(Node *n);
  virtual void visit_return_expression_statement(Node *n);
  virtual void visit_while_statement(Node *n);
  virtual void visit_do_while_statement(Node *n);
  virtual void visit_for_statement(Node *n);
  virtual void visit_if_statement(Node *n);
  virtual void visit_if_else_statement(Node *n);
  virtual void visit_binary_expression(Node *n);
  virtual void visit_unary_expression(Node *n);
  virtual void visit_function_call_expression(Node *n);
  virtual void visit_field_ref_expression(Node *n);
  virtual void visit_indirect_field_ref_expression(Node *n);
  virtual void visit_array_element_ref_expression(Node *n);
  virtual void visit_variable_ref(Node *n);
  virtual void visit_literal_value(Node *n);
  virtual void visit_implicit_conversion(Node *n);

private:
  std::string next_label();
  // TODO: additional private member functions
};
