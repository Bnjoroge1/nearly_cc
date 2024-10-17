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

#ifndef SEMANTIC_ANALYSIS_H
#define SEMANTIC_ANALYSIS_H

#include <cstdint>
#include <memory>
#include <utility>
#include "type.h"
#include "options.h"
#include "symtab.h"
#include "ast_visitor.h"

class SemanticAnalysis : public ASTVisitor {
public:
  typedef std::vector<SymbolTable *> SymbolTableList;

private:
  const Options &m_options;
  SymbolTable *m_global_symtab, *m_cur_symtab;
  SymbolTableList m_all_symtabs;

public:
  SemanticAnalysis(const Options &options);
  virtual ~SemanticAnalysis();

  SymbolTable *get_global_symtab() { return m_global_symtab; }

  // Iterator to access symbol tables
  SymbolTableList::const_iterator symtab_cbegin() const { return m_all_symtabs.cbegin(); }
  SymbolTableList::const_iterator symtab_cend() const { return m_all_symtabs.cend(); }

  virtual void visit_struct_type(Node *n);
  virtual void visit_union_type(Node *n);
  virtual void visit_variable_declaration(Node *n);
  virtual void visit_basic_type(Node *n);
  virtual void visit_named_declarator(Node *n);
  virtual void visit_pointer_declarator(Node *n);
  virtual void visit_array_declarator(Node *n);
  virtual void visit_function_definition(Node *n);
  virtual void visit_function_declaration(Node *n);
  virtual void visit_function_parameter_list(Node *n);
  virtual void visit_function_parameter(Node *n);
  virtual void visit_statement_list(Node *n);
  virtual void visit_return_expression_statement(Node *n);
  virtual void visit_struct_type_definition(Node *n);
  virtual void visit_binary_expression(Node *n);
  virtual void visit_unary_expression(Node *n);
  virtual void visit_postfix_expression(Node *n);
  virtual void visit_conditional_expression(Node *n);
  virtual void visit_cast_expression(Node *n);
  virtual void visit_function_call_expression(Node *n);
  virtual void visit_field_ref_expression(Node *n);
  virtual void visit_indirect_field_ref_expression(Node *n);
  virtual void visit_array_element_ref_expression(Node *n);
  virtual void visit_variable_ref(Node *n);
  virtual void visit_literal_value(Node *n);

private:
  //! Enter a nested scope.
  //! @param name the name of the nested scope
  //! @return pointer to the new SymbolTable representing the scope being entered
  SymbolTable *enter_scope(const std::string &name);

  //! Return to the parent of the current scope.
  void leave_scope();

  // TODO: add helper functions
};

#endif // SEMANTIC_ANALYSIS_H
