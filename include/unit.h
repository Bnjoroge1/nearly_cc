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

#ifndef UNIT_H
#define UNIT_H

#include <memory>
#include <vector>
#include "node.h"
#include "semantic_analysis.h"
#include "string_constant.h"
#include "global_variable.h"
#include "function.h"

// A Unit object represents the entire translation unit,
// including the AST, the results of semantic analysis,
// and representations of string constants, global variables,
// and functions.
class Unit {
public:
  typedef std::vector<StringConstant> StringConstantList;
  typedef std::vector<GlobalVariable> GlobalVariableList;
  typedef std::vector<std::shared_ptr<Function>> FunctionList;

  typedef StringConstantList::const_iterator StringConstantListIterator;
  typedef GlobalVariableList::const_iterator GlobalVariableListIterator;
  typedef FunctionList::const_iterator FunctionListIterator;

private:
  std::unique_ptr<Node> m_ast;
  const Options &m_options;
  SemanticAnalysis m_sema;
  StringConstantList m_str_constants;
  GlobalVariableList m_global_variables;
  FunctionList m_functions;

  // value semantics are disabled
  Unit(const Unit &);
  Unit &operator=(const Unit &);

public:
  // Node that the Unit assumes ownership of the AST
  Unit(Node *ast_to_adopt, const Options &options);
  ~Unit();

  // Accessor to get the AST
  Node *get_ast() { return m_ast.get(); }

  // Accessor to get the Options object
  const Options &get_options() const { return m_options; }

  // Accessor to get the SemanticAnalysis object
  SemanticAnalysis &get_semantic_analysis() { return m_sema; }

  bool has_string_constants() const { return !m_str_constants.empty(); }
  bool has_global_variables() const { return !m_global_variables.empty(); }
  bool has_functions() const { return !m_functions.empty(); }

  // Iterate over string constants
  StringConstantListIterator strconst_cbegin() const { return m_str_constants.cbegin(); }
  StringConstantListIterator strconst_cend() const { return m_str_constants.cend(); }

  // Iterate over global variables
  GlobalVariableListIterator globalvar_cbegin() const { return m_global_variables.cbegin(); }
  GlobalVariableListIterator globalvar_cend() const { return m_global_variables.cend(); }

  // Iterate over functions
  FunctionListIterator fn_cbegin() const { return m_functions.cbegin(); }
  FunctionListIterator fn_cend() const { return m_functions.cend(); }

  // Add a string constant to the module
  void add_str_constant(const StringConstant &str_const);

  // Add a global ariable to the module
  void add_global_variable(const GlobalVariable &global_variable);

  // Add a function to the module
  void add_function(std::shared_ptr<Function> fn);
};


#endif // UNIT_H
