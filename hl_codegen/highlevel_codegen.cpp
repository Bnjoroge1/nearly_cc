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
#include "node.h"
#include "instruction.h"
#include "highlevel.h"
#include "ast.h"
#include "parse.tab.h"
#include "grammar_symbols.h"
#include "exceptions.h"
#include "local_storage_allocation.h"
#include "highlevel_codegen.h"


// Adjust an opcode for a basic type
HighLevelOpcode get_opcode(HighLevelOpcode base_opcode, std::shared_ptr<Type> type) {
  if (type->is_basic())
    return static_cast<HighLevelOpcode>(int(base_opcode) + int(type->get_basic_type_kind()));
  else if (type->is_pointer())
    return static_cast<HighLevelOpcode>(int(base_opcode) + int(BasicTypeKind::LONG));
  else
    RuntimeError::raise("attempt to use type '%s' as data in opcode selection", type->as_str().c_str());
}

HighLevelCodegen::HighLevelCodegen(const Options &options, int next_label_num)
  : m_options(options)
  , m_next_label_num(next_label_num)
{
}

HighLevelCodegen::~HighLevelCodegen() {
}

void HighLevelCodegen::generate(std::shared_ptr<Function> function) {
  assert(function->get_funcdef_ast() != nullptr);
  assert(!function->get_hl_iseq());

  // Create empty InstructionSequence to hold high-level instructions
  std::shared_ptr<InstructionSequence> hl_iseq(new InstructionSequence());
  function->set_hl_iseq(hl_iseq);

  // Member functions can use m_function to access the Function object
  m_function = function;

  // Visit function definition
  visit(function->get_funcdef_ast());
}

void HighLevelCodegen::visit_function_definition(Node *n) {
  // generate the name of the label that return instructions should target
  std::string fn_name = n->get_kid(1)->get_str();
  m_return_label_name = ".L" + fn_name + "_return";

  unsigned total_local_storage = 0U;
/*
  total_local_storage = n->get_total_local_storage();
*/

  get_hl_iseq()->append(new Instruction(HINS_enter, Operand(Operand::IMM_IVAL, total_local_storage)));

  // visit body
  visit(n->get_kid(3));

  get_hl_iseq()->define_label(m_return_label_name);
  get_hl_iseq()->append(new Instruction(HINS_leave, Operand(Operand::IMM_IVAL, total_local_storage)));
  get_hl_iseq()->append(new Instruction(HINS_ret));
}

void HighLevelCodegen::visit_statement_list(Node *n) {
  // TODO: implement
}

void HighLevelCodegen::visit_expression_statement(Node *n) {
  // TODO: implement
}

void HighLevelCodegen::visit_return_statement(Node *n) {
  // TODO: implement
}

void HighLevelCodegen::visit_return_expression_statement(Node *n) {
  // A possible implementation:
/*
  Node *expr = n->get_kid(0);

  // generate code to evaluate the expression
  visit(expr);

  // move the computed value to the return value vreg
  HighLevelOpcode mov_opcode = get_opcode(HINS_mov_b, expr->get_type());
  get_hl_iseq()->append(new Instruction(mov_opcode, Operand(Operand::VREG, LocalStorageAllocation::VREG_RETVAL), expr->get_operand()));

  // jump to the return label
  visit_return_statement(n);
*/
}

void HighLevelCodegen::visit_while_statement(Node *n) {
  // TODO: implement
}

void HighLevelCodegen::visit_do_while_statement(Node *n) {
  // TODO: implement
}

void HighLevelCodegen::visit_for_statement(Node *n) {
  // TODO: implement
}

void HighLevelCodegen::visit_if_statement(Node *n) {
  // TODO: implement
}

void HighLevelCodegen::visit_if_else_statement(Node *n) {
  // TODO: implement
}

void HighLevelCodegen::visit_binary_expression(Node *n) {
  // TODO: implement
}

void HighLevelCodegen::visit_unary_expression(Node *n) {
  // TODO: implement
}

void HighLevelCodegen::visit_function_call_expression(Node *n) {
  // TODO: implement
}

void HighLevelCodegen::visit_field_ref_expression(Node *n) {
  // TODO: implement
}

void HighLevelCodegen::visit_indirect_field_ref_expression(Node *n) {
  // TODO: implement
}

void HighLevelCodegen::visit_array_element_ref_expression(Node *n) {
  // TODO: implement
}

void HighLevelCodegen::visit_variable_ref(Node *n) {
  // TODO: implement
}

void HighLevelCodegen::visit_literal_value(Node *n) {
  // A partial implementation (note that this won't work correctly
  // for string constants!):
  /*
  LiteralValue val = n->get_literal_value();
  int vreg = next_temp_vreg();
  Operand dest(Operand::VREG, vreg);
  HighLevelOpcode mov_opcode = get_opcode(HINS_mov_b, n->get_type());
  get_hl_iseq()->append(new Instruction(mov_opcode, dest, Operand(Operand::IMM_IVAL, val.get_int_value())));
  n->set_operand(dest);
  */
}

void HighLevelCodegen::visit_implicit_conversion(Node *n) {
  // TODO: implement
}

std::string HighLevelCodegen::next_label() {
  std::string label = ".L" + std::to_string(m_next_label_num++);
  return label;
}

// TODO: additional private member functions
