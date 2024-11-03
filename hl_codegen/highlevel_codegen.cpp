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
#include <iostream>


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
  , m_next_vreg(LocalStorageAllocation::VREG_FIRST_LOCAL)
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
  for (unsigned i = 0; i < n->get_num_kids(); i++) {
    Node *stmt = n->get_kid(i);
    if (stmt != nullptr) {
      visit(stmt);
    }
  }
}

void HighLevelCodegen::visit_expression_statement(Node *n) {
  printf("DEBUG: Entering expression statement\n");

  if (n->get_num_kids() > 0) {
    Node *expr = n->get_kid(0);
    if (expr != nullptr) {
      // Visit the expression to generate its code and store result
      printf("DEBUG: Expression node tag: %d\n", expr->get_tag());

      visit(expr);
      
      // For expression statements, we need to ensure the computed value 
      // is stored in a virtual register
      if (expr->has_operand()) {
        // Create a new virtual register for the result
        int vreg = LocalStorageAllocation::VREG_FIRST_LOCAL + m_next_label_num++;
        Operand dest(Operand::VREG, vreg);
        
        // Move the computed value into the new register
        HighLevelOpcode mov_opcode = get_opcode(HINS_mov_b, expr->get_type());
        get_hl_iseq()->append(new Instruction(mov_opcode, dest, expr->get_operand()));
      }
    }
  }
}

void HighLevelCodegen::visit_return_statement(Node *n) {
  // For a return with no value, set vr0 to 0
  get_hl_iseq()->append(new Instruction(HINS_mov_l, 
    Operand(Operand::VREG, LocalStorageAllocation::VREG_RETVAL),
    Operand(Operand::IMM_IVAL, 0)));
    
  // Jump to the function's return label
  if (!m_return_label_name.empty()) {
    get_hl_iseq()->append(new Instruction(HINS_jmp,
      Operand(Operand::LABEL, m_return_label_name)));
  }
}

void HighLevelCodegen::visit_return_expression_statement(Node *n) {
  if (n->get_num_kids() > 0) {
    Node *expr = n->get_kid(0);
    Operand val = expr->get_operand(); 
    // Generate code to evaluate the expression
    visit(expr);
    
    // Move the computed value to the return value vreg
    if (expr->has_operand()) {
      HighLevelOpcode mov_opcode = get_opcode(HINS_mov_b, expr->get_type());
      get_hl_iseq()->append(new Instruction(mov_opcode, 
        Operand(Operand::VREG, LocalStorageAllocation::VREG_RETVAL),
        expr->get_operand()));
    }
  }
  
  // Jump to return label
  if (!m_return_label_name.empty()) {
    get_hl_iseq()->append(new Instruction(HINS_jmp,
      Operand(Operand::LABEL, m_return_label_name)));
  }
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
  printf("DEBUG: Entering binary_expression\n");
  if (n->get_tag() == AST_BINARY_EXPRESSION) {
    std::string op = n->get_kid(0)->get_str();
    printf("DEBUG: Binary expression operator: %s\n", op.c_str());
    
    if (op == "=") {
      // Visit right operand first to evaluate the expression
      printf("DEBUG: Visiting right operand\n");
      visit(n->get_kid(2));
      Operand source = n->get_kid(2)->get_operand();
      
      // Visit left operand (lvalue)
      printf("DEBUG: Visiting left operand\n");
      visit(n->get_kid(1));
      Operand dest = n->get_kid(1)->get_operand();
      
      // Generate single move instruction from source to dest
      printf("DEBUG: Moving value from source to dest\n");
      get_hl_iseq()->append(new Instruction(HINS_mov_l, dest, source));
      printf("DEBUG: Move instruction generated\n");
    }else if (op == "+") {
      // Visit left operand
      visit(n->get_kid(1));
      Operand left = n->get_kid(1)->get_operand();
      
      // Visit right operand
      visit(n->get_kid(2));
      Operand right = n->get_kid(2)->get_operand();
      
      // Create new vreg for result
      int result_vreg = m_next_vreg++;
      Operand result(Operand::VREG, result_vreg);
      
      // Generate add instruction
      get_hl_iseq()->append(new Instruction(HINS_add_l, result, left, right));
      
      // Set this node's operand to the result
      n->set_operand(result);
    }

  }
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
  Symbol *sym = n->get_symbol();
  if (!sym) {
    printf("DEBUG: No symbol found in variable ref\n");
    return;
  }
  
  printf("DEBUG: Entering variable_ref for %s\n", sym->get_name().c_str());
  
  // Get the function's symbol table
  SymbolTable* symtab = m_function->get_symbol()->get_symtab();
  
  // Look up the symbol to get the consistent storage info
  Symbol* stored_sym = symtab->lookup_recursive(sym->get_name());
  if (stored_sym) {
    // Use storage info from the stored symbol
    int vreg = stored_sym->get_vreg();
    printf("DEBUG: Found symbol in symtab with vreg: %d\n", vreg);
    
    // Set operand based on storage location
    if (vreg >= 0) {
      n->set_operand(Operand(Operand::VREG, vreg));
      printf("DEBUG: Set operand to vreg %d\n", vreg);
    } else {
      // Should not happen for simple variables since they're allocated in registers
      printf("DEBUG: ERROR: Simple variable has invalid vreg\n");
    }
  } else {
    printf("DEBUG: Symbol not found in function's symbol table\n");
  }
}

void HighLevelCodegen::visit_literal_value(Node *n) {
  printf("DEBUG: Entering literal_value\n");
  printf("DEBUG: Node tag: %d\n", n->get_tag());
  
  if (!n || !n->get_type()) {
    return;
  }
  

  // Get the literal value from the token node
  Node *literal_token = n->get_kid(0);  // Get the TOK_INT_LIT node
  if (literal_token) {
    int val_vreg = m_next_vreg++;
    Operand dest(Operand::VREG, val_vreg);
    // Create a new LiteralValue from the token's value
    LiteralValue val = LiteralValue::from_int_literal(literal_token->get_str(), literal_token->get_loc());
    
    // Set the immediate value operand directly
    get_hl_iseq()->append(new Instruction(HINS_mov_l, dest, Operand(Operand::IMM_IVAL, val.get_int_value())));

  
  n->set_operand(dest);
  }
}

void HighLevelCodegen::visit_implicit_conversion(Node *n) {
  // TODO: implement
}

std::string HighLevelCodegen::next_label() {
  std::string label = ".L" + std::to_string(m_next_label_num++);
  return label;
}

// TODO: additional private member functions
