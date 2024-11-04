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

HighLevelCodegen::HighLevelCodegen(const Options &options, int next_label_num, int next_vreg)
  : m_options(options)
  , m_next_vreg(next_vreg)
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

  total_local_storage = m_function->get_local_storage_size();
  printf("DEBUG: Total local storage: %u\n", total_local_storage);
  


  get_hl_iseq()->append(new Instruction(HINS_enter, Operand(Operand::IMM_IVAL, total_local_storage)));
  // Handle parameters - copy from argument registers to allocated storage
  Node *params = n->get_kid(2);  // parameter list
  for (unsigned i = 0; i < params->get_num_kids(); i++) {
    Node *param = params->get_kid(i);
    Node *declarator = param->get_kid(1);
    Node *ident = declarator->get_kid(0);
    std::string param_name = ident->get_str();
    
    // Look up parameter's symbol to get its allocated vreg
    Symbol *sym = m_function->get_symbol()->get_symtab()->lookup_local(param_name);
    if (sym) {
      // Generate move from argument register (vr1+i) to parameter's allocated storage
      get_hl_iseq()->append(new Instruction(HINS_mov_l, 
        Operand(Operand::VREG, sym->get_vreg()),           // destination: parameter's vreg
        Operand(Operand::VREG, LocalStorageAllocation::VREG_FIRST_ARG + i)  // source: argument register
      ));
    }
  }
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
  // Generate unique labels for loop start and end
  std::string start_label = next_label();
  std::string body_label = next_label();
  
  // Jump to condition check
  get_hl_iseq()->append(new Instruction(HINS_jmp, Operand(Operand::LABEL, start_label)));
  
  // Define start of loop body
  get_hl_iseq()->define_label(body_label);
  
  // Generate code for loop body
  visit(n->get_kid(1));
  
  // Define label for condition check
  get_hl_iseq()->define_label(start_label);
  
  // Generate code for condition
  visit(n->get_kid(0));
  Operand cond = n->get_kid(0)->get_operand();
  
  
  
  
  
  get_hl_iseq()->append(new Instruction(HINS_cjmp_t, n->get_kid(0)->get_operand(), 
                                      Operand(Operand::LABEL, body_label)));
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


void HighLevelCodegen::visit_unary_expression(Node *n) {
  std::string op = n->get_kid(0)->get_str();
  Node *operand = n->get_kid(1);
  
  if (op == "&") {
    // Handle address-of operator
    visit(operand);
    
   
    // Get operand's storage location
    Operand operand_loc = operand->get_operand();
    
    
      
    // If it's a memory reference, convert it to the register containing the address
    if (operand_loc.get_kind() == Operand::VREG_MEM) {
        n->set_operand(Operand(Operand::VREG, operand_loc.get_base_reg()));
    } else {
        n->set_operand(operand_loc);
    }
  }
  else if (op == "*") {
    // Handle pointer dereference
    visit(operand);
    
    // Get pointer value
    Operand ptr = operand->get_operand();
    
    // Create memory reference using pointer value
    // Use VREG_MEM kind for memory reference through pointer
    n->set_operand(Operand(Operand::VREG_MEM, ptr.get_base_reg()));
  }
  else if (op == "-") {
    // Handle unary minus
    visit(operand);
    
    // Create new vreg for result
    int result_vreg = m_next_vreg++;
    printf("DEBUG: Allocated vreg %d for negation result\n", result_vreg);
    
    // Generate neg instruction
    // Use get_opcode helper to get correct opcode for operand type
    HighLevelOpcode neg_opcode = get_opcode(HINS_neg_b, operand->get_type());
    get_hl_iseq()->append(new Instruction(neg_opcode,
      Operand(Operand::VREG, result_vreg),
      operand->get_operand()));
      
    n->set_operand(Operand(Operand::VREG, result_vreg));
  }
}

void HighLevelCodegen::visit_binary_expression(Node *n) {
  printf("DEBUG: Entering binary_expression\n");
  std::string op = n->get_kid(0)->get_str();
  printf("DEBUG: Binary expression operator: %s\n", op.c_str());

  if (op == "=") {
    // Handle assignment
    visit(n->get_kid(2));  // Visit right side first
    Operand source = n->get_kid(2)->get_operand();
    
    // Visit left side (variable)
    visit(n->get_kid(1));
    Operand dest = n->get_kid(1)->get_operand();
    
    // Get types for pointer assignment checks
    Node* lhs = n->get_kid(1);
    Node* rhs = n->get_kid(2);
    bool lhs_is_ptr = lhs->get_type()->is_pointer();
    bool rhs_is_ptr = rhs->get_type()->is_pointer();
    
    // Choose correct move instruction based on type
    HighLevelOpcode mov_op;
    if (lhs_is_ptr || rhs_is_ptr) {
      // Use 64-bit move for pointers
      mov_op = HINS_mov_q;
    } else {
      // Use regular move for other types
      mov_op = HINS_mov_l;
    }
    
    printf("DEBUG: Moving value from source to dest\n");
    get_hl_iseq()->append(new Instruction(mov_op, dest, source));
    printf("DEBUG: Move instruction generated: dest: %s, source: %s\n", 
           dest, source);
    
    // Assignment expression's value is the assigned value
    n->set_operand(source);
  }
  else if (op == "+") {
    printf("DEBUG: Current register: %d\n", m_next_vreg);
    visit(n->get_kid(1));
    visit(n->get_kid(2));
    
    Node* left_node = n->get_kid(1);
    Node* right_node = n->get_kid(2);
    Operand left = left_node->get_operand();
    Operand right = right_node->get_operand();
    
    printf("DEBUG: Left operand: %s\n", left);
    printf("DEBUG: Right operand: %s\n", right);
    
    bool left_is_ptr = left_node->get_type()->is_pointer();
    bool right_is_ptr = right_node->get_type()->is_pointer();
    
    int result_vreg = m_next_vreg++;
    Operand result(Operand::VREG, result_vreg);
    
    if (left_is_ptr || right_is_ptr) {
      // Handle pointer arithmetic
      const int PTR_SCALE = 4;  // Size of int/pointer
      
      int scaled_vreg = m_next_vreg++;
      Operand scaled_index(Operand::VREG, scaled_vreg);
      
      if (left_is_ptr) {
        // pointer + int case
        get_hl_iseq()->append(new Instruction(HINS_mul_q, scaled_index, right, 
                            Operand(Operand::IMM_IVAL, PTR_SCALE)));
        get_hl_iseq()->append(new Instruction(HINS_add_q, result, left, scaled_index));
      } else {
        // int + pointer case
        get_hl_iseq()->append(new Instruction(HINS_mul_q, scaled_index, left, 
                            Operand(Operand::IMM_IVAL, PTR_SCALE)));
        get_hl_iseq()->append(new Instruction(HINS_add_q, result, scaled_index, right));
      }
    } else {
      // Regular integer addition
      get_hl_iseq()->append(new Instruction(HINS_add_l, result, left, right));
    }
    
    n->set_operand(result);
  }
  else if (op == "<=") {
    visit(n->get_kid(1));
    visit(n->get_kid(2));
    
    Node* left_node = n->get_kid(1);
    Node* right_node = n->get_kid(2);
    Operand left = left_node->get_operand();
    Operand right = right_node->get_operand();
    
    bool left_is_ptr = left_node->get_type()->is_pointer();
    bool right_is_ptr = right_node->get_type()->is_pointer();
    
    int result_vreg = m_next_vreg++;
    Operand result(Operand::VREG, result_vreg);
    
    if (left_is_ptr && right_is_ptr) {
      // Pointer comparison - use 64-bit comparison
      get_hl_iseq()->append(new Instruction(HINS_cmplte_q, result, left, right));
    } else {
      // Regular integer comparison
      get_hl_iseq()->append(new Instruction(HINS_cmplte_l, result, left, right));
    }
    
    n->set_operand(result);
  }
}

void HighLevelCodegen::visit_function_call_expression(Node *n) {
  // Get function name and args
  Node *fn_name = n->get_kid(0);
  Node *args = n->get_kid(1);
  std::string fn_name_str = fn_name->get_kid(0)->get_str();
  printf("DEBUG: Generating call to function: %s\n", fn_name_str.c_str());

  
  // Visit and evaluate each argument
  std::vector<Operand> arg_operands;
  for (unsigned i = 0; i < args->get_num_kids(); i++) {
    Node *arg = args->get_kid(i);
    visit(arg);
    arg_operands.push_back(arg->get_operand());
  }
  
  // Move arguments into argument registers (vr1-vr9)
  for (unsigned i = 0; i < arg_operands.size(); i++) {
    int arg_vreg = LocalStorageAllocation::VREG_FIRST_ARG + i;
    get_hl_iseq()->append(new Instruction(HINS_mov_l, 
                                        Operand(Operand::VREG, arg_vreg),
                                        arg_operands[i]));
  }
  
  // Call function
  get_hl_iseq()->append(new Instruction(HINS_call, 
                                      Operand(Operand::LABEL, fn_name_str)));
  
  // Result will be in vr0
  n->set_operand(Operand(Operand::VREG, LocalStorageAllocation::VREG_RETVAL));
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
  Symbol* stored_sym = symtab->lookup_recursive(sym->get_name());
  
  if (stored_sym) {
    int vreg = stored_sym->get_vreg();
    int offset = stored_sym->get_offset();
        
    printf("DEBUG: Variable %s: vreg=%d, offset=%d\n", 
           stored_sym->get_name().c_str(), vreg, offset);
    
    if (vreg >= 0) {
      // Variable in register
      n->set_operand(Operand(Operand::VREG, vreg));
      printf("DEBUG: Set operand to vreg %d\n", vreg);
    } else if (offset >= 0) {
      // Variable in memory - need to get its address
      int addr_vreg = m_next_vreg++;
      get_hl_iseq()->append(new Instruction(HINS_localaddr, 
                           Operand(Operand::VREG, addr_vreg),
                           Operand(Operand::IMM_IVAL, offset)));
      printf("DEBUG: Generated localaddr: vreg=%d, offset=%d\n", addr_vreg, offset);
      
      // For address-taken variables, return the address
      if (stored_sym->get_type()->is_array() || stored_sym->get_type()->is_struct() ) {
        n->set_operand(Operand(Operand::VREG, addr_vreg));
      } else {
        // For regular variables in memory, return a memory reference
        printf("DEBUG: Variable is in memory, returning memory reference to vreg=%d\n", 
               addr_vreg);
        n->set_operand(Operand(Operand::VREG_MEM, addr_vreg));
      }
    }
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
  printf("DEBUG: Literal token: %s\n", literal_token->get_str().c_str());
  printf("DEBUG: current register: %d\n", m_next_vreg);
  if (literal_token) {
    int val_vreg = m_next_vreg++;
    printf("DEBUG: New register: %d\n", val_vreg);
    Operand dest(Operand::VREG, val_vreg);
    // Create a new LiteralValue from the token's value
    LiteralValue val = LiteralValue::from_int_literal(literal_token->get_str(), literal_token->get_loc());
    
    // Set the immediate value operand directly
    get_hl_iseq()->append(new Instruction(HINS_mov_l, dest, 
                        Operand(Operand::IMM_IVAL, val.get_int_value())));
    printf("DEBUG: Move instruction generated: dest: %s, source: %s\n", 
           dest, Operand(Operand::IMM_IVAL, val.get_int_value()));
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
