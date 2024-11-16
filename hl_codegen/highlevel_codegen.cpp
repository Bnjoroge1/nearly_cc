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
  


  get_hl_iseq()->append(new Instruction(HINS_enter, Operand(Operand::IMM_IVAL, total_local_storage)));
  // Handle parameters - copy from argument registers to allocated storage
  Node *params = n->get_kid(2);  // parameter list
  for (unsigned i = 0; i < params->get_num_kids(); i++) {
    Node *param = params->get_kid(i);
    Node *declarator = param->get_kid(1);
    
    // Get parameter name based on declarator type
    std::string param_name;
    if (declarator->get_tag() == AST_POINTER_DECLARATOR) {
      // For pointer parameters, get name from base declarator
      Node *base_declarator = declarator->get_kid(0);
      param_name = base_declarator->get_kid(0)->get_str();
    } else if (declarator->get_tag() == AST_ARRAY_DECLARATOR) {
      // For array parameters, get name from base declarator
      Node *base_declarator = declarator->get_kid(0);
      param_name = base_declarator->get_kid(0)->get_str();
    } else {
      // Regular parameter
      param_name = declarator->get_kid(0)->get_str();
    }
    
    // Look up parameter's symbol to get its allocated vreg
    Symbol *sym = m_function->get_symbol()->get_symtab()->lookup_local(param_name);
    if (sym) {
      // Choose mov instruction based on parameter type
      HighLevelOpcode mov_op;
      if (sym->get_type()->is_pointer() || 
          sym->get_type()->is_array() || 
          declarator->get_tag() == AST_POINTER_DECLARATOR ||
          declarator->get_tag() == AST_ARRAY_DECLARATOR) {
        mov_op = HINS_mov_q;  
      } else {
        mov_op = HINS_mov_l;  
      }

      // Generate move from argument register (vr1+i) to parameter's allocated storage
      get_hl_iseq()->append(new Instruction(mov_op, 
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

  if (n->get_num_kids() > 0) {
    Node *expr = n->get_kid(0);
    if (expr != nullptr) {
      // Visit the expression to generate its code and store result

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
    // Visit the return expression first
    visit(n->get_kid(0));
    printf("num kids: %d\n", n->get_num_kids());
    // Move result into vr0 (return value register)
    printf("n->get_kid(0)->get_operand(): %s\n", n->get_kid(0)->get_operand());
    get_hl_iseq()->append(new Instruction(HINS_mov_l,
        Operand(Operand::VREG, 0),  // vr0
        n->get_kid(0)->get_operand()));
        
    // Jump to return label
    get_hl_iseq()->append(new Instruction(HINS_jmp,
        Operand(Operand::LABEL, m_return_label_name)));
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



void HighLevelCodegen::visit_for_statement(Node *n) {
  // Create labels for loop start and condition
  std::string loop_start = next_label();  // Loop body
  std::string loop_cond = next_label();   // Condition check
  
  Node *init = n->get_kid(0);
  Node *cond = n->get_kid(1);
  Node *update = n->get_kid(2);
  Node *body = n->get_kid(3);
  printf("init: %s\n", init->get_str().c_str());
  printf("cond: %s\n", cond->get_str().c_str());
  printf("update: %s\n", update->get_str().c_str());
  printf("body: %s\n", body->get_str().c_str());
  
  // Generate initialization
  if (init) {
    visit(init);
  }
  
  // Jump to condition check first
  get_hl_iseq()->append(new Instruction(HINS_jmp, 
    Operand(Operand::LABEL, loop_cond)));
  
  // Loop body
  get_hl_iseq()->define_label(loop_start);
  
  if (body) {
    visit(body);
  }
  
  // Update
  if (update) {
    visit(update);
  }
  
  // Condition check
  get_hl_iseq()->define_label(loop_cond);
  if (cond) {
    visit(cond);
    std::cout << "condition: " << cond->get_str() << std::endl;
    Operand cond_result = cond->get_operand();
    if (cond_result.get_kind() != Operand::NONE) {
      get_hl_iseq()->append(new Instruction(HINS_cjmp_t,
        cond_result,
        Operand(Operand::LABEL, loop_start)));
    }
  }
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
    n->set_operand(Operand(Operand::VREG_MEM, ptr.get_base_reg()));
  }
  else if (op == "-") {
    // Handle unary minus
    visit(operand);
    
    // Create new vreg for result
    int result_vreg = m_next_vreg++;
    
    // Generate neg instruction
    HighLevelOpcode neg_opcode = get_opcode(HINS_neg_b, operand->get_type());
    get_hl_iseq()->append(new Instruction(neg_opcode,
      Operand(Operand::VREG, result_vreg),
      operand->get_operand()));
      
    n->set_operand(Operand(Operand::VREG, result_vreg));
  }
}

void HighLevelCodegen::visit_binary_expression(Node *n) {
  std::string op = n->get_kid(0)->get_str();
  printf("first kid for binary expression: %s\n", n->get_kid(0)->get_str().c_str());

  if (op == "=") {
    // Handle source assignment
    visit(n->get_kid(2));  
    Operand source = n->get_kid(2)->get_operand();
    
    // Visit left side (destination)
    visit(n->get_kid(1));
    Operand dest = n->get_kid(1)->get_operand();
    
    // Get types for pointer assignment checks
    Node* lhs = n->get_kid(1);
    Node* rhs = n->get_kid(2);
    HighLevelOpcode mov_op;
    if (lhs->get_type()->is_pointer() || rhs->get_type()->is_pointer() || 
        lhs->get_type()->is_array() || rhs->get_type()->is_array()) {
      mov_op = HINS_mov_q;  // Use 64-bit move for pointers/arrays
    } else {
      mov_op = HINS_mov_l;  // Use 32-bit move for integers
    }
    
    get_hl_iseq()->append(new Instruction(mov_op, dest, source));
    
    // Assignment expression's value is the assigned value
    n->set_operand(source);
  }
  else  if (op == "<") {
    // Visit operands
    visit(n->get_kid(1));  // i
    visit(n->get_kid(2));  // n
    
    Node* left_node = n->get_kid(1);
    Node* right_node = n->get_kid(2);
    Operand left = left_node->get_operand();
    Operand right = right_node->get_operand();
    
    // Create result vreg for comparison
    int result_vreg = m_next_vreg++;
    Operand result(Operand::VREG, result_vreg);
    
    // Generate less than comparison
    get_hl_iseq()->append(new Instruction(HINS_cmplt_l,
      result,
      left,
      right));
    
    // Set the result as this node's operand
    n->set_operand(result);
  }else if (op == ">") {
    // Visit operands
    visit(n->get_kid(1));  // left operand
    visit(n->get_kid(2));  // right operand
    
    Node* left_node = n->get_kid(1);
    Node* right_node = n->get_kid(2);
    Operand left = left_node->get_operand();
    Operand right = right_node->get_operand();
    
    // Create result vreg for comparison
    int result_vreg = m_next_vreg++;
    Operand result(Operand::VREG, result_vreg);
    
    // Generate greater than comparison
    get_hl_iseq()->append(new Instruction(HINS_cmpgt_l,
        result,
        left,
        right));
    
    // Set the result as this node's operand
    n->set_operand(result);
}
  else if (op == "+") {
    printf("visiting + expression\n");
    visit(n->get_kid(1));
    visit(n->get_kid(2));
    printf("left for +: %s\n", n->get_kid(1));
    printf("right for +: %s\n", n->get_kid(2)->get_str().c_str());
    
    Node* left_node = n->get_kid(1);
    Node* right_node = n->get_kid(2);
    printf("left node kind: %d\n", left_node->get_tag());
    printf("right node kind: %d\n", right_node->get_tag());
    // Make sure operands are valid before using them
    
    Operand left = left_node->get_operand();
    Operand right = right_node->get_operand();
    
    printf("left op for +: %s\n", left_node->get_str().c_str());
    printf("right op for +: %s\n", right_node->get_str().c_str());
    
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
    Operand left_val = left;
    Operand right_val = right;
    
    // Load memory operands into registers if needed
    if (right.get_kind() == Operand::VREG_MEM) {
        int temp_vreg = m_next_vreg++;
        get_hl_iseq()->append(new Instruction(HINS_mov_l,
            Operand(Operand::VREG, temp_vreg), right));
        right_val = Operand(Operand::VREG, temp_vreg);
    }
    
    if (left.get_kind() == Operand::VREG_MEM) {
        int temp_vreg = m_next_vreg++;
        get_hl_iseq()->append(new Instruction(HINS_mov_l,
            Operand(Operand::VREG, temp_vreg), left));
        left_val = Operand(Operand::VREG, temp_vreg);
    }
    
    
    // Now do addition with register operands
    get_hl_iseq()->append(new Instruction(HINS_add_l, result, left_val, right_val));
    n->set_operand(result);
  }
  }else if (op == "*") {
    printf("visiting * expression\n");
    visit(n->get_kid(1));
    visit(n->get_kid(2));
    
    Node* left_node = n->get_kid(1);
    Node* right_node = n->get_kid(2);
    
    Operand left = left_node->get_operand();
    Operand right = right_node->get_operand();
    
    // Create result vreg
    int result_vreg = m_next_vreg++;
    Operand result(Operand::VREG, result_vreg);
    
    // Handle memory operands
    Operand left_val = left;
    Operand right_val = right;
    
    if (right.get_kind() == Operand::VREG_MEM) {
        int temp_vreg = m_next_vreg++;
        get_hl_iseq()->append(new Instruction(HINS_mov_l,
            Operand(Operand::VREG, temp_vreg), right));
        right_val = Operand(Operand::VREG, temp_vreg);
    }
    
    if (left.get_kind() == Operand::VREG_MEM) {
        int temp_vreg = m_next_vreg++;
        get_hl_iseq()->append(new Instruction(HINS_mov_l,
            Operand(Operand::VREG, temp_vreg), left));
        left_val = Operand(Operand::VREG, temp_vreg);
    }
    
    // Perform multiplication
    get_hl_iseq()->append(new Instruction(HINS_mul_l, result, left_val, right_val));
    n->set_operand(result);
}
  else if (op == "-") {
      visit(n->get_kid(1));
      visit(n->get_kid(2));
     
      
      Node* left_node = n->get_kid(1);
      Node* right_node = n->get_kid(2);
      Operand left = left_node->get_operand();
      Operand right = right_node->get_operand();
      printf("left for -: %s\n", left_node->get_str().c_str());
      printf("right for -: %s\n", right_node->get_str().c_str());
      bool left_is_ptr = left_node->get_type()->is_pointer();
      bool right_is_ptr = right_node->get_type()->is_pointer();
      
      int result_vreg = m_next_vreg++;
      Operand result(Operand::VREG, result_vreg);
      
      if (left_is_ptr || right_is_ptr) {
          // Handle pointer arithmetic
          const int PTR_SCALE = 4;  // Size of int/pointer
          
          if (left_is_ptr && right_is_ptr) {
              // pointer - pointer case: divide by scale
              get_hl_iseq()->append(new Instruction(HINS_sub_q, result, left, right));
              int scaled_vreg = m_next_vreg++;
              get_hl_iseq()->append(new Instruction(HINS_div_q, 
                  Operand(Operand::VREG, scaled_vreg),
                  result,
                  Operand(Operand::IMM_IVAL, PTR_SCALE)));
              result = Operand(Operand::VREG, scaled_vreg);
          } else if (left_is_ptr) {
              // pointer - int case: scale the int
              int scaled_vreg = m_next_vreg++;
              get_hl_iseq()->append(new Instruction(HINS_mul_q,
                  Operand(Operand::VREG, scaled_vreg),
                  right,
                  Operand(Operand::IMM_IVAL, PTR_SCALE)));
              get_hl_iseq()->append(new Instruction(HINS_sub_q, result, left,
                  Operand(Operand::VREG, scaled_vreg)));
          }
      } else {
          printf("regular integer subtraction\n");
          // Regular integer subtraction
          get_hl_iseq()->append(new Instruction(HINS_sub_l, result, left, right));
      }
      printf("setting result for -\n");
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
  printf("visiting function call expression\n");
  // Get function name from variable reference
  Node *func_node = n->get_kid(0);
  std::string func_name;
  if (func_node->get_tag() == AST_VARIABLE_REF) {
    func_name = func_node->get_kid(0)->get_str();
  } else {
    func_name = func_node->get_str();
  }
  printf("func_name: %s\n", func_name.c_str());
  printf("visiting arguments\n");
  printf("n->get_num_kids(): %d\n", n->get_num_kids());
  

  // Process arguments
// Process arguments
Node *args = n->get_kid(1);
for (unsigned i = 0; i < args->get_num_kids(); i++) {
    Node *arg = args->get_kid(i);
    visit(arg);  // Generate code for argument expression
    
    // Look through any implicit conversions
    Node *base_arg = arg;
    while (base_arg->get_tag() == AST_IMPLICIT_CONVERSION) {
        base_arg = base_arg->get_kid(0);
    }
    
    // Now check if the base argument is an array
    if (base_arg->get_tag() == AST_VARIABLE_REF) {
        Symbol *sym = base_arg->get_symbol();
        if (sym && sym->is_local() && sym->get_type()->is_array()) {
            // Generate array address for argument
            int addr_vreg = m_next_vreg++;
            get_hl_iseq()->append(new Instruction(HINS_localaddr,
                Operand(Operand::VREG, addr_vreg),
                Operand(Operand::IMM_IVAL, sym->get_offset())));
                
            // Move array address to argument register
            get_hl_iseq()->append(new Instruction(HINS_mov_q,
                Operand(Operand::VREG, LocalStorageAllocation::VREG_FIRST_ARG + i),
                Operand(Operand::VREG, addr_vreg)));
            continue;
        }
    }
    
    // Non-array argument
    get_hl_iseq()->append(new Instruction(HINS_mov_l,
        Operand(Operand::VREG, LocalStorageAllocation::VREG_FIRST_ARG + i),
        arg->get_operand()));
}
// Generate the call instruction
get_hl_iseq()->append(new Instruction(HINS_call,
    Operand(Operand::LABEL, func_name)));

 
// Only move return value to new vreg if this function's result will be used
    //For functions that return values, store in new vreg
    if (n->get_type() && !n->get_type()->is_void()) {
        int result_vreg = m_next_vreg++;
        get_hl_iseq()->append(new Instruction(HINS_mov_l,
            Operand(Operand::VREG, result_vreg),
            Operand(Operand::VREG, LocalStorageAllocation::VREG_RETVAL)));
        n->set_operand(Operand(Operand::VREG, result_vreg));
    }else {
      n->set_operand(Operand(Operand::VREG, 0));
    }
    }


void HighLevelCodegen::visit_field_ref_expression(Node *n) {
  // Visit the struct variable reference
  Node *struct_ref = n->get_kid(0);
  visit(struct_ref);
  
  // Get field name and struct type
  std::string field_name = n->get_kid(1)->get_str();
  std::shared_ptr<Type> struct_type = struct_ref->get_type();
  
  // Get field offset within struct
  int field_offset = struct_type->get_field_offset(field_name);
  
  // Get base address of struct
  Operand struct_loc = struct_ref->get_operand();
  
  if (struct_loc.get_kind() == Operand::VREG_MEM) {
    // If struct is already a memory reference, use its base register
    int base_vreg = struct_loc.get_base_reg();
    
    // Add field offset to create new memory reference
    n->set_operand(Operand(Operand::VREG_MEM, base_vreg, field_offset));
  } else {
    // For struct variables in memory, compute field address
    int addr_vreg = m_next_vreg++;
    
    // Add field offset to struct base address
    get_hl_iseq()->append(new Instruction(HINS_add_q,
      Operand(Operand::VREG, addr_vreg),
      struct_loc,
      Operand(Operand::IMM_IVAL, field_offset)));
      
    n->set_operand(Operand(Operand::VREG_MEM, addr_vreg));
  }
}

void HighLevelCodegen::visit_indirect_field_ref_expression(Node *n) {
  // Visit the pointer expression
  Node *ptr_expr = n->get_kid(0);
  visit(ptr_expr);
  
  // Get field name and pointed-to struct type
  std::string field_name = n->get_kid(1)->get_str();
  std::shared_ptr<Type> struct_type = ptr_expr->get_type()->get_base_type();
  
  // Get field offset within struct
  int field_offset = struct_type->get_field_offset(field_name);
  
  // Get pointer value
  Operand ptr = ptr_expr->get_operand();
  
  if (field_offset == 0) {
    // If field is at start of struct, use pointer directly
    n->set_operand(Operand(Operand::VREG_MEM, ptr.get_base_reg()));
  } else {
    // Add field offset to pointer
    int addr_vreg = m_next_vreg++;
    get_hl_iseq()->append(new Instruction(HINS_add_q,
      Operand(Operand::VREG, addr_vreg),
      ptr,
      Operand(Operand::IMM_IVAL, field_offset)));
      
    n->set_operand(Operand(Operand::VREG_MEM, addr_vreg));
  }
}
void HighLevelCodegen::visit_array_element_ref_expression(Node *n) {
    printf("visiting array element ref expression\n");
    
    // Visit array base expression
    Node *array = n->get_kid(0);
    visit(array);
    printf("array: %s\n", array->get_str().c_str());
    
    // Visit index expression
    Node *index = n->get_kid(1);
    visit(index);
    printf("index: %s\n", index->get_str().c_str());
    
    Operand index_op = index->get_operand();
    printf("Index operand kind: %d\n", index_op.get_kind());
    
    // Get array base address
    Operand array_base = array->get_operand();
    printf("Array base operand kind: %d\n", array_base.get_kind());

    if (array_base.get_kind() == Operand::VREG_MEM) {
        //  VREG_MEM_OFF with an offset of 0 instead of VREG_MEM
        n->set_operand(Operand(Operand::VREG_MEM_OFF, array_base.get_base_reg(), 0));
        array_base = n->get_operand();
    } else if (array_base.get_kind() == Operand::VREG) {
        // If it's a VREG, convert it to VREG_MEM_OFF with offset 0
        n->set_operand(Operand(Operand::VREG_MEM_OFF, array_base.get_base_reg(), 0));
        array_base = n->get_operand();
    }

    // Convert index to 64-bit for address calculation
    int idx_vreg_64 = next_temp_vreg();  
    get_hl_iseq()->append(new Instruction(HINS_sconv_lq,
        Operand(Operand::VREG, idx_vreg_64),
        index_op));
    
    // Get element size from array's type
    std::shared_ptr<Type> array_type = array->get_type();
    int element_size = array_type->get_base_type()->get_storage_size();
    
    // Multiply index by element size
    int offset_vreg = next_temp_vreg();  
    get_hl_iseq()->append(new Instruction(HINS_mul_q,
        Operand(Operand::VREG, offset_vreg),
        Operand(Operand::VREG, idx_vreg_64),
        Operand(Operand::IMM_IVAL, element_size)));
    
    // Add offset to array base address
    int addr_vreg = next_temp_vreg();  
    get_hl_iseq()->append(new Instruction(HINS_add_q,
        Operand(Operand::VREG, addr_vreg),
        array_base,
        Operand(Operand::VREG, offset_vreg)));
    
    // Set as memory reference using VREG_MEM_OFF with offset 0
    n->set_operand(Operand(Operand::VREG_MEM_OFF, addr_vreg, 0));
}
void HighLevelCodegen::visit_variable_ref(Node *n) {
  Symbol *sym = n->get_symbol();
  if (!sym) {
    return;
  }
  
  
  // Get the function's symbol table
  SymbolTable* symtab = m_function->get_symbol()->get_symtab();
  Symbol* stored_sym = symtab->lookup_recursive(sym->get_name());
  
  if (stored_sym) {
    int vreg = stored_sym->get_vreg();
    int offset = stored_sym->get_offset();
        
    if (vreg >= 0) {
      // Variable in register
      n->set_operand(Operand(Operand::VREG, vreg));
    } else if (offset >= 0) {
      // Variable in memory - need to get its address
      int addr_vreg = m_next_vreg++;
      get_hl_iseq()->append(new Instruction(HINS_localaddr, 
                           Operand(Operand::VREG, addr_vreg),
                           Operand(Operand::IMM_IVAL, offset)));
      
      // For address-taken variables, return the address
      if (stored_sym->get_type()->is_array() || stored_sym->get_type()->is_struct() ) {
        n->set_operand(Operand(Operand::VREG, addr_vreg));
      } else {
        // For regular variables in memory, return a memory reference
        n->set_operand(Operand(Operand::VREG_MEM, addr_vreg));
      }
    }
  }
}

void HighLevelCodegen::visit_literal_value(Node *n) {
  
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
    get_hl_iseq()->append(new Instruction(HINS_mov_l, dest, 
                        Operand(Operand::IMM_IVAL, val.get_int_value())));
   n->set_operand(dest);
  }
}

void HighLevelCodegen::visit_implicit_conversion(Node *n) {
    // Visit the value being converted first
    visit(n->get_kid(0));
    
    // Get source and destination types
    std::shared_ptr<Type> source_type = n->get_kid(0)->get_type();
    std::shared_ptr<Type> dest_type = n->get_type();
    
    // If no operand or types are same, propagate operand
    if (!n->get_kid(0)->has_operand() || source_type->is_same(dest_type.get())) {
        n->set_operand(n->get_kid(0)->get_operand());
        return;
    }
    
    // Handle integer conversions
    if (source_type->is_integral() && dest_type->is_integral()) {
        int dest_vreg = m_next_vreg++;
        
        // Choose appropriate conversion instruction
        HighLevelOpcode conv_op;
        if (source_type->get_basic_type_kind() == BasicTypeKind::INT && 
            dest_type->get_basic_type_kind() == BasicTypeKind::LONG) {
            conv_op = HINS_sconv_lq;  // signed int to long
        }
        // Add other conversions as needed
        
        get_hl_iseq()->append(new Instruction(conv_op,
            Operand(Operand::VREG, dest_vreg),
            n->get_kid(0)->get_operand()));
            
        n->set_operand(Operand(Operand::VREG, dest_vreg));
    }
}

void HighLevelCodegen::visit_do_while_statement(Node *n) {
    // Create labels for loop start
    std::string start_label = next_label();
    
    // Emit label for loop start
    get_hl_iseq()->define_label(start_label);
    
    // Visit loop body
    visit(n->get_kid(0));
    
    // Visit condition
    visit(n->get_kid(1));
    
    // Get condition result
    Operand cond = n->get_kid(1)->get_operand();
    
    // Conditional jump back to start if condition is true
    get_hl_iseq()->append(new Instruction(HINS_cjmp_t,
        cond,
        Operand(Operand::LABEL, start_label)));
}

std::string HighLevelCodegen::next_label() {
  std::string label = ".L" + std::to_string(m_next_label_num++);
  return label;
}

