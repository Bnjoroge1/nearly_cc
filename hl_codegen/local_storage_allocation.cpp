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
#include "symtab.h"
#include "local_storage_allocation.h"
#include "type.h"
#include "ast.h"
#include "function.h"
#include "storage.h"
#include <unordered_set>

LocalStorageAllocation::LocalStorageAllocation()
  : m_function(nullptr)
  , m_storage_calc(StorageCalculator::STRUCT, 0)
  , m_total_local_storage(0U)
  , m_next_vreg(VREG_FIRST_LOCAL) {
}

LocalStorageAllocation::~LocalStorageAllocation() {
  m_address_taken_vars.clear();
}

void LocalStorageAllocation::allocate_storage(std::shared_ptr<Function> function) {
  m_function = function;
  
  
  // Reset state
  m_next_vreg = VREG_FIRST_LOCAL;
  m_total_local_storage = 0;
  m_storage_calc = StorageCalculator(StorageCalculator::STRUCT, 0);
  m_address_taken_vars.clear();

  // Get function's symbol table
  SymbolTable *symtab = function->get_symbol()->get_symtab();
  
  // First find arrays and address-taken vars
  for (auto i = symtab->cbegin(); i != symtab->cend(); ++i) {
    Symbol *sym = *i;
    if (sym->get_type()->is_array()) {
      // Calculate array size
     
      
      // Allocate memory at next available offset
      int offset = m_storage_calc.add_field(sym->get_type());
      sym->set_offset(offset);
    }
  }

  // Then find address-taken vars
  find_address_taken_vars(function->get_funcdef_ast());
  



  // Then visit AST to allocate storage
  visit(function->get_funcdef_ast());
  
  // Finally calculate total storage needed
  m_storage_calc.finish();
  m_total_local_storage = m_storage_calc.get_size();
  m_function->set_local_storage_size(m_total_local_storage);
   
  
}





void LocalStorageAllocation::visit_function_definition(Node *n) {
  
  // Get function's symbol table
  Symbol* fn_sym = m_function->get_symbol();
  SymbolTable* symtab = fn_sym->get_symtab();

  
  // Handle parameters (kid(2) is parameter list)
  Node *params = n->get_kid(2);

  for (unsigned i = 0; i < params->get_num_kids(); i++) {
    Node *param = params->get_kid(i);
    Node *declarator = param->get_kid(1);

    // Get parameter name by traversing the declarator tree
    std::string param_name;
    Node *name_node = nullptr;
    
    if (declarator->get_tag() == AST_POINTER_DECLARATOR) {
      // For pointer parameters, name is in the named declarator
      Node *base_declarator = declarator->get_kid(0);
      if (base_declarator && base_declarator->get_tag() == AST_NAMED_DECLARATOR) {
        name_node = base_declarator->get_kid(0);
      }
    } else if (declarator->get_tag() == AST_ARRAY_DECLARATOR) {
      // For array parameters, name is in the named declarator
      Node *base_declarator = declarator->get_kid(0);
      if (base_declarator && base_declarator->get_tag() == AST_NAMED_DECLARATOR) {
        name_node = base_declarator->get_kid(0);
      }
    } else if (declarator->get_tag() == AST_NAMED_DECLARATOR) {
      name_node = declarator->get_kid(0);
    }
    
    if (name_node) {
      param_name = name_node->get_str();
      Symbol *sym = symtab->lookup_local(param_name);

      if (sym) {
        // Both pointer and array parameters are handled as pointers
        if (sym->get_type()->is_pointer() || 
            sym->get_type()->is_array() || 
            declarator->get_tag() == AST_POINTER_DECLARATOR ||
            declarator->get_tag() == AST_ARRAY_DECLARATOR) {
          // Use mov_q for pointer/array parameters
          sym->set_vreg(m_next_vreg++);
        } else {
          // Regular parameter
          sym->set_vreg(m_next_vreg++);
        }
      } else {
      }
    } else {
    }
  }
  
  // Visit function body (kid(3) is statement list)
  Node* body = n->get_kid(3);
  if (!body) {
    return;
  }
  
 
  
  // Use visit_statement_list to process the body
  visit(body);
  
  // Record total local storage in Function object
  m_storage_calc.finish();
  m_total_local_storage = m_storage_calc.get_size();
  m_function->set_local_storage_size(m_total_local_storage);
  
  
}


void LocalStorageAllocation::visit_statement_list(Node *n) {
 
  // Visit all statements in the list
  for (auto i = n->cbegin(); i != n->cend(); ++i) {
    Node* stmt = *i;
    if (stmt->get_tag() == AST_VARIABLE_DECLARATION) {
      visit_variable_declaration(stmt);
    } else {
      visit(stmt);
    }
  }
}


void LocalStorageAllocation::visit_variable_declaration(Node *n) {
  
  Node* declarator_list = n->get_kid(2);
  if (!declarator_list) {
    return;
  }
  
  // Get the function's symbol table ONCE at the start
  SymbolTable* symtab = m_function->get_symbol()->get_symtab();
  if (!symtab) {
    return;
  }
  
  for (auto i = declarator_list->cbegin(); i != declarator_list->cend(); ++i) {
    Node* declarator = *i;
    if (!declarator) {
      continue;
    }

    // Find the actual named declarator if this is a pointer/array declarator
    Node* named_decl = declarator;
    Symbol* temp_sym = nullptr;
    
    if (declarator->get_tag() == AST_POINTER_DECLARATOR) {
      temp_sym = declarator->get_symbol();
    } else {
      while (named_decl && 
             (named_decl->get_tag() == AST_POINTER_DECLARATOR || 
              named_decl->get_tag() == AST_ARRAY_DECLARATOR)) {
        named_decl = named_decl->get_kid(0);
      }
      if (named_decl) {
        temp_sym = named_decl->get_symbol();
      }
    }

    if (!temp_sym) {
      continue;
    }

    // Get the consistent symbol from symbol table
    Symbol* sym = symtab->lookup_local(temp_sym->get_name());
    if (!sym) {
      continue;
    }
    
    
    
    std::shared_ptr<Type> type = sym->get_type();
    if (!type) {
      continue;
    }

   
    bool is_addr_taken = m_address_taken_vars.count(sym) > 0;

    // Determine storage location
    bool needs_memory = type->is_array() || 
                       type->is_struct() || 
                       is_addr_taken;
    
    
    if (needs_memory) {
    
      // Allocate in memory
      unsigned offset = m_storage_calc.add_field(type);
      sym->set_offset(offset);
      sym->set_vreg(-1);
    } else if (type->is_basic() || type->is_pointer()) {
      // Only allocate register if not address-taken
      sym->set_vreg(m_next_vreg++);
      sym->set_offset(-1);
    }
  }
}

void LocalStorageAllocation::visit_unary_expression(Node *n) {
  std::string op = n->get_kid(0)->get_str();
  
  if (op == "&") {
    Node *operand = n->get_kid(1);
    if (Symbol *sym = operand->get_symbol()) {
      // Variable should already be marked as address-taken from pre-pass
      assert(m_address_taken_vars.count(sym) > 0);
      // Make sure it has memory storage
      assert(sym->get_offset() >= 0);
    }
  }
  
  visit(n->get_kid(1));
}
void LocalStorageAllocation::find_address_taken_vars(Node *n) {
  if (!n) return;
  
  // Check for address-of operator
  if (n->get_tag() == AST_UNARY_EXPRESSION) {
    Node *op = n->get_kid(0);
    if (op && op->get_str() == "&") {
      Node *operand = n->get_kid(1);
      if (Symbol *sym = operand->get_symbol()) {
        m_address_taken_vars.insert(sym);
      }
    }
  }
  
  // Recursively process all children
  for (auto i = n->cbegin(); i != n->cend(); ++i) {
    find_address_taken_vars(*i);
  }
}