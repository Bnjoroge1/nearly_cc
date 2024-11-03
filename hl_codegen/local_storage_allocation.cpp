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
  
  // Now safe to clear address taken vars since m_function is valid
  m_address_taken_vars.clear();
  
  // Visit AST to allocate storage for parameters and local variables
  visit(function->get_funcdef_ast());
  printf("DEBUG: Storage allocation complete\n");
  printf("DEBUG: Next vreg: %d\n", m_next_vreg);
  printf("DEBUG: Total local storage: %u\n", m_total_local_storage);
  
  // Finish storage calculation and get final size with padding
  m_storage_calc.finish();
  m_total_local_storage = m_storage_calc.get_size();
  m_function->set_local_storage_size(m_total_local_storage);
}

void LocalStorageAllocation::visit_unary_expression(Node *n) {
  // Check for address-of operator
  if (n->get_tag() == AST_UNARY_EXPRESSION && n->get_str() == "&") {
    Node* operand = n->get_kid(0);
    if (Symbol* sym = operand->get_symbol()) {
      m_address_taken_vars.insert(sym);
    }
  }
  visit(n->get_kid(0));
}
void LocalStorageAllocation::visit_function_definition(Node *n) {
  printf("DEBUG: Visiting function definition\n");
  
  // Initialize next_vreg to first local register
  m_next_vreg = VREG_FIRST_LOCAL;
  
  // Handle parameters (kid(2) is parameter list)
  Node *params = n->get_kid(2);
  printf("DEBUG: Processing parameters\n");
  visit(params);  // This will handle parameter allocation
  
  // Visit function body (kid(3) is statement list)
  printf("DEBUG: Processing function body\n");
  Node* body = n->get_kid(3);
  if (!body) {
    printf("DEBUG: Function body is null!\n");
    return;
  }
  
  printf("DEBUG: Function body tag: %d\n", body->get_tag());
  printf("DEBUG: Number of statements: %d\n", body->get_num_kids());
  
  // Use visit_statement_list to process the body
  visit(body);
  
  // Record total local storage in Function object
  m_storage_calc.finish();
  m_total_local_storage = m_storage_calc.get_size();
  m_function->set_local_storage_size(m_total_local_storage);
  
  printf("DEBUG: Function processing complete\n");
  printf("DEBUG: Total local storage: %u bytes\n", m_total_local_storage);
  printf("DEBUG: Next available vreg: %d\n", m_next_vreg);
}
void LocalStorageAllocation::visit_statement_list(Node *n) {
  printf("DEBUG: Visiting statement list\n");
  printf("DEBUG: Statement list: %s\n", n->get_str().c_str());
  printf("DEBUG: NUMBER OF STATEMENTS: %d\n", n->get_num_kids());
  // Visit all statements in the list
  for (auto i = n->cbegin(); i != n->cend(); ++i) {
    Node* stmt = *i;
    printf("DEBUG: Statement tag: %d\n", stmt->get_tag());
    if (stmt->get_tag() == AST_VARIABLE_DECLARATION) {
      printf("DEBUG: Found variable declaration\n");
      visit_variable_declaration(stmt);
    } else {
      visit(stmt);
    }
  }
}

void LocalStorageAllocation::visit_variable_declaration(Node *n) {
  printf("DEBUG: Entering variable_declaration\n");
  
  // We need to get the declarator node which contains the symbol
  Node* declarator_list = n->get_kid(2);  // AST structure shows declarator list is kid(2)
  if (!declarator_list) {
    printf("DEBUG: No declarator list found\n");
    return;
  }
  
  // Get the first (and in this case only) declarator
  Node* declarator = declarator_list->get_kid(0);
  if (!declarator) {
    printf("DEBUG: No declarator found\n");
    return;
  }
  
  Symbol *sym = declarator->get_symbol();
  if (!sym) {
    printf("DEBUG: No symbol found in declarator\n");
    return;
  }
  
  printf("DEBUG: Processing variable: %s\n", sym->get_name().c_str());
  std::shared_ptr<Type> type = sym->get_type();
  if (!type) {
    printf("DEBUG: No type found\n");
    return;
  }
  

  
  bool needs_memory = type->is_array() || 
                     type->is_struct() || 
                     m_address_taken_vars.count(sym) > 0;
  
  if (needs_memory) {
    unsigned offset = m_storage_calc.add_field(type);
    sym->set_offset(offset);
    sym->set_vreg(-1);
    printf("DEBUG: Variable %s allocated in memory at offset %d\n", 
           sym->get_name().c_str(), offset);
  } else {
    sym->set_vreg(m_next_vreg++);
    sym->set_offset(-1);
    printf("DEBUG: Variable %s allocated to vreg %d\n", 
           sym->get_name().c_str(), sym->get_vreg());
    printf("DEBUG: After allocation - vreg: %d, offset: %d\n",
         sym->get_vreg(), sym->get_offset());
  }
  
  SymbolTable* symtab = m_function->get_symbol()->get_symtab();
  printf("DEBUG: symbol table: %p\n", (void*)symtab);


  Symbol* existing = symtab->lookup_local(sym->get_name());
  if (existing) {
    existing->set_vreg(sym->get_vreg());
    existing->set_offset(sym->get_offset());
  } else {
    // Add new symbol if it doesn't exist
    Symbol* new_sym = symtab->add_entry(Location(), SymbolKind::VARIABLE, 
                                      sym->get_name(), sym->get_type());
    new_sym->set_vreg(sym->get_vreg());
    new_sym->set_offset(sym->get_offset());
  }

  printf("DEBUG: variable declaration Symbol address: %p\n", (void*)sym);
}
