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
  printf("DEBUG: Starting storage allocation for function %s\n", 
         function->get_name().c_str());
  
  // Reset state
  m_next_vreg = VREG_FIRST_LOCAL;
  m_total_local_storage = 0;
  m_storage_calc = StorageCalculator(StorageCalculator::STRUCT, 0);
  m_address_taken_vars.clear();

  // First find address-taken vars
  find_address_taken_vars(function->get_funcdef_ast());
  printf("DEBUG: Pre-pass complete, found %zu address-taken variables\n", 
         m_address_taken_vars.size());

  // Then visit AST to allocate storage
  visit(function->get_funcdef_ast());
  
  // Finally calculate total storage needed
  m_storage_calc.finish();
  m_total_local_storage = m_storage_calc.get_size();
  m_function->set_local_storage_size(m_total_local_storage);
   printf("DEBUG: Storage allocation complete:\n");
  printf("  - Total local storage: %u bytes\n", m_total_local_storage);
  printf("  - Next available vreg: %d\n", m_next_vreg);
  printf("  - Address-taken vars:\n");
  for (Symbol* sym : m_address_taken_vars) {
    printf("    * %s (offset=%d)\n", sym->get_name().c_str(), sym->get_offset());
  }
}





void LocalStorageAllocation::visit_function_definition(Node *n) {
  printf("DEBUG: Visiting function definition\n");
  
  // Get function's symbol table
  Symbol* fn_sym = m_function->get_symbol();
  SymbolTable* symtab = fn_sym->get_symtab();
  printf("DEBUG: Symbol table for function %s: %p\n", fn_sym->get_name().c_str(), (void*)symtab);

  printf("DEBUG: Next vreg: %d\n", m_next_vreg);
  
  // Handle parameters (kid(2) is parameter list)
  Node *params = n->get_kid(2);
  printf("DEBUG: Processing %d parameters\n", params->get_num_kids());
   
  // Allocate registers for parameters
  for (unsigned i = 0; i < params->get_num_kids(); i++) {
    Node *param = params->get_kid(i);
    Node *declarator = param->get_kid(1);   //NAMED declarator
    printf("DEBUG: param: %p\n", (void*)param);
    printf("DEBUG: declarator: %p\n", (void*)declarator);
    // Get parameter name FIRST
    Node *ident = declarator->get_kid(0);   // Get the TOK_IDENT child
    std::string param_name = ident->get_str();  // Now this will get 'a' or 'b'
    printf("DEBUG: Processing parameter %s\n", param_name.c_str());
    
    // THEN look up in symbol table
    Symbol *sym = symtab->lookup_local(param_name);
    printf("DEBUG: Found symbol for %s: %p\n", param_name.c_str(), (void*)sym);

    if (sym) {
      // Allocate a vreg for this parameter
      sym->set_vreg(m_next_vreg++);
      printf("DEBUG: Parameter %s allocated to vreg %d\n", 
             sym->get_name().c_str(), sym->get_vreg());
    } else {
      printf("DEBUG: ERROR - Symbol not found for parameter %s\n", param_name.c_str());
    }
  }
  
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
  printf("DEBUG: Entering visit_variable_declaration\n");
  
  Node* declarator_list = n->get_kid(2);
  if (!declarator_list) {
    printf("DEBUG: No declarator list found\n");
    return;
  }
  
  // Get the function's symbol table ONCE at the start
  SymbolTable* symtab = m_function->get_symbol()->get_symtab();
  if (!symtab) {
    printf("DEBUG: No symbol table found\n");
    return;
  }
  
  for (auto i = declarator_list->cbegin(); i != declarator_list->cend(); ++i) {
    Node* declarator = *i;
    if (!declarator) {
      printf("DEBUG: No declarator found\n");
      continue;
    }

    // Find the actual named declarator if this is a pointer/array declarator
    Node* named_decl = declarator;
    Symbol* temp_sym = nullptr;
    
    if (declarator->get_tag() == AST_POINTER_DECLARATOR) {
      printf("DEBUG: Found pointer declarator\n");
      temp_sym = declarator->get_symbol();
    } else {
      while (named_decl && 
             (named_decl->get_tag() == AST_POINTER_DECLARATOR || 
              named_decl->get_tag() == AST_ARRAY_DECLARATOR)) {
        printf("DEBUG: Traversing through declarator of type %d\n", named_decl->get_tag());
        named_decl = named_decl->get_kid(0);
      }
      if (named_decl) {
        temp_sym = named_decl->get_symbol();
      }
    }

    if (!temp_sym) {
      printf("DEBUG: No symbol found for declarator\n");
      continue;
    }

    // Get the consistent symbol from symbol table
    Symbol* sym = symtab->lookup_local(temp_sym->get_name());
    if (!sym) {
      printf("DEBUG: Symbol %s not found in symbol table\n", temp_sym->get_name().c_str());
      continue;
    }
    
    printf("DEBUG: Processing variable: %s\n", sym->get_name().c_str());
    printf("DEBUG: Symbol address: %p\n", (void*)sym);
    
    std::shared_ptr<Type> type = sym->get_type();
    if (!type) {
      printf("DEBUG: No type found\n");
      continue;
    }
    printf("DEBUG: Variable type is %s\n", type->as_str().c_str());

    // Debug the contents of m_address_taken_vars
    printf("DEBUG: Address taken vars contains:\n");
    for (const Symbol* s : m_address_taken_vars) {
      printf("DEBUG: - %s (addr: %p)\n", s->get_name().c_str(), (void*)s);
    }
    
    bool is_addr_taken = m_address_taken_vars.count(sym) > 0;

    // Determine storage location
    bool needs_memory = type->is_array() || 
                       type->is_struct() || 
                       is_addr_taken;
    
    printf("DEBUG: needs_memory=%d (array=%d, struct=%d, addr_taken=%d)\n",
           needs_memory, 
           type->is_array(), 
           type->is_struct(), 
           is_addr_taken);

    if (needs_memory) {
      // Allocate in memory
      unsigned offset = m_storage_calc.add_field(type);
      sym->set_offset(offset);
      sym->set_vreg(-1);
      printf("DEBUG: Allocated memory offset %u for %s\n", offset, sym->get_name().c_str());
    } else if (type->is_basic() || type->is_pointer()) {
      // Only allocate register if not address-taken
      sym->set_vreg(m_next_vreg++);
      sym->set_offset(-1);
      printf("DEBUG: Allocated vreg %d for %s\n", sym->get_vreg(), sym->get_name().c_str());
    }
  }
  printf("DEBUG: Leaving visit_variable_declaration\n");
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
        printf("DEBUG: Found address-taken variable: %s\n", sym->get_name().c_str());
        m_address_taken_vars.insert(sym);
      }
    }
  }
  
  // Recursively process all children
  for (auto i = n->cbegin(); i != n->cend(); ++i) {
    find_address_taken_vars(*i);
  }
}