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
#include <algorithm>
#include <utility>
#include <map>
#include "grammar_symbols.h"
#include "parse.tab.h"
#include "node.h"
#include "ast.h"
#include "exceptions.h"
#include "semantic_analysis.h"
#include <iostream>


SemanticAnalysis::SemanticAnalysis(const Options &options)
  : m_options(options)
  , m_global_symtab(new SymbolTable(nullptr, "global")) {
  m_cur_symtab = m_global_symtab;
  m_all_symtabs.push_back(m_global_symtab);
}

SemanticAnalysis::~SemanticAnalysis() {
  // The semantic analyzer owns the SymbolTables and their Symbols,
  // so delete them. Note that the AST has pointers to Symbol objects,
  // so the SemanticAnalysis object should live at least as long
  // as the AST.
  for (auto i = m_all_symtabs.begin(); i != m_all_symtabs.end(); ++i)
    delete *i;
}

void SemanticAnalysis::visit_struct_type(Node *n) {
  std::string struct_name = n->get_kid(0)->get_str();
  std::string full_name = "struct " + struct_name;
  std::cerr << "Looking up struct type: " << full_name << std::endl;
  Symbol *sym = m_cur_symtab->lookup_recursive(full_name);
  if (!sym || sym->get_kind() != SymbolKind::TYPE) {
    SemanticError::raise(n->get_loc(), ("Undefined struct type '" + struct_name + "'").c_str());
  }
  
  n->set_type(sym->get_type());
}

void SemanticAnalysis::visit_union_type(Node *n) {
  RuntimeError::raise("union types aren't supported");
}

void SemanticAnalysis::visit_variable_declaration(Node *n) {
  // Process the base type
  visit(n->get_kid(1));
  std::shared_ptr<Type> base_type = n->get_kid(1)->get_type();
  
  if (!base_type) {
    SemanticError::raise(n->get_loc(), "Failed to determine base type for variable declaration");
  }

  // Process each declarator
  Node *declarator_list = n->get_kid(2);
  for (Node::const_iterator it = declarator_list->cbegin(); it != declarator_list->cend(); ++it) {
    Node *declarator = *it;
    
    // Visit the declarator to determine the full type
    m_var_type = base_type;  // Start with the base type
    visit(declarator);
    std::shared_ptr<Type> full_type = m_var_type;

    // Get the variable name
    std::string var_name;
    if (declarator->get_tag() == AST_ARRAY_DECLARATOR) {
      Node* current = declarator;
      while (current->get_tag() == AST_ARRAY_DECLARATOR) {
        current = current->get_kid(0);
      }
      if (current->get_tag() == AST_NAMED_DECLARATOR) {
        var_name = current->get_kid(0)->get_str();
      } else {
        SemanticError::raise(declarator->get_loc(), "Invalid array declarator structure");
      }
    } else if (declarator->get_tag() == AST_NAMED_DECLARATOR) {
      var_name = declarator->get_kid(0)->get_str();
    } else {
      var_name = declarator->get_str();
    }

    std::cerr << "Variable name: " << var_name << std::endl;

    // Check if the variable name is empty
    if (var_name.empty()) {
      SemanticError::raise(declarator->get_loc(), "Empty variable name in declaration");
    }

    // Check if the variable name already exists in the current scope
    if (m_cur_symtab->lookup_local(var_name) != nullptr) {
      SemanticError::raise(declarator->get_loc(), ("Redefinition of variable '" + var_name + "'").c_str());
    }

    // Create a symbol for the variable
    Symbol *sym = new Symbol(SymbolKind::VARIABLE, var_name, full_type, m_cur_symtab);

    // Add the symbol to the current symbol table
    m_cur_symtab->add_entry(declarator->get_loc(), SymbolKind::VARIABLE, var_name, full_type);

    // Annotate the declarator node with the symbol
    declarator->set_symbol(sym);
  }
}
void SemanticAnalysis::visit_basic_type(Node *n) {
  bool is_unsigned = false;
  bool is_const = false;
  bool is_volatile = false;
  BasicTypeKind kind = BasicTypeKind::INT; // Default to int
  std::cerr << "Entering visit_basic_type" << std::endl;
  for (Node::const_iterator it = n->cbegin(); it != n->cend(); ++it) {
    Node *child = *it;
    switch (child->get_tag()) {
      case TOK_UNSIGNED:
        is_unsigned = true;
        break;
      case TOK_CONST:
        is_const = true;
        break;
      case TOK_VOLATILE:
        is_volatile = true;
        break;
      case TOK_CHAR:
        kind = BasicTypeKind::CHAR;
        break;
      case TOK_SHORT:
        kind = BasicTypeKind::SHORT;
        break;
      case TOK_INT:
        kind = BasicTypeKind::INT;
        break;
      case TOK_LONG:
        kind = BasicTypeKind::LONG;
        break;
      case TOK_VOID:
        kind = BasicTypeKind::VOID;
        break;
      default:
        SemanticError::raise(n->get_loc(), "Unexpected token in basic type");
    }
  }

  // Validate type combination
  if (kind == BasicTypeKind::VOID && (is_unsigned || is_const || is_volatile)) {
    SemanticError::raise(n->get_loc(), "Invalid type qualifiers for void");
  }

  // Create BasicType
  std::shared_ptr<Type> type = std::make_shared<BasicType>(kind, !is_unsigned);


  // Wrap in QualifiedType if necessary
  if (is_const && is_volatile) {
    type = std::make_shared<QualifiedType>(type, TypeQualifier::CONST);
    type = std::make_shared<QualifiedType>(type, TypeQualifier::VOLATILE);
  } else if (is_const) {
    type = std::make_shared<QualifiedType>(type, TypeQualifier::CONST);
  } else if (is_volatile) {
    type = std::make_shared<QualifiedType>(type, TypeQualifier::VOLATILE);
  }

  std::cerr << "Created type in visit_basic_type: " << type->as_str() << std::endl;


  // Annotate the AST node
  n->set_type(type);
  m_var_type = type;
  std::cerr << "Type set on node in visit_basic_type: " << (n->get_type() ? n->get_type()->as_str() : "null") << std::endl;

}

void SemanticAnalysis::visit_named_declarator(Node *n) {
  std::shared_ptr<Type> type = m_var_type;
  std::cerr << "Type in visit_named_declarator: " << (type ? type->as_str() : "null") << std::endl;

  n->set_type(type);

  std::string var_name;
  if (n->get_num_kids() > 0 && n->get_kid(0)->get_tag() == TOK_IDENT) {
    var_name = n->get_kid(0)->get_str();
    std::cerr << "Found identifier: " << var_name << std::endl;
  } else {
    std::cerr << "No identifier found in named declarator" << std::endl;
    SemanticError::raise(n->get_loc(), "Expected identifier in named declarator");
  }

  if (var_name.empty()) {
    std::cerr << "Empty variable name in named declarator" << std::endl;
    SemanticError::raise(n->get_loc(), "Empty variable name in named declarator");
  }

  n->set_str(var_name);

  m_var_type = type;
  std::cerr << "Variable name set: " << var_name << ", Type: " << (type ? type->as_str() : "null") << std::endl;
}

void SemanticAnalysis::visit_pointer_declarator(Node *n) {
  // TODO: implement
}

void SemanticAnalysis::visit_array_declarator(Node *n) {
  std::shared_ptr<Type> base_type = m_var_type;
  Node *size_expr = n->get_kid(1);
  int array_size = -1;

  if (size_expr) {
    visit(size_expr);
    std::cerr << "Size expression tag: " << size_expr->get_tag() << std::endl;
    
    if (size_expr->get_tag() == TOK_INT_LIT) {
      try {
        array_size = std::stoi(size_expr->get_str());
        if (array_size <= 0) {
          SemanticError::raise(size_expr->get_loc(), "Array size must be a positive integer");
        }
      } catch (const std::exception&) {
        SemanticError::raise(size_expr->get_loc(), "Invalid array size");
      }
    } else {
      SemanticError::raise(size_expr->get_loc(), "Array size must be a constant expression");
    }
  }

  std::shared_ptr<Type> array_type = std::make_shared<ArrayType>(base_type, array_size);
  
  // Handle nested arrays
  if (n->get_kid(0)->get_tag() == AST_ARRAY_DECLARATOR) {
    visit(n->get_kid(0));
    array_type = std::make_shared<ArrayType>(array_type, array_size);
  }else if (n->get_kid(0)->get_tag() == TOK_IDENT) {
    // Set the variable name when we reach the identifier
    n->set_str(n->get_kid(0)->get_str());
  }

  m_var_type = array_type;
  n->set_type(array_type);

  // Set the variable name
  if (n->get_kid(0)->get_tag() == TOK_IDENT) {
    n->set_str(n->get_kid(0)->get_str());
  } else if (n->get_kid(0)->get_tag() == AST_ARRAY_DECLARATOR) {
    n->set_str(n->get_kid(0)->get_str());
  }
}
void SemanticAnalysis::visit_function_definition(Node *n) {
  // TODO: implement
}

void SemanticAnalysis::visit_function_declaration(Node *n) {
  // TODO: implement
}

void SemanticAnalysis::visit_function_parameter_list(Node *n) {
  // TODO: implement
}

void SemanticAnalysis::visit_function_parameter(Node *n) {
  // TODO: implement
}
bool SemanticAnalysis::is_constant_expression(Node *n) {
    switch (n->get_tag()) {
        case AST_LITERAL_VALUE:
            return true;
        case AST_UNARY_EXPRESSION:
            return n->get_kid(0)->get_tag() == '-' && is_constant_expression(n->get_kid(1));
        case AST_BINARY_EXPRESSION: {
            int op = n->get_kid(1)->get_tag();
            return is_constant_expression(n->get_kid(0)) && is_constant_expression(n->get_kid(2)) &&
                   (op == '+' || op == '-' || op == '*' || op == '/' || op == '%');
        }
        default:
            return false;
    }
}

int SemanticAnalysis::evaluate_constant_expression(Node *n) {
    switch (n->get_tag()) {
        case AST_LITERAL_VALUE:
            return std::stoi(n->get_str());
        case AST_UNARY_EXPRESSION: {
            int op = n->get_kid(0)->get_tag();
            int val = evaluate_constant_expression(n->get_kid(1));
            return (op == '-') ? -val : val;
        }
        case AST_BINARY_EXPRESSION: {
            int left = evaluate_constant_expression(n->get_kid(0));
            int right = evaluate_constant_expression(n->get_kid(2));
            int op = n->get_kid(1)->get_tag();
            switch (op) {
                case '+': return left + right;
                case '-': return left - right;
                case '*': return left * right;
                case '/':
                    if (right == 0) {
                        SemanticError::raise(n->get_loc(), "Division by zero in constant expression");
                    }
                    return left / right;
                case '%':
                    if (right == 0) {
                        SemanticError::raise(n->get_loc(), "Modulo by zero in constant expression");
                    }
                    return left % right;
                default:
                    SemanticError::raise(n->get_loc(), "Invalid operator in constant expression");
            }
        }
        default:
            SemanticError::raise(n->get_loc(), "Invalid node type in constant expression");
    }
    return 0; // This line should never be reached
}
void SemanticAnalysis::visit_statement_list(Node *n) {
  // TODO: implement
}

void SemanticAnalysis::visit_return_expression_statement(Node *n) {
  // TODO: implement
}

 void SemanticAnalysis::visit_struct_type_definition(Node *n) {
  std::cerr << "Entering visit_struct_type_definition" << std::endl;

  // Get the name of the struct type
  std::string name = n->get_kid(0)->get_str();
  Location loc = n->get_loc();
  std::cerr << "Struct name: " << name << std::endl;

  // Create a new StructType
  std::shared_ptr<Type> struct_type = std::make_shared<StructType>(name);
  std::cerr << "Created StructType: " << struct_type->as_str() << std::endl;

  // Add the struct type to the current symbol table
  m_cur_symtab->add_entry(loc, SymbolKind::TYPE, "struct " + name, struct_type);
  std::cerr << "Added struct type to symbol table: struct " << name << std::endl;

  // Create a new scope for the struct members
  SymbolTable *struct_scope = enter_scope("struct " + name);
  std::cerr << "Entered new scope for struct members" << std::endl;

  // Process the struct members
  Node *member_list = n->get_kid(1);
  std::cerr << "Number of struct members: " << member_list->get_num_kids() << std::endl;
  
  for (Node::const_iterator it = member_list->cbegin(); it != member_list->cend(); ++it) {
    Node *member_node = *it;
    std::cerr << "Processing member node" << std::endl;
    visit(member_node);
    
    // After visiting the member, we should have its name and type
    std::string member_name = member_node->get_kid(2)->get_kid(0)->get_str();
    std::shared_ptr<Type> member_type = member_node->get_kid(1)->get_type();
    if (member_type) {
        std::cerr << "Member name: " << member_name << ", Member type: " << member_type->as_str() << std::endl;
    } else {
        std::cerr << "Error: Member type is null for member: " << member_name << std::endl;
    }

    // Add the member to the StructType
    StructType* struct_type_ptr = dynamic_cast<StructType*>(struct_type.get());
    if (struct_type_ptr) {
      Member new_member(member_name, member_type);
      struct_type_ptr->add_member(new_member);
      std::cerr << "Added member to StructType: " << member_name << std::endl;
    } else {
      std::cerr << "Error: Failed to cast to StructType" << std::endl;
      SemanticError::raise(member_node->get_loc(), "Failed to cast to StructType");
    }
  }

  // Leave the struct scope
  leave_scope();
  std::cerr << "Left struct scope" << std::endl;

  // Set the type of the struct definition node
  n->set_type(struct_type);
  std::cerr << "Set type of struct definition node: " << struct_type->as_str() << std::endl;

  std::cerr << "Exiting visit_struct_type_definition" << std::endl;
}

void SemanticAnalysis::visit_binary_expression(Node *n) {
  // TODO: implement
}

void SemanticAnalysis::visit_unary_expression(Node *n) {
  // TODO: implement
}

void SemanticAnalysis::visit_postfix_expression(Node *n) {
  // TODO: implement
}

void SemanticAnalysis::visit_conditional_expression(Node *n) {
  // TODO: implement
}

void SemanticAnalysis::visit_cast_expression(Node *n) {
  // TODO: implement
}

void SemanticAnalysis::visit_function_call_expression(Node *n) {
  // TODO: implement
}

void SemanticAnalysis::visit_field_ref_expression(Node *n) {
  // TODO: implement
}

void SemanticAnalysis::visit_indirect_field_ref_expression(Node *n) {
  // TODO: implement
}

void SemanticAnalysis::visit_array_element_ref_expression(Node *n) {
  // Visit the array expression
  Node *array_expr = n->get_kid(0);
  visit(array_expr);

  // Visit the index expression
  Node *index_expr = n->get_kid(1);
  visit(index_expr);

  // Check that the array expression is an array or pointer type
  std::shared_ptr<Type> array_type = array_expr->get_type();
  if (!array_type || (!array_type->is_array() && !array_type->is_pointer())) {
    SemanticError::raise(array_expr->get_loc(), "Subscripted value is neither array nor pointer");
  }

  // Check that the index expression is an integral type
  std::shared_ptr<Type> index_type = index_expr->get_type();
  if (!index_type || !index_type->is_integral()) {
    SemanticError::raise(index_expr->get_loc(), "Array subscript is not an integer");
  }

  // Determine the element type
  std::shared_ptr<Type> element_type;
  if (array_type->is_array()) {
    element_type = std::dynamic_pointer_cast<ArrayType>(array_type)->get_base_type();
  } else { // is_pointer
    element_type = std::dynamic_pointer_cast<PointerType>(array_type)->get_base_type();
  }

  // Set the type of the array element reference node
  n->set_type(element_type);
}

void SemanticAnalysis::visit_variable_ref(Node *n) {
  // TODO: implement
}

void SemanticAnalysis::visit_literal_value(Node *n) {
  std::string literal_str = n->get_str();
  std::shared_ptr<Type> literal_type;

  switch (n->get_tag()) {
    case TOK_INT_LIT:
      literal_type = std::make_shared<BasicType>(BasicTypeKind::INT, true);
      break;
    case TOK_CHAR_LIT:
      literal_type = std::make_shared<BasicType>(BasicTypeKind::CHAR, true);
      break;
    case TOK_FP_LIT:
      // For now, treat floating-point literals as long
      literal_type = std::make_shared<BasicType>(BasicTypeKind::LONG, true);
      break;
    case TOK_STR_LIT:
      literal_type = std::make_shared<ArrayType>(
        std::make_shared<BasicType>(BasicTypeKind::CHAR, true),
        literal_str.length() + 1  // +1 for null terminator
      );
      break;
    default:
      SemanticError::raise(n->get_loc(), "Unknown literal type");
  }

  n->set_type(literal_type);
}

SymbolTable *SemanticAnalysis::enter_scope(const std::string &name) {
  SymbolTable *symtab = new SymbolTable(m_cur_symtab, name);
  m_all_symtabs.push_back(symtab);
  m_cur_symtab = symtab;
  return symtab;
}

void SemanticAnalysis::leave_scope() {
  assert(m_cur_symtab->get_parent() != nullptr);
  m_cur_symtab = m_cur_symtab->get_parent();
}

// TODO: implement helper functions
