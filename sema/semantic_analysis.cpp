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
    if (!sym) {
      SemanticError::raise(declarator->get_loc(), "Failed to create symbol for variable");
    }
    std::cerr << "Created symbol for variable: " << var_name << std::endl;

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
  bool is_long = false;
  bool is_short = false;
  BasicTypeKind kind = BasicTypeKind::INT; // Default to int
  std::cerr << "Entering visit_basic_type" << std::endl;
  for (Node::const_iterator it = n->cbegin(); it != n->cend(); ++it) {
    Node *child = *it;
    switch (child->get_tag()) {
      case TOK_UNSIGNED:
        is_unsigned = true;
        break;
      case TOK_SIGNED:
        is_unsigned = false;
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
        is_short = true;
        break;
      case TOK_INT:
        kind = BasicTypeKind::INT;
        break;
      case TOK_LONG:
        kind = BasicTypeKind::LONG;
        is_long = true;
        break;
      case TOK_VOID:
        kind = BasicTypeKind::VOID;
        break;
      default:
        SemanticError::raise(n->get_loc(), "Unexpected token in basic type");
    }
  }
  // Check for invalid combinations
  if (kind == BasicTypeKind::CHAR && (is_long || is_short)) {
    SemanticError::raise(n->get_loc(), "'long' and 'short' cannot be used with 'char'");
  }

  // Validate type combination
  if (kind == BasicTypeKind::VOID && (is_unsigned || is_const || is_volatile)) {
    SemanticError::raise(n->get_loc(), "Invalid type qualifiers for void");
  }
  // Check for invalid combinations
  if (kind == BasicTypeKind::CHAR && (kind == BasicTypeKind::LONG || kind == BasicTypeKind::SHORT)) {
    SemanticError::raise(n->get_loc(), "'long' and 'short' cannot be used with 'char'");
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
  std::cerr << "Entering visit_function_definition" << std::endl;

  // Extract function name and return type
  std::string func_name = n->get_kid(1)->get_str();
  visit(n->get_kid(0));  // Visit return type
  std::shared_ptr<Type> return_type = n->get_kid(0)->get_type();

  // Check if function already exists in the global symbol table
  Symbol *existing_sym = m_global_symtab->lookup_local(func_name);
  SymbolTable *func_symtab = nullptr;

  if (existing_sym) {
    if (existing_sym->get_kind() != SymbolKind::FUNCTION) {
      SemanticError::raise(n->get_loc(), ("'" + func_name + "' redefined as different kind of symbol").c_str());
    }
    // Function already declared, check if types match
    std::shared_ptr<FunctionType> existing_type = std::dynamic_pointer_cast<FunctionType>(existing_sym->get_type());
    if (!existing_type || existing_type->get_return_type() != return_type) {
      SemanticError::raise(n->get_loc(), ("Conflicting types for '" + func_name + "'").c_str());
    }
    // Re-enter the existing function's scope
    func_symtab = existing_sym->get_symtab();
  } else {
    // Create a new symbol table for the function
    func_symtab = enter_scope("function " + func_name);
  }

  m_cur_symtab = func_symtab;

  // Process parameter list
  Node *param_list = n->get_kid(2);
  std::vector<std::shared_ptr<Type>> param_types;
  for (Node::const_iterator it = param_list->cbegin(); it != param_list->cend(); ++it) {
    Node *param_node = *it;
    visit(param_node);
    if (!param_node->get_type()) {
      SemanticError::raise(param_node->get_loc(), "Failed to determine parameter type");
    }
    param_types.push_back(param_node->get_type());
  }

  // Create FunctionType object
  std::shared_ptr<Type> func_type = std::make_shared<FunctionType>(return_type, param_types);

  if (!existing_sym) {
    // Add function to global symbol table
    m_global_symtab->add_entry(n->get_loc(), SymbolKind::FUNCTION, func_name, func_type);
  }

  // Set the function type for the current symbol table
  m_cur_symtab->set_fn_type(func_type);

  // Set the type of the function definition node
  n->set_type(func_type);

  // Visit function body
  visit(n->get_kid(3));

  // Return to the previous scope
  leave_scope();

  std::cerr << "Exiting visit_function_definition" << std::endl;
}

void SemanticAnalysis::visit_function_declaration(Node *n) {
  // Extract function name and return type
  std::string func_name = n->get_kid(1)->get_str();
  visit(n->get_kid(0));  // Visit return type
  std::shared_ptr<Type> return_type = n->get_kid(0)->get_type();
  std::cerr << "Return type in visit_function_declaration: " << return_type->as_str() << std::endl;

  // Check if function already exists
  Symbol *existing_sym = m_cur_symtab->lookup_local(func_name);
  if (existing_sym) {
    std::cerr << "Function already declared: " << func_name << std::endl;
    // Function already declared, check if types match
    std::shared_ptr<FunctionType> existing_type = std::dynamic_pointer_cast<FunctionType>(existing_sym->get_type());
    if (!existing_type || existing_type->get_return_type() != return_type) {
      SemanticError::raise(n->get_loc(), ("Conflicting types for '" + func_name + "'").c_str());
    }
    // Re-enter the existing function's scope
    m_cur_symtab = existing_sym->get_symtab();
  } else {
    // Create a new symbol table for the function
    m_cur_symtab = enter_scope("function " + func_name);
  }
  std::cerr << "Created new scope for function: " << func_name << std::endl;
  // Process parameter list
  Node *param_list = n->get_kid(2);
  if (!param_list) {
    SemanticError::raise(n->get_loc(), "Function declaration must have a parameter list");
  }
  std::cerr << "Param list in visit_function_declaration: " << param_list->get_num_kids() << std::endl;
  std::vector<std::shared_ptr<Type>> param_types;
  for (Node::const_iterator it = param_list->cbegin(); it != param_list->cend(); ++it) {
    Node *param_node = *it;
    if (!param_node) {
      std::cerr << "Warning: Null parameter node found" << std::endl;
      continue;
    }
    std::cerr << "Processing parameter node with tag: " << param_node->get_tag() << std::endl;
  
    visit(param_node);
    if (!param_node->get_type()) {
      std::cerr << "Warning: Parameter node has no type after visiting" << std::endl;
      continue;
    }

    std::shared_ptr<Type> param_type = param_node->get_type();
    std::cerr << "Parameter type in visit_function_declaration: " << param_type->as_str() << std::endl;
    // Convert array parameters to pointer types
    if (auto array_type = std::dynamic_pointer_cast<ArrayType>(param_type)) {
      param_type = std::make_shared<PointerType>(array_type->get_base_type());
    }
    param_types.push_back(param_type);
  }
  std::cerr << "Param types in visit_function_declaration: ";
  // Create or update FunctionType object
  std::shared_ptr<Type> func_type = std::make_shared<FunctionType>(return_type, param_types);

  if (!existing_sym) {
    // Add function to global symbol table
    m_global_symtab->add_entry(n->get_loc(), SymbolKind::FUNCTION, func_name, func_type);
  }
  std::cerr << "Created FunctionType: " << func_type->as_str() << std::endl;

  // Set the type of the function declaration node
  n->set_type(func_type);
  std::cerr << "Set type of function declaration node: " << func_type->as_str() << std::endl;
  // Return to the previous scope
  leave_scope();
}

void SemanticAnalysis::visit_function_parameter_list(Node *n) {
  std::cerr << "Entering visit_function_parameter_list" << std::endl;

  std::vector<std::shared_ptr<Type>> param_types;

  for (Node::const_iterator it = n->cbegin(); it != n->cend(); ++it) {
    Node *param_node = *it;
    visit(param_node);  // This will call visit_function_parameter for each parameter

    if (!param_node->get_type()) {
      SemanticError::raise(param_node->get_loc(), "Failed to determine parameter type");
    }

    std::shared_ptr<Type> param_type = param_node->get_type();
    param_types.push_back(param_type);

    std::cerr << "Processed parameter with type: " << param_type->as_str() << std::endl;
  }

  // Set the parameter types on the parameter list node
  n->set_type(std::make_shared<FunctionType>(nullptr, param_types));

  std::cerr << "Exiting visit_function_parameter_list" << std::endl;
}

void SemanticAnalysis::visit_function_parameter(Node *n) {
  std::cerr << "Entering visit_function_parameter" << std::endl;
  
  // Visit the type node
  visit(n->get_kid(0));
  std::shared_ptr<Type> param_type = n->get_kid(0)->get_type();
  
  if (!param_type) {
    SemanticError::raise(n->get_loc(), "Failed to determine parameter type");
  }
  
  // Handle the declarator
  Node *declarator = n->get_kid(1);
  std::string param_name;
  
  if (declarator->get_tag() == AST_NAMED_DECLARATOR) {
    param_name = declarator->get_kid(0)->get_str();
  } else if (declarator->get_tag() == AST_ARRAY_DECLARATOR) {
    // Convert array parameter to pointer
    param_type = std::make_shared<PointerType>(param_type);
    Node *current = declarator;
    while (current->get_tag() == AST_ARRAY_DECLARATOR) {
      current = current->get_kid(0);
    }
    if (current->get_tag() == AST_NAMED_DECLARATOR) {
      param_name = current->get_kid(0)->get_str();
    }
  }
  
  if (param_name.empty()) {
    SemanticError::raise(n->get_loc(), "Failed to determine parameter name");
  }
  
  // Add parameter to current symbol table
  m_cur_symtab->add_entry(n->get_loc(), SymbolKind::VARIABLE, param_name, param_type);
  
  // Set the type of the parameter node
  n->set_type(param_type);
  
  std::cerr << "Parameter processed: " << param_name << " with type " << param_type->as_str() << std::endl;
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
    std::cerr << "Entering visit_statement_list" << std::endl;

    

    // Iterate through each statement in the list
    for (Node::const_iterator it = n->cbegin(); it != n->cend(); ++it) {
        Node *stmt = *it;
        
        switch (stmt->get_tag()) {
            case AST_VARIABLE_DECLARATION:
                visit_variable_declaration(stmt);
                break;
            case AST_RETURN_EXPRESSION_STATEMENT:
                visit_return_expression_statement(stmt);
                break;
            case AST_BINARY_EXPRESSION:
                visit_binary_expression(stmt);
                break;
            // Add cases for other statement types as needed
            default:
                visit(stmt);  // For any other type of statement
        }
    }



    // Leave the scope after processing all statements

    std::cerr << "Exiting visit_statement_list" << std::endl;
}

void SemanticAnalysis::visit_return_expression_statement(Node *n) {
    // Get the current function's return type from the symbol table
    std::shared_ptr<Type> function_return_type = m_cur_symtab->get_fn_type();
    if (!function_return_type) {
        SemanticError::raise(n->get_loc(), "Return statement outside of function");
    }

    // Get the actual return type (base type of the function type)
    function_return_type = function_return_type->get_base_type();
    
    // Get the return expression (if any)
    Node *expr = n->get_kid(0);
    std::shared_ptr<Type> expr_type;

    if (expr) {
        visit(expr);
        expr_type = expr->get_type();
        if (!expr_type) {
            SemanticError::raise(expr->get_loc(), "Return expression has no type");
        }
    }
    std::cerr << "expr_type: " << expr_type->as_str() << std::endl;
    std::cerr << "function_return_type: " << function_return_type->as_str() << std::endl;

    // Type checking
    if (function_return_type->is_void()) {
        if (expr) {
            SemanticError::raise(n->get_loc(), "Void function cannot return a value");
        }
    } else {
        if (!expr) {
            SemanticError::raise(n->get_loc(), "Non-void function must return a value");
        } else {

            // Check if the expression type is compatible with the function return type
            if (!is_assignable(expr_type, function_return_type)) {
                SemanticError::raise(n->get_loc(), "Incompatible return type");
            }
            
            // Handle implicit conversions if necessary
            if (!function_return_type->is_same(expr_type.get())) {
                Node *conversion = create_implicit_conversion(expr, function_return_type);
                n->set_kid(0, conversion);
            }
        }
    }

    // Set the type of the return statement node
    n->set_type(function_return_type);
}
// Helper function to create implicit conversion node
Node *SemanticAnalysis::create_implicit_conversion(Node *n, std::shared_ptr<Type> target_type) {
    std::unique_ptr<Node> conversion(new Node(AST_IMPLICIT_CONVERSION, {n}));
    conversion->set_type(target_type);
    return conversion.release();
}

// Helper function to check if one type can be assigned to another
bool SemanticAnalysis::is_assignable(std::shared_ptr<Type> target, std::shared_ptr<Type> source) {
    if (target->is_same(source.get())) {
        return true;
    }
    
    if (target->is_basic() && source->is_basic()) {
        BasicTypeKind target_kind = target->get_basic_type_kind();
        BasicTypeKind source_kind = source->get_basic_type_kind();
        
        // Allow implicit conversion to more precise types
        if (target_kind >= source_kind) {
            return true;
        }
        
        // Allow conversion from signed to unsigned of same or greater precision
        if (!target->is_signed() && source->is_signed() && target_kind >= source_kind) {
            return true;
        }
    }
    
    // Add more rules for pointer types, struct types, etc. as needed
    
    return false;
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
    Node *op_node = n->get_kid(0);
    Node *left = n->get_kid(1);
    Node *right = n->get_kid(2);
    int op = op_node->get_tag();

    std::cerr << "visiting left: " << left->get_tag() << std::endl;
    std::cerr << "visiting right: " << right->get_tag() << std::endl;
    std::cerr << "Operator: " << op << std::endl;

    // Visit left and right operands
    if (left->get_tag() == AST_VARIABLE_REF) {
        std::cerr << "Explicitly calling visit_variable_ref for left operand" << std::endl;
        visit_variable_ref(left);
    } else {
        visit(left);
    }

    if (right->get_tag() == AST_VARIABLE_REF) {
        std::cerr << "Explicitly calling visit_variable_ref for right operand" << std::endl;
        visit_variable_ref(right);
    } else {
        visit(right);
    }
    // Get types of left and right operands
    std::shared_ptr<Type> left_type = left->get_type();
    std::shared_ptr<Type> right_type = right->get_type();
    if (!left_type) {
        std::cerr << "Left type is null" << std::endl;
        SemanticError::raise(left->get_loc(), "Left operand has no type");
    }
    if (right_type) {
      std::cerr << "Right type: " << right_type->as_str() << std::endl;
    }
    if (!left_type || !right_type) {
        SemanticError::raise(n->get_loc(), "Invalid operand types in binary expression");
    }
   

    if (op == TOK_ASSIGN) {
        // Handle assignment
        if (!is_lvalue(left)) {
            SemanticError::raise(left->get_loc(), "Left-hand side of assignment must be an lvalue");
        }

        if (is_const_qualified(left_type)) {
            SemanticError::raise(left->get_loc(), "Cannot assign to const-qualified lvalue");
        }

        if (left_type->is_basic() && right_type->is_basic()) {
            // Integer assignment
            n->set_type(left_type);
        } else if (left_type->is_pointer() && right_type->is_pointer()) {
            // Pointer assignment
            if (!are_compatible_pointer_types(left_type, right_type)) {
                SemanticError::raise(n->get_loc(), "Incompatible pointer types in assignment");
            }
            n->set_type(left_type);
        } else if (left_type->is_struct() && right_type->is_struct()) {
            // Struct assignment
            if (!left_type->is_same(right_type.get())) {
                SemanticError::raise(n->get_loc(), "Incompatible struct types in assignment");
            }
            n->set_type(left_type);
        } else {
            SemanticError::raise(n->get_loc(), "Incompatible types in assignment");
        }
    } else {
        // Handle other binary operations
        if (op == TOK_PLUS || op == TOK_MINUS) {
            if (left_type->is_pointer() && right_type->is_basic()) {
                // Pointer arithmetic
                n->set_type(left_type);
            } else if (left_type->is_basic() && right_type->is_pointer()) {
                // Pointer arithmetic (reversed operands)
                n->set_type(right_type);
            } else if (left_type->is_basic() && right_type->is_basic()) {
                // Integer arithmetic
                n->set_type(promote_arithmetic_types(left_type, right_type));
            } else {
                SemanticError::raise(n->get_loc(), "Invalid operand types for + or - operator");
            }
        } else if (op == TOK_ASTERISK || op == TOK_DIVIDE || op == TOK_MOD) {
            // Arithmetic operations
            if (left_type->is_basic() && right_type->is_basic()) {
                n->set_type(promote_arithmetic_types(left_type, right_type));
            } else {
                SemanticError::raise(n->get_loc(), "Invalid operand types for arithmetic operator");
            }
        } else {
            // Other binary operators (comparison, logical, etc.)
            if (left_type->is_basic() && right_type->is_basic()) {
                n->set_type(std::make_shared<BasicType>(BasicTypeKind::INT, true));
            } else {
                SemanticError::raise(n->get_loc(), "Invalid operand types for binary operator");
            }
        }
    }
}

void SemanticAnalysis::visit_unary_expression(Node *n) {
    std::cerr << "Entering visit_unary_expression" << std::endl;

    // Get the operator and operand
    Node *op_node = n->get_kid(0);
    Node *operand = n->get_kid(1);
    int op = op_node->get_tag();

    std::cerr << "Unary operator: " << op << std::endl;

    // Visit the operand
    visit(operand);

    // Get the type of the operand
    std::shared_ptr<Type> operand_type = operand->get_type();

    if (!operand_type) {
        SemanticError::raise(operand->get_loc(), "Operand has no type");
    }

    std::cerr << "Operand type: " << operand_type->as_str() << std::endl;

    // Handle different unary operators
    switch (op) {
        case TOK_MINUS:
        case TOK_PLUS:
            // Arithmetic unary operators
            if (operand_type->is_basic() && !operand_type->is_void()) {
                // Promote operand to int if it's less precise
                if (operand_type->get_basic_type_kind() < BasicTypeKind::INT) {
                    std::shared_ptr<Type> promoted_type = std::make_shared<BasicType>(BasicTypeKind::INT, operand_type->is_signed());
                    n->set_kid(1, create_implicit_conversion(operand, promoted_type));
                    operand_type = promoted_type;
                }
                n->set_type(operand_type);
            } else {
                SemanticError::raise(n->get_loc(), "Invalid operand type for unary arithmetic operator");
            }
            break;

        case TOK_NOT:
            // Logical not operator
            if (operand_type->is_basic() && !operand_type->is_void()) {
                // Result is always int (boolean)
                n->set_type(std::make_shared<BasicType>(BasicTypeKind::INT, true));
            } else {
                SemanticError::raise(n->get_loc(), "Invalid operand type for logical not operator");
            }
            break;

        case TOK_ASTERISK:
            // Pointer dereference
            if (operand_type->is_pointer()) {
                n->set_type(operand_type->get_base_type());
            } else {
                SemanticError::raise(n->get_loc(), "Cannot dereference non-pointer type");
            }
            break;

        case TOK_AMPERSAND:
            // Address-of operator
            if (is_lvalue(operand)) {
                n->set_type(std::make_shared<PointerType>(operand_type));
            } else {
                SemanticError::raise(n->get_loc(), "Cannot take address of non-lvalue");
            }
            break;

        default:
            SemanticError::raise(n->get_loc(), "Unknown unary operator");
    }

    std::cerr << "Unary expression type: " << n->get_type()->as_str() << std::endl;
    std::cerr << "Exiting visit_unary_expression" << std::endl;
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
    std::cerr << "Entering visit_function_call_expression" << std::endl;

    // Visit the function name (should be a variable reference)
    Node *func_ref = n->get_kid(0);
    visit(func_ref);

    // Get the symbol and type from the function reference node
    Symbol *func_sym = func_ref->get_symbol();
    if (!func_sym) {
        SemanticError::raise(func_ref->get_loc(), "Function not found");
    }

    std::shared_ptr<Type> func_type = func_sym->get_type();
    if (!func_type || !func_type->is_function()) {
        SemanticError::raise(func_ref->get_loc(), "Called object is not a function");
    }

    // Get the argument list
    Node *arg_list = n->get_kid(1);

    // Check number of arguments
    if (arg_list->get_num_kids() != func_type->get_num_members()) {
        SemanticError::raise(n->get_loc(), "Incorrect number of arguments");
    }

    // Check type compatibility for each argument
    for (unsigned i = 0; i < arg_list->get_num_kids(); ++i) {
        Node *arg = arg_list->get_kid(i);
        visit(arg);
        
        const Member& param = func_type->get_member(i);
        std::shared_ptr<Type> param_type = param.get_type();
        std::shared_ptr<Type> arg_type = arg->get_type();

        if (!arg_type) {
            SemanticError::raise(arg->get_loc(), "Argument has no type");
        }
        std::cerr << "param_type: " << param_type->as_str() << std::endl;
        std::cerr << "arg_type: " << arg_type->as_str() << std::endl;

        if (!is_assignable(param_type, arg_type)) {
            SemanticError::raise(arg->get_loc(), 
                ("Incompatible argument type for parameter " + std::to_string(i + 1)).c_str());
        }

        // Handle implicit conversions if necessary
        if (!param_type->is_same(arg_type.get())) {
            Node *conversion = create_implicit_conversion(arg, param_type);
            arg_list->set_kid(i, conversion);
        }
    }

    // Set the type of the function call expression to the return type of the function
    n->set_type(func_type->get_base_type());

    std::cerr << "Exiting visit_function_call_expression" << std::endl;
}

void SemanticAnalysis::visit_field_ref_expression(Node *n) {
    std::cerr << "Entering visit_field_ref_expression" << std::endl;

    // Visit the left-hand side (struct expression)
    Node *lhs = n->get_kid(0);
    visit(lhs);

    std::shared_ptr<Type> lhs_type = lhs->get_type();
    if (!lhs_type || !lhs_type->is_struct()) {
        SemanticError::raise(n->get_loc(), "Left-hand side of '.' is not a struct");
    }

    // Get the field name
    std::string field_name = n->get_kid(1)->get_str();

    // Look up the field in the struct type
    const Member *member = lhs_type->find_member(field_name);
    if (!member) {
        SemanticError::raise(n->get_loc(), ("Struct has no member named '" + field_name + "'").c_str());
    }

    // Set the type of the field reference expression
    n->set_type(member->get_type());

    std::cerr << "Exiting visit_field_ref_expression" << std::endl;
}

void SemanticAnalysis::visit_indirect_field_ref_expression(Node *n) {
    std::cerr << "Entering visit_indirect_field_ref_expression" << std::endl;

    // Visit the left-hand side (pointer to struct expression)
    Node *lhs = n->get_kid(0);
    visit(lhs);

    std::shared_ptr<Type> lhs_type = lhs->get_type();
    if (!lhs_type || !lhs_type->is_pointer()) {
        SemanticError::raise(n->get_loc(), "Left-hand side of '->' is not a pointer");
    }

    // Get the base type (should be a struct)
    std::shared_ptr<Type> base_type = std::dynamic_pointer_cast<PointerType>(lhs_type)->get_base_type();
    if (!base_type->is_struct()) {
        SemanticError::raise(n->get_loc(), "Left-hand side of '->' is not a pointer to a struct");
    }

    // Get the field name
    std::string field_name = n->get_kid(1)->get_str();

    // Look up the field in the struct type
    const Member *member = base_type->find_member(field_name);
    if (!member) {
        SemanticError::raise(n->get_loc(), ("Struct has no member named '" + field_name + "'").c_str());
    }

    // Set the type of the field reference expression
    n->set_type(member->get_type());

    std::cerr << "Exiting visit_indirect_field_ref_expression" << std::endl;
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
    std::cerr << "Visiting variable reference" << std::endl;

    if (n->get_num_kids() != 1 || n->get_kid(0)->get_tag() != TOK_IDENT) {
        SemanticError::raise(n->get_loc(), "Invalid variable reference node structure");
    }
    std::string var_name = n->get_kid(0)->get_str();
    std::cerr << "Visiting variable reference: " << var_name << std::endl;
  
    // Look up the variable in the current symbol table and its parents
    Symbol *sym = m_cur_symtab->lookup_recursive(var_name);
    std::cerr << "Current scope: " << m_cur_symtab->get_name() << std::endl;
    std::cerr << "Lookup for variable: " << var_name << std::endl;
  
    if (!sym) {
        SemanticError::raise(n->get_loc(), ("Undefined variable '" + var_name + "'").c_str());
    }
    std::cerr << "sym kind: " << static_cast<int>(sym->get_kind()) << std::endl;
    // Handle both variables and functions
    if (sym->get_kind() != SymbolKind::VARIABLE && sym->get_kind() != SymbolKind::FUNCTION) {
        SemanticError::raise(n->get_loc(), ("'" + var_name + "' is neither a variable nor a function").c_str());
    }
    // Check if the symbol has a valid type before setting it
    std::shared_ptr<Type> var_type = sym->get_type();
    if (!var_type) {
        SemanticError::raise(n->get_loc(), ("Variable '" + var_name + "' has no type information").c_str());
    }
    std::cerr << "Variable type: " << var_type->as_str() << std::endl;
  
    // Set the type of the node to the variable's type
    n->set_type(var_type);
  
    // Annotate the node with the symbol
    n->set_symbol(sym);

    std::cerr << "Variable reference processed: " << var_name << " with type " << var_type->as_str() << std::endl;
}

void SemanticAnalysis::visit_literal_value(Node *n) {
  std::cerr << "Visiting literal value node" << std::endl;
  
  // Check if the node has children
  if (n->get_num_kids() > 0) {
    Node *literal_child = n->get_kid(0);
    std::string literal_str = literal_child->get_str();
    std::shared_ptr<Type> literal_type;

    std::cerr << "Literal child tag: " << literal_child->get_tag() << std::endl;
    std::cerr << "Literal value: " << literal_str << std::endl;

    switch (literal_child->get_tag()) {
      case TOK_INT_LIT:
        std::cerr << "Processing integer literal: " << literal_str << std::endl;
        literal_type = std::make_shared<BasicType>(BasicTypeKind::INT, true);
        std::cerr << " Created literal type: " << literal_type->as_str() << std::endl;
        break;
      case TOK_CHAR_LIT:
        literal_type = std::make_shared<BasicType>(BasicTypeKind::CHAR, true);
        break;
      case TOK_DOUBLE:
        literal_type = std::make_shared<BasicType>(BasicTypeKind::SHORT, true);
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
  } else {
    SemanticError::raise(n->get_loc(), "Literal value node has no children");
  }
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
std::shared_ptr<Type> SemanticAnalysis::promote_arithmetic_types(std::shared_ptr<Type> left, std::shared_ptr<Type> right) {
    if (!left->is_basic() || !right->is_basic()) {
        return nullptr;  // Not arithmetic types
    }

    BasicType* left_basic = dynamic_cast<BasicType*>(left.get());
    BasicType* right_basic = dynamic_cast<BasicType*>(right.get());

    if (!left_basic || !right_basic) {
        return nullptr;  // Dynamic cast failed
    }

    // Promote to int or unsigned int if less precise
    if (left_basic->get_basic_type_kind() < BasicTypeKind::INT) {
        left_basic = new BasicType(BasicTypeKind::INT, left_basic->is_signed());
    }
    if (right_basic->get_basic_type_kind() < BasicTypeKind::INT) {
        right_basic = new BasicType(BasicTypeKind::INT, right_basic->is_signed());
    }

    // Promote to the more precise type
    // Promote to the more precise type
    if (left_basic->get_basic_type_kind() > right_basic->get_basic_type_kind()) {
        return std::make_shared<BasicType>(left_basic->get_basic_type_kind(), left_basic->is_signed());
    } else if (right_basic->get_basic_type_kind() > left_basic->get_basic_type_kind()) {
        return std::make_shared<BasicType>(right_basic->get_basic_type_kind(), right_basic->is_signed());
    }

    // If same precision but different signedness, convert to unsigned
    if (!left_basic->is_signed() || !right_basic->is_signed()) {
        return std::make_shared<BasicType>(left_basic->get_basic_type_kind(), false);
    }

    // Same type, return either
    return std::make_shared<BasicType>(left_basic->get_basic_type_kind(), left_basic->is_signed());
}

bool SemanticAnalysis::are_compatible_pointer_types(std::shared_ptr<Type> left, std::shared_ptr<Type> right) {
    if (!left->is_pointer() || !right->is_pointer()) {
        return false;
    }

    PointerType* left_ptr = dynamic_cast<PointerType*>(left.get());
    PointerType* right_ptr = dynamic_cast<PointerType*>(right.get());

    if (!left_ptr || !right_ptr) {
        return false;  // Dynamic cast failed
    }

    std::shared_ptr<Type> left_base = left_ptr->get_base_type();
    std::shared_ptr<Type> right_base = right_ptr->get_base_type();

    // Check if unqualified base types are identical
    if (!left_base->get_unqualified_type()->is_same(right_base->get_unqualified_type())) {
        return false;
    }

    // Check if left base type doesn't lack any qualifiers that right base type has
    if (right_base->is_const() && !left_base->is_const()) {
        return false;
    }
    if (right_base->is_volatile() && !left_base->is_volatile()) {
        return false;
    }

    return true;
}

bool SemanticAnalysis::is_const_qualified(std::shared_ptr<Type> type) {
    if (type->is_qualified()) {
        QualifiedType* qual_type = dynamic_cast<QualifiedType*>(type.get());
        if (qual_type) {
            return qual_type->is_const();
        }
    }
    return false;
}
bool SemanticAnalysis::is_lvalue(Node *n) {
    switch (n->get_tag()) {
        case AST_VARIABLE_REF:
            // Variable reference is always an lvalue
            return true;
        case AST_ARRAY_ELEMENT_REF_EXPRESSION:
            // Array element reference is an lvalue
            return true;
        case AST_UNARY_EXPRESSION:
            // Pointer dereference is an lvalue
            return n->get_kid(0)->get_tag() == TOK_ASTERISK;
        case AST_FIELD_REF_EXPRESSION:
        case AST_INDIRECT_FIELD_REF_EXPRESSION:
            // Struct field reference is an lvalue
            return true;
        default:
            return false;
    }
}
void SemanticAnalysis::visit_assignment_expression(Node *n) {
    std::cerr << "Entering visit_assignment_expression" << std::endl;

    // Visit left-hand side (lvalue)
    Node *lhs = n->get_kid(0);
    visit(lhs);

    // Visit right-hand side (rvalue)
    Node *rhs = n->get_kid(1);
    visit(rhs);

    std::shared_ptr<Type> lhs_type = lhs->get_type();
    std::shared_ptr<Type> rhs_type = rhs->get_type();

    if (!lhs_type || !rhs_type) {
        SemanticError::raise(n->get_loc(), "Invalid types in assignment");
    }

    // Check if lhs is an lvalue
    if (!is_lvalue(lhs)) {
        SemanticError::raise(lhs->get_loc(), "Left-hand side of assignment must be an lvalue");
    }

    // Check for const assignment
    if (is_const_qualified(lhs_type)) {
        SemanticError::raise(lhs->get_loc(), "Cannot assign to const-qualified lvalue");
    }

    // Check type compatibility and perform implicit conversions if necessary
    if (lhs_type->is_basic() && rhs_type->is_basic()) {
        // Implicit conversion for basic types
        n->set_type(lhs_type);
    } else if (lhs_type->is_pointer() && rhs_type->is_pointer()) {
        if (!are_compatible_pointer_types(lhs_type, rhs_type)) {
            SemanticError::raise(n->get_loc(), "Incompatible pointer types in assignment");
        }
        n->set_type(lhs_type);
    } else if (lhs_type->is_struct() && rhs_type->is_struct()) {
        if (!lhs_type->is_same(rhs_type.get())) {
            SemanticError::raise(n->get_loc(), "Incompatible struct types in assignment");
        }
        n->set_type(lhs_type);
    } else {
        SemanticError::raise(n->get_loc(), "Incompatible types in assignment");
    }

    std::cerr << "Exiting visit_assignment_expression" << std::endl;
}