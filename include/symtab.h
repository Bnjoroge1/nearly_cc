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

#ifndef SYMTAB_H
#define SYMTAB_H

#include <map>
#include <vector>
#include <string>
#include <memory>
#include "location.h"
#include "type.h"

class SymbolTable;

enum class SymbolKind {
  FUNCTION,
  VARIABLE,
  TYPE,
};

class Symbol {
private:
  SymbolKind m_kind;
  std::string m_name;
  std::shared_ptr<Type> m_type;
  SymbolTable *m_symtab;

  // value semantics prohibited
  Symbol(const Symbol &);
  Symbol &operator=(const Symbol &);

public:
  Symbol(SymbolKind kind, const std::string &name, std::shared_ptr<Type> type, SymbolTable *symtab);
  ~Symbol();

  SymbolKind get_kind() const;
  const std::string &get_name() const;
  std::shared_ptr<Type> get_type() const;
  SymbolTable *get_symtab() const;
};

class SymbolTable {
private:
  SymbolTable *m_parent;
  std::string m_name;
  std::vector<Symbol *> m_symbols;
  std::map<std::string, unsigned> m_lookup;
  std::shared_ptr<Type> m_fn_type; // this is set to the type of the enclosing function (if any)

  // value semantics prohibited
  SymbolTable(const SymbolTable &);
  SymbolTable &operator=(const SymbolTable &);

public:
  SymbolTable(SymbolTable *parent, const std::string &name);
  ~SymbolTable();

  SymbolTable *get_parent() const;

  const std::string &get_name() const;

  // Operations limited to the current (local) scope.
  // Note that the caller should verify that a name is not defined
  // in the current scope before calling declare or define.
  bool has_symbol_local(const std::string &name) const;
  Symbol *lookup_local(const std::string &name) const;

  //! Add a symbol table entry.
  //! Will throw SemanticError execption if this symbol table
  //! already has an entry with the same name.
  //! @param loc the Location of the type, function, or variable
  //! @param kind the SymbolTableKind
  //! @param name the name of the type, function, or variable
  //! @param type the Type of the type, function, or variable
  //! @return pointer to the Symbol representing the symbol table entry
  Symbol *add_entry(const Location &loc, SymbolKind kind, const std::string &name, std::shared_ptr<Type> type);

  // Get number of symbol table entries.
  unsigned get_num_entries() const;

  // Access a symbol table entry by its position.
  // This is useful for accessing symbol table entries representing
  // function parameters.
  Symbol *get_entry(unsigned index) const;

  // For a symbol table representing a function declaration or definition,
  // return the number of parameters
  unsigned get_num_parameters() const;

  // Iterate through the symbol table entries in the order in which they were added
  // to the symbol table. This is important for struct types, where the representation
  // of the type needs to record the fields (and their types) in the exact order
  // in which they appeared in the source code.
  typedef std::vector<Symbol *>::const_iterator const_iterator;
  const_iterator cbegin() const { return m_symbols.cbegin(); }
  const_iterator cend() const { return m_symbols.cend(); }

  // Operations that search recursively starting from the current (local)
  // scope and expanding to outer scopes as necessary
  Symbol *lookup_recursive(const std::string &name) const;

  // This can be called on the symbol table representing the function parameter
  // scope of a function to record the exact type of the function
  void set_fn_type(std::shared_ptr<Type> fn_type);

  //! Check whether this symbol table has a function type
  //! (i.e., whether the symbol table represents a declaration or
  //! definition of a function).
  bool has_fn_type() const { return bool(m_fn_type); }

  // This returns the function type of the enclosing function, or nullptr
  // if there is no enclosing function. This is helpful for type checking
  // a return statement, to make sure that the value returned is
  // assignment-compatible with the function's return type.
  std::shared_ptr<Type> get_fn_type() const;

  int get_depth() const;

private:
  // TODO: add helper functions
};

#endif // SYMTAB_H
