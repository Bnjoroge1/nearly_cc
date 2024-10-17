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
#include <cstdio>
#include "exceptions.h"
#include "symtab.h"

////////////////////////////////////////////////////////////////////////
// Symbol implementation
////////////////////////////////////////////////////////////////////////

Symbol::Symbol(SymbolKind kind, const std::string &name, std::shared_ptr<Type> type, SymbolTable *symtab)
  : m_kind(kind)
  , m_name(name)
  , m_type(type)
  , m_symtab(symtab) {
}

Symbol::~Symbol() {
}

SymbolKind Symbol::get_kind() const {
  return m_kind;
}

const std::string &Symbol::get_name() const {
  return m_name;
}

std::shared_ptr<Type> Symbol::get_type() const {
  return m_type;
}

SymbolTable *Symbol::get_symtab() const {
  return m_symtab;
}

////////////////////////////////////////////////////////////////////////
// SymbolTable implementation
////////////////////////////////////////////////////////////////////////

SymbolTable::SymbolTable(SymbolTable *parent, const std::string &name)
  : m_parent(parent)
  , m_name(name) {
}

SymbolTable::~SymbolTable() {
  for (auto i = m_symbols.begin(); i != m_symbols.end(); ++i) {
    delete *i;
  }
}

SymbolTable *SymbolTable::get_parent() const {
  return m_parent;
}

const std::string &SymbolTable::get_name() const {
  return m_name;
}

bool SymbolTable::has_symbol_local(const std::string &name) const {
  return lookup_local(name) != nullptr;
}

Symbol *SymbolTable::lookup_local(const std::string &name) const {
  auto i = m_lookup.find(name);
  return (i != m_lookup.end()) ? m_symbols[i->second] : nullptr;
}

Symbol *SymbolTable::add_entry(const Location &loc, SymbolKind kind, const std::string &name, std::shared_ptr<Type> type) {
  assert(name != "");
  assert(type);

  if (m_lookup.find(name) != m_lookup.end())
    SemanticError::raise(loc, "Redefinition of '%s'", name.c_str());
  unsigned index = m_symbols.size();
  Symbol *sym = new Symbol(kind, name, type, this);
  m_symbols.push_back(sym);
  m_lookup[name] = index;

  return sym;
}

unsigned SymbolTable::get_num_entries() const {
  return m_symbols.size();
}

Symbol *SymbolTable::get_entry(unsigned index) const {
  assert(index < get_num_entries());
  return m_symbols[index];
}

unsigned SymbolTable::get_num_parameters() const {
  assert(m_fn_type);
  return m_fn_type->get_num_members();
}

Symbol *SymbolTable::lookup_recursive(const std::string &name) const {
  const SymbolTable *scope = this;

  while (scope != nullptr) {
    Symbol *sym = scope->lookup_local(name);
    if (sym != nullptr)
      return sym;
    scope = scope->get_parent();
  }

  return nullptr;
}

void SymbolTable::set_fn_type(std::shared_ptr<Type> fn_type) {
  assert(!m_fn_type);
  assert(fn_type->is_function());
  m_fn_type = fn_type;
}

std::shared_ptr<Type> SymbolTable::get_fn_type() const {
  const SymbolTable *symtab = this;
  while (symtab != nullptr) {
    if (symtab->m_fn_type)
      return symtab->m_fn_type;
    symtab = symtab->m_parent;
  }

  assert(false);
  return std::shared_ptr<Type>();
}

int SymbolTable::get_depth() const {
  int depth = 0;

  const SymbolTable *symtab = m_parent;
  while (symtab != nullptr) {
    ++depth;
    symtab = symtab->m_parent;
  }

  return depth;
}

// TODO: add helper functions
