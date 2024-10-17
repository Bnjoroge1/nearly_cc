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

#include "function.h"

Function::Function(const std::string &name, Node *funcdef_ast, Symbol *symbol)
  : m_name(name)
  , m_funcdef_ast(funcdef_ast)
  , m_symbol(symbol)
{
}

Function::~Function() {
}

std::string Function::get_name() const {
  return m_name;
}

Node *Function::get_funcdef_ast() const {
  return m_funcdef_ast;
}

Symbol *Function::get_symbol() const {
  return m_symbol;
}

std::shared_ptr<InstructionSequence> Function::get_hl_iseq() const {
  return m_hl_iseq;
}

void Function::set_hl_iseq(std::shared_ptr<InstructionSequence> hl_iseq) {
  m_hl_iseq = hl_iseq;
}

std::shared_ptr<InstructionSequence> Function::get_ll_iseq() const {
  return m_ll_iseq;
}

void Function::set_ll_iseq(std::shared_ptr<InstructionSequence> ll_iseq) {
  m_ll_iseq = ll_iseq;
}
