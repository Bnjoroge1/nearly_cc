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

#ifndef INSTRUCTION_H
#define INSTRUCTION_H

#include <string>
#include <vector>
#include "symtab.h"
#include "operand.h"

//! Instruction object type.
//! This is a traditional "quad"-style instruction representation.
//! Can be used for either high-level or low-level code.
class Instruction {
private:
  int m_opcode;
  std::vector<Operand> m_operands;
  std::string m_comment;
  Symbol *m_symbol;

public:
  //! Contructor from opcode.
  //! The initialized Instruction object will not have
  //! any operands.
  //! @param opcode the high-level or low-level opcode value
  Instruction(int opcode);

  //! Constructor from opcode and one Operand.
  //! @param opcode the high-level or low-level opcode value
  //! @param op1 the first (and only) Operand
  Instruction(int opcode, const Operand &op1);

  //! Constructor from opcode and two Operands.
  //! @param opcode the high-level or low-level opcode value
  //! @param op1 the first Operand
  //! @param op2 the second Operand
  Instruction(int opcode, const Operand &op1, const Operand &op2);

  //! Constructor from opcode and up to three operands.
  //! @param opcode the high-level or low-level opcode value
  //! @param op1 the first Operand
  //! @param op2 the second Operand
  //! @param op3 the third Operand
  //! @param num_operands the number of operands (defaults to 3)
  Instruction(int opcode, const Operand &op1, const Operand &op2, const Operand &op3);

  ~Instruction();

  //! Return an exact duplicate of this Instruction.
  //! @return an exact duplicate of this Instruction
  Instruction *duplicate() const { return new Instruction(*this); }

  //! Get the opcode value.
  //! In general, this will be the ordinal value of either a HighLevelOpcode
  //! or LowLevelOpcode enumeration member, depending on whether the
  //! object is a high-level or low-level Instuctiion.
  //! @return the opcode value
  int get_opcode() const;

  //! Get the number of operands.
  //! @return the number of operands
  unsigned get_num_operands() const;

  //! Get the specified Operand.
  //! @param index the index of the Operand to return (0 for first, etc.)
  //! @return the specified Operand
  const Operand &get_operand(unsigned index) const;

  //! Modify the specified Operand.
  //! @param index the Operand to modify (0 for first, etc.)
  //! @param operand the value to copy to the specified Operand of the Instruction
  void set_operand(unsigned index, const Operand &operand);

  //! Return a copy of the last (rightmost) Operand.
  //! @return a copy of the last (rightmost) Operand
  Operand get_last_operand() const;

  //! Set a textual comment for this Instruction.
  //! The comment will appear in the printed representation of the Instruction.
  //! This is useful for debugging. For example, when a high-level Instruction
  //! is translated to a sequence of low-level Instructions, a string
  //! representation of the high-level instruction can be set as a comment
  //! for the first low-level Instruction.
  //!
  //! @param comment the comment to set
  void set_comment(const std::string &comment) { m_comment = comment; }

  //! Check whether this Instruction has a comment set.
  //! @return true if this Instruction has a comment set, false otherwise
  bool has_comment() const { return !m_comment.empty(); }

  //! Get the textual comment for this Instruction.
  //! @return the comment for this instruction (empty sting if there is no comment)
  const std::string &get_comment() const { return m_comment; }

  //! Set a symbol table entry (Symbol).
  //! This is useful for function calls, to link the function call
  //! with information (such as the FunctionType) of the called function.
  //!
  //! @param sym the Symbol to set
  void set_symbol(Symbol *sym) { m_symbol = sym; }

  //! Return the symbol table entry (Symbol).
  //! @return a pointer to the symbol table entry (Symbol), or a null pointer
  //!         if the Instruction doesn't have a symbol table entry set
  Symbol *get_symbol() const { return m_symbol; }
};

#endif // INSTRUCTION_H
