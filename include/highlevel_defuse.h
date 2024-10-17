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

#ifndef HIGHLEVEL_DEFUSE_H
#define HIGHLEVEL_DEFUSE_H

class Instruction;

namespace HighLevel {

//! Check whether given high-level Instruction is a def,
//! i.e., an assignment to a virtual register.
//!
//! @param ins a high-level Instruction
//! @return true if the high-level Instruction is a def, false if not
bool is_def(Instruction *ins);

//! Get the register number of the virtual register assigned-to
//! by a def Instruction. This is *usually* the base register
//! of the first Operand. However, a `HINS_call` instruction should
//! be considered a def of `vr0`, the return value register,
//! even though it doesn't have an explicit Operand naming
//! that vreg.
//!
//! @param ins a high-level Instruction known to be a def
//! @return the register number of the assigned-to virtual register
int get_def_vreg(Instruction *ins);

//! Check whether a particular Operand of a high-level Instruction
//! is a use of a virtual register. The caller can call
//! `get_base_reg()` on the Operand object to find the register
//! number of the used virtual register.
//!
//! @param ins a high-level Instruction
//! @param operand_index index of operand to check
//! @return true if the specified operand is a use, false if not
bool is_use(Instruction *ins, unsigned operand_index);

};

#endif // HIGHLEVEL_DEFUSE_H
