// Copyright (c) 2021-2023, David H. Hovemeyer <david.hovemeyer@gmail.com>
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

#ifndef LOWLEVEL_DEFUSE_H
#define LOWLEVEL_DEFUSE_H

#include <vector>
#include "lowlevel.h"
class Instruction;

namespace LowLevel {

bool is_def(Instruction *ins);

// Unfortunately, because an x86-64 instruction can modify
// multiple registers, we need a way of knowing explicitly
// *which* registers are modified by a def instruction
std::vector<MachineReg> get_def_mregs(Instruction *ins);

// A similar issue exists for uses: some instructions have
// implicit uses that aren't explicit operands
std::vector<MachineReg> get_use_mregs(Instruction *ins);

}

#endif // LOWLEVEL_DEFUSE_H
