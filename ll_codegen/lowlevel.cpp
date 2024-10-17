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

#include <cassert>
#include "lowlevel.h"

const char *lowlevel_opcode_to_str(LowLevelOpcode opcode) {
  switch (opcode) {
  case MINS_NOP:
    return "nop";
  case MINS_MOVB:
    return "movb";
  case MINS_MOVW:
    return "movw";
  case MINS_MOVL:
    return "movl";
  case MINS_MOVQ:
    return "movq";
  case MINS_ADDB:
    return "addb";
  case MINS_ADDW:
    return "addw";
  case MINS_ADDL:
    return "addl";
  case MINS_ADDQ:
    return "addq";
  case MINS_SUBB:
    return "subb";
  case MINS_SUBW:
    return "subw";
  case MINS_SUBL:
    return "subl";
  case MINS_SUBQ:
    return "subq";
  case MINS_LEAQ:
    return "leaq";
  case MINS_JMP:
    return "jmp";
  case MINS_JE:
    return "je";
  case MINS_JNE:
    return "jne";
  case MINS_JL:
    return "jl";
  case MINS_JLE:
    return "jle";
  case MINS_JG:
    return "jg";
  case MINS_JGE:
    return "jge";
  case MINS_JB:
    return "jb";
  case MINS_JBE:
    return "jbe";
  case MINS_JA:
    return "ja";
  case MINS_JAE:
    return "jae";
  case MINS_CMPB:
    return "cmpb";
  case MINS_CMPW:
    return "cmpw";
  case MINS_CMPL:
    return "cmpl";
  case MINS_CMPQ:
    return "cmpq";
  case MINS_CALL:
    return "call";
  case MINS_IMULL:
    return "imull";
  case MINS_IMULQ:
    return "imulq";
  case MINS_IDIVL:
    return "idivl";
  case MINS_IDIVQ:
    return "idivq";
  case MINS_CDQ:
    return "cdq";
  case MINS_CQTO:
    return "cqto";
  case MINS_PUSHQ:
    return "pushq";
  case MINS_POPQ:
    return "popq";
  case MINS_RET:
    return "ret";
  case MINS_MOVSBW:
    return "movsbw";
  case MINS_MOVSBL:
    return "movsbl";
  case MINS_MOVSBQ:
    return "movsbq";
  case MINS_MOVSWL:
    return "movswl";
  case MINS_MOVSWQ:
    return "movswq";
  case MINS_MOVSLQ:
    return "movslq";
  case MINS_MOVZBW:
    return "movzbw";
  case MINS_MOVZBL:
    return "movzbl";
  case MINS_MOVZBQ:
    return "movzbq";
  case MINS_MOVZWL:
    return "movzwl";
  case MINS_MOVZWQ:
    return "movzwq";
  case MINS_MOVZLQ:
    return "movzlq";
  case MINS_SETL:
    return "setl";
  case MINS_SETLE:
    return "setle";
  case MINS_SETG:
    return "setg";
  case MINS_SETGE:
    return "setge";
  case MINS_SETE:
    return "sete";
  case MINS_SETNE:
    return "setne";
  default:
    assert(false);
    return nullptr;
  }
}

