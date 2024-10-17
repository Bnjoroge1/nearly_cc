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

#ifndef PRINT_INSTRUCTION_SEQ_H
#define PRINT_INSTRUCTION_SEQ_H

#include "instruction.h"
#include "instruction_seq.h"

// Default annotator for instructions: returns the Instruction's comment.
class DefaultInstructionAnnotator {
public:
  std::string get_instruction_annotation(std::shared_ptr<InstructionSequence> iseq, Instruction *ins) const {
    return ins->get_comment();
  }
};

// Print an InstructionSequence to stdout.
// The Formatter should be HighLevelFormatter or LowLevelFormatter,
// depending on whether the InstructionSequence has high-level or
// low-level instructions. The Annotator (which defaults to
// DefaultInstructionAnnotator) returns text "annotations" which
// (if nonempty) are printed as a comment immediately following
// the instruction.
template<typename Formatter, typename Annotator = DefaultInstructionAnnotator>
class PrintInstructionSequence {
private:
  Formatter m_formatter;
  Annotator m_annotator;

public:
  PrintInstructionSequence(Formatter formatter = Formatter(), Annotator annotator = Annotator());
  ~PrintInstructionSequence();

  void print(std::shared_ptr<InstructionSequence> iseq);
};

template<typename Formatter, typename Annotator>
PrintInstructionSequence<Formatter, Annotator>::PrintInstructionSequence(Formatter formatter, Annotator annotator)
  : m_formatter(formatter)
  , m_annotator(annotator) {
}

template<typename Formatter, typename Annotator>
PrintInstructionSequence<Formatter, Annotator>::~PrintInstructionSequence() {
}

template<typename Formatter, typename Annotator>
void PrintInstructionSequence<Formatter, Annotator>::print(std::shared_ptr<InstructionSequence> iseq) {
  for (auto i = iseq->cbegin(); i != iseq->cend(); i++) {
    // print label if there is one
    if (i.has_label()) {
      printf("%s:\n", i.get_label().c_str());
    }

    // print formatted instruction
    std::string formatted_ins = m_formatter.format_instruction(*i);
    printf("\t%s", formatted_ins.c_str());

    // if there is an "annotation" for the instruction, print it
    std::string annotation = m_annotator.get_instruction_annotation(iseq, *i);
    if (!annotation.empty()) {
      size_t len = formatted_ins.size();
      if (len < 28) {
        printf("%s", "                            " + len);
      }
      printf(" /* %s */", annotation.c_str());
    }

    printf("\n");
  }
}

#endif // PRINT_INSTRUCTION_SEQ_H
