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

#ifndef LOWLEVEL_OPT_H
#define LOWLEVEL_OPT_H

#include <memory>
#include "options.h"
#include "function.h"

//! LowLevelOpt is responsible for doing optimizations
//! on the low-level (x64-64) IR for a Function.
class LowLevelOpt {
private:
  // command line options
  const Options &m_options;

  // helper functions can use this to access the Function
  std::shared_ptr<Function> m_function;

  // no value semantics
  LowLevelOpt(const LowLevelOpt &);
  LowLevelOpt &operator=(const LowLevelOpt &);

public:
  //! Constructor.
  //! @param options the command-line options (useful for controlling
  //!                which optimizations are performed)
  LowLevelOpt(const Options &options);
  ~LowLevelOpt();

  //! Optimize the high-level IR for given Function.
  //! The unoptimized low-level InstructionSequence should be replaced
  //! with an optimized (more efficient) low-level InstructionSequence.
  //! @param function the Function containing the low-level InstructionSequence
  //!                 to optimize
  void optimize(std::shared_ptr<Function> function);
};

#endif // LOWLEVEL_OPT_H
