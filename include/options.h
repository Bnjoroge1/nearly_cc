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

#ifndef OPTIONS_H
#define OPTIONS_H

#include <string>
#include <map>

//! @file
//! Command line options handling.

//! Enumeration of IR kinds.
enum class IRKind {
  TOKENS=0,
  AST,              // implies parsing
  SYMBOL_TABLE,     // implies semantic analysis
  HIGHLEVEL_CODE,   // implies high-level codegen
  LOWLEVEL_CODE,    // implies low-level codegen
};

//! Code IR output formats.
//! These need to have higher ordinal values than IRKind members.
enum class CodeFormat {
  ASSEMBLY=100,     // normal assembly code output
  CFG,              // print code as function CFGs
  DATAFLOW_CFG,     // options will indicate which dataflow analysis
};

//! Command-line options.
class Options {
private:
  std::map<std::string, std::string> m_opts;
  IRKind m_ir_kind_goal;
  CodeFormat m_code_format_goal;

public:
  // names of command line options
  static constexpr const char *PRINT_TOKENS   = "-l";
  static constexpr const char *PRINT_AST      = "-p";
  static constexpr const char *PRINT_SYMTAB   = "-a";
  static constexpr const char *OPTIMIZE       = "-o";
  static constexpr const char *PRINT_CFG      = "-C";
  static constexpr const char *HIGHLEVEL      = "-h";
  static constexpr const char *PRINT_DATAFLOW = "-D";

  Options();
  ~Options();

  //! Parse command line arguments.
  //! @throw RuntimeError if provided command line options aren't valid
  //! @return the index of the first command line
  //!         argument that isn't an option (which in theory should be the
  //!         filename)
  int parse(int argc, char **argv);

  //! Check whether specific option was given on the command line.
  //! @return true if the named option was specified
  //!         on the command line, false otherwise
  bool has_option(const std::string &opt_name) const;

  //! Get the argument specified for the given option
  //! (if the option requires an argument). Should only be called
  //! if has_option indicates that the option was specified
  //! on the command line.
  //! @param opt_name name of a command-line option
  //! @return the argument given for the named command-line option
  std::string get_arg(const std::string &opt_name) const;

  //! Get the goal indicating what kind of intermediate representation
  //! the compiler is being asked to produce.
  //! @return the IRKind value indicating what kind of intermediate representation
  //!         should be generated
  IRKind get_ir_kind_goal() const { return m_ir_kind_goal; }

  //! Get the goal indicating the code format (relevant if the
  //! IR goal is high-level code or low-level code).
  //! @return the CodeFormat value indicating the format in which
  //!         the generated code should be printed
  CodeFormat get_code_format_goal() const { return m_code_format_goal; }

  //! Get the usage text.
  //! @return the usage text
  std::string get_usage() const;
};


#endif // OPTIONS_H
