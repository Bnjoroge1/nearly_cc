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

#include <vector>
#include <utility>
#include <cassert>
#include "exceptions.h"
#include "cpputil.h"
#include "options.h"

namespace {

struct CommandLineOption {
  std::string name; // option name
  std::string help; // help text
  int goal;         // IRKind or CodeOutputFormat
  bool needs_arg;   // does option have a required argument
  std::vector<std::string> allowed_args;
  std::vector<std::string> allowed_args_help;

  CommandLineOption(const std::string &name, const std::string &help, int goal = -1)
    : name(name), help(help), goal(goal), needs_arg(false) {
  }

  CommandLineOption(const std::string &name, const std::string &help, int goal, std::initializer_list<std::string> allowed_args_info)
    : name(name), help(help), goal(goal), needs_arg(true) {
    for (auto i = allowed_args_info.begin(); i != allowed_args_info.end(); ++i) {
      // the elements are pairs of allowed arg value and help string
      allowed_args.push_back(*i);
      ++i;
      assert(i != allowed_args_info.end());
      allowed_args_help.push_back(*i);
    }
  }
};

const std::vector<CommandLineOption> OPTIONS = {
  { Options::PRINT_TOKENS, "print tokens", int(IRKind::TOKENS) },
  { Options::PRINT_AST, "print AST", int(IRKind::AST) },
  { Options::PRINT_SYMTAB, "print symbol tables", int(IRKind::SYMBOL_TABLE) },
  { Options::OPTIMIZE, "enable optimizations" },
  { Options::PRINT_CFG, "print control-flow graphs", int(CodeFormat::CFG) },
  { Options::HIGHLEVEL, "high-level code generation", int(IRKind::HIGHLEVEL_CODE) },
  { Options::PRINT_DATAFLOW, "print control-flow graphs with dataflow facts", int(CodeFormat::DATAFLOW_CFG), {
    "liveness", "registers containing live values",
    // If other kinds of dataflow values can be printed could go here
  }},
};

const CommandLineOption &find_option(const std::string &s) {
  for (auto i = OPTIONS.begin(); i != OPTIONS.end(); ++i) {
    const CommandLineOption &opt = *i;
    if (opt.name == s)
      return opt;
  }
  RuntimeError::raise("Unknown option '%s'", s.c_str());
}

} // end anonymous namespace

Options::Options()
  : m_ir_kind_goal(IRKind::LOWLEVEL_CODE)
  , m_code_format_goal(CodeFormat::ASSEMBLY) {
}

Options::~Options() {
}

int Options::parse(int argc, char **argv) {
  int i = 1;
  while (i < argc) {
    std::string s(argv[i]);
    if (s.empty() || s[0] != '-')
      break;

    const CommandLineOption &opt = find_option(s);
    std::string arg;
    if (opt.needs_arg) {
      ++i;
      if (i >= argc)
        RuntimeError::raise("Option '%s' requires an argument", s.c_str());
      arg = argv[i];

      // make sure argument value is one of the allowed ones
      bool found = false;
      for (auto j = opt.allowed_args.begin(); j != opt.allowed_args.end(); ++j)
        if (arg == *j)
          found = true;
      if (!found)
        RuntimeError::raise("Argument '%s' for '%s' option is not an allowed value", arg.c_str(), s.c_str());
    }

    m_opts[s] = arg;

    if (opt.goal >= int(CodeFormat::ASSEMBLY))
      m_code_format_goal = CodeFormat(opt.goal);
    else if (opt.goal >= int(IRKind::TOKENS))
      m_ir_kind_goal = IRKind(opt.goal);

    ++i;
  }

  return i;
}

bool Options::has_option(const std::string &opt_name) const {
  return m_opts.find(opt_name) != m_opts.end();
}

std::string Options::get_arg(const std::string &opt_name) const {
  auto i = m_opts.find(opt_name);
  assert(i != m_opts.end());
  return i->second;
}

std::string Options::get_usage() const {
  std::string usage;

  usage += "Usage: ./nearly_cc [<options>...] <filename>\n";
  usage += "Options:\n";

  for (auto i = OPTIONS.begin(); i != OPTIONS.end(); ++i) {
    const CommandLineOption &opt = *i;
    std::string line;
    line += "  ";

    std::string opt_desc = opt.name;
    if (opt.needs_arg) {
      opt_desc += " <arg>";
    }
    line += cpputil::format("%-12s", opt_desc.c_str());

    line += " ";
    line += opt.help;
    line += "\n";
    usage += line;
  }

  for (auto i = OPTIONS.begin(); i != OPTIONS.end(); ++i) {
    const CommandLineOption &opt = *i;
    if (!opt.needs_arg)
      continue;

    usage += cpputil::format("\nArgument values for '%s' option are:\n", opt.name.c_str());
    for (unsigned j = 0; j < opt.allowed_args.size(); ++j)
      usage += cpputil::format("  %s: %s\n", opt.allowed_args[j].c_str(), opt.allowed_args_help[j].c_str());
  }

  return usage;
}
