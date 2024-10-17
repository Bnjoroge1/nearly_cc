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

#include <cstdio>
#include <set>
#include "node.h"
#include "ast.h"
#include "parse.tab.h"
#include "lex.yy.h"
#include "parser_state.h"
#include "unit.h"
#include "semantic_analysis.h"
#include "local_storage_allocation.h"
#include "highlevel_codegen.h"
#include "lowlevel_codegen.h"
#include "highlevel_opt.h"
#include "lowlevel_opt.h"
#include "highlevel_formatter.h"
#include "lowlevel_formatter.h"
#include "print_instruction_seq.h"
#include "cfg_builder.h"
#include "cfg_printer.h"
#include "live_vregs.h"
#include "live_mregs.h"
#include "exceptions.h"
#include "options.h"

//! @file
//! Driver program for `nearly_cc`.

namespace { // anonymous namespace for helper functions

struct CloseFile {
  void operator()(FILE *in) {
    if (in != nullptr) {
      fclose(in);
    }
  }
};

// Template function to process the source file by instantiating
// the lexer and parser. The callback function can then
// use the lexer (and if desired, parser) to make use of the
// contents of the source file.
template<typename Fn>
void process_source_file(const std::string &filename, Fn fn) {
  // open the input source file
  std::unique_ptr<FILE, CloseFile> in(fopen(filename.c_str(), "r"));
  if (!in) {
    RuntimeError::raise("Couldn't open '%s'", filename.c_str());
  }

  // create and initialize ParserState; note that its destructor
  // will take responsibility for cleaning up the lexer state
  std::unique_ptr<ParserState> pp(new ParserState);
  pp->cur_loc = Location(filename, 1, 1);

  // prepare the lexer
  yylex_init(&pp->scan_info);
  yyset_in(in.get(), pp->scan_info);

  // make the ParserState available from the lexer state
  yyset_extra(pp.get(), pp->scan_info);

  // use the ParserState to either scan tokens or parse the input
  // to build an AST
  fn(pp.get());
}

// Process a source file by just reading all of the tokens
// and adding them to a vector.
void scan_tokens(const std::string &filename, std::vector<Node *> &tokens) {
  auto callback = [&](ParserState *pp) {
    YYSTYPE yylval;

    // the lexer will store pointers to all of the allocated
    // token objects in the ParserState, so all we need to do
    // is call yylex() until we reach the end of the input
    while (yylex(&yylval, pp->scan_info) != 0)
      ;

    std::copy(pp->tokens.begin(), pp->tokens.end(), std::back_inserter(tokens));
  };

  process_source_file(filename, callback);
}

// Process a source file by parsing it to produce an AST of
// the entire translation unit. Returns a pointer to the root
// of the AST.
Node *parse(const std::string &filename) {
  Node *ast = nullptr;

  auto callback = [&](ParserState *pp) {
    // parse the input source code
    yyparse(pp);

    // free memory allocated by flex
    yylex_destroy(pp->scan_info);

    ast = pp->parse_tree;

    // delete any Nodes that were created by the lexer,
    // but weren't incorporated into the parse tree
    std::set<Node *> tree_nodes;
    ast->preorder([&tree_nodes](Node *n) { tree_nodes.insert(n); });
    for (auto i = pp->tokens.begin(); i != pp->tokens.end(); ++i) {
      if (tree_nodes.count(*i) == 0) {
        delete *i;
      }
    }
  };

  process_source_file(filename, callback);

  return ast;
}

void print_tokens(const std::string &filename) {
  std::vector<Node *> tokens;

  scan_tokens(filename, tokens);

  for (auto i = tokens.begin(); i != tokens.end(); ++i) {
    Node *tok = *i;
    printf("%d:%s[%s]\n", tok->get_tag(), get_grammar_symbol_name(tok->get_tag()), tok->get_str().c_str());
    delete tok;
  }
}

void print_symbol_tables(SemanticAnalysis &sema) {
  // This *should* print consistent output independent of how
  // semantic analysis is implemented, as long as
  //
  //   - array parameter types are converted to the corresponding pointer types
  //   - a symbol table entry is created immediately when the semantic
  //     analyzer enters a scope
  //   - symbols are added in the order in which they are encountered
  //
  // Note that it is fine for the semantic analyzer to either
  //
  //   (1) "preemptively" create a function entry before its parameter
  //        types are fully known, and "fix" it later, or
  //   (2) wait to create a function entry until its parameter types
  //        are known

  // for each symbol table...
  for (auto i = sema.symtab_cbegin(); i != sema.symtab_cend(); ++i) {
    SymbolTable *symtab = *i;
    int depth = symtab->get_depth();

    // print name of symbol table
    printf("*** Symbol table %s ***\n", symtab->get_name().c_str());

    // for each symbol table entry...
    for (auto j = symtab->cbegin(); j != symtab->cend(); ++j) {
      // print a textual representation of the symbol table entry
      Symbol *sym = *j;
      printf("%d|", depth);
      printf("%s|", sym->get_name().c_str());
      switch (sym->get_kind()) {
      case SymbolKind::FUNCTION:
        printf("function|"); break;
      case SymbolKind::VARIABLE:
        printf("variable|"); break;
      case SymbolKind::TYPE:
        printf("type|"); break;
      default:
        assert(false);
      }

      printf("%s\n", sym->get_type()->as_str().c_str());
    }
  }
}

// Generate code for a function.
// Return value is the updated next label number
// (so that we can guarantee that label numbers aren't
// reused between functions in the same unit.)
int codegen(std::shared_ptr<Function> function, const Options &options, int next_label_num) {
  assert(options.get_ir_kind_goal() >= IRKind::HIGHLEVEL_CODE);

  // Assign
  //   - vreg numbers to parameters
  //   - local storage offsets to local variables requiring storage in
  //     memory
  //
  // This will also determine the total local storage requirements
  // for the function.
  //
  // Any local variable not assigned storage in memory can/should
  // be allocated a vreg as its storage.
  LocalStorageAllocation local_storage_alloc;
  local_storage_alloc.allocate_storage(function);

  // Generate high-level code
  HighLevelCodegen hl_codegen(options, next_label_num);
  hl_codegen.generate(function);

  // Optimizations on high-level IR (if optimizations are enabled)
  if (options.has_option(Options::OPTIMIZE)) {
    HighLevelOpt hl_opt(options);
    hl_opt.optimize(function);
  }

  if (options.get_ir_kind_goal() > IRKind::HIGHLEVEL_CODE) {
    assert(options.get_ir_kind_goal() == IRKind::LOWLEVEL_CODE);

    // Low-level code gen
    LowLevelCodeGen ll_codegen(options);
    ll_codegen.generate(function);

    // Optimizations on low-level IR (if optimizations are enabled)
    if (options.has_option(Options::OPTIMIZE)) {
      LowLevelOpt ll_opt(options);
      ll_opt.optimize(function);
    }
  }

  return hl_codegen.get_next_label_num();
}

void print_strconst_and_globals(Unit &unit) {
  if (unit.has_string_constants())
    printf("\n\t.section .rodata\n");

  for (auto i = unit.strconst_cbegin(); i != unit.strconst_cend(); ++i) {
    const StringConstant &strconst = *i;
    LiteralValue lv(strconst.get_content());
    printf("%s: .string \"%s\"\n", strconst.get_label().c_str(), lv.get_str_value_escaped().c_str());
  }

  if (unit.has_global_variables())
    printf("\n\t.section .bss\n");

  for (auto i = unit.globalvar_cbegin(); i != unit.globalvar_cend(); ++i) {
    std::string name = i->get_name();
    std::shared_ptr<Type> type = i->get_type();
    printf("\t.globl %s\n", name.c_str());
    printf("%s: .space %u\n", name.c_str(), type->get_storage_size());
  }
}

// Helper function for printing code (or CFGs) for all functions
// in the unit. The callback actually prints the code or CFG,
// given a shared_ptr to the Function and a reference to the Options.
template<typename CallbackFn>
void print_code(Unit &unit, CallbackFn print) {
  for (auto i = unit.fn_cbegin(); i != unit.fn_cend(); ++i) {
    std::shared_ptr<Function> fn = *i;
    std::string fn_name = fn->get_name();

    printf("\n\t.globl %s\n", fn_name.c_str());
    printf("%s:\n", fn_name.c_str());

    print(fn, unit.get_options());
  }
}

// Print assembly code for all functions in the Unit.
void print_assembly(Unit &unit) {
  const Options &options = unit.get_options();

  assert(options.get_code_format_goal() == CodeFormat::ASSEMBLY);

  if (options.get_ir_kind_goal() == IRKind::HIGHLEVEL_CODE)
    print_code(unit, [](std::shared_ptr<Function> fn, const Options &options) {
      // print high-level instructions
      PrintInstructionSequence<HighLevelFormatter> print_iseq_hl;
      std::shared_ptr<InstructionSequence> hl_iseq = fn->get_hl_iseq();
      assert(hl_iseq);
      print_iseq_hl.print(hl_iseq);
    });
  else
    print_code(unit, [](std::shared_ptr<Function> fn, const Options &options) {
      assert(options.get_ir_kind_goal() == IRKind::LOWLEVEL_CODE);
      // print low-level instructions
      PrintInstructionSequence<LowLevelFormatter> print_iseq_ll;
      std::shared_ptr<InstructionSequence> ll_iseq = fn->get_ll_iseq();
      assert(ll_iseq);
      print_iseq_ll.print(ll_iseq);
    });
}

// Print control-flow graphs for all functions in the Unit.
void print_cfg(Unit &unit) {
  const Options &options = unit.get_options();

  assert(unit.get_options().get_code_format_goal() == CodeFormat::CFG);

  if (options.get_ir_kind_goal() ==  IRKind::HIGHLEVEL_CODE)
    print_code(unit, [](std::shared_ptr<Function> fn, const Options &options) {
      // Print high-level control-flow graph
      auto hl_cfg_builder = ::make_highlevel_cfg_builder(fn->get_hl_iseq());
      auto hl_cfg = hl_cfg_builder.build();
      auto hl_cfg_printer = ::make_highlevel_cfg_printer(hl_cfg);
      hl_cfg_printer.print();
    });
  else
    print_code(unit, [](std::shared_ptr<Function> fn, const Options &options) {
      assert(options.get_ir_kind_goal() == IRKind::LOWLEVEL_CODE);
      // Print low-level control-flow graph
      auto ll_cfg_builder = ::make_lowlevel_cfg_builder(fn->get_ll_iseq());
      auto ll_cfg = ll_cfg_builder.build();
      auto ll_cfg_printer = ::make_lowlevel_cfg_printer(ll_cfg);
      ll_cfg_printer.print();
    });
}

// Print control-flow graphs annotated with dataflow facts for all functions
// in the Unit. The specific kind of dataflow analysis to run is based on
// the argument to the -D (Options::PRINT_DATAFLOW) option.
void print_dataflow_cfg(Unit &unit) {
  const Options &options = unit.get_options();

  // what kind of dataflow information to print
  std::string dataflow_kind = options.get_arg(Options::PRINT_DATAFLOW);

  if (options.get_ir_kind_goal() == IRKind::HIGHLEVEL_CODE) {
    // print dataflow results on high-level code

    auto callback = [dataflow_kind](std::shared_ptr<Function> fn, const Options &options) {
      // build the high-level control-flow graph
      auto hl_cfg_builder = ::make_highlevel_cfg_builder(fn->get_hl_iseq());
      auto hl_cfg = hl_cfg_builder.build();

      if (dataflow_kind == "liveness") {
        LiveVregs live_vregs(hl_cfg);
        live_vregs.execute();
        auto hl_cfg_printer = ::make_highlevel_cfg_printer(hl_cfg, DataflowAnnotator<LiveVregs>(live_vregs));
        hl_cfg_printer.print();
      } else {
        RuntimeError::raise("Dataflow kind '%s' on high-level code is not handled yet", dataflow_kind.c_str());
      }
    };

    print_code(unit, callback);
  } else {
    assert(options.get_ir_kind_goal() == IRKind::LOWLEVEL_CODE);
    // print dataflow results on high-level code

    auto callback = [dataflow_kind](std::shared_ptr<Function> fn, const Options &options) {
      // build the low-level control-flow graph
      auto ll_cfg_builder = ::make_lowlevel_cfg_builder(fn->get_ll_iseq());
      auto ll_cfg = ll_cfg_builder.build();

      if (dataflow_kind == "liveness") {
        LiveMregs live_mregs(ll_cfg);
        live_mregs.execute();
        auto ll_cfg_printer = ::make_lowlevel_cfg_printer(ll_cfg, DataflowAnnotator<LiveMregs>(live_mregs));
        ll_cfg_printer.print();
      } else {
        RuntimeError::raise("Dataflow kind '%s' on low-level code is not handled yet", dataflow_kind.c_str());
      }
    };

    print_code(unit, callback);
  }
}

// This function orchestrates parsing, semantic analysis, code generation,
// etc.  It returns the value that should be returned from main as the
// process's exit code.
int process_source_file(Options &options, const std::string &filename) {
  IRKind ir_kind_goal = options.get_ir_kind_goal();

  if (ir_kind_goal == IRKind::TOKENS) {
    // Just scan and print tokens
    print_tokens(filename);
    return 0;
  }

  // We are now committed to parsing and building an AST.
  // Create a Unit object to represent the entire
  // translation unit. The unit assumes ownership of
  // the AST.
  Node *ast = parse(filename);
  Unit unit(ast, options);

  if (ir_kind_goal == IRKind::AST) {
    // Just print the AST
    ASTTreePrint ptp;
    ptp.print(unit.get_ast());
    return 0;
  }

  // We are now committed to performing semantic analysis
  unit.get_semantic_analysis().visit(unit.get_ast());

  if (ir_kind_goal == IRKind::SYMBOL_TABLE) {
    // Just print contents of symbol tables
    print_symbol_tables(unit.get_semantic_analysis());
    return 0;
  }

  // At this point we are committed to doing some form of code
  // generation

  // TODO: find all of the string constants in the AST
  //       and add them to the Unit

  // Add global variables to the unit
  SymbolTable *global_symtab = unit.get_semantic_analysis().get_global_symtab();
  for (auto i = global_symtab->cbegin(); i != global_symtab->cend(); ++i) {
    Symbol *sym = *i;
    if (sym->get_kind() == SymbolKind::VARIABLE)
      unit.add_global_variable(GlobalVariable(sym->get_name(), sym->get_type()));
  }

  // Generate code for functions
  int next_label_num = 0;
  for (auto i = unit.get_ast()->cbegin(); i != unit.get_ast()->cend(); ++i) {
    Node *child = *i;
    if (child->get_tag() == AST_FUNCTION_DEFINITION) {
      std::string fn_name = child->get_kid(1)->get_str();
      Symbol *fn_sym = global_symtab->lookup_local(fn_name);
      assert(fn_sym != nullptr);

      // Create a Function to collect information about the function,
      // and provide a place to store intermediate representations,
      // storage allocation decisions, etc.
      std::shared_ptr<Function> function(new Function(fn_name, child, fn_sym));

      // Generate code!
      next_label_num = codegen(function, options, next_label_num);

      // Add to unit
      unit.add_function(function);
    }
  }

  // Print string constants and global variables
  // (these are the same regardless of code format)
  print_strconst_and_globals(unit);

  // Code is generated in the .text section
  if (unit.has_functions())
    printf("\n\t.section .text\n");

  // Now we can print the code of the unit (in whatever form it was requested)
  CodeFormat code_format_goal = options.get_code_format_goal();
  if (code_format_goal == CodeFormat::ASSEMBLY) {
    print_assembly(unit);
  } else if (code_format_goal == CodeFormat::CFG) {
    print_cfg(unit);
  } else {
    assert(code_format_goal == CodeFormat::DATAFLOW_CFG);
    print_dataflow_cfg(unit);
  }

  return 0;
}

} // end anonymous namespace

int main(int argc, char **argv) {
  // Parse command line options
  Options options;
  std::string filename;
  try {
    int index = options.parse(argc, argv);
    if (index != argc - 1)
      RuntimeError::raise("No filename specified");
    filename = argv[index];
  } catch (RuntimeError &ex) {
    fprintf(stderr, "Error: %s\n", ex.what());
    std::string usage = options.get_usage();
    fprintf(stderr, "%s", usage.c_str());
    return 1;
  }

  // Process the file (lex, parse, semantic analysis, codegen, etc.)
  try {
    return process_source_file(options, filename);
  } catch (BaseException &ex) {
    const Location &loc = ex.get_loc();
    if (loc.is_valid())
      fprintf(stderr, "%s:%d:%d:Error: %s\n", loc.get_srcfile().c_str(), loc.get_line(), loc.get_col(), ex.what());
    else
      fprintf(stderr, "Error: %s\n", ex.what());
    return 1;
  }
}
