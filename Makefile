# nearly_cc Makefile
# In theory you should not need to make any changes.

CXX = g++
CXXFLAGS = -g -Wall -std=c++20 -Iinclude -Ibuild
#ifdef SOLUTION
CXXFLAGS += -DSOLUTION
#endif

# Generated source files and header files.
# Note that these won't necessarily get built correctly
# unless you run "make depend" once before running "make".

GENERATED_SRCS = build/parse.tab.cpp build/lex.yy.cpp build/grammar_symbols.cpp \
	build/ast.cpp build/ast_visitor.cpp build/highlevel.cpp
GENERATED_HDRS = build/parse.tab.h build/lex.yy.h build/grammar_symbols.h \
	build/ast_visitor.h build/highlevel.h

# Source files.
# These get discovered automatically as long as they are in
# one of the known source directories (ast, driver, etc.)

AST_SRCS = $(shell ls ast/*.cpp)
DRIVER_SRCS = $(shell ls driver/*.cpp)
HL_CODEGEN_SRCS = $(shell ls hl_codegen/*.cpp)
LINEAR_IR_SRCS = $(shell ls linear_ir/*.cpp)
LL_CODEGEN_SRCS = $(shell ls ll_codegen/*.cpp)
SEMA_SRCS = $(shell ls sema/*.cpp)

SRCS = $(AST_SRCS) $(DRIVER_SRCS) $(HL_CODEGEN_SRCS) $(LINEAR_IR_SRCS) \
	$(LL_CODEGEN_SRCS) $(SEMA_SRCS) $(GENERATED_SRCS)

# Object files

AST_OBJS = $(AST_SRCS:ast/%.cpp=build/%.o)
DRIVER_OBJS = $(DRIVER_SRCS:driver/%.cpp=build/%.o)
HL_CODEGEN_OBJS = $(HL_CODEGEN_SRCS:hl_codegen/%.cpp=build/%.o)
LINEAR_IR_OBJS = $(LINEAR_IR_SRCS:linear_ir/%.cpp=build/%.o)
LL_CODEGEN_OBJS = $(LL_CODEGEN_SRCS:ll_codegen/%.cpp=build/%.o)
SEMA_OBJS = $(SEMA_SRCS:sema/%.cpp=build/%.o)
GENERATED_OBJS=$(GENERATED_SRCS:build/%.cpp=build/%.o)

OBJS = $(AST_OBJS) $(DRIVER_OBJS) $(HL_CODEGEN_OBJS) $(LINEAR_IR_OBJS) \
	$(LL_CODEGEN_OBJS) $(SEMA_OBJS) $(GENERATED_OBJS)

# Lexer and parser source files
LEXER_SRC = lex.l
PARSER_SRC = parse_buildast.y

EXE = nearly_cc

# Because the source files are in multiple directories,
# but all object files go in build/, we need one compilation
# rule per source directory. (Make doesn't seem to have any
# way to match more than one wildcard in a pattern rule.)

build/%.o : ast/%.cpp
	$(CXX) $(CXXFLAGS) -c $< -o build/$*.o

build/%.o : driver/%.cpp
	$(CXX) $(CXXFLAGS) -c $< -o build/$*.o

build/%.o : hl_codegen/%.cpp
	$(CXX) $(CXXFLAGS) -c $< -o build/$*.o

build/%.o : linear_ir/%.cpp
	$(CXX) $(CXXFLAGS) -c $< -o build/$*.o

build/%.o : ll_codegen/%.cpp
	$(CXX) $(CXXFLAGS) -c $< -o build/$*.o

build/%.o : sema/%.cpp
	$(CXX) $(CXXFLAGS) -c $< -o build/$*.o

build/%.o : build/%.cpp
	$(CXX) $(CXXFLAGS) -c $< -o build/$*.o

# Default target: build nearly_cc
$(EXE) : $(GENERATED_SRCS) $(GENERATED_HDRS) $(OBJS)
	$(CXX) -o $@ $(OBJS)

# Targets for generated source and header files

build/parse.tab.h build/parse.tab.cpp : $(PARSER_SRC)
	bison -v --output-file=build/parse.tab.cpp --defines=build/parse.tab.h $(PARSER_SRC)

build/lex.yy.cpp build/lex.yy.h : $(LEXER_SRC)
	flex --outfile=build/lex.yy.cpp --header-file=build/lex.yy.h $(LEXER_SRC)

build/grammar_symbols.h build/grammar_symbols.cpp : $(PARSER_SRC) scripts/scan_grammar_symbols.rb
	scripts/scan_grammar_symbols.rb < $(PARSER_SRC)

build/ast.cpp build/ast_visitor.h build/ast_visitor.cpp : include/ast.h scripts/gen_ast_code.rb
	scripts/gen_ast_code.rb < include/ast.h

build/highlevel.h build/highlevel.cpp : scripts/gen_highlevel_ir.rb
	scripts/gen_highlevel_ir.rb

# Build a solution zipfile to submit to Gradescope

solution.zip :
	zip -9r solution.zip include ast driver hl_codegen linear_ir ll_codegen sema scripts \
		*.y *.l Makefile README.txt

# Generate header file dependencies.
# Note that g++ -M doesn't know that the object files are meant
# to go in the "build" directory, so we fix its output.
depend : $(GENERATED_SRCS) $(GENERATED_HDRS)
	$(CXX) $(CXXFLAGS) -M $(SRCS) | scripts/fixdeps.rb > depend.mak

depend.mak :
	touch $@

clean :
	rm -f build/*.o depend.mak $(GENERATED_SRCS) $(GENERATED_HDRS) \
		build/parse.output $(EXE)

include depend.mak
