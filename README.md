# NearlyCC: compiler skeleton for the "NearlyC" C subset

This project is a compiler framework for the [NearlyC](https://github.com/daveho/nearly_c)
C subset front end. It contains the following components (in addition to the code provided
by `nearly_c`):

* A [representation for C data types](https://daveho.github.io/nearly_cc/classType.html)
* [Symbol tables](https://daveho.github.io/nearly_cc/classSymbolTable.html)
* [Linear intermediate representation](https://daveho.github.io/nearly_cc/classInstructionSequence.html)
  for both "high-level" code (RISC-like, unlimited virtual registers)
  and low-level x86-64 code
* [Control-flow graphs](https://daveho.github.io/nearly_cc/classControlFlowGraph.html)
* [Generic dataflow analysis framework](https://daveho.github.io/nearly_cc/classDataflow.html),
  with provided implementations for live values on both
  [high-level](https://daveho.github.io/nearly_cc/classLiveVregsAnalysis.html) and
  [low-level](https://daveho.github.io/nearly_cc/classLiveMregsAnalysis.html)
  IR
* [Driver program](https://daveho.github.io/nearly_cc/main_8cpp.html) with
  [command line option handling](https://daveho.github.io/nearly_cc/classOptions.html)

It omits implementation of the following components:

* Semantic analysis (type checking, building symbol tables and annotating
  AST nodes with pointers to symbol table entries, insertion of implicit conversions)
* Code generation
* Optimization

[API documentation](https://daveho.github.io/nearly_cc) is available.
(This is in progress, but should be complete soon.)

This project was developed for the 601.428 Compilers and Interpreters
course at [Johns Hopkins University](http://www.jhu.edu/). The
[Fall 2023 course web page](https://jhucompilers.github.io/fall2023)
has the assignments in which students implement the missing parts.
(Note that the code in this repository has been refactored quite a bit,
so some minor details have changed.)  The Fall 2023 course also has
a [test repository](https://github.com/jhucompilers.github.io/fall2023-tests)
which is useful for testing code generation. (The `assign04` and
`assign05` subdirectories have the code generation tests.)

This repository is intended to be the official "public" release of
NearlyCC. It is in a somewhat preliminary form currently, but is
definitely at a point where it is useful. I will be adding documentation
and making improvements in the future.

Please send comments to <mailto:daveho@cs.jhu.edu>.
