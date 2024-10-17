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

#include <cassert>
#include <map>
#include <memory>
#include "debugvar.h"
#include "lowlevel.h"
#include "peephole_ll.h"

namespace {

bool DEBUG_PEEPHOLE_LL;
DebugVar d("DEBUG_PEEPHOLE_LL", DEBUG_PEEPHOLE_LL);

}

////////////////////////////////////////////////////////////////////////
// Window matching and rewriting
////////////////////////////////////////////////////////////////////////

namespace {

// These are used for naming instances of instructions and
// operands in patterns and replacements.
const char A = 'A';
const char B = 'B';
const char C = 'C';
const char D = 'D';
const char E = 'E';
const char F = 'F';
const char G = 'G';
const char H = 'H';

// Name used to indicate that we don't care about recording
// the result of a match
const char DONT_CARE = '_';

////////////////////////////////////////////////////////////////////////
// MatchContext keeps track of opcodes and operands
// matched in patterns.
////////////////////////////////////////////////////////////////////////

class MatchContext {
private:
  std::map<char, LowLevelOpcode> m_opcode_matches;
  std::map<char, Operand> m_operand_matches;

public:
  MatchContext() { }
  ~MatchContext() { }

  template<typename Map>
  bool has_match(const Map &m, char name) const {
    assert(name != DONT_CARE);
    return m.count(name) > 0;
  }

  template<typename Map>
  typename Map::mapped_type get_match(const Map &m, char name) const {
    assert(name != DONT_CARE);
    auto i = m.find(name);
    assert(i != m.end());
    return i->second;
  }

  template<typename Map>
  void set_match(Map &m, char name, typename Map::mapped_type val) {
    assert(name != DONT_CARE);
    assert(!has_match(m, name));
    m[name] = val;
  }

  bool has_opcode_match(char name) const                    { return has_match(m_opcode_matches, name); }
  void set_opcode_match(char name, LowLevelOpcode opcode)   { set_match(m_opcode_matches, name, opcode); }
  LowLevelOpcode get_opcode_match(char name) const          { return get_match(m_opcode_matches, name); }

  bool has_operand_match(char name) const                   { return has_match(m_operand_matches, name); }
  void set_operand_match(char name, Operand operand)        { set_match(m_operand_matches, name, operand); }
  Operand get_operand_match(char name) const                { return get_match(m_operand_matches, name); }
};

////////////////////////////////////////////////////////////////////////
// Base class for opcode matchers.
////////////////////////////////////////////////////////////////////////

class MatchOpcode {
public:
  virtual ~MatchOpcode();
  virtual bool match(LowLevelOpcode opcode, MatchContext &ctx) const = 0;
};

MatchOpcode::~MatchOpcode() {
}

////////////////////////////////////////////////////////////////////////
// Match a range of opcodes.
////////////////////////////////////////////////////////////////////////

class MatchOpcodeRange : public MatchOpcode {
private:
  int m_ll_opcode;
  int m_range_size;
  char m_name;

public:
  MatchOpcodeRange(int ll_opcode, int range_size, char name);
  virtual ~MatchOpcodeRange();
  virtual bool match(LowLevelOpcode opcode, MatchContext &ctx) const;
};

MatchOpcodeRange::MatchOpcodeRange(int ll_opcode, int range_size, char name)
  : m_ll_opcode(ll_opcode)
  , m_range_size(range_size)
  , m_name(name) {
}

MatchOpcodeRange::~MatchOpcodeRange() {
}

bool MatchOpcodeRange::match(LowLevelOpcode opcode, MatchContext &ctx) const {
  bool matches = int(opcode) >= m_ll_opcode && int(opcode) < m_ll_opcode + m_range_size;
  if (matches && m_name != DONT_CARE)
    ctx.set_opcode_match(m_name, opcode);
  return matches;
}

#if 0
////////////////////////////////////////////////////////////////////////
// Match a previously-matched opcode.
////////////////////////////////////////////////////////////////////////

class MatchPreviousOpcode : public MatchOpcode {
private:
  char m_name;
public:
  MatchPreviousOpcode(char name);
  virtual ~MatchPreviousOpcode();
  virtual bool match(LowLevelOpcode opcode, MatchContext &ctx) const;
};

MatchPreviousOpcode::MatchPreviousOpcode(char name)
  : m_name(name) {
  assert(m_name != DONT_CARE);
}

MatchPreviousOpcode::~MatchPreviousOpcode() {
}

bool MatchPreviousOpcode::match(LowLevelOpcode opcode, MatchContext &ctx) const {
  LowLevelOpcode prev_opcode = ctx.get_opcode_match(m_name);
  return opcode == prev_opcode;
}
#endif

////////////////////////////////////////////////////////////////////////
// Match a "well behaved" ALU opcode of a specified size.
// Note that the size is 1=8 bit, 2=16 bit, 4=32 bit, 8=64 bit.
// In practice only 32 and 64 ALU instructions are generated.
////////////////////////////////////////////////////////////////////////

class MatchAluOpcode : public MatchOpcode {
private:
  char m_name;
  int m_size;
  bool m_require_commutative;

public:
  MatchAluOpcode(char name, int size, bool require_commutative = false);
  virtual ~MatchAluOpcode();

  virtual bool match(LowLevelOpcode opcode, MatchContext &ctx) const;
};

MatchAluOpcode::MatchAluOpcode(char name, int size, bool require_commutative)
  : m_name(name)
  , m_size(size)
  , m_require_commutative(require_commutative) {
}

MatchAluOpcode::~MatchAluOpcode() {
}

bool MatchAluOpcode::match(LowLevelOpcode opcode, MatchContext &ctx) const {
  static const std::map<LowLevelOpcode, int> ALU_OPS = {
    { MINS_ADDL, 4 },
    { MINS_ADDQ, 8 },
    { MINS_SUBL, 4 },
    { MINS_SUBQ, 8 },
    { MINS_IMULL, 4 },
    { MINS_IMULQ, 8 },
  };

  if (m_require_commutative &&
      (opcode == MINS_SUBL || opcode == MINS_SUBQ))
    return false;

  auto i = ALU_OPS.find(opcode);
  if (i == ALU_OPS.end())
    return false;

  if (i->second != m_size)
    return false;

  // Match!

  if (m_name == DONT_CARE)
    return true;

  // If this is the first match, register it.
  if (!ctx.has_opcode_match(m_name)) {
    ctx.set_opcode_match(m_name, opcode);
    return true;
  }

  // Otherwise, make sure this opcode matches the previous one
  return opcode == ctx.get_opcode_match(m_name);
}

////////////////////////////////////////////////////////////////////////
// Base class for matching operands.
////////////////////////////////////////////////////////////////////////

class MatchOperand {
public:
  virtual ~MatchOperand();
  virtual bool match(Operand operand, MatchContext &ctx) const = 0;
};

MatchOperand::~MatchOperand() {
}

////////////////////////////////////////////////////////////////////////
// Match a machine register operand.
////////////////////////////////////////////////////////////////////////

class MatchMreg : public MatchOperand {
private:
  char m_name;
  bool m_want_memref;
public:
  MatchMreg(char name, bool want_memref = false);
  virtual ~MatchMreg();

  virtual bool match(Operand operand, MatchContext &ctx) const;

private:
  bool is_mreg(Operand operand) const {
    return operand.get_kind() >= Operand::MREG8 && operand.get_kind() <= Operand::MREG64;
  }
};

MatchMreg::MatchMreg(char name, bool want_memref)
  : m_name(name)
  , m_want_memref(want_memref) {
}

MatchMreg::~MatchMreg() {
}

bool MatchMreg::match(Operand operand, MatchContext &ctx) const {
  // Make sure we have the desired kind of operand
  if (m_want_memref && operand.get_kind() != Operand::MREG64_MEM)
    return false;
  if (!m_want_memref && !is_mreg(operand))
    return false;

  // If there was no previous match of an operand with this name,
  // then record the match
  if (!ctx.has_operand_match(m_name)) {
    ctx.set_operand_match(m_name, operand);
    return true;
  }

  // There was a previous match of an operand with this name,
  // so check to see whether the new operand is the same as the
  // previously matched one.
  Operand prev_operand = ctx.get_operand_match(m_name);
  return prev_operand.get_base_reg() == operand.get_base_reg();
}

////////////////////////////////////////////////////////////////////////
// Match a specific immediate integer operand
////////////////////////////////////////////////////////////////////////

class MatchSpecificImmediate : public MatchOperand {
private:
  long m_imm_ival;

public:
  MatchSpecificImmediate(long imm_ival);
  virtual ~MatchSpecificImmediate();

  virtual bool match(Operand operand, MatchContext &ctx) const;
};

MatchSpecificImmediate::MatchSpecificImmediate(long imm_ival)
  : m_imm_ival(imm_ival) {
}

MatchSpecificImmediate::~MatchSpecificImmediate() {
}

bool MatchSpecificImmediate::match(Operand operand, MatchContext &ctx) const {
  return operand.get_kind() == Operand::IMM_IVAL && operand.get_imm_ival() == m_imm_ival;
}

////////////////////////////////////////////////////////////////////////
// Match an immediate integer operand and record the match
// in the context (and verify that other references to the
// immediate have the same immediate value)
////////////////////////////////////////////////////////////////////////

class MatchImmediate : public MatchOperand {
private:
  char m_name;

public:
  MatchImmediate(char m_name);
  virtual ~MatchImmediate();

  virtual bool match(Operand operand, MatchContext &ctx) const;
};

MatchImmediate::MatchImmediate(char name)
  : m_name(name) {
}

MatchImmediate::~MatchImmediate() {
}

bool MatchImmediate::match(Operand operand, MatchContext &ctx) const {
  if (operand.get_kind() != Operand::IMM_IVAL)
    return false;

  if (!ctx.has_operand_match(m_name)) {
    // first match of an immediate with this name
    ctx.set_operand_match(m_name, operand);
    return true;
  } else {
    // make sure this immediate matches the previously matched one
    // with the same name
    Operand prev = ctx.get_operand_match(m_name);
    return operand == prev;
  }
}

////////////////////////////////////////////////////////////////////////
// Match any operand (useful for situations where a source
// operand could be either an mreg or an immediate).
// The operand kind can optionally be specified.
////////////////////////////////////////////////////////////////////////

class MatchAny : public MatchOperand {
private:
  char m_name;
  int m_required_operand_kind;

public:
  MatchAny(char name, int required_operand_kind);
  virtual ~MatchAny();

  virtual bool match(Operand operand, MatchContext &ctx) const;
};

MatchAny::MatchAny(char name, int required_operand_kind = -1)
  : m_name(name)
  , m_required_operand_kind(required_operand_kind) {
}

MatchAny::~MatchAny() {
}

bool MatchAny::match(Operand operand, MatchContext &ctx) const {
  if (m_required_operand_kind >= 0 && m_required_operand_kind != int(operand.get_kind()))
    return false;

  if (!ctx.has_operand_match(m_name)) {
    // First match of an operand with this name
    ctx.set_operand_match(m_name, operand);
    return true;
  } else {
    // Make sure this operand matches the previously matched one
    Operand prev_operand = ctx.get_operand_match(m_name);
    return operand == prev_operand;
  }
}

////////////////////////////////////////////////////////////////////////
// Match an instruction
////////////////////////////////////////////////////////////////////////

class InstructionMatcher {
private:
  MatchOpcode *m_match_opcode;
  std::vector<MatchOperand *> m_match_operands;

public:
  InstructionMatcher(MatchOpcode *match_opcode, std::initializer_list<MatchOperand *> match_operands);

  ~InstructionMatcher();

  bool match(const Instruction *ins, MatchContext &ctx) const;
};

InstructionMatcher::InstructionMatcher(MatchOpcode *match_opcode, std::initializer_list<MatchOperand *> match_operands)
  : m_match_opcode(match_opcode)
  , m_match_operands(match_operands) {
}

InstructionMatcher::~InstructionMatcher() {
  delete m_match_opcode;
  for (auto i = m_match_operands.begin(); i != m_match_operands.end(); ++i)
    delete *i;
}

bool InstructionMatcher::match(const Instruction *ins, MatchContext &ctx) const {
  // Make sure number of operands matches
  if (ins->get_num_operands() != m_match_operands.size())
    return false;

  // See whether opcode matches
  if (!m_match_opcode->match(LowLevelOpcode(ins->get_opcode()), ctx))
    return false;

  // See whether operands match
  unsigned idx = 0;
  for (auto i = m_match_operands.begin(); i != m_match_operands.end(); ++i, ++idx) {
    const MatchOperand *match_operand = *i;
    Operand operand = ins->get_operand(idx);
    if (!match_operand->match(operand, ctx))
      return false;
  }

  // The instruction matches!
  return true;
}

////////////////////////////////////////////////////////////////////////
// Opcode generators
////////////////////////////////////////////////////////////////////////

// Base class
class GenerateOpcode {
public:
  virtual ~GenerateOpcode();
  virtual LowLevelOpcode get_opcode(const MatchContext &ctx) const = 0;
};

GenerateOpcode::~GenerateOpcode() {
}

// Previously matched opcode
class GenerateMatchedOpcode : public GenerateOpcode {
private:
  char m_name;
public:
  GenerateMatchedOpcode(char name);
  virtual ~GenerateMatchedOpcode();
  virtual LowLevelOpcode get_opcode(const MatchContext &ctx) const;
};

GenerateMatchedOpcode::GenerateMatchedOpcode(char name)
  : m_name(name) {
}

GenerateMatchedOpcode::~GenerateMatchedOpcode() {
}

LowLevelOpcode GenerateMatchedOpcode::get_opcode(const MatchContext &ctx) const {
  return ctx.get_opcode_match(m_name);
}

// Generate specific opcode
class GenerateSpecificOpcode : public GenerateOpcode {
private:
  LowLevelOpcode m_ll_opcode;

public:
  GenerateSpecificOpcode(LowLevelOpcode ll_opcode);
  virtual ~GenerateSpecificOpcode();
  virtual LowLevelOpcode get_opcode(const MatchContext &ctx) const;
};

GenerateSpecificOpcode::GenerateSpecificOpcode(LowLevelOpcode ll_opcode)
  : m_ll_opcode(ll_opcode) {
}

GenerateSpecificOpcode::~GenerateSpecificOpcode() {
}

LowLevelOpcode GenerateSpecificOpcode::get_opcode(const MatchContext &ctx) const {
  return m_ll_opcode;
}

// Generate a conditional jump instruction using the same
// decision as a previously matched set instruction.
class GenerateConditionalJumpFromSet : public GenerateOpcode {
private:
  char m_name;

public:
  GenerateConditionalJumpFromSet(char name);
  virtual ~GenerateConditionalJumpFromSet();
  virtual LowLevelOpcode get_opcode(const MatchContext &ctx) const;
};

GenerateConditionalJumpFromSet::GenerateConditionalJumpFromSet(char name)
  : m_name(name) {
}

GenerateConditionalJumpFromSet::~GenerateConditionalJumpFromSet() {
}

LowLevelOpcode GenerateConditionalJumpFromSet::get_opcode(const MatchContext &ctx) const {
  LowLevelOpcode set_opcode = ctx.get_opcode_match(m_name);

  // The conditional jump opcodes are in the same order as the
  // set opcodes
  int offset = int(set_opcode) - int(MINS_SETL);
  assert(offset >= 0 && offset < 6);
  LowLevelOpcode j_opcode = LowLevelOpcode(int(MINS_JL) + offset);
  return j_opcode;
}

////////////////////////////////////////////////////////////////////////
// Operand generators
////////////////////////////////////////////////////////////////////////

// Base class
class GenerateOperand {
public:
  virtual ~GenerateOperand();
  virtual Operand get_operand(const MatchContext &ctx) const = 0;
};

GenerateOperand::~GenerateOperand() {
}

// Generate a previously matched operand
class GenerateMatchedOperand : public GenerateOperand {
private:
  char m_name;
public:
  GenerateMatchedOperand(char name);
  virtual ~GenerateMatchedOperand();
  virtual Operand get_operand(const MatchContext &ctx) const;
};

GenerateMatchedOperand::GenerateMatchedOperand(char name)
  : m_name(name) {
}

GenerateMatchedOperand::~GenerateMatchedOperand() {
}

Operand GenerateMatchedOperand::get_operand(const MatchContext &ctx) const {
  return ctx.get_operand_match(m_name);
}

// Generate a fancy indexed/scaled memory reference
class GenerateIndexedMemref : public GenerateOperand {
private:
  char m_base_name, m_index_name;
  int m_scale;

public:
  GenerateIndexedMemref(char base_name, char index_name, int scale);
  virtual ~GenerateIndexedMemref();

  virtual Operand get_operand(const MatchContext &ctx) const;
};

GenerateIndexedMemref::GenerateIndexedMemref(char base_name, char index_name, int scale)
  : m_base_name(base_name)
  , m_index_name(index_name)
  , m_scale(scale) {
}

GenerateIndexedMemref::~GenerateIndexedMemref() {
}

Operand GenerateIndexedMemref::get_operand(const MatchContext &ctx) const {
  Operand base_reg = ctx.get_operand_match(m_base_name);
  Operand index_reg = ctx.get_operand_match(m_index_name);
  return Operand(Operand::MREG64_MEM_IDX_SCALE, base_reg.get_base_reg(), index_reg.get_base_reg(), m_scale);
}

// Generate a memory reference at an offset from a pointer in an mreg
class GenerateOffsetMemref : public GenerateOperand {
private:
  char m_base_name, m_off_name;

public:
  GenerateOffsetMemref(char base_name, char off_name);
  virtual ~GenerateOffsetMemref();

  virtual Operand get_operand(const MatchContext &ctx) const;
};

GenerateOffsetMemref::GenerateOffsetMemref(char base_name, char off_name)
  : m_base_name(base_name)
  , m_off_name(off_name) {
}

GenerateOffsetMemref::~GenerateOffsetMemref() {
}

Operand GenerateOffsetMemref::get_operand(const MatchContext &ctx) const {
  Operand base_reg = ctx.get_operand_match(m_base_name);
  assert(base_reg.get_kind() == Operand::MREG64);

  Operand off = ctx.get_operand_match(m_off_name);
  assert(off.get_kind() == Operand::IMM_IVAL);

  return Operand(Operand::MREG64_MEM_OFF, base_reg.get_base_reg(), off.get_imm_ival());
}

////////////////////////////////////////////////////////////////////////
// Generate an instruction using (potentially) opcode(s) and/or operand(s)
// matched in the original sequence of instructions.
////////////////////////////////////////////////////////////////////////

class InstructionTemplate {
private:
  GenerateOpcode *m_opcode_generator;
  std::vector<GenerateOperand *> m_operand_generators;

public:
  InstructionTemplate(GenerateOpcode *opcode_generator, std::initializer_list<GenerateOperand *> operand_generators);
  virtual ~InstructionTemplate();
  virtual Instruction *generate(const MatchContext &ctx) const;
};

InstructionTemplate::InstructionTemplate(GenerateOpcode *opcode_generator,
                                         std::initializer_list<GenerateOperand *> operand_generators)
  : m_opcode_generator(opcode_generator)
  , m_operand_generators(operand_generators) {
}

InstructionTemplate::~InstructionTemplate() {
  delete m_opcode_generator;
  for (auto i = m_operand_generators.begin(); i != m_operand_generators.end(); ++i)
    delete *i;
}

Instruction *InstructionTemplate::generate(const MatchContext &ctx) const {
  unsigned num_operands = m_operand_generators.size();

  // prepare operands
  Operand operands[3];
  for (unsigned i = 0; i < num_operands; ++i)
    operands[i] = m_operand_generators[i]->get_operand(ctx);

  return new Instruction(m_opcode_generator->get_opcode(ctx),
                         operands[0], operands[1], operands[2]);
}

////////////////////////////////////////////////////////////////////////
// Peephole matcher: a sequence of InstructionMatchers (to match an idiom
// in the generated code) and a sequence of InstructionTemplates (to
// generate equivalent but better code.)
////////////////////////////////////////////////////////////////////////

class PeepholeMatcher {
private:
  std::vector<InstructionMatcher *> m_instruction_matchers;
  std::vector<InstructionTemplate *> m_instruction_templates;
  std::string m_eliminated_assignments;
  std::string m_separate_locs;

public:
  PeepholeMatcher(std::initializer_list<InstructionMatcher *> instruction_matchers,
                  std::initializer_list<InstructionTemplate *> instruction_templates,
                  const std::string &eliminated_assignments = "",
                  const std::string &separate_locs = "");
  ~PeepholeMatcher();

  bool match(std::deque<Instruction *> &window,
             InstructionSequence *ll_iseq,
             const LiveMregs &live_mregs,
             std::shared_ptr<InstructionSequence> bb) const;
};

PeepholeMatcher::PeepholeMatcher(std::initializer_list<InstructionMatcher *> instruction_matchers,
                                 std::initializer_list<InstructionTemplate *> instruction_templates,
                                 const std::string &eliminated_assignments,
                                 const std::string &separate_locs)
  : m_instruction_matchers(instruction_matchers)
  , m_instruction_templates(instruction_templates)
  , m_eliminated_assignments(eliminated_assignments)
  , m_separate_locs(separate_locs) {
}

PeepholeMatcher::~PeepholeMatcher() {
  for (auto i = m_instruction_matchers.begin(); i != m_instruction_matchers.end(); ++i)
    delete *i;
  for (auto i = m_instruction_templates.begin(); i != m_instruction_templates.end(); ++i)
    delete *i;
}

bool PeepholeMatcher::match(std::deque<Instruction *> &window,
                            InstructionSequence *ll_iseq,
                            const LiveMregs &live_mregs,
                            std::shared_ptr<InstructionSequence> bb) const {
  // A match is only possible if the number of instructions in the window
  // is at least as large as the number of instruction matchers.
  if (window.size() < m_instruction_matchers.size())
    return false;

  MatchContext ctx;

  auto j = window.begin();

  // Apply each of the instruction matchers
  Instruction *last_matched_ins = nullptr;
  for (auto i = m_instruction_matchers.begin(); i != m_instruction_matchers.end(); ++i, ++j) {
    const InstructionMatcher *matcher = *i;
    Instruction *ins = *j;
    if (!matcher->match(ins, ctx))
      return false;
    last_matched_ins = ins;
  }

  if (!m_eliminated_assignments.empty()) {
    // Check the eliminated assignments and make sure that all of those
    // mregs are dead

    // Get set of live mregs after the last matched instruction
    LiveMregs::FactType live_after_last_matched_ins =
      live_mregs.get_fact_after_instruction(bb, last_matched_ins);

    // Make sure that registers to which we would be eliminating an
    // assignment are dead
    for (auto i = m_eliminated_assignments.begin(); i != m_eliminated_assignments.end(); ++i) {
      // Determine which mreg needs to be checked
      char name = *i;
      Operand mreg_operand = ctx.get_operand_match(name);
      assert(mreg_operand.get_kind() >= Operand::MREG8 && mreg_operand.get_kind() <= Operand::MREG64);

      if (live_after_last_matched_ins.test(mreg_operand.get_base_reg()))
        // this mreg is still live, so we can't eliminate an assignment to it
        return false;
    }

    // Make sure that any locations that must be separate for correctness
    // are definitely not the same register
    assert(m_separate_locs.size() % 2 == 0);
    for (unsigned i = 0; i < m_separate_locs.size(); i += 2) {
      char loc1_name = m_separate_locs[i];
      char loc2_name = m_separate_locs[i+1];

      Operand loc1 = ctx.get_operand_match(loc1_name);
      Operand loc2 = ctx.get_operand_match(loc2_name);

      if (loc1.get_kind() == loc2.get_kind()) {
        assert(!loc1.is_memref());
        assert(!loc1.is_imm_ival());
        if (loc1.get_base_reg() == loc2.get_base_reg())
          // Oops, these are the same register
          return false;
      }
    }
  }

  // All of the instructions matched, and we won't be eliminating any assignments
  // to mregs whose values will be needed later, so
  //   - remove the matched instructions from the window, and
  //   - generate instructions from the templates

  // FIXME: we should probably only preserve comments if there is only one comment

  std::string comment;

  unsigned num_matched = m_instruction_matchers.size();
  while (num_matched > 0) {
    if (comment.empty()) {
      Instruction *ins = window.front();
      if (ins->has_comment())
        comment = ins->get_comment();
    }
    window.pop_front();
    --num_matched;
  }

  bool added_comment = false;
  for (auto i = m_instruction_templates.begin(); i != m_instruction_templates.end(); ++i) {
    const InstructionTemplate *ins_template = *i;
    Instruction *gen_ins = ins_template->generate(ctx);
    if (!comment.empty() && !added_comment) {
      gen_ins->set_comment(comment);
      added_comment = true;
    }
    ll_iseq->append(gen_ins);
  }

  return true;
}

////////////////////////////////////////////////////////////////////////
// Helper functions to create instruction/opcode/operand matchers
// and instruction templates
////////////////////////////////////////////////////////////////////////

MatchOpcode *m_opcode(LowLevelOpcode ll_opcode, int size, char name) {
  return new MatchOpcodeRange(ll_opcode, size, name);
}

// Match an exact opcode, where we won't need to refer to it again
MatchOpcode *m_opcode(LowLevelOpcode ll_opcode) {
  return new MatchOpcodeRange(ll_opcode, 1, DONT_CARE);
}

/*
MatchOpcode *m_opcode(char name) {
  return new MatchPreviousOpcode(name);
}
*/

MatchOpcode *m_opcode_alu_l(char name) {
  return new MatchAluOpcode(name, 4);
}

MatchOpcode *m_opcode_alu_q(char name) {
  return new MatchAluOpcode(name, 8);
}

MatchOpcode *m_opcode_alu_l_comm(char name) {
  return new MatchAluOpcode(name, 4, true);
}

MatchOpcode *m_opcode_alu_q_comm(char name) {
  return new MatchAluOpcode(name, 8, true);
}

MatchOperand *m_mreg(char name) {
  return new MatchMreg(name);
}

MatchOperand *m_imm(long imm_ival) {
  return new MatchSpecificImmediate(imm_ival);
}

MatchOperand *m_imm_any(char name) {
  return new MatchImmediate(name);
}

MatchOperand *m_mreg_mem(char name) {
  return new MatchMreg(name, true);
}

MatchOperand *m_any(char name) {
  return new MatchAny(name);
}

MatchOperand *m_label(char name) {
  return new MatchAny(name, Operand::LABEL);
}

InstructionMatcher *matcher(MatchOpcode *match_opcode,
                            std::initializer_list<MatchOperand *> match_operands) {
  return new InstructionMatcher(match_opcode, match_operands);
}

GenerateOpcode *g_opcode(char name) {
  return new GenerateMatchedOpcode(name);
}

GenerateOpcode *g_opcode(LowLevelOpcode opcode) {
  return new GenerateSpecificOpcode(opcode);
}

GenerateOpcode *g_opcode_j_from_set(char name) {
  return new GenerateConditionalJumpFromSet(name);
}

GenerateOperand *g_prev(char name) {
  return new GenerateMatchedOperand(name);
}

GenerateOperand *g_mreg_mem_idx(char base_name, char index_name, int scale) {
  return new GenerateIndexedMemref(base_name, index_name, scale);
}

GenerateOperand *g_mreg_mem_off(char base_name, char off_name) {
  return new GenerateOffsetMemref(base_name, off_name);
}

InstructionTemplate *gen(GenerateOpcode *opcode_gen,
                         std::initializer_list<GenerateOperand *> operand_gens) {
  return new InstructionTemplate(opcode_gen, operand_gens);
}

////////////////////////////////////////////////////////////////////////
// Instantiated PeepholeMatchers
////////////////////////////////////////////////////////////////////////

#define pm(args...) std::unique_ptr<PeepholeMatcher>(new PeepholeMatcher(args))

std::unique_ptr<PeepholeMatcher> matchers[] = {
  // Get rid of do-nothing 32-bit r/r moves
  pm(
    // match instruction
    {
      matcher( m_opcode(MINS_MOVL), { m_mreg(A), m_mreg(A) } ),
    },

    // rewrite
    {
    }
  ),

  // Get rid of do-nothing 64-bit r/r moves
  pm(
    // match instruction
    {
      matcher( m_opcode(MINS_MOVQ), { m_mreg(A), m_mreg(A) } ),
    },

    // rewrite
    {
    }
  ),

  // TODO: add your own peephole rewrite patterns!
};

#undef pm

const unsigned NUM_MATCHERS = sizeof(matchers) / sizeof(matchers[0]);

}

////////////////////////////////////////////////////////////////////////
// PeepholeLowLevel implementation
////////////////////////////////////////////////////////////////////////

PeepholeLowLevel::PeepholeLowLevel(std::shared_ptr<ControlFlowGraph> cfg)
  : ControlFlowGraphTransform(cfg)
  , m_live_mregs(cfg)
  , m_num_matched(0) {
  // Compute liveness information about machine registers,
  // since this will be very important for determining which
  // transformations can be done safely
  m_live_mregs.execute();
}

PeepholeLowLevel::~PeepholeLowLevel() {
}

std::shared_ptr<InstructionSequence> PeepholeLowLevel::transform_basic_block(std::shared_ptr<InstructionSequence> orig_bb) {
  // Maximum number of instructions that are kept in the window
  const unsigned MAX_WINDOW_SIZE = 7;

  std::shared_ptr<InstructionSequence> result_iseq(new InstructionSequence());

  auto i = orig_bb->cbegin();

  // Keep going as long as either
  //   - the window has at least one instruction in it, or
  //   - there are instructions in the basic block that haven't
  //     been added to the window yet
  while (!m_window.empty() || i != orig_bb->cend()) {
    // Try to keep the window full
    while (i != orig_bb->cend() && m_window.size() < MAX_WINDOW_SIZE) {
      m_window.push_back(*i);
      ++i;
    }

    // Try to match a pattern
    bool found_match = false;
    for (unsigned j = 0; j < NUM_MATCHERS; ++j) {
      if (matchers[j]->match(m_window, result_iseq.get(), m_live_mregs, orig_bb)) {
        found_match = true;
        ++m_num_matched;
        break;
      }
    }

    if (!found_match) {
      // None of the patterns matched, so emit the earliest instruction
      emit_earliest_in_window(result_iseq.get());
    }
  }

  assert(m_window.empty());
  assert(i == orig_bb->cend());

  return result_iseq;
}

void PeepholeLowLevel::emit_earliest_in_window(InstructionSequence *result_iseq) {
  const Instruction *earliest = m_window.front();
  result_iseq->append(earliest->duplicate());
  m_window.pop_front();
}
