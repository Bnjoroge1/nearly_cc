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
#include "node.h"
#include "instruction.h"
#include "operand.h"
#include "local_storage_allocation.h"
#include "highlevel.h"
#include "lowlevel.h"
#include "highlevel_formatter.h"
#include "exceptions.h"
#include "lowlevel_codegen.h"

// This map has some "obvious" translations of high-level opcodes to
// low-level opcodes.
const std::map<HighLevelOpcode, LowLevelOpcode> HL_TO_LL = {
  { HINS_nop, MINS_NOP},
  { HINS_add_b, MINS_ADDB },
  { HINS_add_w, MINS_ADDW },
  { HINS_add_l, MINS_ADDL },
  { HINS_add_q, MINS_ADDQ },
  { HINS_sub_b, MINS_SUBB },
  { HINS_sub_w, MINS_SUBW },
  { HINS_sub_l, MINS_SUBL },
  { HINS_sub_q, MINS_SUBQ },
  { HINS_neg_b, MINS_XORB },
  { HINS_neg_w, MINS_XORW },
  { HINS_neg_l, MINS_XORL },
  { HINS_neg_q, MINS_XORQ },
  { HINS_mul_l, MINS_IMULL },
  { HINS_mul_q, MINS_IMULQ },
  { HINS_mov_b, MINS_MOVB },
  { HINS_mov_w, MINS_MOVW },
  { HINS_mov_l, MINS_MOVL },
  { HINS_mov_q, MINS_MOVQ },
  { HINS_sconv_bw, MINS_MOVSBW },
  { HINS_sconv_bl, MINS_MOVSBL },
  { HINS_sconv_bq, MINS_MOVSBQ },
  { HINS_sconv_wl, MINS_MOVSWL },
  { HINS_sconv_wq, MINS_MOVSWQ },
  { HINS_sconv_lq, MINS_MOVSLQ },
  { HINS_uconv_bw, MINS_MOVZBW },
  { HINS_uconv_bl, MINS_MOVZBL },
  { HINS_uconv_bq, MINS_MOVZBQ },
  { HINS_uconv_wl, MINS_MOVZWL },
  { HINS_uconv_wq, MINS_MOVZWQ },
  { HINS_uconv_lq, MINS_MOVZLQ },
  { HINS_ret, MINS_RET },
  { HINS_jmp, MINS_JMP },
  { HINS_call, MINS_CALL },

  // For comparisons, it is expected that the code generator will first
  // generate a cmpb/cmpw/cmpl/cmpq instruction to compare the operands,
  // and then generate a setXX instruction to put the result of the
  // comparison into the destination operand. These entries indicate
  // the apprpropriate setXX instruction to use.
  { HINS_cmplt_b, MINS_SETL },
  { HINS_cmplt_w, MINS_SETL },
  { HINS_cmplt_l, MINS_SETL },
  { HINS_cmplt_q, MINS_SETL },
  { HINS_cmplte_b, MINS_SETLE },
  { HINS_cmplte_w, MINS_SETLE },
  { HINS_cmplte_l, MINS_SETLE },
  { HINS_cmplte_q, MINS_SETLE },
  { HINS_cmpgt_b, MINS_SETG },
  { HINS_cmpgt_w, MINS_SETG },
  { HINS_cmpgt_l, MINS_SETG },
  { HINS_cmpgt_q, MINS_SETG },
  { HINS_cmpgte_b, MINS_SETGE },
  { HINS_cmpgte_w, MINS_SETGE },
  { HINS_cmpgte_l, MINS_SETGE },
  { HINS_cmpgte_q, MINS_SETGE },
  { HINS_cmpeq_b, MINS_SETE },
  { HINS_cmpeq_w, MINS_SETE },
  { HINS_cmpeq_l, MINS_SETE },
  { HINS_cmpeq_q, MINS_SETE },
  { HINS_cmpneq_b, MINS_SETNE },
  { HINS_cmpneq_w, MINS_SETNE },
  { HINS_cmpneq_l, MINS_SETNE },
  { HINS_cmpneq_q, MINS_SETNE },
};

LowLevelCodeGen::LowLevelCodeGen(const Options &options)
  : m_options(options)
  , m_total_memory_storage(0) {
}

LowLevelCodeGen::~LowLevelCodeGen() {
}

void LowLevelCodeGen::generate(std::shared_ptr<Function> function) {
  // Make the Function object available to member functions
  m_function = function;

  // The translation is done in the translate_hl_to_ll() member function
  std::shared_ptr<InstructionSequence> ll_iseq = translate_hl_to_ll(function->get_hl_iseq());
  m_function->set_ll_iseq(ll_iseq);
}

std::shared_ptr<InstructionSequence> LowLevelCodeGen::translate_hl_to_ll(std::shared_ptr<InstructionSequence> hl_iseq) {
    std::shared_ptr<InstructionSequence> ll_iseq(new InstructionSequence());

    // Ensure the function definition AST is present
    Node *funcdef_ast = m_function->get_funcdef_ast();
    assert(funcdef_ast != nullptr);

// Step 1: Determine the maximum virtual register used (ALL registers)
int max_vreg = 9;  // Start at 9 since vr0-vr9 are reserved
for (auto it = hl_iseq->cbegin(); it != hl_iseq->cend(); ++it) {
    Instruction *ins = *it;
    for (unsigned j = 0; j < ins->get_num_operands(); j++) {
        Operand op = ins->get_operand(j);
        if (op.get_kind() == Operand::VREG) {
            max_vreg = std::max(max_vreg, op.get_base_reg());
            //printf("max_vreg in: %d\n", max_vreg);
        }
    }
}
m_max_vreg = max_vreg;

// Need storage from VREG_FIRST_ARG (1) up to highest_var_reg
  int num_vregs_needing_storage = m_max_vreg - 10;  // This will give us storage for vr1-vr13
//printf("num_vregs_needing_storage: %d\n", num_vregs_needing_storage);
int vreg_storage = num_vregs_needing_storage * 8;
    
    // Step 3: Get local storage size from the Function object
    int local_storage_size = m_function->get_local_storage_size();
    //printf("local_storage_size: %d\n", local_storage_size);

    // Step 4: Calculate total storage needed
    m_total_memory_storage = local_storage_size + vreg_storage;
    //printf("m_total_memory_storage: %d\n", m_total_memory_storage);

    // Step 5: Align to 16 bytes
    if (m_total_memory_storage % 16 != 0) {
        m_total_memory_storage += (16 - (m_total_memory_storage % 16));
    }

    // Step 6: Determine base offset for vregs
    m_base_vreg_offset = -m_total_memory_storage;

    // Step 7: Translate each high-level instruction to low-level
    for (auto it = hl_iseq->cbegin(); it != hl_iseq->cend(); ++it) {
        Instruction *hl_ins = *it;

        // Define label if present
        if (it.has_label())
            ll_iseq->define_label(it.get_label());

        // Translate instruction
        unsigned ll_idx = ll_iseq->get_length();
        translate_instruction(hl_ins, ll_iseq);

        // Annotate with high-level instruction comment
        HighLevelFormatter hl_formatter;
        ll_iseq->get_instruction(ll_idx)->set_comment(hl_formatter.format_instruction(hl_ins));
    }

    return ll_iseq;
}
// These helper functions are provided to make it easier to handle
// the way that instructions and operands vary based on operand size
// ('b'=1 byte, 'w'=2 bytes, 'l'=4 bytes, 'q'=8 bytes.)

// Check whether hl_opcode matches a range of opcodes, where base
// is a _b variant opcode. Return true if the hl opcode is any variant
// of that base.
bool match_hl(int base, int hl_opcode) {
  return hl_opcode >= base && hl_opcode < (base + 4);
}

// For a low-level instruction with 4 size variants, return the correct
// variant. base_opcode should be the "b" variant, and operand_size
// should be the operand size in bytes (1, 2, 4, or 8.)
LowLevelOpcode select_ll_opcode(LowLevelOpcode base_opcode, int operand_size) {
  int off;

  switch (operand_size) {
  case 1: // 'b' variant
    off = 0; break;
  case 2: // 'w' variant
    off = 1; break;
  case 4: // 'l' variant
    off = 2; break;
  case 8: // 'q' variant
    off = 3; break;
  default:
    assert(false);
    off = 3;
  }

  return LowLevelOpcode(int(base_opcode) + off);
}

// Get the correct Operand::Kind value for a machine register
// of the specified size (1, 2, 4, or 8 bytes.)
Operand::Kind select_mreg_kind(int operand_size) {
  switch (operand_size) {
  case 1:
    return Operand::MREG8;
  case 2:
    return Operand::MREG16;
  case 4:
    return Operand::MREG32;
  case 8:
    return Operand::MREG64;
  default:
    assert(false);
    return Operand::MREG64;
  }
}

void LowLevelCodeGen::translate_instruction(Instruction *hl_ins, std::shared_ptr<InstructionSequence> ll_iseq) {
  HighLevelOpcode hl_opcode = HighLevelOpcode(hl_ins->get_opcode());
   // Handle mov instructions
    if (match_hl(HINS_mov_b, hl_opcode) || match_hl(HINS_mov_w, hl_opcode) || 
    match_hl(HINS_mov_l, hl_opcode) || match_hl(HINS_mov_q, hl_opcode)) {
    int size = highlevel_opcode_get_source_operand_size(hl_opcode);
    LowLevelOpcode mov_opcode;
  
    // Select correct move opcode based on size
    switch (size) {
        case 1: mov_opcode = MINS_MOVB; break;
        case 2: mov_opcode = MINS_MOVW; break;
        case 4: mov_opcode = MINS_MOVL; break;
        case 8: mov_opcode = MINS_MOVQ; break;
        default: RuntimeError::raise("Invalid size for mov instruction");
    }
    
    Operand src = get_ll_operand(hl_ins->get_operand(1), size, ll_iseq);
    Operand dest = get_ll_operand(hl_ins->get_operand(0), size, ll_iseq);
    // Special handling for argument registers (vr1-vr6)
    if (src.get_kind() == Operand::VREG && src.get_base_reg() >= 1 && src.get_base_reg() <= 6) {
        // Map vr1-vr6 to their corresponding argument registers
        
        MachineReg arg_reg;
        switch(src.get_base_reg()) {
            case 1: arg_reg = MREG_RDI; break;
            case 2: arg_reg = MREG_RSI; break;
            case 3: arg_reg = MREG_RDX; break;
            case 4: arg_reg = MREG_RCX; break;
            case 5: arg_reg = MREG_R8; break;
            case 6: arg_reg = MREG_R9; break;
            default: RuntimeError::raise("Invalid argument register");
        }
        // Use 32-bit version of the register for int parameters
        Operand arg_reg_op(Operand::MREG32, arg_reg);
        ll_iseq->append(new Instruction(mov_opcode, dest, arg_reg_op));
        return;
    }
    // Handle array operations first
if (hl_ins->get_operand(0).is_memref()) {
    // Destination is array element
    Operand r11(Operand::MREG64, MREG_R11);
    ll_iseq->append(new Instruction(MINS_LEAQ, dest, r11));
    
    Operand::Kind mreg_kind = select_mreg_kind(size);
    Operand r10(mreg_kind, MREG_R10);
    ll_iseq->append(new Instruction(mov_opcode, src, r10));
    ll_iseq->append(new Instruction(mov_opcode, r10, 
        Operand(Operand::MREG64_MEM, MREG_R11)));
    return;
}

// Then handle regular memory-to-memory moves
if (src.is_memref() && dest.is_memref()) {
    Operand::Kind mreg_kind = select_mreg_kind(size);
    Operand r10(mreg_kind, MREG_R10);  // Use R10, not R11
    ll_iseq->append(new Instruction(mov_opcode, src, r10));
    ll_iseq->append(new Instruction(mov_opcode, r10, dest));
    return;
}

// Direct move for everything else
ll_iseq->append(new Instruction(mov_opcode, src, dest));

    
    return;
}  if (match_hl(HINS_sub_b, hl_opcode)) {
    int size = highlevel_opcode_get_source_operand_size(hl_opcode);
    LowLevelOpcode sub_opcode = select_ll_opcode(MINS_SUBB, size);
    
    // Get operands
    Operand src1 = get_ll_operand(hl_ins->get_operand(1), size, ll_iseq);
    Operand src2 = get_ll_operand(hl_ins->get_operand(2), size, ll_iseq);
    Operand dest = get_ll_operand(hl_ins->get_operand(0), size, ll_iseq);
    
    // Load first operand into r10
    Operand::Kind mreg_kind = select_mreg_kind(size);
    Operand r10(mreg_kind, MREG_R10);
    ll_iseq->append(new Instruction(select_ll_opcode(MINS_MOVB, size), src1, r10));
    
    // Subtract second operand from r10
    ll_iseq->append(new Instruction(sub_opcode, src2, r10));
    
    // Store result
    ll_iseq->append(new Instruction(select_ll_opcode(MINS_MOVB, size), r10, dest));
    return;
}  if (hl_opcode == HINS_cmpgt_l) {
    // Extract operands
    Operand dest = get_ll_operand(hl_ins->get_operand(0), 4, ll_iseq);   // Result (32-bit)
    Operand left = get_ll_operand(hl_ins->get_operand(1), 4, ll_iseq);   // Left operand
    Operand right = get_ll_operand(hl_ins->get_operand(2), 4, ll_iseq);  // Right operand
    
    // Define temporary registers
    Operand temp_flag(Operand::MREG8, MREG_R10);      // 8-bit for setg
    Operand extended_flag(Operand::MREG32, MREG_R11); // 32-bit for final result
    Operand extended_flag1(Operand::MREG32, MREG_R10); // 32-bit for comparison
    
    // 1. Move left operand to temporary register
    ll_iseq->append(new Instruction(MINS_MOVL, left, extended_flag1));
    
    // 2. Compare with right operand
    ll_iseq->append(new Instruction(MINS_CMPL, right, extended_flag1));
    
    // 3. Set byte based on greater than comparison
    ll_iseq->append(new Instruction(MINS_SETG, temp_flag));
    
    // 4. Zero-extend the result to 32 bits
    ll_iseq->append(new Instruction(MINS_MOVZBL, temp_flag, extended_flag));
    
    // 5. Move to destination
    ll_iseq->append(new Instruction(MINS_MOVL, extended_flag, dest));
    
    return;
}if (hl_opcode == HINS_sconv_bl) {
    // Get operands - source is 1 byte, destination is 4 bytes
    Operand dest = get_ll_operand(hl_ins->get_operand(0), 4, ll_iseq);   // 4 bytes for long
    Operand src = get_ll_operand(hl_ins->get_operand(1), 1, ll_iseq);    // 1 byte for byte

    // Move and sign extend from byte to long using MOVSBL
    Operand r10_32(Operand::MREG32, MREG_R10);
    ll_iseq->append(new Instruction(MINS_MOVSBL, src, r10_32));

    // Store result in destination
    ll_iseq->append(new Instruction(MINS_MOVL, r10_32, dest));

    return;
}
if (hl_opcode == HINS_call) {
    // Extract the function name/label from the operand
    std::string func_name = hl_ins->get_operand(0).get_label();
    
    // For each argument, ensure we're using the correct size moves
    for (unsigned i = 1; i < hl_ins->get_num_operands(); i++) {
        Operand src = get_ll_operand(hl_ins->get_operand(i), 8, ll_iseq);  // Use 8 bytes for long
        
        // Map to the correct argument register (rdi, rsi, rdx, etc.)
        MachineReg arg_reg;
        switch(i) {
            case 1: arg_reg = MREG_RDI; break;
            case 2: arg_reg = MREG_RSI; break;
            case 3: arg_reg = MREG_RDX; break;
            case 4: arg_reg = MREG_RCX; break;
            case 5: arg_reg = MREG_R8; break;
            case 6: arg_reg = MREG_R9; break;
            default: RuntimeError::raise("Too many arguments");
        }
        
        // Use MOVQ for 64-bit values (like long)
        Operand dest(Operand::MREG64, arg_reg);
        ll_iseq->append(new Instruction(MINS_MOVQ, src, dest));
    }
    
    // Create a label operand for the function
    Operand func_label(Operand::LABEL, func_name);
    
    // Emit the call instruction
    ll_iseq->append(new Instruction(MINS_CALL, func_label));
    
    return;
} if (hl_opcode == HINS_cmpeq_l) {
    // Extract operands
    Operand dest = get_ll_operand(hl_ins->get_operand(0), 4, ll_iseq);   // Result (32-bit)
    Operand left = get_ll_operand(hl_ins->get_operand(1), 4, ll_iseq);   // Left operand
    Operand right = get_ll_operand(hl_ins->get_operand(2), 4, ll_iseq);  // Right operand
    
    // Define temporary registers
    Operand temp_flag(Operand::MREG8, MREG_R10);      // 8-bit for sete
    Operand extended_flag(Operand::MREG32, MREG_R11); // 32-bit for final result
    Operand extended_flag1(Operand::MREG32, MREG_R10); // 32-bit for final result
    // 1. Move left operand to temporary register
    ll_iseq->append(new Instruction(MINS_MOVL, left, extended_flag1));
    
    // 2. Compare with right operand
    ll_iseq->append(new Instruction(MINS_CMPL, right, extended_flag1));
    
    // 3. Set byte based on equality
    ll_iseq->append(new Instruction(MINS_SETE, temp_flag));
  
    // 4. Zero-extend the result to 32 bits
    ll_iseq->append(new Instruction(MINS_MOVZBL, temp_flag, extended_flag));
    
    // 5. Move to destination
    ll_iseq->append(new Instruction(MINS_MOVL, extended_flag, dest));
    
    return;
}if (hl_opcode == HINS_cmplt_l) {
        // Extract operands
        Operand dest = get_ll_operand(hl_ins->get_operand(0), 4, ll_iseq);   // Destination operand (32-bit)
        Operand left = get_ll_operand(hl_ins->get_operand(1), 4, ll_iseq);   // Left operand (32-bit)
        Operand right = get_ll_operand(hl_ins->get_operand(2), 4, ll_iseq);  // Right operand (32-bit)
        
        // Define temporary registers
        Operand temp_flag(Operand::MREG8, MREG_R10);      // 8-bit register for condition
        Operand extended_flag(Operand::MREG32, MREG_R11); // 32-bit register for extended result
        
        // 1. Move left operand to temporary register
        ll_iseq->append(new Instruction(MINS_MOVL, left, Operand(Operand::MREG32, MREG_R10)));
        
        // 2. Compare with right operand
        ll_iseq->append(new Instruction(MINS_CMPL, right, Operand(Operand::MREG32, MREG_R10)));
        
        // 3. Set byte based on less than comparison
        ll_iseq->append(new Instruction(MINS_SETL, temp_flag));
        
        // 4. Zero-extend the result to 32 bits
        ll_iseq->append(new Instruction(MINS_MOVZBL, temp_flag, extended_flag));
        
        // 5. Move the extended flag to the destination operand
        ll_iseq->append(new Instruction(MINS_MOVL, extended_flag, dest));
        
        return;
    }
if (hl_opcode == HINS_add_q) {
    // Get operands
    Operand dest = get_ll_operand(hl_ins->get_operand(0), 8, ll_iseq);   // Result
    Operand src1 = get_ll_operand(hl_ins->get_operand(1), 8, ll_iseq);   // First source
    Operand src2 = get_ll_operand(hl_ins->get_operand(2), 8, ll_iseq);   // Second source

    // Move first source to temporary register r10
    Operand r10(Operand::MREG64, MREG_R10);
    ll_iseq->append(new Instruction(MINS_MOVQ, src1, r10));
    
    // Add second source to r10
    ll_iseq->append(new Instruction(MINS_ADDQ, src2, r10));
    
    // Move result to destination
    ll_iseq->append(new Instruction(MINS_MOVQ, r10, dest));
    
    return;
}


// Handle add instructions
    if (match_hl(HINS_add_b, hl_opcode)) {
        int size = highlevel_opcode_get_source_operand_size(hl_opcode);
        LowLevelOpcode add_opcode = select_ll_opcode(MINS_ADDB, size);
        
        Operand src1 = get_ll_operand(hl_ins->get_operand(1), size, ll_iseq);
        Operand src2 = get_ll_operand(hl_ins->get_operand(2), size, ll_iseq);
        Operand dest = get_ll_operand(hl_ins->get_operand(0), size, ll_iseq);
        
        // Load first operand into r10
        Operand::Kind mreg_kind = select_mreg_kind(size);
        Operand r10(mreg_kind, MREG_R10);
        ll_iseq->append(new Instruction(select_ll_opcode(MINS_MOVB, size), src1, r10));
        
        // Add second operand to r10
        ll_iseq->append(new Instruction(add_opcode, src2, r10));
        
        // Store result
        ll_iseq->append(new Instruction(select_ll_opcode(MINS_MOVB, size), r10, dest));
        return;
    }
      if (hl_opcode == HINS_cjmp_t) {
    // Operand Structure:
    // Operand 0: Condition Virtual Register (stores 0 or 1)
    // Operand 1: Target Label (e.g., .L0)

    // Extract operands
    Operand condition_vreg = get_ll_operand(hl_ins->get_operand(0), 4, ll_iseq); // 32-bit operand

    // **Handle the target label separately**
    // Assuming hl_ins->get_operand(1) contains the label name as a string
    std::string label = hl_ins->get_operand(1).get_label(); // Ensure get_label() retrieves the label name
    Operand target_label(Operand::LABEL, label); // Construct a label operand

    // Create an immediate value operand for 0
    Operand zero_operand(Operand::IMM_IVAL, 0);

    // 1. Compare the condition register with 0: cmpl $0, condition_vreg
    ll_iseq->append(new Instruction(MINS_CMPL, zero_operand, condition_vreg));

    // 2. Jump if not equal to 0 (condition is true): jne target_label
    ll_iseq->append(new Instruction(MINS_JNE, target_label));

    return;
}// Handle multiplication instructions (both 32-bit and 64-bit)
if (hl_opcode == HINS_mul_l || hl_opcode == HINS_mul_q) {
    // Determine operand size based on instruction type
    int size = (hl_opcode == HINS_mul_l) ? 4 : 8;
    
    // Get operands
    Operand dest = get_ll_operand(hl_ins->get_operand(0), size, ll_iseq);   // Result
    Operand src1 = get_ll_operand(hl_ins->get_operand(1), size, ll_iseq);   // First source
    Operand src2 = get_ll_operand(hl_ins->get_operand(2), size, ll_iseq);   // Second source

    // Select appropriate register size and multiplication instruction
    Operand::Kind mreg_kind = select_mreg_kind(size);
    LowLevelOpcode mul_opcode = (size == 4) ? MINS_IMULL : MINS_IMULQ;
    LowLevelOpcode mov_opcode = (size == 4) ? MINS_MOVL : MINS_MOVQ;
    
    // Move first source to temporary register
    Operand r10(mreg_kind, MREG_R10);
    ll_iseq->append(new Instruction(mov_opcode, src1, r10));
    
    // Multiply r10 by second source
    ll_iseq->append(new Instruction(mul_opcode, src2, r10));
    
    // Move result to destination
    ll_iseq->append(new Instruction(mov_opcode, r10, dest));
    
    return;
}
if (hl_opcode == HINS_sconv_lq) {
    // Get operands - source is 32-bit (long), destination is 64-bit (quad)
    Operand dest = get_ll_operand(hl_ins->get_operand(0), 8, ll_iseq);   // 8 bytes for quad
    Operand src = get_ll_operand(hl_ins->get_operand(1), 4, ll_iseq);    // 4 bytes for long

    // Move source to temporary register (32-bit version)
    Operand r10_32(Operand::MREG32, MREG_R10);
    ll_iseq->append(new Instruction(MINS_MOVL, src, r10_32));

    // Sign extend from 32-bit to 64-bit using MOVSLQ
    Operand r10_64(Operand::MREG64, MREG_R10);
    ll_iseq->append(new Instruction(MINS_MOVSLQ, r10_32, r10_64));

    // Store result in destination
    ll_iseq->append(new Instruction(MINS_MOVQ, r10_64, dest));

    return;
}if (match_hl(HINS_neg_b, hl_opcode)) {
        int size = highlevel_opcode_get_source_operand_size(hl_opcode);
        LowLevelOpcode mov_opcode = select_ll_opcode(MINS_MOVB, size);
        LowLevelOpcode sub_opcode = select_ll_opcode(MINS_SUBB, size);
        
        // Get operands
        Operand src = get_ll_operand(hl_ins->get_operand(1), size, ll_iseq);
        Operand dest = get_ll_operand(hl_ins->get_operand(0), size, ll_iseq);
        
        // Move source to temporary register
        Operand::Kind mreg_kind = select_mreg_kind(size);
        Operand r10(mreg_kind, MREG_R10);
        
        // Move value to temporary register
        ll_iseq->append(new Instruction(mov_opcode, src, r10));
        
        // Move 0 to destination
        ll_iseq->append(new Instruction(mov_opcode, Operand(Operand::IMM_IVAL, 0), dest));
        
        // Subtract source from 0 in destination
        ll_iseq->append(new Instruction(sub_opcode, r10, dest));
        
        return;
    }

    if (hl_opcode == HINS_cmplte_l) {
        

        // Extract operands
        Operand dest = get_ll_operand(hl_ins->get_operand(0), 4, ll_iseq);   // Destination operand (32-bit)
        Operand left = get_ll_operand(hl_ins->get_operand(1), 4, ll_iseq);   // Left operand (32-bit)
        Operand right = get_ll_operand(hl_ins->get_operand(2), 4, ll_iseq);  // Right operand (32-bit)
        
        // Define temporary registers using MREG_R10
        // Using Operand::MREG8 to represent %r10b and Operand::MREG32 for %r10d
        Operand temp_flag(Operand::MREG8, MREG_R10);       // 8-bit register (e.g., %r10b)
        Operand extended(Operand::MREG32, MREG_R10);  
        ll_iseq->append(new Instruction(MINS_MOVL, left, extended));

        ll_iseq->append(new Instruction(MINS_CMPL, right, extended));
        Operand extended_flag(Operand::MREG32, MREG_R11);
        // Set the condition flag based on the comparison: setle %r10b
        ll_iseq->append(new Instruction(MINS_SETLE, temp_flag));
        
        // Zero-extend the condition flag to 32 bits: movzbl %r10b, %r10d
        ll_iseq->append(new Instruction(MINS_MOVZBL, temp_flag, extended_flag));
        
        // Move the extended flag to the destination operand: movl %r10d, dest
        ll_iseq->append(new Instruction(MINS_MOVL, extended_flag, dest));
        
        return;
    }if (hl_opcode == HINS_cjmp_f) {
      // Extract operands
      Operand condition_vreg = get_ll_operand(hl_ins->get_operand(0), 4, ll_iseq); // 32-bit operand
      
      // Handle the target label
      std::string label = hl_ins->get_operand(1).get_label();
      Operand target_label(Operand::LABEL, label);
      
      // Create immediate value operand for 0
      Operand zero_operand(Operand::IMM_IVAL, 0);
      
      // Compare the condition register with 0
      ll_iseq->append(new Instruction(MINS_CMPL, zero_operand, condition_vreg));
      
      // Jump if equal to 0 (condition is false)
      ll_iseq->append(new Instruction(MINS_JE, target_label));
    
      return;
}
    if (hl_opcode == HINS_jmp) {
      ll_iseq->append(new Instruction(MINS_JMP, hl_ins->get_operand(0)));
      return;
    }

    // Handle add instructions
    if (match_hl(HINS_add_b, hl_opcode)) {
        int size = highlevel_opcode_get_source_operand_size(hl_opcode);
        LowLevelOpcode add_opcode = select_ll_opcode(MINS_ADDB, size);
        
        Operand src1 = get_ll_operand(hl_ins->get_operand(1), size, ll_iseq);
        Operand src2 = get_ll_operand(hl_ins->get_operand(2), size, ll_iseq);
        Operand dest = get_ll_operand(hl_ins->get_operand(0), size, ll_iseq);
        
        // Load first operand into r10
        Operand::Kind mreg_kind = select_mreg_kind(size);
        Operand r10(mreg_kind, MREG_R10);
        ll_iseq->append(new Instruction(select_ll_opcode(MINS_MOVB, size), src1, r10));
        
        // Add second operand to r10
        ll_iseq->append(new Instruction(add_opcode, src2, r10));
        
        // Store result
        ll_iseq->append(new Instruction(select_ll_opcode(MINS_MOVB, size), r10, dest));
        return;
    }if (hl_opcode == HINS_localaddr) {
      // Get the destination operand
      Operand dest = get_ll_operand(hl_ins->get_operand(0), 8, ll_iseq);
      
      // Get the offset and adjust it (-8 is the base offset for local vars)
      int offset = -8 + hl_ins->get_operand(1).get_imm_ival();
      
      // Create temporary register for the address
      Operand r10(Operand::MREG64, MREG_R10);
      
      // Create memory reference with adjusted offset
      Operand mem_ref(Operand::MREG64_MEM_OFF, MREG_RBP, offset);
      
      // First instruction: load effective address into r10
      ll_iseq->append(new Instruction(MINS_LEAQ, mem_ref, r10));
      
      // Second instruction: store r10 into destination
      ll_iseq->append(new Instruction(MINS_MOVQ, r10, dest));
      
      return;
}
  if (hl_opcode == HINS_enter) {
    
    ll_iseq->append(new Instruction(MINS_PUSHQ, Operand(Operand::MREG64, MREG_RBP)));
    ll_iseq->append(new Instruction(MINS_MOVQ, Operand(Operand::MREG64, MREG_RSP), Operand(Operand::MREG64, MREG_RBP)));
    if (m_total_memory_storage > 0)
      ll_iseq->append(new Instruction(MINS_SUBQ, Operand(Operand::IMM_IVAL, m_total_memory_storage), Operand(Operand::MREG64, MREG_RSP)));
    return;
  }

  if (hl_opcode == HINS_leave) {
    

    if (m_total_memory_storage > 0)
      ll_iseq->append(new Instruction(MINS_ADDQ, Operand(Operand::IMM_IVAL, m_total_memory_storage), Operand(Operand::MREG64, MREG_RSP)));
      ll_iseq->append(new Instruction(MINS_POPQ, Operand(Operand::MREG64, MREG_RBP)));

    return;
  }

  if (hl_opcode == HINS_ret) {
    ll_iseq->append(new Instruction(MINS_RET));
    return;
  }

 

  RuntimeError::raise("high level opcode %d not handled", int(hl_opcode));
}


Operand LowLevelCodeGen::get_ll_operand(Operand hl_op, int size, std::shared_ptr<InstructionSequence> ll_iseq) {
    if (hl_op.is_imm_ival()) {
        return hl_op;  // Immediate values pass through
    }
    
    if (hl_op.is_label() || hl_op.is_imm_label()) {
        return hl_op; 
    }

    // Handle memory references (array accesses or variables)
    if (hl_op.is_memref()) {
        int vreg_num = hl_op.get_base_reg();
        //f("vreg_num for memory reference: %d\n", vreg_num);
        
        // For array element access (indirect memory reference)
        if (hl_op.get_kind() == Operand::VREG_MEM) {
            //printf("vreg_mem\n");
            // Load the computed address into r11
            Operand addr = Operand(Operand::MREG64_MEM_OFF, MREG_RBP, 
                m_base_vreg_offset + (8 * (vreg_num - VREG_FIRST_LOCAL)));
            //f("addr: %d\n", addr.get_imm_ival());
            ll_iseq->append(new Instruction(MINS_MOVQ, addr, 
                Operand(Operand::MREG64, MREG_R11)));
            // Return memory reference using that address
            return Operand(Operand::MREG64_MEM, MREG_R11);
        }
        
        // For direct memory access
        long offset = hl_op.has_imm_ival() ? hl_op.get_imm_ival() : 0;
        //f("offset for direct memory reference: %ld\n", offset);
        
        // Handle array base and virtual registers
        // For direct memory access
if (vreg_num >= VREG_FIRST_LOCAL) {
            int base_offset;
            //printf("vreg_num for direct memory reference: %d\n", vreg_num);
            // Check if this is from a localaddr instruction (array base)
            if (hl_op.get_kind() == Operand::VREG && offset == 0) {
        // Arrays start at -16(%rbp) in main
        base_offset = -(8);  // Local storage offset
        //printf(" localaddr instruction\n");
    } else {
        // Calculate offset from bottom of stack frame
        // Account for both local storage and vreg storage
        int local_storage = m_function->get_local_storage_size();
        if (local_storage > 0) {
            // Align local_storage to 16 bytes if needed
            if (local_storage % 16 != 0) {
                local_storage += (16 - (local_storage % 16));
            }
            base_offset = -(local_storage + m_total_memory_storage - (8 * (vreg_num - 1)));
            //f("local_storage: %d\n", local_storage);
            //printf("base_offset: %d\n", base_offset);
        } else {
            base_offset = -(m_total_memory_storage - (8 * (vreg_num - 1)));
            //printf("base_offset less than 0: %d\n", base_offset);
        }
    }
            return Operand(Operand::MREG64_MEM_OFF, MREG_RBP, base_offset + offset);
        }
        
        // Handle machine registers (vr0-vr6)
        if (vreg_num <= 6) {
            MachineReg base_reg;
            if (vreg_num == 0) {
                base_reg = MREG_RAX;
            } else if (vreg_num >= 1 && vreg_num <= 6) {
                base_reg = (vreg_num == 2) ? MREG_RSI : 
                          static_cast<MachineReg>(MREG_RDI + (vreg_num - 1));
            } else {
                RuntimeError::raise("Invalid virtual register for memory reference: vr%d", vreg_num);
            }
            if (offset != 0) {
                return Operand(Operand::MREG64_MEM_OFF, base_reg, offset);
            }
            return Operand(Operand::MREG64_MEM, base_reg);
        }
    }

    // Handle regular virtual registers
    int vreg_num = hl_op.get_base_reg();
    
    if (vreg_num == 0) {
        return Operand(select_mreg_kind(size), MREG_RAX);
    }
    
    if (vreg_num >= 1 && vreg_num <= 6) {
        MachineReg mreg = (vreg_num == 2) ? MREG_RSI : 
                         static_cast<MachineReg>(MREG_RDI + (vreg_num - 1));
        return Operand(select_mreg_kind(size), mreg);
    }
    
    if (vreg_num >= VREG_FIRST_LOCAL) {
        int offset = m_base_vreg_offset + (8 * (vreg_num - VREG_FIRST_LOCAL));
        assert(vreg_num <= m_max_vreg && "vreg_num out of bounds");
        return Operand(Operand::MREG64_MEM_OFF, MREG_RBP, offset);
    }
    
    RuntimeError::raise("Invalid virtual register number: vr%d", vreg_num);
}