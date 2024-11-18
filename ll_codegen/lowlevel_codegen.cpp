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

    // Step 1: Determine the maximum virtual register used
    int max_vreg = 9;  // Start at 9 since vr0-vr9 are reserved
    for (auto it = hl_iseq->cbegin(); it != hl_iseq->cend(); ++it) {
        Instruction *ins = *it;
        for (unsigned j = 0; j < ins->get_num_operands(); j++) {
            Operand op = ins->get_operand(j);
            if (op.get_kind() == Operand::VREG) {
                max_vreg = std::max(max_vreg, op.get_base_reg());
            }
        }
    }

    m_max_vreg = max_vreg;

    // Step 2: Calculate storage needed for vregs (vr10 and above)
    int num_vregs_needing_storage = std::max(m_max_vreg - 9, 0);
    int vreg_storage = num_vregs_needing_storage * 8;  // 8 bytes per vreg for alignment

    // Step 3: Get local storage size from the Function object
    int local_storage_size = m_function->get_local_storage_size();

    // Step 4: Calculate total storage needed
    m_total_memory_storage = local_storage_size + vreg_storage;
  

    // Step 5: Align to 16 bytes
    if (m_total_memory_storage % 16 != 0) {
        m_total_memory_storage += (16 - (m_total_memory_storage % 16));
    }

    // Step 6: Determine base offset for vregs
    m_base_vreg_offset = -8 * num_vregs_needing_storage;
    

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
    
    if (src.is_memref() && dest.is_memref() ) {
        // Memory-to-memory moves need an intermediate register
        Operand::Kind mreg_kind = select_mreg_kind(size);
        Operand r10(mreg_kind, MREG_R10);
        
        // mov src -> r10
        ll_iseq->append(new Instruction(mov_opcode, src, r10));
        
        // mov r10 -> dest
        ll_iseq->append(new Instruction(mov_opcode, r10, dest));
    } else {
        // Direct move
        ll_iseq->append(new Instruction(mov_opcode, src, dest));
    }

    
    return;
}     if (hl_opcode == HINS_call) {
    // Extract the function name/label from the operand
    std::string func_name = hl_ins->get_operand(0).get_label();
    
    // Create a label operand for the function
    Operand func_label(Operand::LABEL, func_name);
    
    // Emit the call instruction
    ll_iseq->append(new Instruction(MINS_CALL, func_label));
    
    return;
}   if (hl_opcode == HINS_cmpeq_l) {
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
}
        if (hl_opcode == HINS_cmplte_l) {
        // Operand Structure:
        // Operand 0: Destination Virtual Register (stores 0 or 1)
        // Operand 1: Left Virtual Register
        // Operand 2: Right Virtual Register

        // Extract operands
        Operand dest = get_ll_operand(hl_ins->get_operand(0), 4, ll_iseq);   // Destination operand (32-bit)
        Operand left = get_ll_operand(hl_ins->get_operand(1), 4, ll_iseq);   // Left operand (32-bit)
        Operand right = get_ll_operand(hl_ins->get_operand(2), 4, ll_iseq);  // Right operand (32-bit)
        
        // Define temporary registers using MREG_R10
        // Using Operand::MREG8 to represent %r10b and Operand::MREG32 for %r10d
        Operand temp_flag(Operand::MREG8, MREG_R10);       // 8-bit register (e.g., %r10b)
        Operand extended(Operand::MREG32, MREG_R10);  // 32-bit register (e.g., %r10d)
        
        // 1. Compare right with left: cmpl right, left
        // 1. Move left operand into temporary register
ll_iseq->append(new Instruction(MINS_MOVL, left, extended));

// 2. Compare with right operand: cmpl right, left
ll_iseq->append(new Instruction(MINS_CMPL, right, extended));
        Operand extended_flag(Operand::MREG32, MREG_R11);
        // 2. Set the condition flag based on the comparison: setle %r10b
        ll_iseq->append(new Instruction(MINS_SETLE, temp_flag));
        
        // 3. Zero-extend the condition flag to 32 bits: movzbl %r10b, %r10d
        ll_iseq->append(new Instruction(MINS_MOVZBL, temp_flag, extended_flag));
        
        // 4. Move the extended flag to the destination operand: movl %r10d, dest
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
    }
  if (hl_opcode == HINS_enter) {
    // Function prologue: this will create an ABI-compliant stack frame.
    // The local variable area is *below* the address in %rbp, and local storage
    // can be accessed at negative offsets from %rbp. For example, the topmost
    // 4 bytes in the local storage area are at -4(%rbp).
    ll_iseq->append(new Instruction(MINS_PUSHQ, Operand(Operand::MREG64, MREG_RBP)));
    ll_iseq->append(new Instruction(MINS_MOVQ, Operand(Operand::MREG64, MREG_RSP), Operand(Operand::MREG64, MREG_RBP)));
    if (m_total_memory_storage > 0)
      ll_iseq->append(new Instruction(MINS_SUBQ, Operand(Operand::IMM_IVAL, m_total_memory_storage), Operand(Operand::MREG64, MREG_RSP)));

    // save callee-saved registers (if any)
    // TODO: if you allocated callee-saved registers as storage for local variables,
    //       emit pushq instructions to save their original values

    return;
  }

  if (hl_opcode == HINS_leave) {
    // Function epilogue: deallocate local storage area and restore original value
    // of %rbp

    // TODO: if you allocated callee-saved registers as storage for local variables,
    //       emit popq instructions to save their original values

    if (m_total_memory_storage > 0)
      ll_iseq->append(new Instruction(MINS_ADDQ, Operand(Operand::IMM_IVAL, m_total_memory_storage), Operand(Operand::MREG64, MREG_RSP)));
    ll_iseq->append(new Instruction(MINS_POPQ, Operand(Operand::MREG64, MREG_RBP)));

    return;
  }

  if (hl_opcode == HINS_ret) {
    ll_iseq->append(new Instruction(MINS_RET));
    return;
  }

  // TODO: handle other high-level instructions
  // Note that you can use the highlevel_opcode_get_source_operand_size() and
  // highlevel_opcode_get_dest_operand_size() functions to determine the
  // size (in bytes, 1, 2, 4, or 8) of either the source operands or
  // destination operand of a high-level instruction. This should be useful
  // for choosing the appropriate low-level instructions and
  // machine register operands.

  RuntimeError::raise("high level opcode %d not handled", int(hl_opcode));
}

// TODO: implement other private member functions
Operand LowLevelCodeGen::get_ll_operand(Operand hl_op, int size, std::shared_ptr<InstructionSequence> ll_iseq) {
    if (hl_op.is_imm_ival()) {
        return hl_op;  // Immediate values pass through
    }
    
    // **Handle label operands**
    if (hl_op.is_label() || hl_op.is_imm_label()) {
        return hl_op; // Pass through as label operands
    }

    int vreg_num = hl_op.get_base_reg();
  
    
    // Handle vr0 (return register)
    if (vreg_num == 0) {
        // Map vr0 to %rax or its sub-register based on size
    
        return Operand(static_cast<Operand::Kind>(select_mreg_kind(size)), MREG_RAX);
    }
    
    //vr1 - vr6 (argument registers)
    if (vreg_num >= 1 && vreg_num <= 6) {
    // Map to correct registers: rdi, rsi, rdx, rcx, r8, r9
    int mreg = (vreg_num == 2) ? MREG_RSI : 
               MREG_RDI + (vreg_num - 1);
    return Operand(static_cast<Operand::Kind>(select_mreg_kind(size)), mreg);
}
    
    // Handle vr10 and above (memory)
    if (vreg_num >=10) {
        int offset = m_base_vreg_offset + 8 * (vreg_num - 10);  // 8 bytes per vreg for alignment
 
        assert(vreg_num <= m_max_vreg && "vreg_num out of bounds");
        
        // Correct the order of parameters: base_reg first, then offset
        Operand mem_operand(Operand::MREG64_MEM_OFF, MREG_RBP, offset);
     
        return mem_operand;
    }
    
    // If vr_num does not fit expected categories, raise an error
    RuntimeError::raise("Invalid virtual register number: vr%d", vreg_num);
}
