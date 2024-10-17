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

#ifndef OPERAND_H
#define OPERAND_H

#include <string>

//! Operand of an Instruction.
//! Can be used for both high-level linear IR code and low-level
//! (machine) linear IR code.  These have value semantics, and
//! can (and should) be passed and returned by value.
//!
//! **Notes about registers**: for high-level instructions, thee
//! register numbers are "virtual" registers, of which there is
//! no fixed amount. However, certain virtual registers have
//! a special purpose:
//! - virtual register 0 (`vr0`) is the return value register
//! - virtual registers 1 through 9 are used to pass arguments
//!   to functions (although in practice only `vr1` through
//!   `vr6` will be used)
//!
//! Also note that virtual registers 10 and above are private
//! to the function. I.e., they represent storage locations that
//! aren't accessible to other functions. Thus, they may be used
//! freely as storage for temporary values and/or local variables.
//!
//! For low-level code, register numbers are MachineReg values.
class Operand {
public:
  //! Kinds of operands.
  enum Kind {
    NONE,            // used only for invalid Operand values

                     // Description                       Example
                     // --------------------------------  ---------------------

    // High-level operands
    VREG,            // just a vreg                       vr0
    VREG_MEM,        // memref using vreg ptr             (vr0)
    VREG_MEM_OFF,    // memref using vreg ptr+imm offset  8(vr0)

    // Low-level operands
    MREG8,           // just an mreg                      %al
    MREG16,          // just an mreg                      %ax
    MREG32,          // just an mreg                      %eax
    MREG64,          // just an mreg                      %rax
    MREG64_MEM,      // memref using mreg ptr             (%rax)
    MREG64_MEM_IDX,  // memref using mreg ptr+index       (%rax,%rsi)
    MREG64_MEM_OFF,  // memref using mreg ptr+imm offset  8(%rax)
    MREG64_MEM_IDX_SCALE, // memref mreg ptr+(index*scale) (%r13,%r9,4)

    // Immediate integer operands (used for both high-level and
    // low-level code)
    IMM_IVAL,        // immediate signed int              $1

    // Label an immediate lable operands (used for both high-level and
    // low-level code)
    LABEL,           // label                             .L0
    IMM_LABEL,       // immediate label                   $printf
  };

private:
  Kind m_kind;
  int m_basereg, m_index_reg;
  long m_imm_ival; // also used for offset and scale
  std::string m_label;

public:
  //! Constructor.
  //! @param kind the operand Kind to set (defaults to `NONE`)
  Operand(Kind kind = NONE);

  //! Constructor.
  //! @param ival1 either basereg or imm_ival (depending on operand Kind)
  Operand(Kind kind, long ival1);

  //! Constructor.
  //! @param ival2 either index_reg or imm_ival (depending on operand kind)
  Operand(Kind kind, int basereg, long ival2);

  //! Constructor (only used for MREG64_MEM_IDX_SCALE operands).
  //! @param kind the operand Kind (must be `MREG64_MEM_IDX_SCALE`)
  //! @param basereg the base register (i.e., the base address)
  //! @param indexreg the index register (i.e., the array element)
  //! @param scale the scaling factor (must be 1, 2, 4, or 8)
  Operand(Kind kind, int basereg, int indexreg, int scale);

  //! Constructor for label or immediate label operands.
  //! @param kind the operand Kind
  //! @param label the label
  Operand(Kind kind, const std::string &label);

  ~Operand();

  // use compiler-generated copy ctor and assignment op

  //! Compare two Operands for equality.
  //! @param rhs another Operand
  //! @return true if this Operand is identical to rhs, false otherwise
  bool operator==(const Operand &rhs) const;

  //! Get the operand Kind.
  //! @return the operand Kind
  Kind get_kind() const;

  //! Is the operand an immediate integer value?
  //! @return true if the operand is an immediate integer value, false otherwise
  bool is_imm_ival() const;

  //! Is the operand a non-immediate label?
  //! @return true if the operand is a label, false otherwise
  bool is_label() const;

  //! Is the operand an immediate label?
  //! @return true if the operand is an immediate label, false otherwise
  bool is_imm_label() const;

  //! Does the operand have a base register?
  //! @return true if the operand has a base register, false otherwise
  bool has_base_reg() const { return m_basereg >= 0; }

  //! Does the operand have an index register?
  //! @return true if the operand has an index register, false otherwise
  bool has_index_reg() const;

  //! Does the operand have an immediate integer offset?
  //! @return true if the operand has an immediate integer offset, false otherwise
  bool has_offset() const;

  //! Does the operand have a scaling factor?
  //! @return true if the operand has a scaling factor, false otherwise
  bool has_scale() const;

  //! Is the operand a non-register operand? (i.e., no base or index reg).
  //! @return true if the operand is a non-register operand, false otherwise
  bool is_non_reg() const;

  //! Is the operand a memory reference?
  //! @return true if the operand is a memory reference, false otherwise
  bool is_memref() const;

  //! Does the operand have an immediate integer value?
  //! (Either because it *is* an immediate integer value, or because
  //! it has an immediate integer offset.)
  //! @return true if the operand has an immediate integer value, false otherwise
  bool has_imm_ival() const;

  //! Does the operand have a label?
  //! (Either because it is a label, or is an immediate label.)
  //! @return true if the operand has a label, false otherwise
  bool has_label() const;

  // getters

  //! Get the base register.
  //! @return the base register
  int get_base_reg() const;

  //! Get the index register.
  //! @return the index register
  int get_index_reg() const;

  //! Get the immediate integer value.
  //! @return the immediate integer value
  long get_imm_ival() const;

  //! Get the integer offset.
  //! @return the integer offset
  long get_offset() const;

  //! Get the scaling factor.
  //! @return the scaling factor
  long get_scale() const;

  // setters

  //! Set the base register.
  //! @param regnum the base register to set
  void set_base_reg(int regnum);

  //! Set the index register.
  //! @param regnum the index register to set
  void set_index_reg(int regnum);

  //! Set the immediate integer value.
  //! @param ival the immediate integer value to set
  void set_imm_ival(long ival);

  //! Set the offset.
  //! @param offset the offset to set
  void set_offset(long offset);

  //! Return a memory reference Operand, using the register
  //! this Operand represents as the pointer providing the memory address.
  //! For example, if this operand represents the virtual register
  //! `vr10` (operand Kind value `VREG`), calling `to_memref()`
  //! would return the memory reference operand `(vr10)`
  //! (operand Kind value `VREG_MEM`.)
  //!
  //! @return a memory reference Operand
  Operand to_memref() const;

  //! Get the Operand's label.
  //! @return the label
  std::string get_label() const;
};

#endif // OPERAND_H
