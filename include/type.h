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

#ifndef TYPE_H
#define TYPE_H

#include <memory>
#include <vector>
#include <set>
#include <string>

//! @file
//! Representations of C data and function types.

//! Kinds of basic types.
//! Note that these can be signed or unsigned
//! (except for void).
enum class BasicTypeKind {
  CHAR,
  SHORT,
  INT,
  LONG,
  VOID,
};

//! Type qualifiers.
enum class TypeQualifier {
  VOLATILE,
  CONST,
};

// forward declaration
class Member;

//! Representation of a C data type.
//! Type is a base class that may not be directly
//! instantiated. It defines a "wide" interface of virtual member functions
//! for the complete range of functionality offered by subclasses,
//! and provides default implementations for most of them. Subclasses will
//! override these as necessary/appropriate.
//!
//! **Important**: Type objects should be accessed using `std::shared_ptr`.
//! Types are essentially trees, and if a variable declaration
//! has multiple declarators, the resulting types of the
//! declared variables can share the common part of their
//! representations.
class Type {
private:
  // value semantics not allowed
  Type(const Type &);
  Type& operator=(const Type &);

protected:
  Type();

public:
  virtual ~Type();

  // Some member functions for convenience

  //! Return true if the type is an integral (integer) type.
  bool is_integral() const { return is_basic() && get_basic_type_kind() != BasicTypeKind::VOID; }

  //! Find named member.
  //! @param name name of the member to return
  //! @return pointer to a named `Member` (or a null pointer
  //!         if the named member doesn't exist)
  const Member *find_member(const std::string &name) const;

  // Note that Type provides default implementations of virtual
  // member functions that will be appropriate for most of the
  // subclasses. Each subclass should be able to just override the
  // member functions that are necessary to provide that
  // subclass's functionality.

  //! Equality comparison.
  //! @return true IFF the other type represents exactly the same type as this one
  virtual bool is_same(const Type *other) const = 0;

  //! Convert to a readable string representation of this type.
  //! @return a string containing a description of the type
  std::string as_str() const;

  //! Get unqualified type (strip off type qualifiers, if any).
  //! @return The unqualified type
  virtual const Type *get_unqualified_type() const;

  // subtype tests (safe to call on any Type object)

  //! Check whether the type is a BasicType.
  //! @return true if this type represents a basic type
  //!          (`int`, `char`, etc., including qualified variants
  //!          like `const char`), false otherwise
  virtual bool is_basic() const;

  //! Check whether the type represents `void`.
  //! @return true if this type represents `void`, false otherwise
  virtual bool is_void() const;

  //! Check whether the type is a StructType.
  //! @return true if this is a StructType, false otherwise
  virtual bool is_struct() const;

  //! Check whether the type is a PointerType.
  //! @return true if this is a PointerType, false otherwise
  virtual bool is_pointer() const;

  //! Check whether the type is an ArrayType.
  //! @return true if this an ArrayType, false otherwise
  virtual bool is_array() const;

  //! Check whether the type is a FunctionType.
  //! @return true if this is a FunctionType, false otherwise
  virtual bool is_function() const;

  // qualifier tests (safe to call on any Type object)

  //! Check whether the type is qualified as `volatile`.
  //! @return true if this is a volatile-qualified type, false otherwise
  virtual bool is_volatile() const;

  //! Check whether the type is qualified as `const`.
  //! @return true if this is a const-qualified type, false otherwise
  virtual bool is_const() const;

  // BasicType-only member functions

  //! Get the BasicTypeKind of a BasicType.
  //! @return the BasicTypeKind of this basic type (throws an exception
  //!         if this type is not a basic type)
  virtual BasicTypeKind get_basic_type_kind() const;

  //! Check whether the BasicType is a signed type.
  //! @return true if this basic type is signed, false if it's unsigned
  //!         (throws an exception if this is not a basic type)
  virtual bool is_signed() const;

  // Functions common to StructType and FunctionType.
  // A "member" is a parameter (for FunctionTypes) or a field (for StructTypes).

  //! Add a member. This should only be called if the type type is a StructType
  //! or FunctionType.
  //! @param member the member (field or parameter) to add
  virtual void add_member(const Member &member);

  //! Get the number of members (fields or parameters).
  //! Throws an exception if this type isn't a StructType or FunctionType.
  //! @return the number of members
  virtual unsigned get_num_members() const;

  //! Get the Member with the position indicated by the given
  //! index. The index should be between 0 (inclusive) and the
  //! number of members (exclusive.)
  //! @param index
  //! @return reference to the member at the given index
  virtual const Member &get_member(unsigned index) const;

  //! Get the offset of the named field.
  //! Throws an exception if the type is not a StructType.
  //! @param name the name of a field
  //! @return the offset of the field in bytes
  virtual unsigned get_field_offset(const std::string &name) const;

  //! Determine whether this Type has a base type.
  //! If this function returns true, it is safe to call get_base_type().
  //!
  //! @return true if this Type has a base type, false otherwise
  virtual bool has_base_type() const;

  //! Get the base type.
  //! For a FunctionType, the base type is the return type.
  //! For a PointerType, the base type is the pointed-to type.
  //! For an ArrayType, the base type is the element type.
  //! @return the base type
  virtual std::shared_ptr<Type> get_base_type() const;

  // FunctionType-only member functions
  // (there aren't any, at least for now...)

  // PointerType-only member functions
  // (there actually aren't any, at least for now...)

  //! Get the array size (number of elements).
  //! Throws an exception if the type isn't an ArrayType.
  //! @return the array size (number of elements)
  virtual unsigned get_array_size() const;

  // These member functions can be used on any kind of type other
  // than FunctionType.

  //! Get the number of bytes required to store an instance of this type.
  //! Throws an exception if called on a FunctionType.
  //! @return the number of bytes required to store an instance of this type
  virtual unsigned get_storage_size() const = 0;

  //! Get the storage alignment multiple in bytes for an instance of this type.
  //! I.e., the start address of an instance of this type in memory must
  //! be aligned on a multiple of the returned value.
  //! Throws an exception if called on a FunctionType.
  //! @return the storage alignment multiple in bytes for an instance of this type
  virtual unsigned get_alignment() const = 0;

  //! Implementation function to support `as_str()`.
  //! Deals with the possibility that the representation of the type
  //! could be recursive and thus contain a cycle.
  //! Only the `as_str()` function should ever call this.
  //!
  //! @param seen set of Type objects already seen (to detect cycles)
  //! @return string representation of the type
  virtual std::string as_str(std::set<const Type *> &seen) const = 0;

  //! Implementation function to check whether this type is recursive
  //! (i.e., self referential.)
  //!
  //! @param seen set of Type objects already seen (to detect cycles)
  //! @return true if this Type is recursive, false otherwise
  virtual bool is_recursive(std::set<const Type *> &seen) const;
};

//! Common base class for QualifiedType, FunctionType, PointerType, and
//! ArrayType.
class HasBaseType : virtual public Type {
private:
  std::shared_ptr<Type> m_base_type;

  // value semantics are not allowed
  HasBaseType(const HasBaseType &);
  HasBaseType &operator=(const HasBaseType &);

public:
  HasBaseType(std::shared_ptr<Type> base_type);
  virtual ~HasBaseType();

  virtual bool has_base_type() const;
  virtual std::shared_ptr<Type> get_base_type() const;
};

//! A parameter of a FunctionType or a field of a StructType.
class Member {
private:
  const std::string m_name;
  std::shared_ptr<Type> m_type;
  // Note: you could add additional information here, such as an
  // offset value (for struct fields), etc.

public:
  Member(const std::string &name, std::shared_ptr<Type> type);
  ~Member();

  //! @return the member name
  const std::string &get_name() const;

  //! @return the member Type
  std::shared_ptr<Type> get_type() const;
};

//! Common base class for StructType and FunctionType,
//! which both have "members" (fields or parameters).
class HasMembers : virtual public Type {
private:
  std::vector<Member> m_members;

  // value semantics are disallowed
  HasMembers(const HasMembers &);
  HasMembers &operator=(const HasMembers &);

public:
  HasMembers();
  virtual ~HasMembers();

  virtual std::string as_str(std::set<const Type *> &seen) const;
  virtual bool is_recursive(std::set<const Type *> &seen) const;
  virtual void add_member(const Member &member);
  virtual unsigned get_num_members() const;
  virtual const Member &get_member(unsigned index) const;
};

//! A QualifiedType modifies a "delegate" type with a TypeQualifier
//! (const or volatile). Most of the member functions a just passed
//! on to the delegate.
class QualifiedType : public HasBaseType {
private:
  TypeQualifier m_type_qualifier;

  // value semantics are not allowed
  QualifiedType(const QualifiedType &);
  QualifiedType &operator=(const QualifiedType &);

public:
  QualifiedType(std::shared_ptr<Type> delegate, TypeQualifier type_qualifier);
  virtual ~QualifiedType();

  virtual bool is_same(const Type *other) const;
  virtual std::string as_str(std::set<const Type *> &seen) const;
  virtual const Type *get_unqualified_type() const;
  virtual bool is_basic() const;
  virtual bool is_void() const;
  virtual bool is_struct() const;
  virtual bool is_pointer() const;
  virtual bool is_array() const;
  virtual bool is_function() const;
  virtual bool is_volatile() const;
  virtual bool is_const() const;
  virtual BasicTypeKind get_basic_type_kind() const;
  virtual bool is_signed() const;
  virtual void add_member(const Member &member);
  virtual unsigned get_num_members() const;
  virtual const Member &get_member(unsigned index) const;
  virtual unsigned get_array_size() const;
  virtual unsigned get_storage_size() const;
  virtual unsigned get_alignment() const;
};

//! A BasicType represents a basic type (`int`, `char`, etc.)
class BasicType : public Type {
private:
  BasicTypeKind m_kind;
  bool m_is_signed;

  // value semantics not allowed
  BasicType(const BasicType &);
  BasicType &operator=(const BasicType &);

public:
  BasicType(BasicTypeKind kind, bool is_signed);
  virtual ~BasicType();

  virtual bool is_same(const Type *other) const;
  virtual std::string as_str(std::set<const Type *> &seen) const;
  virtual bool is_basic() const;
  virtual bool is_void() const;
  virtual BasicTypeKind get_basic_type_kind() const;
  virtual bool is_signed() const;
  virtual unsigned get_storage_size() const;
  virtual unsigned get_alignment() const;
};

//! StructType represents a struct type.
//! Each field is represented by a Member.
class StructType : public HasMembers {
private:
  std::string m_name;
  mutable unsigned m_storage_size, m_alignment;

  // value semantics not allowed
  StructType(const StructType &);
  StructType &operator=(const StructType &);

public:
  StructType(const std::string &name);
  virtual ~StructType();

  std::string get_name() const { return m_name; }

  virtual bool is_same(const Type *other) const;
  virtual std::string as_str(std::set<const Type *> &seen) const;
  virtual bool is_struct() const;
  virtual unsigned get_storage_size() const;
  virtual unsigned get_alignment() const;

  virtual unsigned get_field_offset(const std::string &name) const;

private:
  void calculate_storage() const;
};

//! A FunctionType represents the type of a function.
//! Each parameter is represented by a Member.
//! The return type is represented as the base type.
class FunctionType : public HasBaseType, public HasMembers {
private:
  // value semantics not allowed
  FunctionType(const FunctionType &);
  FunctionType &operator=(const FunctionType &);

public:
  FunctionType(std::shared_ptr<Type> base_type);
  virtual ~FunctionType();

  virtual bool is_same(const Type *other) const;
  virtual std::string as_str(std::set<const Type *> &seen) const;
  virtual bool is_function() const;
  virtual unsigned get_storage_size() const;
  virtual unsigned get_alignment() const;
};

//! PointerType represents a pointer type.
//! The base type is the "pointed-to" type.
class PointerType : public HasBaseType {
private:
  // value semantics not allowed
  PointerType(const PointerType &);
  PointerType &operator=(const PointerType &);

public:
  PointerType(std::shared_ptr<Type> base_type);
  virtual ~PointerType();

  virtual bool is_same(const Type *other) const;
  virtual std::string as_str(std::set<const Type *> &seen) const;
  virtual bool is_pointer() const;
  virtual unsigned get_storage_size() const;
  virtual unsigned get_alignment() const;
};

//! ArrayType represents an array type.
//! The base type is the element type.
class ArrayType : public HasBaseType {
private:
  unsigned m_size;

  // value semantics not allowed
  ArrayType(const ArrayType &);
  ArrayType &operator=(const ArrayType &);

public:
  ArrayType(std::shared_ptr<Type> base_type, unsigned size);
  virtual ~ArrayType();

  virtual bool is_same(const Type *other) const;
  virtual std::string as_str(std::set<const Type *> &seen) const;
  virtual bool is_array() const;
  virtual unsigned get_array_size() const;
  virtual unsigned get_storage_size() const;
  virtual unsigned get_alignment() const;
};

#endif // TYPE_H
