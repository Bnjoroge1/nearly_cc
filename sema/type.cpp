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
#include "exceptions.h"
#include "storage.h"
#include "type.h"

////////////////////////////////////////////////////////////////////////
// Type implementation
////////////////////////////////////////////////////////////////////////

Type::Type() {
}

Type::~Type() {
}

const Member *Type::find_member(const std::string &name) const {
  for (unsigned i = 0; i < get_num_members(); ++i) {
    const Member &member = get_member(i);
    if (member.get_name() == name)
      return &member;
  }
  return nullptr;
}

std::string Type::as_str() const {
  std::set<const Type *> seen;
  return as_str(seen);
}

const Type *Type::get_unqualified_type() const {
  // only QualifiedType will need to override this member function
  return this;
}

bool Type::is_basic() const {
  return false;
}

bool Type::is_void() const {
  return false;
}

bool Type::is_struct() const {
  return false;
}

bool Type::is_pointer() const {
  return false;
}

bool Type::is_array() const {
  return false;
}

bool Type::is_function() const {
  return false;
}

bool Type::is_volatile() const {
  return false;
}

bool Type::is_const() const {
  return false;
}

BasicTypeKind Type::get_basic_type_kind() const {
  RuntimeError::raise("not a BasicType");
}

bool Type::is_signed() const {
  RuntimeError::raise("not a BasicType");
}

void Type::add_member(const Member &member) {
  RuntimeError::raise("type does not have members");
}

unsigned Type::get_num_members() const {
  RuntimeError::raise("type does not have members");
}

const Member &Type::get_member(unsigned index) const {
  RuntimeError::raise("type does not have members");
}

unsigned Type::get_field_offset(const std::string &name) const {
  RuntimeError::raise("not a StructType");
}

bool Type::has_base_type() const {
  return false;
}

std::shared_ptr<Type> Type::get_base_type() const {
  RuntimeError::raise("type does not have a base type");
}

unsigned Type::get_array_size() const {
  RuntimeError::raise("not an ArrayType");
}

bool Type::is_recursive(std::set<const Type *> &seen) const {
  if (has_base_type()) {
    seen.insert(this);
    return get_base_type()->is_recursive(seen);
  }
  return false;
}

////////////////////////////////////////////////////////////////////////
// HasBaseType implementation
////////////////////////////////////////////////////////////////////////

HasBaseType::HasBaseType(std::shared_ptr<Type> base_type)
  : m_base_type(base_type) {
}

HasBaseType::~HasBaseType() {
}

bool HasBaseType::has_base_type() const {
  return true;
}

std::shared_ptr<Type> HasBaseType::get_base_type() const {
  return m_base_type;
}

////////////////////////////////////////////////////////////////////////
// Member implementation
////////////////////////////////////////////////////////////////////////

Member::Member(const std::string &name, std::shared_ptr<Type> type)
  : m_name(name)
  , m_type(type) {
}

Member::~Member() {
}

const std::string &Member::get_name() const {
  return m_name;
}

std::shared_ptr<Type> Member::get_type() const {
  return m_type;
}

////////////////////////////////////////////////////////////////////////
// HasMembers implementation
////////////////////////////////////////////////////////////////////////

HasMembers::HasMembers() {
}

HasMembers::~HasMembers() {
}

std::string HasMembers::as_str(std::set<const Type *> &seen) const {
  // subclass should already have marked this object as seen
  assert(seen.count(this) == 1);

  std::string s;

  for (unsigned i = 0; i < get_num_members(); ++i) {
    if (i > 0)
      s += ", ";
    const Member &member = get_member(i);
    s += member.get_type()->as_str(seen);
  }

  return s;
}

bool HasMembers::is_recursive(std::set<const Type *> &seen) const {
  // This should work for StructType and FunctionType
  if (seen.count(this) > 0)
    return true;

  // Mark this Type as seen
  seen.insert(this);

  // See if we detect self-reference in any of the members
  for (unsigned i = 0; i < get_num_members(); ++i) {
    const Member &member = get_member(i);
    if (member.get_type()->is_recursive(seen))
      return true;
  }

  // Delegate to default implementation (which will detect
  // self-reference in the base type if there is one)
  return Type::is_recursive(seen);
}

void HasMembers::add_member(const Member &member) {
  m_members.push_back(member);
}

unsigned HasMembers::get_num_members() const {
  return unsigned(m_members.size());
}

const Member &HasMembers::get_member(unsigned index) const {
  assert(index < m_members.size());
  return m_members[index];
}

////////////////////////////////////////////////////////////////////////
// QualifiedType implementation
////////////////////////////////////////////////////////////////////////

QualifiedType::QualifiedType(std::shared_ptr<Type> delegate, TypeQualifier type_qualifier)
  : HasBaseType(delegate)
  , m_type_qualifier(type_qualifier) {
}

QualifiedType::~QualifiedType() {
}

bool QualifiedType::is_same(const Type *other) const {
  // see whether type qualifiers differ, if they do, return false
  if (is_const() != other->is_const())
    return false;
  if (is_volatile() != other->is_volatile())
    return false;

  // compare unqualified types
  return get_unqualified_type()->is_same(other->get_unqualified_type());
}

std::string QualifiedType::as_str(std::set<const Type *> &seen) const {
  seen.insert(this);
  std::string s;
  assert(is_const() || is_volatile());
  s += is_const() ? "const " : "volatile ";
  s += get_base_type()->as_str(seen);
  return s;
}

const Type *QualifiedType::get_unqualified_type() const {
  return get_base_type()->get_unqualified_type();
}

bool QualifiedType::is_basic() const {
  return get_base_type()->is_basic();
}

bool QualifiedType::is_void() const {
  return get_base_type()->is_void();
}

bool QualifiedType::is_struct() const {
  return get_base_type()->is_struct();
}

bool QualifiedType::is_pointer() const {
  return get_base_type()->is_pointer();
}

bool QualifiedType::is_array() const {
  return get_base_type()->is_array();
}

bool QualifiedType::is_function() const {
  return get_base_type()->is_function();
}

bool QualifiedType::is_volatile() const {
  return m_type_qualifier == TypeQualifier::VOLATILE;
}

bool QualifiedType::is_const() const {
  return m_type_qualifier == TypeQualifier::CONST;
}

BasicTypeKind QualifiedType::get_basic_type_kind() const {
  return get_base_type()->get_basic_type_kind();
}

bool QualifiedType::is_signed() const {
  return get_base_type()->is_signed();
}

void QualifiedType::add_member(const Member &member) {
  get_base_type()->add_member(member);
}

unsigned QualifiedType::get_num_members() const {
  return get_base_type()->get_num_members();
}

const Member &QualifiedType::get_member(unsigned index) const {
  return get_base_type()->get_member(index);
}

unsigned QualifiedType::get_array_size() const {
  return get_base_type()->get_array_size();
}

unsigned QualifiedType::get_storage_size() const {
  return get_base_type()->get_storage_size();
}

unsigned QualifiedType::get_alignment() const {
  return get_base_type()->get_alignment();
}

////////////////////////////////////////////////////////////////////////
// BasicType implementation
////////////////////////////////////////////////////////////////////////

BasicType::BasicType(BasicTypeKind kind, bool is_signed)
  : m_kind(kind)
  , m_is_signed(is_signed) {
}

BasicType::~BasicType() {
}

bool BasicType::is_same(const Type *other) const {
  if (!other->is_basic())
    return false;
  return m_kind == other->get_basic_type_kind()
      && m_is_signed == other->is_signed();
}

std::string BasicType::as_str(std::set<const Type *> &seen) const {
  seen.insert(this);

  std::string s;

  if (!is_signed())
    s += "unsigned ";
  switch (m_kind) {
  case BasicTypeKind::CHAR:
    s += "char"; break;
  case BasicTypeKind::SHORT:
    s += "short"; break;
  case BasicTypeKind::INT:
    s += "int"; break;
  case BasicTypeKind::LONG:
    s += "long"; break;
  case BasicTypeKind::VOID:
    s += "void"; break;
  default:
    assert(false);
  }

  return s;
}

bool BasicType::is_basic() const {
  return true;
}

bool BasicType::is_void() const {
  return m_kind == BasicTypeKind::VOID;
}

BasicTypeKind BasicType::get_basic_type_kind() const {
  return m_kind;
}

bool BasicType::is_signed() const {
  return m_is_signed;
}

unsigned BasicType::get_storage_size() const {
  switch (m_kind) {
  case BasicTypeKind::CHAR: return 1;
  case BasicTypeKind::SHORT: return 2;
  case BasicTypeKind::INT: return 4;
  case BasicTypeKind::LONG: return 8;
  default:
    assert(false);
    return 0;
  }
}

unsigned BasicType::get_alignment() const {
  return get_storage_size();
}

////////////////////////////////////////////////////////////////////////
// StructType implementation
////////////////////////////////////////////////////////////////////////

StructType::StructType(const std::string &name)
  : m_name(name)
  , m_storage_size(0U)
  , m_alignment(0U) {
}

StructType::~StructType() {
}

bool StructType::is_same(const Type *other) const {
  // Trivial base case that avoids infinite recursion for recursive types
  if (this == other) return true;

  // In general, it should not be possible for two struct types
  // with the same name to exist in the same translation unit.
  // So, comparing names *should* be sufficient to determine
  // whether these are the same type.

  if (!other->is_struct())
    return false;


  const StructType *other_st = dynamic_cast<const StructType *>(other);
  if (m_name != other_st->m_name)
    return false;

#ifndef NDEBUG
  // checking structure equality, just to be sure

  if (get_num_members() != other->get_num_members())
    RuntimeError::raise("struct types with same name but different numbers of members");
  for (unsigned i = 0; i < get_num_members(); ++i) {
    const Member &left = get_member(i);
    const Member &right = other->get_member(i);

    if (left.get_name() != right.get_name())
      RuntimeError::raise("struct types with same name but different member name(s)");
    if (!left.get_type()->is_same(right.get_type().get()))
      RuntimeError::raise("struct types with same name but different member type(s)");
  }
#endif

  return true;
}

std::string StructType::as_str(std::set<const Type *> &seen) const {
  bool first_time = (seen.count(this) == 0);
  seen.insert(this);

  bool recursive = false;
  if (!first_time) {
    // If we're seeing a struct type a second time,
    // it might just be a non-recursive struct type that
    // happens to occur multiple times (in parameters,
    // fields, etc.). So, at this point, check to see
    // if the struct type is *actually* recursive.
    // If it isn't we can print its members in full safely.
    std::set<const Type *> rseen;
    recursive = this->is_recursive(rseen);
  }

  std::string s;

  s += "struct ";
  s += m_name;

  // Elaborate the struct type with its members only if this is
  // the first time we're seeing it. This avoids infinite recursion
  // for recursive struct types.

  if (first_time || !recursive) {
    s += " {";
    s += HasMembers::as_str(seen);
    s += "}";
  }

  return s;
}

bool StructType::is_struct() const {
  return true;
}

unsigned StructType::get_storage_size() const {
  if (m_storage_size == 0U)
    calculate_storage();
  return m_storage_size;
}

unsigned StructType::get_alignment() const {
  if (m_alignment == 0U)
    calculate_storage();
  return m_alignment;
}

unsigned StructType::get_field_offset(const std::string &name) const {
  StorageCalculator scalc;
  for (unsigned i = 0; i < get_num_members(); ++i) {
    const Member &member = get_member(i);
    unsigned offset = scalc.add_field(member.get_type());
    if (member.get_name() == name)
      return offset;
  }
  RuntimeError::raise("Attempt to get offset of nonexistent field '%s'", name.c_str());
}

void StructType::calculate_storage() const {
  StorageCalculator scalc;
  for (unsigned i = 0; i < get_num_members(); ++i) {
    const Member &member = get_member(i);
    scalc.add_field(member.get_type());
  }
  scalc.finish();
  m_storage_size = scalc.get_size();
  m_alignment = scalc.get_align();
}

////////////////////////////////////////////////////////////////////////
// FunctionType implementation
////////////////////////////////////////////////////////////////////////

FunctionType::FunctionType(std::shared_ptr<Type> base_type)
  : HasBaseType(base_type) {
}

FunctionType::~FunctionType() {
}

bool FunctionType::is_same(const Type *other) const {
  if (!other->is_function())
    return false;

  // see if return types are the same
  if (!get_base_type()->is_same(other->get_base_type().get()))
    return false;

  // see if numbers of parameters are the same
  if (get_num_members() != other->get_num_members())
    return false;

  // see if parameter types are the same
  for (unsigned i = 0; i < get_num_members(); ++i) {
    if (!get_member(i).get_type()->is_same(other->get_member(i).get_type().get()))
      return false;
  }

  return true;
}

std::string FunctionType::as_str(std::set<const Type *> &seen) const {
  bool first_time = (seen.count(this) == 0);

  // It should not be possible for a function type to be recursive
  assert(first_time);

  seen.insert(this);

  std::string s;

  s += "function (";
  s += HasMembers::as_str(seen);
  s += ") returning ";
  s += get_base_type()->as_str(seen);

  return s;
}

bool FunctionType::is_function() const {
  return true;
}

unsigned FunctionType::get_storage_size() const {
  RuntimeError::raise("a function does not have a storage size");
}

unsigned FunctionType::get_alignment() const {
  RuntimeError::raise("a function does not have an alignment");
}

////////////////////////////////////////////////////////////////////////
// PointerType implementation
////////////////////////////////////////////////////////////////////////

PointerType::PointerType(std::shared_ptr<Type> base_type)
  : HasBaseType(base_type) {
}

PointerType::~PointerType() {
}

bool PointerType::is_same(const Type *other) const {
  if (!other->is_pointer())
    return false;

  return get_base_type()->is_same(other->get_base_type().get());
}

std::string PointerType::as_str(std::set<const Type *> &seen) const {
  seen.insert(this);

  std::string s;

  s += "pointer to ";
  s += get_base_type()->as_str(seen);

  return s;
}

bool PointerType::is_pointer() const {
  return true;
}

unsigned PointerType::get_storage_size() const {
  return 8U;
}

unsigned PointerType::get_alignment() const {
  return 8U;
}

////////////////////////////////////////////////////////////////////////
// ArrayType implementation
////////////////////////////////////////////////////////////////////////

ArrayType::ArrayType(std::shared_ptr<Type> base_type, unsigned size)
  : HasBaseType(base_type)
  , m_size(size) {
}

ArrayType::~ArrayType() {
}

bool ArrayType::is_same(const Type *other) const {
  // Note: the only reason comparison of ArrayTypes might be useful
  // is for comparing pointers to arrays. In theory these
  // could arise if a function has a parameter whose declared type
  // is a multidimensional array.

  if (!other->is_array())
    return false;

  const ArrayType *other_at = dynamic_cast<const ArrayType *>(other);
  return m_size == other_at->m_size
      && get_base_type()->is_same(other->get_base_type().get());
}

std::string ArrayType::as_str(std::set<const Type *> &seen) const {
  seen.insert(this);

  std::string s;

  s += "array of ";
  s += std::to_string(m_size);
  s += " x ";
  s += get_base_type()->as_str(seen);

  return s;
}

bool ArrayType::is_array() const {
  return true;
}

unsigned ArrayType::get_array_size() const {
  return m_size;
}

unsigned ArrayType::get_storage_size() const {
  return get_base_type()->get_storage_size() * m_size;
}

unsigned ArrayType::get_alignment() const {
  return get_base_type()->get_alignment();
}
