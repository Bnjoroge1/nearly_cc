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

#ifndef LITERAL_VALUE_H
#define LITERAL_VALUE_H

#include <cstdint>
#include "location.h"

//! @file
//! Support for working with literal values appearing
//! in source code.
//! The most useful way to create an instance of LiteralValue
//! is to use one of the "`from`" static member functions,
//! which creates a LiteralValue from the lexeme of a token.

//! Kinds of literal values.
enum class LiteralValueKind {
  NONE,
  INTEGER,
  CHARACTER,
  STRING,
};

//! Helper class for representing literal values (integer, character,
//! and string). This class has value semantics (i.e., copying
//! and assignment are allowed.)
class LiteralValue {
private:
  LiteralValueKind m_kind;
  int64_t m_intval;
  std::string m_strval; // this is also used for character literals

  // integer literals can be specified as being unsigned and/or long
  bool m_is_unsigned;
  bool m_is_long;

public:
  //! Default constuctor. The LiteralValueKind is set to `NONE`.
  LiteralValue();

  //! Direct constructor for an `INTEGER` literal value.
  //! @param val the integer value
  //! @param is_unsigned true if the value is unsigned
  //! @param is_long true if the value is long
  LiteralValue(int64_t val, bool is_unsigned, bool is_long);

  //! Direct constructor for a `CHAR` literal value.
  //! @param c the character value
  LiteralValue(char c);

  //! Direct constructor for a `STRING` literal value.
  //! @param s thhe string value
  LiteralValue(const std::string &s);

  //! Copy constructor.
  //! @param other the LiteralValue object to copy from
  LiteralValue(const LiteralValue &other);

  ~LiteralValue();

  //! Assignment operator.
  //! @param rhs the LiteralValue object to copy from
  LiteralValue &operator=(const LiteralValue &rhs);

  //! Get the LiteralValueKind of this literal value.
  //! @return the LiteralValueKind
  LiteralValueKind get_kind() const;

  //! Get the integer value of an `INTEGER` literal value.
  //! @return the integer value
  int64_t get_int_value() const;

  //! Get the character value of an `CHAR` literal value.
  //! @return the character value
  char get_char_value() const;

  //! Get the string value of an `STRING` literal value.
  //! Note that this is the contents of the actual string literal lexeme,
  //! meaning that it will be quoted and could have embedded escape sequences.
  //! Use the @ref get_str_value_escaped() member function to get the
  //! actual value of the string.
  //! @return the contents of the string literal lexeme
  std::string get_str_value() const;

  //! Get the true value of a `STRING` literal value (as opposed to the
  //! contents of the original string literal lexeme.)
  //! @return the true string value
  std::string get_str_value_escaped() const;

  //! Check whether an `INTEGER` value is unsigned.
  //! @return true if the `INTEGER` value is unsigned, false if not
  bool is_unsigned() const;

  //! Check whether an `INTEGER` value is long.
  //! @return true if the `INTEGER` value is long, false if not
  bool is_long() const;

  // helper functions to create a LiteralValue from a token lexeme

  //! Create a LiteralValue from the lexeme of a character literal token.
  //! @param lexeme the lexeme of a character literal token
  //! @param loc the source Location of the character literal token
  //! @return the LiteralValue representing the value of the character literal
  static LiteralValue from_char_literal(const std::string &lexeme, const Location &loc);

  //! Create a LiteralValue from the lexeme of a integer literal token.
  //! @param lexeme the lexeme of a integer literal token
  //! @param loc the source Location of the integer literal token
  //! @return the LiteralValue representing the value of the integer literal
  static LiteralValue from_int_literal(const std::string &lexeme, const Location &loc);

  //! Create a LiteralValue from the lexeme of a string literal token.
  //! @param lexeme the lexeme of a string literal token
  //! @param loc the source Location of the string literal token
  //! @return the LiteralValue representing the value of the string literal
  static LiteralValue from_str_literal(const std::string &lexeme, const Location &loc);

private:
  static std::string strip_quotes(const std::string &lexeme, char quote);
};

#endif // LITERAL_VALUE_H
