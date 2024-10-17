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

#include <cassert>
#include "exceptions.h"
#include "location.h"
#include "literal_value.h"

LiteralValue::LiteralValue()
  : m_kind(LiteralValueKind::NONE)
  , m_intval(0)
  , m_is_unsigned(false)
  , m_is_long(false) {
}

LiteralValue::LiteralValue(int64_t val, bool is_unsigned, bool is_long)
  : m_kind(LiteralValueKind::INTEGER)
  , m_intval(val)
  , m_is_unsigned(is_unsigned)
  , m_is_long(is_long) {
}

LiteralValue::LiteralValue(char c)
  : m_kind(LiteralValueKind::CHARACTER)
  , m_intval(0)
  , m_strval(1, c)
  , m_is_unsigned(false)
  , m_is_long(false) {

}

LiteralValue::LiteralValue(const std::string &s)
  : m_kind(LiteralValueKind::STRING)
  , m_intval(0)
  , m_strval(s)
  , m_is_unsigned(false)
  , m_is_long(false) {
}

LiteralValue::LiteralValue(const LiteralValue &other)
  : m_kind(other.m_kind)
  , m_intval(other.m_intval)
  , m_strval(other.m_strval)
  , m_is_unsigned(other.m_is_unsigned)
  , m_is_long(other.m_is_long) {
}

LiteralValue::~LiteralValue() {
}

LiteralValue &LiteralValue::operator=(const LiteralValue &rhs) {
  if (this != &rhs) {
    m_kind = rhs.m_kind;
    m_intval = rhs.m_intval;
    m_strval = rhs.m_strval;
    m_is_unsigned = rhs.m_is_unsigned;
    m_is_long = rhs.m_is_long;
  }
  return *this;
}

LiteralValueKind LiteralValue::get_kind() const {
  return m_kind;
}

int64_t LiteralValue::get_int_value() const {
  assert(m_kind == LiteralValueKind::INTEGER);
  return m_intval;
}

char LiteralValue::get_char_value() const {
  assert(m_kind == LiteralValueKind::CHARACTER);
  assert(m_strval.size() == 1);
  return m_strval[0];
}

std::string LiteralValue::get_str_value() const {
  assert(m_kind == LiteralValueKind::STRING);
  return m_strval;
}

std::string LiteralValue::get_str_value_escaped() const {
  std::string s = get_str_value();
  std::string escaped;
  for (auto i = s.begin(); i != s.end(); ++i) {
    switch (*i) {
    case '\t':
      escaped += "\\t"; break;
    case '\n':
      escaped += "\\n"; break;
    case '\r':
      escaped += "\\r"; break;
    case '\\':
      escaped += "\\\\"; break;
    case '"':
      escaped += "\\\""; break;
    default:
      escaped += *i;
    }
  }
  return escaped;
}

bool LiteralValue::is_unsigned() const {
  assert(m_kind == LiteralValueKind::INTEGER);
  return m_is_unsigned;
}

bool LiteralValue::is_long() const {
  assert(m_kind == LiteralValueKind::INTEGER);
  return m_is_long;
}

LiteralValue LiteralValue::from_char_literal(const std::string &lexeme, const Location &loc) {
  std::string s = strip_quotes(lexeme, '\'');

  char cval;

  if (s[0] == '\\') {
    // the lexer currently only allows one escape character
    // (so, for example, it doesn't support hex escapes)
    assert(s.size() == 2);

    // decode the escape sequence
    switch (s[1]) {
    case 't': cval = '\t'; break;
    case 'n': cval = '\n'; break;
    case 'r': cval = '\r'; break;
    case '\'': cval = '\''; break;
    case '\\': cval = '\\'; break;
    default:
      SemanticError::raise(loc, "Unsupported escape character %d", int(s[1]));
    }

    return LiteralValue(cval);
  } else {
    assert(s.size() == 1);
    if (s[0] < 32 || s[0] > 127)
      SemanticError::raise(loc, "Invalid literal character value %d", int(s[0]));
    return LiteralValue(s[0]);
  }
}

LiteralValue LiteralValue::from_int_literal(const std::string &lexeme, const Location &loc) {
  std::string s = lexeme;

  int64_t value;

  if (s.rfind("0x", 0) == 0 || s.rfind("0X", 0) == 0) {
    // hex constant
    value = int64_t(std::stoll(lexeme, nullptr, 16));
  } else {
    // decimal int literal (we don't support octal constants)
    value = int64_t(std::stoll(lexeme, nullptr, 10));
  }

  // a somewhat crude way of checking for trailing 'L' and 'U' suffixes
  bool is_unsigned = false, is_long = false;
  size_t i = s.size() - 1;
  while (i > 0) {
    switch (s[i]) {
    case 'l':
    case 'L':
      is_long = true; break;

    case 'u':
    case 'U':
      is_unsigned = true; break;

    default:
      i = 1;
    }

    --i;
  }

  return LiteralValue(value, is_unsigned, is_long);
}

LiteralValue LiteralValue::from_str_literal(const std::string &lexeme, const Location &loc) {
  std::string s = strip_quotes(lexeme, '"');

  size_t pos = 0;

  std::string value;

  while (pos < s.size()) {
    char c = s[pos];

    if (c != '\\')
      value.push_back(c);
    else {
      assert(pos + 1 < s.size());
      ++pos;
      char cval;
      switch (s[pos]) {
      case 't': cval = '\t'; break;
      case 'n': cval = '\n'; break;
      case 'r': cval = '\r'; break;
      case '"': cval = '"'; break;
      case '\\': cval = '\\'; break;
      default:
        SemanticError::raise(loc, "Unsupported escape character %d", int(s[1]));
      }
      value.push_back(cval);
    }

    ++pos;
  }

  return LiteralValue(value);
}

std::string LiteralValue::strip_quotes(const std::string &lexeme, char quote) {
  assert(lexeme.size() >= 3);
  assert(lexeme.front() == quote);
  assert(lexeme.back() == quote);

  std::string s(lexeme.substr(1));
  s.pop_back();
  return s;
}
