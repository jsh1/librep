/* characters.c -- character functions

   Copyright (C) 1993-2015 John Harper <jsh@unfactored.org>

   This file is part of Librep.

   Librep is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Librep is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Librep; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "repint.h"

#include <string.h>
#include <ctype.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

#define CHAR_TABLE_SIZE 128
#define CHAR_HASH(x) ((x) & (CHAR_TABLE_SIZE - 1))

repv
rep_intern_char(uint32_t c)
{
  static repv table;

  if (!table) {
    table = rep_make_vector(CHAR_TABLE_SIZE);
    memset(rep_VECT(table)->array, 0, sizeof(repv) * CHAR_TABLE_SIZE);
    rep_mark_static(&table);
  }

  unsigned int h = CHAR_HASH(c);

  for (repv ch = rep_VECTI(table, h); ch; ch = rep_CHAR(ch)->next) {
    if (rep_CHAR_VALUE(ch) == c) {
      return ch;
    }
  }

  repv ch = rep_make_tuple(rep_Char, 0, 0);

  rep_CHAR(ch)->value = rep_MAKE_INT(c);
  rep_CHAR(ch)->next = rep_VECTI(table, h);
  rep_VECTI(table, h) = ch;

  return ch;
}

static int
char_cmp(repv v1, repv v2)
{
  if (!rep_CHARP(v2)) {
    return 1;
  }

  int c1 = rep_CHAR_VALUE(v1);
  int c2 = rep_CHAR_VALUE(v2);

  return c1 - c2;
}

static void
char_princ(repv stream, repv obj)
{
  uint32_t c = rep_CHAR_VALUE(obj);

  /* FIXME: UTF-8 conversion? */

  if (c < 256) {
    rep_stream_putc(stream, c);
  }
}

static void
char_print(repv stream, repv obj)
{
  uint32_t c = rep_CHAR_VALUE(obj);
  
  if (c < 256) {
    char buf[8];
    buf[0] = '#';
    buf[1] = '\\';
    if (c < 32 || c > 126) {
      buf[2] = '0' + ((c >> 6) & 7);
      buf[3] = '0' + ((c >> 3) & 7);
      buf[4] = '0' + ((c >> 0) & 7);
      rep_stream_puts(stream, buf, 5, false);
    } else {
      buf[2] = (char)c;
      rep_stream_puts(stream, buf, 3, false);
    }
  } else {
    /* FIXME: print #\uHEX representation? */
    rep_stream_puts(stream, "#<char>", strlen("#<char>"), false);
  }
}

DEFUN("char?", Fcharp, Scharp, (repv arg), rep_Subr1) /*
::doc:rep.data#char?::
char? ARG

Returns true if ARG is a character.
::end:: */
{
  return rep_CHARP(arg) ? Qt : rep_nil;
}

DEFUN("integer->char", Finteger_to_char,
      Sinteger_to_char, (repv integer), rep_Subr1) /*
::doc:rep.data#integer->char::
integer->char INTEGER

Returns the character with UTF-32 representation INTEGER.
::end:: */
{
  rep_DECLARE1(integer, rep_INTP);

  return rep_intern_char(rep_INT(integer));
}

DEFUN("char->integer", Fchar_to_integer,
      Schar_to_integer, (repv ch), rep_Subr1) /*
::doc:rep.data#char->integer::
char->integer CHAR

Returns the UTF-32 representation of CHAR.
::end:: */
{
  rep_DECLARE1(ch, rep_CHARP);

  return rep_MAKE_INT(rep_CHAR_VALUE(ch));
}

static repv
char_compare(repv c1, repv c2)
{
  rep_DECLARE1(c1, rep_CHARP);
  rep_DECLARE2(c2, rep_CHARP);

  int C1 = rep_CHAR_VALUE(c1);
  int C2 = rep_CHAR_VALUE(c2);

  return rep_MAKE_INT(C1 - C2);
}

static repv
char_compare_ci(repv c1, repv c2)
{
  rep_DECLARE1(c1, rep_CHARP);
  rep_DECLARE2(c2, rep_CHARP);

  int C1 = rep_CHAR_VALUE(c1);
  int C2 = rep_CHAR_VALUE(c2);

  C1 = rep_toupper(C1);
  C2 = rep_toupper(C2);

  return rep_MAKE_INT(C1 - C2);
}

DEFUN("char=?", Fchar_equal, Schar_equal, (repv c1, repv c2), rep_Subr2) /*
::doc:rep.data#char=?::
char=? CHAR1 CHAR2

Returns t if CHAR1 and CHAR2 are the same, ignoring case.
::end:: */
{
  repv ret = char_compare(c1, c2);
  if (ret) {
    ret = rep_INT(ret) == 0 ? Qt : rep_nil;
  }
  return ret;
}

DEFUN("char<?", Fchar_less, Schar_less, (repv c1, repv c2), rep_Subr2) /*
::doc:rep.data#char<?::
char<? CHAR1 CHAR2

Returns t if CHAR1 is less than CHAR2, ignoring case.
::end:: */
{
  repv ret = char_compare(c1, c2);
  if (ret) {
    ret = rep_INT(ret) < 0 ? Qt : rep_nil;
  }
  return ret;
}

DEFUN("char<=?", Fchar_less_equal, Schar_less_equal,
      (repv c1, repv c2), rep_Subr2) /*
::doc:rep.data#char<=?::
char<=? CHAR1 CHAR2

Returns t if CHAR1 is less than or equal to CHAR2, ignoring case.
::end:: */
{
  repv ret = char_compare(c1, c2);
  if (ret) {
    ret = rep_INT(ret) <= 0 ? Qt : rep_nil;
  }
  return ret;
}

DEFUN("char>?", Fchar_greater, Schar_greater, (repv c1, repv c2), rep_Subr2) /*
::doc:rep.data#char>?::
char>? CHAR1 CHAR2

Returns t if CHAR1 is greater than CHAR2, ignoring case.
::end:: */
{
  repv ret = char_compare(c1, c2);
  if (ret) {
    ret = rep_INT(ret) > 0 ? Qt : rep_nil;
  }
  return ret;
}

DEFUN("char>=?", Fchar_greater_equal, Schar_greater_equal,
      (repv c1, repv c2), rep_Subr2) /*
::doc:rep.data#char>=?::
char>=? CHAR1 CHAR2

Returns t if CHAR1 is greater than or equal to CHAR2, ignoring
case.
::end:: */
{
  repv ret = char_compare(c1, c2);
  if (ret) {
    ret = rep_INT(ret) >= 0 ? Qt : rep_nil;
  }
  return ret;
}

DEFUN("char-ci=?", Fchar_equal_ci, Schar_equal_ci,
      (repv c1, repv c2), rep_Subr2) /*
::doc:rep.data#char-ci=?::
char-ci=? CHAR1 CHAR2

Returns t if CHAR1 and CHAR2 are the same, ignoring case.
::end:: */
{
  repv ret = char_compare_ci(c1, c2);
  if (ret) {
    ret = rep_INT(ret) == 0 ? Qt : rep_nil;
  }
  return ret;
}

DEFUN("char-ci<?", Fchar_less_ci, Schar_less_ci,
      (repv c1, repv c2), rep_Subr2) /*
::doc:rep.data#char-ci<?::
char-ci<? CHAR1 CHAR2

Returns t if CHAR1 is less than CHAR2, ignoring case.
::end:: */
{
  repv ret = char_compare_ci(c1, c2);
  if (ret) {
    ret = rep_INT(ret) < 0 ? Qt : rep_nil;
  }
  return ret;
}

DEFUN("char-ci<=?", Fchar_less_equal_ci, Schar_less_equal_ci,
      (repv c1, repv c2), rep_Subr2) /*
::doc:rep.data#char-ci<=?::
char-ci<=? CHAR1 CHAR2

Returns t if CHAR1 is less than or equal to CHAR2, ignoring case.
::end:: */
{
  repv ret = char_compare_ci(c1, c2);
  if (ret) {
    ret = rep_INT(ret) <= 0 ? Qt : rep_nil;
  }
  return ret;
}

DEFUN("char-ci>?", Fchar_greater_ci, Schar_greater_ci,
      (repv c1, repv c2), rep_Subr2) /*
::doc:rep.data#char-ci>?::
char-ci>? CHAR1 CHAR2

Returns t if CHAR1 is greater than CHAR2, ignoring case.
::end:: */
{
  repv ret = char_compare_ci(c1, c2);
  if (ret) {
    ret = rep_INT(ret) > 0 ? Qt : rep_nil;
  }
  return ret;
}

DEFUN("char-ci>=?", Fchar_greater_equal_ci, Schar_greater_equal_ci,
      (repv c1, repv c2), rep_Subr2) /*
::doc:rep.data#char-ci>=?::
char-ci>=? CHAR1 CHAR2

Returns t if CHAR1 is greater than or equal to CHAR2, ignoring
case.
::end:: */
{
  repv ret = char_compare_ci(c1, c2);
  if (ret) {
    ret = rep_INT(ret) >= 0 ? Qt : rep_nil;
  }
  return ret;
}

DEFSYM(upcase_table, "upcase-table");
DEFSYM(downcase_table, "downcase-table");
DEFSYM(flatten_table, "flatten-table");

/* ::doc:rep.data#upcase-table::
256-byte string holding translations to turn each character into its
upper-case equivalent.
::end::
::doc:rep.data#downcase-table::
256-byte string holding translations to turn each character into its
lower-case equivalent.
::end::
::doc:rep.data#flatten-table::
Translation table to convert newline characters to spaces.
::end:: */

DEFUN("char-alphabetic?", Falpha_char_p,
      Salpha_char_p, (repv ch), rep_Subr1) /*
::doc:rep.data#char-alphabetic?::
char-alphabetic? CHAR

Returns t if CHAR is an alphabetic character.
::end:: */
{
  return rep_CHARP(ch) && rep_isalpha(rep_CHAR_VALUE(ch)) ? Qt : rep_nil;
}

DEFUN("char-upper-case?", Fupper_case_p,
      Supper_case_p, (repv ch), rep_Subr1) /*
::doc:rep.data#char-upper-case?::
char-upper-case? CHAR

Returns t if CHAR is upper case.
::end:: */
{
  return rep_CHARP(ch) && rep_isupper(rep_CHAR_VALUE(ch)) ? Qt : rep_nil;
}

DEFUN("char-lower-case?", Flower_case_p,
      Slower_case_p, (repv ch), rep_Subr1) /*
::doc:rep.data#char-lower-case?::
char-lower-case? CHAR

Returns t if CHAR is lower case.
::end:: */
{
  return rep_CHARP(ch) && rep_islower(rep_CHAR_VALUE(ch)) ? Qt : rep_nil;
}

DEFUN("char-numeric?", Fdigit_char_p, Sdigit_char_p, (repv ch), rep_Subr1) /*
::doc:rep.data#char-numeric?::
char-numeric? CHAR

Returns t if CHAR is a digit.
::end:: */
{
  return rep_CHARP(ch) && rep_isdigit(rep_CHAR_VALUE(ch)) ? Qt : rep_nil;
}

DEFUN("char-alphanumeric?", Falphanumericp,
      Salphanumericp, (repv ch), rep_Subr1) /*
::doc:rep.data#char-alphanumeric?::
char-alphanumeric? CHAR

Returns t if CHAR is alpha-numeric.
::end:: */
{
  return rep_CHARP(ch) && rep_isalnum (rep_CHAR_VALUE(ch)) ? Qt : rep_nil;
}

DEFUN("char-whitespace?", Fspace_char_p,
      Sspace_char_p, (repv ch), rep_Subr1) /*
::doc:rep.data#char-whitespace?::
char-whitespace? CHAR

Returns t if CHAR is whitespace.
::end:: */
{
  return rep_CHARP(ch) && rep_isspace(rep_CHAR_VALUE(ch)) ? Qt : rep_nil;
}

DEFUN("char-upcase", Fchar_upcase, Schar_upcase, (repv ch), rep_Subr1) /*
::doc:rep.data#char-upcase::
char-upcase CHAR

Returns the upper-case equivalent of CHAR.
::end:: */
{
  rep_DECLARE1(ch, rep_CHARP);

  return rep_intern_char(rep_toupper(rep_CHAR_VALUE(ch)));
}

DEFUN("char-downcase", Fchar_downcase, Schar_downcase, (repv ch), rep_Subr1) /*
::doc:rep.data#char-downcase::
char-downcase CHAR

Returns the lower-case equivalent of CHAR.
::end:: */
{
  rep_DECLARE1(ch, rep_CHARP);

  return rep_intern_char(rep_tolower(rep_CHAR_VALUE(ch)));
}

void
rep_characters_init(void)
{
  static rep_type type = {
    .car = rep_Char,
    .name = "character",
    .compare = char_cmp,
    .print = char_print,
    .princ = char_princ,
  };

  rep_define_type(&type);

  repv tem = rep_push_structure("rep.data");

  rep_ADD_SUBR(Scharp);
  rep_ADD_SUBR(Sinteger_to_char);
  rep_ADD_SUBR(Schar_to_integer);

  rep_ADD_SUBR(Schar_equal);
  rep_ADD_SUBR(Schar_less);
  rep_ADD_SUBR(Schar_less_equal);
  rep_ADD_SUBR(Schar_greater);
  rep_ADD_SUBR(Schar_greater_equal);
  rep_ADD_SUBR(Schar_equal_ci);
  rep_ADD_SUBR(Schar_less_ci);
  rep_ADD_SUBR(Schar_less_equal_ci);
  rep_ADD_SUBR(Schar_greater_ci);
  rep_ADD_SUBR(Schar_greater_equal_ci);

  rep_ADD_SUBR(Salpha_char_p);
  rep_ADD_SUBR(Supper_case_p);
  rep_ADD_SUBR(Slower_case_p);
  rep_ADD_SUBR(Sdigit_char_p);
  rep_ADD_SUBR(Salphanumericp);
  rep_ADD_SUBR(Sspace_char_p);
  rep_ADD_SUBR(Schar_upcase);
  rep_ADD_SUBR(Schar_downcase);

  repv up = rep_allocate_string(257);
  repv down = rep_allocate_string(257);

  for(int i = 0; i < 256; i++) {
    rep_MUTABLE_STR(up)[i] = rep_toupper(i);
    rep_MUTABLE_STR(down)[i] = rep_tolower(i);
  }
  rep_MUTABLE_STR(up)[256] = 0;
  rep_MUTABLE_STR(down)[256] = 0;

  rep_INTERN(upcase_table);
  rep_INTERN(downcase_table);
  Fset(Qupcase_table, up);
  Fset(Qdowncase_table, down);

  repv flatten = rep_allocate_string(12);

  for(int i = 0; i < 10; i++) {
    rep_MUTABLE_STR(flatten)[i] = i;
  }
  rep_MUTABLE_STR(flatten)[10] = ' ';
  rep_MUTABLE_STR(flatten)[11] = 0;

  rep_INTERN(flatten_table);
  Fset(Qflatten_table, flatten);

  rep_pop_structure(tem);
}
