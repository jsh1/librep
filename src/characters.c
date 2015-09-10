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
  return rep_INTP(ch) && rep_isalpha(rep_INT(ch)) ? Qt : rep_nil;
}

DEFUN("char-upper-case?", Fupper_case_p,
      Supper_case_p, (repv ch), rep_Subr1) /*
::doc:rep.data#char-upper-case?::
char-upper-case? CHAR

Returns t if CHAR is upper case.
::end:: */
{
  return rep_INTP(ch) && rep_isupper(rep_INT(ch)) ? Qt : rep_nil;
}

DEFUN("char-lower-case?", Flower_case_p,
      Slower_case_p, (repv ch), rep_Subr1) /*
::doc:rep.data#char-lower-case?::
char-lower-case? CHAR

Returns t if CHAR is lower case.
::end:: */
{
  return rep_INTP(ch) && rep_islower(rep_INT(ch)) ? Qt : rep_nil;
}

DEFUN("char-numeric?", Fdigit_char_p, Sdigit_char_p, (repv ch), rep_Subr1) /*
::doc:rep.data#char-numeric?::
char-numeric? CHAR

Returns t if CHAR is a digit.
::end:: */
{
  return rep_INTP(ch) && rep_isdigit(rep_INT(ch)) ? Qt : rep_nil;
}

DEFUN("char-alphanumeric?", Falphanumericp,
      Salphanumericp, (repv ch), rep_Subr1) /*
::doc:rep.data#char-alphanumeric?::
char-alphanumeric? CHAR

Returns t if CHAR is alpha-numeric.
::end:: */
{
  return rep_INTP(ch) && rep_isalnum (rep_INT(ch)) ? Qt : rep_nil;
}

DEFUN("char-whitespace?", Fspace_char_p,
      Sspace_char_p, (repv ch), rep_Subr1) /*
::doc:rep.data#char-whitespace?::
char-whitespace? CHAR

Returns t if CHAR is whitespace.
::end:: */
{
  return rep_INTP(ch) && rep_isspace(rep_INT(ch)) ? Qt : rep_nil;
}

DEFUN("char-upcase", Fchar_upcase, Schar_upcase, (repv ch), rep_Subr1) /*
::doc:rep.data#char-upcase::
char-upcase CHAR

Returns the upper-case equivalent of CHAR.
::end:: */
{
  rep_DECLARE1(ch, rep_INTP);

  return rep_MAKE_INT(rep_toupper(rep_INT(ch)));
}

DEFUN("char-downcase", Fchar_downcase, Schar_downcase, (repv ch), rep_Subr1) /*
::doc:rep.data#char-downcase::
char-downcase CHAR

Returns the lower-case equivalent of CHAR.
::end:: */
{
  rep_DECLARE1(ch, rep_INTP);

  return rep_MAKE_INT(rep_tolower(rep_INT(ch)));
}

void
rep_characters_init(void)
{
  repv tem = rep_push_structure("rep.data");

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
    rep_STR(up)[i] = rep_toupper(i);
    rep_STR(down)[i] = rep_tolower(i);
  }
  rep_STR(up)[256] = 0;
  rep_STR(down)[256] = 0;

  rep_INTERN(upcase_table);
  rep_INTERN(downcase_table);
  Fset(Qupcase_table, up);
  Fset(Qdowncase_table, down);

  repv flatten = rep_allocate_string(12);

  for(int i = 0; i < 10; i++) {
    rep_STR(flatten)[i] = i;
  }
  rep_STR(flatten)[10] = ' ';
  rep_STR(flatten)[11] = 0;

  rep_INTERN(flatten_table);
  Fset(Qflatten_table, flatten);

  rep_pop_structure(tem);
}
