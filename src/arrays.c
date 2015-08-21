/* arrays.c -- array (vector or string) functions

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

DEFUN("arrayp", Farrayp, Sarrayp, (repv arg), rep_Subr1) /*
::doc:rep.data#arrayp::
arrayp ARG

Returns t when ARG is an array.
::end:: */
{
  return rep_VECTORP(arg) || rep_STRINGP(arg) || rep_BYTECODEP(arg) ? Qt : rep_nil;
}

DEFUN("aset", Faset, Saset, (repv array, repv index, repv new), rep_Subr3) /*
::doc:rep.data#aset::
aset ARRAY INDEX NEW-VALUE

Sets element number INDEX (a positive integer) of ARRAY (can be a vector
or a string) to NEW-VALUE, returning NEW-VALUE. Note that strings
can only contain characters (ie, integers).
::end:: */
{
  rep_DECLARE2(index, rep_NON_NEG_INT_P);

  if (rep_STRINGP(array)) {
    if (!rep_STRING_WRITABLE_P(array)) {
      return Fsignal(Qsetting_constant, rep_LIST_1(array));
    }
    if (rep_INT(index) < rep_STRING_LEN(array)) {
      rep_DECLARE3(new, rep_INTP);
      ((unsigned char *)rep_STR(array))[rep_INT(index)] = rep_INT(new);
      rep_string_modified(array);
      return new;
    } else {
      return rep_signal_arg_error(index, 2);
    }
  } else if (rep_VECTORP(array) || rep_BYTECODEP(array)) {
    if (!rep_VECTOR_WRITABLE_P(array)) {
      return Fsignal(Qsetting_constant, rep_LIST_1(array));
    }
    if (rep_INT(index) < rep_VECT_LEN(array)) {
      rep_VECTI(array, rep_INT(index)) = new;
      return new;
    } else {
      return rep_signal_arg_error(index, 2);
    }
  } else {
    return rep_signal_arg_error(array, 1);
  }
}

DEFUN("aref", Faref, Saref, (repv array, repv index), rep_Subr2) /*
::doc:rep.data#aref::
aref ARRAY INDEX

Returns the INDEXth (a non-negative integer) element of ARRAY, which
can be a vector or a string. INDEX starts at zero.
::end:: */
{
  rep_DECLARE2(index, rep_NON_NEG_INT_P);

  if (rep_STRINGP(array)) {
    if (rep_INT(index) < rep_STRING_LEN(array)) {
      return rep_MAKE_INT(((unsigned char *)rep_STR(array))[rep_INT(index)]);
    } else {
      return rep_signal_arg_error (index, 2);
    }
  } else if (rep_VECTORP(array) || rep_BYTECODEP(array)) {
    if (rep_INT(index) < rep_VECT_LEN(array)) {
      return rep_VECTI(array, rep_INT(index));
    } else {
      return rep_signal_arg_error(index, 2);
    }
  } else {
    return rep_signal_arg_error(array, 1);
  }
}

void
rep_arrays_init(void)
{
  repv tem = rep_push_structure("rep.data");
  rep_ADD_SUBR(Sarrayp);
  rep_ADD_SUBR(Saset);
  rep_ADD_SUBR(Saref);
  rep_pop_structure(tem);
}
