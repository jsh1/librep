/* sequences.c -- sequence functions

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

DEFUN("length", Flength, Slength, (repv sequence), rep_Subr1) /*
::doc:rep.data#length::
length SEQUENCE

Returns the number of elements in SEQUENCE (a string, list or vector).
::end:: */
{
  if (sequence == rep_nil) {
    return rep_MAKE_INT(0);
  }

  switch (rep_TYPE(sequence)) {
  case rep_String:
    return rep_MAKE_INT(rep_STRING_LEN(sequence));
    break;

  case rep_Vector:
  case rep_Bytecode:
    return rep_MAKE_INT(rep_VECT_LEN(sequence));
    break;

  case rep_Cons: {
    int len = rep_list_length(sequence);
    if (len < 0) {
      return 0;
    }
    return rep_MAKE_INT(len);
    break; }

  default:
    return rep_signal_arg_error(sequence, 1);
  }
}

DEFUN("copy-sequence", Fcopy_sequence,
      Scopy_sequence, (repv seq), rep_Subr1) /*
::doc:rep.data#copy-sequence::
copy-sequence SEQUENCE

Returns a new sequence whose elements are eq? to those in SEQUENCE.
::end:: */
{
  rep_TEST_INT_LOOP_COUNTER;

  if (seq == rep_nil) {
    return rep_nil;
  }

  repv res = rep_nil;

  switch (rep_TYPE(seq)) {
  case rep_Cons: {
    repv *last = &res;
    while (rep_CONSP(seq)) {
      rep_TEST_INT;
      if (rep_INTERRUPTP) {
	return 0;
      }
      repv cell = Fcons(rep_CAR(seq), rep_nil);
      *last = cell;
      last = &rep_CDR(cell);
      seq = rep_CDR(seq);
    }
    break; }

  case rep_Vector:
  case rep_Bytecode:
    res = rep_make_vector(rep_VECT_LEN(seq));
    if (res) {
      intptr_t len = rep_VECT_LEN(seq);
      rep_VECT(res)->car = rep_VECT(seq)->car;
      for (intptr_t i = 0; i < len; i++) {
	rep_VECTI(res, i) = rep_VECTI(seq, i);
      }
    }
    break;

  case rep_String:
    res = rep_string_copy_n(rep_STR(seq), rep_STRING_LEN(seq));
    break;

  default:
    res = rep_signal_arg_error(seq, 1);
  }

  return res;
}

DEFUN("elt", Felt, Selt, (repv seq, repv index), rep_Subr2) /*
::doc:rep.data#elt::
elt SEQUENCE INDEX

Return the element of SEQUENCE at position INDEX (counting from zero).
::end:: */
{
  if (rep_LISTP(seq)) {
    return Fnth(index, seq);
  } else {
    return Faref(seq, index);
  }
}

DEFUN("sequence?", Fsequencep, Ssequencep, (repv arg), rep_Subr1) /*
::doc:rep.data#sequence?::
sequence? ARG

Returns t is ARG is a sequence (a list, vector or string).
::end:: */
{
  return rep_LISTP(arg) || rep_VECTORP(arg) || rep_STRINGP(arg)
    || rep_BYTECODEP(arg) ? Qt : rep_nil;
}

void
rep_sequences_init(void)
{
  repv tem = rep_push_structure("rep.data");
  rep_ADD_SUBR(Slength);
  rep_ADD_SUBR(Scopy_sequence);
  rep_ADD_SUBR(Selt);
  rep_ADD_SUBR(Ssequencep);
  rep_pop_structure(tem);
}
