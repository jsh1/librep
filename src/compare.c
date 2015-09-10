/* compare.c -- various predicate functions

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

/* Returns zero if V1 == V2, less than zero if V1 < V2, and greater than
   zero otherwise. */

int
rep_value_cmp(repv v1, repv v2)
{
  if (v1 == v2) {
    return 0;
  }

  if (v1 && v2) {
    rep_type *t1 = rep_get_data_type(rep_TYPE(v1));
    if (t1) {
      return t1->compare(v1, v2);
    }
  }

  return 1;
}

DEFUN("equal?", Fequal, Sequal, (repv val1, repv val2), rep_Subr2) /*
::doc:rep.data#equal?::
equal? VALUE1 VALUE2

Compares VALUE1 and VALUE2, compares the actual structure of the
objects not just whether the objects are one and the same. ie, will
return t for two strings built from the same characters in the same
order even if the strings' location in memory is different.
::end:: */
{
  return rep_value_cmp(val1, val2) == 0 ? Qt : rep_nil;
}

DEFUN("eq?", Feq, Seq, (repv val1, repv val2), rep_Subr2) /*
::doc:rep.data#eq?::
eq? VALUE1 VALUE2

Returns t if VALUE1 and VALUE2 are one and the same object. Note that
this may or may not be true for numbers of the same value (see `eqv?').
::end:: */
{
  return val1 == val2 ? Qt : rep_nil;
}

DEFUN("not", Fnot, Snot, (repv arg), rep_Subr1) /*
::doc:rep.data#not::
not ARG

If ARG is nil returns t, else returns nil.
::end:: */
{
  return rep_NILP(arg) ? Qt : rep_nil;
}

#define APPLY_COMPARISON(op)			\
  if (argc < 2) {				\
    return rep_signal_missing_arg(argc + 1);	\
  }						\
  for (intptr_t i = 1; i < argc; i++) {		\
    repv a = argv[i-1], b = argv[i];		\
    int sign;					\
    if (rep_NUMBERP(a) || rep_NUMBERP(b)) {	\
      sign = rep_compare_numbers(a, b);		\
    } else {					\
      sign = rep_value_cmp(a, b);		\
    }						\
    if (!(sign op 0)) {				\
      return rep_nil;				\
    }						\
  }						\
  return Qt;

DEFUN("=", Fnum_eq, Snum_eq, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.data#=::
= ARG1 ARG2 [ARG3 ...]

Returns t if each value is the same as every other value. (Using
`equal?' to compare values, except for numbers, where exactness is
ignored.)
::end:: */
{
  APPLY_COMPARISON(==)
}

DEFUN("/=", Fnum_noteq, Snum_noteq, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.data#:/=::
/= ARG1 ARG2 ...

Returns t if each value is different from every other value. (Using
`equal?' to compare values, except for numbers, where exactness is
ignored.)
::end:: */
{
  repv ret = Fnum_eq(argc, argv);
  return !ret ? 0 : ret == rep_nil ? Qt : rep_nil;
}

DEFUN(">", Fgtthan, Sgtthan, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.data#>::
> ARG1 ARG2 [ARG3 ...]

Returns t if ARG1 is greater than ARG2, and if ARG2 is greater than ARG3,
and so on. Note that this command isn't limited to numbers, it can do
strings, positions, marks, etc as well.
::end:: */
{
  APPLY_COMPARISON(>)
}

DEFUN(">=", Fgethan, Sgethan, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.data#>=::
>= ARG1 ARG2 [ARG3 ...]

Returns t if ARG1 is greater-or-equal than ARG2. Note that this command
isn't limited to numbers, it can do strings, positions, marks, etc as well.
::end:: */
{
  APPLY_COMPARISON(>=)
}

DEFUN("<", Fltthan, Sltthan, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.data#<::
< ARG1 ARG2 [ARG3 ...]

Returns t if ARG1 is less than ARG2. Note that this command isn't limited to
numbers, it can do strings, positions, marks, etc as well.
::end:: */
{
  APPLY_COMPARISON(<)
}

DEFUN("<=", Flethan, Slethan, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.data#<=::
<= ARG1 ARG2 [ARG3 ...]

Returns t if ARG1 is less-or-equal than ARG2. Note that this command isn't
limited to numbers, it can do strings, positions, marks, etc as well.
::end:: */
{
  APPLY_COMPARISON(<=)
}

void
rep_compare_init(void)
{
  repv tem = rep_push_structure("rep.data");
  rep_ADD_SUBR(Snot);
  rep_ADD_SUBR(Sequal);
  rep_ADD_SUBR(Seq);
  rep_ADD_SUBR(Snum_eq);
  rep_ADD_SUBR(Snum_noteq);
  rep_ADD_SUBR(Sgtthan);
  rep_ADD_SUBR(Sgethan);
  rep_ADD_SUBR(Sltthan);
  rep_ADD_SUBR(Slethan);
  rep_pop_structure(tem);
}
