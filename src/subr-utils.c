/* subr-utils.c -- helper functions for built-in subrs

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

#include <stdarg.h>

/* Used to assign a list of argument values into separate variables.
   Note that optional args without values _are not_ initialized to nil,
   the caller of this function should do that.. */

bool
rep_assign_args(repv list, int required, int total, ...)
{
  rep_TEST_INT_LOOP_COUNTER;

  va_list vars;
  va_start(vars, total);

  for (int i = 0; i < total; i++) {
    repv *varp = va_arg(vars, repv *);

    if (!rep_CONSP(list)) {
      if (i >= required) {
	return true;
      } else {
	rep_signal_missing_arg(i);
	return false;
      }
    }

    *varp = rep_CAR(list);
    list = rep_CDR(list);

    rep_TEST_INT;
    if (rep_INTERRUPTP)
      return false;
  }

  return true;
}

/* Used for easy handling of `var' objects */

repv
rep_handle_var_int(repv val, int *data)
{
  int old = *data;
  if (rep_INTP(val)) {
    *data = rep_INT(val);
  }
  return rep_MAKE_INT(old);
}

/* Similar, but for variables containing greater than 24 bits of data,
   passed around as a cons cell containing two integers */

repv
rep_handle_var_long_int(repv val, long *data)
{
  long old = *data;
  if (rep_long_int_p(val)) {
    *data = rep_get_long_int(val);
  }
  return rep_make_long_int(old);
}
