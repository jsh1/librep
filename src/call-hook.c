/* call-hook.c -- lists of functions

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

DEFSYM(or, "or");
DEFSYM(and, "and");

DEFUN("call-hook", Fcall_hook, Scall_hook,
      (repv hook, repv arg_list, repv type), rep_Subr3) /*
::doc:rep.system#call-hook::
call-hook HOOK ARG-LIST [TYPE]

Call the hook named by the symbol HOOK, passing all functions the
arguments in the list ARG-LIST. Note that HOOK may also be the actual
list of functions to call.

TYPE defines how the return values of each function in the hook are
treated. If TYPE is nil they are ignored, if TYPE is the symbol `and'
the hook aborts after a function returns nil, if TYPE is `or' the hook
aborts when a function returns non-nil.

In all cases the value returned by the last-evaluated function is
returned.
::end:: */
{
  rep_TEST_INT_LOOP_COUNTER;

  if (!Qor) {
    rep_INTERN(or);
    rep_INTERN(and);
  }
    
  if (!rep_LISTP(hook)) {
    rep_DECLARE1(hook, rep_SYMBOLP);
    hook = Fsymbol_value(hook, Qt);
    if (rep_VOIDP(hook) || rep_NILP(hook)) {
      return rep_nil;
    }
  }

  rep_DECLARE2(arg_list, rep_LISTP);

  rep_GC_root gc_hook, gc_arg_list, gc_type;
  rep_PUSHGC(gc_hook, hook);
  rep_PUSHGC(gc_arg_list, arg_list);
  rep_PUSHGC(gc_type, type);

  repv value = rep_nil;
  while (rep_CONSP(hook)) {
    value = rep_apply(rep_CAR(hook), arg_list);
    hook = rep_CDR(hook);

    if (!value
	|| (type == Qand && value == rep_nil)
	|| (type == Qor && value != rep_nil))
    {
      break;
    }

    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      value = 0;
      break;
    }
  }

  rep_POPGC; rep_POPGC; rep_POPGC;
  return value;
}

void
rep_call_hook_init(void)
{
  repv tem = rep_push_structure ("rep.system");
  rep_ADD_SUBR(Scall_hook);
  rep_pop_structure (tem);
}
