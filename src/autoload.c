/* autoload.c -- autoloading code

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

DEFSYM(autoload, "autoload");
DEFSYM(load, "load");

DEFSYM(invalid_autoload, "invalid-autoload");
DEFSTRING(err_invalid_autoload, "Invalid autoload definition");

/* Autoloads a value; CLOSURE is a closure enclosing the autoload
   definition. The definition is a list `(autoload SYMBOL FILE ...)'
   This function tries to load FILE, then returns the value of SYMBOL
   if successful, or 0 for some kind of error. Note, closure CLOSURE
   must be active when this function is called. */

repv
rep_load_autoload(repv closure)
{
  DEFSTRING(invl_autoload, "Can only autoload from symbols");

  if (!rep_CLOSUREP(closure)) {
    return Fsignal(Qinvalid_autoload,
		   rep_list_2(closure, rep_VAL(&invl_autoload)));
  }

  repv aload_def = rep_CLOSURE(closure)->fun;

  if (rep_CONSP(aload_def)) {
    aload_def = rep_CDR(aload_def);
  }

  if (!rep_CONSP(aload_def)
      || !rep_SYMBOLP(rep_CAR(aload_def))
      || !rep_CONSP(rep_CDR(aload_def))
      || !rep_STRINGP(rep_CAR(rep_CDR(aload_def))))
  {
    return Fsignal(Qinvalid_autoload,
		   rep_list_2(aload_def, rep_VAL(&invl_autoload)));
  }

  repv fun = rep_CAR(aload_def);
  repv file = rep_CAR(rep_CDR(aload_def));

  /* Find the `load` function in the environment of the autoload def.

     Caller is required to have installed the environment of `closure`
     so this will do the right thing. */

  repv load = Fsymbol_value(Qload, rep_nil);

  if (!load) {
    return 0;
  }

  /* Invalidate the autoload defn, so we don't keep trying to
     autoload indefinitely. */

  rep_CDR(aload_def) = rep_nil;

  rep_GC_root gc_fun;
  rep_PUSHGC(gc_fun, fun);

  repv result = rep_call_lisp2(load, file, Qt);

  rep_POPGC;

  if (!result) {
    return 0;
  }

  return Fsymbol_value(fun, rep_nil);
}

DEFUN("load-autoload", Fload_autoload,
       Sload_autoload, (repv def), rep_Subr1)
{
  rep_DECLARE1(def, rep_CLOSUREP);

  rep_stack_frame lc;
  lc.fun = Qautoload;
  lc.args = def;

  rep_PUSH_CALL(lc);
  rep_USE_CLOSURE(def);

  repv ret = rep_load_autoload(def);

  rep_POP_CALL(lc);

  return ret;
}

void
rep_autoload_init(void)
{
  rep_INTERN(autoload);
  rep_INTERN(load);

  rep_INTERN(invalid_autoload);
  rep_DEFINE_ERROR(invalid_autoload);

  repv tem = rep_push_structure("rep.lang.interpreter");
  rep_ADD_SUBR(Sload_autoload);
  rep_pop_structure(tem);
}
