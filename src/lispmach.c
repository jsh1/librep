/* lispmach.c -- Interpreter for compiled Lisp forms

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

#include <assert.h>

/* Define this to check if the compiler gets things right. */

#undef TRUST_NO_ONE

/* Define this to record bytecode-use histograms. */

#undef BYTECODE_PROFILE

DEFSYM(bytecode_error, "bytecode-error");

#ifdef BYTECODE_PROFILE
static int bytecode_profile[256];
#endif

#ifdef TRUST_NO_ONE
# define ASSERT(x) assert(x)
#else
# define ASSERT(x)
#endif

#define BC_APPLY_SELF 0

/* Pull in the actual bytecode vm. */

#include "lispmach.h"

/* Interface */

repv
rep_apply_bytecode(repv subr, int nargs, repv *args)
{
  assert(rep_BYTECODEP(subr));

  return inline_apply_bytecode(subr, nargs, args);
}

DEFUN("run-byte-code", Frun_byte_code, Srun_byte_code,
      (repv code, repv consts, repv stack), rep_Subr3)
{
  if (rep_STRUCTUREP(code)) {
    /* Install ourselves in this structure as its bytecode executor. */

    rep_STRUCTURE(code)->apply_bytecode = 0;
    return Qt;
  }

  rep_DECLARE1(code, rep_STRINGP);
  rep_DECLARE2(consts, rep_VECTORP);
  rep_DECLARE3(stack, rep_INTP);

  return bytecode_vm(code, consts, stack, 0, NULL);
}

DEFUN("validate-byte-code", Fvalidate_byte_code, Svalidate_byte_code,
      (repv bc_major, repv bc_minor), rep_Subr2) /*
::doc:rep.vm.interpreter#validate-byte-code::
validate-byte-code BC-MAJOR BC-MINOR

Check that byte codes from instruction set BC-MAJOR.BC-MINOR, may be
executed. If not, an error will be signalled.
::end:: */
{
  if (!rep_INTP(bc_major) || !rep_INTP(bc_minor)
      || rep_INT(bc_major) != BYTECODE_MAJOR_VERSION
      || rep_INT(bc_minor) > BYTECODE_MINOR_VERSION)
  {
    DEFSTRING(err, "File needs recompiling for current virtual machine");

    return Fsignal(Qbytecode_error,
		   rep_LIST_2(rep_VAL(&err),
			      Fsymbol_value(Qload_filename, Qt)));
  }

  return Qt;
}

DEFUN("make-byte-code-subr", Fmake_byte_code_subr,
      Smake_byte_code_subr, (repv args), rep_SubrN) /*
::doc:rep.vm.interpreter#make-byte-code-subr::
make-byte-code-subr CODE CONSTANTS STACK [DOC] [INTERACTIVE]

Return an object that can be used as the function value of a symbol.
::end:: */
{
  int len = rep_list_length(args);
  if (len < rep_BYTECODE_MIN_SLOTS) {
    return rep_signal_missing_arg(len + 1);
  }
    
  repv obj[5];

  if (!rep_STRINGP(rep_CAR(args))) {
    return rep_signal_arg_error(rep_CAR(args), 2);
  }

  obj[0] = rep_CAR(args);
  args = rep_CDR(args);

  if (!rep_VECTORP(rep_CAR(args))) {
    return rep_signal_arg_error(rep_CAR(args), 3);
  }

  obj[1] = rep_CAR(args);
  args = rep_CDR(args);

  if (!rep_INTP(rep_CAR(args))) {
    return rep_signal_arg_error(rep_CAR(args), 4);
  }

  obj[2] = rep_CAR(args);
  args = rep_CDR(args);

  int used = 3;

  if (rep_CONSP(args)) {
    obj[used++] = rep_CAR(args);
    args = rep_CDR(args);

    if (rep_CONSP(args)) {
      obj[used++] = rep_CAR(args);
      args = rep_CDR(args);

      if (obj[used - 1] == rep_nil) {
	used--;
      }
    }

    if (used == 4 && obj[used - 1] == rep_nil) {
      used--;
    }
  }

  repv vec = Fmake_vector(rep_MAKE_INT(used), rep_nil);
  if (!vec) {
    return 0;
  }

  rep_BYTECODE(vec)->car = ((rep_BYTECODE(vec)->car
			     & ~rep_CELL8_TYPE_MASK) | rep_Bytecode);
  for (int i = 0; i < used; i++) {
    rep_VECTI(vec, i) = obj[i];
  }

  return vec;
}

DEFUN("bytecode?", Fbytecodep, Sbytecodep, (repv arg), rep_Subr1) /*
::doc:rep.vm.interpreter#bytecode?::
bytecode? ARG

Returns t if ARG is a byte code subroutine(i.e. compiled Lisp code).
::end:: */
{
  return rep_BYTECODEP(arg) ? Qt : rep_nil;
}

#ifdef BYTECODE_PROFILE

static void
print_bytecode_profile(void)
{
  for (int i = 0; i < 256; i++) {
    printf("%8d %8d\n", i, bytecode_profile[i]);
  }
}

DEFUN("bytecode-profile", Fbytecode_profile,
       Sbytecode_profile, (repv reset), rep_Subr1)
{
  if (reset != rep_nil) {
    memset(bytecode_profile, 0, sizeof(bytecode_profile));
  } else {
    print_bytecode_profile();
  }

  return rep_nil;
}

#endif /* BYTECODE_PROFILE */

void
rep_lispmach_init(void)
{
  repv tem = rep_push_structure("rep.vm.interpreter");
  rep_ADD_SUBR(Srun_byte_code);
  rep_ADD_SUBR(Svalidate_byte_code);
  rep_ADD_SUBR(Smake_byte_code_subr);
  rep_ADD_SUBR(Sbytecodep);
#ifdef BYTECODE_PROFILE
  rep_ADD_SUBR(Sbytecode_profile);
  atexit(print_bytecode_profile);
#endif
  rep_INTERN(bytecode_error);
  rep_DEFINE_ERROR(bytecode_error);
  rep_pop_structure(tem);
}
