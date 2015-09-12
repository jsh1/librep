/* error.c -- errors and exceptions

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

/* When a `throw' happens a function stuffs a cons-cell in here with,
   (TAG . VALUE). An error is the above with TAG Qerror and VALUE a
   list of relevant data. */

volatile repv rep_throw_value;

/* Used to avoid costly interrupt checking too often */

volatile int rep_test_int_counter = 0;

/* Function to test asynchronously for interrupts. If it detects an
   interrupt, it should set `rep_throw_value' to `rep_int_cell' */

void (*rep_test_int_fun)(void);

/* This cons cell is used for interrupts. We don't know if it's safe to
   call Fcons() (maybe in gc?) so this is always valid.  */

repv rep_int_cell, rep_term_cell;

DEFSYM(error, "error");
DEFSTRING(err_error, "Error");
DEFSYM(error_message, "error-message");
DEFSYM(invalid_function, "invalid-function");
DEFSTRING(err_invalid_function, "Invalid function");
DEFSYM(void_value, "void-value");
DEFSTRING(err_void_value, "Unbound variable");
DEFSYM(bad_arg, "bad-arg");
DEFSTRING(err_bad_arg, "Bad argument");
DEFSYM(invalid_read_syntax, "invalid-read-syntax");
DEFSTRING(err_invalid_read_syntax, "Invalid read syntax");
DEFSYM(end_of_stream, "end-of-stream");
DEFSTRING(err_end_of_stream, "End of stream");
DEFSYM(premature_end_of_stream, "premature-end-of-stream");
DEFSTRING(err_premature_end_of_stream, "Premature end of stream");
DEFSYM(invalid_lambda_list, "invalid-lambda-list");
DEFSTRING(err_invalid_lambda_list, "Invalid lambda list");
DEFSYM(missing_arg, "missing-arg");
DEFSTRING(err_missing_arg, "Required argument missing");
DEFSYM(invalid_macro, "invalid-macro");
DEFSTRING(err_invalid_macro, "Invalid macro definition");
DEFSYM(no_catcher, "no-catcher");
DEFSTRING(err_no_catcher, "No catcher for throw");
DEFSYM(file_error, "file-error");
DEFSTRING(err_file_error, "File error");
DEFSYM(invalid_stream, "invalid-stream");
DEFSTRING(err_invalid_stream, "Invalid stream");
DEFSYM(setting_constant, "setting-constant");
DEFSTRING(err_setting_constant, "Attempt to set value of constant");
DEFSYM(process_error, "process-error");
DEFSTRING(err_process_error, "Process error");
DEFSYM(no_memory, "no-memory");
DEFSTRING(err_no_memory, "No free memory");
DEFSYM(user_interrupt, "user-interrupt");
DEFSTRING(err_user_interrupt, "User interrupt!");
DEFSYM(arith_error, "arith-error");
DEFSTRING(err_arith_error, "Arithmetic error");
DEFSYM(term_interrupt, "term-interrupt");

DEFSYM(debug_error_entry, "*debug-error-entry*");
DEFSYM(debug_on_error, "*debug-on-error*");
DEFSYM(backtrace_on_error, "*backtrace-on-error*");
DEFSYM(error_handler_function, "*error-handler-function*");

/* ::doc:*debug-on-error*::
When an error is signalled this variable controls whether or not to
enter the Lisp debugger immediately. If the variable's value is t or a
list of symbols--one of which is the signalled error symbol--the
debugger is entered.
::end::
::doc:*backtrace-on-error*::
When an error is signalled this variable controls whether or not to
print a backtrace immediately. If the variable's value is t or a list
of symbols--one of which is the signalled error symbol--the debugger is
entered.
::end::
::doc:*debug-macros*::
When nil, the debugger isn't entered while expanding macro definitions.
::end::
::doc:*error-handler-function*::
When set to a function value, called with two arguments(error type
and data) when lisp errors occur.
::end:: */

static bool
error_matches_var(repv error, repv var)
{
  repv value = Fsymbol_value(var, Qt);

  if (value == Qt && error != Qend_of_stream) {
    return true;
  }

  if (!rep_CONSP(value)) {
    return false;
  }

  repv tem = Fmemq(error, value);

  return tem && tem != rep_nil;
}

DEFUN("call-with-exception-handler", Fcall_with_exception_handler,
      Scall_with_exception_handler, (repv thunk, repv handler),
      rep_Subr2) /*
::doc:rep.lang.interpreter#call-with-exception-handler::
call-with-exception-handler THUNK HANDLER

Call THUNK and return its value. However if an exception of any form
occurs, call HANDLER with a single argument, the exception data, and
return its value.
::end:: */
{
  rep_DECLARE(1, thunk, Ffunctionp(thunk) != rep_nil);
  rep_DECLARE(2, handler, Ffunctionp(handler) != rep_nil);

  rep_GC_root gc_handler;
  rep_PUSHGC(gc_handler, handler);

  repv ret = rep_call_lisp0(thunk);

  rep_POPGC;

  if (!ret) {
    repv data = rep_throw_value;
    rep_throw_value = 0;
    assert(data != 0);
    ret = rep_call_lisp1(handler, data);
  }

  return ret;
}

DEFUN("raise-exception", Fraise_exception,
      Sraise_exception, (repv ex), rep_Subr1) /*
::doc:rep.lang.interpreter#raise-exception::
raise-exception DATA

Raise the exception represented by the cons cell DATA.
::end:: */
{
  rep_DECLARE1(ex, rep_CONSP);

  /* Only one thing can use `rep_throw_value' at once.  */
  if (!rep_throw_value) {
    rep_throw_value = ex;
  }

  return 0;
}

DEFUN("signal", Fsignal, Ssignal, (repv error, repv data), rep_Subr2) /*
::doc:rep.lang.interpreter#signal::
signal ERROR-SYMBOL DATA

Signal that an error has happened. ERROR-SYMBOL is the name of a symbol
classifying the type of error, it should have a property `error-message'
(a string) with a short description of the error message.
DATA is a list of objects which are relevant to the error -- they will
be made available to any error-handler or printed by the default error
handler.
::end:: */
{
  /* Can only have one error at once.	 */
  if (rep_throw_value) {
    return 0;
  }

  rep_DECLARE1(error, rep_SYMBOLP);

  if (error_matches_var(error, Qbacktrace_on_error)) {
    fprintf(stderr, "\nLisp backtrace:\n");
    Fbacktrace(Fstderr_file());
    fputs("\n", stderr);
  }	

  repv errlist = Fcons(error, data);

  if (error_matches_var(error, Qdebug_on_error)) {
    repv old_value = Fsymbol_value(Qdebug_on_error, Qt);
    rep_GC_root gc_old_value;
    rep_PUSHGC(gc_old_value, old_value);

    bool old_flag = rep_single_step_flag;
    rep_single_step_flag = false;

    Fset(Qdebug_on_error, rep_nil);

    repv ret = rep_apply(Fsymbol_value(Qdebug_error_entry, Qt),
      rep_list_2(errlist, rep_MAKE_INT(rep_current_frame_index())));

    Fset(Qdebug_on_error, old_value);

    if (ret && (ret == Qt)) {
      rep_single_step_flag = true;
    } else {
      rep_single_step_flag = old_flag;
    }

    rep_POPGC;
  }

  rep_throw_value = Fcons(Qerror, errlist);
  return 0;
}

/* For an error ERROR (the cdr of rep_throw_value), if it matches the
   error handler HANDLER (the car of the handler list), return true. */

bool
rep_compare_error(repv data, repv handler)
{
  if (rep_CONSP(data)) {
    repv error = rep_CAR(data);

    if (rep_SYMBOLP(handler) && (error == handler || handler == Qerror)) {
      return true;
    } else if (rep_CONSP(handler)) {
      handler = Fmemq(error, handler);
      return handler != 0 && !rep_NILP(handler);
    }
  }

  return false;
}

void
rep_handle_error(repv error, repv data)
{
  DEFSTRING(some_error, "some kind of error occurred");

  static int mutex;

  if (mutex++ == 0) {
    repv fun = Fsymbol_value(Qerror_handler_function, Qt);
    if (Ffunctionp(fun) != rep_nil) {
      rep_call_lisp2(fun, error, data);
      goto out;
    }
  }

  Fbeep();
  Fwrite(Qt, rep_VAL(&some_error), rep_nil);

out:
  mutex--;
}

repv
rep_signal_arg_error(repv obj, int argNum)
{
  return Fsignal(Qbad_arg, rep_list_3(rep_current_function(), obj,
				      rep_MAKE_INT(argNum)));
}

repv
rep_signal_missing_arg(int argnum)
{
  return Fsignal(Qmissing_arg,
		 rep_list_2(rep_current_function(), rep_MAKE_INT(argnum)));
}

/* We normally don't check malloc results, but for failing allocations
   where size was defined by user input, we try to throw this error. */

repv
rep_mem_error(void)
{
  return Fsignal(Qno_memory, rep_nil);
}

void
rep_test_interrupt(void)
{
  rep_test_int_counter = 0;

  if (rep_test_int_fun) {
    (*rep_test_int_fun)();
  }
}

void
rep_errors_init(void)
{
  repv tem;

  rep_mark_static((repv *)&rep_throw_value);

  tem = rep_push_structure("rep.lang.interpreter");
  rep_ADD_SUBR(Scall_with_exception_handler);
  rep_ADD_SUBR(Sraise_exception);
  rep_ADD_SUBR(Ssignal);
  rep_pop_structure(tem);

  rep_INTERN(error_message);

  rep_INTERN(error);
  rep_DEFINE_ERROR(error);
  rep_INTERN(invalid_function);
  rep_DEFINE_ERROR(invalid_function);
  rep_INTERN(void_value);
  rep_DEFINE_ERROR(void_value);
  rep_INTERN(bad_arg);
  rep_DEFINE_ERROR(bad_arg);
  rep_INTERN(invalid_read_syntax);
  rep_DEFINE_ERROR(invalid_read_syntax);
  rep_INTERN(end_of_stream);
  rep_DEFINE_ERROR(end_of_stream);
  rep_INTERN(premature_end_of_stream);
  rep_DEFINE_ERROR(premature_end_of_stream);
  rep_INTERN(invalid_lambda_list);
  rep_DEFINE_ERROR(invalid_lambda_list);
  rep_INTERN(missing_arg);
  rep_DEFINE_ERROR(missing_arg);
  rep_INTERN(invalid_macro);
  rep_DEFINE_ERROR(invalid_macro);
  rep_INTERN(no_catcher);
  rep_DEFINE_ERROR(no_catcher);
  rep_INTERN(file_error);
  rep_DEFINE_ERROR(file_error);
  rep_INTERN(invalid_stream);
  rep_DEFINE_ERROR(invalid_stream);
  rep_INTERN(setting_constant);
  rep_DEFINE_ERROR(setting_constant);
  rep_INTERN(process_error);
  rep_DEFINE_ERROR(process_error);
  rep_INTERN(no_memory);
  rep_DEFINE_ERROR(no_memory);
  rep_INTERN(user_interrupt);
  rep_DEFINE_ERROR(user_interrupt);
  rep_INTERN(arith_error);
  rep_DEFINE_ERROR(arith_error);

  rep_INTERN(term_interrupt);
  rep_INTERN_SPECIAL(error_handler_function);
  rep_INTERN_SPECIAL(debug_error_entry);

  rep_INTERN_SPECIAL(debug_on_error);
  Fset(Qdebug_on_error, rep_nil);

  rep_INTERN_SPECIAL(backtrace_on_error);
  Fset(Qbacktrace_on_error, rep_nil);

  rep_int_cell = Fcons(Quser_interrupt, rep_nil);
  rep_mark_static(&rep_int_cell);
  rep_term_cell = Fcons(Qterm_interrupt, rep_nil);
  rep_mark_static(&rep_term_cell);
}
