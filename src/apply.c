/* apply.c -- function application

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

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

/* The lisp-call backtrace; also used for saving and restoring
   the current environment */

rep_stack_frame *rep_call_stack;

DEFSYM(ellipsis, "...");
DEFSTRING(max_depth, "max-lisp-depth exceeded, possible infinite recursion?");

static void
copy_to_vector(repv argList, int nargs, repv *args)
{
  for (int i = 0; i < nargs; i++) {
    args[i] = rep_CAR(argList);
    argList = rep_CDR(argList);
  }
}

repv
rep_apply_(repv fun, repv arglist, bool tail_posn)
{
  rep_TEST_INT;
  if (rep_INTERRUPTP) {
    return 0;
  }

  if (++rep_lisp_depth > rep_max_lisp_depth) {
    rep_lisp_depth--;
    return Fsignal(Qerror, rep_LIST_1(rep_VAL(&max_depth)));
  }

  /* Putting `fun` and `arglist` in the call stack mean they're
     protected from GC. */

  rep_stack_frame lc;
  lc.fun = fun;
  lc.args = arglist;
  rep_PUSH_CALL(lc);

  if (rep_data_after_gc >= rep_gc_threshold) {
    Fgarbage_collect(rep_nil);
  }

again:;
  repv result = 0;
  bool closure = rep_CLOSUREP(fun);

  if (closure) {
    rep_USE_CLOSURE(fun);
    fun = rep_CLOSURE(fun)->fun;
  }

  int type = rep_TYPE(fun);
  switch (type) {
    int nargs;
    repv argv[5];

  case rep_SubrN:
    if (!rep_SUBR_VEC_P(fun)) {
      result = rep_SUBRNFUN(fun)(arglist);
    } else {
      int length = rep_list_length(arglist);
      if (length < 0) {
	result = 0;
	break;
      }
      repv *vec = rep_stack_alloc(repv, length);
      if (!vec) {
	result = rep_mem_error();
	break;
      }
      copy_to_vector(arglist, length, vec);
      result = rep_SUBRVFUN(fun) (length, vec);
      rep_stack_free(repv, length, vec);
    }
    break;

  case rep_Subr0:
    result = rep_SUBR0FUN(fun)();
    break;

  case rep_Subr1:
    nargs = 1;
    goto call_subr;

  case rep_Subr2:
    nargs = 2;
    goto call_subr;

  case rep_Subr3:
    nargs = 3;
    goto call_subr;

  case rep_Subr4:
    nargs = 4;
    goto call_subr;

  case rep_Subr5:
    nargs = 5;
    // fall through

  call_subr:
    for (int i = 0; i < nargs; i++) {
      if (rep_CONSP(arglist)) {
	argv[i] = rep_CAR(arglist);
	arglist = rep_CDR(arglist);
      } else {
	argv[i] = rep_nil;
      }
    }
    switch (type) {
    case rep_Subr1:
      result = rep_SUBR1FUN(fun)(argv[0]);
      break;
    case rep_Subr2:
      result = rep_SUBR2FUN(fun)(argv[0], argv[1]);
      break;
    case rep_Subr3:
      result = rep_SUBR3FUN(fun)(argv[0], argv[1], argv[2]);
      break;
    case rep_Subr4:
      result = rep_SUBR4FUN(fun)(argv[0], argv[1], argv[2], argv[3]);
      break;
    case rep_Subr5:
      result = rep_SUBR5FUN(fun)(argv[0], argv[1], argv[2], argv[3], argv[4]);
      break;
    }
    break;

  case rep_Cons: {
    if (!closure) {
      goto invalid;
    }
    if (rep_CAR(fun) == Qlambda) {
      result = rep_apply_lambda(fun, arglist, tail_posn);
    } else if (rep_CAR(fun) == Qautoload) {
      fun = rep_load_autoload(lc.fun);
      if (fun) {
	lc.fun = fun;
	goto again;
      }
    } else {
      goto invalid;
    }
    break; }

  case rep_Bytecode: {
    int nargs = rep_list_length(arglist);
    if (nargs < 0) {
      result = 0;
      break;
    }
    repv *args = rep_stack_alloc(repv, nargs);
    if (!args) {
      result = rep_mem_error();
      break;
    }
    copy_to_vector(arglist, nargs, args);
    repv (*bc_apply)(repv, int, repv *) =
      rep_STRUCTURE(rep_structure)->apply_bytecode;
    if (!bc_apply) {
      result = rep_apply_bytecode(fun, nargs, args);
    } else {
      result = bc_apply(fun, nargs, args);
    }
    rep_stack_free(repv, nargs, args);
    break; }

  default:
  invalid:
    Fsignal(Qinvalid_function, rep_LIST_1(lc.fun));
  }

  /* In case I missed a non-local exit somewhere.  */

  if (!result == !rep_throw_value) {
    fprintf(stderr,
	    "rep: function returned both exception and value, or neither!\n");
    if (lc.fun && Fsubrp(lc.fun) != rep_nil
	&& rep_STRINGP(rep_XSUBR(lc.fun)->name))
    {
      fprintf(stderr, "rep: culprit is subr %s\n",
	      rep_STR(rep_XSUBR(lc.fun)->name));
    }
    result = rep_throw_value ? 0 : rep_void_value;
  }

  rep_POP_CALL(lc);

  rep_lisp_depth--;
  return result;
}

repv
rep_apply(repv fun, repv args)
{
  return rep_apply_(fun, args, false);
}

repv
rep_call_lispn(repv fun, int argc, repv *argv)
{
  if (rep_CLOSUREP(fun) && rep_BYTECODEP(rep_CLOSURE(fun)->fun)) {

    /* Call to bytecode, avoid consing argument list */

    rep_stack_frame lc;
    lc.fun = fun;
    lc.args = rep_void_value;
    rep_PUSH_CALL(lc);
    rep_USE_CLOSURE(fun);

    repv(*bc_apply) (repv, int, repv *);
    bc_apply = rep_STRUCTURE(rep_structure)->apply_bytecode;

    repv ret;
    if (!bc_apply) {
      ret = rep_apply_bytecode(rep_CLOSURE(fun)->fun, argc, argv);
    } else {
      ret = bc_apply(rep_CLOSURE(fun)->fun, argc, argv);
    }

    rep_POP_CALL(lc);
    return ret;

  } else {

    /* Normal call, cons up arg list and apply(). */

    repv args = rep_nil;
    argv += argc;
    while (argc-- > 0) {
      args = Fcons(*(--argv), args);
    }
    return rep_apply(fun, args);
  }
}

repv
rep_call_lisp0(repv function)
{
  return rep_call_lispn(function, 0, 0);
}

repv
rep_call_lisp1(repv function, repv arg1)
{
  return rep_call_lispn(function, 1, &arg1);
}

repv
rep_call_lisp2(repv function, repv arg1, repv arg2)
{
  repv vec[2];
  vec[0] = arg1;
  vec[1] = arg2;
  return rep_call_lispn(function, 2, vec);
}

repv
rep_call_lisp3(repv function, repv arg1, repv arg2, repv arg3)
{
  repv vec[3];
  vec[0] = arg1;
  vec[1] = arg2;
  vec[2] = arg3;
  return rep_call_lispn(function, 3, vec);
}

repv
rep_call_lisp4(repv function, repv arg1, repv arg2, repv arg3, repv arg4)
{
  repv vec[4];
  vec[0] = arg1;
  vec[1] = arg2;
  vec[2] = arg3;
  vec[3] = arg4;
  return rep_call_lispn(function, 4, vec);
}

DEFUN("lambda", Flambda, Slambda, (repv args, bool tail_posn), rep_SF) /*
::doc:rep.lang.interpreter#lambda::
lambda LAMBDA-LIST BODY...

Evaluates to an anonymous function.
::end:: */
{
  if (!rep_CONSP(args)) {
    return rep_signal_missing_arg(1);
  }
  return Fmake_closure(Fcons(Qlambda, args), rep_nil);
}

DEFUN("funcall", Ffuncall, Sfuncall, (repv args), rep_SubrN) /*
::doc:rep.lang.interpreter#funcall::
funcall FUNCTION ARGS...

Calls FUNCTION with arguments ARGS... and returns the result.
::end:: */
{
  if (!rep_CONSP(args)) {
    return rep_signal_missing_arg(1);
  }

  return rep_apply(rep_CAR(args), rep_CDR(args));
}

DEFUN("functionp", Ffunctionp, Sfunctionp, (repv arg), rep_Subr1) /*
::doc:rep.lang.interpreter#functionp::
functionp ARG

Returns t if ARG is a function.
::end:: */
{
  switch(rep_TYPE(arg)) {
  case rep_Subr0:
  case rep_Subr1:
  case rep_Subr2:
  case rep_Subr3:
  case rep_Subr4:
  case rep_Subr5:
  case rep_SubrN:
  case rep_Closure:
    return Qt;

  case rep_Cons:
    arg = rep_CAR(arg);
    if(arg == Qautoload) {
      return Qt;
    }
    /* fall through */

  default:
    return rep_nil;
  }
}

repv
rep_current_function(void)
{
  return rep_call_stack != 0 ? rep_call_stack->fun : rep_nil;
}

int
rep_current_frame_index(void)
{
  int i = 0;
  for (const rep_stack_frame *lc = rep_call_stack; lc != 0; lc = lc->next) {
    i++;
  }

  return i - 1;
}

static const rep_stack_frame *
stack_frame_ref(int idx)
{
  int total = 0;
  for (const rep_stack_frame *lc = rep_call_stack; lc != 0; lc = lc->next) {
    total++;
  }

  int wanted = (total - 1) - idx;
  if (wanted < 0) {
    return 0;
  }

  for (const rep_stack_frame *lc = rep_call_stack; lc != 0; lc = lc->next) {
    if (wanted-- == 0) {
      return lc;
    }
  }

  return NULL;
}

DEFUN("backtrace", Fbacktrace, Sbacktrace, (repv strm), rep_Subr1) /*
::doc:rep.lang.debug#backtrace::
backtrace [STREAM]

Prints a backtrace of the current Lisp call stack to STREAM(or to
`*standard-output*'). The format is something like:

  FRAME-ID FUNCTION (ARGLIST) [at FILE:LINE]

where ARGS-EVALLED-P is either `t' or `nil', depending on whether or not
ARGLIST had been evaluated or not before being put into the stack.
::end:: */
{
  if (strm == rep_nil && !(strm = Fsymbol_value(Qstandard_output, rep_nil))) {
    return rep_signal_arg_error(strm, 1);
  }

  repv old_print_escape = Fsymbol_value(Qprint_escape, Qt);
  Fset(Qprint_escape, Qt);

  int total_frames = rep_current_frame_index() + 1;

  for (int i = total_frames - 1; i >= 0; i--) {
    const rep_stack_frame *lc = stack_frame_ref(i);
    if (!lc) {
      continue;
    }

    repv function_name = rep_nil;
    if (rep_CLOSUREP(lc->fun)) {
      if (rep_STRINGP(rep_CLOSURE(lc->fun)->name)) {
	function_name = rep_CLOSURE(lc->fun)->name;
      }
    } else if (Fsubrp(lc->fun) != rep_nil) {
      if (rep_STRINGP(rep_XSUBR(lc->fun)->name)) {
	function_name = rep_XSUBR(lc->fun)->name;
      }
    } else if (rep_CONSP(lc->fun) && rep_CAR(lc->fun) == Qlambda
	       && rep_CONSP(rep_CDR(lc->fun))) {
      function_name = rep_list_3(Qlambda, rep_CADR(lc->fun), Qellipsis);
    }

    if (function_name != rep_nil) {
      char buf[32];
      sprintf(buf, "#%-3d ", i);
      rep_stream_puts(strm, buf, -1, false);

      rep_princ_val(strm, function_name);

      if (rep_VOIDP(lc->args)
	  || (rep_STRINGP(function_name)
	      && strcmp(rep_STR(function_name), "run-byte-code") == 0))
      {
	rep_stream_puts(strm, " ...", -1, false);
      } else {
	rep_stream_putc(strm, ' ');
	rep_print_val(strm, lc->args);
      }

      if (lc->current_form) {
	repv origin = Flexical_origin(lc->current_form);
	if (origin && origin != rep_nil) {
	  char buf[256];
#ifdef HAVE_SNPRINTF
	  snprintf(buf, sizeof(buf), " at %s:%d", rep_STR(rep_CAR(origin)),
		   (int) rep_INT(rep_CDR(origin)));
#else
	  sprintf(buf, " at %s:%d", rep_STR(rep_CAR(origin)),
		  (int) rep_INT(rep_CDR(origin)));
#endif
	  rep_stream_puts(strm, buf, -1, false);
	}
      }

      rep_stream_putc(strm, '\n');
    }
  }

  Fset(Qprint_escape, old_print_escape);
  return Qt;
}

DEFUN("stack-frame-ref", Fstack_frame_ref,
       Sstack_frame_ref, (repv idx), rep_Subr1)
{
  rep_DECLARE1(idx, rep_INTP);

  const rep_stack_frame *lc = stack_frame_ref(rep_INT(idx));

  if (!lc) {
    return rep_nil;
  }

  return rep_list_5(lc->fun, rep_VOIDP(lc->args)
		    ? rep_undefined_value : lc->args,
		    lc->current_form ? lc->current_form : rep_nil,
		    lc->saved_env, lc->saved_structure);
}

DEFUN("max-lisp-depth", Fmax_lisp_depth,
      Smax_lisp_depth, (repv val), rep_Subr1) /*
::doc:rep.lang.interpreter#max-lisp-depth::
max-lisp-depth [NEW-VALUE]

The maximum number of times that apply can be called recursively.

This is intended to stop infinite recursion, if the default value of 250 is
too small(you get errors in normal use) set it to something larger.
::end:: */
{
  return rep_handle_var_int(val, &rep_max_lisp_depth);
}

/* Bind one object, returning the handle to later unbind by. */

static repv
bind_object(repv obj)
{
  rep_type *t = rep_get_data_type(rep_TYPE(obj));
  return t->bind ? t->bind(obj) : rep_nil;
}

static void
unbind_object(repv handle)
{
  if (handle == rep_nil) {
    return;
  }

  repv obj = rep_CONSP(handle) ? rep_CAR(handle) : handle;
  rep_type *t = rep_get_data_type(rep_TYPE(obj));

  if (t->unbind != 0) {
    t->unbind(handle);
  }
}

DEFUN("call-with-object", Fcall_with_object,
      Scall_with_object, (repv arg, repv thunk), rep_Subr2) /*
::doc:rep.lang.interpreter#call-with-object::
call-with-object ARG THUNK

Call the zero-parameter function THUNK, with object ARG temporarily
`bound' (a type-specific operation, usually to make ARG `active' in
some way). When THUNK returns ARG is unbound. The value returned by
THUNK is then returned.
::end:: */
{
  repv data[2];
  data[0] = arg;
  data[1] = bind_object(data[0]);
  if (!data[1]) {
    return 0;
  }

  rep_GC_n_roots gc_data;
  rep_PUSHGCN(gc_data, data, 2);
  repv ret = rep_call_lisp0(thunk);
  rep_POPGCN;

  unbind_object(data[1]);
  return ret;
}

DEFUN("subrp", Fsubrp, Ssubrp, (repv arg), rep_Subr1) /*
::doc:rep.lang.interpreter#subrp::
subrp ARG

Returns t if arg is a primitive function.
::end:: */
{
  switch (rep_TYPE(arg)) {
  case rep_Subr0:
  case rep_Subr1:
  case rep_Subr2:
  case rep_Subr3:
  case rep_Subr4:
  case rep_Subr5:
  case rep_SubrN:
  case rep_SF:
    return Qt;
  default:
    return rep_nil;
  }
}

DEFUN("subr-name", Fsubr_name, Ssubr_name, (repv subr, repv useVar), rep_Subr2) /*
::doc:rep.lang.interpreter#subr-name::
subr-name SUBR [USE-VAR]

Returns the name (a string) associated with SUBR.
::end:: */
{
  switch(rep_TYPE(subr)) {
  case rep_Subr0:
  case rep_Subr1:
  case rep_Subr2:
  case rep_Subr3:
  case rep_Subr4:
  case rep_Subr5:
  case rep_SubrN:
  case rep_SF:
    return rep_SUBR(subr)->name;
  default:
    return rep_nil;
  }
}

void
rep_apply_init(void)
{
  repv tem;

  tem = rep_push_structure("rep.lang.interpreter");
  rep_ADD_SUBR(Slambda);
  rep_ADD_SUBR(Sfuncall);
  rep_ADD_SUBR(Sfunctionp);
  rep_ADD_SUBR(Smax_lisp_depth);
  rep_ADD_SUBR(Scall_with_object);
  rep_ADD_SUBR(Ssubrp);
  rep_ADD_SUBR(Ssubr_name);
  rep_pop_structure(tem);

  tem = rep_push_structure("rep.lang.debug");
  rep_ADD_SUBR(Sbacktrace);
  rep_ADD_SUBR(Sstack_frame_ref);
  rep_pop_structure(tem);

  rep_INTERN(ellipsis);
}
