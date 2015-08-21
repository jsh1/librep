/* eval.c -- form evaluation

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

/* When true Feval() calls the "debug-entry" function */

bool rep_single_step_flag;

/* Prevent infinite recursion */

int rep_lisp_depth, rep_max_lisp_depth = 1000;

DEFSYM(debug_macros, "debug-macros");
DEFSYM(debug_entry, "debug-entry");
DEFSYM(debug_exit, "debug-exit");

DEFUN("apply", Fapply, Sapply, (repv args), rep_SubrN) /*
::doc:rep.lang.interpreter#apply::
apply FUNCTION ARGS... ARG-LIST

Calls FUNCTION passing all of ARGS to it as well as all elements in
ARG-LIST. For example:

  (apply + 1 2 3 '(4 5 6))
   => 21
::end:: */
{
  repv list = rep_nil;
  repv *last = &list;

  if (!rep_CONSP(args)) {
    return rep_signal_missing_arg(1);
  }

  while (rep_CONSP(rep_CDR(args))) {
    repv cell = Fcons(rep_CAR(args), rep_nil);
    *last = cell;
    last = rep_CDRLOC(cell);
    args = rep_CDR(args);
    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      return 0;
    }
  }

  if (rep_LISTP(rep_CAR(args))) {
    *last = rep_CAR(args);
  } else {
    return rep_signal_arg_error(rep_CAR(args), -1);
  }

  return rep_apply(rep_CAR(list), rep_CDR(list));
}

static repv
eval_list(repv list)
{
  repv result = rep_nil;
  repv *last = &result;

  rep_GC_root gc_result, gc_list;
  rep_PUSHGC(gc_result, result);
  rep_PUSHGC(gc_list, list);

  while (rep_CONSP(list)) {
    repv tmp = rep_eval(rep_CAR(list), false);

    if (!tmp) {
      result = 0;
      break;
    }

    repv cell = Fcons(tmp, rep_nil);
    *last = cell;
    last = rep_CDRLOC(cell);

    list = rep_CDR(list);

    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      result = 0;
      break;
    }
  }

  if (result && last && !rep_NILP(list)) {
    *last = rep_eval(list, false);
  }

  rep_POPGC; rep_POPGC;
  return result;
}

static void
copy_to_vector(repv argList, int nargs, repv *args)
{
  for (int i = 0; i < nargs; i++) {
    args[i] = rep_CAR(argList);
    argList = rep_CDR(argList);
  }
}

DEFSTRING(max_depth, "max-lisp-depth exceeded, possible infinite recursion?");

static repv
inner_eval(repv obj, bool tail_posn)
{
  if (rep_SYMBOLP(obj) && !rep_SYMBOL_KEYWORD_P(obj)) {
    return rep_symbol_value(obj, false, true);
  } else if (!rep_CONSP(obj)) {
    return obj;
  }

  if (++rep_lisp_depth > rep_max_lisp_depth) {
    rep_lisp_depth--;
    return Fsignal(Qerror, rep_LIST_1(rep_VAL(&max_depth)));
  }

  if (rep_CONSP(rep_CAR(obj)) && rep_CAAR(obj) == Qlambda
      && Fsymbol_value(Qlambda, Qt) == rep_VAL(&Slambda))
  {
    /* Inline lambda application; don't need to enclose it. */

    rep_stack_frame lc;

    lc.fun = rep_CAR(obj);
    lc.args = rep_CDR(obj);
    rep_PUSH_CALL(lc);

    lc.args = eval_list(lc.args);

    repv ret = 0;
    if (lc.args) {
      ret = rep_apply_lambda(lc.fun, lc.args, tail_posn);
    }

    rep_POP_CALL(lc);
    rep_lisp_depth--;
    return ret;
  }

  rep_GC_root gc_obj;
  rep_PUSHGC(gc_obj, obj);
  repv fun = rep_eval(rep_CAR(obj), false);
  rep_POPGC;

  if (!fun) {
    rep_lisp_depth--;
    return 0;
  }

  repv ret = 0;

  if (rep_CELL8_TYPEP(fun, rep_SF)) {

    /* Special form. */

    ret = rep_SFFUN(fun)(rep_CDR(obj), tail_posn);

  } else if (rep_CONSP(fun) && rep_CAR(fun) == Qmacro) {

    /* Macro. */

    bool old_flag = rep_single_step_flag;
    if (old_flag && Fsymbol_value(Qdebug_macros, Qt) == rep_nil) {
      rep_single_step_flag = false;
    }

    repv form = Fmacroexpand(obj, rep_nil);

    rep_single_step_flag = old_flag;

    if (form) {
      ret = rep_eval(form, tail_posn);
    }

  } else if (tail_posn && (rep_CLOSUREP(fun) || fun == rep_VAL(&Sapply))) {
    /* Wrappable tail-call. */

    rep_PUSHGC(gc_obj, fun);
    repv args = eval_list(rep_CDR(obj));
    rep_POPGC;

    if (args) {
      if (fun == rep_VAL(&Sapply)) {
	if (!rep_CONSP(args)) {
	  rep_signal_missing_arg(1);
	  goto out;
	} else {
	  int len = rep_list_length(rep_CDR(args));
	  repv *vec = rep_stack_alloc(repv, len);
	  if (!vec) {
	    rep_mem_error();
	    goto out;
	  }
	  copy_to_vector(rep_CDR(args), len, vec);
	  rep_CDR(args) = Flist_star(len, vec);
	  rep_stack_free(repv, len, vec);
	}
      } else {
	args = Fcons(fun, args);
      }
      /* Throw the tail call back to whoever's listening. */
      rep_throw_value = rep_tail_call_throw(args);
    }

  } else {

    /* Normal function call. */

    rep_PUSHGC(gc_obj, fun);
    repv args = eval_list(rep_CDR(obj));
    rep_POPGC;

    rep_lisp_depth--;
      
    if (!args) {
      return 0;
    }

    return rep_apply_(fun, args, tail_posn);
  }

out:
  rep_lisp_depth--;
  return ret;
}

repv
rep_eval(repv obj, bool tail_posn)
{
  static int debug_depth;

  rep_TEST_INT;
  if (rep_INTERRUPTP) {
    return 0;
  }

  if (rep_data_after_gc >= rep_gc_threshold) {
    rep_GC_root gc_obj;
    rep_PUSHGC(gc_obj, obj);
    Fgarbage_collect(rep_nil);
    rep_POPGC;
  }

  if (!rep_single_step_flag) {
    return inner_eval(obj, tail_posn);
  }

  /* Debugger support. */

  repv result = 0;
  bool new_step_flag = true;

  debug_depth++;

  rep_single_step_flag = false;

  repv db_args = rep_list_3(obj, rep_MAKE_INT(debug_depth),
			   rep_MAKE_INT(rep_current_frame_index()));

  rep_GC_root gc_db_args;
  rep_PUSHGC(gc_db_args, db_args);

  struct rep_saved_regexp_data re_data;
  rep_push_regexp_data(&re_data);

  repv db_ret = rep_apply(Fsymbol_value(Qdebug_entry, Qt), db_args);

  rep_pop_regexp_data();

  if (db_ret != 0 && rep_CONSP(db_ret)) {
    int status = rep_INT(rep_CAR(db_ret));
    repv arg = rep_CDR(db_ret);

    switch (status) {
    case 1:
      /* Single step arg and continuation.  */
      rep_single_step_flag = true;
      result = inner_eval(arg, false);
      rep_single_step_flag = false;
      break;

    case 2:
      /* Eval arg and step continuation. */
      result = inner_eval(arg, false);
      break;

    case 3:
      /* Eval arg and continuation. */
      result = inner_eval(arg, false);
      new_step_flag = false;
      break;

    case 4:
      /* Result is arg, step continuation.  */
      result = arg;
      break;
    }

    if (result) {
      rep_push_regexp_data(&re_data);

      rep_CAR(db_args) = result;

      if (!rep_apply(Fsymbol_value(Qdebug_exit, Qt), db_args)) {
	result = 0;
      }

      rep_pop_regexp_data();
    }
  }

  rep_POPGC;
  debug_depth--;

  rep_single_step_flag = new_step_flag;
  return result;
}

repv
Feval(repv form)
{
  return rep_eval(form, false);
}

DEFUN("quote", Fquote, Squote, (repv args, bool tail_posn), rep_SF) /*
::doc:rep.lang.interpreter#quote::
quote ARG
'ARG

Returns ARG.
::end:: */
{
  if (!rep_CONSP(args)) {
    return rep_signal_missing_arg(1);
  }

  return rep_CAR(args);
}

DEFUN("progn", Fprogn, Sprogn, (repv args, bool tail_posn), rep_SF) /*
::doc:rep.lang.interpreter#progn::
progn FORMS...

Eval's each of the FORMS in order returning the value of the last.
::end:: */
{
  repv result = rep_nil;
  repv old_current = rep_call_stack ? rep_call_stack->current_form : 0;

  rep_GC_root gc_args, gc_old_current;
  rep_PUSHGC(gc_args, args);
  rep_PUSHGC(gc_old_current, old_current);

  bool last_form = !rep_CONSP(args);

  while (!last_form && result && !rep_INTERRUPTP) {
    repv form = rep_CAR(args);
    args = rep_CDR(args);
    last_form = !rep_CONSP(args);

    if (rep_call_stack) {
      rep_call_stack->current_form = form;
    }

    result = rep_eval(form, last_form ? tail_posn : false);

    rep_TEST_INT;
  }

  if (rep_call_stack) {
    rep_call_stack->current_form = old_current;
  }

  rep_POPGC; rep_POPGC;
  return result;
}

DEFUN("cond", Fcond, Scond, (repv args, bool tail_posn), rep_SF) /*
::doc:rep.lang.interpreter#cond::
cond (CONDITION FORMS... ) ...

Find the first CONDITION whose evaluated value is not nil, then
evaluate each of FORMS sequentially, returning the value of the last.
(If FORMS is an empty list then the value of the CONDITION is
returned.) If no CONDITION evaliates evaluates to a non-nil value then
return nil.

For example:

  (cond
    ((stringp foo)
      (title "foo is a string"))
    ((numberp foo)
      (setq bar foo)
      (title "foo is a number"))
    (t
      (title "foo is something else...")))
::end:: */
{
  repv ret = rep_nil;

  rep_GC_root gc_args;
  rep_PUSHGC(gc_args, args);

  while (rep_CONSP(args) && rep_CONSP(rep_CAR(args))) {
    repv lst = rep_CAR(args);
    repv pred = rep_CAR(lst);
    lst = rep_CDR(lst);
    /* This CONDITION is only guaranteed to be in tail position if
       it has no associated FORMS and no following condition. */
    bool cond_tail = tail_posn && !rep_CONSP(lst) && !rep_CONSP(rep_CDR(args));
    ret = rep_eval(pred, cond_tail);
    if (!ret) {
      break;
    }
    if (ret != rep_nil) {
      if (rep_CONSP(lst)) {
	ret = Fprogn(lst, tail_posn);
	if(!ret) {
	  break;
	}
      }
      break;
    }
    args = rep_CDR(args);
  }

  rep_POPGC;
  return ret;
}

DEFUN("macrop", Fmacrop, Smacrop, (repv arg), rep_Subr1) /*
::doc:rep.lang.interpreter#macrop::
macrop ARG

Returns t if ARG is a macro.
::end:: */
{
  return rep_CONSP(arg) && rep_CAR(arg) == Qmacro ? Qt : rep_nil;
}
	
DEFUN("special-form-p", Fspecial_form_p,
      Sspecial_form_p, (repv arg), rep_Subr1) /*
::doc:rep.lang.interpreter#special-form-p::
special-form-p ARG

Returns t if ARG is a special-form.
::end:: */
{
  return rep_TYPEP(arg, rep_SF) ? Qt : rep_nil;
}

DEFUN("break", Fbreak, Sbreak, (void), rep_Subr0) /*
::doc:rep.lang.debug#break::
break

The next form to be evaluated will be done so through the Lisp debugger.
::end:: */
{
  rep_single_step_flag = true;
  return Qt;
}

DEFUN_INT("step", Fstep, Sstep, (repv form),
	  rep_Subr1, "xForm to step through") /*
::doc:rep.lang.debug#step::
step FORM

Use the Lisp debugger to evaluate FORM.
::end:: */
{
  bool old_flag = rep_single_step_flag;
  rep_single_step_flag = true;

  repv ret = rep_eval(form, false);

  rep_single_step_flag = old_flag;

  return ret;
}

void
rep_eval_init(void)
{
  repv tem;

  tem = rep_push_structure("rep.lang.interpreter");
  rep_ADD_SUBR(Sapply);
  rep_ADD_SUBR(Squote);
  rep_ADD_SUBR(Sprogn);
  rep_ADD_SUBR(Scond);
  rep_ADD_SUBR(Smacrop);
  rep_ADD_SUBR(Sspecial_form_p);
  rep_pop_structure(tem);

  tem = rep_push_structure("rep.lang.debug");
  rep_ADD_SUBR(Sbreak);
  rep_ADD_SUBR_INT(Sstep);
  rep_pop_structure(tem);

  rep_INTERN_SPECIAL(debug_entry);
  rep_INTERN_SPECIAL(debug_exit);
  rep_INTERN_SPECIAL(debug_macros);

  Fset(Qdebug_macros, rep_nil);
}
