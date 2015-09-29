/* variables.c -- variable binding and environments

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

/* Lexical environment. A list of variable bindings, interpreted code
   calls functions in this file and stores each binding as (SYMBOL .
   VALUE). Compiled code manages the bindings directly, and only stores
   the values, knowing exactly how far down the current binding list
   each variable is stored. Lexical variables without bindings are
   dereferenced in the current structure.  */

repv rep_env;

/* Active special bindings, a list of (SYMBOL . VALUE) */

repv rep_special_env;

/* Function vectors to implement local symbols. */

repv (*rep_deref_local_symbol_fun)(repv sym) = 0;
repv (*rep_set_local_symbol_fun)(repv sym, repv val) = 0;

DEFSYM(documentation, "documentation");
DEFSYM(permanent_local, "permanent-local");

/* Returns (KEY . VALUE) if a binding, or nil */

static repv
search_environment(repv key, repv env_list)
{
  for (repv env = env_list; env != rep_nil; env = rep_CDR(env)) {
    if (rep_CAAR(env) == key) {
      return rep_CAR(env);
    }
  }

  return rep_nil;
}

repv
rep_search_special_environment(repv key)
{
  return search_environment(key, rep_special_env);
}

/* The "special environment" (of the current structure) is either `t`
   denoting that all special variables are accessible and that new
   variables may be created, or a list of symbols defining which
   variables may be accessed.

   Returns -1 if unrestricted, +1 if restricted but `sym` is
   accessible, or zero otherwise. */

static int
check_special_variable(repv sym)
{
  rep_TEST_INT_LOOP_COUNTER;

  repv env = rep_STRUCTURE(rep_structure)->special_variables;

  if (env == Qt) {
    return -1;
  }

  while (rep_CONSP(env)) {
    if (rep_CAR(env) == sym) {
      return 1;
    }

    env = rep_CDR(env);

    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      return 0;
    }
  }
  
  return 0;
}

bool
rep_special_variable_accessible_p(repv sym)
{
  return check_special_variable(sym) != 0;
}

repv
rep_bind_special(repv frame, repv symbol, repv value)
{
  if (!rep_special_variable_accessible_p(symbol)) {
    return Fsignal(Qvoid_value, rep_LIST_1(symbol));
  }

  rep_special_env = Fcons(Fcons(symbol, value), rep_special_env);
  return rep_MARK_SPEC_BINDING(frame);
}

/* This give SYMBOL a new value. Returns the new version of FRAME. */

repv
rep_bind_symbol(repv frame, repv symbol, repv value)
{
  if (rep_SYM(symbol)->car & rep_SF_SPECIAL) {
    frame = rep_bind_special(frame, symbol, value);
  } else {
    rep_env = Fcons(Fcons(symbol, value), rep_env);
    frame = rep_MARK_LEX_BINDING(frame);
  }

  return frame;
}

static repv
list_tail(repv list, int n)
{
  for (int i = 0; i < n; i++) {
    list = rep_CDR(list);
  }
  return list;
}

/* Undoes what the above function does. Returns the number of special
   bindings undone. */

int
rep_unbind_symbols(repv frame)
{
  if (!rep_INTP(frame) || frame == rep_EMPTY_BINDING_FRAME) {
    return 0;
  }

  rep_env = list_tail(rep_env, rep_LEX_BINDINGS(frame));
  rep_special_env = list_tail(rep_special_env, rep_SPEC_BINDINGS(frame));

  return rep_SPEC_BINDINGS(frame);
}

DEFUN("defvar", Fdefvar, Sdefvar, (repv args, bool tail_posn), rep_SF) /*
::doc:rep.lang.interpreter#defvar::
defvar NAME [DEFAULT-VALUE [DOC-STRING]]

Define a special variable called NAME whose standard value is DEFAULT-
VALUE. If NAME is already bound to a value (that's not an autoload
definition) it is left as it is, otherwise DEFAULT-VALUE is evaluated
and the special value of NAME is bound to the result.

If DOC-STRING is given, and is a string, it will be used to set the
`documentation' property of the symbol NAME.

(If the symbol NAME is marked buffer-local the default value of the
variable will be set (if necessary) not the local value.)
::end:: */
{
  if (!rep_CONSP(args)) {
    return rep_signal_missing_arg(1);
  }

  repv sym = rep_CAR(args);
  args = rep_CDR(args);

  rep_DECLARE1(sym, rep_SYMBOLP);

  repv bound = Fdefault_boundp(sym);
  if (!bound) {
    return 0;
  }

  repv value = rep_nil;
  repv doc = rep_nil;

  if (rep_CONSP(args)) {
    value = rep_CAR(args);
    args = rep_CDR(args);
    if (rep_CONSP(args)) {
      doc = rep_CAR(args);
      rep_DECLARE3_OPT(doc, rep_STRINGP);
    }
  }

  bool need_to_eval = true;

  if (bound != rep_nil) {
    /* Variable is bound, see if it's an autoload defn to overwrite. */

    repv val = Fsymbol_value(sym, Qt);
    if (rep_CLOSUREP(val)) {
      val = rep_CLOSURE(val)->fun;
      if (rep_CONSP(val) && rep_CAR(val) == Qautoload) {
	Fmakunbound(sym);
	bound = rep_nil;
      }
    }
  }

  /* Only allowed to defvar in restricted environments if the symbol
     hasn't yet been defvar'd or it's weak */

  int spec = check_special_variable(sym);

  if (spec == 0 && (rep_SYM(sym)->car & rep_SF_DEFVAR)
      && !(rep_SYM(sym)->car & rep_SF_WEAK))
  {
    return Fsignal(Qvoid_value, rep_LIST_1(sym));
  }

  /* If initially making it special, check for a lexical binding in the
     user module (i.e. bindings from rc files). */

  if (!(rep_SYM(sym)->car & rep_SF_SPECIAL)) {
    repv tem = rep_get_initial_special_value(sym);
    if (tem) {
      value = tem;
      need_to_eval = false;
      bound = rep_nil;
    }
  }

  /* Only set the [default] value if its not bound? or the definition
    is weak and we're currently unrestricted */

  if (bound == rep_nil
      || ((rep_SYM(sym)->car & rep_SF_WEAK)
	  && !(rep_SYM(sym)->car & rep_SF_WEAK_MOD)
	  && rep_STRUCTURE(rep_structure)->special_variables == Qt))
  {
    if (need_to_eval) {
      rep_GC_root gc_sym, gc_doc;
      rep_PUSHGC(gc_sym, sym);
      rep_PUSHGC(gc_doc, doc);

      value = Feval(value);

      rep_POPGC; rep_POPGC;
      if (!value) {
	return 0;
      }
    }

    Fstructure_define(rep_specials_structure, sym, value);
  }

  rep_SYM(sym)->car |= rep_SF_SPECIAL | rep_SF_DEFVAR;

  if (spec == 0) {

    /* Defvar'ing an undefvar'd variable from a restricted
       environment sets it as weak, and adds it to the env */

    rep_SYM(sym)->car |= rep_SF_WEAK;

    rep_STRUCTURE(rep_structure)->special_variables =
      Fcons(sym, rep_STRUCTURE(rep_structure)->special_variables);

  } else if ((rep_SYM(sym)->car & rep_SF_WEAK)
	     && rep_STRUCTURE(rep_structure)->special_variables == Qt)
  {

    /* Defvar'ing a weak variable from an unrestricted environment
       removes the weak status, but marks it as `was weak, but now
       strong'. This prevents exploits such as:

	   [restricted special environment]
	   (defvar special-var "/bin/rm")

	   [unrestricted environment]
	   (defvar special-var "ls")

	   [back in restricted environment]
	   (set! special-var "/bin/rm")
	     --> error

       Setting the variable the first time (since it's unbound) adds it
       to the restricted environment, but defvar'ing effectively removes
       it */

    rep_SYM(sym)->car &= ~rep_SF_WEAK;
    rep_SYM(sym)->car |= rep_SF_WEAK_MOD;
  }

  if (rep_STRINGP(doc)) {
    if (!Fput(sym, Qdocumentation, doc)) {
      return 0;
    }
  }

  return rep_undefined_value;
}

static repv
symbol_special_value(repv sym, bool only_default)
{
  /* Modified-weak specials can only be accessed from an unrestricted
     environment */
    
  int spec = check_special_variable(sym);

  if (!(spec < 0 || (spec > 0 && !(rep_SYM(sym)->car & rep_SF_WEAK_MOD)))) {
    return rep_void;
  }

  if (!only_default) {
    if (rep_SYM(sym)->car & rep_SF_LOCAL) {
      repv val = (*rep_deref_local_symbol_fun)(sym);
      if (val != rep_void) {
	return val;
      }
    }

    repv tem = search_environment(sym, rep_special_env);
    if (tem != rep_nil) {
      return rep_CDR(tem);
    }
  }

  return Fstructure_ref(rep_specials_structure, sym);
}

static repv
set_symbol_special_value(repv sym, repv val, bool only_default)
{
  /* Not allowed to set `modified' variables unless our environment
     includes all variables implicitly */

  int spec = check_special_variable(sym);

  if (spec == 0) {
    return Fsignal(Qvoid_value, rep_LIST_1(sym));
  } else if (spec > 0 && rep_SYM(sym)->car & rep_SF_WEAK_MOD) {
    return Fsignal(Qvoid_value, rep_LIST_1(sym));
  }

  if (!only_default) {
    if (rep_SYM(sym)->car & rep_SF_LOCAL) {
      repv tem = (*rep_set_local_symbol_fun)(sym, val);
      if (tem) {
	return rep_undefined_value;
      }
    }

    repv tem = search_environment(sym, rep_special_env);
    if (tem != rep_nil) {
      rep_CDR(tem) = val;
      return rep_undefined_value;
    }
  }

  return Fstructure_define(rep_specials_structure, sym, val);
}

static repv
inner_symbol_value(repv sym, bool allow_lexical)
{
  if (rep_SYM(sym)->car & rep_SF_SPECIAL) {
    return symbol_special_value(sym, false);
  }

  /* Don't scan lexical bindings unless being called from eval. */

  if (allow_lexical) {
    repv tem = search_environment(sym, rep_env);
    if (tem != rep_nil) {
      return rep_CDR(tem);
    }
  }

  return Fstructure_ref(rep_structure, sym);
}

repv
rep_symbol_value(repv sym, bool no_error_if_void, bool allow_lexical)
{
  repv val = inner_symbol_value(sym, allow_lexical);

  if (!no_error_if_void && rep_VOIDP(val)) {
    return Fsignal(Qvoid_value, rep_LIST_1(sym));
  } else if (rep_SYM(sym)->car & rep_SF_DEBUG) {
    rep_single_step_flag = true;
  }

  return val;
}

/* Second arg true means don't signal an error if the value is void. */

DEFUN("variable-ref", Fsymbol_value, Ssymbol_value,
      (repv sym, repv no_err), rep_Subr1) /*
::doc:rep.lang.symbols#variable-ref::
variable-ref SYMBOL

Returns the value of the top-level or special binding of SYMBOL, if
SYMBOL is flagged as having buffer-local values look for one of those
first.
::end:: */
{
  rep_DECLARE1(sym, rep_SYMBOLP);

  return rep_symbol_value(sym, no_err != rep_nil, false);
}

DEFUN("variable-ref-default", Fdefault_value, Sdefault_value,
      (repv sym, repv no_err), rep_Subr2) /*
::doc:rep.lang.symbols#variable-ref-default::
variable-ref-default SYMBOL

Returns the default value of the top-level or special binding of
SYMBOL. This will be the value of SYMBOL in buffers or windows which do
not have their own local value.
::end:: */
{
  rep_DECLARE1(sym, rep_SYMBOLP);

  repv val = rep_void;

  if (rep_SYM(sym)->car & rep_SF_SPECIAL) {
    val = symbol_special_value(sym, true);
  } else {
    val = Fstructure_ref(rep_structure, sym);
  }

  if (no_err == rep_nil && rep_VOIDP(val)) {
    return Fsignal(Qvoid_value, rep_LIST_1(sym));
  }

  return val;
}

/* Backwards compatibility for C callers. */

repv
Fset(repv sym, repv val)
{
  rep_DECLARE1(sym, rep_SYMBOLP);

  if (rep_SYM(sym)->car & rep_SF_SPECIAL) {
    return set_symbol_special_value(sym, val, false);
  } else {
    return Fstructure_define(rep_structure, sym, val);
  }
}

DEFUN_INT("variable-set!", Freal_set, Sset, (repv sym, repv val), rep_Subr2,
	  "vVariable:\nxNew value of %s:") /*
::doc:rep.lang.symbols#variable-set!::
variable-set! SYMBOL VALUE

Sets the value of the top-level or special binding of SYMBOL to VALUE.

If SYMBOL has a buffer-local binding in the current buffer or
`make-variable-buffer-local' has been called on SYMBOL the buffer-local
value in the current buffer is set.
::end:: */
{
  rep_DECLARE1(sym, rep_SYMBOLP);

  if (rep_SYM(sym)->car & rep_SF_SPECIAL) {
    return set_symbol_special_value(sym, val, false);
  } else {
    return Fstructure_set(rep_structure, sym, val);
  }
}

DEFUN("variable-set-default!", Fset_default, Sset_default,
      (repv sym, repv val), rep_Subr2) /*
::doc:rep.lang.symbols#variable-set-default!::
variable-set-default! SYMBOL VALUE

Sets the default value of SYMBOL's top-level or special binding to
VALUE.
::end:: */
{
  rep_DECLARE1(sym, rep_SYMBOLP);

  if (rep_SYM(sym)->car & rep_SF_SPECIAL) {
    return set_symbol_special_value(sym, val, true);
  } else {
    return Fstructure_set(rep_structure, sym, val);
  }
}

DEFUN("variable-bound?", Fboundp, Sboundp, (repv sym), rep_Subr1) /*
::doc:rep.lang.symbols#variable-bound?::
variable-bound? SYMBOL

Returns t if SYMBOL has a top-level or special binding.
::end:: */
{
  rep_DECLARE1(sym, rep_SYMBOLP);

  return rep_VOIDP(rep_symbol_value(sym, true, false)) ? rep_nil : Qt;
}

DEFUN("variable-bound-default?", Fdefault_boundp,
      Sdefault_boundp, (repv sym), rep_Subr1) /*
::doc:rep.lang.symbols#variable-bound-default?::
variable-bound-default? SYMBOL

Returns t if SYMBOL has a default top-level or special binding.
::end:: */
{
  rep_DECLARE1(sym, rep_SYMBOLP);

  if (rep_SYM(sym)->car & rep_SF_SPECIAL) {
    repv val = symbol_special_value(sym, true);
    return rep_VOIDP(val) ? rep_nil : Qt;
  } else {
    return Fstructure_bound_p(rep_structure, sym);
  }
}

DEFUN("set!", Fset_, Sset_, (repv args, bool tail_posn), rep_SF) /*
::doc:rep.lang.interpreter#set!::
set! SYMBOL FORM

Set the value of the current binding of SYMBOL to the result of
evaluating FORM.
::end:: */
{
  if (!rep_CONSP(args)) {
    return rep_signal_missing_arg(1);
  }

  repv sym = rep_CAR(args);
  args = rep_CDR(args);

  rep_DECLARE1(sym, rep_SYMBOLP);

  if (!rep_CONSP(args)) {
    return rep_signal_missing_arg(2);
  }

  /* FIXME: protection against incorrect conversion from `setq`. */

  if (rep_CDR(args) != rep_nil) {
    DEFSTRING(too_many, "too many args to set!");
    return Fsignal(Qerror, rep_LIST_1(rep_VAL(&too_many)));
  }

  rep_GC_root gc_sym;
  rep_PUSHGC(gc_sym, sym);

  repv value = Feval(rep_CAR(args));

  rep_POPGC;

  if (!value) {
    return 0;
  }

  if (rep_SYM(sym)->car & rep_SF_SPECIAL) {
    return set_symbol_special_value(sym, value, false);
  }

  /* Only scan lexical bindings when being called from set!, i.e.
     interpreted code. (Compiled code doesn't create named lexical
     bindings, so if we found a binding it would be one from outer
     interpreted code -- which by definition of lexical scope would be
     incorrect.) */

  repv tem = search_environment(sym, rep_env);
  if (tem != rep_nil) {
    rep_CDR(tem) = value;
    return rep_undefined_value;
  }

  return Fstructure_set(rep_structure, sym, value);
}

DEFUN("%define", F_define, S_define, (repv args, bool tail_posn), rep_SF) /*
::doc:rep.lang.interpreter#%define::
%define SYMBOL FORM [DOC-STRING]

Evaluate FORM, then create a top-level binding of SYMBOL whose value is
the result of the evaluation. If such a binding already exists, it will
be overwritten.
::end:: */
{
  if (!rep_CONSP(args)) {
    return rep_signal_missing_arg(1);
  }

  repv sym = rep_CAR(args);
  args = rep_CDR(args);

  rep_DECLARE1(sym, rep_SYMBOLP);

  /* Don't allow special variables to have lexical bindings as well. */

  if (rep_SYM(sym)->car & rep_SF_SPECIAL) {
    return rep_signal_arg_error(sym, 1);
  }

  if (!rep_CONSP(args)) {
    return rep_signal_missing_arg(2);
  }

  repv value = rep_CAR(args);
  args = rep_CDR(args);

  rep_GC_root gc_sym, gc_args;
  rep_PUSHGC(gc_sym, sym);
  rep_PUSHGC(gc_args, args);

  value = Feval(value);

  rep_POPGC; rep_POPGC;

  if (!value) {
    return 0;
  }

  if (!Fstructure_define(rep_structure, sym, value)) {
    return 0;
  }

  if (rep_CONSP(args)) {
    repv doc = rep_CAR(args);
    if (doc != rep_nil) {
      repv prop = rep_documentation_property(rep_structure);
      if (prop != rep_nil && !Fput(sym, prop, doc)) {
	return 0;
      }
    }
  }

  return rep_undefined_value;
}

DEFUN("makunbound", Fmakunbound, Smakunbound, (repv sym), rep_Subr1) /*
::doc:rep.lang.symbols#makunbound::
makunbound SYMBOL

Removes the top-level variable binding of SYMBOL.
::end:: */
{
  return Freal_set(sym, rep_void);
}

DEFUN("make-variable-special", Fmake_variable_special,
      Smake_variable_special, (repv sym), rep_Subr1) /*
::doc:rep.lang.symbols#make-variable-special::
make-variable-special SYMBOL

Mark SYMBOL as being a special (dynamically-bound) variable.
::end:: */
{
  rep_DECLARE1(sym, rep_SYMBOLP);

  if (!rep_special_variable_accessible_p(sym)) {
    return Fsignal(Qvoid_value, rep_LIST_1(sym));
  }

  if (!(rep_SYM(sym)->car & rep_SF_SPECIAL)) {
    repv tem = rep_get_initial_special_value(sym);
    if (tem) {
      Fstructure_define(rep_specials_structure, sym, tem);
    }

    rep_SYM(sym)->car |= rep_SF_SPECIAL;
  }

  return rep_undefined_value;
}

DEFUN("special-variable?", Fspecial_variable_p, Sspecial_variable_p,
      (repv sym), rep_Subr1) /*
::doc:rep.lang.symbols#special-variable?::
special-variable? SYMBOL

Returns t if SYMBOL is a special variable (dynamically scoped).
::end:: */
{
  rep_DECLARE1(sym, rep_SYMBOLP);

  return (rep_SYM(sym)->car & rep_SF_SPECIAL) ? Qt : rep_nil;
}

DEFUN("void?", Fvoidp, Svoidp, (repv arg), rep_Subr1) /*
::doc:rep.lang.interpreter#void?::
void? ARG

Returns true if ARG is the void value.
::end:: */
{
  return rep_VOIDP(arg) ? Qt : rep_nil;
}

void
rep_variables_init(void)
{
  repv tem;

  rep_INTERN(documentation);
  rep_INTERN(permanent_local);
  
  rep_env = rep_nil;
  rep_special_env = rep_nil;

  rep_mark_static(&rep_env);
  rep_mark_static(&rep_special_env);
  
  tem = rep_push_structure("rep.lang.symbols");
  rep_ADD_SUBR(Ssymbol_value);
  rep_ADD_SUBR_INT(Sset);
  rep_ADD_SUBR(Sdefault_value);
  rep_ADD_SUBR(Sdefault_boundp);
  rep_ADD_SUBR(Sset_default);
  rep_ADD_SUBR(Sboundp);
  rep_ADD_SUBR(Smakunbound);
  rep_ADD_SUBR(Smake_variable_special);
  rep_ADD_SUBR(Sspecial_variable_p);
  rep_pop_structure(tem);
  
  tem = rep_push_structure("rep.lang.interpreter");
  rep_ADD_SUBR(Sset_);
  rep_ADD_SUBR(S_define);
  rep_ADD_SUBR(Sdefvar);
  rep_ADD_SUBR(Svoidp);
  rep_pop_structure(tem);
}
