/* lambda.c -- lambda list interpreter

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

/* Used to mark tail calling throws */

rep_ALIGN_CELL(static rep_cell tail_call_tag) = {rep_Void};
#define TAIL_CALL_TAG rep_VAL(&tail_call_tag)

DEFSYM(lambda, "lambda");

repv ex_optional, ex_rest, ex_key;

static repv
bind_lambda_list_1(repv lambda_list, repv *args, int nargs)
{
  enum lambda_state {
    STATE_REQUIRED,
    STATE_OPTIONAL,
    STATE_KEY,
    STATE_REST,
    STATE_DONE,
  };

  struct lambda_var {
    repv sym;
    repv value;
    repv evalp;
  };

  const size_t lambda_var_elts = sizeof(struct lambda_var) / sizeof(repv);

  int lambda_len = rep_list_length(lambda_list);
  if (lambda_len < 0) {
    return 0;
  }

  struct lambda_var vars[lambda_len + 1];
  intptr_t var_count = 0;

  /* Verify vars is a contiguous array, needed for gc protection. */
  rep_static_assert(&vars[1].sym - &vars[0].sym == lambda_var_elts);

  enum lambda_state state = STATE_REQUIRED;

  /* Pass 1: traverse the lambda list, recording var-value pairs and
     whether each value needs to be evaluated or not.. */

  while (1) {
    repv argspec, def;
    if (rep_CONSP(lambda_list)) {
      argspec = rep_CAR(lambda_list);
      lambda_list = rep_CDR(lambda_list);
      if (argspec == ex_optional) {
	if (state >= STATE_OPTIONAL) {
	invalid:
	  return Fsignal(Qinvalid_lambda_list, rep_LIST_1(lambda_list));
	}
	state = STATE_OPTIONAL;
	continue;
      } else if (argspec == ex_key) {
	if (state >= STATE_KEY) {
	  goto invalid;
	}
	state = STATE_KEY;
	continue;
      } else if (argspec == ex_rest) {
	if (state >= STATE_REST) {
	  goto invalid;
	}
	state = STATE_REST;
	continue;
      }
    } else if (lambda_list == rep_nil) {
      break;
    } else if (rep_SYMBOLP(lambda_list)) {
      if (state >= STATE_REST) {
	goto invalid;
      }
      state = STATE_REST;
      argspec = lambda_list;
      lambda_list = rep_nil;
    } else {
      goto invalid;
    }

    if (rep_SYMBOLP(argspec)) {
      vars[var_count].sym = argspec;
      def = rep_nil;
    } else if (rep_CONSP(argspec) && rep_SYMBOLP(rep_CAR(argspec))) {
      vars[var_count].sym = rep_CAR(argspec);
      if (rep_CONSP(rep_CDR(argspec))) {
	def = rep_CADR(argspec);
      } else {
	def = rep_nil;
      }
    } else {
      goto invalid;
    }

    vars[var_count].evalp = rep_nil;

    switch (state) {
    case STATE_REQUIRED:
    case STATE_OPTIONAL:
      if (nargs > 0) {
	vars[var_count].value = *args++;
	nargs--;
      } else if (state == STATE_OPTIONAL) {
	vars[var_count].value = def;
	vars[var_count].evalp = Qt;
      } else {
	repv fun = rep_call_stack != 0 ? rep_call_stack->fun : rep_nil;
	return Fsignal(Qmissing_arg, rep_list_2(fun, argspec));
      }
      break;

    case STATE_KEY: {
      repv key = Fmake_keyword(vars[var_count].sym);
      vars[var_count].value = def;
      vars[var_count].evalp = Qt;
      for (int i = 0; i < nargs - 1; i++) {
	if (args[i] == key && args[i+1] != 0) {
	  vars[var_count].value = args[i+1];
	  vars[var_count].evalp = rep_nil;
	  args[i] = args[i+1] = 0;
	  break;
	}
      }
      break; }

    case STATE_REST: {
      repv list = rep_nil;
      repv *ptr = &list;
      while (nargs > 0) {
	if (*args != 0) {
	  *ptr = Fcons(*args, rep_nil);
	  ptr = rep_CDRLOC(*ptr);
	}
	args++; nargs--;
      }
      vars[var_count].value = list;
      state = STATE_DONE;
      break; }

    case STATE_DONE:
      goto invalid;
    }

    var_count++;

    rep_TEST_INT;
    if (rep_INTERRUPTP)
      return 0;
  }

  /* Pass 2: evaluate any values that need it.. */

  if (var_count > 0) {
    rep_GC_n_roots gc_vars;
    rep_PUSHGCN(gc_vars, (repv *)vars, var_count * lambda_var_elts);

    for (int i = 0; i < var_count; i++) {
      if (vars[i].evalp != rep_nil) {
	repv tem = Feval(vars[i].value);
	if (!tem) {
	  rep_POPGCN;
	  return 0;
	}
	vars[i].value = tem;
      }
    }

    rep_POPGCN;
  }

  /* Pass 3: instantiate the bindings */

  repv frame = rep_EMPTY_BINDING_FRAME;

  for (int i = 0; i < var_count; i++) {
    frame = rep_bind_symbol(frame, vars[i].sym, vars[i].value);
  }

  return frame;
}

/* format of lambda-lists is something like,

   [<required-params>*] [#!optional <optional-param>*]
   [#!key <keyword-param>*] [#!rest <rest-param>]

   A keyword parameter X is associated with an argument by a keyword
   symbol #:X. If no such symbol exists, it's bound to false

   <optional-param> and <keyword-param> is either <symbol> or(<symbol>
   <default>) where <default> is a constant

   Note that the lambda_list arg isn't protected from gc by this
   function; it's assumed that this is done by the caller.

   IMPORTANT: this expects the top of the call stack to have the
   saved environments in which arguments need to be evaluated */

static repv
bind_lambda_list(repv lambda_list, repv args)
{
  int argc = rep_list_length(args);
  if (argc < 0) {
    return 0;
  }
  repv *argv = rep_stack_alloc(repv, argc);
  if (!argv) {
    return rep_mem_error();
  }

  for (int i = 0; i < argc; i++) {
    argv[i] = rep_CAR(args);
    args = rep_CDR(args);
    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      return 0;
    }
  }

  repv frame = bind_lambda_list_1(lambda_list, argv, argc);

  rep_stack_free(repv, argc, argv);

  return frame;
}

repv
rep_apply_lambda(repv lambda_exp, repv arg_list, bool tail_posn)
{
  /* loop for tail-calling. */

  while (1) {
    lambda_exp = rep_CDR(lambda_exp);

    if (!rep_CONSP(lambda_exp)) {
      return 0;
    }

    rep_GC_root gc_lambda_exp, gc_arg_list;
    rep_PUSHGC(gc_lambda_exp, lambda_exp);
    rep_PUSHGC(gc_arg_list, arg_list);

    repv frame = bind_lambda_list(rep_CAR(lambda_exp), arg_list);

    rep_POPGC; rep_POPGC;

    repv result = 0;

    if (!frame) {
      return 0;
    }

    rep_GC_root gc_frame;
    rep_PUSHGC(gc_frame, frame);

    /* The body of the function is only in the tail position if the
       parameter list only creates lexical bindings */

    result = Fprogn(rep_CDR(lambda_exp), rep_SPEC_BINDINGS(frame) == 0);

    rep_POPGC;

    rep_unbind_symbols(frame);

    if (tail_posn || result || !rep_throw_value
	|| rep_CAR(rep_throw_value) != TAIL_CALL_TAG
	|| !rep_CONSP(rep_CDR(rep_throw_value)))
    {
      /* Result isn't a preserved tail-call, exit immediately. Or if
         we were called in tail-position, defer to our caller to unwrap
	 the inner function application. */

      return result;
    }

    /* Caught a preserved tail-call, execute it. */

    repv func = rep_CADR(rep_throw_value);
    repv args = rep_CDDR(rep_throw_value);

    rep_throw_value = 0;

    if (!rep_CLOSUREP(func)
	|| !rep_CONSP(rep_CLOSURE(func)->fun)
	|| rep_CAR(rep_CLOSURE(func)->fun) != Qlambda)
    {
      /* Tail-call is not a lambda we can eval directly. */

      return rep_apply(func, args);
    }

    /* Evaluate the lambda we caught in the next iteration. */

    rep_USE_CLOSURE(func);
    lambda_exp = rep_CLOSURE(func)->fun;
    arg_list = args;
  }

  /* not reached. */
}

/* LST is (FUN . ARGS) */

repv
rep_tail_call_throw(repv lst)
{
  return Fcons(TAIL_CALL_TAG, lst);
}

void
rep_lambda_init(void)
{
  DEFSTRING(optional, "#!optional");
  DEFSTRING(rest, "#!rest");
  DEFSTRING(key, "#!key");

  rep_INTERN(lambda);

  ex_optional = Fmake_symbol(rep_VAL(&optional));
  ex_rest = Fmake_symbol(rep_VAL(&rest));
  ex_key = Fmake_symbol(rep_VAL(&key));

  rep_SYM(ex_optional)->car |= rep_SF_LITERAL;
  rep_SYM(ex_rest)->car |= rep_SF_LITERAL;
  rep_SYM(ex_key)->car |= rep_SF_LITERAL;

  rep_mark_static(&ex_optional);
  rep_mark_static(&ex_rest);
  rep_mark_static(&ex_key);
}
