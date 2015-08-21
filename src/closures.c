/* closures.c -- function + environment

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

#define CLOSURES_PER_BLOCK 204 /* ~4k */

typedef struct closure_block_struct closure_block;

struct closure_block_struct {
  closure_block *next;
  rep_ALIGN_CELL(rep_closure data[CLOSURES_PER_BLOCK]);
};

static closure_block *closure_block_list;
static rep_closure *closure_free_list;

int rep_allocated_closures, rep_used_closures;

DEFUN("make-closure", Fmake_closure, Smake_closure,
      (repv fun, repv name), rep_Subr2) /*
::doc:rep.lang.interpreter#make-closure::
make-closure FUNCTION &optional NAME

Return a functional object which makes the closure of FUNCTION and the
current environment.
::end:: */
{
  if (!closure_free_list) {
    closure_block *sb = rep_alloc(sizeof(closure_block));
    if (!sb) {
      return rep_mem_error();
    }

    sb->next = closure_block_list;
    closure_block_list = sb;

    for (int i = 0; i < CLOSURES_PER_BLOCK - 1; i++) {
      sb->data[i].car = rep_VAL(&sb->data[i + 1]);
    }
    sb->data[CLOSURES_PER_BLOCK - 1].car = rep_VAL(closure_free_list);
    closure_free_list = sb->data;

    rep_allocated_closures += CLOSURES_PER_BLOCK;
  }

  rep_closure *f = closure_free_list;
  closure_free_list = rep_CLOSURE(f->car);

  f->car = rep_Closure;
  f->fun = fun;
  f->name = name;
  f->env = rep_env;
  f->structure = rep_structure;

  rep_data_after_gc += sizeof(rep_closure);
  return rep_VAL(f);
}

DEFUN("closure-function", Fclosure_function,
      Sclosure_function, (repv closure), rep_Subr1) /*
::doc:rep.lang.interpreter#closure-function::
closure-function CLOSURE

Return the function value associated with the closure CLOSURE.
::end:: */
{
  rep_DECLARE1(closure, rep_CLOSUREP);

  return rep_CLOSURE(closure)->fun;
}

DEFUN("set-closure-function", Fset_closure_function,
      Sset_closure_function, (repv closure, repv fun), rep_Subr2) /*
::doc:rep.lang.interpreter#set-closure-function::
set-closure-function CLOSURE FUNCTION

Set the function value in the closure CLOSURE to FUNCTION.
::end:: */
{
  rep_DECLARE1(closure, rep_CLOSUREP);

  rep_CLOSURE(closure)->fun = fun;
  return fun;
}

DEFUN("closure-structure", Fclosure_structure,
      Sclosure_structure, (repv closure), rep_Subr1) /*
::doc:rep.structures#closure-function::
closure-function CLOSURE

Return the structure associated with the closure CLOSURE.
::end:: */
{
  rep_DECLARE1(closure, rep_CLOSUREP);

  return rep_CLOSURE(closure)->structure;
}

DEFUN("set-closure-structure", Fset_closure_structure,
      Sset_closure_structure, (repv closure, repv structure), rep_Subr2)
{
  rep_DECLARE1(closure, rep_CLOSUREP);
  rep_DECLARE2(structure, rep_STRUCTUREP);

  rep_CLOSURE(closure)->structure = structure;
  return rep_nil;
}

DEFUN("closure-name", Fclosure_name,
      Sclosure_name, (repv closure), rep_Subr1) /*
::doc:rep.lang.interpreter#closure-name::
closure-name CLOSURE

Return the name associated with the closure CLOSURE.
::end:: */
{
  rep_DECLARE1(closure, rep_CLOSUREP);

  return rep_CLOSURE(closure)->name;
}

DEFUN("set-closure-name", Fset_closure_name,
      Sset_closure_name, (repv closure, repv name), rep_Subr2)
{
  rep_DECLARE1(closure, rep_CLOSUREP);
  rep_DECLARE2(name, rep_STRINGP);

  rep_CLOSURE(closure)->name = name;
  return rep_nil;
}

DEFUN("closurep", Fclosurep, Sclosurep, (repv arg), rep_Subr1) /*
::doc:rep.lang.interpreter#closurep::
closurep ARG

Returns t if ARG is a closure
::end:: */
{
  return rep_CLOSUREP(arg) ? Qt : rep_nil;
}

repv
rep_call_with_closure(repv closure, repv (*fun)(repv arg), repv arg)
{
  repv ret = 0;

  if (rep_CLOSUREP(closure)) {
    rep_stack_frame lc;
    lc.fun = lc.args = rep_nil;
    rep_PUSH_CALL(lc);
    rep_USE_CLOSURE(closure);
    ret = fun(arg);
    rep_POP_CALL(lc);
  }

  return ret;
}

static void
closure_sweep(void)
{
  closure_free_list = NULL;
  rep_used_closures = 0;

  closure_block *sb = closure_block_list;
  while (sb) {
    closure_block *next = sb->next;
    for (int i = 0; i < CLOSURES_PER_BLOCK; i++) {
      /* If on the freelist then the CELL_IS_8 bit will be unset
         (since the pointer is long aligned) */
      if (rep_CELL_CONS_P(rep_VAL(&sb->data[i]))
	  || !rep_GC_CELL_MARKEDP(rep_VAL(&sb->data[i])))
      {
	sb->data[i].car = rep_VAL(closure_free_list);
	closure_free_list = &sb->data[i];
      } else {
	rep_GC_CLR_CELL(rep_VAL(&sb->data[i]));
	rep_used_closures++;
      }
    }
    sb = next;
  }
}

void
rep_closures_init(void)
{
  static rep_type closure = {
    .car = rep_Closure,
    .name = "closure",
    .print = rep_lisp_prin,
    .sweep = closure_sweep,
    // marked inline by rep_mark_value()
  };

  rep_define_type(&closure);

  repv tem = rep_push_structure("rep.lang.interpreter");
  rep_ADD_SUBR(Smake_closure);
  rep_ADD_SUBR(Sclosure_function);
  rep_ADD_SUBR(Sset_closure_function);
  rep_ADD_SUBR(Sclosure_name);
  rep_ADD_SUBR(Sset_closure_name);
  rep_ADD_SUBR(Sclosurep);
  rep_pop_structure(tem);
  
  tem = rep_push_structure("rep.structures");
  rep_ADD_SUBR(Sclosure_structure);
  rep_ADD_SUBR(Sset_closure_structure);
  rep_pop_structure(tem);
}

void
rep_closures_kill(void)
{
  closure_block *b = closure_block_list;
  while (b) {
    closure_block *next = b->next;
    rep_free(b);
    b = next;
  }

  closure_block_list = NULL;
  closure_free_list = NULL;

  rep_allocated_closures = 0;
  rep_used_closures = 0;
}
