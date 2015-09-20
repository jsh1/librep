/* gc.c -- garbage collector

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

#include <stdlib.h>
#include <assert.h>

DEFSYM(after_gc_hook, "*after-gc-hook*");

static repv **static_roots;
static int next_static_root, allocated_static_roots;

rep_GC_root *rep_gc_root_stack = 0;
rep_GC_n_roots *rep_gc_n_roots_stack = 0;

/* Bytes of storage used since last gc. */

int rep_data_after_gc;

/* Value that rep_data_after_gc should be before collecting. */

int rep_gc_threshold = 200000;

/*  Value that rep_data_after_gc should be before collecting while idle. */

int rep_idle_gc_threshold = 20000;

void
rep_mark_static(repv *obj)
{
  if (next_static_root == allocated_static_roots) {
    int new_size = allocated_static_roots ? (allocated_static_roots * 2) : 256;
    if (static_roots != 0) {
      static_roots = rep_realloc (static_roots, new_size * sizeof (repv *));
    } else {
      static_roots = rep_alloc (new_size * sizeof (repv *));
    }
    assert (static_roots != 0);
    allocated_static_roots = new_size;
  }

  static_roots[next_static_root++] = obj;
}

/* Mark a single Lisp object. Note that VAL must not be NULL, and must
   not already have been marked, (see the rep_MARKVAL macro in lisp.h) */

void
rep_mark_value(repv val)
{
  if (rep_INTP(val)) {
    return;
  }

  /* Must now be a cell (i.e. pointer). */

again:
  if (rep_CELL_CONS_P(val)) {
    /* A pair. */

    rep_GC_SET_CONS(val);

    if (rep_GCDR(val) == rep_nil) {
      val = rep_CAR(val);
    } else {
      rep_MARKVAL(rep_CAR(val));
      val = rep_GCDR(val);
    }

    if (val && !rep_INTP(val) && !rep_GC_MARKEDP(val)) {
      goto again;
    }

    return;
  }

  if (rep_CELL16P(val)) {
    /* A user allocated type. */

    rep_GC_SET_CELL(val);

    rep_type *t = rep_get_data_type(rep_CELL16_TYPE(val));
    if (t->mark) {
      t->mark(val);
    }

    return;
  }

  /* Must now be a cell8 object. */

  switch (rep_CELL8_TYPE(val)) {
  case rep_Vector:
  case rep_Bytecode:
    rep_GC_SET_CELL(val);
    int len = rep_VECTOR_LEN(val);
    for (int i = 0; i < len; i++) {
      rep_MARKVAL(rep_VECTI(val, i));
    }
    break;

  case rep_Symbol:
    rep_GC_SET_CELL(val);
    rep_MARKVAL(rep_SYM(val)->name);
    val = rep_SYM(val)->next;
    if (val && !rep_VOIDP(val) && !rep_GC_MARKEDP(val)) {
      goto again;
    }
    break;

  case rep_String:
    if (!rep_CELL_STATIC_P(val)) {
      rep_GC_SET_CELL(val);
    }
    break;

  case rep_Number:
    rep_GC_SET_CELL(val);
    break;

  case rep_Closure:
    rep_GC_SET_CELL(val);
    rep_MARKVAL(rep_CLOSURE(val)->name);
    rep_MARKVAL(rep_CLOSURE(val)->env);
    rep_MARKVAL(rep_CLOSURE(val)->structure);
    val = rep_CLOSURE(val)->fun;
    if (val && !rep_INTP(val) && !rep_GC_MARKEDP(val)) {
      goto again;
    }
    break;

  case rep_Char:
    rep_GC_SET_CELL(val);
    val = rep_CHAR(val)->next;
    if (val && !rep_GC_MARKEDP(val)) {
      goto again;
    }
    break;

  case rep_Subr:
  case rep_SF:
    break;

  default: {
    rep_GC_SET_CELL(val);
    rep_type *t = rep_get_data_type(rep_CELL8_TYPE(val));
    if (t->mark) {
      t->mark(val);
    }
    break; }
  }
}

DEFUN("garbage-threshold", Fgarbage_threshold,
      Sgarbage_threshold, (repv val), rep_Subr1) /*
::doc:rep.data#garbage-threshold::
garbage-threshold [NEW-VALUE]

The number of bytes of storage which must be used before a garbage-
collection is triggered.
::end:: */
{
  return rep_handle_var_int(val, &rep_gc_threshold);
}

DEFUN("idle-garbage-threshold", Fidle_garbage_threshold,
      Sidle_garbage_threshold, (repv val), rep_Subr1) /*
::doc:rep.data#idle-garbage-threshold::
idle-garbage-threshold [NEW-VALUE]

The number of bytes of storage which must be used before a garbage-
collection is triggered when the editor is idle.
::end:: */
{
  return rep_handle_var_int(val, &rep_idle_gc_threshold);
}

DEFUN_INT("garbage-collect", Fgarbage_collect,
	  Sgarbage_collect, (repv stats), rep_Subr1, "") /*
::doc:rep.data#garbage-collect::
garbage-collect

Scans all allocated storage for unusable data, and puts it onto the free-
list. This is done automatically when the amount of storage used since the
last garbage-collection is greater than `garbage-threshold'.
::end:: */
{
  rep_macros_before_gc();

  /* Mark static objects. */

  for(int i = 0; i < next_static_root; i++) {
    rep_MARKVAL(*static_roots[i]);
  }

  /* Mark stack based objects protected from GC. */

  for (rep_GC_root *root = rep_gc_root_stack; root; root = root->next) {
    rep_MARKVAL(*root->ptr);
  }

  for (rep_GC_n_roots *root = rep_gc_n_roots_stack; root; root = root->next) {
    for (int i = 0; i < root->count; i++) {
      rep_MARKVAL(root->first[i]);
    }
  }

  /* Do data-type specific marking. */

  rep_mark_types();

  rep_mark_regexp_data();
  rep_mark_origins ();

#ifdef HAVE_DYNAMIC_LOADING
  rep_dl_mark_data();
#endif

  /* Mark the Lisp backtrace. */

  for (rep_stack_frame *lc = rep_call_stack; lc; lc = lc->next) {
    rep_MARKVAL(lc->fun);
    rep_MARKVAL(lc->args);
    rep_MARKVAL(lc->current_form);
    rep_MARKVAL(lc->saved_env);
    rep_MARKVAL(lc->saved_structure);
  }

  /* Handle weak or guarded objects that weren't marked. */

  rep_run_guardians ();
  rep_scan_weak_refs ();
  rep_scan_origins ();

  /* Finished marking, start sweeping. */

  rep_sweep_tuples ();
  rep_sweep_types();

  /* Done. */

  rep_data_after_gc = 0;

  rep_types_after_gc();
  Fcall_hook(Qafter_gc_hook, rep_nil, rep_nil);

  if (stats != rep_nil) {
    return rep_list_5(Fcons(rep_MAKE_INT(rep_used_cons),
			    rep_MAKE_INT(rep_allocated_cons - rep_used_cons)),
		      Fcons(rep_MAKE_INT(rep_used_tuples),
			    rep_MAKE_INT(rep_allocated_tuples
					 - rep_used_tuples)),
		      rep_list_3(rep_MAKE_INT(rep_used_strings),
				 rep_MAKE_INT(rep_allocated_strings),
				 rep_MAKE_INT(rep_allocated_string_bytes)),
		      rep_MAKE_INT(rep_used_vector_slots),
		      Fcons(rep_MAKE_INT(rep_used_closures),
			    rep_MAKE_INT(rep_allocated_closures
					 - rep_used_closures)));
  } else {
    return Qt;
  }
}

void
rep_gc_init(void)
{
  repv tem = rep_push_structure("rep.data");
  rep_ADD_SUBR(Sgarbage_threshold);
  rep_ADD_SUBR(Sidle_garbage_threshold);
  rep_ADD_SUBR_INT(Sgarbage_collect);
  rep_INTERN_SPECIAL(after_gc_hook);
  rep_pop_structure(tem);
}
