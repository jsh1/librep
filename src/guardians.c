/* guardians.c -- GC guardians

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

static rep_guardian *guardians;

DEFUN("make-primitive-guardian", Fmake_primitive_guardian,
      Smake_primitive_guardian, (void), rep_Subr0)
{
  rep_guardian *g = rep_alloc(sizeof(rep_guardian));

  g->car = rep_Guardian;
  g->accessible = rep_nil;
  g->inaccessible = rep_nil;
  g->next = guardians;
  guardians = g;

  rep_data_after_gc += sizeof(rep_guardian);

  return rep_VAL(g);
}

DEFUN("primitive-guardian-push", Fprimitive_guardian_push,
       Sprimitive_guardian_push, (repv g, repv obj), rep_Subr2)
{
  rep_DECLARE1(g, rep_GUARDIANP);
  if (rep_CELLP(obj)) { 
    rep_GUARDIAN(g)->accessible = Fcons(obj, rep_GUARDIAN(g)->accessible);
  }
  return g;
}

DEFUN("primitive-guardian-pop", Fprimitive_guardian_pop,
      Sprimitive_guardian_pop, (repv g), rep_Subr1)
{
  rep_DECLARE1 (g, rep_GUARDIANP);

  if (rep_GUARDIAN(g)->inaccessible == rep_nil) {
    return rep_nil;
  }

  repv ret = rep_CAR(rep_GUARDIAN(g)->inaccessible);
  rep_GUARDIAN(g)->inaccessible = rep_CDR(rep_GUARDIAN(g)->inaccessible);
  return ret;
}

static void
mark_guardian(repv g)
{
  /* Accessible list is marked by rep_run_guardians(). */

  rep_MARKVAL(rep_GUARDIAN(g)->inaccessible);
}

void
rep_run_guardians(void)
{
  struct list_node {
    struct list_node *next;
    repv obj;
  };

  struct list_node *changed_list = NULL;

  /* Scan all guardians for unmarked objects that used to be accessible. */

  for (rep_guardian *g = guardians; g; g = g->next) {
    repv *ptr = &g->accessible;

    while (1) {
      /* Cons cells store mark bit in cdr, so mask it out. */

      repv cell = *ptr & ~rep_VALUE_CONS_MARK_BIT;
      if (cell == rep_nil) {
	break;
      }

      if (!rep_GC_MARKEDP(rep_CAR(cell))) {
	/* Move object to inaccessible list. Have to preserve the cons
	   mark bit in '*ptr'. */

	*ptr = rep_GCDR(cell) | (*ptr & rep_VALUE_CONS_MARK_BIT);
	rep_CDR(cell) = g->inaccessible;
	g->inaccessible = cell;

	/* Note that we need to mark this object. */

	struct list_node *new = alloca(sizeof (struct list_node));
	new->obj = rep_CAR(cell);
	new->next = changed_list;
	changed_list = new;
      } else {
	ptr = rep_CDRLOC (cell);
      }

      /* Mark the list infrastructure. */

      rep_GC_SET_CONS(cell);
    }
  }

  /* Mark any objects that changed state. */

  for (struct list_node *node = changed_list; node; node = node->next) {
    rep_MARKVAL(node->obj);
  }
}

static void
sweep_guardians(void)
{
  rep_guardian *g = guardians;
  guardians = NULL;

  while (g) {
    rep_guardian *next = g->next;
    if (!rep_GC_CELL_MARKEDP(rep_VAL(g))) {
      rep_free(g);
    } else {
      rep_GC_CLR_CELL(rep_VAL (g));
      g->next = guardians;
      guardians = g;
    }
    g = next;
  }
}

static void
print_guardian(repv stream, repv obj)
{
  rep_stream_puts(stream, "#<guardian>", -1, false);
}

void
rep_guardians_init(void)
{
  static rep_type guardian = {
    .car = rep_Guardian,
    .name = "guardian",
    .print = print_guardian,
    .sweep = sweep_guardians,
    .mark = mark_guardian,
  };

  rep_define_type(&guardian);

  repv tem = rep_push_structure("rep.data");
  rep_ADD_INTERNAL_SUBR(Smake_primitive_guardian);
  rep_ADD_INTERNAL_SUBR(Sprimitive_guardian_push);
  rep_ADD_INTERNAL_SUBR(Sprimitive_guardian_pop);
  rep_pop_structure(tem);
}
