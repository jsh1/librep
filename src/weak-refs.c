/* weak-refs.c -- 

   Copyright (C) 2001 John Harper <jsh@pixelslut.com>

   This file is part of librep.

   librep is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   librep is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with librep; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "repint.h"

#define WEAKP(x)	rep_CELL8_TYPEP(x, rep_Weak_Ref)
#define WEAK(v)		((rep_tuple *) rep_PTR (v))
#define WEAK_NEXT(v)	(WEAK(v)->a)
#define WEAK_REF(v)	(WEAK(v)->b)

static repv weak_refs;

DEFUN("make-weak-ref", Fmake_weak_ref, Smake_weak_ref, (repv ref), rep_Subr1)
{
  repv weak_ref = rep_make_tuple(rep_Weak_Ref, 0, 0);

  WEAK_REF(weak_ref) = ref;
  WEAK_NEXT(weak_ref) = weak_refs;
  weak_refs = weak_ref;
 
  return weak_ref;
}

DEFUN("weak-ref", Fweak_ref, Sweak_ref, (repv weak), rep_Subr1)
{
  rep_DECLARE1(weak, WEAKP);

  return WEAK_REF(weak);
}
    
DEFUN("weak-ref-set!", Fweak_ref_set, Sweak_ref_set,
      (repv weak, repv value), rep_Subr2)
{
  rep_DECLARE1(weak, WEAKP);

  WEAK_REF(weak) = value;
  return value;
}

void
rep_scan_weak_refs(void)
{
  repv ref = weak_refs;
  weak_refs = 0;

  while (ref) {
    repv next = WEAK_NEXT(ref);

    if (rep_GC_CELL_MARKEDP(ref)) {

      /* This ref wasn't gc'd. */

      WEAK_NEXT(ref) = weak_refs;
      weak_refs = ref;

      if (rep_CELLP(WEAK_REF(ref))
	  && !rep_GC_MARKEDP(WEAK_REF(ref)))
      {
	/* But the object it points to was. */

	WEAK_REF(ref) = rep_nil;
      }
    }

    ref = next;
  }
}

static void
weak_ref_print(repv stream, repv arg)
{
  rep_stream_puts(stream, "#<weak-reference>", -1, false);
}

void
rep_weak_refs_init(void)
{
  static rep_type weak_ref = {
    .car = rep_Weak_Ref,
    .name = "weak-ref",
    .print = weak_ref_print,
  };

  rep_define_type(&weak_ref);

  repv tem = rep_push_structure("rep.data");
  rep_ADD_SUBR(Smake_weak_ref);
  rep_ADD_SUBR(Sweak_ref);
  rep_ADD_SUBR(Sweak_ref_set);
  rep_pop_structure(tem);
}
