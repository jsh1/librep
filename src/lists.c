/* lists.c -- list / pair functions

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

#define CONS_PER_BLOCK 1022 /* ~8k */

typedef struct cons_block_struct cons_block;

struct cons_block_struct {
  cons_block *next;
  rep_ALIGN_CELL(rep_cons cons[CONS_PER_BLOCK]);
};

static cons_block *cons_block_list;
static rep_cons *cons_free_list;

int rep_allocated_cons, rep_used_cons;

void
rep_cons_free(repv cn)
{
  rep_CDR(cn) = rep_VAL(cons_free_list);
  cons_free_list = rep_CONS(cn);
  rep_used_cons--;
}

/* Note that the cell returned is not linked into the free-list! */

static rep_cons *
refill_free_list(void)
{
  cons_block *cb = rep_alloc(sizeof(cons_block));
  rep_allocated_cons += CONS_PER_BLOCK;

  cb->next = cons_block_list;
  cons_block_list = cb;

  for (int i = 1; i < CONS_PER_BLOCK - 1; i++) {
    cb->cons[i].cdr = rep_VAL(&cb->cons[i + 1]);
  }
  cb->cons[CONS_PER_BLOCK - 1].cdr = 0;
  cons_free_list = &cb->cons[1];

  return &cb->cons[0];
}

DEFUN("cons", Fcons, Scons, (repv car, repv cdr), rep_Subr2) /*
::doc:rep.data#cons::
cons CAR CDR

Returns a new cons-cell with car CAR and cdr CDR.
::end:: */
{
  rep_cons *c = cons_free_list;

  if (c) {
    cons_free_list = rep_CONS(c->cdr);
  } else {
    c = refill_free_list();
  }

  rep_used_cons++;
  rep_data_after_gc += sizeof(rep_cons);

  c->car = car;
  c->cdr = cdr;

  return rep_VAL (c);
}

void
rep_cons_sweep(void)
{
  rep_cons *free_list = 0;
  int used = 0;

  for (cons_block *cb = cons_block_list; cb; cb = cb->next) {
    for (int i = 0; i < CONS_PER_BLOCK; i++) {
      rep_cons *cell = &cb->cons[i];
      if (!rep_GC_CONS_MARKEDP(rep_VAL(cell))) {
	cell->cdr = rep_VAL(free_list);
	free_list = cell;
      } else {
	rep_GC_CLR_CONS(rep_VAL(cell));
	used++;
      }
    }
  }

  cons_free_list = free_list;
  rep_used_cons = used;
}

int
rep_cons_cmp(repv v1, repv v2)
{
  if (rep_TYPE(v1) != rep_TYPE(v2)) {
    return 1;
  }

  int ret = rep_value_cmp(rep_CAR(v1), rep_CAR(v2));

  if (ret == 0) {
    ret = rep_value_cmp(rep_CDR(v1), rep_CDR(v2));
  }

  return ret;
}

repv
rep_list_1(repv v1)
{
  return rep_LIST_1(v1);
}

repv
rep_list_2(repv v1, repv v2)
{
  return rep_LIST_2(v1, v2);
}

repv
rep_list_3(repv v1, repv v2, repv v3)
{
  return rep_LIST_3(v1, v2, v3);
}

repv
rep_list_4(repv v1, repv v2, repv v3, repv v4)
{
  return rep_LIST_4(v1, v2, v3, v4);
}

repv
rep_list_5(repv v1, repv v2, repv v3, repv v4, repv v5)
{
  return rep_LIST_5(v1, v2, v3, v4, v5);
}

/* Returns -1 if an exception occurred (e.g. user interrupt due to
   infinite list). */

int
rep_list_length(repv list)
{
  int i = 0;

  while (rep_CONSP(list)) {
    i++;
    list = rep_CDR(list);
    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      return -1;
    }
  }

  return i;
}

repv
rep_copy_list(repv list)
{
  repv result;
  repv *last = &result;

  while (rep_CONSP(list) && !rep_INTERRUPTP) {
    repv cell = Fcons(rep_CAR(list), rep_nil);
    *last = cell;
    list = rep_CDR(list);
    last = rep_CDRLOC(cell);
    rep_TEST_INT;
  }

  *last = list;
  return result;
}

repv
rep_concat_lists(repv args)
{
  int len = rep_list_length(args);
  if (len < 0) {
    return 0;
  }

  repv *vec = rep_stack_alloc(repv, len);
  if (!vec) {
    return 0;
  }

  for (int i = 0; i < len; i++) {
    vec[i] = rep_CAR(args);
    args = rep_CDR(args);
    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      return 0;
    }
  }

  repv ret = Fnconc(len, vec);

  rep_stack_free(repv, len, vec);

  return ret;
}

DEFUN("car", Fcar, Scar, (repv cons), rep_Subr1) /*
::doc:rep.data#car::
car CONS-CELL

Returns the value stored in the car slot of CONS-CELL, or nil if CONS-CELL
is nil.
::end:: */
{
  return rep_CONSP(cons) ? rep_CAR(cons) : rep_nil;
}

DEFUN("cdr", Fcdr, Scdr, (repv cons), rep_Subr1) /*
::doc:rep.data#cdr::
cdr CONS-CELL

Returns the value stored in the cdr slot of CONS-CELL, or nil if CONS-CELL
is nil.
::end:: */
{
  return rep_CONSP(cons) ? rep_CDR(cons) : rep_nil;
}

DEFUN("rplaca", Frplaca, Srplaca, (repv cons, repv car), rep_Subr2) /*
::doc:rep.data#rplaca::
rplaca CONS-CELL NEW-CAR

Sets the value of the car slot in CONS-CELL to NEW-CAR.
Returns the CONS-CELL.
::end:: */
{
  rep_DECLARE1(cons, rep_CONSP);

  rep_CAR(cons) = car;
  return cons;
}

DEFUN("rplacd", Frplacd, Srplacd, (repv cons, repv cdr), rep_Subr2) /*
::doc:rep.data#rplacd::
rplacd CONS-CELL NEW-CDR

Sets the value of the cdr slot in CONS-CELL to NEW-CDR.
Returns the CONS-CELL.
::end:: */
{
  rep_DECLARE1(cons, rep_CONSP);

  rep_CDR(cons) = cdr;
  return cons;
}

DEFUN("list", Flist, Slist, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.data#list::
list ARGS...

Returns a new list with elements ARGS...
::end:: */
{
  repv lst = rep_nil;
  for (int i = argc - 1; i >= 0; i--) {
    lst = Fcons(argv[i], lst);
  }
  return lst;
}

DEFUN("list*", Flist_star, Slist_star, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.data#list*::
list* ARG1 ARG2 ... ARGN

Returns a new list (ARG1 ARG2 ... ARGN-1 . ARGN). That is, the same as from
`list' but the last argument is dotted to the last but one argument.
::end:: */
{
  if (argc == 0) {
    return rep_nil;
  } else {
    repv lst = argv[argc - 1];
    for (int i = argc - 2; i >= 0; i--) {
      lst = Fcons(argv[i], lst);
    }
    return lst;
  }
}

DEFUN("make-list", Fmake_list, Smake_list, (repv len, repv init), rep_Subr2) /*
::doc:rep.data#make-list::
make-list LENGTH [INITIAL-VALUE]

Returns a new list with LENGTH members, each of which is initialised to
INITIAL-VALUE, or nil.
::end:: */
{
  rep_DECLARE1(len, rep_NON_NEG_INT_P);

  repv list = rep_nil;
  for (int i = 0; i < rep_INT(len); i++) {
    list = Fcons(init, list);
  }
  return list;
}

DEFUN("append", Fappend, Sappend, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.data#append::
append LISTS...

Non-destructively concatenates each of it's argument LISTS... into one
new list which is returned.
::end:: */
{
  repv res = rep_nil;
  repv *res_end = &res;

  for (int i = 0; i < argc; i++) {
    if (i < argc - 1) {
      if (!rep_LISTP(argv[i])) {
	return rep_signal_arg_error(argv[i], i + 1);
      }

      *res_end = rep_copy_list(argv[i]);
    } else {
      *res_end = argv[i];
    }

    while (rep_CONSP(*res_end)) {
      rep_TEST_INT;
      if (rep_INTERRUPTP) {
	return 0;
      }
      res_end = rep_CDRLOC(*res_end);
    }
  }

  return res;
}

DEFUN("nconc", Fnconc, Snconc, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.data#nconc::
nconc LISTS...

Destructively concatenates each of it's argument LISTS... into one new
list. Every LIST but the last is modified so that it's last cdr points
to the beginning of the next list. Returns the new list.
::end:: */
{
  repv res = rep_nil;
  repv *res_end = &res;

  for (int i = 0; i < argc; i++) {
    if (i < argc - 1 && !rep_LISTP(argv[i])) {
      return rep_signal_arg_error(argv[i], i + 1);
    }

    *res_end = argv[i];
 
    while (rep_CONSP(*res_end)) {
      rep_TEST_INT;
      if (rep_INTERRUPTP) {
	return 0;
      }
      res_end = rep_CDRLOC(*res_end);
    }
  }

  return res;
}

DEFUN("reverse", Freverse, Sreverse, (repv head), rep_Subr1) /*
::doc:rep.data#reverse::
reverse LIST

Returns a new list which is a copy of LIST except that the members are in
reverse order.
::end:: */
{
  rep_DECLARE1(head, rep_LISTP);

  repv res = rep_nil;
  while (rep_CONSP(head)) {
    res = Fcons(rep_CAR(head), res);
    head = rep_CDR(head);
    rep_TEST_INT;
    if(rep_INTERRUPTP) {
      return 0;
    }
  }
  return res;
}

DEFUN("nreverse", Fnreverse, Snreverse, (repv head), rep_Subr1) /*
::doc:rep.data#nreverse::
nreverse LIST

Returns LIST altered so that it's members are in reverse order to what they
were. This function is destructive towards it's argument.
::end:: */
{
  rep_DECLARE1(head, rep_LISTP);

  if (rep_NILP(head)) {
    return head;
  }

  repv res = rep_nil;
  while (rep_CONSP(head)) {
    repv next = rep_CDR(head);
    rep_CDR(head) = res;
    res = head;
    head = next;
    rep_TEST_INT;
    if(rep_INTERRUPTP) {
      return 0;
    }
  }

  return res;
}

DEFUN("assoc", Fassoc, Sassoc, (repv elt, repv list), rep_Subr2) /*
::doc:rep.data#assoc::
assoc ELT ASSOC-LIST

Searches ASSOC-LIST for a list whose first element is ELT. `assoc' uses
`equal?' to compare elements. Returns the sub-list starting from the first 
matching association.
For example,
    (assoc 'three '((one . 1) (two . 2) (three . 3) (four . 4)))
     => (three . 3)
::end:: */
{
  rep_DECLARE2(list, rep_LISTP);

  while (rep_CONSP(list)) {
    repv car = rep_CAR(list);
    if (rep_CONSP(car) && !rep_value_cmp(elt, rep_CAR(car))) {
      return car;
    }
    list = rep_CDR(list);
    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      return 0;
    }
  }

  return rep_nil;
}

DEFUN("assq", Fassq, Sassq, (repv elt, repv list), rep_Subr2) /*
::doc:rep.data#assq::
assq ELT ASSOC-LIST

Searches ASSOC-LIST for a list whose first element is ELT. `assq' uses `eq?'
to compare elements. Returns the sub-list starting from the first matching
association.
::end:: */
{
  rep_DECLARE2(list, rep_LISTP);

  while (rep_CONSP(list)) {
    repv car = rep_CAR(list);
    if (rep_CONSP(car) && elt == rep_CAR(car)) {
      return car;
    }
    list = rep_CDR(list);
    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      return 0;
    }
  }

  return rep_nil;
}

DEFUN("rassoc", Frassoc, Srassoc, (repv elt, repv list), rep_Subr2) /*
::doc:rep.data#rassoc::
rassoc ELT ASSOC-LIST

Searches ASSOC-LIST for a cons-cell whose cdr element is `equal?' to ELT. 
Returns the first cons-cell which matches, or nil.
For example,
    (rassoc 3 '((one . 1) (two . 2) (three . 3) (four . 4)))
     => (three . 3)
::end:: */
{
  rep_DECLARE2(list, rep_LISTP);

  while (rep_CONSP(list)) {
    repv car = rep_CAR(list);
    if (rep_CONSP(car) && !rep_value_cmp(elt, rep_CDR(car))) {
      return car;
    }
    list = rep_CDR(list);
    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      return 0;
    }
  }

  return rep_nil;
}

DEFUN("rassq", Frassq, Srassq, (repv elt, repv list), rep_Subr2) /*
::doc:rep.data#rassq::
rassq ELT ASSOC-LIST

Searches ASSOC-LIST for a cons-cell whose cdr is `eq?' to ELT.
Returns the first matching cons-cell, else nil.
::end:: */
{
  rep_DECLARE2(list, rep_LISTP);

  while (rep_CONSP(list)) {
    repv car = rep_CAR(list);
    if (rep_CONSP(car) && elt == rep_CDR(car)) {
      return car;
    }
    list = rep_CDR(list);
    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      return 0;
    }
  }

  return rep_nil;
}

DEFUN("nth", Fnth, Snth, (repv index, repv list), rep_Subr2) /*
::doc:rep.data#nth::
nth INDEX LIST

Returns the INDEXth element of LIST. The first element has an INDEX of zero.
::end:: */
{
  rep_DECLARE1(index, rep_NON_NEG_INT_P);
  rep_DECLARE2(list, rep_LISTP);

  intptr_t i = rep_INT(index);
  while (i-- > 0 && rep_CONSP(list)) {
    list = rep_CDR(list);
    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      return 0;
    }
  }

  return i <= 0 && rep_CONSP(list) ? rep_CAR(list) : rep_nil;
}

DEFUN("nthcdr", Fnthcdr, Snthcdr, (repv index, repv list), rep_Subr2) /*
::doc:rep.data#nthcdr::
nthcdr INDEX LIST

Returns the INDEXth cdr of LIST. The first is INDEX zero.
::end:: */
{
  rep_DECLARE1(index, rep_NON_NEG_INT_P);
  rep_DECLARE2(list, rep_LISTP);

  intptr_t i = rep_INT(index);
  while (i-- > 0 && rep_CONSP(list)) {
    list = rep_CDR(list);
    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      return 0;
    }
  }

  return list;
}

DEFUN("last", Flast, Slast, (repv list), rep_Subr1) /*
::doc:rep.data#last::
last LIST

Returns the last element of LIST.
::end:: */
{
  rep_DECLARE1(list, rep_LISTP);

  if (!rep_CONSP(list)) {
    return rep_nil;
  }

  while (rep_CONSP(rep_CDR(list))) {
    list = rep_CDR(list);
    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      return 0;
    }
  }
  return rep_CAR(list);
}

DEFSTRING(map_invalid_lists, "lists are different lengths");

DEFUN("map", Fmap, Smap, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.data#map::
map FUNCTION LISTS...

Calls FUNCTION with groups of element from LISTS..., i.e. all the first
elements then all the second elements and so on, returning a new list
made from the result of each function call. If more than one list is
provided, they must all have the same number of elements.
::end:: */
{
  if (argc < 2) {
    return rep_signal_missing_arg(argc + 1);
  }

  for (int i = 1; i < argc; i++) {
    rep_DECLARE(i + 1, argv[i], rep_LISTP(argv[i]));
  }

  repv ret = rep_nil;
  repv *tail = &ret;

  rep_GC_root gc_ret;
  rep_GC_n_roots gc_argv;
  rep_PUSHGC(gc_ret, ret);
  rep_PUSHGCN(gc_argv, argv, argc);

  while (1) {
    repv elts[argc-1];
    int elts_count = 0;
    for (int i = 1; i < argc; i++) {
      if (rep_CONSP(argv[i])) {
	elts[i-1] = rep_CAR(argv[i]);
	argv[i] = rep_CDR(argv[i]);
	elts_count++;
      }
    }
    if (elts_count == 0) {
      break;
    } else if (elts_count != argc - 1) {
      ret = Fsignal(Qerror, rep_LIST_1(rep_VAL(&map_invalid_lists)));
      break;
    }

    repv tem = rep_call_lispn(argv[0], argc - 1, elts);
    if (!tem) {
      ret = 0;
      break;
    }

    repv cell = Fcons(tem, rep_nil);
    *tail = cell;
    tail = rep_CDRLOC(cell);

    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      ret = 0;
      break;
    }
  }

  rep_POPGCN;
  rep_POPGC;

  return ret;
}

DEFUN("for-each", Ffor_each, Sfor_each, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.data#for-each::
for-each FUNCTION LISTS...

Calls FUNCTION with groups of element from LISTS..., i.e. all the first
elements then all the second elements and so on, discarding the
results. If more than one list is provided, they must all have the same
number of elements.
::end:: */
{
  if (argc < 2) {
    return rep_signal_missing_arg(argc + 1);
  }

  for (int i = 1; i < argc; i++) {
    rep_DECLARE(i + 1, argv[i], rep_LISTP(argv[i]));
  }

  repv ret = 0;

  rep_GC_n_roots gc_argv;
  rep_PUSHGCN(gc_argv, argv, argc);

  while (1) {
    repv elts[argc-1];
    int elts_count = 0;
    for (int i = 1; i < argc; i++) {
      if (rep_CONSP(argv[i])) {
	elts[i-1] = rep_CAR(argv[i]);
	argv[i] = rep_CDR(argv[i]);
	elts_count++;
      }
    }
    if (elts_count == 0) {
      ret = rep_undefined_value;
      break;
    } else if (elts_count != argc - 1) {
      ret = Fsignal(Qerror, rep_LIST_1(rep_VAL(&map_invalid_lists)));
      break;
    }

    if (!rep_call_lispn(argv[0], argc - 1, elts)) {
      break;
    }

    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      break;
    }
  }

  rep_POPGCN;

  return ret;
}

DEFUN("mapcar", Fmapcar, Smapcar, (repv fun, repv list), rep_Subr2) /*
::doc:rep.data#mapcar::
mapcar FUNCTION LIST

Equivalent to (map FUNCTION LIST).
::end:: */
{
  repv argv[2] = {fun, list};
  return Fmap(2, argv);
}

DEFUN("mapc", Fmapc, Smapc, (repv fun, repv list), rep_Subr2) /*
::doc:rep.data#mapc::
mapc FUNCTION LIST

Equivalent to (for-each FUNCTION LIST).
::end:: */
{
  repv argv[2] = {fun, list};
  return Ffor_each(2, argv);
}

DEFUN("filter", Ffilter, Sfilter, (repv pred, repv list), rep_Subr2) /*
::doc:rep.data#filter::
filter PREDICATE LIST

Return a new list, consisting of the elements in LIST which the function
PREDICATE returns t when applied to; i.e. something like

(mapcar 'nconc (mapcar #'(lambda (x)
			   (when (PREDICATE x)
			     (list x)))
		       LIST))
::end:: */
{
  rep_DECLARE2(list, rep_LISTP);

  repv output = rep_nil, *ptr = &output;

  rep_GC_root gc_pred, gc_list, gc_output;
  rep_PUSHGC(gc_pred, pred);
  rep_PUSHGC(gc_list, list);
  rep_PUSHGC(gc_output, output);

  while (rep_CONSP(list)) {
    repv tem = rep_call_lisp1(pred, rep_CAR(list));
    rep_TEST_INT;
    if(tem == 0 || rep_INTERRUPTP) {
      output = 0;
      break;
    }
    if (!rep_NILP(tem)) {
      repv cell = Fcons(rep_CAR(list), rep_nil);
      *ptr = cell;
      ptr = &rep_CDR(cell);
    }
    list = rep_CDR(list);
  }

  rep_POPGC; rep_POPGC; rep_POPGC;
  return output;
}

DEFUN("member", Fmember, Smember, (repv elt, repv list), rep_Subr2) /*
::doc:rep.data#member::
member ELT LIST

If ELT is a member of list LIST then return the tail of the list starting
from the matched ELT, ie,
  (member 1 '(2 1 3))
   => (1 3)
`member' uses `equal?' to compare atoms.
::end:: */
{
  rep_DECLARE2(list, rep_LISTP);

  while (rep_CONSP(list)) {
    if (!rep_value_cmp(elt, rep_CAR(list))) {
      return list;
    }
    list = rep_CDR(list);
    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      return 0;
    }
  }

  return rep_nil;
}

DEFUN("memq", Fmemq, Smemq, (repv elt, repv list), rep_Subr2) /*
::doc:rep.data#memq::
memq ELT LIST

If ELT is a member of list LIST then return the tail of the list starting
from the matched ELT, ie,
  (memq 1 '(2 1 3))
   => (1 3)
`memq' uses `eq?' to compare atoms.
::end:: */
{
  rep_DECLARE2(list, rep_LISTP);

  while (rep_CONSP(list)) {
    if (elt == rep_CAR(list)) {
      return list;
    }
    list = rep_CDR(list);
    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      return 0;
    }
  }

  return rep_nil;
}

DEFUN("memql", Fmemql, Smemql, (repv elt, repv list), rep_Subr2) /*
::doc:rep.data#memql::
memql ELT LIST

If ELT is a member of list LIST then return the tail of the list starting
from the matched ELT. `memql' uses `eqv?' to compare list items.
::end:: */
{
  rep_DECLARE2(list, rep_LISTP);

  while (rep_CONSP(list)) {
    if (elt == rep_CAR(list)) {
      return list;
    }
    repv tem = Feql(elt, rep_CAR(list));
    if (tem && tem != rep_nil) {
      return list;
    }
    list = rep_CDR(list);
    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      return 0;
    }
  }

  return rep_nil;
}

DEFUN("delete", Fdelete, Sdelete, (repv elt, repv list), rep_Subr2) /*
::doc:rep.data#delete::
delete ELT LIST

Returns LIST with any members `equal?' to ELT destructively removed.
::end:: */
{
  rep_DECLARE2(list, rep_LISTP);

  repv *ptr = &list;
  while (rep_CONSP(*ptr)) {
    repv cell = *ptr;
    if (!rep_value_cmp(elt, rep_CAR(cell))) {
      *ptr = rep_CDR(cell);
    } else {
      ptr = &rep_CDR(cell);
    }
    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      return 0;
    }
  }

  return list;
}

DEFUN("delq", Fdelq, Sdelq, (repv elt, repv list), rep_Subr2) /*
::doc:rep.data#delq::
delq ELT LIST

Returns LIST with any members `eq?' to ELT destructively removed.
::end:: */
{
  rep_DECLARE2(list, rep_LISTP);

  repv *ptr = &list;
  while (rep_CONSP(*ptr)) {
    repv cell = *ptr;
    if (elt == rep_CAR(cell)) {
      *ptr = rep_CDR(cell);
    } else {
      ptr = &rep_CDR(cell);
    }
    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      return 0;
    }
  }

  return list;
}

DEFUN("delete-if", Fdelete_if, Sdelete_if, (repv pred, repv list), rep_Subr2) /*
::doc:rep.data#delete-if::
delete-if FUNCTION LIST

Similar to `delete' except that a predicate function, FUNCTION-NAME, is
used to decide which elements to delete (remove destructively).
`delete-if' deletes an element if FUNCTION-NAME returns non-nil when 
applied to that element, ie,
  (delete-if '(lambda (x) (= x 1)) '(1 2 3 4 1 2))
   => (2 3 4 2)
::end:: */
{
  rep_DECLARE2(list, rep_LISTP);

  rep_GC_root gc_list, gc_pred;
  rep_PUSHGC(gc_list, list);
  rep_PUSHGC(gc_pred, pred);

  repv *ptr = &list;
  while (rep_CONSP(*ptr)) {
    repv tmp = rep_call_lisp1(pred, rep_CAR(*ptr));
    rep_TEST_INT;
    if(rep_INTERRUPTP || !tmp) {
      list = 0;
      break;
    }
    if (!rep_NILP(tmp)) {
      *ptr = rep_CDR(*ptr);
    } else {
      ptr = &rep_CDR(*ptr);
    }
  }

  rep_POPGC; rep_POPGC;
  return list;
}

DEFUN("delete-if-not", Fdelete_if_not, Sdelete_if_not, (repv pred, repv list), rep_Subr2) /*
::doc:rep.data#delete-if-not::
delete-if-not FUNCTION LIST

Similar to `delete' except that a predicate function, FUNCTION-NAME, is
used to decide which elements to delete (remove destructively).
`delete-if-not' deletes an element if FUNCTION-NAME returns nil when 
applied to that element, ie,
  (delete-if-not '(lambda (x) (= x 1)) '(1 2 3 4 1 2))
   => (1 1)
::end:: */
{
  rep_DECLARE2(list, rep_LISTP);

  rep_GC_root gc_list, gc_pred;
  rep_PUSHGC(gc_list, list);
  rep_PUSHGC(gc_pred, pred);

  repv *ptr = &list;
  while (rep_CONSP(*ptr)) {
    repv tmp = rep_call_lisp1(pred, rep_CAR(*ptr));
    rep_TEST_INT;
    if (rep_INTERRUPTP || !tmp) {
      list = 0;
      break;
    }
    if (rep_NILP(tmp)) {
      *ptr = rep_CDR(*ptr);
    } else {
      ptr = &rep_CDR(*ptr);
    }
  }

  rep_POPGC; rep_POPGC;
  return list;
}

DEFUN("null?", Fnull, Snull, (repv arg), rep_Subr1) /*
::doc:rep.data#null?::
null? ARG

Returns t if ARG is nil.
::end:: */
{
  return rep_NILP(arg) ? Qt : rep_nil;
}

DEFUN("atom", Fatom, Satom, (repv arg), rep_Subr1) /*
::doc:rep.data#atom::
atom ARG

Returns t if ARG is not a cons-cell.
::end:: */
{
  return rep_CONSP(arg) ? rep_nil : Qt;
}

DEFUN("pair?", Fconsp, Sconsp, (repv arg), rep_Subr1) /*
::doc:rep.data#pair?::
pair? ARG

Returns t if ARG is a cons-cell.
::end:: */
{
  return rep_CONSP(arg) ? Qt : rep_nil;
}

DEFUN("list?", Flistp, Slistp, (repv arg), rep_Subr1) /*
::doc:rep.data#list?::
list? ARG

Returns t if ARG is a list, (either a cons-cell or nil).
::end:: */
{
  return rep_LISTP(arg) ? Qt : rep_nil;
}

void
rep_lists_init(void)
{
  repv tem = rep_push_structure("rep.data");
  rep_ADD_SUBR(Scons);
  rep_ADD_SUBR(Scar);
  rep_ADD_SUBR(Scdr);
  rep_ADD_SUBR(Slist);
  rep_ADD_SUBR(Slist_star);
  rep_ADD_SUBR(Smake_list);
  rep_ADD_SUBR(Sappend);
  rep_ADD_SUBR(Snconc);
  rep_ADD_SUBR(Srplaca);
  rep_ADD_SUBR(Srplacd);
  rep_ADD_SUBR(Sreverse);
  rep_ADD_SUBR(Snreverse);
  rep_ADD_SUBR(Sassoc);
  rep_ADD_SUBR(Sassq);
  rep_ADD_SUBR(Srassoc);
  rep_ADD_SUBR(Srassq);
  rep_ADD_SUBR(Snth);
  rep_ADD_SUBR(Snthcdr);
  rep_ADD_SUBR(Slast);
  rep_ADD_SUBR(Smap);
  rep_ADD_SUBR(Sfor_each);
  rep_ADD_SUBR(Smapcar);
  rep_ADD_SUBR(Smapc);
  rep_ADD_SUBR(Sfilter);
  rep_ADD_SUBR(Smember);
  rep_ADD_SUBR(Smemq);
  rep_ADD_SUBR(Smemql);
  rep_ADD_SUBR(Sdelete);
  rep_ADD_SUBR(Sdelq);
  rep_ADD_SUBR(Sdelete_if);
  rep_ADD_SUBR(Sdelete_if_not);
  rep_ADD_SUBR(Snull);
  rep_ADD_SUBR(Satom);
  rep_ADD_SUBR(Sconsp);
  rep_ADD_SUBR(Slistp);
  rep_pop_structure(tem);
}

void
rep_lists_kill(void)
{
  cons_block *cb = cons_block_list;

  cons_block_list = NULL;
  rep_allocated_cons = rep_used_cons = 0;

  while (cb) {
    cons_block *next = cb->next;
    rep_free(cb);
    cb = next;
  }
}
