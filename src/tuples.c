/* tuples.c -- management of `tuples' (car and two values)

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

#define TUPLES_PER_BLOCK 680 /* ~8k */

typedef struct tuple_block_struct tuple_block;

struct tuple_block_struct {
  tuple_block *next;
  rep_ALIGN_CELL(rep_tuple tuples[TUPLES_PER_BLOCK]);
};

static tuple_block *tuple_block_list;
static rep_tuple *tuple_free_list;

int rep_allocated_tuples, rep_used_tuples;

static rep_tuple *
refill_free_list(void)
{
  tuple_block *b = rep_alloc(sizeof(tuple_block));
  rep_allocated_tuples += TUPLES_PER_BLOCK;

  b->next = tuple_block_list;
  tuple_block_list = b;

  for (int i = 1; i < TUPLES_PER_BLOCK - 1; i++) {
    b->tuples[i].car = 0;
    b->tuples[i].a = rep_VAL(&b->tuples[i + 1]);
  }
  b->tuples[TUPLES_PER_BLOCK - 1].car = 0;
  b->tuples[TUPLES_PER_BLOCK - 1].a = rep_VAL(tuple_free_list);

  tuple_free_list = &b->tuples[1];

  return &b->tuples[0];
}

repv
rep_make_tuple(repv car, repv a, repv b)
{
  rep_tuple *t = tuple_free_list;

  if (t) {
    tuple_free_list = rep_TUPLE(t->a);
  } else {
    t = refill_free_list();
  }

  rep_used_tuples++;
  rep_data_after_gc += sizeof(rep_tuple);

  t->car = car;
  t->a = a;
  t->b = b;

  return rep_VAL(t);
}

void
rep_mark_tuple(repv t)
{
  rep_MARKVAL(rep_TUPLE(t)->a);
  rep_MARKVAL(rep_TUPLE(t)->b);
}

void
rep_sweep_tuples(void)
{
  rep_tuple *free_list = 0;
  int used = 0;

  for (tuple_block *b = tuple_block_list; b != 0; b = b->next) {
    for (int i = 0; i < TUPLES_PER_BLOCK; i++) {
      rep_tuple *ptr = &b->tuples[i];
      if (!rep_GC_CELL_MARKEDP(rep_VAL(ptr))) {
	ptr->a = rep_VAL(free_list);
	free_list = ptr;
      } else {
	rep_GC_CLR_CELL(rep_VAL(ptr));
	used++;
      }
    }
  }

  tuple_free_list = free_list;
  rep_used_tuples = used;
}

void
rep_tuples_kill(void)
{
  tuple_block *b = tuple_block_list;
  while (b) {
    tuple_block *next = b->next;
    rep_free(b);
    b = next;
  }

  tuple_block_list = NULL;
  tuple_free_list = NULL;

  rep_allocated_tuples = 0;
  rep_used_tuples = 0;
}
