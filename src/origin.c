/* origin.c -- tracking location from which lists were read

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

typedef struct origin_item origin_item;

struct origin_item {
  origin_item *next;
  repv form;
  repv file;
  int line;
};

#define BLOCK_SIZE (4084 / sizeof (struct origin_item))

typedef struct origin_block origin_block;

struct origin_block {
  origin_block *next;
  struct origin_item items[BLOCK_SIZE];
};

static origin_item *free_list;
static origin_block *block_list;

bool rep_record_origins;

#define HASH_SIZE 1024
#define HASH(x) (((x) >> 3) % HASH_SIZE)

static origin_item *buckets[HASH_SIZE];
static size_t item_count;

static void
new_item_block (void)
{
  origin_block *b = rep_alloc (sizeof (origin_block));

  int i;
  for (i = 0; i < (BLOCK_SIZE - 1); i++) {
    b->items[i].next = &(b->items[i+1]);
  }

  b->items[i].next = free_list;
  free_list = &(b->items[0]);

  b->next = block_list;
  block_list = b;
}

void
rep_record_origin(repv form, repv stream, int start_line)
{
  if (!rep_record_origins
      || !rep_CONSP(form)
      || !rep_FILEP(stream)
      || (rep_FILE(stream)->car & rep_LFF_BOGUS_LINE_NUMBER) != 0)
  {
    /* nothing to record here */
    return;
  }

  if (free_list == 0) {
    new_item_block();
  }

  origin_item *item = free_list;
  free_list = item->next;

  item->form = form;
  item->file = rep_FILE(stream)->name;
  item->line = start_line > 0 ? start_line : rep_FILE(stream)->line_number;

  unsigned int h = HASH(form);

  item->next = buckets[h];
  buckets[h] = item;

  item_count++;
}

DEFUN("call-with-lexical-origins", Fcall_with_lexical_origins,
       Scall_with_lexical_origins, (repv thunk), rep_Subr1)
{
  bool old_record_origins = rep_record_origins;
  rep_record_origins = true;

  repv ret = rep_call_lisp0(thunk);

  rep_record_origins = old_record_origins;

  return ret;
}

DEFUN("lexical-origin", Flexical_origin,
       Slexical_origin, (repv form), rep_Subr1)
{
  if (rep_CLOSUREP(form)) {
    form = rep_CLOSURE(form)->fun;
  }

  if (!rep_CONSP(form)) {
    return rep_nil;
  }

  unsigned int h = HASH(form);

  for (origin_item *item = buckets[h]; item != 0; item = item->next) {
    if (item->form == form) {
      return Fcons(item->file, rep_MAKE_INT(item->line));
    }
  }

  /* no direct hit, scan into the list */

  while (rep_CONSP(form)) {
    repv out = Flexical_origin(rep_CAR(form));
    if (out != rep_nil) {
      return out;
    }
    form = rep_CDR(form);
  }

  return rep_nil;
}

void
rep_mark_origins(void)
{
  if (item_count == 0) {
    return;
  }

  for (int i = 0; i < HASH_SIZE; i++) {
    for (origin_item *item = buckets[i]; item != 0; item = item->next) {
      rep_MARKVAL(item->file);
    }
  }
}

void
rep_scan_origins(void)
{
  if (item_count == 0) {
    return;
  }

  for (int i = 0; i < HASH_SIZE; i++) {
    origin_item **ptr = &buckets[i];
    origin_item *item = *ptr;
    for (ptr = &buckets[i]; (item = *ptr);) {
      if (rep_GC_MARKEDP(item->form)) {
	ptr = &item->next;
      } else {
	*ptr = item->next;
	item->next = free_list;
	free_list = item;
	item_count--;
      }
    }
  }
}

void
rep_origin_init(void)
{
  repv tem = rep_push_structure("rep.lang.debug");
  rep_ADD_SUBR(Scall_with_lexical_origins);
  rep_ADD_SUBR(Slexical_origin);
  rep_pop_structure(tem);
}
