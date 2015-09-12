/* types.c -- type functions

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

#define TYPE_HASH_SIZE 32
#define TYPE_HASH(type) (((type) >> 1) & (TYPE_HASH_SIZE-1))

static rep_type *data_types[TYPE_HASH_SIZE];

static int
ptr_cmp(repv v1, repv v2)
{
  if (rep_TYPE(v1) == rep_TYPE(v2)) {
    return !(rep_PTR(v1) == rep_PTR(v2));
  } else {
    return 1;
  }
}

int
rep_type_cmp(repv val1, repv val2)
{
    return !(rep_TYPE(val1) == rep_TYPE(val2));
}

repv
rep_define_type(rep_type *t)
{
  if (!t->initialized) {
    if (t->car == 0) {
      static int next_free_type = 0;
      assert(next_free_type != 256);
      t->car = ((next_free_type++ << rep_CELL16_TYPE_SHIFT)
		 | rep_CELL_IS_8 | rep_CELL_IS_16);
    }

    if (!t->compare) {
      t->compare = ptr_cmp;
    }

    if (!t->princ && t->print) {
      t->princ = t->print;
    }

    unsigned int hash = TYPE_HASH(t->car);

    t->next = data_types[hash];
    data_types[hash] = t;

    t->initialized = true;
  }

  return t->car;
}

rep_type *
rep_get_data_type(repv car)
{
  for (rep_type *t = data_types[TYPE_HASH(car)]; t; t = t->next) {
    if (t->car == car) {
      return t;
    }
  }
  return NULL;
}

void
rep_mark_types(void)
{
  for (int i = 0; i < TYPE_HASH_SIZE; i++) {
    for (rep_type *t = data_types[i]; t; t = t->next) {
      if (t->mark_type) {
	t->mark_type();
      }
    }
  }
}

void
rep_sweep_types(void)
{
  for (int i = 0; i < TYPE_HASH_SIZE; i++) {
    for (rep_type *t = data_types[i]; t; t = t->next) {
      if (t->sweep) {
	t->sweep();
      }
    }
  }
}

void
rep_types_after_gc(void)
{
  for (int i = 0; i < TYPE_HASH_SIZE; i++) {
    for (rep_type *t = data_types[i]; t; t = t->next) {
      if (t->after_gc) {
	t->after_gc();
      }
    }
  }
}

void
rep_types_init(void)
{
  static rep_type types[] = {{
    .car = rep_Cons,
    .name = "cons",
    .compare = rep_cons_cmp,
    .print = rep_lisp_prin,
    .sweep = rep_cons_sweep,
  }, {
    .car = rep_Vector,
    .name = "vector",
    .compare = rep_vector_cmp,
    .print = rep_lisp_prin,
    .sweep = rep_vector_sweep,
  }, {
    .car = rep_String,
    .name = "string",
    .compare = rep_string_cmp,
    .princ = rep_string_princ,
    .print = rep_string_print,
    .sweep = rep_string_sweep,
  }, {
    .car = rep_Bytecode,
    .name = "bytecode",
    .compare = rep_vector_cmp,
    .print = rep_lisp_prin,
  }, {
    .car = rep_SF,
    .name = "special-form",
    .print = rep_lisp_prin,
  }, {
    .car = rep_Subr,
    .name = "subr",
    .print = rep_lisp_prin,
  }};

  for (size_t i = 0; i < sizeof(types) / sizeof(types[0]); i++) {
    rep_define_type(&types[i]);
  }
}
