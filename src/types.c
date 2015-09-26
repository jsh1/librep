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

static repv
apply_error(repv fun, int argc, repv *argv)
{
  return Fsignal(Qinvalid_function, rep_list_1(fun));
}

static int
getc_error(repv obj)
{
  Fsignal(Qinvalid_stream, rep_list_1(obj));
  return EOF;
}

static int
ungetc_error(repv obj, int c)
{
  Fsignal(Qinvalid_stream, rep_list_1(obj));
  return EOF;
}

static int
putc_error(repv obj, int c)
{
  Fsignal(Qinvalid_stream, rep_list_1(obj));
  return EOF;
}

static intptr_t
puts_error(repv obj, const void *data, intptr_t length, bool lisp_obj_p)
{
  Fsignal(Qinvalid_stream, rep_list_1(obj));
  return -1;
}

repv
rep_define_type(rep_type *t)
{
  if (!(t->flags & rep_TYPE_INITIALIZED)) {
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

    if (!t->apply) {
      t->apply = apply_error;
    } else {
      t->flags |= rep_TYPE_HAS_APPLY;
    }

    if (!t->getc || !t->ungetc) {
      assert(!t->getc && !t->ungetc);
      t->getc = getc_error;
      t->ungetc = ungetc_error;
    } else {
      t->flags |= rep_TYPE_INPUT_STREAM;
    }

    if (!t->putc || !t->puts) {
      assert(!t->puts && !t->putc);
      t->putc = putc_error;
      t->puts = puts_error;
    } else {
      t->flags |= rep_TYPE_OUTPUT_STREAM;
    }

    unsigned int hash = TYPE_HASH(t->car);

    t->next = data_types[hash];
    data_types[hash] = t;

    t->flags |= rep_TYPE_INITIALIZED;
  }

  return t->car;
}

const rep_type *
rep_get_type(repv car)
{
  for (rep_type *t = data_types[TYPE_HASH(car)]; t; t = t->next) {
    if (t->car == car) {
      return t;
    }
  }
  return NULL;
}

const rep_type *
rep_value_type(repv value)
{
  return rep_get_type(rep_TYPE(value));
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
