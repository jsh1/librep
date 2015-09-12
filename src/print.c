/* print.c -- object serialization and display

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

#include <inttypes.h>

DEFSYM(print_escape, "*print-escape*");
DEFSYM(print_length, "*print-length*");
DEFSYM(print_level, "*print-level*");
DEFSYM(newlines, "newlines");

/* ::doc:*print-escape*::
Defines which control characters `print' should quote. Acceptable values
are:
	nil		Only escape double-quote and backslash
	newlines	Escape double-quote, backslash, newline,
			 TAB, and formfeed.
	t		Escape all control codes(characters with a
			 value less than 32), and all characters with
			 a value greater than 126.
::end::
::doc:*print-length*::
The maximum number of list elements to print before abbreviating.
::end::
::doc:*print-level*::
The number of list levels to descend when printing before abbreviating.
::end:: */

void
rep_lisp_prin(repv strm, repv obj)
{
  static int print_level = 0;

  rep_TEST_INT_LOOP_COUNTER;

  switch (rep_TYPE(obj)) {
    char tbuf[64];

  case rep_Cons: {
    repv tem = Fsymbol_value(Qprint_level, Qt);
    if (tem && rep_INTP(tem) && print_level >= rep_INT(tem)) {
      rep_stream_puts(strm, "...", 3, false);
      return;
    }
    print_level++;
    rep_stream_putc(strm, '(');
    tem = Fsymbol_value(Qprint_length, Qt);
    int print_length = 0;
    while (rep_CONSP(rep_CDR(obj))) {
      if (tem && rep_INTP(tem) && print_length >= rep_INT(tem)) {
	rep_stream_puts(strm, "...", 3, false);
	goto cons_out;
      }
      rep_print_val(strm, rep_CAR(obj));
      obj = rep_CDR(obj);
      rep_stream_putc(strm, ' ');
      rep_TEST_INT;
      if (rep_INTERRUPTP) {
	goto cons_out;
      }
      print_length++;
    }
    if (tem && rep_INTP(tem) && print_length >= rep_INT(tem)) {
      rep_stream_puts(strm, "...", 3, false);
    } else {
      rep_print_val(strm, rep_CAR(obj));
      if (!rep_NILP(rep_CDR(obj))) {
	rep_stream_puts(strm, " . ", -1, false);
	rep_print_val(strm, rep_CDR(obj));
      }
    }
  cons_out:
    rep_stream_putc(strm, ')');
    print_level--;
    break; }

  case rep_Bytecode:
    rep_stream_putc(strm, '#');
    /* fall through */

  case rep_Vector: {
    int len = rep_VECT_LEN(obj);
    rep_stream_putc(strm, '[');
    for (int j = 0; j < len; j++) {
      if (rep_VECT(obj)->array[j]) {
	rep_print_val(strm, rep_VECT(obj)->array[j]);
      } else {
	rep_stream_puts(strm, "#<void>", -1, false);
      }
      if (j != (len - 1)) {
	rep_stream_putc(strm, ' ');
      }
    }
    rep_stream_putc(strm, ']');
    break; }

  case rep_Subr:
#ifdef HAVE_SNPRINTF
    snprintf(tbuf, sizeof(tbuf), "#<subr %s>", rep_STR(rep_XSUBR(obj)->name));
#else
    sprintf(tbuf, "#<subr %lx>", (long long)obj);
#endif
    rep_stream_puts(strm, tbuf, -1, false);
    break;

  case rep_SF:
#ifdef HAVE_SNPRINTF
    snprintf(tbuf, sizeof(tbuf),
	     "#<special-form %s>", rep_STR(rep_XSUBR(obj)->name));
#else
    sprintf(tbuf, "#<special-form %lx>", (long long)obj);
#endif
    rep_stream_puts(strm, tbuf, -1, false);
    break;

  case rep_Closure:
    rep_stream_puts(strm, "#<closure ", -1, false);
    if (rep_STRINGP(rep_CLOSURE(obj)->name)) {
      rep_stream_puts(strm, rep_STR(rep_CLOSURE(obj)->name), -1, false);
    } else {
#ifdef HAVE_SNPRINTF
      snprintf(tbuf, sizeof(tbuf), "%" PRIxPTR, obj);
#else
      sprintf(tbuf, "%" PRIxPTR, obj);
#endif
      rep_stream_puts(strm, tbuf, -1, false);
    }
    rep_stream_putc(strm, '>');
    break;

  default:
    rep_stream_puts(strm, "#<unknown object type>", -1, false);
  }
}

void
rep_princ_val(repv strm, repv val)
{
  if (val) {
    rep_type *t = rep_get_data_type(rep_TYPE(val));

    rep_GC_root gc_strm, gc_val;
    rep_PUSHGC(gc_strm, strm);
    rep_PUSHGC(gc_val, val);

    t->princ(strm, val);

    rep_POPGC; rep_POPGC;
  }
}

void
rep_print_val(repv strm, repv val)
{
  if (val) {
    rep_type *t = rep_get_data_type(rep_TYPE(val));

    rep_GC_root gc_strm, gc_val;
    rep_PUSHGC(gc_strm, strm);
    rep_PUSHGC(gc_val, val);

    t->print(strm, val);

    rep_POPGC; rep_POPGC;
  }
}

void
rep_print_init(void)
{
  rep_INTERN_SPECIAL(print_escape); 
  rep_INTERN_SPECIAL(print_length);
  rep_INTERN_SPECIAL(print_level);
  Fset(Qprint_escape, rep_nil);
  Fset(Qprint_length, rep_nil);
  Fset(Qprint_level, rep_nil);
  rep_INTERN(newlines);
}
