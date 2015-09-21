/* symbols.c -- symbol management

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

#include <string.h>
#include <ctype.h>
#include <stdlib.h>

/* The number of hash buckets in each rep_obarray, this is a prime number. */

#define rep_OBSIZE		509
#define rep_KEY_OBSIZE		127

/* Global symbol tables.  */

repv rep_obarray, rep_keyword_obarray;

DEFSYM(t, "t");

/* Void value. */

/* Value that marks end of an obarray hash-bucket list. It can be any
   Lisp value which isn't a symbol.  */

#define OB_NIL rep_void

/* #f, #t and #undefined pseudo-symbols. */

repv rep_scm_t, rep_scm_f, rep_undefined_value;

static uintptr_t
string_hash(const char *name, size_t len)
{
  uintptr_t value = 5381;

  const uint8_t *str = (const uint8_t *)name;
  for (size_t i = 0; i < len; i++) {
    value = (value * 33) + str[i];
  }

  return value;
}

static inline uintptr_t
symbol_name_hash(repv name)
{
  return string_hash(rep_STR(name), rep_STRING_LEN(name));
}

static int
symbol_cmp(repv v1, repv v2)
{
  if (v1 == v2) {
    return 0;
  }

  if (rep_TYPE(v1) != rep_TYPE(v2)) {
    return 1;
  }

  return rep_value_cmp(rep_SYM(v1)->name, rep_SYM(v2)->name);
}

static void
symbol_princ(repv strm, repv obj)
{
  rep_stream_puts(strm, rep_PTR(rep_SYM(obj)->name), -1, true);
}

static void
symbol_print(repv strm, repv obj)
{
  if (rep_SYMBOL_LITERAL_P(obj)) {
    symbol_princ(strm, obj);
    return;
  }

  /* output a maximum of 2n chars for a symbol name of length n */

  size_t max_size = rep_STRING_LEN(rep_SYM(obj)->name) * 2;

  char *buf = rep_stack_alloc(char, max_size);
  if (!buf) {
    rep_mem_error();
    return;
  }

  char *out = buf;

  /* Decide if it might be confused for a number. */

  const char *s = rep_STR(rep_SYM(obj)->name);
  bool seen_digit = false;

  switch (*s++) {
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    seen_digit = true;
    /* fall through */

  case '-': case '+': case '.':
  pass1:
    switch (*s++) {
    case 0:
      if (seen_digit) {
	*out++ = '\\';
      }
      break;

    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      seen_digit = true;
      /* fall through */

    case '/': case '.':
      goto pass1;
    }
  }

  s = rep_STR(rep_SYM(obj)->name);

  while (1) {
    char c = *s++;
    switch (c) {
    case 0:
      goto out;

    case ' ': case '\t': case '\n': case '\f':
    case '(': case ')': case '[': case ']':
    case '\'': case '"': case ';': case '\\':
    case '|': case ',': case '`':
      *out++ = '\\';
      break;

    case '#':
      if (!(rep_KEYWORDP(obj) && s-1 == rep_STR(rep_SYM(obj)->name))) {
	*out++ = '\\';
      }
      break;

    default:
      if (rep_iscntrl(c)) {
	*out++ = '\\';
      }
      break;
    }
    *out++ = c;
  }

out:
  rep_stream_puts(strm, buf, out - buf, false);

  rep_stack_free(char, max_size, buf);
}

void
rep_intern_static(repv *symp, repv name)
{
  repv symbol = Fintern(name, rep_nil);
  *symp = symbol;
  rep_mark_static(symp);
}

DEFUN("make-symbol", Fmake_symbol, Smake_symbol, (repv name), rep_Subr1) /*
::doc:rep.lang.symbols#make-symbol::
make-symbol NAME

Returns a new, uninterned, symbol with print-name NAME. It's value and
function definition are both void and it has a nil property-list.
::end:: */
{
  /* Create an immutable copy of the name (if necessary). Prevents its
     hash value changing after it's been interned. */

  name = Fstring_to_immutable_string(name);
  if (!name) {
    return 0;
  }

  return rep_make_tuple(rep_Symbol, 0, name);
}

DEFUN("gensym", Fgensym, Sgensym, (void), rep_Subr0) /*
::doc:rep.lang.symbols#gensym::
gensym

Returns a new (non-interned) symbol with a unique print name.
::end:: */
{
  static int counter;

  char buf[32];

  counter++;
#ifdef HAVE_SNPRINTF
  snprintf(buf, sizeof(buf), "G%04d", counter);
#else
  sprintf(buf, "G%04d", counter);
#endif

  repv name = rep_string_copy(buf);
  Fmake_string_immutable(name);		/* avoids another copy */

  return Fmake_symbol(name);
}

DEFUN("symbol-name", Fsymbol_name, Ssymbol_name, (repv sym), rep_Subr1) /*
::doc:rep.lang.symbols#symbol-name::
symbol-name SYMBOL

Returns the print-name of SYMBOL.
::end:: */
{
  rep_DECLARE1(sym, rep_SYMBOLP);

  return rep_SYM(sym)->name;
}

DEFUN("symbol?", Fsymbolp, Ssymbolp, (repv sym), rep_Subr1) /*
::doc:rep.lang.symbols#symbol?::
symbol? ARG

Returns t if ARG is a symbol.
::end:: */
{
  return rep_SYMBOLP(sym) ? Qt : rep_nil;
}

DEFUN("make-keyword", Fmake_keyword, Smake_keyword, (repv in), rep_Subr1) /*
::doc:rep.lang.symbols#make-keyword::
make-keyword SYMBOL

Return the keyword symbol that should be used in argument lists to
provide the mark the value of the argument called SYMBOL. An error is
signalled if SYMBOL is itself a keyword.
::end:: */
{
  rep_DECLARE(1, in, rep_SYMBOLP(in) && !rep_KEYWORDP(in));

  repv name = rep_SYM(in)->name;
  int name_len = rep_STRING_LEN(name);

  repv str = rep_allocate_string(name_len + 3);
  rep_MUTABLE_STR(str)[0] = '#';
  rep_MUTABLE_STR(str)[1] = ':';
  memcpy(rep_MUTABLE_STR(str) + 2, rep_STR(name), name_len);
  rep_MUTABLE_STR(str)[name_len+2] = 0;
  Fmake_string_immutable(str);

  repv key = Fintern(str, rep_keyword_obarray);
  rep_SYM(key)->car |= rep_SF_KEYWORD;
  return key;
}

DEFUN("keyword?", Fkeywordp, Skeywordp, (repv arg), rep_Subr1) /*
::doc:rep.lang.symbols#keyword?::
keyword? ARG

Return true if ARG is a keyword symbol.
::end:: */
{
  return rep_KEYWORDP(arg) ? Qt : rep_nil;
}

DEFUN("make-obarray", Fmake_obarray, Smake_obarray, (repv size), rep_Subr1) /*
::doc:rep.lang.symbols#make-obarray::
make-obarray SIZE

Creates a new structure for storing symbols in. This is basically a
vector with a few slight differences (all elements initialised to a
special value).
::end:: */
{
  rep_DECLARE1(size, rep_INTP);

  return Fmake_vector(size, OB_NIL);
}

DEFUN("find-symbol", Ffind_symbol, Sfind_symbol,
      (repv name, repv ob), rep_Subr2) /*
::doc:rep.lang.symbols#find-symbol::
find-symbol NAME [OBARRAY]

Returns the symbol with print-name NAME, found by searching OBARRAY (or
the default `rep_obarray' if nil), or nil if no such symbol exists.
::end:: */
{
  rep_DECLARE1(name, rep_STRINGP);

  if (!rep_VECTORP(ob)) {
    ob = rep_obarray;
  }

  uintptr_t vsize = rep_VECTOR_LEN(ob);
  if (vsize == 0) {
    return rep_signal_arg_error(ob, 2);
  }
                                
  uintptr_t h = symbol_name_hash(name) % vsize;

  repv sym = rep_VECT(ob)->array[h];

  while (rep_SYMBOLP(sym)) {
    if (strcmp(rep_STR(name), rep_STR(rep_SYM(sym)->name)) == 0) {
      return sym;
    }
    sym = rep_SYM(sym)->next;
  }

  return rep_nil;
}

DEFSTRING(already_interned, "Symbol is already interned");

DEFUN("intern-symbol", Fintern_symbol,
      Sintern_symbol, (repv sym, repv ob), rep_Subr2) /*
::doc:rep.lang.symbols#intern-symbol::
intern-symbol SYMBOL [OBARRAY]

Stores SYMBOL in OBARRAY (or the default). If SYMBOL has already been
interned somewhere an error is signalled.
::end:: */
{
  rep_DECLARE1(sym, rep_SYMBOLP);

  if (rep_SYM(sym)->next) {
    Fsignal(Qerror, rep_list_2(rep_VAL(&already_interned), sym));
    return 0;
  }

  if (!rep_VECTORP(ob)) {
    ob = rep_obarray;
  }

  uintptr_t vsize = rep_VECTOR_LEN(ob);
  if (vsize == 0) {
    return rep_signal_arg_error(ob, 2);
  }

  uintptr_t h = symbol_name_hash(rep_SYM(sym)->name) % vsize;

  rep_SYM(sym)->next = rep_VECT(ob)->array[h];
  rep_VECT(ob)->array[h] = sym;

  return sym;
}

DEFUN("intern", Fintern, Sintern, (repv name, repv ob), rep_Subr2) /*
::doc:rep.lang.symbols#intern::
intern NAME [OBARRAY]

If a symbol with print-name exists in OBARRAY (or the default) return
it. Else use `(make-symbol NAME)' to create a new symbol, intern that
into the OBARRAY, then return it.
::end:: */
{
  rep_DECLARE1(name, rep_STRINGP);

  repv sym = Ffind_symbol(name, ob);

  if (sym != rep_nil) {
    return sym;
  }

  sym = Fmake_symbol(name);
  if (!sym) {
    return 0;
  }

  return Fintern_symbol(sym, ob);
}

repv
rep_intern_symbol(const char *str, size_t len, repv obarray)
{
  /* Inlined Ffind_symbol() to avoid string allocation. */

  uintptr_t vsize = rep_VECTOR_LEN(obarray);
  uintptr_t h = string_hash(str, len) % vsize;

  for (repv sym = rep_VECT(obarray)->array[h];
       rep_SYMBOLP(sym); sym = rep_SYM(sym)->next) {
    repv name = rep_SYM(sym)->name;
    if (rep_STRING_LEN(name) == len && memcmp(rep_STR(name), str, len) == 0) {
      return sym;
    }
  }

  repv name = rep_string_copy_n(str, len);
  rep_STRING(name)->car |= rep_STRING_IMMUTABLE;

  repv sym = Fmake_symbol(name);
  if (!sym) {
    return 0;
  }

  return Fintern_symbol(sym, obarray);
}

DEFUN("unintern", Funintern, Sunintern, (repv sym, repv ob), rep_Subr2) /*
::doc:rep.lang.symbols#unintern::
unintern SYMBOL [OBARRAY]

Removes SYMBOL from OBARRAY (or the default). Use this with caution.
::end:: */
{
  rep_DECLARE1(sym, rep_SYMBOLP);

  if (!rep_VECTORP(ob)) {
    ob = rep_obarray;
  }

  uintptr_t vsize = rep_VECTOR_LEN(ob);
  if (vsize == 0) {
    return rep_signal_arg_error(ob, 2);
  }
  
  uintptr_t h = symbol_name_hash(rep_SYM(sym)->name) % vsize;

  repv list = rep_VECT(ob)->array[h];
  rep_VECT(ob)->array[h] = OB_NIL;

  while (rep_SYMBOLP(list)) {
    repv next = rep_SYM(list)->next;
    if (list != sym) {
      rep_SYM(list)->next = rep_VECT(ob)->array[h];
      rep_VECT(ob)->array[h] = rep_VAL(list);
    }
    list = next;
  }

  rep_SYM(sym)->next = 0;
  return sym;
}

DEFUN("apropos", Fapropos, Sapropos,
      (repv re, repv pred, repv ob), rep_Subr3) /*
::doc:rep.lang.symbols#apropos::
apropos REGEXP [PREDICATE] [OBARRAY]

Returns a list of symbols from OBARRAY (or the default) whose
print-name matches the regular-expression REGEXP. If PREDICATE is given
and non-nil, each symbol which matches is applied to the function
PREDICATE, if the value is non-nil it is considered a match.
::end:: */
{
  rep_DECLARE1(re, rep_STRINGP);

  if (!rep_VECTORP(ob)) {
    ob = rep_obarray;
  }

  rep_regexp *prog = rep_regcomp(rep_STR(re));
  if (!prog) {
    return 0;
  }

  repv ret = rep_nil;
  int len = rep_VECTOR_LEN(ob);

  rep_GC_root gc_ret, gc_ob, gc_pred;
  rep_PUSHGC(gc_ret, ret);
  rep_PUSHGC(gc_ob, ob);
  rep_PUSHGC(gc_pred, pred);

  for (int i = 0; i < len; i++) {
    for (repv sym = rep_VECT(ob)->array[i];
	 rep_SYMBOLP(sym); sym = rep_SYM(sym)->next)
    {
      if (rep_regexec(prog, rep_STR(rep_SYM(sym)->name))) {
	if (pred && pred != rep_nil) {
	  repv tmp = rep_apply(pred, rep_LIST_1(sym));
	  if (!tmp || tmp == rep_nil) {
	    continue;
	  }
	}
	ret = Fcons(sym, ret);
      }
    }
  }
  
  rep_POPGC; rep_POPGC; rep_POPGC;

  free(prog);

  return ret;
}

DEFUN_INT("trace", Ftrace, Strace, (repv sym),
	  rep_Subr1, "aFunction to trace") /*
::doc:rep.lang.debug#trace::
trace SYMBOL

Flag that whenever SYMBOL is evaluated (as a variable or a function)
the debugger is entered.
::end:: */
{
  rep_DECLARE1(sym, rep_SYMBOLP);

  rep_SYM(sym)->car |= rep_SF_DEBUG;
  return(sym);
}

DEFUN_INT("untrace", Funtrace, Suntrace,
	  (repv sym), rep_Subr1, "aFunction to untrace") /*
::doc:rep.lang.debug#untrace::
untrace SYMBOL

Cancel the effect of (trace SYMBOL).
::end:: */
{
  rep_DECLARE1(sym, rep_SYMBOLP);

  rep_SYM(sym)->car &= ~rep_SF_DEBUG;
  return(sym);
}

void
rep_obarray_init(void)
{
  static rep_type symbol = {
    .car = rep_Symbol,
    .name = "symbol",
    .compare = symbol_cmp,
    .princ = symbol_princ,
    .print = symbol_print,
    // marked inline by rep_mark_value()
  };

  rep_define_type(&symbol);

  rep_obarray = Fmake_obarray(rep_MAKE_INT(rep_OBSIZE));
  rep_keyword_obarray = Fmake_obarray(rep_MAKE_INT(rep_KEY_OBSIZE));

  rep_mark_static(&rep_obarray);
  rep_mark_static(&rep_keyword_obarray);
}

void
rep_symbols_init(void)
{
  repv tem;
  
  DEFSTRING(hash_f, "#f");
  DEFSTRING(hash_t, "#t");
  DEFSTRING(hash_undefined, "#undefined");
  
  rep_pre_datums_init();		/* initializes rep_nil */
  rep_INTERN(t);
  rep_pre_structures_init();
  
  rep_scm_f = Fmake_symbol(rep_VAL(&hash_f));
  rep_scm_t = Fmake_symbol(rep_VAL(&hash_t));
  rep_undefined_value = Fmake_symbol(rep_VAL(&hash_undefined));
  rep_SYM(rep_scm_f)->car |= rep_SF_LITERAL;
  rep_SYM(rep_scm_t)->car |= rep_SF_LITERAL;
  rep_SYM(rep_undefined_value)->car |= rep_SF_LITERAL;
  rep_mark_static(&rep_scm_f);
  rep_mark_static(&rep_scm_t);
  rep_mark_static(&rep_undefined_value);
  
  tem = rep_push_structure("rep.lang.symbols");
  rep_ADD_SUBR(Smake_symbol);
  rep_ADD_SUBR(Sgensym);
  rep_ADD_SUBR(Ssymbol_name);
  rep_ADD_SUBR(Ssymbolp);
  rep_ADD_SUBR(Smake_keyword);
  rep_ADD_SUBR(Skeywordp);
  rep_ADD_SUBR(Smake_obarray);
  rep_ADD_SUBR(Sfind_symbol);
  rep_ADD_SUBR(Sintern_symbol);
  rep_ADD_SUBR(Sintern);
  rep_ADD_SUBR(Sunintern);
  rep_ADD_SUBR(Sapropos);
  rep_pop_structure(tem);
  
  tem = rep_push_structure("rep.lang.debug");
  rep_ADD_SUBR_INT(Strace);
  rep_ADD_SUBR_INT(Suntrace);
  rep_pop_structure(tem);
}
