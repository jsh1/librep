/* tables.c -- hash tables

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

/* notes:

   The api of this module (except for make-table) was mostly borrowed
   from Scheme48. The implementation is all my own fault.. */

#include "repint.h"

#include <string.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

typedef struct node_struct node;
typedef struct table_struct table;

struct node_struct {
  node *next;
  repv key, value;
  uintptr_t hash;
};

struct table_struct {
  repv car;
  table *next;
  int total_buckets, total_nodes;
  node **buckets;
  repv hash_fun;
  repv compare_fun;
  repv guardian;			/* non-null if a weak table */
};

#define TABLEP(v) rep_CELL16_TYPEP(v, table_type())
#define TABLE(v)  ((table *) rep_PTR(v))

static table *all_tables;

/* Ensure X is +ve and in an int. */

#define TRUNC(x) (((x) << (rep_VALUE_INT_SHIFT+1)) >> (rep_VALUE_INT_SHIFT+1))

static void
table_print(repv stream, repv arg)
{
  rep_stream_puts(stream, "#<table ", -1, false);
  rep_princ_val(stream, TABLE(arg)->hash_fun);
  rep_stream_putc(stream, ' ');
  rep_princ_val(stream, TABLE(arg)->compare_fun);
  rep_stream_putc(stream, '>');
}

static void
table_mark(repv val)
{
  for (int i = 0; i < TABLE(val)->total_buckets; i++) {
    for (node *n = TABLE(val)->buckets[i]; n; n = n->next) {
      if (!TABLE(val)->guardian) {
	rep_MARKVAL(n->key);
      }
      rep_MARKVAL(n->value);
    }
  }

  rep_MARKVAL(TABLE(val)->hash_fun);
  rep_MARKVAL(TABLE(val)->compare_fun);
  rep_MARKVAL(TABLE(val)->guardian);
}

static void
free_table(table *x)
{
  for (int i = 0; i < x->total_buckets; i++) {
    node *n = x->buckets[i];
    while (n) {
      node *next = n->next;
      rep_free(n);
      n = next;
    }
  }

  if (x->total_buckets > 0) {
    rep_free(x->buckets);
  }
  rep_free(x);
}

static void
table_sweep(void)
{
  table *ptr = all_tables;
  all_tables = 0;

  while (ptr) {
    table *next = ptr->next;
    if (!rep_GC_CELL_MARKEDP(rep_VAL(ptr))) {
      free_table(ptr);
    } else {
      rep_GC_CLR_CELL(rep_VAL(ptr));
      ptr->next = all_tables;
      all_tables = ptr;
    }
    ptr = next;
  }
}

static void
table_after_gc(void)
{
  for (table *ptr = all_tables; ptr != 0; ptr = ptr->next) {
    if (ptr->guardian) {
      repv key;
      while ((key = Fprimitive_guardian_pop(ptr->guardian)) != rep_nil) {
	rep_GC_root gc_key;
	rep_PUSHGC(gc_key, key);
	Ftable_unset(rep_VAL(ptr), key);
	rep_POPGC;
      }
    }
  }
}

static repv
table_type(void)
{
  static repv type;

  if (!type) {
    static rep_type table = {
      .name = "table",
      .print = table_print,
      .mark = table_mark,
      .sweep = table_sweep,
      .after_gc = table_after_gc,
    };

    type = rep_define_type(&table);
  }

  return type;
}

static inline repv
hash_string(register char *ptr)
{
  uintptr_t value = 5381;

  while (*ptr) {
    value = (value * 33) + *ptr++;
  }

  return rep_MAKE_INT(TRUNC(value));
}

DEFUN("string-hash", Fstring_hash, Sstring_hash, (repv string), rep_Subr1) /*
::doc:rep.data.tables#string-hash::
string-hash STRING

Return a positive fixnum somehow related to the contents of STRING,
such that (string= X Y) implies (= (string-hash X) (string-hash Y)).
::end:: */
{
  rep_DECLARE1(string, rep_STRINGP);

  return hash_string(rep_STR(string));
}

DEFUN("symbol-hash", Fsymbol_hash, Ssymbol_hash, (repv sym), rep_Subr1) /*
::doc:rep.data.tables#symbol-hash::
symbol-hash SYMBOL

Return a positive fixnum somehow related to the name of SYMBOL.
::end:: */
{
  rep_DECLARE1(sym, rep_SYMBOLP);

  return hash_string(rep_STR(rep_SYM(sym)->name));
}

DEFUN("eq-hash", Feq_hash, Seq_hash, (repv value), rep_Subr1) /*
::doc:rep.data.tables#eq-hash::
eq-hash ARG

Return a positive fixnum somehow related to object ARG, such that (eq X
Y) implies (= (eq-hash X) (eq-hash Y)).
::end:: */
{
  uintptr_t hv = value;
  return rep_MAKE_INT(TRUNC(hv));
}

/* FIXME This is probably _very_ sub-optimal.. */

static uintptr_t
equal_hash(repv x, unsigned int n)
{
  if (rep_CONSP(x)) {
    uintptr_t hash = 5381 * rep_Cons;
    int i = n;
    while (rep_CONSP(x) && i-- > 0) {
      hash = hash * 33 + equal_hash(rep_CAR(x), n / 2);
      x = rep_CDR(x);
    }
    if (i > 0) {
      hash = hash * 33 + equal_hash(x, n / 2);
    }
    return hash;
  } else if (rep_VECTORP(x) || rep_BYTECODEP(x)) {
    uintptr_t hash = 5381 * rep_Vector;
    int i = MIN(n, rep_VECT_LEN(x));
    while (i-- > 0) {
      hash = hash * 33 + equal_hash(rep_VECTI(x, i), n / 2);
    }
    return hash;
  } else if (rep_STRINGP(x)) {
    return hash_string(rep_STR(x));
  } else if (rep_SYMBOLP(x)) {
    return hash_string(rep_STR(rep_SYM(x)->name));
  } else if (rep_INTP(x)) {
    return rep_INT(x);
  } else if (rep_NUMBERP(x)) {
    return rep_get_long_uint(x);
  } else {
    return 5381 * 33 + rep_TYPE(x);
  }
}

DEFUN("equal-hash", Fequal_hash, Sequal_hash, (repv x), rep_Subr1) /*
::doc:rep.data.tables#equal-hash::
equal-hash ARG

Return a positive fixnum somehow related to ARG, such that (equal X Y)
implies (= (equal-hash X) (equal-hash Y)).
::end:: */
{
  const unsigned int bits = sizeof(uintptr_t) * CHAR_BIT;

  uintptr_t hash = equal_hash(x, bits / 2);

  return rep_MAKE_INT(TRUNC(hash));
}

static repv
make_table(repv hash_fun, repv cmp_fun, bool weak_keys)
{
  rep_DECLARE(1, hash_fun, Ffunctionp(hash_fun) != rep_nil);
  rep_DECLARE(2, cmp_fun, Ffunctionp(cmp_fun) != rep_nil);

  table *tab = rep_alloc(sizeof(table));
  rep_data_after_gc += sizeof(table);

  tab->car = table_type();
  tab->next = all_tables;
  all_tables = tab;
  tab->hash_fun = hash_fun;
  tab->compare_fun = cmp_fun;
  tab->total_buckets = 0;
  tab->total_nodes = 0;
  tab->guardian = !weak_keys ? 0 : Fmake_primitive_guardian();

  return rep_VAL(tab);
}

DEFUN("make-table", Fmake_table, Smake_table,
      (repv hash_fun, repv cmp_fun), rep_Subr2) /*
::doc:rep.data.tables#make-table::
make-table HASH-FUNCTION COMPARE-FUNCTION

Create and return a new hash table. When storing and referencing keys
it will use the function HASH-FUNCTION to map keys to hash codes
(positive fixnums), and the predicate function COMPARE-FUNCTION to
compare two keys (should return true if the keys are considered equal).
::end:: */
{
  return make_table(hash_fun, cmp_fun, false);
}

DEFUN("make-weak-table", Fmake_weak_table, Smake_weak_table,
      (repv hash_fun, repv cmp_fun), rep_Subr2) /*
::doc:rep.data.tables#make-weak-table::
make-weak-table HASH-FUNCTION COMPARE-FUNCTION

Similar to `make-table, except that key-value pairs stored in the table
are said to be ``weakly keyed''. That is, they are only retained in the
table as long the key has not been garbage collected.

Unlike with tables created by the `make-table function, the fact that
the key is stored in the table is not considered good enough to prevent
it being garbage collected.
::end:: */
{
  return make_table(hash_fun, cmp_fun, true);
}

DEFUN("tablep", Ftablep, Stablep, (repv arg), rep_Subr1) /*
::doc:rep.data.tables#tablep::
tablep ARG

Return true if ARG is a hash table.
::end:: */
{
  return TABLEP(arg) ? Qt : rep_nil;
}

static uintptr_t
hash_key(repv tab, repv key)
{
  repv hash;
  if (TABLE(tab)->hash_fun == rep_VAL(&Sstring_hash)) {
    hash = Fstring_hash(key);
  } else if (TABLE(tab)->hash_fun == rep_VAL(&Ssymbol_hash)) {
    hash = Fsymbol_hash(key);
  } else if (TABLE(tab)->hash_fun == rep_VAL(&Seq_hash)) {
    hash = Feq_hash(key);
  } else if (TABLE(tab)->hash_fun == rep_VAL(&Sequal_hash)) {
    hash = Fequal_hash(key);
  } else {
    rep_GC_root gc_tab;
    rep_PUSHGC(gc_tab, tab);
    hash = rep_call_lisp1(TABLE(tab)->hash_fun, key);
    rep_POPGC;
  }
  return rep_INT(hash);
}

static inline int
hash_key_to_bin(repv tab, uintptr_t hash)
{
  return hash % TABLE(tab)->total_buckets;
}

static inline bool
compare(repv tab, repv val1, repv val2)
{
  rep_GC_root gc_tab;
  rep_PUSHGC(gc_tab, tab);
  repv ret = rep_call_lisp2(TABLE(tab)->compare_fun, val1, val2);
  rep_POPGC;
  return ret != rep_nil;
}

static node *
lookup(repv tab, repv key)
{
  if (TABLE(tab)->total_buckets == 0) {
    return NULL;
  }

  uintptr_t hv = hash_key(tab, key);
  int index = hash_key_to_bin(tab, hv);

  for (node *ptr = TABLE(tab)->buckets[index]; ptr; ptr = ptr->next) {
    if (ptr->hash == hv && compare(tab, key, ptr->key)) {
      return ptr;
    }
  }

  return NULL;
}

DEFUN("table-ref", Ftable_ref, Stable_ref, (repv tab, repv key), rep_Subr2) /*
::doc:rep.data.tables#table-ref::
table-ref TABLE KEY

Return the value stored in hash table TABLE indexed by object KEY.
Returns false if no such value exists.
::end:: */
{
  rep_DECLARE1(tab, TABLEP);

  node *n = lookup(tab, key);
  return n ? n->value : rep_nil;
}

DEFUN("table-bound-p", Ftable_bound_p,
      Stable_bound_p, (repv tab, repv key), rep_Subr2) /*
::doc:rep.data.tables#table-bound-p::
table-bound-p TABLE KEY

Returns true if the hash table TABLE contains a value associated with
KEY.
::end:: */
{
  rep_DECLARE1(tab, TABLEP);

  node *n = lookup(tab, key);
  return n ? Qt : rep_nil;
}

DEFUN("table-set", Ftable_set, Stable_set,
      (repv tab, repv key, repv value), rep_Subr3) /*
::doc:rep.data.tables#table-set::
table-set TABLE KEY VALUE

Associate VALUE with KEY in hash table TABLE. Returns VALUE.
::end:: */
{
  rep_DECLARE1(tab, TABLEP);

  node *n = lookup(tab, key);

  if (!n) {
    n = rep_alloc(sizeof(node));
    rep_data_after_gc += sizeof(node);

    n->key = key;
    n->value = value;
    n->hash = hash_key(tab, key);

    TABLE(tab)->total_nodes++;
    if (TABLE(tab)->total_nodes >= 2 * TABLE(tab)->total_buckets) {

      /* The (misguided?) idea is to set number of buckets as (2^N) - 1,
         then increase N each time we get twice as many keys as buckets.
	 Start at N=5 */

      node **old_bins = TABLE(tab)->buckets;
      int old_size = TABLE(tab)->total_buckets;

      int new_size;
      if (old_size == 0) {
	new_size = 31;
      } else {
	new_size = (old_size + 1) * 2 - 1;
      }

      node **new_bins = rep_alloc(sizeof(node *) * new_size);
      rep_data_after_gc += sizeof(node *) * new_size;
      memset(new_bins, 0, sizeof(node *) * new_size);

      TABLE(tab)->buckets = new_bins;
      TABLE(tab)->total_buckets = new_size;

      for (int i = 0; i < old_size; i++) {
	node *ptr = old_bins[i];
	while (ptr) {
	  int index = hash_key_to_bin(tab, ptr->hash);
	  node *next = ptr->next;
	  ptr->next = new_bins[index];
	  new_bins[index] = ptr;
	  ptr = next;
	}
      }

      if (old_size > 0) {
	rep_free(old_bins);
      }
    }

    int bin = hash_key_to_bin(tab, n->hash);
    n->next = TABLE(tab)->buckets[bin];
    TABLE(tab)->buckets[bin] = n;
    if (TABLE(tab)->guardian) {
      Fprimitive_guardian_push(TABLE(tab)->guardian, n->key);
    }
  }

  n->value = value;
  return value;
}

DEFUN("table-unset", Ftable_unset, Stable_unset,
      (repv tab, repv key), rep_Subr2) /*
::doc:rep.data.tables#table-unset::
table-unset TABLE KEY

Remove any value stored in TABLE associated with KEY.
::end:: */
{
  rep_DECLARE1(tab, TABLEP);

  node *n = lookup(tab, key);

  if (n) {
    int bin = hash_key_to_bin(tab, n->hash);
    for (node **ptr = &(TABLE(tab)->buckets[bin]);
	 *ptr; ptr = &((*ptr)->next))
    {
      if (*ptr == n) {
	*ptr = n->next;
	rep_free(n);
	TABLE(tab)->total_nodes--;
	return Qt;
      }
    }
  }

  return rep_nil;
}

DEFUN("table-walk", Ftable_walk, Stable_walk,
      (repv fun, repv tab), rep_Subr2) /*
::doc:rep.data.tables#table-walk::
table-walk FUNCTION TABLE

Call FUNCTION for every key-value pair stored in hash table TABLE. For
each pair, the function is called with arguments `(KEY VALUE)'.
::end:: */
{
  rep_DECLARE1(tab, TABLEP);

  rep_GC_root gc_tab, gc_fun;
  rep_PUSHGC(gc_tab, tab);
  rep_PUSHGC(gc_fun, fun);

  for (int i = 0; i < TABLE(tab)->total_buckets; i++) {
    for (node *n = TABLE(tab)->buckets[i]; n != 0; n = n->next) {
      if (!rep_call_lisp2(fun, n->key, n->value)) {
	break;
      }
    }
  }

  rep_POPGC; rep_POPGC;

  return rep_throw_value ? 0 : rep_nil;
}

DEFUN("table-size", Ftable_size, Stable_size, (repv tab), rep_Subr1) /*
::doc:rep.data.tables#table-size::
table-size TABLE

Returns the number of items currently stored in TABLE.
::end:: */
{
  rep_DECLARE1(tab, TABLEP);

  return rep_make_long_int(TABLE(tab)->total_nodes);
}

static void
tables_init(void)
{
  rep_ADD_SUBR(Smake_table);
  rep_ADD_SUBR(Smake_weak_table);
  rep_ADD_SUBR(Sstring_hash);
  rep_ADD_SUBR(Ssymbol_hash);
  rep_ADD_SUBR(Seq_hash);
  rep_ADD_SUBR(Sequal_hash);
  rep_ADD_SUBR(Stablep);
  rep_ADD_SUBR(Stable_ref);
  rep_ADD_SUBR(Stable_bound_p);
  rep_ADD_SUBR(Stable_set);
  rep_ADD_SUBR(Stable_unset);
  rep_ADD_SUBR(Stable_walk);
  rep_ADD_SUBR(Stable_size);
}

void
rep_tables_init(void)
{
  rep_lazy_structure("rep.data.tables", tables_init);
}
