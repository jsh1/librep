/* tables.c -- hash tables
   Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>
   $Id$

   This file is part of librep.

   librep is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   librep is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with librep; see the file COPYING.	If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* notes:

   The api of this module (except for make-table) was mostly borrowed
   from Scheme48. The implementation is all my own fault..

   todo:

   support for weak keys */

#define _GNU_SOURCE

#include "repint.h"
#include <string.h>
#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

typedef rep_PTR_SIZED_INT hash_value;

typedef struct node_struct node;
struct node_struct {
    node *next;
    repv key, value;
    hash_value hash;
};

typedef struct table_struct table;
struct table_struct {
    repv car;
    table *next;
    int total_buckets, total_nodes;
    node **buckets;
    repv hash_fun;
    repv compare_fun;
};

#define TABLEP(v) rep_CELL16_TYPEP(v, table_type)
#define TABLE(v)  ((table *) rep_PTR(v))

static int table_type;
static table *all_tables;

DEFSYM(tables, "tables");

/* ensure X is +ve and in an int */
#define TRUNC(x) (((x) << (rep_VALUE_INT_SHIFT+1)) >> (rep_VALUE_INT_SHIFT+1))


/* type hooks */

static void
table_mark (repv val)
{
    int i;
    for (i = 0; i < TABLE(val)->total_buckets; i++)
    {
	node *n;
	for (n = TABLE(val)->buckets[i]; n != 0; n = n->next)
	{
	    rep_MARKVAL(n->key);
	    rep_MARKVAL(n->value);
	}
    }
}

static void
free_table (table *x)
{
    int i;
    for (i = 0; i < x->total_buckets; i++)
    {
	node *n, *next;
	for (n = x->buckets[i]; n != 0; n = next)
	{
	    next = n->next;
	    rep_free (n);
	}
    }
    if (x->total_buckets > 0)
	rep_free (x->buckets);
    rep_FREE_CELL (x);
}

static void
table_sweep (void)
{
    table *x = all_tables;
    all_tables = 0;
    while (x != 0)
    {
	table *next = x->next;
	if (!rep_GC_CELL_MARKEDP (rep_VAL(x)))
	    free_table (x);
	else
	{
	    rep_GC_CLR_CELL (rep_VAL(x));
	    x->next = all_tables;
	    all_tables = x;
	}
	x = next;
    }
}

static void
table_print (repv stream, repv arg)
{
    rep_stream_puts (stream, "#<table ", -1, rep_FALSE);
    rep_princ_val (stream, TABLE(arg)->hash_fun);
    rep_stream_putc (stream, ' ');
    rep_princ_val (stream, TABLE(arg)->compare_fun);
    rep_stream_putc (stream, '>');
}


/* hash functions */

static inline repv
hash_string (register u_char *ptr)
{
    register u_long value = 0;
    while(*ptr != 0)
	value = (value * 33) + *ptr++;
    return rep_MAKE_INT (TRUNC (value));
}

DEFUN("string-hash", Fstring_hash, Sstring_hash, (repv string), rep_Subr1)
{
    rep_DECLARE1(string, rep_STRINGP);
    return hash_string (rep_STR (string));
}

DEFUN("symbol-hash", Fsymbol_hash, Ssymbol_hash, (repv sym), rep_Subr1)
{
    rep_DECLARE1(sym, rep_SYMBOLP);
    return hash_string (rep_STR (rep_SYM (sym)->name));
}

DEFUN("eq-hash", Feq_hash, Seq_hash, (repv value), rep_Subr1)
{
    return rep_MAKE_INT (TRUNC (value));
}

/* XXX This is probably _very_ sub-optimal.. */
DEFUN("equal-hash", Fequal_hash, Sequal_hash, (repv x), rep_Subr1)
{
    if (rep_CONSP (x))
    {
	repv left = Fequal_hash (rep_CAR(x));
	repv right = Fequal_hash (rep_CDR(x));
	return rep_MAKE_INT (rep_INT (left) ^ (rep_INT (right) >> 1));
    }
    else if (rep_VECTORP (x) || rep_COMPILEDP (x))
    {
	int i;
	repv hash = 0;
	for (i = 0; i < rep_VECT_LEN (x); i++)
	{
	    repv tem = Fequal_hash (rep_VECTI (x, i));
	    hash = hash * 33 + rep_INT (tem);
	}
	return rep_MAKE_INT (TRUNC (hash));
    }
    else if (rep_STRINGP (x))
	return Fstring_hash (x);
    else if (rep_SYMBOLP (x))
	return Fsymbol_hash (x);
    else
	return Feq_hash (x);
}


/* table functions */

DEFUN("make-table", Fmake_table, Smake_table,
      (repv hash_fun, repv cmp_fun), rep_Subr3)
{
    table *tab;
    rep_DECLARE(1, hash_fun, Ffunctionp (hash_fun) != Qnil);
    rep_DECLARE(2, cmp_fun, Ffunctionp (cmp_fun) != Qnil);

    tab = rep_ALLOC_CELL (sizeof (table));
    rep_data_after_gc += sizeof (table);
    tab->car = table_type;
    tab->next = all_tables;
    all_tables = tab;
    tab->hash_fun = hash_fun;
    tab->compare_fun = cmp_fun;
    tab->total_buckets = 0;
    tab->total_nodes = 0;

    return rep_VAL(tab);
}

DEFUN("tablep", Ftablep, Stablep, (repv arg), rep_Subr1)
{
    return TABLEP(arg) ? Qt : Qnil;
}

static inline hash_value
hash_key (repv tab, repv key)
{
    repv hash;
    if (TABLE(tab)->hash_fun == rep_VAL(&Sstring_hash))
	hash = Fstring_hash (key);
    else if (TABLE(tab)->hash_fun == rep_VAL(&Ssymbol_hash))
	hash = Fsymbol_hash (key);
    else
    {
	rep_GC_root gc_tab;
	rep_PUSHGC (gc_tab, tab);
	hash = rep_call_lisp1 (TABLE(tab)->hash_fun, key);
	rep_POPGC;
    }
    return rep_INT(hash);
}

static inline int
hash_key_to_bin (repv tab, hash_value hash)
{
    return hash % TABLE(tab)->total_buckets;
}

static inline rep_bool
compare (repv tab, repv val1, repv val2)
{
    repv ret;
    rep_GC_root gc_tab;
    rep_PUSHGC (gc_tab, tab);
    ret = rep_call_lisp2 (TABLE(tab)->compare_fun, val1, val2);
    rep_POPGC;
    return ret != Qnil;
}

static node *
lookup (repv tab, repv key)
{
    hash_value hv;
    node *ptr;
    int index;
    if (TABLE(tab)->total_buckets == 0)
	return 0;
    hv = hash_key (tab, key);
    index = hash_key_to_bin (tab, hv);
    for (ptr = TABLE(tab)->buckets[index]; ptr != 0; ptr = ptr->next)
    {
	if (ptr->hash == hv && compare (tab, key, ptr->key))
	    return ptr;
    }
    return 0;
}

DEFUN("table-ref", Ftable_ref, Stable_ref, (repv tab, repv key), rep_Subr2)
{
    node *n;
    rep_DECLARE1(tab, TABLEP);
    n = lookup (tab, key);
    return n ? n->value : Qnil;
}

DEFUN("table-set", Ftable_set, Stable_set,
      (repv tab, repv key, repv value), rep_Subr3)
{
    node *n;
    rep_DECLARE1(tab, TABLEP);
    n = lookup (tab, key);
    if (n == 0)
    {
	int bin;
	n = rep_alloc (sizeof (node));
	rep_data_after_gc += sizeof (node);
	n->key = key;
	n->value = value;
	n->hash = hash_key (tab, key);
	TABLE(tab)->total_nodes++;
	if (TABLE(tab)->total_nodes >= 2 * TABLE(tab)->total_buckets)
	{
	    int old_size, new_size, i;
	    node **new_bins, **old_bins;

	    old_bins = TABLE(tab)->buckets;
	    old_size = TABLE(tab)->total_buckets;

	    /* The (misguided?) idea is to set number of buckets as
	        (2^N) - 1, then increase N each time we get twice as
		many keys as buckets. Start at N=5 */

	    if (old_size == 0)
		new_size = 31;
	    else
		new_size = (old_size + 1) * 2 - 1;

	    new_bins = rep_alloc (sizeof (node *) * new_size);
	    rep_data_after_gc += sizeof (node *) * new_size;
	    memset (new_bins, 0, sizeof (node *) * new_size);

	    TABLE(tab)->buckets = new_bins;
	    TABLE(tab)->total_buckets = new_size;
	    for (i = 0; i < old_size; i++)
	    {
		node *ptr, *next;
		for (ptr = old_bins[i]; ptr != 0; ptr = next)
		{
		    int index = hash_key_to_bin (tab, ptr->hash);
		    next = ptr->next;
		    ptr->next = new_bins[index];
		    new_bins[index] = ptr;
		}
	    }

	    if (old_size > 0)
		rep_free (old_bins);
	}
	bin = hash_key_to_bin (tab, n->hash);
	n->next = TABLE(tab)->buckets[bin];
	TABLE(tab)->buckets[bin] = n;
    }
    n->value = value;
    return value;
}

DEFUN("table-walk", Ftable_walk, Stable_walk, (repv fun, repv tab), rep_Subr2)
{
    rep_GC_root gc_tab, gc_fun;
    int i;

    rep_DECLARE1(tab, TABLEP);
    rep_PUSHGC (gc_tab, tab);
    rep_PUSHGC (gc_fun, fun);

    for (i = 0; i < TABLE(tab)->total_buckets; i++)
    {
	node *n;
	for (n = TABLE(tab)->buckets[i]; n != 0; n = n->next)
	{
	    if (!rep_call_lisp2 (fun, n->key, n->value))
		break;
	}
    }

    rep_POPGC; rep_POPGC;
    return rep_throw_value ? rep_NULL : Qnil;
}


/* dl hooks */

repv
rep_dl_init (void)
{
    table_type = rep_register_new_type ("table", 0, table_print, table_print,
					table_sweep, table_mark,
					0, 0, 0, 0, 0, 0, 0);
    rep_ADD_SUBR(Smake_table);
    rep_ADD_SUBR(Sstring_hash);
    rep_ADD_SUBR(Ssymbol_hash);
    rep_ADD_SUBR(Seq_hash);
    rep_ADD_SUBR(Sequal_hash);
    rep_ADD_SUBR(Stablep);
    rep_ADD_SUBR(Stable_ref);
    rep_ADD_SUBR(Stable_set);
    rep_ADD_SUBR(Stable_walk);
    rep_INTERN (tables);
    return Qtables;
}