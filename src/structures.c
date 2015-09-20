/* structures.c -- basis for module system

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

/* Define VERBOSE to print cache miss ratios. */

#undef VERBOSE

/* The kind of cache for storing resolved module references. */

#define SINGLE_SA_CACHE 1

/* Notes:

   rep's module system is based on the Scheme48 system, which itself
   takes ideas from Standard ML and Xerox scheme.

   Modules are known as structures (from SML) and may be anonymous or
   named (as with functions, but in a separate namespace), but only
   named structures may be imported or accessed. Each structure is
   basically a separate global namespace, with a number of variable
   bindings. Each closure contains a reference to the structure it was
   instantiated in, providing the source for referencing any unbound
   variables.

   Each structure presents an interface to any structures that import
   its bindings. This interface is simply the list of symbols whose
   bindings may be referenced from outside.

   Structures may either `open' or `access' other structures; when
   opening a structure all its exported bindings are immediately
   referenceable from the importing structures. Exported bindings from
   accessed structures are referenced using the `structure-ref' form

   Structures are implemented as first-class objects, but only a second-
   class view is presented to most lisp code, this is to enable static
   analysis of package imports and exports at compile time

   Here is the module language grammar adapted from Rees' memo:

   <definition> -> (define-structure <name> <interface> <config> <form>*)
		   (define-interface <name> <interface>)

   <structure> -> (structure <interface> <config> <form>*)

   <interface> -> (export <id>*)
		  <name>
		  (compound-interface <interface>*)

   <config> -> (<clause>*)
	       <clause>

   <clause> -> (open <name>*)
	       (access <name>*)

   Most files will just contain a single `(define-structure ...)' form.
   E.g.:

   (define-structure foo (export foo) (open rep)
     (defun foo (x)
       (1+ x)))

   As Rees points out, this changes load from being used for its side
   effects to being used for its value, the created structure.

   For backwards compatibility, the `require' form now works with both
   simple files and files containing module definitions. E.g. if a file
   called `foo.jl' contains the above example, then doing "(require
   'foo)" would open the module in the current environment.

   Special variables have their own isolated namespace (the structure
   called `%specials') and thus their names can still clash across
   structures..  */

#include "repint.h"

#include <assert.h>
#include <string.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

#define MIN_BUCKETS 8
#define MAX_MULTIPLIER 2

static rep_struct *all_structures;

#define rep_INTERFACEP(v) rep_LISTP(v)

/* The currently active namespace. */

repv rep_structure;

/* The `default' namespace, where all rep language bindings go. */

repv rep_default_structure;

/* The namespace of special variables. */

repv rep_specials_structure;

/* The structure namespace. */

static repv rep_structures_structure;

DEFSYM(_features, "%features");
DEFSYM(_structures, "%structures");
DEFSYM(_meta, "%meta");
DEFSYM(rep, "rep");
DEFSYM(_specials, "%specials");
DEFSYM(_user_structure_, "*user-structure*");
DEFSYM(rep_structures, "rep.structures");
DEFSYM(rep_lang_interpreter, "rep.lang.interpreter");
DEFSYM(rep_vm_interpreter, "rep.vm.interpreter");
DEFSYM(external, "external");
DEFSYM(local, "local");

static rep_struct_node *lookup_or_add(rep_struct *s, repv var);


/* Cached lookups. */

#ifdef VERBOSE

/* Hits and misses are obvious. Collisions occur when a miss ejects
   data from the cache, conflicts when a miss ejects data for the
   _same_ symbol. */

static int ref_cache_hits, ref_cache_misses,
    ref_cache_collisions, ref_cache_conflicts;

static void
print_cache_stats(void)
{
  fprintf(stderr, "ref cache miss ratio: %g\n",
	  (double) ref_cache_misses / (ref_cache_hits + ref_cache_misses));
  fprintf(stderr, "        - collisions: %g\n",
	  (double) ref_cache_collisions / ref_cache_misses);
  fprintf(stderr, "        -  conflicts: %g\n",
	  (double) ref_cache_conflicts / ref_cache_misses);
}

#endif /* VERBOSE */

#if defined(SINGLE_DM_CACHE)

/* This is a very simple cache; a single direct-mapped table, indexed by
   symbol address */

#define CACHE_SETS 256
#define CACHE_HASH(x) (((x) >> 4) % CACHE_SETS)

struct cache_line {
  rep_struct *s;
  repv var;
  rep_struct_node *n;
  uint32_t epoch;
};

static struct cache_line ref_cache[CACHE_SETS];
uint32_t ref_epoch = 1;

static inline void
enter_cache(rep_struct *s, repv var, rep_struct_node *binding)
{
  uint32_t current_epoch = ref_epoch;
  unsigned int hash = CACHE_HASH(var);

#ifdef VERBOSE
  if (ref_cache[hash].s && ref_cache[hash].epoch == current_epoch) {
    if (ref_cache[hash].var == var) {
      ref_cache_conflicts++;
    } else {
      ref_cache_collisions++;
    }
  }
#endif

  ref_cache[hash].s = s;
  ref_cache[hash].var = var;
  ref_cache[hash].n = binding;
  ref_cache[hash].epoch = current_epoch;
}

static inline bool
lookup_cache(rep_struct *s, repv var, rep_struct_node **ret_n)
{
  uint32_t current_epoch = ref_epoch;
  unsigned int hash = CACHE_HASH(var);

  if (ref_cache[hash].epoch == current_epoch
      && ref_cache[hash].var == var
      && ref_cache[hash].s == s)
  {
#ifdef VERBOSE
    ref_cache_hits++;
#endif
    *ret_n = ref_cache[hash].n;
    return true;
  } else {
#ifdef VERBOSE
    ref_cache_misses++;
#endif
    return false;
  }
}

static inline void
cache_invalidate_symbol(repv var)
{
  unsigned int hash = CACHE_HASH(var);

  if (ref_cache[hash].s && ref_cache[hash].var == var) {
    ref_cache[hash].epoch = 0;
  }
}

static void
cache_invalidate_struct(rep_struct *s)
{
  for (int i = 0; i < CACHE_SETS; i++) {
    if (ref_cache[i].s == s) {
      ref_cache[i].epoch = 0;
    }
  }
}

static inline void
cache_flush(void)
{
  ref_epoch++;
}

#elif defined(SINGLE_SA_CACHE )

/* The above doesn't work so well now that there are more modules,
   moving to 4-way set-associative eliminates significant conflict
   misses in most cases. */

#define CACHE_SETS 128
#define CACHE_HASH(x) (((x) >> 3) % CACHE_SETS)
#define CACHE_ASSOC 4

struct cache_line {
  rep_struct *s;
  repv var;
  rep_struct_node *n;
  uint32_t lru;
  uint32_t epoch;
};

static struct cache_line ref_cache[CACHE_SETS][CACHE_ASSOC];
static uint32_t ref_lru;
static uint32_t ref_epoch = 1;

static inline void
enter_cache(rep_struct *s, repv var, rep_struct_node *binding)
{
  uint32_t current_epoch = ref_epoch;
  unsigned int hash = CACHE_HASH(var);

  unsigned int oldest_i = 0;
  uint32_t oldest_lru = UINT32_MAX;

  for (unsigned int i = 0; i < CACHE_ASSOC; i++) {
    if (ref_cache[hash][i].epoch != current_epoch) {
      oldest_i = i;
      break;
    } else if (ref_cache[hash][i].lru < oldest_lru) {
      oldest_i = i;
      oldest_lru = ref_cache[hash][i].lru;
    }
  }

#ifdef VERBOSE
  if (ref_cache[hash][oldest_i].s
      && ref_cache[hash][oldest_i].epoch == current_epoch)
  {
    if (ref_cache[hash][oldest_i].var == var) {
      ref_cache_conflicts++;
    } else {
      ref_cache_collisions++;
    }
  }
#endif

  ref_cache[hash][oldest_i].s = s;
  ref_cache[hash][oldest_i].var = var;
  ref_cache[hash][oldest_i].n = binding;
  ref_cache[hash][oldest_i].lru = ++ref_lru;
  ref_cache[hash][oldest_i].epoch = current_epoch;
}

static inline bool
lookup_cache(rep_struct *s, repv var, rep_struct_node **ret_n)
{
  uint32_t current_epoch = ref_epoch;
  unsigned int hash = CACHE_HASH(var);

  for (unsigned int i = 0; i < CACHE_ASSOC; i++) {
    if (ref_cache[hash][i].epoch == current_epoch
	&& ref_cache[hash][i].var == var
	&& ref_cache[hash][i].s == s)
    {
#ifdef VERBOSE
      ref_cache_hits++;
#endif
      ref_cache[hash][i].lru = ++ref_lru;
      *ret_n = ref_cache[hash][i].n;
      return true;
    }
  }

#ifdef VERBOSE
  ref_cache_misses++;
#endif
  return false;
}

static inline void
cache_invalidate_symbol(repv var)
{
  unsigned int hash = CACHE_HASH(var);

  for (unsigned int i = 0; i < CACHE_ASSOC; i++) {
    if (ref_cache[hash][i].var == var) {
      ref_cache[hash][i].epoch = 0;
    }
  }
}

static void
cache_invalidate_struct(rep_struct *s)
{
  for (unsigned int i = 0; i < CACHE_SETS; i++) {
    for (unsigned int j = 0; j < CACHE_ASSOC; j++) {
      if (ref_cache[i][j].s == s) {
	ref_cache[i][j].epoch = 0;
      }
    }
  }
}

static inline void
cache_flush(void)
{
  ref_epoch++;
}

#else /* defined(SINGLE_SA_CACHE) */

/* No cache at all. */

static inline void
enter_cache(rep_struct *s, rep_struct_node *binding)
{
}

static inline bool
lookup_cache(rep_struct *s, repv var, rep_struct_node **ret_n)
{
#ifdef VERBOSE
  ref_cache_misses++;
#endif
  return false;
}

static inline void
cache_invalidate_symbol(repv var)
{
}

static void
cache_invalidate_struct(rep_struct *s)
{
}

static void
cache_flush(void)
{
}

#endif /* No cache. */


/* Type hooks. */

static void
structure_mark(repv x)
{
  for (int i = 0; i < rep_STRUCTURE(x)->total_buckets; i++) {
    rep_struct_node *n;
    for (n = rep_STRUCTURE(x)->buckets[i]; n; n = n->next) {
      rep_MARKVAL(n->symbol);
      rep_MARKVAL(n->binding);
    }
  }

  rep_MARKVAL(rep_STRUCTURE(x)->name);
  rep_MARKVAL(rep_STRUCTURE(x)->inherited);
  rep_MARKVAL(rep_STRUCTURE(x)->imports);
  rep_MARKVAL(rep_STRUCTURE(x)->accessible);
  rep_MARKVAL(rep_STRUCTURE(x)->special_variables);
  rep_MARKVAL(rep_STRUCTURE(x)->file_handlers);
}

static void
free_structure(rep_struct *x)
{
  cache_invalidate_struct(x);

  for (int i = 0; i < x->total_buckets; i++) {
    rep_struct_node *n, *next;
    for (n = x->buckets[i]; n; n = next) {
      next = n->next;
      rep_free(n);
    }
  }

  if (x->total_buckets > 0) {
    rep_free(x->buckets);
  }
  rep_free(x);
}

static void
structure_sweep(void)
{
  rep_struct *s = all_structures;
  all_structures = 0;

  while (s) {
    rep_struct *next = s->next;

    if (!rep_GC_CELL_MARKEDP(rep_VAL(s))) {
      free_structure(s);
    } else {
      rep_GC_CLR_CELL(rep_VAL(s));
      s->next = all_structures;
      all_structures = s;
    }

    s = next;
  }

  cache_flush();
}

static void
structure_print(repv stream, repv arg)
{
  if (rep_STRUCTURE(arg)->name == rep_nil) {
    rep_stream_puts(stream, "#<structure>", -1, false);
  } else {
    rep_stream_puts(stream, "#<structure ", -1, false);
    rep_princ_val(stream, rep_STRUCTURE(arg)->name);
    rep_stream_putc(stream, '>');
  }
}


/* Utility functions. */

static inline bool
exports_all(rep_struct *s)
{
  return (s->car & rep_STF_EXPORT_ALL) != 0;
}

/* Return true iff structure S exports a binding of symbol VAR that it
   inherits from one of its opened structures */

static bool
structure_exports_inherited_p(rep_struct *s, repv var)
{
  if (s->car & rep_STF_EXPORT_ALL) {
    return true;
  }

  for (repv tem = s->inherited; rep_CONSP(tem); tem = rep_CDR(tem)) {
    if (rep_CAR(tem) == var) {
      return true;
    }
  }

  return false;
}

static void
init_struct(rep_struct *s)
{
  if (!s->init) {
    return;
  }

  void (*init)(void) = s->init;
  s->init = NULL;

  repv old = rep_structure;
  rep_structure = rep_VAL(s);

  init();

  rep_structure = old;
}

/* Scan for an immediate binding of symbol VAR in structure S, or
   return a null pointer if no such binding */

static inline rep_struct_node *
lookup(rep_struct *s, repv var)
{
  /* Also in OP_REFQ in lispmach.h */

  if (s->total_buckets == 0) {
    if (!s->init) {
      return NULL;
    }
    init_struct(s);
    if (s->total_buckets == 0) {
      return NULL;
    }
  }

  unsigned int hash = rep_STRUCT_HASH(var, s->total_buckets);

  for (rep_struct_node *n = s->buckets[hash]; n != 0; n = n->next) {
    if (n->symbol == var) {
      return n;
    }
  }

  return NULL;
}

static rep_struct_node *
lookup_or_add(rep_struct *s, repv var)
{
  rep_struct_node *n = lookup(s, var);

  if (n) {
    return n;
  }

  if (s->total_buckets == 0) {
    s->total_buckets = MIN_BUCKETS;
    s->buckets = rep_alloc(sizeof(rep_struct_node *) * s->total_buckets);
    memset(s->buckets, 0, sizeof(rep_struct_node *) * s->total_buckets);
    rep_data_after_gc += sizeof(rep_struct_node *) * s->total_buckets;
  }

  if (s->total_bindings > s->total_buckets * MAX_MULTIPLIER) {
    int new_total = s->total_buckets * 2;
    rep_struct_node **buckets =
      rep_alloc(new_total * sizeof(rep_struct_node *));
    memset(buckets, 0, new_total * sizeof(rep_struct_node *));
    rep_data_after_gc += new_total * sizeof(rep_struct_node *);
    for (int i = 0; i < s->total_buckets; i++) {
      rep_struct_node *next;
      for (rep_struct_node *n = s->buckets[i]; n != 0; n = next) {
	unsigned int hash = rep_STRUCT_HASH(n->symbol, new_total);
	next = n->next;
	n->next = buckets[hash];
	buckets[hash] = n;
      }
    }
    s->total_buckets = new_total;
    rep_free(s->buckets);
    s->buckets = buckets;
  }

  n = rep_alloc(sizeof(rep_struct_node));
  rep_data_after_gc += sizeof(rep_struct_node);

  n->symbol = var;
  n->is_constant = false;
  n->is_exported = false;

  unsigned int hash = rep_STRUCT_HASH(var, s->total_buckets);
  n->next = s->buckets[hash];
  s->buckets[hash] = n;

  s->total_bindings++;

  if (structure_exports_inherited_p(s, var)) {
    n->is_exported = true;
    s->inherited = Fdelq(var, s->inherited);
  }

  cache_invalidate_symbol(var);

  return n;
}

static void
remove_binding(rep_struct *s, repv var)
{
  if (s->total_buckets == 0) {
    return;
  }

  unsigned int hash = rep_STRUCT_HASH(var, s->total_buckets);

  rep_struct_node **ptr = &(s->buckets[hash]);
  rep_struct_node *n;

  while ((n = *ptr)) {
    if (n->symbol == var) {
      *ptr = n->next;
      rep_free(n);
      cache_invalidate_symbol(var);
      return;
    }
    ptr = &(n->next);
  }
}

/* Scan for a binding of symbol VAR under structure S, or return null.
   This also searches the exports of any structures that S has opened. */

static rep_struct_node *
lookup_recursively(repv s, repv var)
{
  if (rep_SYMBOLP(s)) {
    s = Fget_structure(s);
  }

  if (!s || !rep_STRUCTUREP(s)
      || (rep_STRUCTURE(s)->car & rep_STF_EXCLUSION))
  {
    return NULL;
  }

  rep_struct_node *n = lookup(rep_STRUCTURE(s), var);

  if (n) {
    return n->is_exported || exports_all(rep_STRUCTURE(s)) ? n : NULL;
  }

  rep_STRUCTURE(s)->car |= rep_STF_EXCLUSION;

  if (structure_exports_inherited_p(rep_STRUCTURE(s), var)) {
    n = rep_search_imports(rep_STRUCTURE(s), var);
  }

  rep_STRUCTURE(s)->car &= ~rep_STF_EXCLUSION;

  return n;
}

rep_struct_node *
rep_search_imports(rep_struct *s, repv var)
{
  rep_struct_node *n;

  if (lookup_cache(s, var, &n)) {
    return n;
  }

  n = NULL;

  for (repv lst = s->imports; rep_CONSP(lst); lst = rep_CDR(lst)) {
    n = lookup_recursively(rep_CAR(lst), var);
    if (n) {
      break;
    }
  }

  enter_cache(s, var, n);
  return n;
}


/* Lisp functions. */

DEFUN("get-structure", Fget_structure,
      Sget_structure, (repv name), rep_Subr1) /*
::doc:rep.structures#get-structure::
get-structure NAME

Return the structure called NAME (a symbol), or return `nil' if no
such structure.
::end:: */
{
  rep_DECLARE1(name, rep_SYMBOLP);

  rep_struct_node *n = lookup(rep_STRUCTURE(rep_structures_structure), name);

  return n ? n->binding : rep_nil;
}

DEFUN("name-structure", Fname_structure,
      Sname_structure, (repv structure, repv name), rep_Subr2) /*
::doc:rep.structures#name-structure::
name-structure STRUCTURE NAME

Assign the name NAME(a symbol) to structure object STRUCTURE.
::end:: */
{
  rep_DECLARE1(structure, rep_STRUCTUREP);

  if (name != rep_nil) {
    rep_DECLARE2(name, rep_SYMBOLP);
    Fstructure_define(rep_structures_structure, name, structure);

    /* FIXME: I'm not sure about this..? */

    if (rep_STRUCTURE(structure)->name == rep_nil) {
      rep_STRUCTURE(structure)->name = name;
    }
  } else if (rep_STRUCTURE(structure)->name != rep_nil) {

    /* Remove the name->structure relation. */

    Fstructure_define(rep_structures_structure,
		      rep_STRUCTURE(structure)->name, rep_nil);
  }

  cache_flush();
  return rep_undefined_value;
}

/* The environment of the thunks are modified! */

DEFUN("make-structure", Fmake_structure, Smake_structure,
       (repv sig, repv header_thunk, repv body_thunk, repv name), rep_Subr4) /*
::doc:rep.structures#make-structure::
make-structure INTERFACE CONFIG-THUNK BODY-THUNK [NAME]

Create and return a new structure. If NAME is a non-nil symbol the
structure will take that name.

The new structure will be advertised as exporting bindings defined by
INTERFACE (currently just a list of symbols).

If CONFIG-THUNK is non-nil it is a zero-parameter function to be called
to define the configuration of the structure (currently its opened and
accessed structures.) This thunk will be evaluated in the environment
of the new structure, but with only the `%meta' (module-configuration)
structure opened.

If BODY-THUNK is non-nil it is a zero-parameter function to be called
to define the values of the bindings exported by the structure. It will
be evaluated in the environment of the new structure.

Note that the captured state of the closures CONFIG-THUNK and
BODY-THUNK may be modified by this function!
::end:: */
{
  rep_DECLARE1(sig, rep_INTERFACEP);
  rep_DECLARE2_OPT(header_thunk, rep_CLOSUREP);
  rep_DECLARE3_OPT(body_thunk, rep_CLOSUREP);
  rep_DECLARE4_OPT(name, rep_SYMBOLP);

  rep_struct *s = rep_alloc(sizeof(rep_struct));
  rep_data_after_gc += sizeof(rep_struct);

  s->car = rep_Structure;
  s->inherited = sig;
  s->name = name;
  s->total_buckets = s->total_bindings = 0;
  s->imports = rep_nil;
  s->accessible = rep_nil;
  s->special_variables = Qt;
  s->file_handlers = Qt;
  s->apply_bytecode = rep_structure ?
    rep_STRUCTURE(rep_structure)->apply_bytecode : NULL;
  s->init = NULL;

  s->next = all_structures;
  all_structures = s;

  repv s_ = rep_VAL(s);

  rep_GC_root gc_s;
  rep_PUSHGC(gc_s, s_);

  if (s->name != rep_nil) {
    Fname_structure(rep_VAL(s), s->name);
  }

  rep_GC_root gc_body;
  rep_PUSHGC(gc_body, body_thunk);

  if (header_thunk != rep_nil) {
    s->imports = Fcons(Q_meta, s->imports);
    rep_CLOSURE(header_thunk)->structure = s_;
    repv tem = rep_call_lisp0(header_thunk);
    s->imports = Fdelq(Q_meta, s->imports);
    if (!tem) {
      s = 0;
    }
  }

  rep_POPGC;

  if (s && body_thunk != rep_nil) {
    rep_CLOSURE(body_thunk)->structure = s_;
    repv tem = rep_call_lisp0(body_thunk);
    if (!tem) {
      s = 0;
    }
  }

  rep_POPGC;

  if (s) {
    return rep_VAL(s);
  }

  /* Initialization failed. */

  s = rep_STRUCTURE(s_);
  if (s->name != rep_nil) {
    Fname_structure(rep_VAL(s), rep_nil);
  }

  return 0;
}

DEFUN("%structure-ref", F_structure_ref,
       S_structure_ref, (repv structure, repv var), rep_Subr2) /*
::doc:rep.structures#%structure-ref::
%structure-ref STRUCTURE VAR

Return the value of the binding of symbol VAR in structure object
STRUCTURE or any inner opened structures.

Returns a void value if no such binding.
::end::*/
{
  rep_DECLARE1(structure, rep_STRUCTUREP);
  rep_DECLARE2(var, rep_SYMBOLP);

  rep_struct *s = rep_STRUCTURE(structure);

  rep_struct_node *n = lookup(s, var);

  if (!n) {
    n = rep_search_imports(s, var);
  }

  return n ? n->binding : rep_void;
}

DEFUN("structure-bound?", Fstructure_bound_p,
       Sstructure_bound_p, (repv structure, repv var), rep_Subr2) /*
::doc:rep.structures#structure-bound?::
structure-bound? STRUCTURE VAR

Return `t' if symbol VAR has a non-void binding in STRUCTURE.
::end:: */
{
  repv tem = F_structure_ref(structure, var);

  if (tem) {
    tem = rep_VOIDP(tem) ? rep_nil : Qt;
  }

  return tem;
}

DEFUN("structure-set!", Fstructure_set, Sstructure_set,
       (repv structure, repv var, repv value), rep_Subr3) /*
::doc:rep.structures#structure-set!::
structure-set! STRUCTURE VAR VALUE

Set the value of the binding of symbol VAR in structure object
STRUCTURE to VALUE. If no such binding exists, an error is signalled.
::end:: */
{
  rep_DECLARE1(structure, rep_STRUCTUREP);
  rep_DECLARE2(var, rep_SYMBOLP);

  rep_struct *s = rep_STRUCTURE(structure);

  if (rep_VOIDP(value)) {
    remove_binding(s, var);
    return rep_nil;
  }

  rep_struct_node *n;
  if (!(s->car & rep_STF_SET_BINDS)) {
    n = lookup(s, var);
  } else {
    n = lookup_or_add(s, var);
  }

  if (!n) {
    return Fsignal(Qvoid_value, rep_LIST_1(var));
  } else if (n->is_constant) {
    return Fsignal(Qsetting_constant, rep_LIST_1(var));
  }

  n->binding = value;

  return rep_undefined_value;
}

DEFUN("structure-define", Fstructure_define, Sstructure_define,
       (repv structure, repv var, repv value), rep_Subr3) /*
::doc:rep.structures#structure-define::
structure-define STRUCTURE VAR VALUE

Set the value of the binding of symbol VAR in structure object
STRUCTURE to VALUE. If no such binding exists, one is created.
::end:: */
{
  rep_DECLARE1(structure, rep_STRUCTUREP);
  rep_DECLARE2(var, rep_SYMBOLP);

  rep_struct *s = rep_STRUCTURE(structure);

  if (rep_VOIDP(value)) {
    remove_binding(s, var);
    return rep_undefined_value;
  }

  rep_struct_node *n = lookup_or_add(s, var);

  if (n->is_constant) {
    return Fsignal(Qsetting_constant, rep_LIST_1(var));
  }

  n->binding = value;

  return rep_undefined_value;
}

DEFUN("external-structure-ref", Fexternal_structure_ref,
       Sexternal_structure_ref, (repv name, repv var), rep_Subr2) /*
::doc:rep.structures#external-structure-ref::
external-structure-ref STRUCT-NAME VAR

Return the value of the binding of symbol VAR within the structure
called STRUCT-NAME. This structure must have previously been marked as
accessible by the current structure (by using the `access' module
configuration directive).

Signals an error if no such binding exists.
::end:: */
{
  rep_DECLARE1(name, rep_SYMBOLP);
  rep_DECLARE2(var, rep_SYMBOLP);

  /* FIXME: caching here? */

  repv tem = Fmemq(name, rep_STRUCTURE(rep_structure)->accessible);

  if (tem == rep_nil) {
    tem = Fmemq(name, rep_STRUCTURE(rep_structure)->imports);
  }

  if (tem && tem != rep_nil) {
    rep_struct_node *n = lookup_recursively(name, var);
    if (n) {
      repv val = n->binding;
      if (!rep_VOIDP(val))
	return val;
    }
  }

  return Fsignal(Qvoid_value, rep_LIST_1(var));
}

DEFUN("structure-name", Fstructure_name,
       Sstructure_name, (repv structure), rep_Subr1) /*
::doc:rep.structures#structure-name::
structure-name STRUCTURE

Returns the name of structure object STRUCTURE.
::end:: */
{
  rep_DECLARE1(structure, rep_STRUCTUREP);

  return rep_STRUCTURE(structure)->name;
}

DEFUN("structure-interface", Fstructure_interface,
       Sstructure_interface, (repv structure), rep_Subr1) /*
::doc:rep.structures#structure-interface::
structure-interface STRUCTURE

Returns the interface of structure object STRUCTURE.
::end:: */
{
  rep_DECLARE1(structure, rep_STRUCTUREP);

  rep_struct *s = rep_STRUCTURE(structure);
  repv list = s->inherited;

  for (int i = 0; i < s->total_buckets; i++) {
    for (rep_struct_node *n = s->buckets[i]; n; n = n->next) {
      if (n->is_exported || exports_all(s)) {
	list = Fcons(n->symbol, list);
      }
    }
  }

  if (exports_all(s)) {
    for (repv lst = s->imports; rep_CONSP(lst); lst = rep_CDR(lst)) {
      repv si = rep_CAR(lst);
      if (rep_SYMBOLP(si)) {
	si = Fget_structure(si);
      }
      if (!si || !rep_STRUCTUREP(si)
	  || (rep_STRUCTURE(si)->car & rep_STF_EXCLUSION))
      {
	continue;
      }
      rep_STRUCTURE(si)->car |= rep_STF_EXCLUSION;
      repv inner = Fstructure_interface(si);
      rep_STRUCTURE(si)->car &= ~rep_STF_EXCLUSION;
      if (!inner) {
	return 0;
      }
      repv argv[2] = {inner, list};
      list = Fappend(2, argv);
    }
  }

  return list;
}

DEFUN("structure-exports?", Fstructure_exports_p,
       Sstructure_exports_p, (repv s, repv var), rep_Subr2) /*
::doc:rep.structures#structure-exports?::
structure-exports? STRUCTURE VAR

Returns true if structure object STRUCTURE exports a binding of symbol
VAR.
::end:: */
{
  rep_DECLARE1(s, rep_STRUCTUREP);
  rep_DECLARE2(var, rep_SYMBOLP);

  rep_struct_node *n = lookup(rep_STRUCTURE(s), var);

  if (n) {
    return n->is_exported || exports_all(rep_STRUCTURE(s)) ? Qlocal : rep_nil;
  }

  n = rep_search_imports(rep_STRUCTURE(s), var);
  if (n) {
    return structure_exports_inherited_p(rep_STRUCTURE(s), var) ?
      Qexternal : rep_nil;
  }

  return rep_nil;
}

DEFUN("structure-imports", Fstructure_imports,
       Sstructure_imports, (repv structure), rep_Subr1) /*
::doc:rep.structures#structure-imports::
structure-imports STRUCTURE

Returns the list of structure names opened by structure object
STRUCTURE.
::end:: */
{
  rep_DECLARE1(structure, rep_STRUCTUREP);

  return rep_STRUCTURE(structure)->imports;
}

DEFUN("structure-accessible", Fstructure_accessible,
       Sstructure_accessible, (repv structure), rep_Subr1) /*
::doc:rep.structures#structure-accessible::
structure-accessible STRUCTURE

Returns the list of structure names accessed by structure object
STRUCTURE.
::end:: */
{
  rep_DECLARE1(structure, rep_STRUCTUREP);

  return rep_STRUCTURE(structure)->accessible;
}

DEFUN("structure-set-interface!", Fstructure_set_interface,
       Sstructure_set_interface, (repv structure, repv sig), rep_Subr2) /*
::doc:rep.structures#structure-set-interface!::
structure-set-interface! STRUCTURE INTERFACE

Set the interface of structure object STRUCTURE to INTERFACE.
::end:: */
{
  rep_DECLARE1(structure, rep_STRUCTUREP);
  rep_DECLARE2(sig, rep_INTERFACEP);

  rep_struct *s = rep_STRUCTURE(structure);

  s->inherited = Fcopy_sequence(sig);
  s->car &= ~rep_STF_EXPORT_ALL;

  for (int i = 0; i < s->total_buckets; i++) {
    for (rep_struct_node *n = s->buckets[i]; n; n = n->next) {
      if (structure_exports_inherited_p(s, n->symbol)) {
	n->is_exported = true;
	s->inherited = Fdelq(n->symbol, s->inherited);
      } else {
	n->is_exported = false;
      }
    }
  }

  cache_flush();
  return rep_undefined_value;
}

DEFUN("structure-file", Fstructure_file,
      Sstructure_file, (repv name), rep_Subr1) /*
::doc:rep.structures#structure-file::
structure-file NAME

Return a string that would be used to locate a structure called NAME (a
symbol).
::end:: */
{
  if (rep_SYMBOLP(name)) {
    name = rep_SYM(name)->name;
  }

  rep_DECLARE1(name, rep_STRINGP);

  /* Convert dots to slashes.  FIXME: escape meta chars? */

  if (!strchr(rep_STR(name), '.')) {
    return name;
  }

  repv copy = rep_string_copy_n(rep_STR(name), rep_STRING_LEN(name));

  for (char *ptr = rep_MUTABLE_STR(copy); *ptr != 0; ptr++) {
    if (*ptr == '.') {
      *ptr = '/';
    }
  }

  return copy;
}

DEFUN("intern-structure", Fintern_structure,
      Sintern_structure, (repv name), rep_Subr1) /*
::doc:rep.structures#intern-structure::
intern-structure STRUCT-NAME

Return the structure called STRUCT-NAME. If no such structure exists,
attempt to load it.
::end:: */
{
  rep_DECLARE1(name, rep_SYMBOLP);

  repv s = Fget_structure(name);

  if (s != rep_nil) {
    return s;
  }

  repv old = rep_structure;
  rep_GC_root gc_name, gc_old;

  /* We need to load the file from within a well-defined structure, not
     just the current one. Look for the value of the *user-structure*
     variable first, then fall back to the default structure */

  rep_structure = rep_default_structure;

  repv user = Fsymbol_value(Q_user_structure_, Qt);
  if (user && rep_SYMBOLP(user)) {
    user = Fget_structure(user);
    if (rep_STRUCTUREP(user)) {
      rep_structure = user;
    }
  }

  rep_PUSHGC(gc_old, old);
  rep_PUSHGC(gc_name, name);

  s = Fload(Fstructure_file(name), rep_nil, rep_nil, rep_nil, rep_nil);

  rep_POPGC; rep_POPGC;

  rep_structure = old;

  if (s && !rep_STRUCTUREP(s))
    s = rep_nil;

  return s;
}

DEFSTRING(no_struct, "No such structure");

DEFUN("open-structures", Fopen_structures,
       Sopen_structures, (repv args), rep_Subr1) /*
::doc:rep.structures#open-structures::
open-structures STRUCT-NAMES

Mark that the current structures has opened the list of structures
named in the list STRUCT-NAMES.
::end:: */
{
  rep_TEST_INT_LOOP_COUNTER;

  rep_DECLARE1(args, rep_LISTP);

  rep_struct *dst = rep_STRUCTURE(rep_structure);

  repv ret = rep_nil;

  rep_GC_root gc_args;
  rep_PUSHGC(gc_args, args);

  while (rep_CONSP(args)) {
    repv s = rep_CAR(args);

    repv tem = Fmemq(s, dst->imports);
    if (tem == rep_nil) {
      if (rep_SYMBOLP(s)) {
	s = Fintern_structure(s);
      }
      if (!s || !rep_STRUCTUREP(s)) {
	ret = Fsignal(Qerror, rep_list_2(rep_VAL(&no_struct), rep_CAR(args)));
	break;
      }
      dst->imports = Fcons(rep_CAR(args), dst->imports);
    } else if (s == Qrep_structures && (dst->car & rep_PENDING_CLOSE)) {
      /* DST had rep.structures opened temporarily during bootstrap,
         but it's now opened it for real, so don't close that struct
	 after bootstrap has finished. */
      dst->car &= ~rep_PENDING_CLOSE;
    }

    args = rep_CDR(args);

    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      ret = 0;
      break;
    }
  }

  rep_POPGC;

  cache_flush();
  return ret;
}

DEFUN("access-structures", Faccess_structures,
       Saccess_structures, (repv args), rep_Subr1) /*
::doc:rep.structures#access-structures::
access-structures STRUCT-NAMES

Mark that the current structures may access the list of structures
named in the list STRUCT-NAMES.
::end:: */
{
  rep_TEST_INT_LOOP_COUNTER;

  rep_DECLARE1(args, rep_LISTP);

  rep_struct *dst = rep_STRUCTURE(rep_structure);

  repv ret = rep_nil;

  rep_GC_root gc_args;
  rep_PUSHGC(gc_args, args);

  while (rep_CONSP(args)) {
    repv tem = Fmemq(rep_CAR(args), dst->accessible);
    if (tem == rep_nil) {
      repv s = Fintern_structure(rep_CAR(args));
      if (!s || !rep_STRUCTUREP(s)) {
	ret = Fsignal(Qerror, rep_list_2(rep_VAL(&no_struct), rep_CAR(args)));
	break;
      }
      dst->accessible = Fcons(rep_CAR(args), dst->accessible);
    }
    args = rep_CDR(args);

    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      ret = 0;
      break;
    }
  }

  rep_POPGC;

  cache_flush();
  return ret;
}

DEFUN("current-structure", Fcurrent_structure,
      Scurrent_structure, (void), rep_Subr0) /*
::doc:rep.structures#current-structure::
current-structure

Return the current structure object.
::end:: */
{
  return rep_structure;
}

DEFUN("structure?", Fstructurep, Sstructurep, (repv arg), rep_Subr1) /*
::doc:rep.structures#structure?::
structure? ARG

Return `t' if ARG is a structure object.
::end:: */
{
  return rep_STRUCTUREP(arg) ? Qt : rep_nil;
}

DEFUN("eval", Freal_eval, Seval_real,
       (repv form, repv structure, repv env), rep_Subr3) /*
::doc:rep.structures#eval::
eval FORM [STRUCTURE]

Return the result of evaluating FORM inside structure object STRUCTURE
(with a null lexical environment).
::end:: */
{
  if (structure == rep_nil) {
    structure = rep_structure;
  }

  rep_DECLARE2(structure, rep_STRUCTUREP);

  repv old_s = rep_structure;
  repv old_env = rep_env;

  rep_GC_root gc_old_s, gc_old_env;
  rep_PUSHGC(gc_old_s, old_s);
  rep_PUSHGC(gc_old_env, old_env);

  init_struct(rep_STRUCTURE(structure));

  rep_structure = structure;
  rep_env = env;

  repv result = Feval(form);

  rep_structure = old_s;
  rep_env = old_env;

  rep_POPGC; rep_POPGC;

  return result;
}

DEFUN("structure-walk", Fstructure_walk,
       Sstructure_walk, (repv fun, repv structure), rep_Subr2) /*
::doc:rep.structures#structure-walk::
structure-walk FUNCTION STRUCTURE

Call FUNCTION for each binding in structure object STRUCTURE. The
function is called with two arguments, the variable and the binding's
value.
::end:: */
{
  rep_DECLARE2(structure, rep_STRUCTUREP);

  repv ret = rep_nil;

  rep_GC_root gc_fun, gc_structure;
  rep_PUSHGC(gc_fun, fun);
  rep_PUSHGC(gc_structure, structure);

  rep_struct *s = rep_STRUCTURE(structure);

  init_struct(s);

  for (int i = 0; i < s->total_buckets; i++) {
    for (rep_struct_node *n = s->buckets[i]; n != 0; n = n->next) {
      if (!rep_VOIDP(n->binding)) {
	ret = rep_call_lisp2(fun, n->symbol, n->binding);
	if (!ret) {
	  goto out;
	}
      }
    }
  }

out:
  rep_POPGC; rep_POPGC;

  return ret ? rep_undefined_value : 0;
}

#ifdef VERBOSE

DEFUN("structure-stats", Fstructure_stats,
       Sstructure_stats, (repv structure), rep_Subr1)
{
  rep_DECLARE1(structure, rep_STRUCTUREP);

  rep_struct *s = rep_STRUCTURE(structure);

  int empties = 0;

  for (int i = 0; i < s->total_buckets; i++) {
    if (s->buckets[i] == 0) {
      empties++;
    }
  }

  fprintf(stderr, "%d buckets, %d of which are empty,\n"
	  "%g bindings per non-empty bucket\n", s->total_buckets, empties,
	  (double) s->total_bindings / (s->total_buckets - empties));

  return Qt;
}

#endif /* VERBOSE */

DEFUN("make-binding-immutable", Fmake_binding_immutable,
       Smake_binding_immutable, (repv var), rep_Subr1) /*
::doc:rep.structures#make-binding-immutable::
make-binding-immutable VAR

Flag that the binding of symbol VAR in the current structure may not be
changed.
::end:: */
{
  rep_DECLARE1(var, rep_SYMBOLP);

  rep_struct_node *n = lookup(rep_STRUCTURE(rep_structure), var);

  if (!n) {
    return Fsignal(Qvoid_value, rep_LIST_1(var));
  }

  n->is_constant = true;

  return rep_undefined_value;
}

DEFUN("binding-immutable?", Fbinding_immutable_p,
       Sbinding_immutable_p, (repv var, repv structure), rep_Subr2) /*
::doc:rep.structures#binding-immutable?::
binding-immutable? VAR [STRUCTURE]

Return `t' if the binding of symbol VAR in the STRUCTURE has been made
constant.
::end:: */
{
  rep_DECLARE1(var, rep_SYMBOLP);

  if (structure == rep_nil) {
    structure = rep_structure;
  }

  rep_DECLARE2(structure, rep_STRUCTUREP);

  rep_struct_node *n = lookup(rep_STRUCTURE(structure), var);

  if (!n) {
    n = rep_search_imports(rep_STRUCTURE(structure), var);
  }

  return n && n->is_constant ? Qt : rep_nil;
}

repv
Fexport_binding(repv var)
{
  rep_DECLARE1(var, rep_SYMBOLP);

  rep_struct *s = rep_STRUCTURE(rep_structure);
  rep_struct_node *n = lookup(s, var);

  if (n) {
    if (!n->is_exported) {
      n->is_exported = true;
      cache_invalidate_symbol(var);
    }
  } else if (!structure_exports_inherited_p(s, var)) {
    s->inherited = Fcons(var, s->inherited);
    cache_invalidate_symbol(var);
  }

  return rep_nil;
}

DEFUN("export-bindings", Fexport_bindings,
       Sexport_bindings, (repv vars), rep_Subr1)
{
  rep_TEST_INT_LOOP_COUNTER;

  rep_DECLARE1(vars, rep_LISTP);

  while (rep_CONSP(vars)) {
    if (!Fexport_binding(rep_CAR(vars))) {
      return 0;
    }
    vars = rep_CDR(vars);
    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      return 0;
    }
  }

  return rep_nil;
}


/* Features. */

DEFUN("feature?", Ffeaturep, Sfeaturep, (repv feature), rep_Subr1) /*
::doc:rep.structures#feature?::
feature? FEATURE

Return non-nil if feature FEATURE has already been loaded by the current
structure.
::end:: */
{
  rep_DECLARE1(feature, rep_SYMBOLP);

  repv tem = Fmemq(feature, rep_STRUCTURE(rep_structure)->imports);
  if (tem && tem != rep_nil) {
    return Qt;
  }

  repv value = F_structure_ref(rep_structure, Q_features);
  if (rep_VOIDP(value)) {
    return rep_nil;
  }

  tem = Fmemq(feature, value);
  if (tem && tem != rep_nil) {
    return Qt;
  }

  return rep_nil;
}

DEFUN("provide", Fprovide, Sprovide, (repv feature), rep_Subr1) /*
::doc:rep.structures#provide::
provide FEATURE

Show that the feature FEATURE (a symbol) has been loaded in the current
structure.
::end:: */
{
  rep_DECLARE1(feature, rep_SYMBOLP);

  repv tem = Fmemq(feature, rep_STRUCTURE(rep_structure)->imports);
  if (tem && tem != rep_nil) {
    return rep_undefined_value;
  }

  repv value = F_structure_ref(rep_structure, Q_features);
  if (rep_VOIDP(value)) {
    value = rep_nil;
  }

  tem = Fmemq(feature, value);
  if (tem && tem == rep_nil) {
    value = Fcons(feature, value);
  }

  Fstructure_define(rep_structure, Q_features, value);

  return rep_undefined_value;
}

DEFUN_INT("require", Frequire, Srequire, (repv feature), rep_Subr1,
	  "SFeature to load:") /*
::doc:rep.structures#require::
require FEATURE

If FEATURE (a symbol) has not already been loaded, load it. The file
loaded is either FILE(if given), or the print name of FEATURE.
::end:: */
{
  rep_DECLARE1(feature, rep_SYMBOLP);

  repv tem = Ffeaturep(feature);
  if (tem != rep_nil) {
    return rep_undefined_value;
  }

  rep_struct *dst = rep_STRUCTURE(rep_structure);

  /* Need to do all this locally, since the file providing the feature
    / module has to be loaded into the _current_ structure (in case it
    contains bare code). %intern-structure OTOH always loads into
    *user-structure*, since it's often called with only the %meta
    structure imported */

  tem = Fmemq(feature, dst->imports);

  if (tem == rep_nil) {
    tem = Fget_structure(feature);
    if (!rep_STRUCTUREP(tem)) {
      rep_GC_root gc_feature;
      rep_PUSHGC(gc_feature, feature);

      tem = Fload(Fstructure_file(feature),
		  rep_nil, rep_nil, rep_nil, rep_nil);

      rep_POPGC;
	    
      if (!tem) {
	return 0;
      }

      if (rep_STRUCTUREP(tem)) {
	Fname_structure(tem, feature);
      }
    }

    if (rep_STRUCTUREP(tem)) {
      dst->imports = Fcons(feature, dst->imports);
      Fprovide(feature);
      cache_flush();
    }
  }

  return rep_undefined_value;
}


/* C interface for structure building. */

repv
rep_push_structure_name(repv name)
{
  if (rep_STRINGP(name)) {
    name = Fintern(name, rep_nil);
  }

  if (!rep_SYMBOLP(name)) {
    return rep_nil;
  }

  repv old = rep_structure;

  repv s = Fget_structure(name);
  if (s == rep_nil) {
    s = Fmake_structure(rep_nil, rep_nil, rep_nil, name);
  }
  rep_structure = s;

  return old;
}

repv
rep_push_structure(const char *name)
{
  return rep_push_structure_name(rep_string_copy(name));
}

repv
rep_pop_structure(repv old)
{
  if (!rep_STRUCTUREP(old)) {
    return rep_nil;
  }

  repv new = rep_structure;
  rep_structure = old;

  return new;
}

void
rep_lazy_structure(const char *name_str, void (*init)(void))
{
  repv name = Fintern(rep_string_copy(name_str), rep_nil);
  repv s = Fget_structure(name);
  if (s == rep_nil) {
    s = Fmake_structure(rep_nil, rep_nil, rep_nil, name);
  } else {
    assert(rep_STRUCTURE(s)->init == NULL);
  }
  rep_STRUCTURE(s)->init = init;
}

repv
rep_bootstrap_structure(const char *name_str)
{
  repv name = rep_string_copy(name_str);
  repv old = rep_push_structure_name(name);

  rep_struct *s = rep_STRUCTURE(rep_structure);

  /* Open rep.lang.interpreter and rep.vm.interpreter. */

  if (s->name != Qrep_lang_interpreter) {
    s->imports = Fcons(Qrep_lang_interpreter, s->imports);
  }

  s->imports = Fcons(Qrep_vm_interpreter, s->imports);

  /* Allow the module to use structure functions, but only while
     bootstrapping. */

  repv structures_cell = 0;
  if (s->name != Qrep_structures) {
    s->imports = Fcons(Qrep_structures, s->imports);
    structures_cell = s->imports;
    s->car |= rep_PENDING_CLOSE;
  }

  repv ret = Fload(Fstructure_file(name), rep_nil, rep_nil, rep_nil, rep_nil);

  if ((s->car & rep_PENDING_CLOSE) && structures_cell) {
    /* Close the rep.structures module. */
    repv *ptr = &s->imports, cell;
    while ((cell = *ptr) && rep_CONSP(cell)) {
      if (cell == structures_cell) {
	*ptr = rep_CDR(cell);
	cache_flush();
	break;
      }
      ptr = rep_CDRLOC(cell);
    }
  }

  rep_pop_structure(old);
  return ret;
}

repv
rep_add_subr(rep_xsubr *subr, bool export)
{
  repv sym = Fintern(subr->name, rep_nil);

  if (sym) {
    rep_struct *s = rep_STRUCTURE(rep_structure);
    rep_struct_node *n = lookup_or_add(s, sym);
    n->binding = rep_VAL(subr);
    n->is_exported = export;
  }

  return sym;
}

DEFUN("structure-exports-all", Fstructure_exports_all,
      Sstructure_exports_all, (repv s, repv status), rep_Subr2)
{
  rep_DECLARE1(s, rep_STRUCTUREP);

  if (status) {
    rep_STRUCTURE(s)->car |= rep_STF_EXPORT_ALL;
  } else {
    rep_STRUCTURE(s)->car &= ~rep_STF_EXPORT_ALL;
  }

  return rep_undefined_value;
}

DEFUN("structure-set-binds", Fstructure_set_binds,
      Sstructure_set_binds, (repv s, repv status), rep_Subr2)
{
  rep_DECLARE1(s, rep_STRUCTUREP);

  if (status) {
    rep_STRUCTURE(s)->car |= rep_STF_SET_BINDS;
  } else {
    rep_STRUCTURE(s)->car &= ~rep_STF_SET_BINDS;
  }

  return rep_undefined_value;
}

void
rep_structure_exports_all(repv s, bool status)
{
  Fstructure_exports_all(s, status ? Qt : rep_nil);
}

void
rep_structure_set_binds(repv s, bool status)
{
  Fstructure_set_binds(s, status ? Qt : rep_nil);
}

static repv
invalid_apply_bytecode(repv subr, int nargs, repv *args)
{
  return Fsignal(Qinvalid_function, rep_LIST_1(subr));
}

DEFUN("set-bytecode-interpreter!", Fstructure_install_vm,
      Sstructure_install_vm, (repv s, repv vm), rep_Subr2)
{
  rep_DECLARE1(s, rep_STRUCTUREP);

  if (vm == rep_nil) {
    rep_STRUCTURE(s)->apply_bytecode = invalid_apply_bytecode;
    return rep_nil;
  }

  rep_DECLARE(2, vm, Ffunctionp(vm) != rep_nil);

  return rep_call_lisp1(vm, s);
}

DEFUN("set-special-variables!", Fset_special_variables,
      Sset_special_variables, (repv s, repv env), rep_Subr2) /*
::doc:rep.structures#set-special-variables!::
set-special-environment! ENV STRUCTURE
::end:: */
{
  rep_DECLARE1(s, rep_STRUCTUREP);

  rep_STRUCTURE(s)->special_variables = env;

  return rep_undefined_value;
}

DEFUN("set-file-handlers!", Fset_file_handlers,
      Sset_file_handlers, (repv s, repv env), rep_Subr2) /*
::doc:rep.structures#set-file-handlers!::
set-file-handlers! STRUCTURE ENV
::end:: */
{
  rep_DECLARE1(s, rep_STRUCTUREP);

  rep_STRUCTURE(s)->file_handlers = env;

  return rep_undefined_value;
}

/* This is a horrible kludge :-(

   The problem is that we are used to doing (set! foo-special 42) in rc
   files, even though foo-special is yet to be marked special. So the
   binding gets made in the current structure, and is then ignored when
   the variable finally gets defvar'd.

   So my solution is to mark a structure as the `user' structure (by
   storing its name in the variable *user-structure*), then check this
   structure for bindings when defvar'ing variables

   This function may not gc */

repv
rep_get_initial_special_value(repv sym)
{
  repv user = F_structure_ref(rep_specials_structure, Q_user_structure_);

  if (rep_SYMBOLP(user)) {
    repv s = Fget_structure(user);
    if (rep_STRUCTUREP(s)) {
      repv old = F_structure_ref(s, sym);
      if (!rep_VOIDP(old)) {
	Fstructure_define(s, sym, rep_void);
	cache_invalidate_symbol(sym);
	return old;
      }
    }
  }

  return 0;
}

repv
rep_documentation_property(repv structure)
{
  repv name = rep_STRUCTURE(structure)->name;

  if (!rep_SYMBOLP(name)) {
    return rep_nil;
  }

  name = rep_SYM(name)->name;
  size_t name_len = rep_STRING_LEN(name);

  const char *doc_str = "documentation#";
  size_t doc_len = strlen("documentation#");

  repv buf = rep_allocate_string(doc_len + name_len + 1);
  if (!buf) {
    return rep_nil;
  }

  strcpy(rep_MUTABLE_STR(buf), doc_str);
  memcpy(rep_MUTABLE_STR(buf) + doc_len, rep_STR(name), name_len);
  rep_MUTABLE_STR(buf)[doc_len + name_len] = 0;

  return Fintern(buf, rep_nil);
}


/* Initialization. */

void
rep_pre_structures_init(void)
{
  static rep_type structure = {
    .car = rep_Structure,
    .name = "structure",
    .print = structure_print,
    .sweep = structure_sweep,
    .mark = structure_mark,
  };

  rep_define_type(&structure);

  rep_default_structure = Fmake_structure(rep_nil, rep_nil, rep_nil, rep_nil);
  rep_specials_structure = Fmake_structure(rep_nil, rep_nil, rep_nil, rep_nil);
  rep_structures_structure = Fmake_structure(rep_nil, rep_nil, rep_nil, rep_nil);

  rep_structure = rep_default_structure;
}

void
rep_structures_init(void)
{
  repv tem = rep_push_structure("rep.structures");
  rep_ADD_SUBR(Smake_structure);
  rep_ADD_SUBR(S_structure_ref);
  rep_ADD_SUBR(Sstructure_bound_p);
  rep_ADD_SUBR(Sstructure_set);
  rep_ADD_SUBR(Sstructure_define);
  rep_ADD_SUBR(Sexternal_structure_ref);
  rep_ADD_SUBR(Sstructure_name);
  rep_ADD_SUBR(Sstructure_interface);
  rep_ADD_SUBR(Sstructure_exports_p);
  rep_ADD_SUBR(Sstructure_imports);
  rep_ADD_SUBR(Sstructure_accessible);
  rep_ADD_SUBR(Sstructure_set_interface);
  rep_ADD_SUBR(Sget_structure);
  rep_ADD_SUBR(Sname_structure);
  rep_ADD_SUBR(Sstructure_file);
  rep_ADD_SUBR(Sintern_structure);
  rep_ADD_SUBR(Sopen_structures);
  rep_ADD_SUBR(Saccess_structures);
  rep_ADD_SUBR(Scurrent_structure);
  rep_ADD_SUBR(Sstructurep);
  rep_ADD_SUBR(Seval_real);
  rep_ADD_SUBR(Sstructure_walk);
#ifdef VERBOSE
  rep_ADD_SUBR(Sstructure_stats);
#endif
  rep_ADD_SUBR(Smake_binding_immutable);
  rep_ADD_SUBR(Sbinding_immutable_p);
  rep_ADD_SUBR(Sexport_bindings);
  rep_ADD_SUBR(Sstructure_exports_all);
  rep_ADD_SUBR(Sstructure_set_binds);
  rep_ADD_SUBR(Sstructure_install_vm);
  rep_ADD_SUBR(Sset_special_variables);
  rep_ADD_SUBR(Sset_file_handlers);
  rep_pop_structure(tem);

  tem = rep_push_structure("rep.module-system");
  rep_ADD_SUBR(Sfeaturep);
  rep_ADD_SUBR(Sprovide);
  rep_ADD_SUBR_INT(Srequire);
  rep_pop_structure(tem);

  rep_INTERN(_features);
  rep_INTERN(_structures);
  rep_INTERN(_meta);
  rep_INTERN(rep);
  rep_INTERN(_specials);
  rep_INTERN_SPECIAL(_user_structure_);
  rep_INTERN(rep_structures);
  rep_INTERN(rep_lang_interpreter);
  rep_INTERN(rep_vm_interpreter);
  rep_INTERN(external);
  rep_INTERN(local);

  rep_mark_static(&rep_structure);
  rep_mark_static(&rep_default_structure);
  rep_mark_static(&rep_specials_structure);
  rep_mark_static(&rep_structures_structure);

  Fname_structure(rep_default_structure, Qrep);
  Fname_structure(rep_specials_structure, Q_specials);
  Fname_structure(rep_structures_structure, Q_structures);
#ifdef VERBOSE
  atexit(print_cache_stats);
#endif
}
