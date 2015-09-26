/* repint.h -- Main include file for library internal objects

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

#ifndef REPINT_H
#define REPINT_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/* Maximum/minimum macros. Don't use when X or Y have side-effects! */

#define MAX(x,y) (((x) > (y)) ? (x) : (y))
#define MIN(x,y) (((x) < (y)) ? (x) : (y))
#define POS(x)   MAX(x, 0)
#define ABS(x)   MAX(x, -(x))

#if __GNUC__ >= 4
# define NOT_INLINE __attribute__((noinline))
#else
# define NOT_INLINE
#endif

/* Compile-time assert() macro. */

#define rep_static_assert(x) switch(0) {case 0:; case (x): ;}

#define rep_INTERNAL 1
#include "rep.h"

enum file_ops {
  op_file_name_absolute_p,
  op_expand_file_name,
  op_local_file_name,
  op_canonical_file_name,
  op_file_name_nondirectory,
  op_file_name_directory,
  op_file_name_as_directory,
  op_directory_file_name,
  op_open_file,
  op_close_file,
  op_flush_file,
  op_seek_file,
  op_write_buffer_contents,		/* these three for jade */
  op_read_file_contents,
  op_insert_file_contents,
  op_delete_file,
  op_rename_file,
  op_make_directory,
  op_delete_directory,
  op_copy_file,
  op_copy_file_to_local_fs,
  op_copy_file_from_local_fs,
  op_file_readable_p,
  op_file_writable_p,
  op_file_exists_p,
  op_file_regular_p,
  op_file_directory_p,
  op_file_symlink_p,
  op_file_owner_p,
  op_file_nlinks,
  op_file_size,
  op_file_modes,
  op_set_file_modes,
  op_file_modes_as_string,
  op_file_modtime,
  op_directory_files,
  op_read_symlink,
  op_make_symlink,
  op_MAX
};

struct blocked_op {
  struct blocked_op *next;
  repv handler;
};

extern struct blocked_op *rep_blocked_ops[op_MAX];

/* Safe stack allocation macros. */

#define rep_stack_alloc(type, count) 		\
  (sizeof(type) * (count) <= 4096 		\
   ? (type *)alloca(sizeof(type) * (count)) 	\
   : (type *)rep_alloc(sizeof(type) * (count)))

#define rep_stack_free(type, count, ptr) 	\
  do {						\
    if (sizeof(type) * (count) > 4096)		\
      rep_free(ptr);				\
  } while (0)


/* Module system. */

/* Structure encapsulating a single namespace. */

typedef struct rep_struct_struct rep_struct;
typedef struct rep_struct_node_struct rep_struct_node;

struct rep_struct_struct {
  repv car;
  rep_struct *next;
  repv name;
  repv inherited;	/* exported symbols that have no local binding */
  int total_buckets, total_bindings;
  rep_struct_node **buckets;
  repv imports;
  repv accessible;

  /* The lists of special variables and named file handlers that may be
     accessed in this environment, or Qt to denote all specials. */

  repv special_variables;
  repv file_handlers;

  /* Bytecode interpreter to use when calling functions defined here.
     If null, call rep_apply_bytecode  */

  repv (*apply_bytecode)(repv subr, int nargs, repv *args);

  /* Lazy initialization function. Called the first time the structure
     is searched for a binding. */

  void (*init)(void);
};

struct rep_struct_node_struct {
  rep_struct_node *next;
  repv symbol;
  repv binding;
  bool is_constant : 1;
  bool is_exported : 1;
};

#define rep_STRUCTUREP(v)	rep_CELL8_TYPEP(v, rep_Structure)
#define rep_STRUCTURE(v)	((rep_struct *) rep_PTR(v))

/* If set, currently recursively searching this module for a binding */
#define rep_STF_EXCLUSION	(1 << (rep_CELL16_TYPE_BITS + 0))

/* If set, all (local) bindings are exported by default. */
#define rep_STF_EXPORT_ALL	(1 << (rep_CELL16_TYPE_BITS + 1))

/* If set, bindings can be created by set! et al. */
#define rep_STF_SET_BINDS	(1 << (rep_CELL16_TYPE_BITS + 2))

#define rep_PENDING_CLOSE	(1 << (rep_CELL16_TYPE_BITS + 3))

#define rep_STRUCT_HASH(x,n)	(((x) >> 3) % (n))


/* Binding frames. */

#define rep_EMPTY_BINDING_FRAME	rep_MAKE_INT(0)
#define rep_MARK_LEX_BINDING(x)	(x + (1 << rep_VALUE_INT_SHIFT))
#define rep_MARK_SPEC_BINDING(x) (x + (1 << (16 + rep_VALUE_INT_SHIFT)))
#define rep_LEX_BINDINGS(x)	(rep_INT(x) & 0xffff)
#define rep_SPEC_BINDINGS(x)	(rep_INT(x) >> 16)

#define rep_USE_CLOSURE(f)			\
  do {						\
    rep_env = rep_CLOSURE(f)->env;		\
    rep_structure = rep_CLOSURE(f)->structure;	\
  } while (0)


/* Call history. */

/* Keeps a backtrace of all lisp functions called. */

typedef struct rep_stack_frame_struct rep_stack_frame;

struct rep_stack_frame_struct {
  rep_stack_frame *next;
  repv fun;
  repv args;
  repv current_form;			/* used for debugging, set by progn */
  repv saved_env;
  repv saved_structure;
  int frame_index;
};

#define rep_PUSH_CALL(lc)				\
  do {							\
    (lc).current_form = 0;				\
    (lc).saved_env = rep_env;				\
    (lc).saved_structure = rep_structure; 		\
    (lc).frame_index = rep_call_stack->frame_index + 1;	\
    (lc).next = rep_call_stack;				\
    rep_call_stack = &(lc);				\
  } while (0)

#define rep_POP_CALL(lc)		\
  do {					\
    rep_env = (lc).saved_env;		\
    rep_structure = (lc).saved_structure; \
    rep_call_stack = (lc).next;		\
  } while (0)


/* Guardians. */

typedef struct rep_guardian_struct {
  repv car;
  struct rep_guardian_struct *next;
  repv accessible;
  repv inaccessible;
} rep_guardian;


/* Locale-aware ctype wrappers. */

#ifdef HAVE_XLOCALE_H
# include <xlocale.h>
# define rep_tolower(c) tolower_l(c, NULL)
# define rep_toupper(c) toupper_l(c, NULL)
# define rep_islower(c) islower_l(c, NULL)
# define rep_isupper(c) isupper_l(c, NULL)
# define rep_isalpha(c) isalpha_l(c, NULL)
# define rep_isdigit(c) isdigit_l(c, NULL)
# define rep_isxdigit(c) isxdigit_l(c, NULL)
# define rep_isalnum(c) isalnum_l(c, NULL)
# define rep_isspace(c) isspace_l(c, NULL)
# define rep_iscntrl(c) iscntrl_l(c, NULL)
#else
# define rep_tolower(c) tolower(c)
# define rep_toupper(c) toupper(c)
# define rep_islower(c) islower(c)
# define rep_isupper(c) isupper(c)
# define rep_isalpha(c) isalpha(c)
# define rep_isdigit(c) isdigit(c)
# define rep_isxdigit(c) isxdigit(c)
# define rep_isalnum(c) isalnum(c)
# define rep_isspace(c) isspace(c)
# define rep_iscntrl(c) iscntrl(c)
#endif

#if defined(__has_builtin) && __has_builtin(__builtin_saddl_overflow)
# define HAVE_OVERFLOW_BUILTINS 1
#elif defined(__GNUC__) && __GNUC__ >= 5
# define HAVE_OVERFLOW_BUILTINS 1
#endif


/* For flags field of rep_type. */

enum rep_type_flags {
  rep_TYPE_INITIALIZED = 1U << 0,
  rep_TYPE_HAS_APPLY = 1U << 1,		/* originally had non-nil apply */
  rep_TYPE_INPUT_STREAM = 1U << 2,	/* originally had non-nil getc */
  rep_TYPE_OUTPUT_STREAM = 1U << 3,	/* originally had non-nil putc */
};


/* prototypes */

#include "repint_subrs.h"

#endif /* REPINT_H */
