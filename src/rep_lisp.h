/* rep_lisp.h -- Data structures/objects for Lisp

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

/* library-private definitions are in repint.h */

#ifndef REP_LISP_H
#define REP_LISP_H

/* Stringify X. Expands macros in X. */
#define rep_QUOTE(x) rep_QUOTE__(x)
#define rep_QUOTE__(x) #x

/* Concat two tokens. Expands macros in X and Y. */
#define rep_CONCAT(x, y) rep_CONCAT__(x, y)
#define rep_CONCAT__(x, y) x##y


/* Lisp values. */

/* A `repv' is a lisp value, perhaps a pointer to an object, but not a real
   pointer; it's two lowest bits define its type. */
typedef uintptr_t repv;

/* The number of bits in the lisp value type. */
#define rep_VALUE_BITS (sizeof(intptr_t) * CHAR_BIT)


/* Structure of Lisp objects and the pointers to them. */

/* Bit definitions for repv pointers. The lowest bit is always zero
   except during GC. If bit one is set the object is a 30-bit signed
   integer, with the data bits stored in the pointer as bits 2->31.

   Otherwise (i.e. bit 1 of the pointer is clear), the value is a
   pointer to a "cell"; all objects other than integers are represented
   by various types of cells. Every cell has a repv as its first
   element (called the car), the lowest bits of this define the actual
   type of the cell.

   If bit zero of the car is unset, the cell is a cons, a pair of two
   values the car and the cdr (the GC mark bit of the cons is bit zero
   of the cdr).

   If bit zero of the car is set, then further type information is
   stored in bits 1->5 of the car, with bit 5 used to denote statically
   allocated objects and bit 7 the mark bit.

   So there are 2^4 possible types of cells. This isn't enough, so bit
   6 of the car is used to denote a ``cell16'' type -- a cell in which
   bits 8->15 give the actual type. These cell16 types are allocated
   dynamically.

   Note that some assumptions are made about data object alignment. All
   Lisp cells _must_ be aligned to four-byte boundaries. If using GNU
   CC, we'll use the alignment attribute. Otherwise the rep_ALIGN macro
   needs setting.. */

#define rep_VALUE_CONS_MARK_BIT	1
#define rep_VALUE_IS_INT	2
#define rep_VALUE_INT_SHIFT	2
#define rep_CELL_ALIGNMENT	sizeof(intptr_t)

/* Align the variable or struct member D to the necessary cell alignment.
   This is used like: ``rep_ALIGN_CELL(rep_cell foo) = ...'' */
#ifdef __GNUC__
# define rep_ALIGN_CELL(d) d __attribute__ ((aligned (rep_CELL_ALIGNMENT)))
#else
# define rep_ALIGN_CELL(d) d
#endif

/* Is repv V a cell type? */
#define rep_CELLP(v)		(((v) & rep_VALUE_IS_INT) == 0)

/* Is repv V a fixnum? */
#define rep_INTP(v)		(!rep_CELLP(v))

/* Is V a fixnum and not negative? */
#define rep_NON_NEG_INT_P(v)	(rep_INTP(v) && !(rep_INT(v) < 0))

/* Convert a repv into a signed integer. */
#define rep_INT(v)		(((intptr_t)(v)) >> rep_VALUE_INT_SHIFT)

/* Convert a signed integer into a repv. */
#define rep_MAKE_INT(x)		(((x) << rep_VALUE_INT_SHIFT) \
				 | rep_VALUE_IS_INT)

/* Bounds of the integer type */
#define rep_LISP_INT_BITS	(rep_VALUE_BITS - rep_VALUE_INT_SHIFT)
#define rep_LISP_MAX_INT	((((intptr_t)1) << (rep_LISP_INT_BITS-1)) - 1)
#define rep_LISP_MIN_INT	(-(((intptr_t)1) << (rep_LISP_INT_BITS-1)))


/* Structure of a cell */

typedef struct {
    /* Low bits of this value define type of the cell. See below. All
       other bits (8->31) are available */
    repv car;

    /* Data follows, in real objects. */
} rep_cell;

/* If bit zero is set in the car of a cell, bits 1->4 of the car are
   type data, bit 5 denotes a cell16 type, bit 6 is set if the object
   is allocated statically, bit 7 is the GC mark bit. This means a
   maximum of 2^3, i.e. 16, cell8 types.

   cell16 types have eight extra type bits, bits 8->15, this gives 256
   dynamically allocated type codes: [256 k + 0x21 | k <- [0..255]]. */

#define rep_CELL_IS_8		0x01
#define rep_CELL_IS_16		0x20
#define rep_CELL_STATIC_BIT	0x40
#define rep_CELL_MARK_BIT	0x80
#define rep_CELL8_TYPE_MASK	0x3f
#define rep_CELL8_TYPE_BITS	8
#define rep_CELL16_TYPE_MASK	0xff21	/* is8 and is16 bits set */
#define rep_CELL16_TYPE_SHIFT	8
#define rep_CELL16_TYPE_BITS	16

/* Build a `rep_cell *' pointer out of a repv of a normal type */
#define rep_PTR(v) 		((rep_cell *)(v))

/* Build a repv out of a pointer to a Lisp_Normal object */
#define rep_VAL(x)		((repv)(x))

/* Is V of cell8 type? */
#define rep_CELL8P(v)		(rep_PTR(v)->car & rep_CELL_IS_8)

/* Is V a cons? */
#define rep_CELL_CONS_P(v)	(!rep_CELL8P(v))

/* Is V statically allocated? */
#define rep_CELL_STATIC_P(v)	(rep_PTR(v)->car & rep_CELL_STATIC_BIT)

/* Is V not an integer or cons? */
#define rep_CELL8_TYPE(v) 	(rep_PTR(v)->car & rep_CELL8_TYPE_MASK)

/* Get the actual cell8 type of V to T */
#define rep_SET_CELL8_TYPE(v, t) \
   (rep_PTR(v)->car = (rep_PTR(v)->car & rep_CELL8_TYPE_MASK) | (t))

/* Is V of cell16 type? */
#define rep_CELL16P(v)		(rep_PTR(v)->car & rep_CELL_IS_16)

/* Get the actual cell16 type of V */
#define rep_CELL16_TYPE(v)	(rep_PTR(v)->car & rep_CELL16_TYPE_MASK)

/* Set the actual cell16 type of V to T */
#define rep_SET_CELL16_TYPE(v, t) \
   (rep_PTR(v)->car = (rep_PTR(v)->car & rep_CELL16_TYPE_MASK) | (t))


/* Structure of a cons cell, the only non-cell8 ptr type */

typedef struct {
    repv car;
    repv cdr;				/* low bit is GC mark */
} rep_cons;

#define rep_CONSP(v)	(rep_CELLP(v) && rep_CELL_CONS_P(v))

/* Get a pointer to a cons cell from a repv. */
#define rep_CONS(v)	((rep_cons *) rep_PTR(v))

/* Get the car or cdr from a cons repv. */
#define rep_CAR(v)	(rep_CONS(v)->car)
#define rep_CDR(v)	(rep_CONS(v)->cdr)
#define rep_CDRLOC(v)	(&(rep_CONS(v)->cdr))

/* Get the cdr when GC is in progress. */
#define rep_GCDR(v)	(rep_CDR(v) & ~rep_VALUE_CONS_MARK_BIT)


/* Type data */

/* Information about each type */

typedef struct rep_type_struct {
  /* Private values. */

  struct rep_type_struct *next;
  bool initialized;

  /* Value to be put in the car of each object to denote this type. */

  repv car;

  /* Name of the type. */

  char *name;

  /* Compares two values, rc is similar to strcmp() */

  int (*compare)(repv val1, repv val2);

  /* Prints a textual representation of the object, not necessarily in
     a readable format */

  void (*princ)(repv stream, repv obj);

  /* Prints a textual representation of the object, if possible in a
     readable format */

  void (*print)(repv stream, repv obj);

  /* When called, should mark any objects that must persist across the
     GC, no matter what. */

  void (*mark_type)(void);

  /* When non-null, a function to mark OBJ and all objects it
     references. */

  void (*mark)(repv obj);

  /* When non-null, a function that should be called during the sweep
     phase of garbage collection. */

  void (*sweep)(void);

  /* When non-null, called immediately before the *after-gc-hook* runs. */

  void (*after_gc)(void);

  /* When non-null, functions called for the stream OBJ. */

  int (*getc)(repv obj);
  int (*ungetc)(repv obj, int c);
  int (*putc)(repv obj, int c);
  intptr_t (*puts)(repv obj, const void *data,
		   intptr_t length, bool lisp_obj_p);

  /* When non-null, a function to ``bind'' to OBJ temporarily,
     returning some handle for later unbinding. */

  repv (*bind)(repv obj);

  /* When non-null, a function to ``unbind'' OBJ, the result of the
     earlier bind call. */

  void (*unbind)(repv obj);

} rep_type;

/* Each type of Lisp object has a type code associated with it.

   Note how non-cons cells are given odd values, so that the
   rep_CELL_IS_8 bit doesn't have to be masked out. */

#define rep_Symbol	0x01
#define rep_Int		0x02		/* made up */
#define rep_Vector	0x03
#define rep_Cons	0x04		/* made up */
#define rep_String	0x05
#define rep_Bytecode	0x07		/* a vector */
#define rep_Datum	0x09
#define rep_File	0x0b
#define rep_Number	0x0d
#define rep_SF		0x0f
#define rep_Subr	0x11
#define rep_Structure	0x13
#define rep_Guardian	0x15
#define rep_Closure	0x17
#define rep_Process	0x19
#define rep_Weak_Ref	0x1b
#define rep_Char	0x1d
#define rep_Unused2	0x1f

/* Assuming that V is a cell, return the type code */
#define rep_CELL_TYPE(v) (rep_CONSP(v) ? rep_Cons		\
			  : !rep_CELL16P(v) ? rep_CELL8_TYPE(v)	\
			  : rep_CELL16_TYPE(v))
/* Return a type code given a repv */
#define rep_TYPE(v)	(rep_INTP(v) ? rep_Int : rep_CELL_TYPE(v))

/* true if V is of type T (T must be a cell8 type) */
#define rep_CELL8_TYPEP(v, t) \
    (rep_CELLP(v) && rep_CELL8_TYPE(v) == (t))

#define rep_CELL16_TYPEP(v, t) \
    (rep_CELLP(v) && rep_CELL16_TYPE(v) == (t))

/* true if V is of type T. */
#define rep_TYPEP(v, t)	(rep_TYPE(v) == t)


/* tuples, cells containing two values */

typedef struct {
  repv car;
  repv a;
  repv b;
} rep_tuple;

#define rep_TUPLE(v)		((rep_tuple *) rep_PTR (v))


/* End-of-list / false value. */

extern rep_tuple rep_eol_datum, rep_void_datum;

#define rep_nil rep_VAL(&rep_eol_datum)
#define rep_void rep_VAL(&rep_void_datum)

#define rep_VOIDP(v)		((v) == rep_void)



/* Numbers (private defs in numbers.c) */

/* Is V a non-fixnum number? */
#define rep_NUMBERP(v)		rep_CELL8_TYPEP(v, rep_Number)

/* Is V numeric? */
#define rep_NUMERICP(v)		(rep_INTP(v) || rep_NUMBERP(v))

/* bits 8-9 of car define number type (except when on freelist) */
typedef rep_cell rep_number;

/* these are in order of promotion */
#define rep_NUMBER_INT		0	/* faked */
#define rep_NUMBER_BIGNUM	0x100
#define rep_NUMBER_RATIONAL	0x200
#define rep_NUMBER_FLOAT	0x400

#define rep_NUMBER_TYPE(v)	(((rep_number *)rep_PTR(v))->car & 0x700)
#define rep_NUMBER_BIGNUM_P(v)	(rep_NUMBER_TYPE(v) & rep_NUMBER_BIGNUM)
#define rep_NUMBER_RATIONAL_P(v) (rep_NUMBER_TYPE(v) & rep_NUMBER_RATIONAL)
#define rep_NUMBER_FLOAT_P(v)	(rep_NUMBER_TYPE(v) & rep_NUMBER_FLOAT)
#define rep_NUMBER_INEXACT_P(v) (rep_NUMBERP(v) && rep_NUMBER_FLOAT_P(v))

#define rep_NUMERIC_TYPE(v) \
    (rep_INTP(v) ? rep_NUMBER_INT : rep_NUMBER_TYPE(v))

#define rep_INTEGERP(v) \
    (rep_INTP(v) || (rep_NUMBERP(v) && rep_NUMBER_BIGNUM_P(v)))


/* Strings */

typedef struct rep_string_utf32_struct rep_string_utf32;

typedef struct {
  /* Also contains the number of bytes in the string. */

  repv car;

  /* Byte vector, character access assumes UTF-8 encoding. */

  uint8_t *utf8_data;

  /* Either zero, the number of UTF-32 code points in the string
     encoded as a rep_INT, or a pointer to cached UTF-32 data (not
     actually a real lisp object). If length is set, and it's the same
     as the number of bytes, then string is ASCII. */

  repv utf32_data;

} rep_string;

/* String contents are immutable. */
#define rep_STRING_IMMUTABLE	(1 << (rep_CELL8_TYPE_BITS + 0))

/* String may be in regexp cache. */
#define rep_STRING_REGEXP	(1 << (rep_CELL8_TYPE_BITS + 1))

/* Length is in high bits of car. */
#define rep_STRING_LEN_SHIFT	(rep_CELL8_TYPE_BITS + 2)

#define rep_MAX_STRING_LEN \
    ((((size_t)1) << (rep_VALUE_BITS - rep_STRING_LEN_SHIFT)) - 1)

#define rep_STRINGP(v)		rep_CELL8_TYPEP(v, rep_String)
#define rep_STRING(v)		((rep_string *) rep_PTR(v))

/* Size in bytes (not including terminating NUL). */
#define rep_STRING_LEN(v)	rep_string_ptr_size(v)

/* True if this string may be written to; generally static strings
   are made from C string-constants and usually in read-only storage. */
#define rep_STRING_WRITABLE_P(v) (!(rep_STRING(v)->car & rep_STRING_IMMUTABLE))

/* Access to plain byte content. */
#define rep_MUTABLE_STR(v)	rep_string_mutable_ptr(v)
#define rep_STR(v)		rep_string_ptr(v)

/* Define a variable V, containing a static string S. This must be cast
   to a repv via the rep_VAL() macro when using. S must be true ASCII. */
#define DEFSTRING(v, s)				\
  rep_ALIGN_CELL(static const rep_string v) = {	\
    ((sizeof(s) - 1) << rep_STRING_LEN_SHIFT)	\
    | rep_STRING_IMMUTABLE			\
    | rep_CELL_STATIC_BIT | rep_String,		\
    (uint8_t *)s, rep_MAKE_INT(sizeof(s) - 1)	\
  }


/* Symbols */

/* symbol object, actual allocated as a tuple */
typedef struct {
  repv car;				/* bits 8->11 are flags */
  repv next;				/* next symbol in rep_obarray bucket */
  repv name;
} rep_symbol;

#define rep_SF_KEYWORD	(1 << (rep_CELL8_TYPE_BITS + 0))

/* Means that the symbol's value may be in some form of local storage,
   if so then that occurrence takes precedence. */
#define rep_SF_LOCAL 	(1 << (rep_CELL8_TYPE_BITS + 1))

/* This means that setting the value of the symbol always sets the
   local value, even if one doesn't already exist.  */
#define rep_SF_SET_LOCAL (1 << (rep_CELL8_TYPE_BITS + 2))

/* When a function is evaluated whose symbol has this bit set, the
   next evaluated form will invoke the Lisp debugger. */
#define rep_SF_DEBUG	(1 << (rep_CELL8_TYPE_BITS + 3))

/* Dynamically bound */
#define rep_SF_SPECIAL	(1 << (rep_CELL8_TYPE_BITS + 4))

/* A special, but was first set from an environment in which specials
   can't normally be accessed; if the symbol is later defvar'd its
   original value will be overwritten. */
#define rep_SF_WEAK	(1 << (rep_CELL8_TYPE_BITS + 5))

/* A variable that was weak, but has been modified via defvar from an
   unrestricted special environment */
#define rep_SF_WEAK_MOD	(1 << (rep_CELL8_TYPE_BITS + 6))

/* Set when the variable has been defvar'd */
#define rep_SF_DEFVAR	(1 << (rep_CELL8_TYPE_BITS + 7))

#define rep_SF_LITERAL	(1 << (rep_CELL8_TYPE_BITS + 8))

#define rep_SYM(v)		((rep_symbol *)rep_PTR(v))
#define rep_SYMBOLP(v)		rep_CELL8_TYPEP(v, rep_Symbol)

#define rep_NILP(v)		((v) == rep_nil)
#define rep_LISTP(v)		(rep_NILP(v) || rep_CONSP(v))

#define rep_KEYWORDP(v)		(rep_SYMBOLP(v) \
				 && (rep_SYM(v)->car & rep_SF_KEYWORD) != 0)

#define rep_SYMBOL_KEYWORD_P(v)	((rep_SYM(v)->car & rep_SF_KEYWORD) != 0)
#define rep_SYMBOL_LITERAL_P(v)	((rep_SYM(v)->car & rep_SF_LITERAL) != 0)


/* Vectors */

typedef struct rep_vector_struct {
  repv car;
  struct rep_vector_struct *next;
  repv array[1];
} rep_vector;

/* Bytes to allocate for S objects */
#define rep_VECT_SIZEOF(s)	((sizeof(repv) * ((s)-1)) + sizeof(rep_vector))

/* Vector contents are immutable. */
#define rep_VECTOR_IMMUTABLE	(1 << (rep_CELL8_TYPE_BITS + 0))

/* Length is in high bits of car. */
#define rep_VECTOR_LEN_SHIFT	(rep_CELL8_TYPE_BITS + 1)
#define rep_VECTOR_LEN(v)	(rep_VECT(v)->car >> rep_VECTOR_LEN_SHIFT)

#define rep_VECT(v)		((rep_vector *)rep_PTR(v))
#define rep_VECTI(v,i)		(rep_VECT(v)->array[(i)])

#define rep_VECTORP(v)		rep_CELL8_TYPEP(v, rep_Vector)
#define rep_VECTOR_WRITABLE_P(v) (!(rep_VECT(v)->car & rep_VECTOR_IMMUTABLE))

/* adaptation of rep_CELL8_TYPEP(). */
#define rep_VECTOR_OR_BYTECODE_P(v) (rep_CELLP(v) \
  && (rep_CELL8_TYPE(v) == rep_Vector || rep_CELL8_TYPE(v) == rep_Bytecode))


/* Characters */

/* allocated as a tuple. */
typedef struct {
  repv car;
  repv next;				/* next in table */
  repv value;				/* fixnum */
} rep_char;

#define rep_CHARP(v)		rep_CELL8_TYPEP(v, rep_Char)
#define rep_CHAR(v)		((rep_char *)rep_PTR(v))
#define rep_CHAR_VALUE(v)	((uint32_t)rep_INT(rep_CHAR(v)->value))
#define rep_CHAR_8BIT_P(v)	(rep_CHARP(v) && rep_CHAR_VALUE(v) < 256)


/* Compiled Lisp functions; this is a vector. Some of these definitions
   are probably hard coded into lispmach.c */

#define rep_BYTECODEP(v)	rep_CELL8_TYPEP(v, rep_Bytecode)
#define rep_BYTECODE(v)		((rep_vector *)rep_PTR(v))

/* First elt is byte-code string */
#define rep_BYTECODE_CODE(v)	rep_VECTI(v, 0)

/* Second is constant vector */
#define rep_BYTECODE_CONSTANTS(v) rep_VECTI(v, 1)

/* Third is an (opaque) integer: memory requirements */
#define rep_BYTECODE_STACK(v)	rep_VECTI(v, 2)

#define rep_BYTECODE_MIN_SLOTS	3

/* Optional fifth element is documentation. */
#define rep_BYTECODE_DOC(v)	((rep_VECTOR_LEN(v) >= 4) \
				 ? rep_VECTI(v, 3) : rep_nil)

/* Optional sixth element is interactive specification. */
#define rep_BYTECODE_INTERACTIVE(v) ((rep_VECTOR_LEN(v) >= 5) \
				     ? rep_VECTI(v, 4) : rep_nil)


/* Files */

typedef struct rep_file_struct {
  repv car;				/* single flag at bit 16 */
  struct rep_file_struct *next;

  /* Name as user sees it */

  repv name;

  /* Function to call to handle file operations, or t for file in local fs */

  repv handler;

  /* Data for handler's use; for local files, this is the name of the
     file opened in the local fs. */

  repv handler_data;

  /* For local files, a buffered file handle; for others a stream. */

  union {
    FILE *fh;
    repv stream;
  } file;

  /* For input streams */

  int line_number;

} rep_file;

#define rep_LFF_DONT_CLOSE	(1 << (rep_CELL8_TYPE_BITS + 0))
#define rep_LFF_BOGUS_LINE_NUMBER (1 << (rep_CELL8_TYPE_BITS + 1))
#define rep_LFF_SILENT_ERRORS	(1 << (rep_CELL8_TYPE_BITS + 2))

#define rep_FILE(v)		((rep_file *)rep_PTR(v))
#define rep_FILEP(v)		rep_CELL8_TYPEP(v, rep_File)
#define rep_LOCAL_FILE_P(v)	(rep_FILE(v)->handler == Qt)


/* Built-in subroutines */

/* Calling conventions are straightforward, returned value is result of
   function. But returning 0 signifies some kind of abnormal exit (i.e.
   an error or throw, or ..?), should be treated as rep_INTERRUPTP
   defined below is */

/* C subroutine, can take from zero to five arguments.  */

typedef struct {
  repv car;
  union {
    repv (*fun0)(void);
    repv (*fun1)(repv);
    repv (*fun2)(repv, repv);
    repv (*fun3)(repv, repv, repv);
    repv (*fun4)(repv, repv, repv, repv);
    repv (*fun5)(repv, repv, repv, repv, repv);
    repv (*funv)(int argc, repv *argv);
    repv (*funsf)(repv args, bool tail_posn);
  } fun;
  repv name;
  repv int_spec;
} rep_subr;

typedef struct {
  repv car;
  repv (*fun)();
  repv name;
  repv int_spec;			/* put this in plist? */
} rep_xsubr;

/* Subroutine "arity", L = f(args_lst), V = f(argc, argv). */

enum {
  rep_SUBR_0,
  rep_SUBR_1,
  rep_SUBR_2,
  rep_SUBR_3,
  rep_SUBR_4,
  rep_SUBR_5,
  rep_SUBR_L,
  rep_SUBR_V,
};

#define rep_Subr0	(rep_Subr | (rep_SUBR_0 << rep_CELL8_TYPE_BITS))
#define rep_Subr1	(rep_Subr | (rep_SUBR_1 << rep_CELL8_TYPE_BITS))
#define rep_Subr2	(rep_Subr | (rep_SUBR_2 << rep_CELL8_TYPE_BITS))
#define rep_Subr3	(rep_Subr | (rep_SUBR_3 << rep_CELL8_TYPE_BITS))
#define rep_Subr4	(rep_Subr | (rep_SUBR_4 << rep_CELL8_TYPE_BITS))
#define rep_Subr5	(rep_Subr | (rep_SUBR_5 << rep_CELL8_TYPE_BITS))
#define rep_SubrL	(rep_Subr | (rep_SUBR_L << rep_CELL8_TYPE_BITS))
#define rep_SubrV	(rep_Subr | (rep_SUBR_V << rep_CELL8_TYPE_BITS))

#define rep_SUBRP(v)	rep_CELL8_TYPEP(v, rep_Subr)
#define rep_SUBR_ARITY(v) (rep_PTR(v)->car >> rep_CELL8_TYPE_BITS)

#define rep_SUBR(v)	((rep_subr *)rep_PTR(v))
#define rep_XSUBR(v)	((rep_xsubr *)rep_PTR(v))
#define rep_SUBR_F0(v)	(rep_SUBR(v)->fun.fun0)
#define rep_SUBR_F1(v)	(rep_SUBR(v)->fun.fun1)
#define rep_SUBR_F2(v)	(rep_SUBR(v)->fun.fun2)
#define rep_SUBR_F3(v)	(rep_SUBR(v)->fun.fun3)
#define rep_SUBR_F4(v)	(rep_SUBR(v)->fun.fun4)
#define rep_SUBR_F5(v)	(rep_SUBR(v)->fun.fun5)
#define rep_SUBR_FL(v)	(rep_SUBR(v)->fun.fun1)
#define rep_SUBR_FV(v)	(rep_SUBR(v)->fun.funv)
#define rep_SF_FUN(v)	(rep_SUBR(v)->fun.funsf)


/* Closures */

typedef struct rep_closure_struct {
  repv car;
  repv fun;
  repv name;
  repv env;
  repv structure;
} rep_closure;

#define rep_CLOSURE(v) ((rep_closure *)rep_PTR(v))
#define rep_CLOSUREP(v) (rep_CELL8_TYPEP(v, rep_Closure))


/* Guardians */

#define rep_GUARDIAN(v)		((rep_guardian *) rep_PTR(v))
#define rep_GUARDIANP(v)	rep_CELL8_TYPEP(v, rep_Guardian)


/* Other definitions */

/* Building lists */
#define rep_LIST_1(v1)			Fcons(v1, rep_nil)
#define rep_LIST_2(v1,v2)		Fcons(v1, rep_LIST_1(v2))
#define rep_LIST_3(v1,v2,v3)		Fcons(v1, rep_LIST_2(v2, v3))
#define rep_LIST_4(v1,v2,v3,v4)		Fcons(v1, rep_LIST_3(v2, v3, v4))
#define rep_LIST_5(v1,v2,v3,v4,v5)	Fcons(v1, rep_LIST_4(v2, v3, v4, v5))

#define rep_CAAR(obj)           rep_CAR(rep_CAR(obj))
#define rep_CDAR(obj)           rep_CDR(rep_CAR(obj))
#define rep_CADR(obj)           rep_CAR(rep_CDR(obj))
#define rep_CDDR(obj)           rep_CDR(rep_CDR(obj))

#define rep_CAAAR(obj)          rep_CAR(rep_CAR(rep_CAR(obj)))
#define rep_CDAAR(obj)          rep_CDR(rep_CAR(rep_CAR(obj)))
#define rep_CADAR(obj)          rep_CAR(rep_CDR(rep_CAR(obj)))
#define rep_CDDAR(obj)          rep_CDR(rep_CDR(rep_CAR(obj)))
#define rep_CAADR(obj)          rep_CAR(rep_CAR(rep_CDR(obj)))
#define rep_CDADR(obj)          rep_CDR(rep_CAR(rep_CDR(obj)))
#define rep_CADDR(obj)          rep_CAR(rep_CDR(rep_CDR(obj)))
#define rep_CDDDR(obj)          rep_CDR(rep_CDR(rep_CDR(obj)))

#define rep_CAAAAR(obj)         rep_CAR(rep_CAR(rep_CAR(rep_CAR(obj))))
#define rep_CDAAAR(obj)         rep_CDR(rep_CAR(rep_CAR(rep_CAR(obj))))
#define rep_CADAAR(obj)         rep_CAR(rep_CDR(rep_CAR(rep_CAR(obj))))
#define rep_CDDAAR(obj)         rep_CDR(rep_CDR(rep_CAR(rep_CAR(obj))))
#define rep_CAADAR(obj)         rep_CAR(rep_CAR(rep_CDR(rep_CAR(obj))))
#define rep_CDADAR(obj)         rep_CDR(rep_CAR(rep_CDR(rep_CAR(obj))))
#define rep_CADDAR(obj)         rep_CAR(rep_CDR(rep_CDR(rep_CAR(obj))))
#define rep_CDDDAR(obj)         rep_CDR(rep_CDR(rep_CDR(rep_CAR(obj))))
#define rep_CAAADR(obj)         rep_CAR(rep_CAR(rep_CAR(rep_CDR(obj))))
#define rep_CDAADR(obj)         rep_CDR(rep_CAR(rep_CAR(rep_CDR(obj))))
#define rep_CADADR(obj)         rep_CAR(rep_CDR(rep_CAR(rep_CDR(obj))))
#define rep_CDDADR(obj)         rep_CDR(rep_CDR(rep_CAR(rep_CDR(obj))))
#define rep_CAADDR(obj)         rep_CAR(rep_CAR(rep_CDR(rep_CDR(obj))))
#define rep_CDADDR(obj)         rep_CDR(rep_CAR(rep_CDR(rep_CDR(obj))))
#define rep_CADDDR(obj)         rep_CAR(rep_CDR(rep_CDR(rep_CDR(obj))))
#define rep_CDDDDR(obj)         rep_CDR(rep_CDR(rep_CDR(rep_CDR(obj))))


/* Garbage collection definitions */

/* GC macros for cell8/16 values */

#define rep_GC_CELL_MARKEDP(v)	(rep_PTR(v)->car & rep_CELL_MARK_BIT)
#define rep_GC_SET_CELL(v)	(rep_PTR(v)->car |= rep_CELL_MARK_BIT)
#define rep_GC_CLR_CELL(v)	(rep_PTR(v)->car &= ~rep_CELL_MARK_BIT)

/* GC macros for cons values */

#define rep_GC_CONS_MARKEDP(v)	(rep_CDR(v) & rep_VALUE_CONS_MARK_BIT)
#define rep_GC_SET_CONS(v)	(rep_CDR(v) |= rep_VALUE_CONS_MARK_BIT)
#define rep_GC_CLR_CONS(v)	(rep_CDR(v) &= ~rep_VALUE_CONS_MARK_BIT)

/* True when cell V has been marked. */

#define rep_GC_MARKEDP(v) \
    (rep_CELL_CONS_P(v) ? rep_GC_CONS_MARKEDP(v) : rep_GC_CELL_MARKEDP(v))

/* Set the mark bit of cell V. */

#define rep_GC_SET(v)		\
  do {				\
    if (rep_CELLP(v)) {		\
      rep_GC_SET_CELL(v);	\
    } else {			\
      rep_GC_SET_CONS(v);	\
    }				\
  } while (0)

/* Clear the mark bit of cell V. */

#define rep_GC_CLR(v)		\
  do {				\
    if(rep_CELLP(v)) {		\
      rep_GC_CLR_CELL(v);	\
    } else {			\
      rep_GC_CLR_CONS(v);	\
    }				\
  } while (0)

/* Recursively mark object V. */

#define rep_MARKVAL(v)					\
  do {							\
    if(v != 0 && !rep_INTP(v) && !rep_GC_MARKEDP(v)) {	\
      rep_mark_value(v);				\
    }							\
  } while (0)

/* A stack of dynamic GC roots, i.e. objects to start marking from. */

typedef struct rep_gc_root {
  repv *ptr;
  struct rep_gc_root *next;
} rep_GC_root;

typedef struct rep_gc_n_roots {
  repv *first;
  int count;
  struct rep_gc_n_roots *next;
} rep_GC_n_roots;

/* Push a root to VAL using ROOT as storage (ROOT is rep_GC_root type). */

#define rep_PUSHGC(root, val)		\
  do {					\
    (root).ptr = &(val);		\
    (root).next = rep_gc_root_stack;	\
    rep_gc_root_stack = &(root);	\
  } while (0)

/* Push a root to N values starting at PTR using ROOT as storage (ROOT
   is rep_GC_n_roots type). */

#define rep_PUSHGCN(root, ptr, n)	\
  do {					\
    (root).first = (ptr);		\
    (root).count = (n);			\
    (root).next = rep_gc_n_roots_stack;	\
    rep_gc_n_roots_stack = &(root);	\
  } while (0)

#if !rep_PARANOID_GC

# define rep_CHECK_GC(root) do {} while (0)

#else /* !rep_PARANOID_GC */

/* Check that GC roots are popped when they should have been. */

# if STACK_DIRECTION < 0
#  define rep_CHECK_GC(root)		\
  do {					\
    char x__ = 1;			\
    assert(&x__ <= (char *)root);	\
  } while (0)
# elif STACK_DIRECTION > 0
#  define rep_CHECK_GC(root)		\
  do {					\
    char x__ = 1;			\
    assert(&x__ >= (char *)root);	\
  } while (0)
# else
#  define rep_CHECK_GC(root) do {} while (0)
# endif

#endif /* rep_PARANOID_GC */

#define rep_POPGC 					\
  do {							\
    rep_CHECK_GC(rep_gc_root_stack);			\
    rep_gc_root_stack = rep_gc_root_stack->next;	\
  } while (0)

#define rep_POPGCN 					\
  do {							\
    rep_CHECK_GC(rep_gc_n_roots_stack);			\
    rep_gc_n_roots_stack = rep_gc_n_roots_stack->next;	\
  } while (0)


/* Macros for declaring functions */

/* Define a function named NAME (a string), whose function body will
   be called FSYM, whose rep_subr will be called SSYM, with argument
   list ARGS, of type code TYPE. */

#define DEFUN(name,fsym,ssym,args,type)					\
  DEFSTRING(rep_CONCAT(ssym, __name), name);				\
  extern repv fsym args;						\
  rep_ALIGN_CELL(rep_xsubr ssym) = {					\
    type, (repv (*)()) fsym, rep_VAL(&rep_CONCAT(ssym, __name)), 0	\
  };									\
  repv fsym args

/* Same as above but with an extra arg -- an interactive-spec string. */

#define DEFUN_INT(name,fsym,ssym,args,type,interactive)			\
  DEFSTRING(rep_CONCAT(ssym, __name), name);				\
  DEFSTRING(rep_CONCAT(ssym, __int), interactive);			\
  extern repv fsym args;						\
  rep_ALIGN_CELL(rep_xsubr ssym) = {					\
    type, (repv (*)()) fsym, rep_VAL(&rep_CONCAT(ssym, __name)), 	\
    rep_VAL(&rep_CONCAT(ssym, __int))					\
  };									\
  repv fsym args

/* Add a subroutine. */    

#define rep_ADD_SUBR(subr) rep_add_subr(&subr, true)

/* Add a non-exported subroutine. */

#define rep_ADD_INTERNAL_SUBR(subr) rep_add_subr(&subr, false)

/* Add an interactive subroutine. */

#define rep_ADD_SUBR_INT(subr) rep_add_subr(&subr, true)

/* Declare a symbol stored in variable QX. */

#define DEFSYM(x, name) \
  repv Q ## x; DEFSTRING(str_ ## x, name)

/* Intern a symbol stored in QX, whose name (a lisp string) is stored
   in str_X (i.e. declared with DEFSYM). */

#define rep_INTERN(x) rep_intern_static(& Q ## x, rep_VAL(& str_ ## x))

/* Same as above, but also marks the variable as dynamically scoped. */

#define rep_INTERN_SPECIAL(x) 				\
  do {							\
    rep_INTERN(x);					\
    Fmake_variable_special (Q ## x);			\
    rep_SYM(Q ## x)->car |= rep_SF_DEFVAR;		\
  } while (0)

/* Add an error string called err_X for symbol stored in QX. */

#define rep_DEFINE_ERROR(x) \
  Fput(Q ## x, Qerror_message, rep_VAL(& err_ ## x))


/* Macros for ensuring an object is of a certain type i.e. to ensure
   first arg `foo' is a string, rep_DECLARE1(foo, rep_STRINGP);  */

#define rep_DECLARE(n,x,e)		\
  do { 					\
    if(!(e)) { 				\
      rep_signal_arg_error(x, n);	\
      return 0; 			\
    } 					\
  } while (0)

#define rep_DECLARE1(x,t) rep_DECLARE(1,x,t(x))
#define rep_DECLARE2(x,t) rep_DECLARE(2,x,t(x))
#define rep_DECLARE3(x,t) rep_DECLARE(3,x,t(x))
#define rep_DECLARE4(x,t) rep_DECLARE(4,x,t(x))
#define rep_DECLARE5(x,t) rep_DECLARE(5,x,t(x))

#define rep_DECLARE1_OPT(x,t) rep_DECLARE(1, x, (x) == rep_nil || t(x))
#define rep_DECLARE2_OPT(x,t) rep_DECLARE(2, x, (x) == rep_nil || t(x))
#define rep_DECLARE3_OPT(x,t) rep_DECLARE(3, x, (x) == rep_nil || t(x))
#define rep_DECLARE4_OPT(x,t) rep_DECLARE(4, x, (x) == rep_nil || t(x))
#define rep_DECLARE5_OPT(x,t) rep_DECLARE(5, x, (x) == rep_nil || t(x))


/* Macros for interrupt handling */

/* rep_TEST_INT is called within loops, to check for async input
   arriving containing user-interrupt sequences. */

#ifndef rep_TEST_INT_PERIOD
# define rep_TEST_INT_PERIOD 1000
#endif

#define rep_TEST_INT					\
  do {							\
    if (++rep_test_int_counter > rep_TEST_INT_PERIOD) {	\
      rep_test_interrupt();				\
    }							\
  } while (0)

#define rep_TEST_INT_SLOW	\
  do {				\
      rep_test_interrupt();	\
  } while (0)

/* Can be used by functions that loop calling rep_TEST_INT -- allocates
   the loop counter locally, to avoid costly global references. */

#define rep_TEST_INT_LOOP_COUNTER int rep_test_int_counter = 0

/* True when an interrupt has occurred; this means that the function
   should exit as soon as possible, returning 0. */

#define rep_INTERRUPTP (rep_throw_value != 0)

#endif /* REP_LISP_H */
