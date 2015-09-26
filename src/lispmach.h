/* lispmach.h -- Interpreter for compiled Lisp forms

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

/* free macros:

	ASSERT(expr)
	BYTECODE_PROFILE
	THREADED_VM
	BC_APPLY_SELF

   defined functions:

	bytecode_vm(repv code, repv consts, repv stack, int argc, repv *argv);
	inline_apply_bytecode(repv subr, int nargs, repv *args); */

/* Use the threaded interpreter with GNU CC. */

#ifdef __GNUC__
# define THREADED_VM 1
#endif

#include "bytecodes.h"

#include <string.h>

DEFSTRING(err_bytecode_error, "Byte-code error");
DEFSTRING(unknown_op, "Unknown lisp opcode");

static repv
bytecode_vm(repv code, repv consts, repv stack, int argc, repv *argv);


/* Helper functions. */

static repv
list_ref(repv list, int elt)
{
  while (rep_CONSP(list) && elt-- > 0) {
    list = rep_CDR(list);
  }
  return rep_CONSP(list) ? rep_CAR(list) : rep_nil;
}

/* Note this doesn't check LIST is longer than N elements. */

static inline repv
list_tail(repv list, int n)
{
  while (n-- > 0) {
    list = rep_CDR(list);
  }
  return list;
}

/* Unbind one level of the BIND-STACK and return the new head of the stack.
   Each item in the BIND-STACK may be one of:

   INTEGER
   -- variable binding frame

   (error . (PC . STACK-DEPTH))
   -- exception handler at PC

   The function returns the number of dynamic bindings removed. */

static int
unbind(repv item)
{
  if (item == rep_EMPTY_BINDING_FRAME) {
    return 0;
  } else if (rep_INTP(item)) {
    /* A variable binding frame. */
    rep_env = list_tail(rep_env, rep_LEX_BINDINGS(item));
    rep_special_env = list_tail(rep_special_env, rep_SPEC_BINDINGS(item));
    return rep_SPEC_BINDINGS(item);
  } else {
    return 0;
  }
}

static inline void
unbind_n(repv *ptr, int n)
{
  while (n-- > 0) {
    unbind(ptr[n]);
  }
}


/* Lisp VM. */

#define STACK_USAGE	(sp - stack)
#define TOP		(*sp)
#define POP		(*sp--)
#define POPN(n)		do {sp -= (n);} while (0)
#define PUSH(v)		do {*(++sp) = (v);} while (0)

#define BIND_USAGE	(bp - (bindings - 1))
#define BIND_RET_POP	(*bp--)
#define BIND_TOP	(*bp)
#define BIND_TOP_P	(bp < bindings)
#define BIND_PUSH(x)	(*(++bp) = (x))

#define CHECK_NEXT	 					\
  do {								\
    ASSERT(STACK_USAGE <= stack_size);				\
    ASSERT(((char *)pc - pc_base) < rep_STRING_LEN(code));	\
  } while (0)

#ifdef BYTECODE_PROFILE
# define PROFILE_NEXT do {bytecode_profile[*pc]++;} while (0)
#else
# define PROFILE_NEXT
#endif

#define SAFE_NEXT__	\
  do {			\
    CHECK_NEXT;		\
    PROFILE_NEXT;	\
    X_SAFE_NEXT;	\
  } while (0)

#define SAFE_NEXT SAFE_NEXT__

#define FETCH		(*pc++)
#define FETCH2(var)	((var) = (FETCH << ARG_SHIFT), (var) += FETCH)

#define SYNC_GC				\
  do {					\
    gc_stack.count = STACK_USAGE;	\
    gc_bindings.count = BIND_USAGE;	\
  } while (0)

/* These macros pop as many args as required then call the specified
   function properly. */

#define CALL_1(cmd)	\
  do {			\
    TOP = cmd(TOP);	\
    NEXT;		\
  } while (0)
    
#define CALL_2(cmd)		\
  do {				\
    repv arg2 = POP;		\
    TOP = cmd(TOP, arg2);	\
    NEXT;			\
  } while (0)

#define CALL_3(cmd)			\
  do {					\
    repv arg3 = POP;			\
    repv arg2 = POP;			\
    TOP = cmd(TOP, arg2, arg3);		\
    NEXT;				\
  } while (0)

/* We used to check for both rep_throw_value != 0, and TOP == 0. But since
   rep_throw_value is a (volatile) global, this is slower than just
   checking TOP (by about 1%) */

#define ERROR_OCCURRED_P (TOP == 0)

#ifndef THREADED_VM

/* Non-threaded interpretation, just use a big switch statement in
   a while loop. */

# define BEGIN_DISPATCH fetch: switch (FETCH) {
# define END_DISPATCH }

/* Output the case statement for an instruction OP, with an embedded
   argument. The code for the instruction should start at the following
   piece of code. */

# define INSN_WITH_ARG(op)			\
  case op+7:					\
    FETCH2(arg); goto rep_CONCAT(op_, op);	\
  case op+6:					\
    arg = FETCH; goto rep_CONCAT(op_, op);	\
  case op: case op+1: case op+2:		\
  case op+3: case op+4: case op+5:		\
    arg = pc[-1] - op;				\
    rep_CONCAT(op_, op):

# define INSN(op) case op:
# define DEFAULT_INSN default:

# define X_SAFE_NEXT	goto fetch
# define INLINE_NEXT	if (!ERROR_OCCURRED_P) SAFE_NEXT; else HANDLE_ERROR
# define NEXT		goto check_error
# define RETURN		goto quit
# define HANDLE_ERROR	goto error

#else /* !THREADED_VM */

/* Indirect threading, as described in: A Portable Forth Engine.

   @InProceedings{ertl93,
     author =       "M. Anton Ertl",
     title =        "A Portable {Forth} Engine",
     booktitle =    "EuroFORTH '93 conference proceedings",
     year =         "1993",
     address =      "Mari\'ansk\'e L\'azn\`e (Marienbad)",
     url =          "http://www.complang.tuwien.ac.at/papers/ertl93.ps.Z",
   }

   the intitial implementation by Ceri Storey, completed by John Harper. */

# define BEGIN_DISPATCH SAFE_NEXT; {
# define END_DISPATCH }

# define TAG(op)	rep_CONCAT(insn_, op)
# define TAG0(op)	rep_CONCAT(insn_0_, op)
# define TAG1(op)	rep_CONCAT(insn_1_, op)
# define TAG2(op)	rep_CONCAT(insn_2_, op)
# define TAG_DEFAULT	insn_default

# define INSN(op) TAG(op):
# define DEFAULT_INSN TAG_DEFAULT:

# define INSN_WITH_ARG(op)		\
  TAG2(op):				\
    FETCH2(arg); goto TAG(op);		\
  TAG1(op):				\
    arg = FETCH; goto TAG(op);		\
  TAG0(op):				\
    arg = pc[-1] - op;			\
    INSN(op)

# define X_SAFE_NEXT	goto *cfa[FETCH]
# define INLINE_NEXT	if (!ERROR_OCCURRED_P) SAFE_NEXT; else HANDLE_ERROR
# define NEXT		goto check_error
# define RETURN		goto quit
# define HANDLE_ERROR	goto error

# define JUMP_TABLE							\
  /* 0x00 */								\
  &&TAG(OP_REG_REF_0), &&TAG(OP_REG_REF_1), &&TAG(OP_REG_REF_2),	\
  &&TAG(OP_REG_REF_3), &&TAG(OP_REG_REF_4), &&TAG(OP_REG_REF_5),	\
  &&TAG(OP_REG_REF_6), &&TAG(OP_REG_REF_7), &&TAG0(OP_CALL),		\
  &&TAG0(OP_CALL), &&TAG0(OP_CALL), &&TAG0(OP_CALL), &&TAG0(OP_CALL),	\
  &&TAG0(OP_CALL), &&TAG1(OP_CALL), &&TAG2(OP_CALL),			\
  /* 0x10 */								\
  &&TAG0(OP_PUSH), &&TAG0(OP_PUSH), &&TAG0(OP_PUSH),			\
  &&TAG0(OP_PUSH), &&TAG0(OP_PUSH), &&TAG0(OP_PUSH),			\
  &&TAG1(OP_PUSH), &&TAG2(OP_PUSH), &&TAG0(OP_REFQ),			\
  &&TAG0(OP_REFQ), &&TAG0(OP_REFQ), &&TAG0(OP_REFQ),			\
  &&TAG0(OP_REFQ), &&TAG0(OP_REFQ), &&TAG1(OP_REFQ),			\
  &&TAG2(OP_REFQ),							\
  /* 0x20 */								\
  &&TAG0(OP_SETQ), &&TAG0(OP_SETQ), &&TAG0(OP_SETQ),			\
  &&TAG0(OP_SETQ), &&TAG0(OP_SETQ), &&TAG0(OP_SETQ),			\
  &&TAG1(OP_SETQ), &&TAG2(OP_SETQ), &&TAG0(OP_ENV_SET),			\
  &&TAG0(OP_ENV_SET), &&TAG0(OP_ENV_SET), &&TAG0(OP_ENV_SET),		\
  &&TAG0(OP_ENV_SET), &&TAG0(OP_ENV_SET), &&TAG1(OP_ENV_SET),		\
  &&TAG2(OP_ENV_SET),							\
  /* 0x30 */								\
  &&TAG(OP_REG_SET_0), &&TAG(OP_REG_SET_1), &&TAG(OP_REG_SET_2),	\
  &&TAG(OP_REG_SET_3), &&TAG(OP_REG_SET_4), &&TAG(OP_REG_SET_5),	\
  &&TAG(OP_REG_SET_6), &&TAG(OP_REG_SET_7), &&TAG(OP_ENV_REF_0),	\
  &&TAG(OP_ENV_REF_1), &&TAG(OP_ENV_REF_2), &&TAG(OP_ENV_REF_3),	\
  &&TAG(OP_ENV_REF_4), &&TAG(OP_ENV_REF_5), &&TAG(OP_ENV_REF_6),	\
  &&TAG(OP_ENV_REF_7),							\
  /* 0x40 */								\
  &&TAG(OP_REF), &&TAG(OP__SET), &&TAG(OP_FLUID_REF),			\
  &&TAG(OP_ENCLOSE), &&TAG(OP_PUSH_FRAME), &&TAG(OP_POP_FRAME),		\
  &&TAG(OP_DUP), &&TAG(OP_SWAP), &&TAG(OP_POP), &&TAG(OP_NIL),		\
  &&TAG(OP_T), &&TAG(OP_CONS), &&TAG(OP_CAR), &&TAG(OP_CDR),		\
  &&TAG(OP_SET_CAR), &&TAG(OP_SET_CDR),					\
  /* 0x50 */								\
  &&TAG(OP_LIST_REF), &&TAG(OP_LIST_TAIL),				\
  &&TAG(OP_ARRAY_SET), &&TAG(OP_ARRAY_REF),				\
  &&TAG(OP_LENGTH), &&TAG(OP_BIND), &&TAG(OP_ADD), &&TAG(OP_NEG),	\
  &&TAG(OP_SUB), &&TAG(OP_MUL), &&TAG(OP_DIV), &&TAG(OP_REM),		\
  &&TAG(OP_LNOT), &&TAG(OP_NOT), &&TAG(OP_LOR), &&TAG(OP_LAND),		\
  /* 0x60 */								\
  &&TAG(OP_EQUAL), &&TAG(OP_EQ), &&TAG(OP_STRUCT_REF),			\
  &&TAG(OP_LIST_LENGTH), &&TAG(OP_GT), &&TAG(OP_GE), &&TAG(OP_LT),	\
  &&TAG(OP_LE), &&TAG(OP_INC), &&TAG(OP_DEC), &&TAG(OP_ASH),		\
  &&TAG(OP_ZEROP), &&TAG(OP_NULL), &&TAG(OP_ATOM), &&TAG(OP_CONSP),	\
  &&TAG(OP_LISTP),							\
  /* 0x70 */								\
  &&TAG(OP_NUMBERP), &&TAG(OP_STRINGP), &&TAG(OP_VECTORP),		\
  &&TAG(OP_CATCH), &&TAG(OP_THROW), &&TAG(OP_BINDERR),			\
  &&TAG(OP_RETURN), &&TAG(OP_POP_FRAMES), &&TAG(OP_BOUNDP),		\
  &&TAG(OP_SYMBOLP), &&TAG(OP_GET), &&TAG(OP_PUT),			\
  &&TAG(OP_ERRORPRO), &&TAG(OP_SIGNAL), &&TAG(OP_QUOTIENT),		\
  &&TAG(OP_REVERSE),							\
  /* 0x80 */								\
  &&TAG(OP_NREVERSE), &&TAG(OP_ASSOC), &&TAG(OP_ASSQ),			\
  &&TAG(OP_RASSOC), &&TAG(OP_RASSQ), &&TAG(OP_LAST),			\
  &&TAG(OP_MAPCAR), &&TAG(OP_MAPC), &&TAG(OP_MEMBER),			\
  &&TAG(OP_MEMQ), &&TAG(OP_DELETE), &&TAG(OP_DELQ),			\
  &&TAG(OP_DELETE_IF), &&TAG(OP_DELETE_IF_NOT),				\
  &&TAG(OP_COPY_SEQUENCE), &&TAG(OP_SEQUENCEP),				\
  /* 0x90 */								\
  &&TAG(OP_FUNCTIONP), &&TAG(OP_SPECIAL_FORM_P), &&TAG(OP_SUBRP),	\
  &&TAG(OP_EQL), &&TAG(OP_LXOR), &&TAG(OP_MAX), &&TAG(OP_MIN),		\
  &&TAG(OP_FILTER), &&TAG(OP_MACROP), &&TAG(OP_BYTECODEP),		\
  &&TAG(OP_PUSHI0), &&TAG(OP_PUSHI1), &&TAG(OP_PUSHI2),			\
  &&TAG(OP_PUSHIM1), &&TAG(OP_PUSHIM2), &&TAG(OP_PUSHI),		\
  /* 0xa0 */								\
  &&TAG(OP_PUSHIWN), &&TAG(OP_PUSHIWP), &&TAG(OP_CAAR),			\
  &&TAG(OP_CADR), &&TAG(OP_CDAR), &&TAG(OP_CDDR),			\
  &&TAG(OP_CADDR), &&TAG(OP_CADDDR), &&TAG(OP_CADDDDR),			\
  &&TAG(OP_CADDDDDR), &&TAG(OP_CADDDDDDR),				\
  &&TAG(OP_CADDDDDDDR), &&TAG(OP_FLOOR), &&TAG(OP_CEILING),		\
  &&TAG(OP_TRUNCATE), &&TAG(OP_ROUND),					\
  /* 0xb0 */								\
  &&TAG(OP_APPLY), &&TAG(OP_ARRAY_LENGTH), &&TAG(OP_VECTOR_LENGTH),	\
  &&TAG(OP_EXP), &&TAG(OP_LOG), &&TAG(OP_SIN), &&TAG(OP_COS),		\
  &&TAG(OP_TAN), &&TAG(OP_SQRT), &&TAG(OP_EXPT), &&TAG(OP_SWAP2),	\
  &&TAG(OP_MODULO), &&TAG(OP_MAKE_CLOSURE), &&TAG(OP_RESET_FRAMES),	\
  &&TAG(OP_CLOSUREP), &&TAG(OP_POP_ALL),				\
  /* 0xc0 */								\
  &&TAG(OP_FLUID_SET), &&TAG(OP_FLUID_BIND), &&TAG(OP_MEMV),		\
  &&TAG(OP_NUM_EQ), &&TAG_DEFAULT, &&TAG_DEFAULT,			\
  &&TAG(OP__DEFINE), &&TAG(OP_SPEC_BIND), &&TAG(OP_SET),		\
  &&TAG(OP_REQUIRED_ARG), &&TAG(OP_OPTIONAL_ARG),			\
  &&TAG(OP_REST_ARG), &&TAG(OP_NOT_ZERO_P),				\
  &&TAG(OP_KEYWORD_ARG), &&TAG(OP_OPTIONAL_ARG_),			\
  &&TAG(OP_KEYWORD_ARG_),						\
  /* 0xd0 */								\
  &&TAG(OP_VECTOR_REF), &&TAG(OP_VECTOR_SET), &&TAG(OP_STRING_LENGTH),	\
  &&TAG(OP_STRING_REF), &&TAG(OP_STRING_SET), &&TAG(OP_UNDEFINED),	\
  &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT,		\
  &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT,		\
  &&TAG_DEFAULT, &&TAG_DEFAULT,						\
  /* 0xe0 */								\
  &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT,		\
  &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT,		\
  &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT,		\
  &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT,		\
  /* 0xf0 */								\
  &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT,		\
  &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT,		\
  &&TAG(OP_EJMP), &&TAG(OP_JPN), &&TAG(OP_JPT), &&TAG(OP_JMP),		\
  &&TAG(OP_JN), &&TAG(OP_JT), &&TAG(OP_JNP), &&TAG(OP_JTP)

#endif /* THREADED_VM */

DEFSTRING(max_depth, "max-lisp-depth exceeded, possible infinite recursion?");

static inline repv
inline_apply_bytecode(repv subr, int nargs, repv *args)
{
  return bytecode_vm(rep_BYTECODE_CODE(subr), rep_BYTECODE_CONSTANTS(subr),
		     rep_BYTECODE_STACK(subr), nargs, args);
}

static repv
call_subr(repv fun, int argc, repv *argv)
{
  switch (rep_SUBR_ARITY(fun)) {
  case 0:
    return rep_SUBR_F0(fun)();
    break;

  case 1:
    return rep_SUBR_F1(fun)(argc >= 1 ? argv[0] : rep_nil);
    break;

  case 2:
    switch (argc) {
    case 0:
      return rep_SUBR_F2(fun)(rep_nil, rep_nil);
      break;
    case 1:
      return rep_SUBR_F2(fun)(argv[0], rep_nil);
      break;
    default:
      return rep_SUBR_F2(fun)(argv[0], argv[1]);
      break;
    }
    break;

  case 3:
    switch (argc) {
    case 0:
      return rep_SUBR_F3(fun)(rep_nil, rep_nil, rep_nil);
      break;
    case 1:
      return rep_SUBR_F3(fun)(argv[0], rep_nil, rep_nil);
      break;
    case 2:
      return rep_SUBR_F3(fun)(argv[0], argv[1], rep_nil);
      break;
    default:
      return rep_SUBR_F3(fun)(argv[0], argv[1], argv[2]);
      break;
    }
    break;

  case 4:
    switch (argc) {
    case 0:
      return rep_SUBR_F4(fun)(rep_nil, rep_nil, rep_nil, rep_nil);
      break;
    case 1:
      return rep_SUBR_F4(fun)(argv[0], rep_nil, rep_nil, rep_nil);
      break;
    case 2:
      return rep_SUBR_F4(fun)(argv[0], argv[1], rep_nil, rep_nil);
      break;
    case 3:
      return rep_SUBR_F4(fun)(argv[0], argv[1], argv[2], rep_nil);
      break;
    default:
      return rep_SUBR_F4(fun)(argv[0], argv[1], argv[2], argv[3]);
      break;
    }
    break;

  case 5:
    switch (argc) {
    case 0:
      return rep_SUBR_F5(fun)(rep_nil, rep_nil, rep_nil, rep_nil, rep_nil);
      break;
    case 1:
      return rep_SUBR_F5(fun)(argv[0], rep_nil, rep_nil, rep_nil, rep_nil);
      break;
    case 2:
      return rep_SUBR_F5(fun)(argv[0], argv[1], rep_nil, rep_nil, rep_nil);
      break;
    case 3:
      return rep_SUBR_F5(fun)(argv[0], argv[1], argv[2], rep_nil, rep_nil);
      break;
    case 4:
      return rep_SUBR_F5(fun)(argv[0], argv[1], argv[2], argv[3], rep_nil);
      break;
    default:
      return rep_SUBR_F5(fun)(argv[0], argv[1], argv[2], argv[3], argv[4]);
      break;
    }
    break;

  case rep_SUBR_L: {
    repv lst = rep_nil;
    for (int i = argc - 1; i >= 0; i--) {
      lst = Fcons(argv[i], lst);
    }
    rep_call_stack->args = lst;
    return rep_SUBR_FL(fun)(lst);
    break; }

  case rep_SUBR_V:
    return rep_SUBR_FV(fun)(argc, argv);
    break;
  }

  return 0;
}

static repv
bytecode_vm(repv code, repv consts, repv req, int argc, repv *argv)
{
  rep_TEST_INT_LOOP_COUNTER;

  /* Actual reusable size of the argv array. I did try reusing the
     passed in argv, but that caused stack corruption in some cases.. */

  repv *argv_base = 0;
  int argv_size = 0;

  if (++rep_lisp_depth > rep_max_lisp_depth) {
    rep_lisp_depth--;
    return Fsignal(Qerror, rep_LIST_1(rep_VAL(&max_depth)));
  }

  /* When tail-calling we'll only allocate a new stack if the current
     is too small. (this guarantees bounded space requirements) */

  int stack_size = rep_INT(req) & 0x3ff;
  int bindings_size = (rep_INT(req) >> 10) & 0x3ff;
  int registers_size = rep_INT(req) >> 20;

  repv *stack = alloca(sizeof(repv) * (stack_size + 1));
  repv *bindings = alloca(sizeof(repv) * (bindings_size + 1));
  repv *registers = alloca(sizeof(repv) * registers_size);

  /* Clear registers initially, solely for GC. */

  for (int i = 0; i < registers_size; i++) {
    registers[i] = 0;
  }

  /* The `count` field is only filled in with the stack-size when
     there's a chance of gc. */

  rep_GC_root gc_code, gc_consts;
  rep_GC_n_roots gc_stack, gc_bindings, gc_registers, gc_argv;

#ifdef SLOW_GC_PROTECT

  rep_PUSHGC(gc_code, code);
  rep_PUSHGC(gc_consts, consts);
  rep_PUSHGCN(gc_bindings, bindings, 0);
  rep_PUSHGCN(gc_stack, stack + 1, 0);
  rep_PUSHGCN(gc_registers, registers, registers_size);
  rep_PUSHGCN(gc_argv, argv, argc);

#else

  /* Avoid multiple accesses to global variables, this ordering is
     assumed by popping code at end of fn ] */

  gc_code.ptr = &code;
  gc_consts.ptr = &consts;
  gc_bindings.first= bindings;
  gc_stack.first = stack + 1;
  gc_registers.first = registers;
  gc_registers.count = registers_size;
  gc_argv.first = argv;
  gc_argv.count = argc;

  gc_code.next = &gc_consts;
  gc_consts.next = rep_gc_root_stack;
  rep_gc_root_stack = &gc_code;

  gc_bindings.next = &gc_stack;
  gc_stack.next = &gc_registers;
  gc_registers.next = &gc_argv;
  gc_argv.next = rep_gc_n_roots_stack;
  rep_gc_n_roots_stack = &gc_bindings;

#endif
    
  /* Jump to this label when tail-calling */
again: {

  /* Make sure that even when the stack has no entries, the TOP element
     still != 0 (for the error-detection at label quit:) */

  stack[0] = Qt;

  /* Always start with a null frame. Functions will add their args */

  bindings[0] = rep_EMPTY_BINDING_FRAME;

  /* This is the number of dynamic `bindings' in effect, including
     non-variable bindings. */

  int impurity = 0;

  /* Number of function arguments that have been used. */

  int argptr = 0;

  /* Initialize the various virtual registers. */

  const uint8_t *pc_base = (const uint8_t *)rep_STR(code);
  const uint8_t *pc = pc_base;

  repv *sp = stack;
  repv *bp = bindings;
  repv *rp = registers;

  /* Start of the VM fetch-execute sequence. */
  {
#ifdef THREADED_VM
    static void *cfa__[256] = { JUMP_TABLE };
    void **cfa = cfa__;
#endif

    /* Shared argument register for INSN_WITH_ARG(). */

    int arg;

    BEGIN_DISPATCH

    INSN_WITH_ARG(OP_CALL) {

      /* Args are still available above the top of the stack, this just
         makes things a bit easier. */

      POPN(arg);
      repv fun = TOP;

      rep_stack_frame lc;
      lc.fun = fun;
      lc.args = rep_void;
      rep_PUSH_CALL(lc);

      SYNC_GC;

      bool was_closed = rep_CLOSUREP(fun);
      if (was_closed) {
	rep_USE_CLOSURE(fun);
	fun = rep_CLOSURE(fun)->fun;
      }

      if (rep_CELLP(fun) && !rep_CELL_CONS_P(fun)) {
	if (rep_CELL8_TYPE(fun) == rep_Subr) {
	  TOP = call_subr(fun, arg, sp + 1);
	} else if (was_closed && rep_CELL8_TYPE(fun) == rep_Bytecode) {
	  repv (*bc_apply) (repv, int, repv *) =
	    rep_STRUCTURE(rep_structure)->apply_bytecode;

	  if (bc_apply == BC_APPLY_SELF) {
	    if (impurity != 0 || *pc != OP_RETURN) {

	      /* Not a tail-call we can eliminate. */

	      TOP = inline_apply_bytecode(fun, arg, sp+1);

	    } else {

	      /* Tail-calling in place. */

	      rep_call_stack = lc.next;
	      rep_call_stack->fun = lc.fun;
	      rep_call_stack->args = lc.args;

	      /* Arguments for the function call */

	      argv = sp + 1;
	      argc = arg;

	      /* Switch old argv and stack, or reallocate? */

	      int n_stack_size = rep_INT(rep_BYTECODE_STACK(fun)) & 0x3ff;
	      if (argv_size >= n_stack_size) {
		/* argv is big enough to be new stack */
		repv *tem_stack = stack;
		int tem_size = stack_size;
		stack = argv_base;
		stack_size = argv_size;
		argv_base = tem_stack;
		argv_size = tem_size;
	      } else {
		argv_base = stack;
		argv_size = stack_size;
		stack = alloca(sizeof(repv) * (n_stack_size + 1));
		stack_size = n_stack_size;
	      }

	      code = fun;

	      /* Also called from F_APPLY. Inputs: code = bytecode-subr. */

	    do_tail_recursion:;

	      /* Allocate new bind-stack? */

	      int n_bindings_size
	        = (rep_INT(rep_BYTECODE_STACK(code)) >> 10) & 0x3ff;
	      if (bindings_size < n_bindings_size) {
		bindings = alloca(sizeof(repv) * (n_bindings_size + 1));
		bindings_size = n_bindings_size;
	      }

	      /* Allocate new registers? */

	      int n_registers_size = rep_INT(rep_BYTECODE_STACK(code)) >> 20;
	      if (registers_size < n_registers_size) {
		registers = alloca(sizeof(repv) * n_registers_size);
		registers_size = n_registers_size;
		for (int i = 0; i < registers_size; i++) {
		  registers[i] = 0;
		}
	      }

	      consts = rep_BYTECODE_CONSTANTS(code);
	      code = rep_BYTECODE_CODE(code);

	      gc_bindings.first = bindings;
	      gc_stack.first = stack + 1;
	      gc_registers.first = registers;
	      gc_registers.count = registers_size;
	      gc_argv.first = argv;
	      gc_argv.count = argc;

	      goto again;
	    }
	  } else {
	    TOP = bc_apply(fun, arg, sp+1);
	  }
	} else {
	  TOP = rep_value_type(fun)->apply(fun, argc, argv);
	}
	rep_POP_CALL(lc);
	INLINE_NEXT;
      } else {				/* not cell8 type */
	POPN(-arg);
	repv lst = rep_nil;
	while (arg-- > 0) {
	  repv x = POP;
	  lst = Fcons(x, lst);
	}
	rep_POP_CALL(lc);
	TOP = rep_apply(TOP, lst);
	NEXT;
      }
    }

    INSN_WITH_ARG(OP_PUSH) {
      ASSERT(arg < rep_VECTOR_LEN(consts));
      PUSH(rep_VECT(consts)->array[arg]);
      SAFE_NEXT;
    }

    INSN(OP_BIND) {
      repv value = POP;
      rep_env = Fcons(value, rep_env);
      BIND_TOP = rep_MARK_LEX_BINDING(BIND_TOP);
      SAFE_NEXT;
    }

    INSN(OP_SPEC_BIND) {
      repv sym = POP;
      repv value = POP;
      impurity++;
      BIND_TOP = rep_bind_special(BIND_TOP, sym, value);
      if (rep_throw_value) {
	HANDLE_ERROR;
      }
      NEXT;
    }

    INSN(OP_ENV_REF_0) {
      ASSERT(rep_list_length(rep_env) > 0);
      PUSH(rep_CAR(rep_env));
      SAFE_NEXT;
    }

    INSN(OP_ENV_REF_1) {
      ASSERT(rep_list_length(rep_env) > 1);
      PUSH(rep_CADR(rep_env));
      SAFE_NEXT;
    }

    INSN(OP_ENV_REF_2) {
      ASSERT(rep_list_length(rep_env) > 2);
      PUSH(rep_CADDR(rep_env));
      SAFE_NEXT;
    }

    INSN(OP_ENV_REF_3) {
      ASSERT(rep_list_length(rep_env) > 3);
      PUSH(rep_CADDDR(rep_env));
      SAFE_NEXT;
    }

    INSN(OP_ENV_REF_4) {
      ASSERT(rep_list_length(rep_env) > 4);
      PUSH(rep_CAR(rep_CDDDDR(rep_env)));
      SAFE_NEXT;
    }

    INSN(OP_ENV_REF_5) {
      ASSERT(rep_list_length(rep_env) > 5);
      PUSH(rep_CADR(rep_CDDDDR(rep_env)));
      SAFE_NEXT;
    }

    INSN(OP_ENV_REF_6) {
      arg = FETCH;
      ASSERT(rep_list_length(rep_env) > arg);
      PUSH(rep_CAR(list_tail(rep_env, arg)));
      SAFE_NEXT;
    }

    INSN(OP_ENV_REF_7) {
      FETCH2(arg);
      ASSERT(rep_list_length(rep_env) > arg);
      PUSH(rep_CAR(list_tail(rep_env, arg)));
      SAFE_NEXT;
    }

    INSN_WITH_ARG(OP_ENV_SET) {
      ASSERT(rep_list_length(rep_env) > arg);
      repv value = POP;
      rep_CAR(list_tail(rep_env, arg)) = value;
      SAFE_NEXT;
    }

    /* This code expanded from F_structure_ref() and lookup() in
       structures.c */

    INSN_WITH_ARG(OP_REFQ) {
      ASSERT(arg < rep_VECTOR_LEN(consts));
      repv var = rep_VECT(consts)->array[arg];
      rep_struct *s = rep_STRUCTURE(rep_structure);
      if (s->total_buckets != 0) {
	for (rep_struct_node *n
	     = s->buckets[rep_STRUCT_HASH(var, s->total_buckets)];
	     n; n = n->next)
	{
	  if (n->symbol == var) {
	    PUSH(n->binding);
	    SAFE_NEXT;
	  }
	}
      } else if (s->init) {
	TOP = F_structure_ref(rep_VAL(s), var);
	NEXT;
      }
      rep_struct_node *n = rep_search_imports(s, var);
      if (n) {
	PUSH(n->binding);
	SAFE_NEXT;
      }
      Fsignal(Qvoid_value, rep_LIST_1(var));
      HANDLE_ERROR;
    }

    INSN_WITH_ARG(OP_SETQ) {
      ASSERT(arg < rep_VECTOR_LEN(consts));
      repv sym = rep_VECT(consts)->array[arg];
      repv value = POP;
      Fstructure_set(rep_structure, sym, value);
      SAFE_NEXT;
    }

    INSN(OP_REG_REF_0) {
      ASSERT(registers_size > 0);
      PUSH(rp[0]);
      ASSERT(TOP != 0);
      SAFE_NEXT;
    }

    INSN(OP_REG_REF_1) {
      ASSERT(registers_size > 1);
      PUSH(rp[1]);
      ASSERT(TOP != 0);
      SAFE_NEXT;
    }

    INSN(OP_REG_REF_2) {
      ASSERT(registers_size > 2);
      PUSH(rp[2]);
      ASSERT(TOP != 0);
      SAFE_NEXT;
    }

    INSN(OP_REG_REF_3) {
      ASSERT(registers_size > 3);
      PUSH(rp[3]);
      ASSERT(TOP != 0);
      SAFE_NEXT;
    }

    INSN(OP_REG_REF_4) {
      ASSERT(registers_size > 4);
      PUSH(rp[4]);
      ASSERT(TOP != 0);
      SAFE_NEXT;
    }

    INSN(OP_REG_REF_5) {
      ASSERT(registers_size > 5);
      PUSH(rp[5]);
      ASSERT(TOP != 0);
      SAFE_NEXT;
    }

    INSN(OP_REG_REF_6) {
      arg = FETCH;
      ASSERT(registers_size > arg);
      PUSH(rp[arg]);
      ASSERT(TOP != 0);
      SAFE_NEXT;
    }

    INSN(OP_REG_REF_7) {
      FETCH2(arg);
      ASSERT(registers_size > arg);
      PUSH(rp[arg]);
      ASSERT(TOP != 0);
      SAFE_NEXT;
    }

    INSN(OP_REG_SET_0) {
      ASSERT(registers_size > 0);
      rp[0] = POP;
      SAFE_NEXT;
    }

    INSN(OP_REG_SET_1) {
      ASSERT(registers_size > 1);
      rp[1] = POP;
      SAFE_NEXT;
    }

    INSN(OP_REG_SET_2) {
      ASSERT(registers_size > 2);
      rp[2] = POP;
      SAFE_NEXT;
    }

    INSN(OP_REG_SET_3) {
      ASSERT(registers_size > 3);
      rp[3] = POP;
      SAFE_NEXT;
    }

    INSN(OP_REG_SET_4) {
      ASSERT(registers_size > 4);
      rp[4] = POP;
      SAFE_NEXT;
    }

    INSN(OP_REG_SET_5) {
      ASSERT(registers_size > 5);
      rp[5] = POP;
      SAFE_NEXT;
    }

    INSN(OP_REG_SET_6) {
      arg = FETCH;
      ASSERT(registers_size > arg);
      rp[arg] = POP;
      SAFE_NEXT;
    }

    INSN(OP_REG_SET_7) {
      FETCH2(arg);
      ASSERT(registers_size > arg);
      rp[arg] = POP;
      SAFE_NEXT;
    }

    INSN(OP_REF) {
      TOP = Fsymbol_value(TOP, rep_nil);
      NEXT;
    }

    INSN(OP__SET) {
      repv sym = POP;
      repv value = POP;
      Freal_set(sym, value);
      NEXT;
    }

    INSN(OP_FLUID_REF) {
      repv cell = rep_search_special_environment(TOP);
      if (cell != rep_nil) {
	TOP = rep_CDR(cell);
	SAFE_NEXT;
      } else if (rep_CONSP(TOP)) {
	TOP = rep_CDR(TOP);
	SAFE_NEXT;
      }
      Fsignal(Qvoid_value, rep_LIST_1(TOP));
      HANDLE_ERROR;
    }

    INSN(OP_ENCLOSE) {
      TOP = Fmake_closure(TOP, rep_nil);
      INLINE_NEXT;
    }

    INSN(OP_PUSH_FRAME) {
      ASSERT(BIND_USAGE < bindings_size + 1);
      BIND_PUSH(rep_EMPTY_BINDING_FRAME);
      SAFE_NEXT;
    }

    INSN(OP_POP_FRAME) {
      ASSERT(bp > bindings);
      impurity -= unbind(BIND_RET_POP);
      SAFE_NEXT;
    }

    INSN(OP_DUP) {
      repv tem = TOP;
      PUSH(tem);
      SAFE_NEXT;
    }

    INSN(OP_SWAP) {
      ASSERT(STACK_USAGE >= 2);
      repv tem = TOP;
      TOP = sp[-1];
      sp[-1] = tem;
      SAFE_NEXT;
    }

    INSN(OP_POP) {
      POP;
      SAFE_NEXT;
    }

    INSN(OP_NIL) {
      PUSH(rep_nil);
      SAFE_NEXT;
    }

    INSN(OP_T) {
      PUSH(Qt);
      SAFE_NEXT;
    }

    INSN(OP_CONS) {
      CALL_2(Fcons);
    }

    INSN(OP_CAR) {
      repv tem = TOP;
      if (rep_CONSP(tem)) {
	TOP = rep_CAR(tem);
      } else {
	TOP = rep_nil;
      }
      SAFE_NEXT;
    }

    INSN(OP_CDR) {
      repv tem = TOP;
      if (rep_CONSP(tem)) {
	TOP = rep_CDR(tem);
      } else {
	TOP = rep_nil;
      }
      SAFE_NEXT;
    }

    INSN(OP_SET_CAR) {
      CALL_2(Fset_car);
    }

    INSN(OP_SET_CDR) {
      CALL_2(Fset_cdr);
    }

    INSN(OP_LIST_REF) {
      CALL_2(Flist_ref);
    }

    INSN(OP_LIST_TAIL) {
      CALL_2(Flist_tail);
    }

    INSN(OP_ARRAY_SET) {
      CALL_3(Faset);
    }

    INSN(OP_ARRAY_REF) {
      CALL_2(Faref);
    }

    INSN(OP_LENGTH) {
      CALL_1(Flength);
    }

    INSN(OP_ADD) {
      /* Open-code fixnum arithmetic */
      repv arg2 = POP;
      repv arg1 = TOP;
      if (rep_INTP_2(arg1, arg2)) {
#if defined(HAVE_OVERFLOW_BUILTINS) && INTPTR_MAX == LONG_MAX
	/* see rep_number_add(). */
	long x;
	if (!__builtin_saddl_overflow(arg1, arg2 - 2, &x)) {
	  TOP = (repv)x;
	  SAFE_NEXT;
	}
#else
	intptr_t x = rep_INT(arg1) + rep_INT(arg2);
	if (x >= rep_LISP_MIN_INT && x <= rep_LISP_MAX_INT) {
	  TOP = rep_MAKE_INT(x);
	  SAFE_NEXT;
	}
#endif
      }
      TOP = rep_number_add(arg1, arg2);
      INLINE_NEXT;
    }

    INSN(OP_NEG) {
      /* Open-code fixnum arithmetic */
      repv tem = TOP;
      if (rep_INTP(tem)) {
#if defined(HAVE_OVERFLOW_BUILTINS) && INTPTR_MAX == LONG_MAX
	/* see rep_number_neg(). */
	long x;
	if (!__builtin_ssubl_overflow(4, tem, &x)) {
	  TOP = (repv)x;
	  SAFE_NEXT;
	}
#else
	intptr_t x = - rep_INT(tem);
	if (x >= rep_LISP_MIN_INT && x <= rep_LISP_MAX_INT) {
	  TOP = rep_MAKE_INT(x);
	  SAFE_NEXT;
	}
#endif
      }
      TOP = rep_number_neg(tem);
      INLINE_NEXT;
    }

    INSN(OP_SUB) {
      /* Open-code fixnum arithmetic */
      repv arg2 = POP;
      repv arg1 = TOP;
      if (rep_INTP_2(arg1, arg2)) {
#if defined(HAVE_OVERFLOW_BUILTINS) && INTPTR_MAX == LONG_MAX
	/* see rep_number_sub(). */
	long x;
	if (!__builtin_ssubl_overflow(arg1, arg2 - 2, &x)) {
	  TOP = (repv)x;
	  SAFE_NEXT;
	}
#else
	intptr_t x = rep_INT(arg1) - rep_INT(arg2);
	if (x >= rep_LISP_MIN_INT && x <= rep_LISP_MAX_INT) {
	  TOP = rep_MAKE_INT(x);
	  SAFE_NEXT;
	}
#endif
      }
      TOP = rep_number_sub(arg1, arg2);
      INLINE_NEXT;
    }

    INSN(OP_MUL) {
      repv arg2 = POP;
      repv arg1 = TOP;
#if defined(HAVE_OVERFLOW_BUILTINS) && INTPTR_MAX == LONG_MAX
      if (rep_INTP_2(arg1, arg2)) {
	/* see rep_number_mul(). */
	long x;
	if (!__builtin_smull_overflow(arg1 - 2, rep_INT(arg2), &x)
	    && !__builtin_saddl_overflow(x, 2, &x)) {
	  TOP = x;
	  SAFE_NEXT;
	}
      }
#endif
      TOP = rep_number_sub(arg1, arg2);
      NEXT;
    }

    INSN(OP_DIV) {
      CALL_2(rep_number_div);
    }

    INSN(OP_REM) {
      CALL_2(Fremainder);
    }

    INSN(OP_LNOT) {
      CALL_1(Flognot);
    }

    INSN(OP_NOT) {
      if (TOP == rep_nil) {
	TOP = Qt;
      } else {
	TOP = rep_nil;
      }
      SAFE_NEXT;
    }

    INSN(OP_NULL) {
      if (TOP == rep_nil) {
	TOP = Qt;
      } else {
	TOP = rep_nil;
      }
      SAFE_NEXT;
    }

    INSN(OP_LOR) {
      CALL_2(rep_number_logior);
    }

    INSN(OP_LXOR) {
      CALL_2(rep_number_logxor);
    }

    INSN(OP_LAND) {
      CALL_2(rep_number_logand);
    }

    INSN(OP_EQUAL) {
      repv tem = POP;
      TOP = (rep_value_cmp(TOP, tem) == 0) ? Qt : rep_nil;
      NEXT;
    }

    INSN(OP_EQ) {
      repv tem = POP;
      TOP = (TOP == tem) ? Qt : rep_nil;
      SAFE_NEXT;
    }

    INSN(OP_STRUCT_REF) {
      CALL_2(Fexternal_structure_ref);
    }

    INSN(OP_LIST_LENGTH) {
      CALL_1(Flist_length);
    }

    INSN(OP_GT) {
      repv arg2 = POP;
      repv arg1 = TOP;
      if (rep_INTP_2(arg1, arg2)) {
	TOP = rep_INT(arg1) > rep_INT(arg2) ? Qt : rep_nil;
	SAFE_NEXT;
      } else if (rep_NUMBERP(arg1) || rep_NUMBERP(arg2)) {
	TOP = rep_compare_numbers(arg1, arg2) > 0 ? Qt : rep_nil;
	SAFE_NEXT;
      } else {
	TOP = rep_value_cmp(arg1, arg2) > 0 ? Qt : rep_nil;
	NEXT;
      }
    }

    INSN(OP_GE) {
      repv arg2 = POP;
      repv arg1 = TOP;
      if (rep_INTP_2(arg1, arg2)) {
	TOP = rep_INT(arg1) >= rep_INT(arg2) ? Qt : rep_nil;
	SAFE_NEXT;
      } else if (rep_NUMBERP(arg1) || rep_NUMBERP(arg2)) {
	TOP = rep_compare_numbers(arg1, arg2) >= 0 ? Qt : rep_nil;
	SAFE_NEXT;
      } else {
	TOP = rep_value_cmp(arg1, arg2) >= 0 ? Qt : rep_nil;
	NEXT;
      }
    }

    INSN(OP_LT) {
      repv arg2 = POP;
      repv arg1 = TOP;
      if (rep_INTP_2(arg1, arg2)) {
	TOP = rep_INT(arg1) < rep_INT(arg2) ? Qt : rep_nil;
	SAFE_NEXT;
      } else if (rep_NUMBERP(arg1) || rep_NUMBERP(arg2)) {
	TOP = rep_compare_numbers(arg1, arg2) < 0 ? Qt : rep_nil;
	SAFE_NEXT;
      } else {
	TOP = rep_value_cmp(arg1, arg2) < 0 ? Qt : rep_nil;
	NEXT;
      }
    }

    INSN(OP_LE) {
      repv arg2 = POP;
      repv arg1 = TOP;
      if (rep_INTP_2(arg1, arg2)) {
	TOP = rep_INT(arg1) <= rep_INT(arg2) ? Qt : rep_nil;
	SAFE_NEXT;
      } else if (rep_NUMBERP(arg1) || rep_NUMBERP(arg2)) {
	TOP = rep_compare_numbers(arg1, arg2) <= 0 ? Qt : rep_nil;
	SAFE_NEXT;
      } else {
	TOP = rep_value_cmp(arg1, arg2) <= 0 ? Qt : rep_nil;
	NEXT;
      }
    }

    INSN(OP_INC) {
      repv tem = TOP;
      if (rep_INTP(tem)) {
#if defined(HAVE_OVERFLOW_BUILTINS) && INTPTR_MAX == LONG_MAX
	/* see Fplus1(). */
	long x;
	if (!__builtin_saddl_overflow(tem, 4, &x)) {
	  TOP = (repv)x;
	  SAFE_NEXT;
	}
#else
	intptr_t x = rep_INT(tem) + 1;
	if (x <= rep_LISP_MAX_INT) {
	  TOP = rep_MAKE_INT(x);
	  SAFE_NEXT;
	}
#endif
      }
      TOP = Fplus1(tem);
      NEXT;
    }

    INSN(OP_DEC) {
      repv tem = TOP;
      if (rep_INTP(tem)) {
#if defined(HAVE_OVERFLOW_BUILTINS) && INTPTR_MAX == LONG_MAX
	/* see Fsub1(). */
	long x;
	if (!__builtin_ssubl_overflow(tem, 4, &x)) {
	  TOP = (repv)x;
	  SAFE_NEXT;
	}
#else
	intptr_t x = rep_INT(tem) - 1;
	if (x >= rep_LISP_MIN_INT) {
	  TOP = rep_MAKE_INT(x);
	  SAFE_NEXT;
	}
#endif
      }
      TOP = Fsub1(tem);
      NEXT;
    }

    INSN(OP_ASH) {
      CALL_2(Fash);
    }

    INSN(OP_ZEROP) {
      repv tem = TOP;
      if (rep_INTP(tem)) {
	TOP = (tem == rep_MAKE_INT(0)) ? Qt : rep_nil;
	SAFE_NEXT;
      }
      TOP = Fzerop(tem);
      NEXT;
    }

    INSN(OP_NOT_ZERO_P) {
      repv tem = TOP;
      if (rep_INTP(tem)) {
	TOP = (tem != rep_MAKE_INT(0)) ? Qt : rep_nil;
	SAFE_NEXT;
      }
      tem = Fzerop(tem);
      if (tem) {
	tem = tem == rep_nil ? Qt : rep_nil;
      }
      TOP = tem;
      NEXT;
    }

    INSN(OP_ATOM) {
      if (!rep_CONSP(TOP)) {
	TOP = Qt;
      } else {
	TOP = rep_nil;
      }
      SAFE_NEXT;
    }

    INSN(OP_CONSP) {
      if (rep_CONSP(TOP)) {
	TOP = Qt;
      } else {
	TOP = rep_nil;
      }
      SAFE_NEXT;
    }

    INSN(OP_LISTP) {
      if (rep_CONSP(TOP) || rep_NILP(TOP)) {
	TOP = Qt;
      } else {
	TOP = rep_nil;
      }
      SAFE_NEXT;
    }

    INSN(OP_NUMBERP) {
      if (rep_NUMERICP(TOP)) {
	TOP = Qt;
      } else {
	TOP = rep_nil;
      }
      SAFE_NEXT;
    }

    INSN(OP_STRINGP) {
      if (rep_STRINGP(TOP)) {
	TOP = Qt;
      } else {
	TOP = rep_nil;
      }
      SAFE_NEXT;
    }

    INSN(OP_VECTORP) {
      if (rep_VECTORP(TOP)) {
	TOP = Qt;
      } else {
	TOP = rep_nil;
      }
      SAFE_NEXT;
    }

    /* This takes two arguments, TAG and THROW-VALUE. THROW-VALUE is
       the saved copy of rep_throw_value, if (car THROW-VALUE) == TAG
       we match, and we leave two values on the stack, nil on top (to
       pacify EJMP), (cdr THROW-VALUE) below that. */

    INSN(OP_CATCH) {
      repv tag = POP;
      repv value = TOP;		/* rep_throw_value */
      if (rep_CONSP(value) && rep_CAR(value) == tag) {
	TOP = rep_CDR(value);	/* leave result at stk[1] */
	PUSH(rep_nil);		/* cancel error */
      }
      SAFE_NEXT;
    }

    INSN(OP_THROW) {
      repv value = POP;
      if (!rep_throw_value) {
	rep_throw_value = Fcons(TOP, value);
	HANDLE_ERROR;
      }
      SAFE_NEXT;
    }

    /* Pop our single argument and cons it onto the bind-stack in a
       pair with the current stack-pointer. This installs an address in
       the code string as an error handler. */

    INSN(OP_BINDERR) {
      repv handler = POP;
      ASSERT(BIND_USAGE < bindings_size + 1);
      BIND_PUSH(Fcons(Qerror, Fcons(handler, rep_MAKE_INT(STACK_USAGE))));
      impurity++;
      SAFE_NEXT;
    }

    INSN(OP_RETURN) {
      unbind_n(bindings, BIND_USAGE);
      RETURN;
    }

    INSN(OP_POP_FRAMES) {
      unbind_n(bindings + 1, BIND_USAGE - 1);
      bp = bindings;
      impurity = rep_SPEC_BINDINGS(BIND_TOP);
      SAFE_NEXT;
    }

    INSN(OP_BOUNDP) {
      CALL_1(Fboundp);
    }

    INSN(OP_SYMBOLP) {
      if (rep_SYMBOLP(TOP)) {
	TOP = Qt;
      } else {
	TOP = rep_nil;
      }
      SAFE_NEXT;
    }

    INSN(OP_GET) {
      CALL_2(Fget);
    }

    INSN(OP_PUT) {
      CALL_3(Fput);
    }

    /* This should be called with two values on the stack.
       1. conditions of the error handler
       2. rep_throw_value of the exception

       This function pops(1) and tests it against the error in(2). If
       they match it sets(2) to nil, and binds the error data to the
       next lexical binding. */

    INSN(OP_ERRORPRO) {
      repv tem = POP;
      if (rep_CONSP(TOP) && rep_CAR(TOP) == Qerror
	  && rep_compare_error(rep_CDR(TOP), tem))
      {
	/* The handler matches the error. */
	tem = rep_CDR(TOP);	/* the error data */
	rep_env = Fcons(tem, rep_env);
	ASSERT(BIND_USAGE < bindings_size + 1);
	BIND_PUSH(rep_MARK_LEX_BINDING(rep_EMPTY_BINDING_FRAME));
	TOP = rep_nil;
      }
      NEXT;
    }

    INSN(OP_SIGNAL) {
      SYNC_GC;
      CALL_2(Fsignal);
    }

    INSN(OP_QUOTIENT) {
      CALL_2(Fquotient);
    }

    INSN(OP_REVERSE) {
      CALL_1(Freverse);
    }

    INSN(OP_NREVERSE) {
      CALL_1(Fnreverse);
    }

    INSN(OP_ASSOC) {
      CALL_2(Fassoc);
    }

    INSN(OP_ASSQ) {
      CALL_2(Fassq);
    }

    INSN(OP_RASSOC) {
      CALL_2(Frassoc);
    }

    INSN(OP_RASSQ) {
      CALL_2(Frassq);
    }

    INSN(OP_LAST) {
      CALL_1(Flast);
    }

    INSN(OP_MAPCAR) {
      SYNC_GC;
      CALL_2(Fmapcar);
    }

    INSN(OP_MAPC) {
      SYNC_GC;
      CALL_2(Fmapc);
    }

    INSN(OP_MEMBER) {
      CALL_2(Fmember);
    }

    INSN(OP_MEMQ) {
      CALL_2(Fmemq);
    }

    INSN(OP_DELETE) {
      CALL_2(Fdelete);
    }

    INSN(OP_DELQ) {
      CALL_2(Fdelq);
    }

    INSN(OP_DELETE_IF) {
      SYNC_GC;
      CALL_2(Fdelete_if);
    }

    INSN(OP_DELETE_IF_NOT) {
      SYNC_GC;
      CALL_2(Fdelete_if_not);
    }

    INSN(OP_COPY_SEQUENCE) {
      CALL_1(Fcopy_sequence);
    }

    INSN(OP_SEQUENCEP) {
      CALL_1(Fsequencep);
    }

    INSN(OP_FUNCTIONP) {
      CALL_1(Ffunctionp);
    }

    INSN(OP_SPECIAL_FORM_P) {
      CALL_1(Fspecial_form_p);
    }

    INSN(OP_SUBRP) {
      CALL_1(Fsubrp);
    }

    INSN(OP_EQL) {
      CALL_2(Feql);
    }

    INSN(OP_MAX) {
      CALL_2(rep_number_max);
    }

    INSN(OP_MIN) {
      CALL_2(rep_number_min);
    }

    INSN(OP_FILTER) {
      SYNC_GC;
      CALL_2(Ffilter);
    }

    INSN(OP_MACROP) {
      CALL_1(Fmacrop);
    }

    INSN(OP_BYTECODEP) {
      CALL_1(Fbytecodep);
    }

    INSN(OP_PUSHI0) {
      PUSH(rep_MAKE_INT(0));
      SAFE_NEXT;
    }

    INSN(OP_PUSHI1) {
      PUSH(rep_MAKE_INT(1));
      SAFE_NEXT;
    }

    INSN(OP_PUSHI2) {
      PUSH(rep_MAKE_INT(2));
      SAFE_NEXT;
    }

    INSN(OP_PUSHIM1) {
      PUSH(rep_MAKE_INT(-1));
      SAFE_NEXT;
    }
 
    INSN(OP_PUSHIM2) {
      PUSH(rep_MAKE_INT(-2));
      SAFE_NEXT;
    }

    INSN(OP_PUSHI) {
      arg = FETCH;
      if (arg < 128) {
	PUSH(rep_MAKE_INT(arg));
      } else {
	PUSH(rep_MAKE_INT(((int) arg) - 256));
      }
      SAFE_NEXT;
    }

    INSN(OP_PUSHIWN) {
      FETCH2(arg);
      PUSH(rep_MAKE_INT(- ((int) arg)));
      SAFE_NEXT;
    }

    INSN(OP_PUSHIWP) {
      FETCH2(arg);
      PUSH(rep_MAKE_INT(arg));
      SAFE_NEXT;
    }

    INSN(OP_CAAR) {
      repv tem = TOP;
      if (rep_CONSP(tem) && rep_CONSP(rep_CAR(tem))) {
	TOP = rep_CAAR(tem);
      } else {
	TOP = rep_nil;
      }
      SAFE_NEXT;
    }

    INSN(OP_CADR) {
      repv tem = TOP;
      if (rep_CONSP(tem) && rep_CONSP(rep_CDR(tem))) {
	TOP = rep_CADR(tem);
      } else {
	TOP = rep_nil;
      }
      SAFE_NEXT;
    }

    INSN(OP_CDAR) {
      repv tem = TOP;
      if (rep_CONSP(tem) && rep_CONSP(rep_CAR(tem))) {
	TOP = rep_CDAR(tem);
      } else {
	TOP = rep_nil;
      }
      SAFE_NEXT;
    }

    INSN(OP_CDDR) {
      repv tem = TOP;
      if (rep_CONSP(tem) && rep_CONSP(rep_CDR(tem))) {
	TOP = rep_CDDR(tem);
      } else {
	TOP = rep_nil;
      }
      SAFE_NEXT;
    }

    INSN(OP_CADDR) {
      TOP = list_ref(TOP, 2);
      SAFE_NEXT;
    }

    INSN(OP_CADDDR) {
      TOP = list_ref(TOP, 3);
      SAFE_NEXT;
    }

    INSN(OP_CADDDDR) {
      TOP = list_ref(TOP, 4);
      SAFE_NEXT;
    }

    INSN(OP_CADDDDDR) {
      TOP = list_ref(TOP, 5);
      SAFE_NEXT;
    }

    INSN(OP_CADDDDDDR) {
      TOP = list_ref(TOP, 6);
      SAFE_NEXT;
    }

    INSN(OP_CADDDDDDDR) {
      TOP = list_ref(TOP, 7);
      SAFE_NEXT;
    }

    INSN(OP_FLOOR) {
      CALL_1(Ffloor);
    }

    INSN(OP_CEILING) {
      CALL_1(Fceiling);
    }

    INSN(OP_TRUNCATE) {
      CALL_1(Ftruncate);
    }

    INSN(OP_ROUND) {
      CALL_1(Fround);
    }

    INSN(OP_APPLY) {
      repv args = POP;
      repv fun = TOP;
      SYNC_GC;
      if (impurity == 0 && *pc == OP_RETURN && rep_CLOSUREP(fun)
	  && rep_BYTECODEP(rep_CLOSURE(fun)->fun)
	  && rep_STRUCTURE(rep_CLOSURE(fun)->structure)->apply_bytecode == 0)
      {
	rep_USE_CLOSURE(fun);
	fun = rep_CLOSURE(fun)->fun;
	int nargs = rep_list_length(args);
	if (nargs < 0) {
	  HANDLE_ERROR;
	}
	if (nargs <= argv_size) {
	  argv = argv_base;
	} else {
	  /* Can't just copy over argv, reallocate */
	  argv = alloca(sizeof(repv) * nargs);
	  argv_base = argv; argv_size = nargs;
	}
	for (int i = 0; i < nargs; i++) {
	  argv[i] = rep_CAR(args);
	  args = rep_CDR(args);
	}
	argc = nargs;
	int n_stack_size = rep_INT(rep_BYTECODE_STACK(fun)) & 0x3ff;
	if (n_stack_size > stack_size) {
	  stack = alloca(sizeof(repv) * (n_stack_size + 1));
	  stack_size = n_stack_size;
	}
	code = fun;
	goto do_tail_recursion;	/* passes `code' */
      }
      /* not a tail call */
      TOP = rep_apply(fun, args);
      NEXT;
    }

    INSN(OP_ARRAY_LENGTH) {
      CALL_1(Farray_length);
    }

    INSN(OP_VECTOR_LENGTH) {
      CALL_1(Fvector_length);
    }

    INSN(OP_EXP) {
      CALL_1(Fexp);
    }

    INSN(OP_LOG) {
      CALL_1(Flog);
    }

    INSN(OP_COS) {
      CALL_1(Fcos);
    }

    INSN(OP_SIN) {
      CALL_1(Fsin);
    }

    INSN(OP_TAN) {
      CALL_1(Ftan);
    }

    INSN(OP_SQRT) {
      CALL_1(Fsqrt);
    }

    INSN(OP_EXPT) {
      CALL_2(Fexpt);
    }

    INSN(OP_SWAP2) {
      ASSERT(STACK_USAGE >= 3);
      repv tem = TOP;
      TOP = sp[-1];
      sp[-1] = sp[-2];
      sp[-2] = tem;
      SAFE_NEXT;
    }

    INSN(OP_MODULO) {
      CALL_2(Fmod);
    }

    INSN(OP_MAKE_CLOSURE) {
      CALL_2(Fmake_closure);
    }

    INSN(OP_RESET_FRAMES) {
      unbind_n(bindings, BIND_USAGE);
      bp = bindings - 1;
      impurity = 0;
      SAFE_NEXT;
    }

    INSN(OP_CLOSUREP) {
      if (rep_CLOSUREP(TOP)) {
	TOP = Qt;
      } else {
	TOP = rep_nil;
      }
      SAFE_NEXT;
    }

    INSN(OP_POP_ALL) {
      sp = stack;
      SAFE_NEXT;
    }

    INSN(OP_FLUID_SET) {
      CALL_2(Ffluid_set);
    }

    INSN(OP_FLUID_BIND) {
      repv arg2 = POP;
      repv arg1 = POP;
      rep_special_env = Fcons(Fcons(arg1, arg2), rep_special_env);
      BIND_TOP = rep_MARK_SPEC_BINDING(BIND_TOP);
      impurity++;
      SAFE_NEXT;
    }

    INSN(OP_MEMV) {
      CALL_2(Fmemql);
    }

    INSN(OP_NUM_EQ) {
      repv arg2 = POP;
      repv arg1 = TOP;
      if (rep_INTP_2(arg1, arg2)) {
	TOP = arg1 == arg2 ? Qt : rep_nil;
	SAFE_NEXT;
      } else if (rep_NUMBERP(arg1) || rep_NUMBERP(arg2)) {
	TOP = rep_compare_numbers(arg1, arg2) == 0 ? Qt : rep_nil;
	SAFE_NEXT;
      } else {
	TOP = rep_value_cmp(arg1, arg2) == 0 ? Qt : rep_nil;
	NEXT;
      }
    }

    INSN(OP__DEFINE) {
      repv value = POP;
      TOP = Fstructure_define(rep_structure, TOP, value);
      NEXT;
    }

    INSN(OP_SET) {
      CALL_2(Freal_set);
    }

    INSN(OP_REQUIRED_ARG) {
      if (argptr < argc) {
	PUSH(argv[argptr++]);
	SAFE_NEXT;
      }
      rep_signal_missing_arg(argptr + 1);
      HANDLE_ERROR;
    }

    INSN(OP_OPTIONAL_ARG) {
      PUSH((argptr < argc) ? argv[argptr++] : rep_nil);
      SAFE_NEXT;
    }

    INSN(OP_REST_ARG) {
      repv lst = rep_nil;
      for (int i = argc - 1; i >= argptr; i--) {
	if (argv[i] != 0) {
	  lst = Fcons(argv[i], lst);
	}
      }
      argptr = argc;
      PUSH(lst);
      SAFE_NEXT;
    }

    INSN(OP_KEYWORD_ARG) {
      repv sym = POP;
      for (int i = argptr; i < argc - 1; i++) {
	if (argv[i] == sym) {
	  PUSH(argv[i+1]);
	  argv[i] = argv[i+1] = 0;
	  SAFE_NEXT;
	}
      }
      PUSH(rep_nil);
      SAFE_NEXT;
    }

    INSN(OP_OPTIONAL_ARG_) {
      if (argptr < argc) {
	PUSH(argv[argptr++]);
	PUSH(Qt);
      } else {
	PUSH(rep_nil);
      }
      SAFE_NEXT;
    }

    INSN(OP_KEYWORD_ARG_) {
      repv sym = POP;
      for (int i = argptr; i < argc - 1; i += 2) {
	if (argv[i] == sym) {
	  PUSH(argv[i+1]);
	  PUSH(Qt);
	  argv[i] = argv[i+1] = 0;
	  SAFE_NEXT;
	}
      }
      PUSH(rep_nil);
      SAFE_NEXT;
    }

    INSN(OP_VECTOR_REF) {
      CALL_2(Fvector_ref);
    }

    INSN(OP_VECTOR_SET) {
      CALL_3(Fvector_set);
    }

    INSN(OP_STRING_LENGTH) {
      CALL_1(Fstring_length);
    }

    INSN(OP_STRING_REF) {
      CALL_2(Fstring_ref);
    }

    INSN(OP_STRING_SET) {
      CALL_3(Fstring_set);
    }

    INSN(OP_UNDEFINED) {
      PUSH(rep_undefined_value);
      SAFE_NEXT;
    }

    /** Jump instructions. **/

    /* Pop the stack; if it's nil jmp pc[0,1], otherwise set
       rep_throw_value = ARG and goto the error handler. */

    INSN(OP_EJMP) {
      repv tem = POP;
      if (rep_NILP(tem)) {
	goto do_jmp;
      }
      rep_throw_value = tem;
      HANDLE_ERROR;
    }

    INSN(OP_JN) {
      repv tem = POP;
      if (rep_NILP(tem)) {
	goto do_jmp;
      }
      pc += 2;
      SAFE_NEXT;
    }

    INSN(OP_JT) {
      repv tem = POP;
      if (!rep_NILP(tem)) {
	goto do_jmp;
      }
      pc += 2;
      SAFE_NEXT;
    }

    INSN(OP_JPN) {
      if (rep_NILP(TOP)) {
	POP;
	goto do_jmp;
      }
      pc += 2;
      SAFE_NEXT;
    }

    INSN(OP_JPT) {
      if (!rep_NILP(TOP)) {
	POP;
	goto do_jmp;
      }
      pc += 2;
      SAFE_NEXT;
    }

    INSN(OP_JNP) {
      if (rep_NILP(TOP)) {
	goto do_jmp;
      }
      POP;
      pc += 2;
      SAFE_NEXT;
    }

    INSN(OP_JTP) {
      if (!rep_NILP(TOP)) {
	goto do_jmp;
      }
      POP;
      pc += 2;
      SAFE_NEXT;
    }

    INSN(OP_JMP) {
    do_jmp:;
      const uint8_t *old_pc = pc;
      pc = pc_base + ((pc[0] << ARG_SHIFT) | pc[1]);

      /* Only check for interrupts / GC on backwards jumps, i.e. loops. */

      if (pc < old_pc) {
	rep_TEST_INT;
	if (rep_INTERRUPTP) {
	  HANDLE_ERROR;
	}

	if (rep_data_after_gc >= rep_gc_threshold) {
	  SYNC_GC;
	  Fgarbage_collect(rep_nil);
	}
      }

      SAFE_NEXT;
    }

    DEFAULT_INSN {
      Fsignal(Qbytecode_error,
	      rep_list_2(rep_VAL(&unknown_op), rep_MAKE_INT(pc[-1])));
      HANDLE_ERROR;
    }

    END_DISPATCH
	
    /* Check if the instruction raised an exception. */

  check_error:
    if (ERROR_OCCURRED_P) {
      /* Some form of error occurred. Unwind the binding stack. */
    error:
      while (!BIND_TOP_P) {
	repv item = BIND_RET_POP;

	if (!rep_CONSP(item) || rep_CAR(item) != Qerror) {

	  rep_GC_root gc_throwval;
	  repv throwval = rep_throw_value;
	  rep_throw_value = 0;
	  rep_PUSHGC(gc_throwval, throwval);
	  SYNC_GC;
	  impurity -= unbind(item);
	  rep_POPGC;
	  rep_throw_value = throwval;

	} else if (rep_throw_value) {

	  item = rep_CDR(item);

	  /* item is an exception-handler, (PC . SP)

	     When the code at PC is called, it will have the current
	     stack usage set to SP, and then the value of
	     rep_throw_value pushed on top.

	     The handler can then use the EJMP instruction to pass
	     control back to the error: label, or simply continue
	     execution as normal. */

	  sp = stack + rep_INT(rep_CDR(item));
	  PUSH(rep_throw_value);
	  rep_throw_value = 0;
	  pc = pc_base + rep_INT(rep_CAR(item));
	  impurity--;
	  SAFE_NEXT;

	} else {

	  /* car is an exception handler, but rep_throw_value isn't
	     set, so there's nothing to handle. Keep unwinding. */
	  impurity--;
	}
      }
      TOP = 0;
      RETURN;
    }
    SAFE_NEXT__;
  }

quit:
  /* only use this var to save declaring another */
  code = TOP;

  SYNC_GC;

  /* close the register scope */ }

  /* moved to after the execution, to avoid needing to gc protect argv */

  if (rep_data_after_gc >= rep_gc_threshold) {
    Fgarbage_collect(rep_nil);
  }

  rep_lisp_depth--;

#ifdef SLOW_GC_PROTECT
  rep_POPGCN; rep_POPGCN; rep_POPGCN; rep_POPGCN; rep_POPGC; rep_POPGC;
#else
  rep_gc_root_stack = gc_consts.next;
  rep_gc_n_roots_stack = gc_argv.next;
#endif

  return code;
}
