/* lispmach.c -- Interpreter for compiled Lisp forms

   $Id$

   Copyright (C) 1993, 1994, 2000 John Harper <john@dcs.warwick.ac.uk>

   This file is part of Jade.

   Jade is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Jade is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#define _GNU_SOURCE

/* AIX requires this to be the first thing in the file.  */
#include <config.h>
#ifdef __GNUC__
# define alloca __builtin_alloca
#else
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#endif

#include "repint.h"
#include "bytecodes.h"
#include <assert.h>

/* Define this to check if the compiler reserves enough stack */
#undef CHECK_STACK_USAGE

/* Use the threaded interpreter with GNU CC. */
#ifdef __GNUC__
# define THREADED_VM 1
#endif

DEFSYM(run_byte_code, "run-byte-code");
DEFSYM(bytecode_error, "bytecode-error");
DEFSTRING(err_bytecode_error, "Invalid byte code version");
DEFSTRING(unknown_op, "Unknown lisp opcode");

static repv vm (repv code, repv consts, repv frame,
		int v_stkreq, int b_stkreq);

/* Helper functions

   Note the careful use of inlining.. the icache is crucial, we want
   the VM to be as small as possible, so that as much other code as
   possible fits in cache as well. However, if a helper function is
   only called once (or maybe is in a crucial path), then inline it..

   The speedup from this (_not_ inlining everything) is _huge_ */

static inline repv
list_tail (repv list, int n)
{
    while (n-- > 0)
	list = rep_CDR (list);
    return list;
}

/* Unbind one level of the BIND-STACK and return the new head of the stack.
   Each item in the BIND-STACK may be one of:
	INTEGER
		variable binding frame
	(error . (PC . STACK-DEPTH))
		not unbound here; install exception handler at PC

   returns the number of dynamic bindings removed */
static inline int
inline_unbind (repv item)
{
    if (rep_INTP (item))
    {
	/* A set of symbol bindings (let or let*). */
	int lexicals = rep_LEX_BINDINGS (item);
	int specials = rep_SPEC_BINDINGS (item);
	rep_env = list_tail (rep_env, lexicals);
	rep_special_bindings = list_tail (rep_special_bindings, specials);
	return specials;
    }
    else if (item == Qnil || (rep_CONSP (item) && rep_CAR (item) == Qerror))
	return 0;
    else
	abort ();
}

static int
unbind (repv item)
{
    return inline_unbind (item);
}

static inline void
unbind_n (repv *ptr, int n)
{
    while (n-- > 0)
	unbind (ptr[n]);
}

/* Walk COUNT entries down the environment */
static inline repv
snap_environment (int count)
{
    register repv ptr = rep_env;
    while (count-- > 0)
	ptr = rep_CDR(ptr);
    return ptr;
}

static repv
search_special_bindings (repv sym)
{
    register repv env = rep_special_bindings;
    while (env != Qnil && rep_CAAR(env) != sym)
	env = rep_CDR(env);
    return env != Qnil ? rep_CAR(env) : env;
}


/* Lisp VM. */

static inline repv
list_ref (repv list, int elt)
{
    while (rep_CONSP(list) && elt-- > 0)
	list = rep_CDR(list);
    return rep_CONSP(list) ? rep_CAR(list) : Qnil;
}

#define TOP	    (*stackp)
#define RET_POP	    (*stackp--)
#define POP	    (stackp--)
#define POPN(n)	    (stackp -= n)
#define PUSH(v)	    (*(++stackp) = (v))
#define STK_USE	    (stackp - (stackbase - 1))

#define BIND_USE	(bindp - (bindbase - 1))
#define BIND_RET_POP	(*bindp--)
#define BIND_TOP	(*bindp)
#define BIND_TOP_P	(bindp < bindbase)
#define BIND_PUSH(x)	(*(++bindp) = (x))

#define FETCH	    (*pc++)
#define FETCH2(var) ((var) = (FETCH << ARG_SHIFT), (var) += FETCH)

#define SYNC_GC				\
    do {				\
	gc_stackbase.count = STK_USE;	\
	gc_bindbase.count = BIND_USE;	\
    } while (0)

/* These macros pop as many args as required then call the specified
   function properly. */

#define CALL_1(cmd)	\
    TOP = cmd (TOP);	\
    NEXT;
    
#define CALL_2(cmd)		\
    tmp = RET_POP;		\
    TOP = cmd (TOP, tmp);	\
    NEXT;

#define CALL_3(cmd)			\
    tmp = RET_POP;			\
    tmp2 = RET_POP;			\
    TOP = cmd (TOP, tmp2, tmp);		\
    NEXT;

/* We used to check for both rep_throw_value != 0, and TOP == 0. But since
   rep_throw_value is a (volatile) global, this is slower than just
   checking TOP (by about 1%) */
#define ERROR_OCCURRED_P (TOP == rep_NULL)

#ifndef THREADED_VM

/* Non-threaded interpretation, just use a big switch statement in
   a while loop. */

# define BEGIN_DISPATCH fetch: switch (FETCH) {
# define END_DISPATCH }

/* Output the case statement for an instruction OP, with an embedded
   argument. The code for the instruction should start at the following
   piece of code. */
# define BEGIN_INSN_WITH_ARG(op)					\
	case op+7:							\
	    FETCH2(arg); goto rep_CONCAT(op_, op);			\
	case op+6:							\
	    arg = FETCH; goto rep_CONCAT(op_, op);			\
	case op: case op+1: case op+2: case op+3: case op+4: case op+5:	\
	    arg = pc[-1] - op;						\
	rep_CONCAT(op_, op): {

# define BEGIN_INSN(op) case op: {
# define BEGIN_DEFAULT_INSN default: {
# define END_INSN }

# define SAFE_NEXT	goto fetch
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

# define BEGIN_INSN(op) TAG(op): { 
# define BEGIN_DEFAULT_INSN TAG_DEFAULT: {
# define END_INSN	}

# define BEGIN_INSN_WITH_ARG(op)	\
    TAG2(op):				\
	FETCH2(arg); goto TAG(op);	\
    TAG1(op):				\
	arg = FETCH; goto TAG(op);	\
    TAG0(op):				\
	arg = pc[-1] - op;		\
    BEGIN_INSN(op)

# define SAFE_NEXT	goto *cfa[FETCH]
# define INLINE_NEXT	if (!ERROR_OCCURRED_P) SAFE_NEXT; else HANDLE_ERROR
# define NEXT		goto check_error
# define RETURN		goto quit
# define HANDLE_ERROR	goto error

# define JUMP_TABLE										\
 &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT,	/*00*/				\
 &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT,					\
 &&TAG0(OP_CALL), &&TAG0(OP_CALL), &&TAG0(OP_CALL), &&TAG0(OP_CALL), /*08*/			\
 &&TAG0(OP_CALL), &&TAG0(OP_CALL), &&TAG1(OP_CALL), &&TAG2(OP_CALL),				\
 &&TAG0(OP_PUSH), &&TAG0(OP_PUSH), &&TAG0(OP_PUSH), &&TAG0(OP_PUSH), /*10*/			\
 &&TAG0(OP_PUSH), &&TAG0(OP_PUSH), &&TAG1(OP_PUSH), &&TAG2(OP_PUSH),				\
 &&TAG0(OP_REFQ), &&TAG0(OP_REFQ), &&TAG0(OP_REFQ), &&TAG0(OP_REFQ), /*18*/			\
 &&TAG0(OP_REFQ), &&TAG0(OP_REFQ), &&TAG1(OP_REFQ), &&TAG2(OP_REFQ),				\
 &&TAG0(OP_SETQ), &&TAG0(OP_SETQ), &&TAG0(OP_SETQ), &&TAG0(OP_SETQ), /*20*/			\
 &&TAG0(OP_SETQ), &&TAG0(OP_SETQ), &&TAG1(OP_SETQ), &&TAG2(OP_SETQ),				\
 &&TAG0(OP_LIST), &&TAG0(OP_LIST), &&TAG0(OP_LIST), &&TAG0(OP_LIST), /*28*/			\
 &&TAG0(OP_LIST), &&TAG0(OP_LIST), &&TAG1(OP_LIST), &&TAG2(OP_LIST),				\
 &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT, /*30*/			\
 &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT,				\
 &&TAG0(OP_REFN), &&TAG0(OP_REFN), &&TAG0(OP_REFN), &&TAG0(OP_REFN), /*38*/			\
 &&TAG0(OP_REFN), &&TAG0(OP_REFN), &&TAG1(OP_REFN), &&TAG2(OP_REFN),				\
												\
 &&TAG(OP_REF), &&TAG(OP_SET), &&TAG(OP_FLUID_REF), &&TAG(OP_ENCLOSE), /*40*/			\
 &&TAG(OP_INIT_BIND), &&TAG(OP_UNBIND), &&TAG(OP_DUP), &&TAG(OP_SWAP),				\
 &&TAG(OP_POP), &&TAG(OP_NIL), &&TAG(OP_T), &&TAG(OP_CONS), /*48*/				\
 &&TAG(OP_CAR), &&TAG(OP_CDR), &&TAG(OP_RPLACA), &&TAG(OP_RPLACD),				\
 &&TAG(OP_NTH), &&TAG(OP_NTHCDR), &&TAG(OP_ASET), &&TAG(OP_AREF), /*50*/			\
 &&TAG(OP_LENGTH), &&TAG(OP_BIND), &&TAG(OP_ADD), &&TAG(OP_NEG),				\
 &&TAG(OP_SUB), &&TAG(OP_MUL), &&TAG(OP_DIV), &&TAG(OP_REM), /*58*/				\
 &&TAG(OP_LNOT), &&TAG(OP_NOT), &&TAG(OP_LOR), &&TAG(OP_LAND),					\
												\
 &&TAG(OP_EQUAL), &&TAG(OP_EQ), &&TAG(OP_STRUCT_REF), &&TAG(OP_SCM_TEST), /*60*/		\
 &&TAG(OP_GT), &&TAG(OP_GE), &&TAG(OP_LT), &&TAG(OP_LE),					\
 &&TAG(OP_INC), &&TAG(OP_DEC), &&TAG(OP_ASH), &&TAG(OP_ZEROP), /*68*/				\
 &&TAG(OP_NULL), &&TAG(OP_ATOM), &&TAG(OP_CONSP), &&TAG(OP_LISTP),				\
												\
 &&TAG(OP_NUMBERP), &&TAG(OP_STRINGP), &&TAG(OP_VECTORP), &&TAG(OP_CATCH), /*70*/		\
 &&TAG(OP_THROW), &&TAG(OP_BINDERR), &&TAG(OP_RETURN), &&TAG(OP_UNBINDALL),			\
 &&TAG(OP_BOUNDP), &&TAG(OP_SYMBOLP), &&TAG(OP_GET), &&TAG(OP_PUT), /*78*/			\
 &&TAG(OP_ERRORPRO), &&TAG(OP_SIGNAL), &&TAG(OP_QUOTIENT), &&TAG(OP_REVERSE),			\
												\
 &&TAG(OP_NREVERSE), &&TAG(OP_ASSOC), &&TAG(OP_ASSQ), &&TAG(OP_RASSOC), /*80*/			\
 &&TAG(OP_RASSQ), &&TAG(OP_LAST), &&TAG(OP_MAPCAR), &&TAG(OP_MAPC),				\
 &&TAG(OP_MEMBER), &&TAG(OP_MEMQ), &&TAG(OP_DELETE), &&TAG(OP_DELQ), /*88*/			\
 &&TAG(OP_DELETE_IF), &&TAG(OP_DELETE_IF_NOT), &&TAG(OP_COPY_SEQUENCE), &&TAG(OP_SEQUENCEP),	\
												\
 &&TAG(OP_FUNCTIONP), &&TAG(OP_SPECIAL_FORM_P), &&TAG(OP_SUBRP), &&TAG(OP_EQL), /*90*/		\
 &&TAG(OP_LXOR), &&TAG(OP_MAX), &&TAG(OP_MIN), &&TAG(OP_FILTER),				\
 &&TAG(OP_MACROP), &&TAG(OP_BYTECODEP), &&TAG(OP_PUSHI0), &&TAG(OP_PUSHI1), /*98*/		\
 &&TAG(OP_PUSHI2), &&TAG(OP_PUSHIM1), &&TAG(OP_PUSHIM2), &&TAG(OP_PUSHI),			\
												\
 &&TAG(OP_PUSHIWN), &&TAG(OP_PUSHIWP), &&TAG(OP_CAAR), &&TAG(OP_CADR), /*A0*/			\
 &&TAG(OP_CDAR), &&TAG(OP_CDDR), &&TAG(OP_CADDR), &&TAG(OP_CADDDR),				\
 &&TAG(OP_CADDDDR), &&TAG(OP_CADDDDDR), &&TAG(OP_CADDDDDDR), &&TAG(OP_CADDDDDDDR), /*A8*/	\
 &&TAG(OP_FLOOR), &&TAG(OP_CEILING), &&TAG(OP_TRUNCATE), &&TAG(OP_ROUND),			\
												\
 &&TAG_DEFAULT, &&TAG(OP_FORBID), &&TAG(OP_PERMIT), &&TAG(OP_EXP), /*B0*/			\
 &&TAG(OP_LOG), &&TAG(OP_SIN), &&TAG(OP_COS), &&TAG(OP_TAN),					\
 &&TAG(OP_SQRT), &&TAG(OP_EXPT), &&TAG(OP_SWAP2), &&TAG(OP_MOD), /*B8*/				\
 &&TAG(OP_MAKE_CLOSURE), &&TAG(OP_UNBINDALL_0), &&TAG(OP_CLOSUREP), &&TAG(OP_POP_ALL),		\
												\
 &&TAG(OP_FLUID_SET), &&TAG(OP_FLUID_BIND), &&TAG(OP_MEMQL), &&TAG(OP_NUM_EQ), /*C0*/		\
 &&TAG(OP_TEST_SCM), &&TAG(OP_TEST_SCM_F), &&TAG_DEFAULT, &&TAG_DEFAULT,			\
 &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT, /*C8*/				\
 &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT,					\
												\
 &&TAG0(OP_BINDSPEC), &&TAG0(OP_BINDSPEC), &&TAG0(OP_BINDSPEC), &&TAG0(OP_BINDSPEC), /*D0*/	\
 &&TAG0(OP_BINDSPEC), &&TAG0(OP_BINDSPEC), &&TAG1(OP_BINDSPEC), &&TAG2(OP_BINDSPEC),		\
 &&TAG0(OP_SETG), &&TAG0(OP_SETG), &&TAG0(OP_SETG), &&TAG0(OP_SETG), /*D8*/			\
 &&TAG0(OP_SETG), &&TAG0(OP_SETG), &&TAG1(OP_SETG), &&TAG2(OP_SETG),				\
 &&TAG0(OP_REFG), &&TAG0(OP_REFG), &&TAG0(OP_REFG), &&TAG0(OP_REFG), /*E0*/			\
 &&TAG0(OP_REFG), &&TAG0(OP_REFG), &&TAG1(OP_REFG), &&TAG2(OP_REFG),				\
 &&TAG0(OP_SETN), &&TAG0(OP_SETN), &&TAG0(OP_SETN), &&TAG0(OP_SETN), /*E8*/			\
 &&TAG0(OP_SETN), &&TAG0(OP_SETN), &&TAG1(OP_SETN), &&TAG2(OP_SETN),				\
												\
 &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT, /*F0*/				\
 &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT, &&TAG_DEFAULT,					\
												\
 &&TAG(OP_EJMP), &&TAG(OP_JPN), &&TAG(OP_JPT), &&TAG(OP_JMP), /*F8*/				\
 &&TAG(OP_JN), &&TAG(OP_JT), &&TAG(OP_JNP), &&TAG(OP_JTP)

#endif /* THREADED_VM */

/* Register optimization. [ stolen from ocaml-3.00/byterun/interp.c ]

   Some compilers underestimate the use of the local variables representing
   the abstract machine registers, and don't put them in hardware registers,
   which slows down the interpreter considerably.
   For GCC, I have hand-assigned hardware registers for several architectures.
*/

#ifdef __GNUC__
#ifdef __mips__
#define PC_REG asm("$16")
#define SP_REG asm("$17")
#define BP_REG asm("$18")
#endif
#ifdef __sparc__
#define PC_REG asm("%l0")
#define SP_REG asm("%l1")
#define BP_REG asm("%l2")
#endif
#ifdef __alpha__
#ifdef __CRAY__
#define PC_REG asm("r9")
#define SP_REG asm("r10")
#define BP_REG asm("r11")
#else
#define PC_REG asm("$9")
#define SP_REG asm("$10")
#define BP_REG asm("$11")
#endif
#endif
#ifdef __i386__
#define PC_REG asm("%esi")
#define SP_REG asm("%edi")
#endif
#if defined(PPC) || defined(_POWER) || defined(_IBMR2)
#define PC_REG asm("26")
#define SP_REG asm("27")
#define BP_REG asm("28")
#endif
#ifdef __hppa__
#define PC_REG asm("%r18")
#define SP_REG asm("%r17")
#define BP_REG asm("%r16")
#endif
#ifdef __mc68000__
#define PC_REG asm("a5")
#define SP_REG asm("a4")
#endif
#ifdef __arm__
#define PC_REG asm("r9")
#define SP_REG asm("r8")
#define BP_REG asm("r7")
#endif
#endif

#ifndef PC_REG
#define PC_REG
#endif
#ifndef SP_REG
#define SP_REG
#endif
#ifndef BP_REG
#define BP_REG
#endif

DEFSTRING(max_depth, "max-lisp-depth exceeded, possible infinite recursion?");

static repv
bytecode_binder (repv frame, repv sym, repv value)
{
    if (frame == Qnil)
	frame = rep_NEW_FRAME;

    if (rep_SYM(sym)->car & rep_SF_SPECIAL)
	return rep_bind_symbol (frame, sym, value);
    else
    {
	rep_env = Fcons (value, rep_env);
	return rep_MARK_LEX_BINDING (frame);
    }
}

static repv
make_bytecode_frame (repv spec, int nargs, repv *args)
{
    if (rep_INTP (spec))
    {
	int lambda = rep_INT (spec);
	repv env = rep_env;
	int i;

	int min_args = lambda & 0xfff;
	int max_args = min_args + ((lambda >> 12) & 0xfff);
	int rest_arg = lambda >> 24;

	if (nargs < min_args)
	{
	    return Fsignal (Qmissing_arg,
			    rep_LIST_1 (rep_MAKE_INT (nargs + 1)));
	}

	if (rest_arg)
	{
	    repv tem = Qnil;
	    for (i = nargs - 1; i >= max_args; i--)
		tem = inline_Fcons (args[i], tem);
	    env = inline_Fcons (tem, env);
	}

	for (i = max_args - 1; i >= min_args; i--)
	    env = inline_Fcons (i < nargs ? args[i] : Qnil, env);
	for (i = min_args - 1; i >= 0; i--)
	    env = inline_Fcons (args[i], env);

	rep_env = env;
	return rep_MAKE_INT (max_args + (rest_arg ? 1 : 0));
    }
    else
	return rep_bind_lambda_list_1 (spec, args, nargs, bytecode_binder);
}

#define APPLY(subr, nargs, args)					\
    vm (rep_COMPILED_CODE (subr), rep_COMPILED_CONSTANTS (subr),	\
	make_bytecode_frame (rep_COMPILED_LAMBDA (subr), nargs, args),	\
	rep_INT (rep_COMPILED_STACK (subr)) & 0xffff,			\
	rep_INT (rep_COMPILED_STACK (subr)) >> 16)

repv
rep_apply_bytecode (repv subr, int nargs, repv *args)
{
    assert (rep_COMPILEDP (subr));
    return APPLY (subr, nargs, args);
}

static repv
vm (repv code, repv consts, repv frame, int v_stkreq, int b_stkreq)
{
    rep_GC_root gc_code, gc_consts;
    /* The `gcv_N' field is only filled in with the stack-size when there's
       a chance of gc.	*/
    rep_GC_n_roots gc_stackbase, gc_bindbase;

    /* this is the number of dynamic `bindings' in effect
       (including non-variable bindings). */
    int impurity;

    /* APPLY macro doesn't check this.. */
    if (frame == rep_NULL)
	return rep_NULL;

    if(++rep_lisp_depth > rep_max_lisp_depth)
    {
	rep_lisp_depth--;
	return Fsignal(Qerror, rep_LIST_1(rep_VAL(&max_depth)));
    }

    /* Jump to this label when tail-calling but the current stack
       is insufficiently large */
again_stack: {
    register u_char *pc PC_REG;
    register repv *stackp SP_REG;
    repv *stackbase;
    register repv *bindp BP_REG;
    repv *bindbase;

#if defined (__GNUC__)
    /* Using GCC's variable length auto arrays is better for this since
       the stack space is freed when leaving the containing scope */
    repv stack[v_stkreq + 1];
    repv bindstack[b_stkreq + 1];
#else
    /* Otherwise just use alloca (). When tail-calling we'll only
       allocate a new stack if the current is too small. */
    repv *stack = alloca(sizeof(repv) * (v_stkreq + 1));
    repv *bindstack = alloca(sizeof(repv) * (b_stkreq + 1));
#endif

    /* Make sure that even when the stack has no entries, the TOP
       element still != 0 (for the error-detection at label quit:) */
    stack[0] = Qt;

    /* Jump to this label when tail-calling with a large enough stack */
again:
    rep_DECLARE1(code, rep_STRINGP);
    rep_DECLARE2(consts, rep_VECTORP);

    /* Initialize the frame and stack pointers */
    stackbase = stack + 1;
    stackp = stackbase - 1;
    bindbase = bindstack;
    bindp = bindbase - 1;

    /* Push the binding frame of the function arguments */
    BIND_PUSH (frame);
    impurity = rep_SPEC_BINDINGS (frame);

    rep_PUSHGC(gc_code, code);
    rep_PUSHGC(gc_consts, consts);
    rep_PUSHGCN(gc_bindbase, bindbase, BIND_USE);
    rep_PUSHGCN(gc_stackbase, stackbase, STK_USE);

    if(rep_data_after_gc >= rep_gc_threshold)
	Fgarbage_collect (Qnil);

    rep_MAY_YIELD;

    pc = rep_STR(code);

    /* Start of the VM fetch-execute sequence. */
    {
#ifdef THREADED_VM
	static void *cfa[256] = { JUMP_TABLE };
#endif
	int arg;
	repv tmp, tmp2;

	BEGIN_DISPATCH

	BEGIN_INSN_WITH_ARG (OP_CALL)
	    struct rep_Call lc;
	    rep_bool was_closed;

	    /* args are still available above the top of the stack,
	       this just makes things a bit easier. */
	    POPN(arg);
	    tmp = TOP;
	    lc.fun = tmp;
	    lc.args = rep_void_value;
	    lc.args_evalled_p = Qt;
	    rep_PUSH_CALL (lc);
	    SYNC_GC;

	    was_closed = rep_FALSE;
	    if (rep_FUNARGP(tmp))
	    {
		rep_USE_FUNARG(tmp);
		tmp = rep_FUNARG(tmp)->fun;
		was_closed = rep_TRUE;
	    }

	    switch(rep_TYPE(tmp))
	    {
	    case rep_Subr0:
		TOP = rep_SUBR0FUN(tmp)();
		break;

	    case rep_Subr1:
		TOP = rep_SUBR1FUN(tmp)(arg >= 1 ? stackp[1] : Qnil);
		break;

	    case rep_Subr2:
		switch(arg)
		{
		case 0:
		    TOP = rep_SUBR2FUN(tmp)(Qnil, Qnil);
		    break;
		case 1:
		    TOP = rep_SUBR2FUN(tmp)(stackp[1], Qnil);
		    break;
		default:
		    TOP = rep_SUBR2FUN(tmp)(stackp[1], stackp[2]);
		    break;
		}
	        break;

	    case rep_Subr3:
		switch(arg)
		{
		case 0:
		    TOP = rep_SUBR3FUN(tmp)(Qnil, Qnil, Qnil);
		    break;
		case 1:
		    TOP = rep_SUBR3FUN(tmp)(stackp[1], Qnil, Qnil);
		    break;
		case 2:
		    TOP = rep_SUBR3FUN(tmp)(stackp[1], stackp[2], Qnil);
		    break;
		default:
		    TOP = rep_SUBR3FUN(tmp)(stackp[1], stackp[2], stackp[3]);
		    break;
		}
	        break;

	    case rep_Subr4:
		switch(arg)
		{
		case 0:
		    TOP = rep_SUBR4FUN(tmp)(Qnil, Qnil,
					 Qnil, Qnil);
		    break;
		case 1:
		    TOP = rep_SUBR4FUN(tmp)(stackp[1], Qnil,
					 Qnil, Qnil);
		    break;
		case 2:
		    TOP = rep_SUBR4FUN(tmp)(stackp[1], stackp[2],
					 Qnil, Qnil);
		    break;
		case 3:
		    TOP = rep_SUBR4FUN(tmp)(stackp[1], stackp[2],
					 stackp[3], Qnil);
		    break;
		default:
		    TOP = rep_SUBR4FUN(tmp)(stackp[1], stackp[2],
					 stackp[3], stackp[4]);
		    break;
		}
	        break;

	    case rep_Subr5:
		switch(arg)
		{
		case 0:
		    TOP = rep_SUBR5FUN(tmp)(Qnil, Qnil, Qnil,
					 Qnil, Qnil);
		    break;
		case 1:
		    TOP = rep_SUBR5FUN(tmp)(stackp[1], Qnil, Qnil,
					 Qnil, Qnil);
		    break;
		case 2:
		    TOP = rep_SUBR5FUN(tmp)(stackp[1], stackp[2], Qnil,
					 Qnil, Qnil);
		    break;
		case 3:
		    TOP = rep_SUBR5FUN(tmp)(stackp[1], stackp[2], stackp[3],
					 Qnil, Qnil);
		    break;
		case 4:
		    TOP = rep_SUBR5FUN(tmp)(stackp[1], stackp[2], stackp[3],
					 stackp[4], Qnil);
		    break;
		default:
		    TOP = rep_SUBR5FUN(tmp)(stackp[1], stackp[2], stackp[3],
					 stackp[4], stackp[5]);
		    break;
		}
	        break;

	    case rep_SubrN:
		tmp2 = Qnil;
		POPN(-arg); /* reclaim my args */
		while(arg--)
		    tmp2 = inline_Fcons(RET_POP, tmp2);
		lc.args = tmp2;
		TOP = rep_SUBRNFUN(tmp)(tmp2);
		break;

	    case rep_Compiled:
		if (was_closed && rep_bytecode_interpreter != 0)
		{
		    if (impurity != 0 || *pc != OP_RETURN)
		    {
			TOP = APPLY (tmp, arg, stackp+1);
		    }
		    else
		    {
			/* A tail call that's safe for eliminating */
			repv bindings;

			/* snap the call stack */
			rep_call_stack = lc.next;
			rep_call_stack->fun = lc.fun;
			rep_call_stack->args = lc.args;
			rep_call_stack->args_evalled_p = lc.args_evalled_p;

			/* since impurity==0 there can only be lexical
			   bindings; these were unbound when switching
			   environments.. */

			bindings = (make_bytecode_frame
				    (rep_COMPILED_LAMBDA(tmp), arg, stackp+1));
			if(bindings != rep_NULL)
			{
			    int n_req_s, n_req_b;

			    /* set up parameters */
			    code = rep_COMPILED_CODE (tmp);
			    consts = rep_COMPILED_CONSTANTS (tmp);
			    frame = bindings;

			    rep_POPGCN; rep_POPGCN; rep_POPGC; rep_POPGC;

			    /* do the goto, after deciding if the
			       current stack allocation is sufficient. */
			    n_req_s = rep_INT (rep_COMPILED_STACK (tmp)) & 0xffff;
			    n_req_b = rep_INT (rep_COMPILED_STACK (tmp)) >> 16;
			    if (n_req_s > v_stkreq || n_req_b > b_stkreq)
			    {
				v_stkreq = n_req_s;
				b_stkreq = n_req_b;
				goto again_stack;
			    }
			    else
				goto again;
			}
		    }
		}
		else
		    goto invalid;
		break;

	    case rep_Cons:
		/* a call to intepreted code, just cons up the args
		   and send it to the interpreter.. */
		POPN(-arg);
		for (tmp2 = Qnil; arg-- > 0;)
		    tmp2 = Fcons (RET_POP, tmp2);
		rep_POP_CALL (lc);
		TOP = rep_funcall(TOP, tmp2, rep_FALSE);
		NEXT;

	    default:
	    invalid:
		TOP = Fsignal(Qinvalid_function, rep_LIST_1(TOP));
	    }
	    rep_POP_CALL(lc);
	    INLINE_NEXT;
	END_INSN

	BEGIN_INSN_WITH_ARG (OP_PUSH)
	    PUSH(rep_VECT(consts)->array[arg]);
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN_WITH_ARG (OP_REFQ)
	    /* this instruction is normally only used for special
	       variables, so optimize the usual path */
	    tmp = rep_VECT(consts)->array[arg];
	    if ((rep_SYM(tmp)->car & rep_SF_SPECIAL)
		&& !(rep_SYM(tmp)->car & rep_SF_LOCAL))
	    {
		/* bytecode interpreter is allowed to assume
		   unrestricted environment.. */
		repv tem = search_special_bindings (tmp);
		if (tem != Qnil)
		{
		    tem = rep_CDR (tem);
		    if (!rep_VOIDP(tem))
		    {
			PUSH (tem);
			SAFE_NEXT;
		    }
		}
	    }
	    /* fall back to common case */
	    PUSH(Fsymbol_value(rep_VECT(consts)->array[arg], Qnil));
	    NEXT;
	END_INSN

	BEGIN_INSN_WITH_ARG (OP_SETQ)
	    /* this instruction is normally only used for special
	       variables, so optimize the usual path */
	    tmp = rep_VECT(consts)->array[arg];
	    if ((rep_SYM(tmp)->car & rep_SF_SPECIAL)
		&& !(rep_SYM(tmp)->car & rep_SF_LOCAL))
	    {
		/* bytecode interpreter is allowed to assume
		   unrestricted environment.. */
		repv tem = search_special_bindings (tmp);
		if (tem != Qnil)
		{
		    rep_CDR (tem) = RET_POP;
		    SAFE_NEXT;
		}
	    }
	    /* fall back to common case */
	    Fset(rep_VECT(consts)->array[arg], RET_POP);
	    NEXT;
	END_INSN

	BEGIN_INSN_WITH_ARG (OP_LIST)
	    tmp = Qnil;
	    while(arg--)
		tmp = inline_Fcons(RET_POP, tmp);
	    PUSH(tmp);
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_BIND)
	    tmp2 = RET_POP;
	    rep_env = inline_Fcons (tmp2, rep_env);
	    BIND_TOP = rep_MARK_LEX_BINDING (BIND_TOP);
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN_WITH_ARG (OP_BINDSPEC)
	    tmp = rep_VECT(consts)->array[arg];
	    tmp2 = RET_POP;
	    /* assuming non-restricted environment */
	    rep_special_bindings = Fcons (Fcons (tmp, tmp2),
					  rep_special_bindings);
	    BIND_TOP = rep_MARK_SPEC_BINDING (BIND_TOP);
	    impurity++;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN_WITH_ARG (OP_REFN)
	    PUSH (rep_CAR (snap_environment (arg)));
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN_WITH_ARG (OP_SETN)
	    rep_CAR (snap_environment (arg)) = RET_POP;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN_WITH_ARG (OP_REFG)
	    /* this code expanded from F_structure_ref () and lookup ()
	       in structures.c */
	    rep_struct *s = rep_STRUCTURE (rep_structure);
	    repv var = rep_VECT(consts)->array[arg];
	    rep_struct_node *n;
	    if (s->total_buckets != 0)
	    {
		for (n = s->buckets[rep_STRUCT_HASH (var, s->total_buckets)];
		     n != 0; n = n->next)
		{
		    if (n->symbol == var)
		    {
			PUSH (n->binding);
			SAFE_NEXT;
		    }
		}
	    }
	    n = rep_search_imports (s, var);
	    if (n != 0)
	    {
		PUSH (n->binding);
		SAFE_NEXT;
	    }
	    Fsignal (Qvoid_value, rep_LIST_1 (var));
	    HANDLE_ERROR;
	END_INSN

	BEGIN_INSN_WITH_ARG (OP_SETG)
	    tmp = rep_VECT(consts)->array[arg];
	    F_structure_set (rep_structure, tmp, RET_POP);
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_REF)
	    TOP = Fsymbol_value(TOP, Qnil);
	    NEXT;
	END_INSN

	BEGIN_INSN (OP_SET)
	    CALL_2(Fset);
	END_INSN

	BEGIN_INSN (OP_FLUID_REF)
	    tmp = search_special_bindings (TOP);
	    if (tmp != Qnil)
	    {
		TOP = rep_CDR (tmp);
		SAFE_NEXT;
	    }
	    else if (rep_CONSP (TOP))
	    {
		TOP = rep_CDR (TOP);
		SAFE_NEXT;
	    }
	    Fsignal (Qvoid_value, rep_LIST_1 (TOP));
	    HANDLE_ERROR;
	END_INSN

	BEGIN_INSN (OP_ENCLOSE)
	    TOP = Fmake_closure (TOP, Qnil);
	    INLINE_NEXT;
	END_INSN

	BEGIN_INSN (OP_INIT_BIND)
	    BIND_PUSH (rep_NEW_FRAME);
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_UNBIND)
	    SYNC_GC;
	    impurity -= inline_unbind(BIND_RET_POP);
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_DUP)
	    tmp = TOP;
	    PUSH(tmp);
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_SWAP)
	    tmp = TOP;
	    TOP = stackp[-1];
	    stackp[-1] = tmp;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_POP)
	    POP;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_NIL)
	    PUSH(Qnil);
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_T)
	    PUSH(Qt);
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_CONS)
	    CALL_2(inline_Fcons);
	END_INSN

	BEGIN_INSN (OP_CAR)
	    tmp = TOP;
	    if(rep_CONSP(tmp))
		TOP = rep_CAR(tmp);
	    else
		TOP = Qnil;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_CDR)
	    tmp = TOP;
	    if(rep_CONSP(tmp))
		TOP = rep_CDR(tmp);
	    else
		TOP = Qnil;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_RPLACA)
	    CALL_2(Frplaca);
	END_INSN

	BEGIN_INSN (OP_RPLACD)
	    CALL_2(Frplacd);
	END_INSN

	BEGIN_INSN (OP_NTH)
	    CALL_2(Fnth);
	END_INSN

	BEGIN_INSN (OP_NTHCDR)
	    CALL_2(Fnthcdr);
	END_INSN

	BEGIN_INSN (OP_ASET)
	    CALL_3(Faset);
	END_INSN

	BEGIN_INSN (OP_AREF)
	    CALL_2(Faref);
	END_INSN

	BEGIN_INSN (OP_LENGTH)
	    CALL_1(Flength);
	END_INSN

	BEGIN_INSN (OP_ADD)
	    /* open-code fixnum arithmetic */
	    tmp = RET_POP;
	    tmp2 = TOP;
	    if (rep_INTP (tmp) && rep_INTP (tmp2))
	    {
		long x = rep_INT (tmp2) + rep_INT (tmp);
		if (x >= rep_LISP_MIN_INT && x <= rep_LISP_MAX_INT)
		{
		    TOP = rep_MAKE_INT (x);
		    SAFE_NEXT;
		}
	    }
	    TOP = rep_number_add (tmp2, tmp);
	    INLINE_NEXT;
	END_INSN

	BEGIN_INSN (OP_NEG)
	    /* open-code fixnum arithmetic */
	    tmp = TOP;
	    if (rep_INTP (tmp))
	    {
		long x = - rep_INT (tmp);
		if (x >= rep_LISP_MIN_INT && x <= rep_LISP_MAX_INT)
		{
		    TOP = rep_MAKE_INT (x);
		    SAFE_NEXT;
		}
	    }
	    TOP = rep_number_neg (tmp);
	    INLINE_NEXT;
	END_INSN

	BEGIN_INSN (OP_SUB)
	    /* open-code fixnum arithmetic */
	    tmp = RET_POP;
	    tmp2 = TOP;
	    if (rep_INTP (tmp) && rep_INTP (tmp2))
	    {
		long x = rep_INT (tmp2) - rep_INT (tmp);
		if (x >= rep_LISP_MIN_INT && x <= rep_LISP_MAX_INT)
		{
		    TOP = rep_MAKE_INT (x);
		    SAFE_NEXT;
		}
	    }
	    TOP = rep_number_sub (tmp2, tmp);
	    INLINE_NEXT;
	END_INSN

	BEGIN_INSN (OP_MUL)
	    CALL_2(rep_number_mul);
	END_INSN

	BEGIN_INSN (OP_DIV)
	    CALL_2(rep_number_div);
	END_INSN

	BEGIN_INSN (OP_REM)
	    CALL_2(Fremainder);
	END_INSN

	BEGIN_INSN (OP_LNOT)
	    CALL_1(Flognot);
	END_INSN

	BEGIN_INSN (OP_NOT)
	    if(TOP == Qnil)
		TOP = Qt;
	    else
		TOP = Qnil;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_NULL)
	    if(TOP == Qnil)
		TOP = Qt;
	    else
		TOP = Qnil;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_LOR)
	    CALL_2(rep_number_logior);
	END_INSN

	BEGIN_INSN (OP_LXOR)
	    CALL_2(rep_number_logxor);
	END_INSN

	BEGIN_INSN (OP_LAND)
	    CALL_2(rep_number_logand);
	END_INSN

	BEGIN_INSN (OP_EQUAL)
	    tmp = RET_POP;
	    TOP = (rep_value_cmp(TOP, tmp) == 0) ? Qt : Qnil;
	    NEXT;
	END_INSN

	BEGIN_INSN (OP_EQ)
	    tmp = RET_POP;
	    TOP = (TOP == tmp) ? Qt : Qnil;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_STRUCT_REF)
	    CALL_2 (F_external_structure_ref);
	END_INSN

	BEGIN_INSN (OP_SCM_TEST)
	    TOP = (TOP == rep_scm_f) ? Qnil : Qt;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_GT)
	    tmp = RET_POP;
	    tmp2 = TOP;
	    if (rep_INTP (tmp2) && rep_INTP (tmp))
	    {
		TOP = (rep_INT (tmp2) > rep_INT (tmp)) ? Qt : Qnil;
		SAFE_NEXT;
	    }
	    else if (rep_NUMBERP (tmp2) || rep_NUMBERP (tmp))
	    {
		TOP = (rep_compare_numbers (tmp2, tmp) > 0) ? Qt : Qnil;
		SAFE_NEXT;
	    }
	    else
	    {
		TOP = (rep_value_cmp (tmp2, tmp) > 0) ? Qt : Qnil;
		NEXT;
	    }
	END_INSN

	BEGIN_INSN (OP_GE)
	    tmp = RET_POP;
	    tmp2 = TOP;
	    if (rep_INTP (tmp2) && rep_INTP (tmp))
	    {
		TOP = (rep_INT (tmp2) >= rep_INT (tmp)) ? Qt : Qnil;
		SAFE_NEXT;
	    }
	    else if (rep_NUMBERP (tmp2) || rep_NUMBERP (tmp))
	    {
		TOP = (rep_compare_numbers (tmp2, tmp) >= 0) ? Qt : Qnil;
		SAFE_NEXT;
	    }
	    else
	    {
		TOP = (rep_value_cmp (tmp2, tmp) >= 0) ? Qt : Qnil;
		NEXT;
	    }
	END_INSN

	BEGIN_INSN (OP_LT)
	    tmp = RET_POP;
	    tmp2 = TOP;
	    if (rep_INTP (tmp2) && rep_INTP (tmp))
	    {
		TOP = (rep_INT (tmp2) < rep_INT (tmp)) ? Qt : Qnil;
		SAFE_NEXT;
	    }
	    else if (rep_NUMBERP (tmp2) || rep_NUMBERP (tmp))
	    {
		TOP = (rep_compare_numbers (tmp2, tmp) < 0) ? Qt : Qnil;
		SAFE_NEXT;
	    }
	    else
	    {
		TOP = (rep_value_cmp (tmp2, tmp) < 0) ? Qt : Qnil;
		NEXT;
	    }
	END_INSN

	BEGIN_INSN (OP_LE)
	    tmp = RET_POP;
	    tmp2 = TOP;
	    if (rep_INTP (tmp2) && rep_INTP (tmp))
	    {
		TOP = (rep_INT (tmp2) <= rep_INT (tmp)) ? Qt : Qnil;
		SAFE_NEXT;
	    }
	    else if (rep_NUMBERP (tmp2) || rep_NUMBERP (tmp))
	    {
		TOP = (rep_compare_numbers (tmp2, tmp) <= 0) ? Qt : Qnil;
		SAFE_NEXT;
	    }
	    else
	    {
		TOP = (rep_value_cmp (tmp2, tmp) <= 0) ? Qt : Qnil;
		NEXT;
	    }
	END_INSN

	BEGIN_INSN (OP_INC)
	    tmp = TOP;
	    if (rep_INTP (tmp))
	    {
		long x = rep_INT (tmp) + 1;
		if (x <= rep_LISP_MAX_INT)
		{
		    TOP = rep_MAKE_INT (x);
		    SAFE_NEXT;
		}
	    }
	    TOP = Fplus1 (tmp);
	    NEXT;
	END_INSN

	BEGIN_INSN (OP_DEC)
	    tmp = TOP;
	    if (rep_INTP (tmp))
	    {
		long x = rep_INT (tmp) - 1;
		if (x >= rep_LISP_MIN_INT)
		{
		    TOP = rep_MAKE_INT (x);
		    SAFE_NEXT;
		}
	    }
	    TOP = Fsub1 (tmp);
	    NEXT;
	END_INSN

	BEGIN_INSN (OP_ASH)
	    CALL_2(Fash);
	END_INSN

	BEGIN_INSN (OP_ZEROP)
	    tmp = TOP;
	    if (rep_INTP (tmp))
	    {
		TOP = (tmp == rep_MAKE_INT (0)) ? Qt : Qnil;
		SAFE_NEXT;
	    }
	    TOP = Fzerop (tmp);
	    NEXT;
	END_INSN

	BEGIN_INSN (OP_ATOM)
	    if(!rep_CONSP(TOP))
		TOP = Qt;
	    else
		TOP = Qnil;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_CONSP)
	    if(rep_CONSP(TOP))
		TOP = Qt;
	    else
		TOP = Qnil;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_LISTP)
	    if(rep_CONSP(TOP) || rep_NILP(TOP))
		TOP = Qt;
	    else
		TOP = Qnil;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_NUMBERP)
	    if(rep_NUMERICP(TOP))
		TOP = Qt;
	    else
		TOP = Qnil;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_STRINGP)
	    if(rep_STRINGP(TOP))
		TOP = Qt;
	    else
		TOP = Qnil;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_VECTORP)
	    if(rep_VECTORP(TOP))
		TOP = Qt;
	    else
		TOP = Qnil;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_CATCH)
	    /* This takes two arguments, TAG and THROW-VALUE.
	       THROW-VALUE is the saved copy of rep_throw_value,
	       if (car THROW-VALUE) == TAG we match, and we
	       leave two values on the stack, nil on top (to
	       pacify EJMP), (cdr THROW-VALUE) below that. */
	    tmp = RET_POP;		/* tag */
	    tmp2 = TOP;		/* rep_throw_value */
	    if(rep_CONSP(tmp2) && rep_CAR(tmp2) == tmp)
	    {
		TOP = rep_CDR(tmp2);	/* leave result at stk[1] */
		PUSH(Qnil);		/* cancel error */
	    }
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_THROW)
	    tmp = RET_POP;
	    if(rep_throw_value == rep_NULL)
	    {
		rep_throw_value = Fcons(TOP, tmp);
		HANDLE_ERROR;
	    }
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_BINDERR)
	    /* Pop our single argument and cons it onto the bind-
	       stack in a pair with the current stack-pointer.
	       This installs an address in the code string as an
	       error handler. */
	    tmp = RET_POP;
	    BIND_PUSH (Fcons (Qerror, Fcons (tmp, rep_MAKE_INT(STK_USE))));
	    impurity++;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_RETURN)
	    SYNC_GC;
	    unbind_n (bindbase, BIND_USE);
	    RETURN;
	END_INSN

	BEGIN_INSN (OP_UNBINDALL)
	    SYNC_GC;
	    unbind_n (bindbase + 1, BIND_USE - 1);
	    bindp = bindbase;
	    impurity = rep_SPEC_BINDINGS (BIND_TOP);
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_BOUNDP)
	    CALL_1(Fboundp);
	END_INSN

	BEGIN_INSN (OP_SYMBOLP)
	    if(rep_SYMBOLP(TOP))
		TOP = Qt;
	    else
		TOP = Qnil;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_GET)
	    CALL_2(Fget);
	END_INSN

	BEGIN_INSN (OP_PUT)
	    CALL_3(Fput);
	END_INSN

	BEGIN_INSN (OP_ERRORPRO)
	    /* This should be called with two values on the stack.
		1. conditions of the error handler
		2. rep_throw_value of the exception

	       This function pops (1) and tests it against the error
	       in (2). If they match it sets (2) to nil, and binds the
	       error data to the next lexical slot. */
	    tmp = RET_POP;
	    if(rep_CONSP(TOP) && rep_CAR(TOP) == Qerror
	       && rep_compare_error(rep_CDR(TOP), tmp))
	    {
		/* The handler matches the error. */
		tmp = rep_CDR(TOP);	/* the error data */
		rep_env = Fcons (tmp, rep_env);
		BIND_PUSH(rep_MARK_LEX_BINDING (rep_NEW_FRAME));
		TOP = Qnil;
	    }
	    NEXT;
	END_INSN

	BEGIN_INSN (OP_SIGNAL)
	    SYNC_GC;
	    CALL_2(Fsignal);
	END_INSN

	BEGIN_INSN (OP_QUOTIENT)
	    CALL_2(Fquotient);
	END_INSN

	BEGIN_INSN (OP_REVERSE)
	    CALL_1(Freverse);
	END_INSN

	BEGIN_INSN (OP_NREVERSE)
	    CALL_1(Fnreverse);
	END_INSN

	BEGIN_INSN (OP_ASSOC)
	    CALL_2(Fassoc);
	END_INSN

	BEGIN_INSN (OP_ASSQ)
	    CALL_2(Fassq);
	END_INSN

	BEGIN_INSN (OP_RASSOC)
	    CALL_2(Frassoc);
	END_INSN

	BEGIN_INSN (OP_RASSQ)
	    CALL_2(Frassq);
	END_INSN

	BEGIN_INSN (OP_LAST)
	    CALL_1(Flast);
	END_INSN

	BEGIN_INSN (OP_MAPCAR)
	    SYNC_GC;
	    CALL_2(Fmapcar);
	END_INSN

	BEGIN_INSN (OP_MAPC)
	    SYNC_GC;
	    CALL_2(Fmapc);
	END_INSN

	BEGIN_INSN (OP_MEMBER)
	    CALL_2(Fmember);
	END_INSN

	BEGIN_INSN (OP_MEMQ)
	    CALL_2(Fmemq);
	END_INSN

	BEGIN_INSN (OP_DELETE)
	    CALL_2(Fdelete);
	END_INSN

	BEGIN_INSN (OP_DELQ)
	    CALL_2(Fdelq);
	END_INSN

	BEGIN_INSN (OP_DELETE_IF)
	    SYNC_GC;
	    CALL_2(Fdelete_if);
	END_INSN

	BEGIN_INSN (OP_DELETE_IF_NOT)
	    SYNC_GC;
	    CALL_2(Fdelete_if_not);
	END_INSN

	BEGIN_INSN (OP_COPY_SEQUENCE)
	    CALL_1(Fcopy_sequence);
	END_INSN

	BEGIN_INSN (OP_SEQUENCEP)
	    CALL_1(Fsequencep);
	END_INSN

	BEGIN_INSN (OP_FUNCTIONP)
	    CALL_1(Ffunctionp);
	END_INSN

	BEGIN_INSN (OP_SPECIAL_FORM_P)
	    CALL_1(Fspecial_form_p);
	END_INSN

	BEGIN_INSN (OP_SUBRP)
	    CALL_1(Fsubrp);
	END_INSN

	BEGIN_INSN (OP_EQL)
	    CALL_2(Feql);
	END_INSN

	BEGIN_INSN (OP_MAX)
	    CALL_2(rep_number_max);
	END_INSN

	BEGIN_INSN (OP_MIN)
	    CALL_2(rep_number_min);
	END_INSN

	BEGIN_INSN (OP_FILTER)
	    SYNC_GC;
	    CALL_2(Ffilter);
	END_INSN

	BEGIN_INSN (OP_MACROP)
	    CALL_1(Fmacrop);
	END_INSN

	BEGIN_INSN (OP_BYTECODEP)
	    CALL_1(Fbytecodep);
	END_INSN

	BEGIN_INSN (OP_PUSHI0)
	    PUSH(rep_MAKE_INT(0));
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_PUSHI1)
	    PUSH(rep_MAKE_INT(1));
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_PUSHI2)
	    PUSH(rep_MAKE_INT(2));
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_PUSHIM1)
	    PUSH(rep_MAKE_INT(-1));
	    SAFE_NEXT;
	END_INSN
 
	BEGIN_INSN (OP_PUSHIM2)
	    PUSH(rep_MAKE_INT(-2));
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_PUSHI)
	    arg = FETCH;
	    if (arg < 128)
		PUSH(rep_MAKE_INT(arg));
	    else
		PUSH(rep_MAKE_INT(arg - 256));
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_PUSHIWN)
	    FETCH2(arg);
	    PUSH(rep_MAKE_INT(-arg));
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_PUSHIWP)
	    FETCH2(arg);
	    PUSH(rep_MAKE_INT(arg));
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_CAAR)
	    tmp = TOP;
	    if (rep_CONSP(tmp) && rep_CONSP(rep_CAR(tmp)))
		TOP = rep_CAAR(tmp);
	    else
		TOP = Qnil;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_CADR)
	    tmp = TOP;
	    if (rep_CONSP(tmp) && rep_CONSP(rep_CDR(tmp)))
		TOP = rep_CADR(tmp);
	    else
		TOP = Qnil;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_CDAR)
	    tmp = TOP;
	    if (rep_CONSP(tmp) && rep_CONSP(rep_CAR(tmp)))
		TOP = rep_CDAR(tmp);
	    else
		TOP = Qnil;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_CDDR)
	    tmp = TOP;
	    if (rep_CONSP(tmp) && rep_CONSP(rep_CDR(tmp)))
		TOP = rep_CDDR(tmp);
	    else
		TOP = Qnil;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_CADDR)
	    TOP = list_ref (TOP, 2);
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_CADDDR)
	    TOP = list_ref (TOP, 3);
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_CADDDDR)
	    TOP = list_ref (TOP, 4);
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_CADDDDDR)
	    TOP = list_ref (TOP, 5);
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_CADDDDDDR)
	    TOP = list_ref (TOP, 6);
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_CADDDDDDDR)
	    TOP = list_ref (TOP, 7);
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_FLOOR)
	    CALL_1(Ffloor);
	END_INSN

	BEGIN_INSN (OP_CEILING)
	    CALL_1(Fceiling);
	END_INSN

	BEGIN_INSN (OP_TRUNCATE)
	    CALL_1(Ftruncate);
	END_INSN

	BEGIN_INSN (OP_ROUND)
	    CALL_1(Fround);
	END_INSN

	BEGIN_INSN (OP_FORBID)
	    rep_FORBID;
	    PUSH (rep_PREEMPTABLE_P ? Qnil : Qt);
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_PERMIT)
	    rep_PERMIT;
	    PUSH (rep_PREEMPTABLE_P ? Qnil : Qt);
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_EXP)
	    CALL_1(Fexp);
	END_INSN

	BEGIN_INSN (OP_LOG)
	    CALL_1(Flog);
	END_INSN

	BEGIN_INSN (OP_COS)
	    CALL_1(Fcos);
	END_INSN

	BEGIN_INSN (OP_SIN)
	    CALL_1(Fsin);
	END_INSN

	BEGIN_INSN (OP_TAN)
	    CALL_1(Ftan);
	END_INSN

	BEGIN_INSN (OP_SQRT)
	    CALL_1(Fsqrt);
	END_INSN

	BEGIN_INSN (OP_EXPT)
	    CALL_2(Fexpt);
	END_INSN

	BEGIN_INSN (OP_SWAP2)
	    tmp = TOP;
	    TOP = stackp[-1];
	    stackp[-1] = stackp[-2];
	    stackp[-2] = tmp;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_MOD)
	    CALL_2(Fmod);
	END_INSN

	BEGIN_INSN (OP_MAKE_CLOSURE)
	    CALL_2(Fmake_closure);
	END_INSN

	BEGIN_INSN (OP_UNBINDALL_0)
	    SYNC_GC;
	    unbind_n (bindbase, BIND_USE);
	    bindp = bindbase - 1;
	    impurity = 0;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_CLOSUREP)
	    if(rep_FUNARGP(TOP))
		TOP = Qt;
	    else
		TOP = Qnil;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_POP_ALL)
	    stackp = stackbase - 1;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_FLUID_SET)
	    CALL_2 (Ffluid_set);
	END_INSN

	BEGIN_INSN (OP_FLUID_BIND)
	    tmp = RET_POP;
	    rep_special_bindings = Fcons (Fcons (RET_POP, tmp),
					  rep_special_bindings);
	    BIND_TOP = rep_MARK_SPEC_BINDING (BIND_TOP);
	    impurity++;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_MEMQL)
	    CALL_2(Fmemql);
	END_INSN

	BEGIN_INSN (OP_NUM_EQ)
	    tmp = RET_POP;
	    tmp2 = TOP;
	    if (rep_INTP (tmp) && rep_INTP (tmp2))
	    {
		TOP = (tmp2 == tmp) ? Qt : Qnil;
		SAFE_NEXT;
	    }
	    else if (rep_NUMBERP (tmp2) || rep_NUMBERP (tmp))
	    {
		TOP = (rep_compare_numbers (tmp2, tmp) == 0) ? Qt : Qnil;
		SAFE_NEXT;
	    }
	    else
	    {
		TOP = (rep_value_cmp (tmp2, tmp) == 0) ? Qt : Qnil;
		NEXT;
	    }
	END_INSN

	BEGIN_INSN (OP_TEST_SCM)
	    TOP = (TOP == Qnil) ? rep_scm_f : rep_scm_t;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_TEST_SCM_F)
	    if (TOP == Qnil)
		TOP = rep_scm_f;
	    SAFE_NEXT;
	END_INSN

	/* Jump instructions follow */

	BEGIN_INSN (OP_EJMP)
	    /* Pop the stack; if it's nil jmp pc[0,1], otherwise
	       set rep_throw_value=ARG and goto the error handler. */
	    tmp = RET_POP;
	    if(rep_NILP(tmp))
		goto do_jmp;
	    rep_throw_value = tmp;
	    HANDLE_ERROR;
	END_INSN

	BEGIN_INSN (OP_JN)
	    if(rep_NILP(RET_POP))
		goto do_jmp;
	    pc += 2;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_JT)
	    if(!rep_NILP(RET_POP))
		goto do_jmp;
	    pc += 2;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_JPN)
	    if(rep_NILP(TOP))
	    {
		POP;
		goto do_jmp;
	    }
	    pc += 2;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_JPT)
	    if(!rep_NILP(TOP))
	    {
		POP;
		goto do_jmp;
	    }
	    pc += 2;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_JNP)
	    if(rep_NILP(TOP))
		goto do_jmp;
	    POP;
	    pc += 2;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_JTP)
	    if(!rep_NILP(TOP))
		goto do_jmp;
	    POP;
	    pc += 2;
	    SAFE_NEXT;
	END_INSN

	BEGIN_INSN (OP_JMP)
	do_jmp:
	    pc = rep_STR(code) + ((pc[0] << ARG_SHIFT) | pc[1]);

	    /* Test if an interrupt occurred... */
	    rep_TEST_INT;
	    if(rep_INTERRUPTP)
		HANDLE_ERROR;

	    /* ...or if it's time to gc... */
	    SYNC_GC;
	    if(rep_data_after_gc >= rep_gc_threshold)
		Fgarbage_collect (Qnil);

	    /* ...or time to switch threads */
	    rep_MAY_YIELD;
	    NEXT;
	END_INSN

#ifndef THREADED_VM
	BEGIN_INSN (0)
	    /* ensure the jump table for the switch starts from zero
	       (removes a subtraction instruction).. */
	END_INSN
	    /* fall through */
#endif

	BEGIN_DEFAULT_INSN
	    Fsignal(Qerror, rep_list_2(rep_VAL(&unknown_op),
				       rep_MAKE_INT(pc[-1])));
	    HANDLE_ERROR;
	END_INSN

	END_DISPATCH
	
	/* Check if the instruction raised an exception. */
    check_error:
	if (ERROR_OCCURRED_P)
	{
	    /* Some form of error occurred. Unwind the binding stack. */
	error:
	    while(!BIND_TOP_P)
	    {
		repv item = BIND_RET_POP;
		if(!rep_CONSP(item) || rep_CAR(item) != Qerror)
		{
		    rep_GC_root gc_throwval;
		    repv throwval = rep_throw_value;
		    rep_throw_value = rep_NULL;
		    rep_PUSHGC(gc_throwval, throwval);
		    SYNC_GC;
		    impurity -= unbind(item);
		    rep_POPGC;
		    rep_throw_value = throwval;
		}
		else if(rep_throw_value != rep_NULL)
		{
		    item = rep_CDR(item);

		    /* item is an exception-handler, (PC . SP)

		       When the code at PC is called, it will have
		       the current stack usage set to SP, and then
		       the value of rep_throw_value pushed on top.

		       The handler can then use the EJMP instruction
		       to pass control back to the error: label, or
		       simply continue execution as normal. */

		    stackp = (stackbase - 1) + rep_INT(rep_CDR(item));
		    PUSH(rep_throw_value);
		    rep_throw_value = rep_NULL;
		    pc = rep_STR(code) + rep_INT(rep_CAR(item));
		    impurity--;
		    SAFE_NEXT;
		}
		else
		{
		    /* car is an exception handler, but rep_throw_value isn't
		       set, so there's nothing to handle. Keep unwinding. */
		    impurity--;
		}
	    }
	    TOP = rep_NULL;
	    RETURN;
	}
#ifdef CHECK_STACK_USAGE
        assert (STK_USE <= v_stkreq);
        assert (BIND_USE <= b_stkreq + 1);
#endif
	SAFE_NEXT;
    }

quit:
    /* only use this var to save declaring another */
    code = TOP;

    /* close the stack scope */ }

    rep_lisp_depth--;
    rep_POPGCN; rep_POPGCN; rep_POPGC; rep_POPGC;
    return code;
}

DEFUN("run-byte-code", Frun_byte_code, Srun_byte_code,
      (repv code, repv consts, repv stkreq), rep_Subr3)
{
    int v_stkreq, b_stkreq;

    rep_DECLARE3(stkreq, rep_INTP);

    v_stkreq = rep_INT (stkreq) & 0xffff;
    b_stkreq = rep_INT (stkreq) >> 16;

    return vm (code, consts, Qnil, v_stkreq, b_stkreq);
}

DEFUN("validate-byte-code", Fvalidate_byte_code, Svalidate_byte_code, (repv bc_major, repv bc_minor), rep_Subr2) /*
::doc:validate-byte-code::
validate-byte-code BC-MAJOR BC-MINOR

Check that byte codes from instruction set BC-MAJOR.BC-MINOR, may be
executed. If not, an error will be signalled.
::end:: */
{
    if(!rep_INTP(bc_major) || !rep_INTP(bc_minor)
       || rep_INT(bc_major) != BYTECODE_MAJOR_VERSION
       || rep_INT(bc_minor) > BYTECODE_MINOR_VERSION)
	return Fsignal(Qbytecode_error, Qnil);
    else
	return Qt;
}

DEFUN("make-byte-code-subr", Fmake_byte_code_subr, Smake_byte_code_subr, (repv args), rep_SubrN) /*
::doc:make-byte-code-subr::
make-byte-code-subr ARGS CODE CONSTANTS STACK [DOC] [INTERACTIVE]

Return an object that can be used as the function value of a symbol.
::end:: */
{
    int len = rep_list_length(args);
    repv obj[6], vec;
    int used;

    if(len < rep_COMPILED_MIN_SLOTS)
	return rep_signal_missing_arg(len + 1);
    
    obj[0] = rep_CAR(args); args = rep_CDR(args);
    if(!rep_STRINGP(rep_CAR(args)))
	return rep_signal_arg_error(rep_CAR(args), 2);
    obj[1] = rep_CAR(args); args = rep_CDR(args);
    if(!rep_VECTORP(rep_CAR(args)))
	return rep_signal_arg_error(rep_CAR(args), 3);
    obj[2] = rep_CAR(args); args = rep_CDR(args);
    if(!rep_INTP(rep_CAR(args)))
	return rep_signal_arg_error(rep_CAR(args), 4);
    obj[3] = rep_CAR(args); args = rep_CDR(args);
    used = 4;

    if(rep_CONSP(args))
    {
	obj[used++] = rep_CAR(args); args = rep_CDR(args);
	if(rep_CONSP(args))
	{
	    obj[used++] = rep_CAR(args); args = rep_CDR(args);
	    if(rep_NILP(obj[used - 1]))
		used--;
	}
	if(used == 5 && rep_NILP(obj[used - 1]))
	    used--;
    }

    vec = Fmake_vector(rep_MAKE_INT(used), Qnil);
    if(vec != rep_NULL)
    {
	int i;
	rep_COMPILED(vec)->car = ((rep_COMPILED(vec)->car
				   & ~rep_CELL8_TYPE_MASK) | rep_Compiled);
	for(i = 0; i < used; i++)
	    rep_VECTI(vec, i) = obj[i];
    }
    return vec;
}

void
rep_lispmach_init(void)
{
    rep_INTERN(run_byte_code);
    rep_ADD_SUBR(Srun_byte_code);
    rep_ADD_SUBR(Svalidate_byte_code);
    rep_ADD_SUBR(Smake_byte_code_subr);
    rep_INTERN(bytecode_error); rep_ERROR(bytecode_error);
}

void
rep_lispmach_kill(void)
{
}
