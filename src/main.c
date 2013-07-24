/* main.c -- Entry point for Jade
   Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>
   $Id$

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
   along with Jade; see the file COPYING.	If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#define _GNU_SOURCE

#include "repint.h"
#include <string.h>
#include <limits.h>

void *rep_common_db;

int rep_recurse_depth = -1;

bool (*rep_on_idle_fun)(int since_last);
DEFSYM(idle_hook, "idle-hook"); /*
::doc:idle-hook::
This hook gets evaluated every second while the editor is idle. Don't depend
on how regularly this gets called, any events from the window-system will
delay it. Also, auto-saving files and garbage-collection take precedence
when there's idle time available. Use this hook sparingly, or for short
periods only!
::end::
::doc:program-name::
The name of the program running the rep interpreter.
::end::
::doc:error-mode::
When nil, errors are handled at the current event loop, other possible
values include `exit' and `top-level'.
::end::
::doc:interrupt-mode::
When nil, interrupts are handled at the current event loop, other possible
values include `exit' and `top-level'.
::end:: */

/* Called when we get a termination signal. */
void (*rep_on_termination_fun)(void);

/* The event-loop function, may be entered recursively. */
repv (*rep_event_loop_fun)(void) = rep_event_loop;

/* rep_init () will set this to an early stack pointer */
char *rep_stack_bottom;

DEFSYM(exit, "exit");
DEFSYM(quit, "quit");
DEFSYM(top_level, "top-level");
DEFSYM(command_line_args, "command-line-args");
DEFSYM(batch_mode, "batch-mode");
DEFSYM(interpreted_mode, "interpreted-mode");
DEFSYM(program_name, "program-name");
DEFSYM(error_mode, "error-mode");
DEFSYM(interrupt_mode, "interrupt-mode");
DEFSYM(before_exit_hook, "before-exit-hook");

static void rep_main_init(void);

DEFSTRING(noarg, "No argument for option");

/* Look for the command line option called OPTION. If ARGP is non-null,
   the option requires an argument, it will be stored in *ARGP. If
   the option isn't given return false, else return true. */
bool
rep_get_option (char *option, repv *argp)
{
    int optlen = strlen(option);
    repv tem = Fsymbol_value (Qcommand_line_args, Qt);
    while (!rep_INTERRUPTP && rep_CONSP(tem) && rep_STRINGP(rep_CAR(tem)))
    {
	if (strncmp (option, rep_STR(rep_CAR(tem)), optlen) == 0)
	{
	    repv opt = rep_CAR(tem), cdr = rep_CDR(tem);
	    if (rep_STR(opt)[optlen] == '=' || rep_STR(opt)[optlen] == 0)
	    {
		Fset (Qcommand_line_args,
		      Fdelq (opt, Fsymbol_value (Qcommand_line_args, Qt)));
		if (argp != 0)
		{
		    if (rep_STR(opt)[optlen] == '=')
		    {
			*argp = rep_string_dup (rep_STR(opt) + optlen + 1);
			return true;
		    }
		    else if (rep_CONSP(cdr) && rep_STRINGP(rep_CAR(cdr)))
		    {
			*argp = rep_CAR(cdr);
			Fset (Qcommand_line_args,
			      Fdelq (*argp, Fsymbol_value(Qcommand_line_args, Qt)));
			return true;
		    }
		    else
		    {
			Fsignal (Qerror, rep_list_2(rep_VAL(&noarg),
						    rep_string_dup(option)));
			return false;
		    }
		}
		else
		    return true;
	    }
	}
	tem = rep_CDR(tem);
	rep_TEST_INT;
    }
    return false;
}

static int
get_main_options(char *prog_name, int *argc_p, char ***argv_p)
{
    int argc = *argc_p;
    char **argv = *argv_p;
    repv head, *last;

    /* any command line args are made into a list of strings
       in symbol command-line-args.  */
    head = rep_nil;
    last = &head;
    while(argc > 0)
    {
	*last = Fcons(rep_string_dup(*argv), rep_nil);
	last = &rep_CDR(*last);
	argc--;
	argv++;
    }
    Fset (Qcommand_line_args, head);
    *argc_p = argc;
    *argv_p = argv;

    if (rep_get_option("--batch", 0))
	Fset (Qbatch_mode, Qt);

    if (rep_get_option("--interp", 0))
    {
	Fset (Qinterpreted_mode, Qt);

	/* XXX somewhat non-related, but.. */
	rep_record_origins = true;
    }

    return true;
}

/* GCC 4 helpfully inlines this function and breaks the stack check. */
#if __GNUC__ >= 4
static void check_configuration (int *stack_low) __attribute__ ((noinline));
#endif

static void
check_configuration (int *stack_low)
{
    int stack_high;
    int stack_dir = (&stack_high < stack_low) ? -1 : +1;

    if (stack_dir != STACK_DIRECTION)
    {
	fprintf (stderr,
	" ** error: --with-stack-direction is incorrect; it should be %d\n",
		 stack_dir);
	exit (10);
    }
}

void
rep_init(char *prog_name, int *argc, char ***argv,
	 void (*sys_symbols)(void), void (*obsolete_sys_usage)(void))
{
    int dummy;
    check_configuration (&dummy);

    rep_common_db = rep_db_alloc("common", 4096);

    rep_pre_values_init();
    rep_pre_sys_os_init();
    if(rep_pre_symbols_init())
    {
	rep_symbols_init();
	rep_structures_init ();
	rep_numbers_init ();

	rep_lisp_init();
	rep_values_init();
	rep_origin_init ();		/* must be after values */
	rep_macros_init ();
	rep_lispcmds_init();
	rep_lispmach_init();
	rep_find_init();
	rep_main_init();
	rep_misc_init();
	rep_streams_init();
	rep_files_init();
	rep_datums_init();
	rep_fluids_init();
	rep_weak_refs_init ();
	rep_sys_os_init();

	/* XXX Assumes that argc is on the stack. I can't think of
	   XXX any other way to reliably find the real base of the
	   XXX stack.. */
	rep_stack_bottom = (char *) argc;
	rep_continuations_init ();

	if (sys_symbols != 0)
	    (*sys_symbols)();

	Fset (Qprogram_name, rep_string_dup (prog_name));

	if(get_main_options(prog_name, argc, argv))
	    return;
    }
    exit (10);
}

/* Should be called sometime after calling rep_init*. It will load
   the standard init scripts, plus FILE if non-nil. Returns the
   result of the last form evaluated. */
repv
rep_load_environment (repv file)
{
    /* Modules that have Lisp code stored in the filing system. */
    static const char *init[] = {
	"rep.lang.interpreter",
	"rep.structures",
	"rep.module-system",
	"rep.lang.math",
	"rep.data",
	"rep.regexp",
	"rep.system",
	"rep.io.streams",
	"rep.io.files",
	"rep.io.file-handlers",
	"rep",
	0
    };
    const char **ptr;

    repv res = rep_nil;
    rep_GC_root gc_file;

    rep_PUSHGC (gc_file, file);

    /* 1. Do the rep bootstrap */

    for (ptr = init; res != 0 && *ptr != 0; ptr++)
	res = rep_bootstrap_structure (*ptr);

    /* 2. Do the caller-local bootstrap */

    if (res != 0 && rep_STRINGP(file))
	res = Fload (file, rep_nil, rep_nil, rep_nil, rep_nil);

    rep_POPGC;
    return res;
}

void
rep_kill(void)
{
    rep_sys_os_kill();
    rep_find_kill();
    rep_files_kill();
#ifdef HAVE_DYNAMIC_LOADING
    rep_kill_dl_libraries();
#endif
    rep_lispmach_kill();
    rep_db_kill();
    rep_tuples_kill();
    rep_values_kill();
}

/* This function gets called when we have idle time available. The
   single argument is the number of seconds since we weren't idle.
   The first idle period after a non-idle period should pass zero.
   Returns true if the display should be refreshed. */
bool
rep_on_idle(int since_last_event)
{
    static bool called_hook;
    static int depth;
    bool res = false;

    depth++;

    /* A timeout; do one of:
	* Remove messages in minibuffers
	* Print the current key-prefix
	* Auto-save a buffer
	* GC if enough data allocated
	* Run the `idle-hook' (only once per idle-period)  */

    if(since_last_event == 0)
	called_hook = false;

    if(rep_on_idle_fun != 0 && (*rep_on_idle_fun)(since_last_event))
	res = true;
    else if(rep_data_after_gc > rep_idle_gc_threshold)
	/* nothing was saved so try a GC */
	Fgarbage_collect (rep_nil);
    else if(!called_hook && depth == 1)
    {
	repv hook = Fsymbol_value(Qidle_hook, Qt);
	if(!rep_VOIDP(hook) && !rep_NILP(hook))
	{
	    Fcall_hook(hook, rep_nil, rep_nil);
	    res = true;
	}
	called_hook = true;
    }

    depth--;
    return res;
}

/* The input loop should call this function when rep_throw_value == 0.
   It returns true when the input loop should exit, returning whatever
   is stored in *RESULT-P. */
bool
rep_handle_input_exception(repv *result_p)
{
    repv tv = rep_throw_value;
    repv car = rep_CAR(tv);
    rep_throw_value = 0;
    *result_p = 0;
    
    if(car == Qexit)
    {
	*result_p = rep_CDR(tv);
	if(rep_recurse_depth > 0)
	    return true;
    }
    else if((car == Qtop_level) && (rep_recurse_depth == 0))
	*result_p = rep_CDR(tv);
    else if(car == Qquit)
    {
	*result_p = rep_CDR(tv);
	return true;
    }
    else if(car == Quser_interrupt)
    {
	repv tem = Fsymbol_value (Qinterrupt_mode, Qt);
	if (tem == Qexit && rep_recurse_depth == 0)
	    goto terminate;
	else if (rep_recurse_depth == 0 || tem != Qtop_level)
	    rep_handle_error(car, rep_nil);
	else
	    goto unhandled;
    }
    else if(car == Qerror)
    {
	repv tem = Fsymbol_value (Qerror_mode, Qt);
	if (tem == Qexit && rep_recurse_depth == 0)
	{
	    rep_handle_error(rep_CAR(rep_CDR(tv)), rep_CDR(rep_CDR(tv)));
	    goto terminate;
	}
	else if (rep_recurse_depth == 0 || tem != Qtop_level)
	    rep_handle_error(rep_CAR(rep_CDR(tv)), rep_CDR(rep_CDR(tv)));
	else
	    goto unhandled;
    }
    else if(car == Qterm_interrupt)
    {
    terminate:
	if(rep_recurse_depth == 0 && rep_on_termination_fun != 0)
	    (*rep_on_termination_fun)();
	*result_p = rep_nil;
	return true;
    }
#if 0
    else if(rep_recurse_depth == 0)
	rep_handle_error(Qno_catcher, rep_LIST_1(car));
#endif
    else
    {
    unhandled:
	rep_throw_value = tv;
	return true;
    }
    return false;
}

/* should be called before exiting (for any reason). returns the value
   that should be returned by the process */
int
rep_top_level_exit (void)
{
    rep_GC_root gc_throw;
    repv throw = rep_throw_value;
    rep_throw_value = 0;
    if(throw && rep_CAR(throw) == Qerror)
    {
	/* If quitting due to an error, print the error cell if
	   at all possible. */
	repv stream = Fstderr_file();
	if(stream && rep_FILEP(stream))
	{
	    fputs("error--> ", stderr);
	    Fprin1(rep_CDR(throw), stream);
	    fputc('\n', stderr);
	}
	else
	    fputs("error in initialisation\n", stderr);
	return 10;
    }

    rep_PUSHGC(gc_throw, throw);
    Fcall_hook (Qbefore_exit_hook, rep_nil, rep_nil);
    rep_throw_value = 0;
    rep_POPGC;

    if (throw && rep_CAR (throw) == Qquit && rep_INTP (rep_CDR(throw)))
	return (rep_INT (rep_CDR(throw)));

    return 0;
}

DEFUN_INT("recursive-edit", Frecursive_edit, Srecursive_edit, (void), rep_Subr0, "") /*
::doc:rep.system#recursive-edit::
recursive-edit

Enter a new recursive-edit.
::end:: */
{
    repv ret;

    rep_recurse_depth++;
    ret = (*rep_event_loop_fun)();
    rep_recurse_depth--;

#ifdef C_ALLOCA
    /* Using the C implementation of alloca. So garbage collect
       anything below the current stack depth. */
    alloca(0);
#endif

    return ret;
}

/* Called from the main function of input-driven programs. Avoids the
   program exiting due to an unhandled exception */
repv
rep_top_level_recursive_edit (void)
{
    repv ret;
again:
    ret = Frecursive_edit ();
    if (rep_recurse_depth < 0
	&& rep_throw_value && rep_CONSP (rep_throw_value))
    {
	repv type = rep_CAR (rep_throw_value);
	if (type != Qquit
	    && type != Qerror
	    && type != Qterm_interrupt
	    && type != Quser_interrupt)
	{
	    rep_throw_value = 0;
	    rep_handle_error (Qno_catcher, rep_LIST_1 (type));
	    goto again;
	}
    }
    return ret;
}

DEFUN("recursion-depth", Frecursion_depth, Srecursion_depth, (void), rep_Subr0) /*
::doc:rep.system#recursion-depth::
recursion-depth

Returns the number of recursive-edit's deep we are, zero signifies the
original level.
::end:: */
{
    return rep_MAKE_INT(rep_recurse_depth);
}

void
rep_deprecated (bool *seen, const char *desc)
{
    if (!*seen)
    {
	fprintf (stderr, "rep: using deprecated feature - %s\n", desc);
	*seen = true;
    }
}

static void
rep_main_init(void)
{
    repv tem = rep_push_structure ("rep.system");
    rep_ADD_SUBR_INT(Srecursive_edit);
    rep_ADD_SUBR(Srecursion_depth);
    rep_pop_structure (tem);

    rep_INTERN(quit);
    rep_INTERN(exit);
    rep_INTERN(top_level);
    rep_INTERN_SPECIAL(command_line_args);
    rep_INTERN_SPECIAL(idle_hook);
    rep_INTERN_SPECIAL(batch_mode);
    Fset (Qbatch_mode, rep_nil);
    rep_INTERN_SPECIAL(interpreted_mode);
    Fset (Qinterpreted_mode, rep_nil);
    rep_INTERN_SPECIAL(program_name);
    rep_INTERN_SPECIAL(error_mode);
    Fset (Qerror_mode, rep_nil);
    rep_INTERN_SPECIAL(interrupt_mode);
    Fset (Qinterrupt_mode, rep_nil);
    rep_INTERN_SPECIAL(before_exit_hook);
}
