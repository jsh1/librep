/* main.c -- Entry point for Jade

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
#include <limits.h>

void *rep_common_db;

int rep_recurse_depth = -1;

bool (*rep_on_idle_fun)(int since_last);

DEFSYM(idle_hook, "*idle-hook*");

/* ::doc:*idle-hook*::
This hook gets evaluated every second while the editor is idle. Don't depend
on how regularly this gets called, any events from the window-system will
delay it. Also, auto-saving files and garbage-collection take precedence
when there's idle time available. Use this hook sparingly, or for short
periods only!
::end::
::doc:rep.system#program-name::
The name of the program running the rep interpreter.
::end::
::doc:*error-mode*::
When nil, errors are handled at the current event loop, other possible
values include `exit' and `top-level'.
::end::
::doc:*interrupt-mode*::
When nil, interrupts are handled at the current event loop, other possible
values include `exit' and `top-level'.
::end:: */

/* Called when we get a termination signal. */

void (*rep_on_termination_fun)(void);

/* The event-loop function, may be entered recursively. */

repv (*rep_event_loop_fun)(void) = rep_event_loop;

DEFSYM(exit, "exit");
DEFSYM(quit, "quit");
DEFSYM(top_level, "top-level");
DEFSYM(command_line_args, "*command-line-args*");
DEFSYM(batch_mode, "*batch-mode*");
DEFSYM(interpreted_mode, "*interpreted-mode*");
DEFSYM(program_name, "program-name");
DEFSYM(error_mode, "*error-mode*");
DEFSYM(interrupt_mode, "*interrupt-mode*");
DEFSYM(before_exit_hook, "*before-exit-hook*");

static void rep_main_init(void);

DEFSTRING(noarg, "No argument for option");

/* Look for the command line option called OPTION. If ARGP is non-null,
   the option requires an argument, it will be stored in *ARGP. If
   the option isn't given return false, else return true. */

bool
rep_get_option(char *option, repv *argp)
{
  size_t optlen = strlen(option);
  repv tem = Fsymbol_value(Qcommand_line_args, Qt);

  while (!rep_INTERRUPTP && rep_CONSP(tem) && rep_STRINGP(rep_CAR(tem))) {
    if (strncmp(option, rep_STR(rep_CAR(tem)), optlen) == 0) {
      repv opt = rep_CAR(tem);
      repv cdr = rep_CDR(tem);

      if (rep_STR(opt)[optlen] == '=' || rep_STR(opt)[optlen] == 0) {
	Fset(Qcommand_line_args,
	     Fdelq(opt, Fsymbol_value(Qcommand_line_args, Qt)));

	if (argp) {
	  if (rep_STR(opt)[optlen] == '=') {
	    *argp = rep_string_copy(rep_STR(opt) + optlen + 1);
	    return true;
	  } else if (rep_CONSP(cdr) && rep_STRINGP(rep_CAR(cdr))) {
	    *argp = rep_CAR(cdr);
	    Fset(Qcommand_line_args,
		 Fdelq(*argp, Fsymbol_value(Qcommand_line_args, Qt)));
	    return true;
	  } else {
	    Fsignal(Qerror, rep_list_2(rep_VAL(&noarg),
				       rep_string_copy(option)));
	    return false;
	  }
	} else {
	  return true;
	}
      }
    }

    tem = rep_CDR(tem);
    rep_TEST_INT;
  }

  return false;
}

static void
get_main_options(char *prog_name, int *argc_p, char ***argv_p)
{
  int argc = *argc_p;
  char **argv = *argv_p;

  /* Any command line args are made into a list of strings in symbol
     *command-line-args*.  */

  repv head = rep_nil;
  repv *last = &head;

  while (argc > 0) {
    *last = Fcons(rep_string_copy(*argv), rep_nil);
    last = rep_CDRLOC(*last);
    argc--;
    argv++;
  }

  Fset(Qcommand_line_args, head);

  *argc_p = argc;
  *argv_p = argv;

  if (rep_get_option("--batch", 0)) {
    Fset(Qbatch_mode, Qt);
  }

  if (rep_get_option("--interp", 0)) {
    Fset(Qinterpreted_mode, Qt);

    /* Somewhat non-related, but.. */
    rep_record_origins = true;
  }
}

static rep_NOT_INLINE void
check_configuration(int *stack_low)
{
  int stack_high = 1;
  int stack_dir = (&stack_high < stack_low) ? -1 : +1;

  if (stack_dir != STACK_DIRECTION) {
    fprintf(stderr,
	" ** error: --with-stack-direction is incorrect; it should be %d\n",
	stack_dir);
    exit(10);
  }
}

void
rep_init(char *prog_name, int *argc, char ***argv)
{
  int stack_low = 1;
  check_configuration(&stack_low);

  rep_common_db = rep_db_alloc("common", 4096);

  rep_types_init();
  rep_signals_init();
  rep_obarray_init();
  rep_symbols_init();
  rep_variables_init();
  rep_closures_init();
  rep_structures_init();
  rep_plists_init();
  rep_numbers_init();
  rep_lambda_init();
  rep_apply_init();
  rep_eval_init();
  rep_errors_init();
  rep_autoload_init();
  rep_read_init();
  rep_print_init();
  rep_gc_init();
  rep_guardians_init();
  rep_origin_init();		/* must be after guardians */
  rep_macros_init();
  rep_compare_init();
  rep_call_hook_init();
  rep_load_init();
  rep_lists_init();
  rep_strings_init();
  rep_vectors_init();
  rep_sequences_init();
  rep_arrays_init();
  rep_characters_init();
  rep_lispmach_init();
  rep_find_init();
  rep_main_init();
  rep_streams_init();
  rep_input_init();
  rep_misc_init();
  rep_time_init();
  rep_files_init();
  rep_datums_init();
  rep_fluids_init();
  rep_weak_refs_init();
  rep_tables_init();
  rep_environ_init();
  rep_processes_init();
  rep_sockets_init();

  repv tem = rep_push_structure("rep.system");
  Fset(Qprogram_name, rep_string_copy(prog_name));
  rep_pop_structure(tem);

  get_main_options(prog_name, argc, argv);
}

/* Should be called sometime after calling rep_init*. It will load
   the standard init scripts, plus FILE if non-nil. Returns the
   result of the last form evaluated. */

repv
rep_load_environment(repv file)
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

  repv ret = rep_nil;

  rep_GC_root gc_file;
  rep_PUSHGC(gc_file, file);

  /* 1. Do the rep bootstrap */

  for (ptr = init; ret && *ptr != 0; ptr++) {
    ret = rep_bootstrap_structure(*ptr);
  }

  /* 2. Do the caller-local bootstrap */

  if (ret && rep_STRINGP(file)) {
    ret = Fload(file, rep_nil, rep_nil, rep_nil, rep_nil);
  }

  rep_POPGC;
  return ret;
}

void
rep_kill(void)
{
  rep_processes_kill();
  rep_sockets_kill();
  rep_find_kill();
  rep_files_kill();
#ifdef HAVE_DYNAMIC_LOADING
  rep_dl_kill_libraries();
#endif
  rep_db_kill();
  rep_tuples_kill();
  rep_strings_kill();
  rep_lists_kill();
  rep_vectors_kill();
  rep_numbers_kill();
  rep_closures_kill();
}

/* This function gets called when we have idle time available. The
   single argument is the number of seconds since we weren't idle.
   The first idle period after a non-idle period should pass zero.
   Returns true if external code ran. */

bool
rep_on_idle(int since_last_event)
{
  static bool called_hook;
  static int depth;
  bool ret = false;

  depth++;

  if (since_last_event == 0) {
    called_hook = false;
  }

  /* Only one non-trivial action per idle invocation. */

  if (rep_on_idle_fun != 0 && (*rep_on_idle_fun)(since_last_event)) {
    ret = true;
  } else if (rep_data_after_gc > rep_idle_gc_threshold) {
    Fgarbage_collect(rep_nil);
  } else if (!called_hook && depth == 1) {
    repv hook = Fsymbol_value(Qidle_hook, Qt);
    if (!rep_VOIDP(hook) && !rep_NILP(hook)) {
      Fcall_hook(hook, rep_nil, rep_nil);
      ret = true;
    }
    called_hook = true;
  }

  depth--;

  return ret;
}

/* The input loop should call this function when rep_throw_value != 0.
   It returns true when the input loop should exit, returning whatever
   is stored in *RESULT-P. */

bool
rep_handle_input_exception(repv *result_p)
{
  repv tv = rep_throw_value;
  rep_throw_value = 0;

  repv car = rep_CAR(tv);

  *result_p = 0;
    
  if (car == Qexit) {
    *result_p = rep_CDR(tv);
    if (rep_recurse_depth > 0) {
      return true;
    }
  } else if ((car == Qtop_level) && (rep_recurse_depth == 0)) {
    *result_p = rep_CDR(tv);
  } else if (car == Qquit) {
    *result_p = rep_CDR(tv);
    return true;
  } else if (car == Quser_interrupt) {
    repv tem = Fsymbol_value(Qinterrupt_mode, Qt);
    if (tem == Qexit && rep_recurse_depth == 0) {
      goto terminate;
    } else if (rep_recurse_depth == 0 || tem != Qtop_level) {
      rep_handle_error(car, rep_nil);
    } else {
      goto unhandled;
    }
  } else if (car == Qerror) {
    repv tem = Fsymbol_value(Qerror_mode, Qt);
    if (tem == Qexit && rep_recurse_depth == 0) {
      rep_handle_error(rep_CAR(rep_CDR(tv)), rep_CDR(rep_CDR(tv)));
      goto terminate;
    } else if (rep_recurse_depth == 0 || tem != Qtop_level) {
      rep_handle_error(rep_CAR(rep_CDR(tv)), rep_CDR(rep_CDR(tv)));
    } else {
      goto unhandled;
    }
  } else if (car == Qterm_interrupt) {
  terminate:
    if (rep_recurse_depth == 0 && rep_on_termination_fun != 0) {
      (*rep_on_termination_fun)();
    }
    *result_p = rep_nil;
    return true;
  } else {
  unhandled:
    rep_throw_value = tv;
    return true;
  }

  return false;
}

/* Should be called before exiting (for any reason). Returns the value
   that should be returned by the process. */

int
rep_top_level_exit(void)
{
  repv throw = rep_throw_value;
  rep_throw_value = 0;

  if (throw && rep_CAR(throw) == Qerror) {
    /* If quitting due to an error, print the error cell if possible. */

    repv stream = Fstderr_file();
    if (stream && rep_FILEP(stream)) {
      fputs("error--> ", stderr);
      Fprin1(rep_CDR(throw), stream);
      fputc('\n', stderr);
    } else {
      fputs("error in initialisation\n", stderr);
    }
    return 10;
  }

  rep_GC_root gc_throw;
  rep_PUSHGC(gc_throw, throw);

  Fcall_hook(Qbefore_exit_hook, rep_nil, rep_nil);

  rep_throw_value = 0;
  rep_POPGC;

  if (throw && rep_CAR(throw) == Qquit && rep_INTP(rep_CDR(throw))) {
    return(rep_INT(rep_CDR(throw)));
  } else {
    return 0;
  }
}

DEFUN_INT("recursive-edit", Frecursive_edit,
	  Srecursive_edit, (void), rep_Subr0, "") /*
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
  /* Garbage collect. */
  alloca(0);
#endif

  return ret;
}

/* Called from the main function of input-driven programs. Avoids the
   program exiting due to an unhandled exception */

repv
rep_top_level_recursive_edit(void)
{
  bool should_exit = false;
  repv ret = rep_nil;

  while (!should_exit) {
    should_exit = true;

    ret = Frecursive_edit();

    if (rep_recurse_depth < 0 && rep_throw_value
	&& rep_CONSP(rep_throw_value))
    {
      repv type = rep_CAR(rep_throw_value);

      if (type != Qquit && type != Qerror
	  && type != Qterm_interrupt && type != Quser_interrupt)
      {
	rep_throw_value = 0;
	rep_handle_error(Qno_catcher, rep_LIST_1(type));
	should_exit = false;
      }
    }
  }

  return ret;
}

DEFUN("recursion-depth", Frecursion_depth,
      Srecursion_depth, (void), rep_Subr0) /*
::doc:rep.system#recursion-depth::
recursion-depth

Returns the number of recursive-edit's deep we are, zero signifies the
original level.
::end:: */
{
  return rep_MAKE_INT(rep_recurse_depth);
}

void
rep_deprecated(bool *seen, const char *desc)
{
  if (!*seen) {
    fprintf(stderr, "rep: using deprecated feature - %s\n", desc);
    *seen = true;
  }
}

static void
rep_main_init(void)
{
  repv tem = rep_push_structure("rep.system");
  rep_ADD_SUBR_INT(Srecursive_edit);
  rep_ADD_SUBR(Srecursion_depth);
  rep_pop_structure(tem);

  rep_INTERN(quit);
  rep_INTERN(exit);
  rep_INTERN(top_level);
  rep_INTERN_SPECIAL(command_line_args);
  rep_INTERN_SPECIAL(idle_hook);
  rep_INTERN_SPECIAL(batch_mode);
  Fset(Qbatch_mode, rep_nil);
  rep_INTERN_SPECIAL(interpreted_mode);
  Fset(Qinterpreted_mode, rep_nil);
  rep_INTERN(program_name);
  rep_INTERN_SPECIAL(error_mode);
  Fset(Qerror_mode, rep_nil);
  rep_INTERN_SPECIAL(interrupt_mode);
  Fset(Qinterrupt_mode, rep_nil);
  rep_INTERN_SPECIAL(before_exit_hook);
}
