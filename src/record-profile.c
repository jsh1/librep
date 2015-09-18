/* record-profile.c -- very basic Lisp profiler

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

/* Commentary:

   Hook into the interrupt-checking code to record the current
   backtrace statistics. Uses SIGPROF to tell the lisp system when it
   should interrupt (can't run the profiler off the signal itself,
   since data would need to be allocated from the signal handler) */

#include "repint.h"

#include <signal.h>
#include <time.h>

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

static repv profile_table;
static bool profiling;

static void (*chained_test_interrupt)(void);

static int profile_interval = 10;		/* microseconds */

#ifdef HAVE_SETITIMER

static RETSIGTYPE
sigprof_handler(int unused)
{
  /* force an interrupt */
  rep_test_int_counter = rep_TEST_INT_PERIOD;
}

#endif

static void
set_timer(void)
{
#ifdef HAVE_SETITIMER
  struct itimerval it, tem;
  it.it_interval.tv_usec = 0;
  it.it_interval.tv_sec = 0;
  it.it_value.tv_usec = profile_interval % 1000000;
  it.it_value.tv_sec = profile_interval / 1000000;
  setitimer(ITIMER_PROF, &it, &tem);
  signal(SIGPROF, sigprof_handler);
#endif
}

static void
clear_timer(void)
{
#ifdef HAVE_SETITIMER
  signal(SIGPROF, SIG_IGN);
#endif
}

static void
test_interrupt(void)
{
  if (!profiling) {
    goto out;
  }

  repv *seen = rep_stack_alloc(repv, rep_max_lisp_depth);
  if (!seen) {
    goto out;
  }

  size_t seen_count = 0;

  for (const rep_stack_frame *c = rep_call_stack;
       c != 0 && c->fun != rep_nil; c = c->next)
  {
    repv name;

    switch (rep_TYPE(c->fun)) {
    case rep_Subr:
      name = rep_SUBR(c->fun)->name;
      break;
    case rep_Closure:
      name = rep_CLOSURE(c->fun)->name;
      break;
    default:
      continue;
    }

    if (rep_SYMBOLP(name)) {
      name = rep_SYM(name)->name;
    }

    if (!rep_STRINGP(name)) {
      continue;
    }

    name = Fintern(name, rep_nil);
    if (!name) {
      continue;
    }

    bool seen_name = false;
    for (int j = 0; j < seen_count; j++) {
      if (seen[j] == name) {
	seen_name = true;
	break;
      }
    }
    if (seen_name) {
      continue;
    }

    repv tem = F_structure_ref(profile_table, name);
    if (rep_VOIDP(tem)) {
      tem = Fcons(rep_MAKE_INT(0), rep_MAKE_INT(0));
    }

    if (c == rep_call_stack) {
      rep_CAR(tem) = rep_MAKE_INT(rep_INT(rep_CAR(tem)) + 1);
    }

    rep_CDR(tem) = rep_MAKE_INT(rep_INT(rep_CDR(tem)) + 1);

    Fstructure_define(profile_table, name, tem);
    seen[seen_count++] = name;
  }

  set_timer();

out:
  if (chained_test_interrupt) {
    (*chained_test_interrupt)();
  }
}

DEFUN("start-profiler", Fstart_profiler, Sstart_profiler, (void), rep_Subr0)
{
  profile_table = Fmake_structure(rep_nil, rep_nil, rep_nil, rep_nil);
  profiling = true;
  set_timer();
  return Qt;
}

DEFUN("stop-profiler", Fstop_profiler, Sstop_profiler, (void), rep_Subr0)
{
  profiling = false;
  clear_timer();
  return Qt;
}

DEFUN("fetch-profile", Ffetch_profile, Sfetch_profile, (void), rep_Subr0)
{
  return profile_table ? profile_table : rep_nil;
}

DEFUN("profile-interval", Fprofile_interval,
       Sprofile_interval, (repv arg), rep_Subr1)
{
  repv ret = rep_MAKE_INT(profile_interval);

  if (rep_INTP(arg) && rep_INT(arg) > 0) {
    profile_interval = rep_INT(arg);
  }

  return ret;
}


/* init */

repv
rep_dl_init(void)
{
#ifdef HAVE_SETITIMER
  signal(SIGPROF, SIG_IGN);
#endif

  chained_test_interrupt = rep_test_int_fun;
  rep_test_int_fun = test_interrupt;

  repv tem = rep_push_structure("rep.lang.record-profile");
  rep_ADD_SUBR(Sstart_profiler);
  rep_ADD_SUBR(Sstop_profiler);
  rep_ADD_SUBR(Sfetch_profile);
  rep_ADD_SUBR(Sprofile_interval);
  rep_mark_static(&profile_table);
  return rep_pop_structure(tem);
}
