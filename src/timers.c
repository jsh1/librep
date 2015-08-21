/* timers.c -- call a function after a period of time has passed

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

#include <signal.h>
#include <time.h>
#include <assert.h>

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

static int timer_type;

#define TIMER(v)  ((Lisp_Timer *)rep_PTR(v))
#define TIMERP(v) rep_CELL16_TYPEP(v, timer_type)

typedef struct lisp_timer Lisp_Timer;

struct lisp_timer {
  repv car;
  Lisp_Timer *next;
  Lisp_Timer *next_alloc;
  repv function;
  int secs, msecs;
  int rel_secs, rel_msecs;
  bool fired : 1;
  bool deleted : 1;
};

/* List of all allocated timer objects, through next_alloc field. */

static Lisp_Timer *allocated_timers;

/* List of all pending timers, through next field. Only ever touch this
  variable if SIGALRM is blocked! */

static Lisp_Timer *timer_chain;

/* Pipe used to trigger the input callback. */

static int pipe_fds[2];

/* Contains SIGALRM. */

static sigset_t alrm_sigset;

static RETSIGTYPE
timer_signal_handler(int sig)
{
  Lisp_Timer *t = timer_chain;
  assert(t != 0);

  t->rel_secs = t->rel_msecs = 0;

  while (t && t->rel_secs == 0 && t->rel_msecs == 0) {
    t->fired = true;
    t = t->next;
  }

  int dummy = 0;
  write(pipe_fds[1], &dummy, sizeof(dummy));
}

/* Only called with SIGALRM blocked. */

static void
setup_next_timer(void)
{
  if (timer_chain
      && (timer_chain->rel_secs > 0 || timer_chain->rel_msecs > 0))
  {
#ifdef HAVE_SETITIMER
    struct itimerval it, tem;
    it.it_interval.tv_usec = 0;
    it.it_interval.tv_sec = 0;
    it.it_value.tv_usec = timer_chain->rel_msecs * 1000;
    it.it_value.tv_sec = timer_chain->rel_secs;
    setitimer(ITIMER_REAL, &it, &tem);
#else
    alarm(timer_chain->secs);
#endif
    signal(SIGALRM, timer_signal_handler);
  } else {
    signal(SIGALRM, SIG_IGN);
  }
}

static inline void
fix_time(int *secs, int *msecs)
{
  while (*msecs < 0) {
    *msecs += 1000;
    (*secs)--;
  }

  while (*msecs >= 1000) {
    *msecs -= 1000;
    (*secs)++;
  }
}

static void
insert_timer(Lisp_Timer *t)
{
  sigset_t old;
  sigprocmask(SIG_BLOCK, &alrm_sigset, &old);

  if (t->secs > 0 || t->msecs > 0) {
    t->rel_secs = t->secs;
    t->rel_msecs = t->msecs;
    t->fired = false;
    t->deleted = false;

    Lisp_Timer **ptr = &timer_chain;
    while (*ptr != 0
	   && ((*ptr)->rel_secs < t->rel_secs
	       || ((*ptr)->rel_secs == t->rel_secs
		   && (*ptr)->rel_msecs <= t->rel_msecs)))
    {
      t->rel_msecs -= (*ptr)->rel_msecs;
      t->rel_secs -= (*ptr)->rel_secs;
      fix_time(&t->rel_secs, &t->rel_msecs);
      ptr = &((*ptr)->next);
    }

    if (*ptr) {
      (*ptr)->rel_msecs -= t->rel_msecs;
      (*ptr)->rel_secs -= t->rel_secs;
      fix_time(&(*ptr)->rel_secs, &(*ptr)->rel_msecs);
    }

    t->next = *ptr;
    *ptr = t;

    if (timer_chain == t) {
      setup_next_timer();
    }
  }

  sigprocmask(SIG_SETMASK, &old, 0);
}

static void
delete_timer(Lisp_Timer *t)
{
  sigset_t old;
  sigprocmask(SIG_BLOCK, &alrm_sigset, &old);

  t->deleted = true;

  Lisp_Timer **ptr = &timer_chain;
  while (*ptr != 0 && (*ptr) != t) {
    ptr = &((*ptr)->next);
  }

  if (*ptr == t) {
    if (t->next) {
      t->next->rel_msecs += t->rel_msecs;
      t->next->rel_secs += t->rel_secs;
      fix_time(&t->next->rel_secs, &t->next->rel_msecs);
    }

    t->rel_secs = t->rel_msecs = 0;

    *ptr = t->next;

    if (ptr == &timer_chain) {
      setup_next_timer();
    }
  }

  sigprocmask(SIG_SETMASK, &old, 0);
}

static void
timer_fd_handler(int fd)
{
  int dummy;
  read(pipe_fds[0], &dummy, sizeof(dummy));

  sigset_t old;
  sigprocmask(SIG_BLOCK, &alrm_sigset, &old);

  int ready = 0;
  for (Lisp_Timer *t = timer_chain; t != 0 && t->fired; t = t->next) {
    ready++;
  }

  repv timers[ready];

  for (int i = 0; i < ready; i++) {
    timers[i] = rep_VAL(timer_chain);
    timer_chain = timer_chain->next;
  }

  setup_next_timer();
  sigprocmask(SIG_SETMASK, &old, 0);

  rep_GC_n_roots gc_timers;
  rep_PUSHGCN(gc_timers, timers, ready);

  for (int i = 0; i < ready; i++) {
    if (!TIMER(timers[i])->deleted) {
      rep_call_lisp1(TIMER(timers[i])->function, timers[i]);
    }
  }

  rep_POPGCN;
}

DEFUN("make-timer", Fmake_timer, Smake_timer,
      (repv fun, repv secs, repv msecs), rep_Subr3) /*
::doc:rep.io.timers#make-timer::
make-timer FUNCTION [SECONDS] [MILLISECONDS]

Create and return a new one-shot timer object. After SECONDS*1000 +
MILLISECONDS milliseconds FUNCTION will be called.

Note that the timer will only fire _once_, use the `set-timer' function
to re-enable it.
::end:: */
{
  Lisp_Timer *t = rep_alloc(sizeof(Lisp_Timer));
  rep_data_after_gc += sizeof(Lisp_Timer);

  t->car = timer_type;
  t->function = fun;
  t->secs = (int) rep_get_long_int(secs);
  t->msecs = (int) rep_get_long_int(msecs);
  fix_time(&t->secs, &t->msecs);

  t->next_alloc = allocated_timers;
  allocated_timers = t;

  insert_timer(t);

  return rep_VAL(t);
}

DEFUN("delete-timer", Fdelete_timer, Sdelete_timer, (repv timer), rep_Subr1) /*
::doc:rep.io.timers#delete-timer::
delete-timer TIMER

Prevent the one-shot timer TIMER from firing(i.e. calling the function
associated with it). If the timer has already fired, this function has
no effect.
::end:: */
{
  rep_DECLARE1(timer, TIMERP);

  delete_timer(TIMER(timer));

  return timer;
}

DEFUN("set-timer", Fset_timer, Sset_timer,
      (repv timer, repv secs, repv msecs), rep_Subr3) /*
::doc:rep.io.timers#set-timer::
set-timer TIMER [SECONDS] [MILLISECONDS]

Restart the one-shot timer TIMER. If SECONDS and/or MILLISECONDS is
defined the period after which it fires will be reset to the specified
duration. Otherwise, the existing values are preserved.
::end:: */
{
  rep_DECLARE1(timer, TIMERP);
  rep_DECLARE2_OPT(secs, rep_NUMERICP);
  rep_DECLARE3_OPT(msecs, rep_NUMERICP);

  delete_timer(TIMER(timer));

  if (secs != rep_nil || msecs != rep_nil) {
    TIMER(timer)->secs = (int) rep_get_long_int(secs);
    TIMER(timer)->msecs = (int) rep_get_long_int(msecs);
    fix_time(&TIMER(timer)->secs, &TIMER(timer)->msecs);
  }

  insert_timer(TIMER(timer));

  return timer;
}

static void
timer_mark(repv val)
{
  rep_MARKVAL(TIMER(val)->function);
}

static void
timer_mark_active(void)
{
  sigset_t old;
  sigprocmask(SIG_BLOCK, &alrm_sigset, &old);

  for (Lisp_Timer *t = timer_chain; t; t = t->next) {
    rep_MARKVAL(rep_VAL(t));
  }

  sigprocmask(SIG_SETMASK, &old, 0);
}

static void
timer_sweep(void)
{
  Lisp_Timer *t = allocated_timers;
  allocated_timers = 0;

  while (t) {
    Lisp_Timer *next = t->next_alloc;

    if (!rep_GC_CELL_MARKEDP(rep_VAL(t))) {
      rep_free(t);
    } else {
      rep_GC_CLR_CELL(rep_VAL(t));
      t->next_alloc = allocated_timers;
      allocated_timers = t;
    }

    t = next;
  }
}

static void
timer_print(repv stream, repv arg)
{
  char buf[64];
#ifdef HAVE_SNPRINTF
  snprintf(buf, sizeof(buf), "#<timer %ds, %dms>",
	   TIMER(arg)->secs, TIMER(arg)->msecs);
#else
  sprintf(buf, "#<timer %lds, %ldms>", TIMER(arg)->secs, TIMER(arg)->msecs);
#endif
  rep_stream_puts(stream, buf, -1, false);
}

repv
rep_dl_init(void)
{
  static rep_type timer = {
    .name = "timer",
    .print = timer_print,
    .sweep = timer_sweep,
    .mark = timer_mark,
    .mark_type = timer_mark_active,
  };

  timer_type = rep_define_type(&timer);

  pipe(pipe_fds);
  rep_register_input_fd(pipe_fds[0], timer_fd_handler);
  rep_set_fd_cloexec(pipe_fds[1]);
  sigemptyset(&alrm_sigset);
  sigaddset(&alrm_sigset, SIGALRM);
  rep_sig_restart(SIGALRM, true);

  repv tem = rep_push_structure("rep.io.timers");
  rep_ADD_SUBR(Smake_timer);
  rep_ADD_SUBR(Sdelete_timer);
  rep_ADD_SUBR(Sset_timer);
  return rep_pop_structure(tem);
}

void
rep_dl_kill(void)
{
  rep_deregister_input_fd(pipe_fds[0]);
  close(pipe_fds[0]);
  close(pipe_fds[1]);
  signal(SIGALRM, SIG_IGN);
}
