/* signals.c -- signal handlers

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

static volatile bool in_fatal_signal_handler;

/* Turns on or off restarted system calls for signal SIG. */

void
rep_sig_restart(int sig, bool flag)
{
#if defined(HAVE_SIGINTERRUPT)
  siginterrupt(sig, !flag);
#else
  struct sigaction act;
  sigaction(sig, 0, &act);
  if (flag) {
# if defined(SA_RESTART)
    act.sa_flags |= SA_RESTART;
# elif defined(SA_INTERRUPT)
    act.sa_flags &= ~SA_INTERRUPT;
# endif
  } else {
# if defined(SA_RESTART)
    act.sa_flags &= ~SA_RESTART;
# elif defined(SA_INTERRUPT)
    act.sa_flags |= SA_INTERRUPT;
# endif
  }
  sigaction(sig, &act, 0);
#endif /* !HAVE_SIGINTERRUPT */
}

/* Invoked by any of the handlable error reporting signals. */

static RETSIGTYPE
fatal_signal_handler(int sig)
{
  /* Sometimes this function can get in an infinite loop, even with the
     in_fatal_signal_handler exclusion? Does this help..? */

  signal(sig, SIG_DFL);

  /* Check for nested calls to this function. */

  if (in_fatal_signal_handler) {
    raise(sig);
  }

  in_fatal_signal_handler = true;

#ifdef HAVE_PSIGNAL
  psignal(sig, "rep: received fatal signal");
#else
# ifdef HAVE_STRSIGNAL
  fprintf(stderr, "rep: received fatal signal: %s\n", strsignal(sig));
# else
  fprintf(stderr, "rep: received fatal signal: %d\n", sig);
# endif
#endif

  /* Save the C backtrace. */

  rep_db_print_backtrace(rep_common_db, "fatal_signal_handler");

  /* Output all debug buffers. */

  rep_db_spew_all();

  /* Try and output the Lisp call stack; this may or may not provoke
     another error, but who cares.. */

  fprintf(stderr, "\nLisp backtrace:\n");
  Fbacktrace(Fstderr_file());
  fputs("\n", stderr);

  /* Now reraise the signal, since it's currently blocked the default
     action will occur, i.e. termination */

  raise(sig);
}

/* Invoked by SIGINT (i.e. ^C) */

static RETSIGTYPE
interrupt_signal_handler(int sig)
{
  if (rep_throw_value == rep_int_cell) {
    signal(sig, SIG_DFL);
    raise(sig);
  } else {
    rep_throw_value = rep_int_cell;
    signal(sig, interrupt_signal_handler);
  }
}

/* Invoked by trappable termination signals. */

static RETSIGTYPE
termination_signal_handler(int sig)
{
  if (rep_throw_value == rep_term_cell) {
    signal(sig, SIG_DFL);
    raise(sig);
  } else {
    rep_throw_value = rep_term_cell;
    signal(sig, termination_signal_handler);
  }
}

/* Invoked by SIGUSR1 or SIGUSR2. */

static RETSIGTYPE
usr_signal_handler(int sig)
{
  switch (sig) {
  case SIGUSR1:
    fprintf(stderr, "\n\nLisp backtrace:\n");
    Fbacktrace(Fstderr_file());
    fputs("\n\n", stderr);
    break;

  case SIGUSR2:
    fprintf(stderr, "\n\nDebug buffers:\n");
    rep_db_spew_all();
    fputc('\n', stderr);
    break;
  }

  signal(sig, usr_signal_handler);
}

void
rep_signals_init(void)
{
  /* First the error signals. */

#ifndef IGNORE_FATAL_SIGNALS
#ifdef SIGFPE
  if (signal(SIGFPE, fatal_signal_handler) == SIG_IGN) {
    signal(SIGFPE, SIG_IGN);
  }
#endif
#ifdef SIGILL
  if (signal(SIGILL, fatal_signal_handler) == SIG_IGN) {
    signal(SIGILL, SIG_IGN);
  }
#endif
#ifdef SIGSEGV
  if (signal(SIGSEGV, fatal_signal_handler) == SIG_IGN) {
    signal(SIGSEGV, SIG_IGN);
  }
#endif
#ifdef SIGBUS
  if (signal(SIGBUS, fatal_signal_handler) == SIG_IGN) {
    signal(SIGBUS, SIG_IGN);
  }
#endif
#ifdef SIGQUIT
  if (signal(SIGQUIT, fatal_signal_handler) == SIG_IGN) {
    signal(SIGQUIT, SIG_IGN);
  }
#endif
#ifdef SIGABRT
  if (signal(SIGABRT, fatal_signal_handler) == SIG_IGN) {
    signal(SIGABRT, SIG_IGN);
  }
#endif
#endif

  /* Install the interrupt handler. */

#ifdef SIGINT
  if (signal(SIGINT, interrupt_signal_handler) == SIG_IGN) {
    signal(SIGINT, SIG_IGN);
  } else {
    rep_sig_restart(SIGINT, false);
  }
#endif

  /* Finally, the termination signals. */

#ifdef SIGTERM
  if (signal(SIGTERM, termination_signal_handler) == SIG_IGN) {
    signal(SIGTERM, SIG_IGN);
  } else {
    rep_sig_restart(SIGTERM, false);
  }
#endif
#ifdef SIGHUP
  if (signal(SIGHUP, termination_signal_handler) == SIG_IGN) {
    signal(SIGHUP, SIG_IGN);
  } else {
    rep_sig_restart(SIGHUP, false);
  }
#endif

#ifdef SIGUSR1
  signal(SIGUSR1, usr_signal_handler);
#endif
#ifdef SIGUSR2
  signal(SIGUSR2, usr_signal_handler);
#endif
}
