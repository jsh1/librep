/* input.c -- generic input multiplexing

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
#include <stdlib.h>
#include <signal.h>
#include <sys/select.h>

/* Function to call to flush any changes to the screen. */

void (*rep_redisplay_fun)(void);

/* Functions to call as file descriptors are registered for input. */

void (*rep_register_input_fd_fun)(int fd, void (*callback)(int fd)) = 0;
void (*rep_deregister_input_fd_fun)(int fd) = 0;

/* When non-nil, overrides part of wait_for_input(). */

int (*rep_wait_for_input_fun)(fd_set *inputs, int timeout_msecs);

int rep_input_timeout_secs = 1;

/* The file descriptors we're waiting for input from. */

static fd_set input_fdset;

/* For every bit set in `input_set` there should be a corresponding
   function in here that will be called when input becomes available. */

static void (*input_actions[FD_SETSIZE])(int);

/* File descriptors in this set have input available but not yet read. */

static fd_set input_pending;
static int input_pending_count;

#define MAX_EVENT_LOOP_CALLBACKS 16
static int next_event_loop_callback;
static bool (*event_loop_callbacks[MAX_EVENT_LOOP_CALLBACKS])(void);

void
rep_register_input_fd(int fd, void (*callback)(int fd))
{
  FD_SET(fd, &input_fdset);
  input_actions[fd] = callback;

  if (rep_register_input_fd_fun) {
    (*rep_register_input_fd_fun) (fd, callback);
  }

  rep_set_fd_cloexec(fd);
}

void
rep_deregister_input_fd(int fd)
{
  FD_CLR(fd, &input_fdset);
  input_actions[fd] = NULL;

  if (rep_deregister_input_fd_fun) {
    (*rep_deregister_input_fd_fun) (fd);
  }
}

void
rep_map_inputs(void (*fun)(int fd, void (*callback)(int)))
{
  for (int i = 0; i < FD_SETSIZE; i++) {
    if (input_actions[i] != 0) {
      fun(i, input_actions[i]);
    }
  }
}

void
rep_mark_input_pending(int fd)
{
  if (!FD_ISSET(fd, &input_pending)) {
    FD_SET(fd, &input_pending);
    input_pending_count++;
  }
}

void
rep_sleep_for(int secs, int msecs)
{
  struct timeval timeout;
  timeout.tv_sec = secs + msecs / 1000;
  timeout.tv_usec = (msecs % 1000) * 1000;
  select(FD_SETSIZE, NULL, NULL, NULL, &timeout);
}

void
rep_add_event_loop_callback(bool (*callback)(void))
{
  if (next_event_loop_callback == MAX_EVENT_LOOP_CALLBACKS) {
    abort();
  }

  event_loop_callbacks[next_event_loop_callback++] = callback;
}

bool
rep_proc_periodically(void)
{
  bool ret = false;

  for (int i = 0; i < next_event_loop_callback; i++) {
    if (event_loop_callbacks[i]()) {
      ret = true;
    }
  }

  return ret;
}

/* Wait for input for no longer than TIMEOUT-MSECS for input fds defined
   by INPUTS. If input arrived return the number of ready fds, with the
   actual fds defined by the fdset INPUTS. Return zero if the timeout
   was reached. */

static int
wait_for_input(fd_set *inputs, int timeout_msecs)
{
  /* While there's pending input available, return it. */

  if (input_pending_count > 0) {
    fd_set out;
    int count = 0;
    int seen = 0;
    for (int i = 0; seen < input_pending_count && i < FD_SETSIZE; i++) {
      if (FD_ISSET(i, &input_pending)) {
	seen++;
	if (FD_ISSET(i, inputs)) {
	  if (count == 0) {
	    FD_ZERO(&out);
	  }
	  FD_SET(i, &out);
	  count++;
	}
      }
    }

    if (count > 0) {
      memcpy(inputs, &out, sizeof(out));
      return count;
    }
  }

  /* Allow embedders to override this part of the function. */

  if (rep_wait_for_input_fun) {
    return (*rep_wait_for_input_fun)(inputs, timeout_msecs);
  }

  /* Break the timeout into one-second chunks, then check for interrupt
     between each call to select. */

  while (timeout_msecs > 0) {
    int this_timeout_msecs = MIN(timeout_msecs, rep_input_timeout_secs * 1000);
    int actual_timeout_msecs = this_timeout_msecs;

    struct timeval timeout;
    timeout.tv_sec = actual_timeout_msecs / 1000;
    timeout.tv_usec = (actual_timeout_msecs % 1000) * 1000;

    fd_set copy;
    memcpy(&copy, inputs, sizeof(copy));

    /* Don't want select() to restart after a SIGCHLD or SIGALRM; there
       may be a notification to dispatch.  */

    rep_sig_restart(SIGCHLD, false);
    rep_sig_restart(SIGALRM, false);

    int ready = select(FD_SETSIZE, &copy, NULL, NULL, &timeout);

    rep_sig_restart(SIGALRM, true);
    rep_sig_restart(SIGCHLD, true);

    timeout_msecs -= this_timeout_msecs;

    if (ready > 0) {
      memcpy(inputs, &copy, sizeof(copy));
      return ready;
    }

    rep_TEST_INT_SLOW;
    if (rep_INTERRUPTP) {
      break;
    }
  }

  return 0;
}

/* Handles the READY fds with pending input (defined by fdset INPUTS).
   Returns true if the display might require updating. Returns
   immediately if an exception occurs. */

static bool
handle_input(fd_set *inputs, int ready)
{
  static int idle_period;

  bool should_redisplay = false;

  if (ready > 0) {
    idle_period = 0;

    for (int i = 0; i < FD_SETSIZE && ready > 0 && !rep_INTERRUPTP; i++) {
      if (FD_ISSET(i, inputs)) {
	ready--;
	if (FD_ISSET(i, &input_pending)) {
	  FD_CLR(i, &input_pending);
	  input_pending_count--;
	}
	if (input_actions[i] != NULL) {
	  input_actions[i](i);
	  should_redisplay = true;
	}
      }
    }

  } else if (ready == 0) {
    if (rep_INTERRUPTP || rep_on_idle(idle_period)) {
      should_redisplay = true;
    }

    idle_period++;
  }

  if (!rep_INTERRUPTP && rep_proc_periodically()) {
    should_redisplay = true;
  }

  return should_redisplay;
}

/* The input handler loop. */

repv
rep_event_loop(void)
{
  repv result = rep_nil;

  if (rep_redisplay_fun) {
    (*rep_redisplay_fun)();
  }

  while (1) {
    bool should_redisplay = false;

    if (!rep_INTERRUPTP) {
      fd_set copy;
      memcpy(&copy, &input_fdset, sizeof(copy));

      int ready = wait_for_input(&copy, rep_input_timeout_secs * 1000);

      should_redisplay = handle_input(&copy, ready);
    }

    if (rep_INTERRUPTP) {
      if (rep_handle_input_exception(&result)) {
	return result;
      } else {
	should_redisplay = true;
      }
    }

    if (should_redisplay && rep_redisplay_fun) {
      (*rep_redisplay_fun)();
    }

#ifdef C_ALLOCA
    alloca(0);
#endif
  }

  return result;
}

repv
rep_sit_for(int timeout_msecs)
{
  if (timeout_msecs != 0 && rep_redisplay_fun) {
    (*rep_redisplay_fun)();
  }

  fd_set copy;
  memcpy(&copy, &input_fdset, sizeof(copy));

  int ready = wait_for_input(&copy, timeout_msecs);

  return rep_INTERRUPTP ? 0 : ready > 0 ? rep_nil : Qt;
}

/* Wait TIMEOUT_MSECS for input, ignoring any input fds that would
   invoke any callback function except CALLBACKS. Return nil if any
   input was serviced, t if the timeout expired, 0 for an error. */

repv
rep_accept_input_for_callbacks(int timeout_msecs, int ncallbacks,
			       void (**callbacks)(int))
{
  fd_set copy;
  FD_ZERO(&copy);

  for (int i = 0; i < FD_SETSIZE; i++) {
    if (FD_ISSET(i, &input_fdset)) {
      for (int j = 0; j < ncallbacks; j++) {
	if (input_actions[i] == callbacks[j]) {
	  FD_SET(i, &copy);
	  break;
	}
      }
    }
  }

  int ready = wait_for_input(&copy, timeout_msecs);

  if (ready > 0 && !rep_INTERRUPTP) {
    handle_input(&copy, ready);
  }

  return rep_INTERRUPTP ? 0 : ready > 0 ? rep_nil : Qt;
}

/* Wait TIMEOUT_MSECS for input from the NFDS file descriptors stored
   in FDS. Return nil if any input was serviced, t if the timeout
   expired, 0 for an error. */

repv
rep_accept_input_for_fds(int timeout_msecs, int nfds, int *fds)
{
  fd_set copy;
  FD_ZERO(&copy);

  for (int i = 0; i < nfds; i++) {
    if (FD_ISSET(fds[i], &input_fdset)) {
      FD_SET(fds[i], &copy);
    }
  }

  int ready = wait_for_input(&copy, timeout_msecs);

  if (ready > 0 && !rep_INTERRUPTP) {
    handle_input(&copy, ready);
  }

  return rep_INTERRUPTP ? 0 : ready > 0 ? rep_nil : Qt;
}

/* For compatibility. */

repv
rep_accept_input(int timeout_msecs, void (*callback)(int))
{
  return rep_accept_input_for_callbacks(timeout_msecs, 1, &callback);
}

bool
rep_poll_input(int fd)
{
  fd_set in;
  FD_ZERO(&in);
  FD_SET(fd, &in);

  return wait_for_input(&in, 0);
}

DEFUN("sleep-for", Fsleep_for, Ssleep_for, (repv secs, repv msecs),
      rep_Subr2) /*
::doc:rep.system#sleep-for::
sleep-for SECONDS [MILLISECONDS]

Pause for SECONDS (plus the optional MILLISECOND component) length of time.
::end:: */
{
  rep_DECLARE1(secs, rep_NUMERICP);
  rep_DECLARE2_OPT(msecs, rep_NUMERICP);

  rep_sleep_for(rep_get_long_int(secs), rep_get_long_int(msecs));

  return Qt;
}

DEFUN("sit-for", Fsit_for, Ssit_for, (repv secs, repv msecs),
      rep_Subr2) /*
::doc:rep.system#sit-for::
sit-for [SECONDS] [MILLISECONDS]

Wait for input to arrive and be processed. No more than SECONDS seconds plus
MILLISECONDS milliseconds will be waited. If at the end of this time no
input has arrived, return t. Otherwise return nil if input was found.

If neither SECONDS nor MILLISECONDS is defined the command will return
immediately, using a null timeout.
::end:: */
{
  rep_DECLARE1_OPT(secs, rep_NUMERICP);
  rep_DECLARE2_OPT(msecs, rep_NUMERICP);

  return rep_sit_for(((rep_get_long_int(secs)) * 1000)
		     + rep_get_long_int(msecs));
}

void
rep_input_init(void)
{
  FD_ZERO(&input_fdset);
  FD_ZERO(&input_pending);

  repv tem = rep_push_structure("rep.system");
  rep_ADD_SUBR(Ssleep_for);
  rep_ADD_SUBR(Ssit_for);
  rep_pop_structure(tem);
}
