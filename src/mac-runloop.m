/* mac-runloop.m -- runloop integration for Mac OS X

   Copyright (C) 1999-2015 John Harper <jsh@unfactored.org>

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

#include "rep.h"

#include <pthread.h>
#include <libkern/OSAtomic.h>

#import <AppKit/AppKit.h>
#import <Foundation/Foundation.h>
#import <CoreFoundation/CoreFoundation.h>

#define OBJC_BEGIN 				\
  do {						\
    @autoreleasepool {				\
      @try

#define OBJC_END				\
      @catch (id ex) {				\
	NSLog(@"rep: objc exception: %@", ex);	\
      }						\
    }						\
  } while (0);

/* public functions. */

extern bool mac_defer_event(void *view, void *ns_event);
extern void mac_set_needs_display(void);

typedef struct input_data_struct input_data;
typedef struct runloop_context_struct runloop_context;

struct input_data_struct {
  input_data *next;
  int fd;
  void (*func)(int);
  CFRunLoopSourceRef source;
  int32_t pending;
};

struct runloop_context_struct {
  runloop_context *next;
  int timed_out;
  int idle_counter;
  int this_timeout_msecs;
  int actual_timeout_msecs;
  CFRunLoopTimerRef timer;
};

static CFRunLoopObserverRef runloop_observer;

static int input_pipe[2];
static input_data *inputs;
static pthread_mutex_t input_mutex = PTHREAD_MUTEX_INITIALIZER;

static int sigchld_pipe[2];
static CFRunLoopSourceRef sigchld_source;

static int waiting_for_input;

static runloop_context *current_context;

static bool needs_redisplay;

static void
empty_pipe(int fd)
{
  char buf[64];
  while (read(fd, buf, sizeof(buf)) > 0) {
  }
}

/* Writes a single byte into the given pipe file descriptor. */

static void
signal_pipe(int fd)
{
  /* This function can be called from signal handlers, so can't do much. */

  while (1) {
    char c = 1;
    int err = write(fd, &c, 1);
    if (err >= 0 || errno != EINTR) {
      return;
    }
  }
}

static void *
input_thread(void *arg)
{
  pthread_mutex_lock(&input_mutex);

  while (1) {
    fd_set copy;
    FD_ZERO(&copy);
    FD_SET(input_pipe[0], &copy);
    FD_SET(sigchld_pipe[0], &copy);

    for (input_data *d = inputs; d; d = d->next) {
      if (d->pending == 0) {
	FD_SET(d->fd, &copy);
      }
    }

    pthread_mutex_unlock(&input_mutex);

    int err = select(FD_SETSIZE, &copy, NULL, NULL, NULL);

    /* We may get EBADF errors if the fd-set changed while we were
       blocked, but that's okay, we'll be in sync with the main thread
       next time we wait. All the other possible errors should be
       harmless also. */

    pthread_mutex_lock(&input_mutex);

    bool need_wake = false;

    if (err > 0 && FD_ISSET(input_pipe[0], &copy)) {
      empty_pipe(input_pipe[0]);
      err--;
    }

    if (err > 0 && FD_ISSET(sigchld_pipe[0], &copy)) {
      empty_pipe(sigchld_pipe[0]);
      CFRunLoopSourceSignal(sigchld_source);
      need_wake = true;
      err--;
    }

    for (input_data *d = inputs; err > 0 && d; d = d->next) {
      if (FD_ISSET(d->fd, &copy)) {
	OSAtomicIncrement32(&d->pending);
	CFRunLoopSourceSignal(d->source);
	need_wake = true;
	err--;
      }
    }

    if (need_wake) {
      CFRunLoopWakeUp(CFRunLoopGetMain());
    }
  }

  /* not reached */
}

static void
stop_application(void)
{
  /* Just calling -stop: doesn't work, we're probably stuck waiting for
     an event to arrive, and -stop: simply sets a variable to true. So
     post ourselves an event to shake things through. */

  OBJC_BEGIN {

    [NSApp stop:nil];

    NSEvent *e = [NSEvent otherEventWithType:NSApplicationDefined
      location:[NSEvent mouseLocation] modifierFlags:0 timestamp:0
      windowNumber:0 context:nil subtype:0 data1:0 data2:0];

    [NSApp postEvent:e atStart:NO];

  } OBJC_END
}

static void
source_perform(void *info)
{
  input_data *d = info;

  if (waiting_for_input == 0 && d->pending != 0) {
    signal_pipe(input_pipe[1]);
    OSAtomicDecrement32(&d->pending);
    d->func(d->fd);
    needs_redisplay = true;
  } else {
    stop_application();
  }
}

static void
register_input_fd(int fd, void(*callback)(int fd))
{
  if (!callback) {
    return;
  }

  input_data *d = calloc(1, sizeof(*d));

  CFRunLoopSourceContext ctx;
  memset(&ctx, 0, sizeof(ctx));
  ctx.version = 0;
  ctx.info = d;
  ctx.perform = source_perform;

  d->fd = fd;
  d->func = callback;
  d->source = CFRunLoopSourceCreate(NULL, 0, &ctx);

  pthread_mutex_lock(&input_mutex);

  d->next = inputs;
  inputs = d;

  pthread_mutex_unlock(&input_mutex);

  CFRunLoopAddSource(CFRunLoopGetCurrent(), d->source, kCFRunLoopCommonModes);

  signal_pipe(input_pipe[1]);
}

static void
deregister_input_fd(int fd)
{
  pthread_mutex_lock(&input_mutex);

  input_data **ptr, *d;
  for (ptr = &inputs; (d = *ptr) != 0; ptr = &d->next) {
    if (d->fd == fd) {
      *ptr = d->next;
      CFRunLoopRemoveSource(CFRunLoopGetCurrent(), d->source,
			    kCFRunLoopCommonModes);
      CFRelease(d->source);
      free(d);
      signal_pipe(input_pipe[1]);
      break;
    }
  }

  pthread_mutex_unlock(&input_mutex);
}

static void
timer_callback(CFRunLoopTimerRef timer, void *info)
{
  runloop_context *d = info;

  d->timed_out = 1;

  /* Only quit if we'd return to the correct event loop.
     FIXME: this doesn't do anything until an event arrives? */

  if (current_context == d) {
    stop_application();
  }
}

static void
remove_timeout(void)
{
  runloop_context *ctx = current_context;

  if (ctx && ctx->timer) {
    CFRunLoopRemoveTimer(CFRunLoopGetCurrent(), ctx->timer,
			 kCFRunLoopCommonModes);
    CFRelease(ctx->timer);
    ctx->timer = NULL;
  }
}

static void
set_timeout(int timeout_msecs)
{
  runloop_context *ctx = current_context;

  if (ctx) {
    ctx->this_timeout_msecs = timeout_msecs;
    ctx->actual_timeout_msecs = ctx->this_timeout_msecs;

    CFAbsoluteTime abs_t = (CFAbsoluteTimeGetCurrent()
			    + ctx->actual_timeout_msecs / 1000.);

    if (!ctx->timer) {
      CFRunLoopTimerContext c;
      memset(&c, 0, sizeof(c));
      c.info = ctx;
      ctx->timer = CFRunLoopTimerCreate(NULL, abs_t, 3153600000,
					0, 0, timer_callback, &c);
      CFRunLoopAddTimer(CFRunLoopGetCurrent(), ctx->timer,
			kCFRunLoopCommonModes);
    } else {
      CFRunLoopTimerSetNextFireDate(ctx->timer, abs_t);
    }
  }
}

/* This function replaces the standard rep event loop. */

static repv
event_loop(void)
{
  runloop_context ctx;
  memset(&ctx, 0, sizeof(ctx));
  ctx.next = current_context;
  current_context = &ctx;

  while (1) {
    bool kick_input = false;

  again:
    for (input_data *d = inputs; d; d = d->next) {
      if (d->pending != 0) {
	OSAtomicDecrement32(&d->pending);
	d->func(d->fd);
	/* callout may have modified inputs list. */
	goto again;
      }
    }

    if (kick_input) {
      signal_pipe(input_pipe[1]);
    }

    if (rep_redisplay_fun) {
      (*rep_redisplay_fun)();
    }

    needs_redisplay = false;

    [NSApp run];

    rep_proc_periodically();

    /* Check for exceptional conditions. */

    if (rep_throw_value) {
      repv result;
      if (rep_handle_input_exception(&result)) {
	remove_timeout();
	current_context = ctx.next;
	/* reset the timeout for any containing event loop */
	if (current_context && current_context->timer) {
	  set_timeout(rep_input_timeout_secs * 1000);
	}
	return result;
      }
    }
  }
}

static int
wait_for_input(fd_set *fds, int timeout_msecs)
{
  ++waiting_for_input;

  runloop_context ctx;
  memset(&ctx, 0, sizeof(ctx));
  ctx.next = current_context;
  current_context = &ctx;

  set_timeout(timeout_msecs);
  [NSApp run];
  remove_timeout();

  current_context = ctx.next;

  --waiting_for_input;

  int count = 0;

  if (!ctx.timed_out) {
    for (input_data *d = inputs; d; d = d->next) {
      if (!FD_ISSET(d->fd, fds)) {
	continue;
      }
      if (d->pending != 0) {
	count++;
      } else {
	FD_CLR(d->fd, fds);
      }
    }
  }

  return count;
}

static void
observer_callback(CFRunLoopObserverRef observer,
		   CFRunLoopActivity activity, void *info)
{
  runloop_context *ctx = current_context;

  if (ctx) {
    if (rep_INTERRUPTP) {
      stop_application();
      return;
    }

    if (!ctx->timed_out && needs_redisplay) {
      if (rep_redisplay_fun) {
	(*rep_redisplay_fun)();
      }

      needs_redisplay = false;
    }

    ctx->timed_out = 0;
    ctx->idle_counter = 0;

    if (ctx->timer != NULL) {
      set_timeout(rep_input_timeout_secs * 1000);
    }
  }
}

static void
mac_sigchld_handler(void *info)
{
  if (current_context) {
    stop_application();
  }
}

/* Called by librep/src/unix_processes.c whenever SIGCHLD is received
   (from the signal handler) */

static void
sigchld_callback(void)
{
  signal_pipe(sigchld_pipe[1]);
}

/* Called by the view when it receives events. Makes sure that cancels
   any calls to wait_for_input. */

bool
mac_defer_event(void *view, void *ns_event)
{
  if (waiting_for_input == 0) {
    return false;
  }

  stop_application();

  OBJC_BEGIN {

    [NSApp postEvent:(NSEvent *)ns_event atStart:YES];

  } OBJC_END

  return true;
}

void
mac_set_needs_display(void)
{
  needs_redisplay = true;
}

repv
rep_dl_init(void)
{
  CFRunLoopSourceContext ctx;
  pthread_attr_t attr;
  pthread_t tid;

  runloop_observer = CFRunLoopObserverCreate(NULL, kCFRunLoopBeforeWaiting
    | kCFRunLoopExit, true, 2000000, observer_callback, NULL);
  CFRunLoopAddObserver(CFRunLoopGetCurrent(), runloop_observer,
    kCFRunLoopCommonModes);

  pipe(input_pipe);
  rep_set_fd_nonblocking(input_pipe[0]);
  rep_set_fd_cloexec(input_pipe[0]);
  rep_set_fd_cloexec(input_pipe[1]);

  pipe(sigchld_pipe);
  rep_set_fd_nonblocking(sigchld_pipe[0]);
  rep_set_fd_cloexec(sigchld_pipe[0]);
  rep_set_fd_cloexec(sigchld_pipe[1]);

  memset(&ctx, 0, sizeof(ctx));
  ctx.perform = mac_sigchld_handler;
  sigchld_source = CFRunLoopSourceCreate(NULL, 0, &ctx);
  CFRunLoopAddSource(CFRunLoopGetCurrent(),
		     sigchld_source, kCFRunLoopCommonModes);

  pthread_attr_init(&attr);
  pthread_attr_setscope(&attr, PTHREAD_SCOPE_SYSTEM);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  pthread_create(&tid, &attr, input_thread, NULL);
  pthread_attr_destroy(&attr);

  rep_register_input_fd_fun = register_input_fd;
  rep_deregister_input_fd_fun = deregister_input_fd;
  rep_map_inputs(register_input_fd);
  rep_event_loop_fun = event_loop;
  rep_wait_for_input_fun = wait_for_input;
  rep_sigchld_fun = sigchld_callback;

  repv tem = rep_push_structure("mac.runloop");
  return rep_pop_structure(tem);
}
