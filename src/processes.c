/* processes.c -- subprocess management

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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <signal.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#else
# include <sys/fcntl.h>
#endif

#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

#if HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif
#ifndef WEXITSTATUS
# define WEXITSTATUS(stat_val) ((unsigned)(stat_val) >> 8)
#endif
#ifndef WIFEXITED
# define WIFEXITED(stat_val) (((stat_val) & 255) == 0)
#endif

#ifdef HAVE_SYS_IOCTL_H
# include <sys/ioctl.h>
#endif

#ifdef HAVE_TERMIOS_H
# include <termios.h>
#endif

#ifdef HAVE_DEV_PTMX
# ifdef HAVE_STROPTS_H
#  include <stropts.h>
# endif
#endif

#ifdef ENVIRON_UNDECLARED
extern char **environ;
#endif

void (*rep_sigchld_fun)(void) = 0;

static struct sigaction chld_sigact;
static sigset_t chld_sigset;

typedef struct rep_process_struct rep_process;

struct rep_process_struct {
  repv car; /* status in high bits */

  rep_process *next;
  rep_process *notify_next;

  pid_t	pid;
  int exit_status;

  /* stdin is where we write, stdout where we read, they may be the
     same. stderr is only used with pipes -- it may be a separate
     connection to the stderr stream of the process. At all other times
     it will be equal to stdout. */

  int stdin_fd;
  int stdout_fd;
  int stderr_fd;

  repv output_stream;
  repv error_stream;

  repv notify_function;
  repv program;
  repv args;
  repv directory;
  repv connection_type;
};

static repv process_type(void);

static rep_process *process_list;
static rep_process *notify_list;
static int active_process_count;

#define PROC(v) ((rep_process *)rep_PTR(v))
#define PROCESSP(v) rep_CELL16_TYPEP(v, process_type())

/* Status is two bits above the type code (presently 8->9) */

#define PR_ACTIVE  (1 << (rep_CELL16_TYPE_BITS + 0))	/* active or stopped */
#define PR_STOPPED (2 << (rep_CELL16_TYPE_BITS + 1))	/* stopped */
#define PR_DEAD    0

#define PR_ACTIVE_P(p)  ((p)->car & PR_ACTIVE)
#define PR_STOPPED_P(p) ((p)->car & PR_STOPPED)
#define PR_RUNNING_P(p) (PR_ACTIVE_P(p) && !PR_STOPPED_P(p))
#define PR_DEAD_P(p)    !PR_ACTIVE_P(p)

#define PR_SET_STATUS(p, s) \
    ((p)->car = (((p)->car & ~(PR_ACTIVE | PR_STOPPED)) | (s)))

DEFSYM(pipe, "pipe");
DEFSYM(pty, "pty");
DEFSYM(socketpair, "socketpair");

#define PR_CONN_PTY_P(p) ((p)->connection_type == Qpty)
#define PR_CONN_PIPE_P(p) ((p)->connection_type == Qpipe)
#define PR_CONN_SOCKETPAIR_P(p) ((p)->connection_type == Qsocketpair)

static volatile bool pending_sigchld;

static void read_from_process_fd(rep_process *pr, bool from_stderr);

DEFSTRING(not_running, "Not running");
DEFSTRING(not_stopped, "Not stopped");
DEFSTRING(no_link, "No link to input");
DEFSTRING(in_use, "Process in use");
DEFSTRING(no_pty, "Can't find unused pty");
DEFSTRING(no_prog, "No program");
DEFSTRING(cant_start, "Can't start");
DEFSTRING(dev_null, "/dev/null");
DEFSTRING(dot, ".");
DEFSTRING(not_local, "Need a local file");
DEFSTRING(forkstr, "fork");
DEFSTRING(nosig, "Unknown signal");



static RETSIGTYPE
sigchld_handler(int sig)
{
  pending_sigchld = true;

  if (rep_sigchld_fun) {
    int old_errno = errno;
    (*rep_sigchld_fun)();
    errno = old_errno;
  }
}

static void
close_files(rep_process *pr)
{
  if (pr->stdout_fd) {
    rep_deregister_input_fd(pr->stdout_fd);
    close(pr->stdout_fd);
  }

  if (pr->stderr_fd && pr->stderr_fd != pr->stdout_fd) {
    rep_deregister_input_fd(pr->stderr_fd);
    close(pr->stderr_fd);
  }

  if (pr->stdin_fd && pr->stdin_fd != pr->stdout_fd) {
    close(pr->stdin_fd);
  }

  pr->stdout_fd = pr->stdin_fd = pr->stderr_fd = 0;
}
    
static void
queue_notification(rep_process *pr)
{
  if (pr->notify_next == NULL) {
    pr->notify_next = notify_list;
    notify_list = pr;
  }
}

static bool
dispatch_notifications(void)
{
  if (!notify_list) {
    return false;
  }

  while (notify_list && !rep_INTERRUPTP) {
    rep_process *pr = notify_list;
    notify_list = pr->notify_next;
    pr->notify_next = NULL;
    if (pr->notify_function && pr->notify_function != rep_nil) {
      rep_call_lisp1(pr->notify_function, rep_VAL(pr));
    }
  }

  return true;
}

static bool
notification_queued_p(rep_process *pr)
{
  return pr->notify_next != NULL;
}

static void
notify_process(rep_process *pr)
{
  if (notification_queued_p (pr)) {
    rep_process **ptr = &notify_list;
    while (*ptr != pr) {
      ptr = &((*ptr)->notify_next);
    }
    *ptr = pr->notify_next;
    pr->notify_next = NULL;
    if (pr->notify_function && pr->notify_function != rep_nil) {
      rep_call_lisp1(pr->notify_function, rep_VAL(pr));
    }
  }
}

static bool
handle_process_events(void)
{
  if (!pending_sigchld) {
    return false;
  }

  pending_sigchld = false;

  while (active_process_count > 0) {
    int status;
    pid_t pid = waitpid(-1, &status, WNOHANG | WUNTRACED);
    if (pid == 0) {
      break;
    } else if (pid > 0) {
      for (rep_process *pr = process_list; pr; pr = pr->next) {
	if (!PR_ACTIVE_P(pr) || pr->pid != pid) {
	  continue;
	}
#ifdef WIFSTOPPED
	if (WIFSTOPPED(status)) {
	  PR_SET_STATUS(pr, PR_ACTIVE | PR_STOPPED);
	  queue_notification(pr);
	  break;
	}
#endif
	/* Process died. */

	pr->exit_status = status;
	active_process_count--;
	PR_SET_STATUS(pr, PR_DEAD);

	if (pr->stdout_fd) {
	  read_from_process_fd(pr, false);
	}
	if (pr->stderr_fd && pr->stderr_fd != pr->stdout_fd) {
	  read_from_process_fd(pr, true);
	}

	close_files(pr);
	queue_notification(pr);
	break;
      }
    } else /* if (pid < 0) */ {
      if (errno == EINTR) {
	continue;
      } else {
	break;
      }
    }
  }

  return true;
}

/* Called by the event loop after each event or timeout. Returns true
   if the display should be updated. */

static bool
event_loop_callback(void)
{
  bool ret = handle_process_events();
  if (dispatch_notifications()) {
    ret = true;
  }
  return ret;
}

/* Process output from one file descriptor of PROC. */

static void
read_from_process_fd(rep_process *pr, bool from_stderr)
{
  int fd = from_stderr ? pr->stderr_fd : pr->stdout_fd;
  repv stream = from_stderr ? pr->error_stream : pr->output_stream;

  char buf[4097];
  int actual;

  do {
    actual = read(fd, buf, sizeof(buf) - 1);

    if (actual > 0) {
      buf[actual] = 0;
      if (stream != rep_nil) {
	rep_stream_puts(stream, buf, actual, false);
      }
    }

  } while (actual > 0 || (actual < 0 && errno == EINTR));

  /* On EOF or error, close file descriptor. */

  if (actual == 0 || (actual < 0 && errno != EWOULDBLOCK && errno != EAGAIN)) {
    rep_deregister_input_fd(fd);
    close(fd);

    /* Check all fds, could be reused. */

    if (pr->stdin_fd == fd) {
      pr->stdin_fd = 0;
    }
    if (pr->stdout_fd == fd) {
      pr->stdout_fd = 0;
    }
    if (pr->stderr_fd == fd) {
      pr->stderr_fd = 0;
    }
  }
}

static void
read_from_fd(int fd)
{
  for (rep_process *pr = process_list; pr; pr = pr->next) {
    if (!PR_ACTIVE_P(pr)) {
      continue;
    }
    if (pr->stdout_fd == fd) {
      read_from_process_fd(pr, false);
    }
    if (pr->stderr_fd == fd) {
      read_from_process_fd(pr, true);
    }
  }
}

static intptr_t
write_to_process(repv pr, const char *buf, intptr_t buf_len)
{
  if (!PROCESSP(pr)) {
    return 0;
  }

  if (!PR_ACTIVE_P(PROC(pr))) {
    Fsignal(Qprocess_error, rep_list_2(pr, rep_VAL(&not_running)));
    return 0;
  }

  if (PROC(pr)->stdin_fd == 0) {
    Fsignal(Qprocess_error, rep_list_2(pr, rep_VAL(&no_link)));
    return 0;
  }

  intptr_t total = 0;

  do {
    /* The call to write() will block. */

    intptr_t bytes = write(PROC(pr)->stdin_fd, buf + total, buf_len - total);

    if (bytes < 0) {
      if (errno != EINTR) {
	rep_signal_file_error(pr);
	break;
      }
    } else {
      total += bytes;
    }

  } while (total < buf_len);

  return total;
}

static bool
signal_process(rep_process *pr, int sig, bool signal_group)
{
  bool ret = true;

  if (signal_group) {
    if (pr->stdin_fd && PR_CONN_PTY_P(pr)) {
      pid_t gid = tcgetpgrp(pr->stdin_fd);
      if (gid != -1) {
	kill(-gid, sig);
      } else if (PR_ACTIVE_P(pr)) {
	kill(-pr->pid, sig);
      } else {
	ret = false;
      }
    } else {
      if (PR_ACTIVE_P(pr)) {
	kill(-pr->pid, sig);
      } else {
	ret = false;
      }
    }
  } else {
    if (PR_ACTIVE_P(pr)) {
      kill(pr->pid, sig);
    } else {
      ret = false;
    }
  }

  return ret;
}

/* This is only called during GC, when the process isn't being referenced.
   it will already have been taken out of the chain. Also active processes
   should have been marked anyway. */

static void
delete_process(rep_process *pr)
{
  if (PR_ACTIVE_P(pr)) {
    if (!signal_process(pr, SIGKILL, true)) {
      kill(-pr->pid, SIGKILL);
    }
    waitpid(pr->pid, &pr->exit_status, 0);
    active_process_count--;
    close_files(pr);
  }

  rep_free(pr);
}

/* Return the file descriptor (or 0 if an error) of the first available
   pty master. SLAVENAM will contain the name of the associated slave. */

static int
get_pty(char *slavenam)
{
#if defined(HAVE_PTYS)
# if defined(HAVE_DEV_PTMX) && defined(HAVE_GRANTPT)
  int master = open("/dev/ptmx", O_RDWR);
  if (master >= 0) {
    grantpt(master);
    unlockpt(master);
    const char *tem = ptsname(master);
    if (tem) {
      strcpy(slavenam, tem);
      return master;
    }
    close(master);
  }
# endif

# if defined(FIRST_PTY_LETTER)
  /* Assume /dev/ptyXNN and /dev/ttyXN naming system. The
     FIRST_PTY_LETTER gives the first X to try. We try in the sequence
     FIRST_PTY_LETTER, .., 'z', 'a', .., FIRST_PTY_LETTER. Is this
     worthwhile, or just over-zealous? */

  char c = FIRST_PTY_LETTER;
  do {
    for (int i = 0; i < 16; i++) {
      sprintf(slavenam, "/dev/pty%c%x", c, i);
      struct stat statb;
      if (stat(slavenam, &statb) < 0) {
	goto none;
      }
      int master = open(slavenam, O_RDWR);
      if (master >= 0) {
	slavenam[strlen("/dev/")] = 't';
	if (access(slavenam, R_OK | W_OK) == 0) {
	  return master;
	}
	close(master);
      }
    }
    if (++c > 'z') {
      c = 'a';
    }
  } while (c != FIRST_PTY_LETTER);
none:
# endif /* FIRST_PTY_LETTER */
#endif /* HAVE_PTYS */

  Fsignal(Qprocess_error, rep_LIST_1(rep_VAL(&no_pty)));
  return 0;
}

static void
set_child_environ(void)
{
  repv lst = Fsymbol_value(Qprocess_environment, Qt);
  if (!rep_LISTP(lst)) {
    lst = rep_nil;
  }

  repv len = Flength(lst);
  if (!len || !rep_INTP(len)) {
    return;
  }

  char **array = rep_alloc(sizeof(char *) * (rep_INT(len) + 1));
  if (!array) {
    return;
  }

  char **ptr = array;

  while (rep_CONSP(lst)) {
    *ptr++ = rep_STR(rep_CAR(lst));
    lst = rep_CDR(lst);
  }

  *ptr++ = 0;

  environ = array;
}

/* Returns true when no more output should be read (after error or EOF). */

static bool
copy_sync_output(int fd, repv stream)
{
  char buf[4097];
  int actual;

again:
  actual = read(fd, buf, sizeof(buf) - 1);

  if (actual == 0) {
    return true;
  }

  if (actual < 0) {
    if (errno == EINTR) {
      goto again;
    } else {
      return errno != EAGAIN && errno != EWOULDBLOCK;
    }
  }

  if (stream != rep_nil) {
    buf[actual] = 0;
    rep_stream_puts(stream, buf, actual, false);
  }

  return false;
}

static void
read_synchronous_output(rep_process *pr)
{
  fd_set inputs;
  FD_ZERO(&inputs);
  FD_SET(pr->stdout_fd, &inputs);
  FD_SET(pr->stderr_fd, &inputs);

  fcntl(pr->stdout_fd, F_SETFL, O_NONBLOCK);
  fcntl(pr->stderr_fd, F_SETFL, O_NONBLOCK);

  bool stdout_finished = false;
  bool stderr_finished = false;
  int interrupt_count = 0;

  while (!(stdout_finished && stderr_finished)) {
    fd_set copy = inputs;
    struct timeval timeout;
    timeout.tv_sec = 1;
    timeout.tv_usec = 0;

    rep_sig_restart(SIGCHLD, false);

    int ready = select(FD_SETSIZE, &copy, NULL, NULL, &timeout);

    rep_sig_restart(SIGCHLD, true);

    rep_TEST_INT_SLOW;
    if (rep_INTERRUPTP) {
      static int signals[] = {0, SIGINT, SIGTERM, SIGQUIT};
      if (interrupt_count < 3) {
	interrupt_count++;
      }
      signal_process(pr, signals[interrupt_count], true);
      if (rep_throw_value == rep_int_cell) {
	rep_throw_value = 0;
      }
    }

    if (ready > 0) {
      rep_GC_root gc_pr;
      repv vpr = rep_VAL(pr);
      rep_PUSHGC(gc_pr, vpr);
      if (!stdout_finished && FD_ISSET(pr->stdout_fd, &copy)) {
	if (copy_sync_output(pr->stdout_fd, pr->output_stream)) {
	  stdout_finished = true;
	  FD_CLR(pr->stdout_fd, &inputs);
	}
      }
      if (!stderr_finished && FD_ISSET(pr->stderr_fd, &copy)) {
	if (copy_sync_output(pr->stderr_fd, pr->error_stream)) {
	  stderr_finished = true;
	  FD_CLR(pr->stderr_fd, &inputs);
	}
      }
      rep_POPGC;
    }
  }

  close(pr->stdout_fd);
  close(pr->stderr_fd);
  pr->stdout_fd = 0;
  pr->stderr_fd = 0;

  while (1) {
    int pid = waitpid(pr->pid, &pr->exit_status, 0);
    if (pid >= 0 || errno != EINTR) {
      break;
    }
  }

  PR_SET_STATUS(pr, PR_DEAD);
  queue_notification(pr);
}

/* Does the dirty stuff of getting the process running. if SYNC-INPUT
   is non-NULL it means to run the process synchronously with its stdin
   connected to the file SYNC-INPUT. Otherwise this function returns
   immediately after starting the process. */

static bool
run_process(rep_process *pr, char **argv, const char *sync_input)
{
  if (!PR_DEAD_P(pr)) {
    DEFSTRING(already_running, "Already running");
    Fsignal(Qprocess_error,
	    rep_list_2(rep_VAL(pr), rep_VAL(&already_running)));
    return false;
  }

  pr->exit_status = -1;

  bool use_pty = PR_CONN_PTY_P(pr);

  char slavenam[32];
  int stdin_fds[2], stdout_fds[2], stderr_fds[2];

  if (sync_input || !use_pty) {
    use_pty = false;
    pr->connection_type = Qpipe;
    if (pipe(stdout_fds) == 0) {
      if (pipe(stderr_fds) == 0) {
	if (sync_input) {
	  stdin_fds[0] = open(sync_input, O_RDONLY);
	  if (stdin_fds[0] >= 0) {
	    pr->stdin_fd = stdin_fds[0]; /* fake */
	  }
	} else {
	  if (pipe(stdin_fds) == 0) {
	    pr->stdin_fd = stdin_fds[1];
	  }
	}
	if (pr->stdin_fd != 0) {
	  pr->stdout_fd = stdout_fds[0];
	  pr->stderr_fd = stderr_fds[0];
	} else {
	  close(stderr_fds[0]);
	  close(stderr_fds[1]);
	}
      } else {
	close(stdout_fds[0]);
	close(stdout_fds[1]);
      }
    }
  } else if (PR_CONN_SOCKETPAIR_P(pr)) {
    /* FIXME: separate stdout from stderr? */
    if (socketpair(AF_UNIX, SOCK_STREAM, 0, stdin_fds) == 0) {
      pr->stdin_fd = stdin_fds[0];
      pr->stdout_fd = stdin_fds[0];
      pr->stderr_fd = stdin_fds[0];
    }
  } else if (use_pty) {
    pr->stdin_fd = get_pty(slavenam);
    pr->stdout_fd = pr->stdin_fd;
    pr->stderr_fd = pr->stdin_fd;
  }

  if (pr->stdin_fd == 0) {
    if (!rep_throw_value) {
      Fsignal(Qprocess_error, rep_LIST_1(rep_lookup_errno()));
    }
    return false;
  }

  int pty_slave_fd = -1;

  /* Must set up pty slave before forking, to avoid race condition if
     master writes to it first */

  if (use_pty) {
    struct termios st;
    pty_slave_fd = open(slavenam, O_RDWR);
    if (pty_slave_fd >= 0) {
#ifdef HAVE_DEV_PTMX
# ifdef I_PUSH
      /* Push modules onto the slave to get terminal semantics. */
      ioctl(pty_slave_fd, I_PUSH, "ptem");
      ioctl(pty_slave_fd, I_PUSH, "ldterm");
# endif
#endif
#ifdef TIOCSCTTY
      ioctl(pty_slave_fd, TIOCSCTTY, 0);
#endif
      tcgetattr(pty_slave_fd, &st);
      st.c_iflag &= ~(ISTRIP | IGNCR | INLCR | IXOFF);
      st.c_iflag |= (ICRNL | IGNPAR | BRKINT | IXON);
      st.c_oflag &= ~OPOST;
      st.c_cflag &= ~CSIZE;
      st.c_cflag |= CREAD | CS8 | CLOCAL;
      st.c_lflag &= ~(ECHO | ECHOE | ECHOK | NOFLSH | TOSTOP);
      st.c_lflag |= ISIG;
#if 0
      st.c_cc[VMIN] = 1;
      st.c_cc[VTIME] = 0;
#endif
      /* Set some control codes to default values */
      st.c_cc[VINTR]  = '\003';	/* ^c */
      st.c_cc[VQUIT]  = '\034';	/* ^| */
      st.c_cc[VERASE] = '\177';	/* ^? */
      st.c_cc[VKILL]  = '\025';	/* ^u */
      st.c_cc[VEOF]   = '\004';	/* ^d */
      tcsetattr(pty_slave_fd, TCSANOW, &st);
    }
  }

  pr->pid = fork();

  switch (pr->pid) {
  case 0:				/* child */
    set_child_environ ();
    if (use_pty) {
      if (setsid() < 0) {
	perror("child: setsid()");
	_exit(255);
      }
      if (pty_slave_fd < 0) {
	perror("child: open(slave)");
	_exit(255);
      }
      close(pr->stdin_fd);
      dup2(pty_slave_fd, 0);
      dup2(pty_slave_fd, 1);
      dup2(pty_slave_fd, 2);
      if (pty_slave_fd > 2) {
	close(pty_slave_fd);
	pty_slave_fd = -1;
      }
    } else if (PR_CONN_SOCKETPAIR_P(pr)) {
      if (setpgid(0, 0) != 0) {
	perror("setpgid");
	_exit(255);
      }
      close(stdin_fds[0]);
      dup2(stdin_fds[1], 0);
      dup2(stdin_fds[1], 1);
      dup2(stdin_fds[1], 2);
      close(stdin_fds[1]);
    } else /* if pipe */ {
      if (setpgid(0, 0) != 0) {
	perror("setpgid");
	_exit(255);
      }
      dup2(stdin_fds[0], 0);
      close(stdin_fds[0]);
      if (!sync_input) {
	close(stdin_fds[1]);
      }
      dup2(stdout_fds[1], 1);
      dup2(stderr_fds[1], 2);
      close(stdout_fds[0]);
      close(stdout_fds[1]);
      close(stderr_fds[0]);
      close(stderr_fds[1]);
    }
    if (rep_STRINGP(pr->directory)) {
      if (rep_STRING_LEN(pr->directory) > 0) {
	chdir(rep_STR(pr->directory));
      }
    }
    signal(SIGPIPE, SIG_DFL);
    execvp(argv[0], argv);
    perror("child subprocess can't exec");
    _exit(255);
    /* not reached */

  case -1:				/* error forking */
    if (pty_slave_fd != -1) {
      close (pty_slave_fd);
    }
    if (PR_CONN_SOCKETPAIR_P(pr)) {
      close (stdin_fds[0]);
      close (stdin_fds[1]);
    }
    if (sync_input || !use_pty) {
      close(stdout_fds[0]);
      close(stdout_fds[1]);
      close(stderr_fds[0]);
      close(stderr_fds[1]);
      close(stdin_fds[0]);
      if (sync_input) {
	close(stdin_fds[1]);
      }
    } else {
      close(pr->stdin_fd);
    }
    pr->stdin_fd = pr->stdout_fd = pr->stderr_fd = 0;
    rep_signal_file_error(rep_VAL(&forkstr));
    return false;

  default:				/* parent */
    break;
  }

  if (pty_slave_fd != -1) {
    close (pty_slave_fd);
  }

  PR_SET_STATUS(pr, PR_ACTIVE);

  active_process_count++;

  if (PR_CONN_SOCKETPAIR_P(pr)) {
    close(stdin_fds[1]);
  } else if (!use_pty) {
    close(stdin_fds[0]);
    close(stdout_fds[1]);
    close(stderr_fds[1]);
  }

  rep_set_fd_cloexec(pr->stdin_fd);

  if (sync_input) {
    pr->stdin_fd = 0;
    read_synchronous_output(pr);
    return true;
  }

  if (pr->stdin_fd == pr->stdout_fd) {
    /* So stdout can be non-blocking, copy it.  */
    if ((pr->stdin_fd = dup(pr->stdout_fd)) < 0) {
      perror("dup(pr->stdout_fd)");
      pr->stdin_fd = pr->stdout_fd;
    }
  }

  rep_set_fd_nonblocking(pr->stdout_fd);
  rep_register_input_fd(pr->stdout_fd, read_from_fd);

  if (pr->stderr_fd != pr->stdout_fd) {
    rep_set_fd_nonblocking(pr->stderr_fd);
    rep_register_input_fd(pr->stderr_fd, read_from_fd);
  }

  return true;
}

static void
process_mark(repv pr)
{
  rep_MARKVAL(PROC(pr)->output_stream);
  rep_MARKVAL(PROC(pr)->error_stream);
  rep_MARKVAL(PROC(pr)->notify_function);
  rep_MARKVAL(PROC(pr)->program);
  rep_MARKVAL(PROC(pr)->args);
  rep_MARKVAL(PROC(pr)->directory);
  rep_MARKVAL(PROC(pr)->connection_type);
}

static void
process_mark_active(void)
{
  for (rep_process *pr = process_list; pr; pr = pr->next) {
    if (PR_ACTIVE_P(pr)) {
      rep_MARKVAL(rep_VAL(pr));
    }
  }
}

static void
process_sweep(void)
{
  rep_process *pr;

  pr = notify_list;
  notify_list = NULL;

  while (pr) {
    if (rep_GC_CELL_MARKEDP(rep_VAL(pr))) {
      pr->notify_next = notify_list;
      notify_list = pr;
    }
    pr = pr->notify_next;
  }

  pr = process_list;
  process_list = NULL;

  while (pr) {
    rep_process *next = pr->next;
    if (!rep_GC_CELL_MARKEDP(rep_VAL(pr))) {
      delete_process(pr);
    } else {
      rep_GC_CLR_CELL(rep_VAL(pr));
      pr->next = process_list;
      process_list = pr;
    }
    pr = next;
  }
}

static void
process_print(repv strm, repv obj)
{
  rep_process *pr = PROC(obj);
  rep_stream_puts(strm, "#<process", -1, false);
  if (PR_RUNNING_P(pr)) {
    rep_stream_puts(strm, " running: ", -1, false);
    rep_stream_puts(strm, rep_PTR(pr->program), -1, true);
  } else if (PR_STOPPED_P(pr)) {
    rep_stream_puts(strm, " stopped: ", -1, false);
    rep_stream_puts(strm, rep_PTR(pr->program), -1, true);
  } else {
    if (pr->exit_status != -1) {
      char buf[40];
#ifdef HAVE_SNPRINTF
      snprintf(buf, sizeof(buf), " exited: 0x%x", pr->exit_status);
#else
      sprintf(buf, " exited: 0x%x", pr->exit_status);
#endif
      rep_stream_puts(strm, buf, -1, false);
    }
  }
  rep_stream_putc(strm, '>');
}

static int
process_putc(repv stream, int c)
{
  char tmps[2];
  tmps[0] = (char)c;
  tmps[1] = 0;
  return write_to_process(stream, tmps, 1);
}

static intptr_t
process_puts(repv stream, const void *data, intptr_t len, bool lisp_string)
{
  const char *buf = lisp_string ? rep_STR(data) : data;
  return write_to_process(stream, buf, len);
}

DEFUN("make-process", Fmake_process, Smake_process, (repv stream, repv fun, repv dir, repv prog, repv args), rep_Subr5) /*
::doc:rep.io.processes#make-process::
make-process [OUTPUT-STREAM] [FUN] [DIR] [PROGRAM] [ARGS]

Creates a new process-object, OUTPUT-STREAM is where all output from
this process goes, both stdout and stderr, FUN is a function to call
each time the process running on this object changes state. DIR is the
process' current directory, PROGRAM the filename of the program to run
and ARGS a list of arguments passed to the process.

Any of the arguments may be unspecified, in which case they can be set
either by the functions provided or by the function called to create
the actual running process.

If the DIR parameter is nil it will be inherited from the
`*default-directory*' variable of the current buffer.
::end:: */
{
  rep_process *pr = rep_alloc(sizeof(rep_process));
  rep_data_after_gc += sizeof (rep_process);

  pr->car = process_type();
  pr->next = process_list;
  process_list = pr;
  pr->notify_next = NULL;
  PR_SET_STATUS(pr, PR_DEAD);
  pr->pid = 0;
  pr->stdin_fd = pr->stdout_fd = 0;
  pr->exit_status = -1;
  pr->output_stream = stream;
  pr->error_stream = stream;
  pr->notify_function = fun;
  pr->program = prog;
  pr->args = args;
  pr->connection_type = Qpipe;
  pr->directory = dir;

  /* Ensure that directory refers to an absolute local file */

  rep_GC_root gc_pr;
  repv tem_pr = rep_VAL(pr);
  rep_PUSHGC(gc_pr, tem_pr);

  dir = Flocal_file_name(rep_STRINGP(dir) ? dir : rep_VAL(&dot));

  rep_POPGC;

  if (dir && rep_STRINGP(dir)) {
    pr->directory = dir;
  } else {
    pr->directory = rep_nil;
  }

  return rep_VAL(pr);
}

DEFUN("close-process", Fclose_process,
      Sclose_process, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#close-process::
close-processes [PROCESS]

Closes the stdin, stdout, and stderr streams of the asynchronous
process-object PROCESS.
::end:: */
{
  rep_DECLARE1(proc, PROCESSP);

  close_files(PROC(proc));

  return rep_nil; 
}

DEFUN("start-process", Fstart_process,
      Sstart_process, (repv arg_list), rep_SubrN) /*
::doc:rep.io.processes#start-process::
start-process [PROCESS] [PROGRAM] [ARGS...]

Starts a process running on process-object PROCESS. The child-process
runs asynchronously with the editor. If PROCESS is unspecified the
make-process function will be called (with zero arguments) to create
one.

PROGRAM is the filename of the binary image, it will be searched for in
all directories listed in the `PATH' environment variable. ARGS are the
arguments to give to the process.

If any of the optional parameters are unspecified they should have been
set in the PROCESS prior to calling this function.
::end:: */
{
  rep_process *pr = NULL;

  if (rep_CONSP(arg_list)) {
    if (PROCESSP(rep_CAR(arg_list))) {
      pr = PROC(rep_CAR(arg_list));
    }
    arg_list = rep_CDR(arg_list);
  }

  if (!pr) {
    pr = PROC(Fmake_process(rep_nil, rep_nil, rep_nil, rep_nil, rep_nil));
    if (!pr) {
      return 0;
    }
  }
  
  if (rep_CONSP(arg_list)) {
    if (rep_STRINGP(rep_CAR(arg_list))) {
      pr->program = rep_CAR(arg_list);
    }
    arg_list = rep_CDR(arg_list);
    if (rep_CONSP(arg_list)) {
      pr->args = arg_list;
    }
  }

  if (!rep_STRINGP(pr->program)) {
    return Fsignal(Qprocess_error, rep_list_2(rep_VAL(&no_prog), rep_VAL(pr)));
  }

  int argc = rep_list_length(pr->args) + 1;
  if (argc < 0) {
    return 0;
  }

  char **argv = rep_alloc(sizeof(char *) * (argc + 1));
  if (!argv) {
    return rep_mem_error();
  }

  arg_list = pr->args;
  argv[0] = rep_STR(pr->program);

  for (int i = 1; i < argc; i++) {
    if (rep_STRINGP(rep_CAR(arg_list))) {
      argv[i] = rep_STR(rep_CAR(arg_list));
    } else {
      argv[i] = "";
    }
    arg_list = rep_CDR(arg_list);
  }

  argv[argc] = NULL;

  repv ret;
  if (run_process(pr, argv, NULL)) {
    ret = Qt;
  } else {
    ret = Fsignal(Qprocess_error,
		  rep_list_2(rep_VAL(&cant_start), rep_VAL(pr)));
  }

  rep_free(argv);
  return ret;
}

DEFUN("call-process", Fcall_process,
      Scall_process, (repv arg_list), rep_SubrN) /*
::doc:rep.io.processes#call-process::
call-process [PROCESS] [IN-FILE] [PROGRAM] [ARGS...]

Starts a process running on process-object PROCESS. Waits for the child
to exit, then returns the exit-value of the child. If PROCESS is
unspecified the make-process function will be called (with zero
arguments) to create one.

IN-FILE is the name of the file to connect to the process' standard
input, if this is not defined `/dev/null' is used. PROGRAM is the
filename of the binary image, it will be searched for in all
directories listed in the `PATH' environment variable. ARGS are the
arguments to give to the process.

If any of the optional parameters are unspecified they should have been
set in the PROCESS prior to calling this function.
::end:: */
{
  rep_process *pr = NULL;

  repv input_file = rep_VAL(&dev_null);

  if (rep_CONSP(arg_list)) {
    if (PROCESSP(rep_CAR(arg_list))) {
      pr = PROC(rep_CAR(arg_list));
    }
    arg_list = rep_CDR(arg_list);
  }

  if (!pr) {
    pr = PROC(Fmake_process(rep_nil, rep_nil, rep_nil, rep_nil, rep_nil));
    if (!pr) {
      return 0;
    }
  }

  if (rep_CONSP(arg_list)) {
    if (rep_STRINGP(rep_CAR(arg_list))) {
      input_file = rep_CAR(arg_list);
    }
    arg_list = rep_CDR(arg_list);
    if (rep_CONSP(arg_list)) {
      if (rep_STRINGP(rep_CAR(arg_list))) {
	pr->program = rep_CAR(arg_list);
      }
      arg_list = rep_CDR(arg_list);
      if (rep_CONSP(arg_list)) {
	pr->args = arg_list;
      }
    }
  }

  if (input_file != rep_VAL(&dev_null)) {
    /* Ensure that INPUT-FILE is a real name in the local file system,
       and that the file actually exists. */

    rep_GC_root gc_arg_list, gc_pr, gc_input_file;
    repv tem_pr = rep_VAL(pr);
    rep_PUSHGC(gc_arg_list, arg_list);
    rep_PUSHGC(gc_pr, tem_pr);
    rep_PUSHGC(gc_input_file, input_file);

    input_file = Flocal_file_name(input_file);

    bool failed = false;		/* can't return error immediately */

    if (!input_file || !rep_STRINGP(input_file)) {
      Fsignal(Qprocess_error, rep_LIST_2(rep_VAL(&not_local), rep_VAL(pr)));
      failed = true;
    } else if (rep_file_exists_p(input_file) == rep_nil) {
      rep_signal_file_error(input_file);
      failed = true;
    }

    rep_POPGC; rep_POPGC; rep_POPGC;

    if (failed) {
      return 0;
    }
  }

  if (!rep_STRINGP(pr->program)) {
    return Fsignal(Qprocess_error, rep_LIST_2(rep_VAL(&no_prog), rep_VAL(pr)));
  }

  int argc = rep_list_length(pr->args) + 1;
  if (argc < 0) {
    return 0;
  }

  char **argv = rep_alloc(sizeof(char *) * (argc + 1));
  if (!argv) {
    return rep_mem_error();
  }

  arg_list = pr->args;
  argv[0] = rep_STR(pr->program);

  for (int i = 1; i < argc; i++) {
    if (rep_STRINGP(rep_CAR(arg_list))) {
      argv[i] = rep_STR(rep_CAR(arg_list));
    } else {
      argv[i] = "";
    }
    arg_list = rep_CDR(arg_list);
  }

  argv[argc] = NULL;

  repv ret;
  if (run_process(pr, argv, rep_STR(input_file))) {
    ret = rep_MAKE_INT(pr->exit_status);
  } else {
    ret = Fsignal(Qprocess_error,
		  rep_list_2(rep_VAL(&cant_start), rep_VAL(pr)));
  }

  rep_free(argv);
  return ret;
}

/* If PROC is running asynchronously then send signal number SIGNAL
   to it. If SIGNAL-GROUP is non-nil send the signal to all processes
   in the process group of PROC. Returns t if successful. */

static repv
do_signal_command(repv proc, int signal, repv signal_group)
{
  rep_DECLARE1(proc, PROCESSP);

  if (!PR_ACTIVE_P(PROC(proc))) {
    return Fsignal(Qprocess_error, rep_list_2(proc, rep_VAL(&not_running)));
  }

  return signal_process(PROC(proc), signal,
			signal_group != rep_nil) ? Qt : rep_nil;
}

DEFUN("interrupt-process", Finterrupt_process,
      Sinterrupt_process, (repv proc, repv grp), rep_Subr2) /*
::doc:rep.io.processes#interrupt-process::
interrupt-process PROCESS [SIGNAL-GROUP]

Interrupt the asynchronous process PROCESS. If SIGNAL-GROUP is t,
interrupt all child processes of PROCESS (it's process group).
::end:: */
{
  return do_signal_command(proc, SIGINT, grp);
}

DEFUN("kill-process", Fkill_process,
      Skill_process, (repv proc, repv grp), rep_Subr2) /*
::doc:rep.io.processes#kill-process::
kill-process PROCESS [SIGNAL-GROUP]

Kill the asynchronous process PROCESS. If SIGNAL-GROUP is t, kill all
child processes of PROCESS (it's process group).
::end:: */
{
  return do_signal_command(proc, SIGKILL, grp);
}

DEFUN("stop-process", Fstop_process,
      Sstop_process, (repv proc, repv grp), rep_Subr2) /*
::doc:rep.io.processes#stop-process::
stop-process PROCESS [SIGNAL-GROUP]

Suspends execution of PROCESS, see `continue-process'. If SIGNAL-GROUP
is non-nil also suspends the processes in the process group of PROCESS.
::end:: */
{
  return do_signal_command(proc, SIGSTOP, grp);
}

DEFUN("continue-process", Fcontinue_process,
      Scontinue_process, (repv proc, repv signal_group), rep_Subr2) /*
::doc:rep.io.processes#continue-process::
continue-process PROCESS [SIGNAL-GROUP]

Restarts PROCESS after it has been stopped (via `stop-process'). If
SIGNAL-GROUP is non-nil also continues the processes in the process
group of PROCESS.
::end:: */
{
  rep_DECLARE1(proc, PROCESSP);

  if (!PR_STOPPED_P(PROC(proc))) {
    return Fsignal(Qprocess_error, rep_list_2(proc, rep_VAL(&not_stopped)));
  }

  if (!signal_process(PROC(proc), SIGCONT, signal_group != rep_nil)) {
    return rep_nil;
  }

  PR_SET_STATUS(PROC(proc), PR_ACTIVE);
  queue_notification(PROC(proc));
  return Qt;
}

DEFUN("signal-process", Fsignal_process, Ssignal_process,
      (repv proc, repv sig, repv signal_group), rep_Subr3) /*
::doc:rep.io.processes#signal_process::
signal-process PROCESS SIGNAL [SIGNAL-GROUP]

Sends the signal SIGNAL to the process PROCESS. If SIGNAL-GROUP is
non-nil also continues the processes in the process group of PROCESS.

PROCESS may be either a Lisp process object, or an integer giving the
process-id of a process (not necessarily started by rep).

SIGNAL may either be a numeric signal, or a symbol naming a signal,
i.e. the symbol `INT' for the UNIX SIGINT signal.
::end:: */
{
  static const struct {
    const char *name;
    int sig;
  } signals[] = {
#ifdef SIGFPE
    {"FPE", SIGFPE},
#endif
#ifdef SIGILL
    {"ILL", SIGILL},
#endif
#ifdef SIGSEGV
    {"SEGV", SIGSEGV},
#endif
#ifdef SIGBUS
    {"BUS", SIGBUS},
#endif
#ifdef SIGABRT
    {"ABRT", SIGABRT},
#endif
#ifdef SIGIOT
    {"IOT", SIGIOT},
#endif
#ifdef SIGTRAP
    {"TRAP", SIGTRAP},
#endif
#ifdef SIGEMT
    {"EMT", SIGEMT},
#endif
#ifdef SIGSYS
    {"SYS", SIGSYS},
#endif
#ifdef SIGTERM
    {"TERM", SIGTERM},
#endif
#ifdef SIGINT
    {"INT", SIGINT},
#endif
#ifdef SIGQUIT
    {"QUIT", SIGQUIT},
#endif
#ifdef SIGKILL
    {"KILL", SIGKILL},
#endif
#ifdef SIGHUP
    {"HUP", SIGHUP},
#endif
#ifdef SIGALRM
    {"ALRM", SIGALRM},
#endif
#ifdef SIGVTALRM
    {"VTALRM", SIGVTALRM},
#endif
#ifdef SIGPROF
    {"PROF", SIGPROF},
#endif
#ifdef SIGIO
    {"IO", SIGIO},
#endif
#ifdef SIGURG
    {"URG", SIGURG},
#endif
#ifdef SIGPOLL
    {"POLL", SIGPOLL},
#endif
#ifdef SIGCHLD
    {"CHLD", SIGCHLD},
    {"CLD", SIGCHLD},
#endif
#ifdef SIGCONT
    {"CONT", SIGCONT},
#endif
#ifdef SIGSTOP
    {"STOP", SIGSTOP},
#endif
#ifdef SIGTSTP
    {"TSTP", SIGTSTP},
#endif
#ifdef SIGTTIN
    {"TTIN", SIGTTIN},
#endif
#ifdef SIGTTOU
    {"TTOU", SIGTTOU},
#endif
#ifdef SIGPIPE
    {"PIPE", SIGPIPE},
#endif
#ifdef SIGLOST
    {"LOST", SIGLOST},
#endif
#ifdef SIGXCPU
    {"XCPU", SIGXCPU},
#endif
#ifdef SIGXFSZ
    {"XFSZ", SIGXFSZ},
#endif
#ifdef SIGUSR1
    {"USR1", SIGUSR1},
#endif
#ifdef SIGUSR2
    {"USR2", SIGUSR2},
#endif
#ifdef SIGWINCH
    {"WINCH", SIGWINCH},
#endif
#ifdef SIGINFO
    {"INFO", SIGINFO},
#endif
    {0}
  };

  int signal = -1;

  rep_DECLARE(1, proc, PROCESSP(proc) || rep_INTP(proc));
  rep_DECLARE(2, sig, rep_INTP(sig) || rep_SYMBOLP(sig));

  if (rep_INTP(sig)) {
    signal = rep_INT(sig);
  } else {
    char *s = rep_STR(rep_SYM(sig)->name);
    for (int i = 0; signals[i].name != 0; i++) {
      if (strcmp(s, signals[i].name) == 0) {
	signal = signals[i].sig;
	break;
      }
    }
    if (signal == -1) {
      return Fsignal(Qerror, rep_list_2(rep_VAL(&nosig), sig));
    }
  }

  if (rep_INTP(proc) && rep_INT(proc) > 0) {
    rep_process *pr = process_list;
    while (pr != 0 && pr->pid != rep_INT(proc)) {
      pr = pr->next;
    }
    if (pr) {
      proc = rep_VAL(pr);
    }
  }

  if (PROCESSP(proc)) {
    return do_signal_command (proc, signal, signal_group);
  }

  int r;
  if (signal_group != rep_nil) {
    r = kill (- rep_INT(proc), signal);
  } else {
    r = kill (rep_INT(proc), signal);
  }
  return r == 0 ? Qt : rep_nil;
}

DEFUN("process-exit-status", Fprocess_exit_status,
      Sprocess_exit_status, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-exit-status::
process-exit-status PROCESS

Returns the unprocessed exit-status of the last process to be run on
the process-object PROCESS. If PROCESS is currently running, return
nil.
::end:: */
{
  rep_DECLARE1(proc, PROCESSP);

  if (PR_DEAD_P(PROC(proc))) {
    if (PROC(proc)->exit_status != -1) {
      return rep_MAKE_INT(PROC(proc)->exit_status);
    }
  }

  return rep_nil;
}

DEFUN("process-exit-value", Fprocess_exit_value,
      Sprocess_exit_value, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-exit-value::
process-exit-value PROCESS

Returns the return-value of the last process to be run on PROCESS, or
nil if:
  a) no process has run on PROCESS
  b) PROCESS is still running
  c) PROCESS exited abnormally
::end:: */
{
  rep_DECLARE1(proc, PROCESSP);

  if ((PR_DEAD_P(PROC(proc))) && (PROC(proc)->exit_status != -1)) {
    return rep_MAKE_INT(WEXITSTATUS(PROC(proc)->exit_status));
  }

  return rep_nil;
}

DEFUN("process-id", Fprocess_id, Sprocess_id, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-id::
process-id [PROCESS]

If PROCESS is running or stopped, return the process-identifier
associated with it (ie, its pid).

If PROCESS is nil, return the process id of the Lisp interpreter.
::end:: */
{
  if (proc == rep_nil) {
    return rep_MAKE_INT(getpid());
  } else {
    rep_DECLARE1(proc, PROCESSP);
    if (PR_ACTIVE_P(PROC(proc))) {
      return rep_MAKE_INT(PROC(proc)->pid);
    }
  }

  return rep_nil;
}

DEFUN("process-running?", Fprocess_running_p,
      Sprocess_running_p, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-running?::
process-running? PROCESS

Return t if PROCESS is running.
::end:: */
{
  rep_DECLARE1(proc, PROCESSP);

  return PR_RUNNING_P(PROC(proc)) ? Qt : rep_nil;
}

DEFUN("process-stopped?", Fprocess_stopped_p, Sprocess_stopped_p, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-stopped?::
process-stopped? PROCESS

Return t if PROCESS has been stopped.
::end:: */
{
  rep_DECLARE1(proc, PROCESSP);

  return PR_STOPPED_P(PROC(proc)) ? Qt : rep_nil;
}

DEFUN("process-in-use?", Fprocess_in_use_p, Sprocess_in_use_p, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-in-use?::
process-in-use? PROCESS

Similar to `process-running?' except that this returns t even when the
process has stopped.
::end:: */
{
  rep_DECLARE1(proc, PROCESSP);

  return PR_ACTIVE_P(PROC(proc)) ? Qt : rep_nil;
}

DEFUN("process?", Fprocessp, Sprocessp, (repv arg), rep_Subr1) /*
::doc:rep.io.processes#process?::
process? ARG

Return t is ARG is a process-object.
::end:: */
{
  return PROCESSP(arg) ? Qt : rep_nil;
}

DEFUN("process-prog", Fprocess_prog, Sprocess_prog, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-prog::
process-prog PROCESS

Return the name of the program in PROCESS.
::end:: */
{
  rep_DECLARE1(proc, PROCESSP);

  return PROC(proc)->program;
}

DEFUN("set-process-prog", Fset_process_prog, Sset_process_prog, (repv proc, repv prog), rep_Subr2) /*
::doc:rep.io.processes#set-process-prog::
set-process-prog PROCESS PROGRAM

Sets the name of the program to run on PROCESS to FILE.
::end:: */
{
  rep_DECLARE1(proc, PROCESSP);
  rep_DECLARE2(prog, rep_STRINGP);

  PROC(proc)->program = prog;
  return prog;
}

DEFUN("process-args", Fprocess_args, Sprocess_args, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-args::
process-args PROCESS

Return the list of arguments to PROCESS.
::end:: */
{
  rep_DECLARE1(proc, PROCESSP);

  return PROC(proc)->args;
}

DEFUN("set-process-args", Fset_process_args,
      Sset_process_args, (repv proc, repv args), rep_Subr2) /*
::doc:rep.io.processes#set-process-args::
set-process-args PROCESS ARG-LIST

Set the arguments to PROCESS.
::end:: */
{
  rep_DECLARE1(proc, PROCESSP);
  rep_DECLARE2(args, rep_LISTP);

  PROC(proc)->args = args;
  return args;
}

DEFUN("process-output-stream", Fprocess_output_stream,
      Sprocess_output_stream, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-output-stream::
process-output-stream PROCESS

Return the stream to which all output from PROCESS is sent.
::end:: */
{
  rep_DECLARE1(proc, PROCESSP);

  return PROC(proc)->output_stream;
}

DEFUN("set-process-output-stream", Fset_process_output_stream,
      Sset_process_output_stream, (repv proc, repv stream), rep_Subr2) /*
::doc:rep.io.processes#set-process-output-stream::
set-process-output-stream PROCESS STREAM

Set the output-stream of PROCESS to STREAM. nil means discard all
output.
::end:: */
{
  rep_DECLARE1(proc, PROCESSP);

  PROC(proc)->output_stream = stream;
  return stream;
}

DEFUN("process-error-stream", Fprocess_error_stream,
      Sprocess_error_stream, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-error-stream::
process-error-stream PROCESS

Return the stream to which all *standard-error* output from PROCESS is
sent.
::end:: */
{
  rep_DECLARE1(proc, PROCESSP);

  return PROC(proc)->error_stream;
}

DEFUN("set-process-error-stream", Fset_process_error_stream,
      Sset_process_error_stream, (repv proc, repv stream), rep_Subr2) /*
::doc:rep.io.processes#set-process-error-stream::
set-process-error-stream PROCESS STREAM

Set the error-stream of PROCESS to STREAM. nil means discard all
output.

Note that this currently only works correctly with pipe connections.
::end:: */
{
  rep_DECLARE1(proc, PROCESSP);

  PROC(proc)->error_stream = stream;
  return stream;
}

DEFUN("process-function", Fprocess_function,
      Sprocess_function, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-function::
process-function PROCESS

Return the function which is called when PROCESS changes state (i.e. it
exits or is stopped).
::end:: */
{
  rep_DECLARE1(proc, PROCESSP);

  return PROC(proc)->notify_function;
}

DEFUN("set-process-function", Fset_process_function,
      Sset_process_function, (repv proc, repv fn), rep_Subr2) /*
::doc:rep.io.processes#set-process-function::
set-process-function PROCESS FUNCTION

Set the function which is called when PROCESS changes state to
FUNCTION.
::end:: */
{
  rep_DECLARE1(proc, PROCESSP);

  PROC(proc)->notify_function = fn;
  return fn;
}

DEFUN("process-dir", Fprocess_dir, Sprocess_dir, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-dir::
process-dir PROCESS

Return the name of the directory which becomes the working directory of
PROCESS when it is started.
::end:: */
{
  rep_DECLARE1(proc, PROCESSP);

  return PROC(proc)->directory;
}

DEFUN("set-process-dir", Fset_process_dir,
      Sset_process_dir, (repv proc, repv dir), rep_Subr2) /*
::doc:rep.io.processes#set-process-dir::
set-process-dir PROCESS DIR

Set the directory of PROCESS to DIR.
::end:: */
{
  rep_DECLARE1(proc, PROCESSP);
  rep_DECLARE2(dir, rep_STRINGP);

  /* Ensure that directory refers to an absolute local file */

  rep_GC_root gc_proc;
  rep_PUSHGC(gc_proc, proc);

  dir = Flocal_file_name(rep_STRINGP(dir) ? dir : rep_VAL(&dot));

  rep_POPGC;

  if (dir && rep_STRINGP(dir)) {
    PROC(proc)->directory = dir;
  } else {
    PROC(proc)->directory = rep_nil;
  }

  return PROC(proc)->directory;;
}

DEFUN("process-connection-type", Fprocess_connection_type,
      Sprocess_connection_type, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-connection-type::
process-connection-type PROCESS

Returns a symbol defining the type of stream (i.e. pipe, pty, or
socketpair) used to connect PROCESS with its physical process.
::end:: */
{
  rep_DECLARE1(proc, PROCESSP);

  return PROC(proc)->connection_type;
}

DEFUN("set-process-connection-type", Fset_process_connection_type,
      Sset_process_connection_type, (repv proc, repv type), rep_Subr2) /*
::doc:rep.io.processes#set-process-connection-type::
set-process-connection-type PROCESS TYPE

Define how PROCESS communicates with it's child process, TYPE may be
one of the following symbols:

  pty		Use a pty
  pipe		Three pipes are used
  socketpair	Use a socketpair

This function can only be used when PROCESS is not in use.

Note that only the `pipe' connection type allows process output and
process error output to be differentiated.
::end:: */
{
  rep_DECLARE1(proc, PROCESSP);

  if (PR_ACTIVE_P(PROC(proc))) {
    return Fsignal(Qprocess_error, rep_list_2(rep_VAL(&in_use), proc));
  }

  PROC(proc)->connection_type = type;
  return type;
}

DEFUN("active-processes", Factive_processes,
      Sactive_processes, (void), rep_Subr0) /*
::doc:rep.io.processes#active-processes::
active-processes

Return a list containing all active process objects.
::end:: */
{
  repv head = rep_nil;
  repv *ptr = &head;

  for (rep_process *pr = process_list; pr; pr = pr->next) {
    if (PR_ACTIVE_P(pr)) {
      *ptr = Fcons(rep_VAL(pr), rep_nil);
      ptr = rep_CDRLOC(*ptr);
    }
  }

  return head;
}

#define MAX_HANDLERS 16
static void (*input_handlers[MAX_HANDLERS])(int);
static int n_input_handlers = 0;

void
rep_register_process_input_handler(void (*handler)(int))
{
  assert(n_input_handlers < MAX_HANDLERS);

  input_handlers[n_input_handlers++] = handler;
}

DEFUN("accept-process-output", Faccept_process_output,
      Saccept_process_output, (repv secs, repv msecs), rep_Subr2) /*
::doc:rep.io.processes#accept-process-output::
accept-process-output [SECONDS] [MILLISECONDS]

Wait SECONDS plus MILLISECONDS for output from any asynchronous
subprocesses. If any arrives, process it, then return nil. Otherwise
return t.

Note that output includes notification of process termination.
::end:: */
{
  rep_DECLARE2_OPT(secs, rep_NUMERICP);
  rep_DECLARE3_OPT(msecs, rep_NUMERICP);

  repv result = Qt;

  /* Only wait for output if nothing already waiting. */

  if (!pending_sigchld && !notify_list) {
    result = (rep_accept_input_for_callbacks
	      ((rep_get_long_int(secs) * 1000)
	       + (rep_get_long_int(msecs)),
	       n_input_handlers, input_handlers));
  }

  if (pending_sigchld || notify_list) {
    result = rep_nil;
    rep_proc_periodically();
  }

  return result;
}

DEFUN("accept-process-output-1", Faccept_process_output_1,
      Saccept_process_output_1,
      (repv process, repv secs, repv msecs), rep_Subr3) /*
::doc:rep.io.processes#accept-process-output-1::
accept-process-output-1 PROCESS [SECONDS] [MILLISECONDS]

Wait SECONDS plus MILLISECONDS for output from the asynchronous
subprocess PROCESS. If any arrives, process it, then return nil.
Otherwise return t.

Note that output includes notification of process termination.
::end:: */
{
  rep_DECLARE1(process, PROCESSP);
  rep_DECLARE2_OPT(secs, rep_NUMERICP);
  rep_DECLARE3_OPT(msecs, rep_NUMERICP);

  /* Only wait for output if nothing already waiting. */

  if (pending_sigchld) {
    handle_process_events();
  }

  repv result = Qt;

  if (!notification_queued_p(PROC(process))) {
    int fds[2];
    fds[0] = PROC(process)->stdout_fd;
    fds[1] = PROC(process)->stderr_fd;
    result = (rep_accept_input_for_fds
	      ((rep_get_long_int(secs) * 1000)
	       + rep_get_long_int(msecs), 2, fds));
  }

  if (pending_sigchld) {
    handle_process_events();
  }

  if (notification_queued_p(PROC(process))) {
    notify_process(PROC(process));
    result = Qt;
  }

  return result;
}

/* Don't use libc system(), since it blocks signals. */

repv
rep_system(char *command)
{
  int pid = fork();

  switch (pid) {
    DEFSTRING(cant_fork, "can't fork()");

  case -1:
    return Fsignal(Qerror, Fcons(rep_VAL(&cant_fork), rep_nil));

  case 0: {
    set_child_environ(); 
    char *argv[4];
    argv[0] = "sh";
    argv[1] = "-c";
    argv[2] = command;
    argv[3] = 0;
    signal(SIGPIPE, SIG_DFL);
    execve("/bin/sh", argv, environ);
    perror("can't exec /bin/sh");
    _exit(255); }

  default:
    break;
  }

  repv ret = rep_nil;
  int interrupt_count = 0;

  rep_sig_restart(SIGCHLD, false);

  while (1) {
    struct timeval timeout;

    rep_TEST_INT_SLOW;
    if (rep_INTERRUPTP) {
      static int signals[] = {0, SIGINT, SIGTERM, SIGQUIT};
      if (interrupt_count < 3) {
	interrupt_count++;
      }
      kill(pid, signals[interrupt_count]);
      if (rep_throw_value == rep_int_cell) {
	rep_throw_value = 0;
      }
    }

    int status;
    int x = waitpid(pid, &status, WNOHANG);
    if (x == -1) {
      if (errno != EINTR && errno != EAGAIN) {
	DEFSTRING(cant_waitpid, "can't waitpid()");
	ret = Fsignal(Qerror, Fcons(rep_VAL(&cant_waitpid), rep_nil));
	break;
      }
    } else if (x == pid) {
      ret = rep_MAKE_INT(status);
      break;
    }

    timeout.tv_sec = 1;
    timeout.tv_usec = 0;
    select(FD_SETSIZE, NULL, NULL, NULL, &timeout);
  }

  rep_sig_restart(SIGCHLD, true);
  return ret;
}

static void
processes_init(void)
{
  rep_INTERN(pipe);
  rep_INTERN(pty);
  rep_INTERN(socketpair);

  rep_ADD_SUBR(Sclose_process);
  rep_ADD_SUBR(Smake_process);
  rep_ADD_SUBR(Sstart_process);
  rep_ADD_SUBR(Scall_process);
  rep_ADD_SUBR(Sinterrupt_process);
  rep_ADD_SUBR(Skill_process);
  rep_ADD_SUBR(Sstop_process);
  rep_ADD_SUBR(Scontinue_process);
  rep_ADD_SUBR(Ssignal_process);
  rep_ADD_SUBR(Sprocess_exit_status);
  rep_ADD_SUBR(Sprocess_exit_value);
  rep_ADD_SUBR(Sprocess_id);
  rep_ADD_SUBR(Sprocess_running_p);
  rep_ADD_SUBR(Sprocess_stopped_p);
  rep_ADD_SUBR(Sprocess_in_use_p);
  rep_ADD_SUBR(Sprocessp);
  rep_ADD_SUBR(Sprocess_prog);
  rep_ADD_SUBR(Sset_process_prog);
  rep_ADD_SUBR(Sprocess_args);
  rep_ADD_SUBR(Sset_process_args);
  rep_ADD_SUBR(Sprocess_output_stream);
  rep_ADD_SUBR(Sset_process_output_stream);
  rep_ADD_SUBR(Sprocess_error_stream);
  rep_ADD_SUBR(Sset_process_error_stream);
  rep_ADD_SUBR(Sprocess_function);
  rep_ADD_SUBR(Sset_process_function);
  rep_ADD_SUBR(Sprocess_dir);
  rep_ADD_SUBR(Sset_process_dir);
  rep_ADD_SUBR(Sprocess_connection_type);
  rep_ADD_SUBR(Sset_process_connection_type);
  rep_ADD_SUBR(Sactive_processes);
  rep_ADD_SUBR(Saccept_process_output);
  rep_ADD_SUBR(Saccept_process_output_1);
}

static repv
process_type(void)
{
  static repv type;

  if (!type) {
    static rep_type process = {
      .name = "process",
      .print = process_print,
      .sweep = process_sweep,
      .mark = process_mark,
      .mark_type = process_mark_active,
      .putc = process_putc,
      .puts = process_puts,
    };

    type = rep_define_type(&process);

    rep_register_process_input_handler(read_from_fd);
    rep_add_event_loop_callback(event_loop_callback);
  }

  return type;
}

void
rep_processes_init(void)
{
  /* Setup SIGCHLD stuff.  */

  sigemptyset(&chld_sigset);
  sigaddset(&chld_sigset, SIGCHLD);
  chld_sigact.sa_handler = sigchld_handler;
  chld_sigact.sa_mask = chld_sigset;
#ifdef SA_RESTART
  chld_sigact.sa_flags = SA_RESTART;
#else
  chld_sigact.sa_flags = 0;
#endif
  sigaction(SIGCHLD, &chld_sigact, NULL);

  signal(SIGPIPE, SIG_IGN);

  rep_lazy_structure("rep.io.processes", processes_init);
}

void
rep_processes_kill(void)
{
  signal(SIGCHLD, SIG_DFL);

  rep_process *pr = process_list;
  process_list = NULL;

  while (pr) {
    rep_process *next = pr->next;
    delete_process(pr);
    pr = next;
  }
}
