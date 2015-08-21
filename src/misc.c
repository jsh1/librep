/* misc.c -- Miscellaneous functions

   Copyright (C) 2000-2015 John Harper <jsh@unfactored.org>

   This file is part of librep.

   librep is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   librep is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with librep; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "repint.h"

#include <string.h>
#include <stdlib.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

void (*rep_beep_fun)(void);

#ifndef HAVE_STPCPY

/* Copy src to dst, returning pointer to terminating '\0' in dst. */

char *
stpcpy(char *dst, const char *src)
{
  while((*dst++ = *src++) != 0) {
  }
  return dst - 1;
}

#endif /* !HAVE_STPCPY */

#ifndef HAVE_STRNCASECMP

/* Compare no more than N characters of S1 and S2, ignoring case,
   returning less than, equal to or greater than zero if S1 is
   lexicographically less than, equal to or greater than S2. (from
   glibc) */

int
strncasecmp(const char *s1, const char *s2, size_t n)
{
  const unsigned char *p1 = (const unsigned char *) s1;
  const unsigned char *p2 = (const unsigned char *) s2;
  unsigned char c1, c2;

  if (p1 == p2 || n == 0) {
    return 0;
  }

  do {
    c1 = rep_tolower(*p1++);
    c2 = rep_tolower(*p2++);
    if (c1 == '\0' || c1 != c2) {
      return c1 - c2;
    }
  } while (--n > 0);

  return c1 - c2;
}

#endif /* !HAVE_STRNCASECMP */

char *
rep_str_dupn(const char *old, int len)
{
  char *new = rep_alloc(len + 1);

  if (new) {
    memcpy(new, old, len);
    new[len] = 0;
  }

  return new;
}

static void
default_beep(void)
{
  fputc(7, stdout);
  fflush(stdout);
}

DEFUN_INT("beep", Fbeep, Sbeep, (void), rep_Subr0, "") /*
::doc:rep.system#beep::
beep

Rings a bell.
::end:: */
{
  if (rep_beep_fun) {
    (*rep_beep_fun)();
  }
  return Qt;
}

DEFUN("message", Fmessage, Smessage, (repv string, repv now), rep_Subr2) /*
::doc:rep.system#message::
message STRING [DISPLAY-NOW]

Temporarily sets the status display to STRING, this may not happen until the
next complete redisplay, unless DISPLAY-NOW is non-nil.
::end:: */
{
  rep_DECLARE1(string, rep_STRINGP);

  if (rep_message_fun) {
    (*rep_message_fun)(rep_message, rep_STR(string));

    if(now != rep_nil) {
      (*rep_message_fun)(rep_redisplay_message);
    }
  }

  return string;
}

DEFUN_INT("system", Fsystem, Ssystem, (repv command), rep_Subr1, "sShell command:") /*
::doc:rep.system#system::
system SHELL-COMMAND

Synchronously execute the shell command string SHELL-COMMAND. Returns the
exit status of the command, or signals an error if the shell couldn't
be started.

Note that the exit status is _not_ the same as the return code. It
depends on the operating system, but under unix the return code may be
found by right-shifting the exit status by eight bits. Low non-zero
values represent that the process was killed by a signal.
::end:: */
{
  rep_DECLARE1(command, rep_STRINGP);

  return rep_system(rep_STR(command));
}

DEFUN("crypt", Fcrypt, Scrypt, (repv key, repv salt), rep_Subr2) /*
::doc:rep.system#crypt::
crypt KEY SALT

The `crypt' function takes a password, KEY, as a string, and a SALT
character array, and returns a printable ASCII string which starts with
another salt.  It is believed that, given the output of the function,
the best way to find a KEY that will produce that output is to guess
values of KEY until the original value of KEY is found.

See crypt(3) for more information.
::end:: */
{
  rep_DECLARE1(key, rep_STRINGP);
  rep_DECLARE2(salt, rep_STRINGP);

#ifdef HAVE_CRYPT
  const char *output = crypt(rep_STR(key), rep_STR(salt));
  return rep_string_copy(output);
#else
  DEFSTRING(err, "crypt() isn't supported on this system");
  return Fsignal(Qerror, rep_LIST_1(rep_VAL(&err)));
#endif
}

void
rep_misc_init(void)
{
  if (!rep_beep_fun) {
    rep_beep_fun = default_beep;
  }

  repv tem = rep_push_structure("rep.system");
  rep_ADD_SUBR_INT(Sbeep);
  rep_ADD_SUBR(Scrypt);
  rep_ADD_SUBR_INT(Ssystem);
  rep_ADD_SUBR(Smessage);
  rep_pop_structure(tem);
}
