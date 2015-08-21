/* time.c -- time and date functions

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
#include <time.h>

#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

uintptr_t
rep_time(void)
{
  return time(0);
}

DEFUN("current-time", Fcurrent_time, Scurrent_time, (void), rep_Subr0) /*
::doc:rep.system#current-time::
current-time

Return a value denoting the current system time, represented as the
number of seconds since the Unix epoch.
::end:: */
{
  return rep_make_long_uint(rep_time());
}

DEFUN("current-utime", Fcurrent_utime, Scurrent_utime, (void), rep_Subr0) /*
::doc:rep.system#current-utime::
current-utime

Return the current time in microseconds.
::end:: */
{
  long long t;

#ifdef HAVE_GETTIMEOFDAY
  struct timeval time;
  gettimeofday (&time, 0);
  t = ((long long)time.tv_sec * 1000000) + time.tv_usec;
#else
  t = (long long)rep_time () * 1000000;
#endif

  return rep_make_longlong_int(t);
}

DEFUN("current-time-string", Fcurrent_time_string,
      Scurrent_time_string, (repv time, repv format), rep_Subr2) /*
::doc:rep.system#current-time-string::
current-time-string [TIME] [FORMAT]

Returns a human-readable string defining the current date and time, or if
specified, that defining TIME.

If defined, FORMAT is a string defining how to create the string. It has
the same conventions as the template to the C library's strftime function.
::end:: */
{
  time_t timestamp;

  if (rep_NUMBERP(time)) {
    timestamp = rep_get_long_uint(time);
  } else {
    timestamp = rep_time();
  }

  if (rep_STRINGP(format)) {
    struct tm *loctime = localtime(&timestamp);
    char buf[256];
    int len = strftime(buf, sizeof(buf), rep_STR(format), loctime);
    if(len > 0) {
      return rep_string_copy_n(buf, len);
    } else {
      return rep_null_string();
    }
  } else {
    char *str = ctime(&timestamp);
    if (str != 0) {
      return rep_string_copy_n(str, strlen(str) - 1);
    } else {
      return rep_null_string();
    }
  }
}

void
rep_time_init(void)
{
  repv tem = rep_push_structure("rep.system");
  rep_ADD_SUBR(Scurrent_time);
  rep_ADD_SUBR(Scurrent_utime);
  rep_ADD_SUBR(Scurrent_time_string);
  rep_pop_structure(tem);
}
