/*
 * regsub @(#)regsub.c	1.3 of 2 April 86
 *
 * Copyright (c) 1986 by University of Toronto. Written by Henry Spencer.  Not
 * derived from licensed software.
 *
 * Permission is granted to anyone to use this software for any purpose on any
 * computer system, and to redistribute it freely, subject to the following
 * restrictions:
 *
 * 1. The author is not responsible for the consequences of use of this
 * software, no matter how awful, even if they arise from defects in it.
 *
 * 2. The origin of this software must not be misrepresented, either by explicit
 * claim or by omission.
 *
 * 3. Altered versions must be plainly marked as such, and must not be
 * misrepresented as being the original software.
 */

#define rep_NEED_REGEXP_INTERNALS

#include "repint.h"

#include <stdio.h>
#include <string.h>

/*
 * Local changes:
 *
 * - added function prototypes.
 * - added the regsublen() function.
 */

#ifndef CHARBITS
#define UCHARAT(p)	((int)*(unsigned char *)(p))
#else
#define UCHARAT(p)	((int)*(p)&CHARBITS)
#endif

/*
 * - regsub - perform substitutions after a regexp match
 *
 * data is null if the last match was a string, or the TX if the last
 * match was on a buffer.
 */

void
rep_default_regsub(int last_type, rep_regsubs *matches,
		   const char *source, char *dest, void *data)
{
  if (!matches || !source || !dest) {
    rep_regerror("NULL parm to regsub");
    return;
  }

  if ((last_type == rep_reg_string && !rep_STRINGP(rep_VAL(data)))
      || (last_type == rep_reg_obj))
  {
    rep_regerror("Bad type of data to regsub");
    return;
  }

  const char *src = source;
  char *dst = dest;
  int c;

  while ((c = *src++) != '\0') {
    int idx = -1;
    if (c == '&') {
      idx = 0;
    } else if (c == '\\' && '0' <= *src && *src <= '9') {
      idx = *src++ - '0';
    }

    if (idx < 0) {
      if (c == '\\' && (*src == '\\' || *src == '&')) {
	c = *src++;
      }
      *dst++ = c;
    } else {
      if (last_type == rep_reg_string) {
	if (matches->string.startp[idx] && matches->string.endp[idx]) {
	  size_t len = matches->string.endp[idx] - matches->string.startp[idx];
	  strncpy(dst, matches->string.startp[idx], len);
	  dst += len;
	  if (len != 0 && *(dst - 1) == '\0') {
	    /* strncpy hit NUL. */
	    rep_regerror("damaged match string");
	    return;
	  }
	}
      }
    }
  }

  *dst++ = '\0';
}

/*
 * - regsublen - dummy regsub() returning length of contructed string,
 * including terminating '\0'
 */

size_t
rep_default_regsublen(int last_type, rep_regsubs *matches,
		      const char *source, void *data)
{
  if (!matches || !source) {
    rep_regerror("NULL parm to regsublen");
    return 0;
  }

  if ((last_type == rep_reg_string && !rep_STRINGP(rep_VAL(data)))
      || (last_type == rep_reg_obj))
  {
    rep_regerror("Bad type of data to regsublen");
    return 0;
  }

  const char *src = source;
  size_t dstlen = 0;
  int c;

  while ((c = *src++) != 0) {
    int idx = -1;
    if (c == '&') {
      idx = 0;
    } else if (c == '\\' && '0' <= *src && *src <= '9') {
      idx = *src++ - '0';
    }

    if (idx < 0) {
      if (c == '\\' && (*src == '\\' || *src == '&')) {
	c = *src++;
      }
      dstlen++;
    } else {
      if (last_type == rep_reg_string) {
	if (matches->string.startp[idx] && matches->string.endp[idx]) {
	  dstlen += matches->string.endp[idx] - matches->string.startp[idx];
	}
      }
    }
  }

  return dstlen + 1;
}
