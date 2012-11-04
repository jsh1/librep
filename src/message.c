/* message.c -- 
   Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>
   $Id$

   This file is part of Jade.

   Jade is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Jade is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Jade; see the file COPYING.	If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#define _GNU_SOURCE

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "rep.h"
#include <stdio.h>

static void
default_message (enum rep_message fn, ...)
{
    va_list args;
    va_start (args, fn);
    switch (fn)
    {
	size_t len;
	char *msg;

    case rep_messagen:
	msg = (char *)va_arg(args, char *);
	len = (size_t)va_arg(args, size_t);
	fwrite(msg, 1, len, stderr);
	fputc('\n', stderr);
	break;

    case rep_message:
	msg = (char *)va_arg(args, char *);
	fputs (msg, stderr);
	fputc ('\n', stderr);
	break;

    case rep_messagef:
	msg = (char *)va_arg(args, char *);
	vfprintf (stderr, msg, args);
	fputc ('\n', stderr);
	break;

    case rep_append_message:
	msg = (char *)va_arg(args, char *);
	len = (size_t)va_arg(args, size_t);
	fwrite(msg, len, 1, stderr);
	fputc('\n', stderr);
	break;

    case rep_reset_message: 		/* (void) */
    case rep_redisplay_message:		/* (void) */
	break;
    }
}

void (*rep_message_fun)(enum rep_message fn, ...) = default_message;
