/* repdoc.c -- Program to strip doc-strings from C source

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

#include <stdio.h>
#include <string.h>
#include <gdbm.h>
#include <fcntl.h>
#include <stdlib.h>

#ifndef GDBM_NOLOCK
# define GDBM_NOLOCK 0
#endif

static void
usage(void)
{
  fputs("usage: repdoc doc-file [src-files...]\n", stderr);
  exit(1);
}

static void
scan_file(FILE *src, GDBM_FILE sdbm)
{
  char start_buf[512];

  while (fgets(start_buf, 512, src)) {
    char *start = strstr(start_buf, "::doc:");
    if (start) {
      char *id = start + 6;
      start = strstr(id, "::");
      if (!start) {
	continue;
      }
      *start = 0;

      char doc_buf[16384];		/* so lazy.. */
      char *out = doc_buf;

      while (fgets(out, sizeof(doc_buf) - (out - doc_buf), src)) {
	char *end = strstr(out, "::end::");
	if (end) {
	  break;
	}
	out += strlen(out);
      }

      /* ignore trailing newline */
      if (out > doc_buf) {
	out--;
      }
      *out = 0;

      datum key, value;
      key.dptr = id;
      key.dsize = strlen(id);
      value.dptr = doc_buf;
      value.dsize = strlen(doc_buf);

      if (gdbm_store(sdbm, key, value, GDBM_REPLACE) < 0) {
	perror ("gdbm_store");
      }
    }
  }
}

int
main(int argc, char **argv)
{
  argc--;
  argv++;

  if (argc < 2) {
    usage();
  }

  GDBM_FILE db = gdbm_open(argv[0], 0, GDBM_WRCREAT | GDBM_NOLOCK, 0666, 0);

  if (!db) {
    perror(argv[0]);
    exit(2);
  }

  argc--;
  argv++;

  if (argc == 0) {
    scan_file(stdin, db);
  } else {
    while (argc > 0) {
      FILE *file = fopen(argv[0], "r");
      if (file) {
	scan_file(file, db);
	fclose(file);
      } else {
	perror(argv[0]);
	exit(1);
      }
      argc--;
      argv++;
    }
  }

  gdbm_close(db);

  return 0;
}
