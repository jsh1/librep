/* rep-remote.c -- remote filesystem back-end

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

/* todo:

    * support non 8-bit clean connections?
    * make `mv' work across filesystems? */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <assert.h>
#include <pwd.h>
#include <grp.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdarg.h>

#if HAVE_DIRENT_H
# include <dirent.h>
# define NAMLEN(dirent) strlen((dirent)->d_name)
#else
# define dirent direct
# define NAMLEN(dirent) (dirent)->d_namlen
# if HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif
# if HAVE_SYS_DIR_H
#  include <sys/dir.h>
# endif
# if HAVE_NDIR_H
#  include <ndir.h>
# endif
#endif

#ifndef PATH_MAX
# define PATH_MAX 256
#endif

#ifndef S_ISLNK
#define S_ISLNK(mode)  (((mode) & S_IFMT) == S_IFLNK)
#endif

#ifndef S_ISSOCK
#define S_ISSOCK(mode)  (((mode) & S_IFMT) == S_IFSOCK)
#endif

#define PROTOCOL_VERSION 1


/* Utility functions. */

static void
x_perror(char *fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
  fprintf(stderr, ": %s\n", strerror(errno));
  exit(10);
}

static void
send_char(char c)
{
  if (write(1, &c, 1) != 1) {
    x_perror("send_char");
  }
}    

static int
read_char(void)
{
  char c;
  if (read(0, &c, 1) != 1) {
    return EOF;
  }
  return c;
}

static void
send_long(intptr_t value)
{
  unsigned char size;
  char lbuf[64];
  size = sprintf(lbuf, "%lx", value);
  if (write(1, &size, 1) != 1) {
    x_perror("send_long");
  }
  if (write(1, lbuf, size) != (int) size) {
    x_perror("send_long");
  }
}

static intptr_t
read_long(void)
{
  unsigned char size;
  char lbuf[64];
  if (read(0, &size, 1) != 1 || size > sizeof(lbuf)-1) {
    x_perror("read_long");
  }
  if (read(0, lbuf, size) != (int) size) {
    x_perror("read_long");
  }
  lbuf[size] = 0;
  return strtol(lbuf, 0, 16);
}

static void
send_string(char *string)
{
  intptr_t length = strlen(string);
  send_long(length);
  if (write(1, string, length) != length) {
    x_perror("send_string");
  }
}

static char *
read_string()
{
  intptr_t length = read_long();
  char *buf = malloc(length + 1);
  if (read(0, buf, length) != length) {
    x_perror("read_string");
  }
  buf[length] = 0;
  return buf;
}

static void
send_success(void)
{
  send_char('\001');
}

static void
send_errno(int error)
{
  send_char('\177');
  send_string(strerror(error));
}

static char *
quote_string(char *out, char *in)
{
  *out++ = '"';

  char c;
  while ((c = *in++) != 0) {
    switch (c) {
    case 0:
      *out++ = '\\';
      *out++ = '0';
      *out++ = '0';
      *out++ = '0';
      break;

    case '"':
      *out++ = '\\';
      *out++ = '"';
      break;

    case '\\':
      *out++ = '\\';
      *out++ = '\\';
      break;

    default:
      *out++ = c;
    }
  }

  *out++ = '"';

  *out = 0;
  return out;
}

static char *
uid_name(uid_t uid)
{
  struct passwd *pw = getpwuid(uid);
  return pw ? pw->pw_name : NULL;
}

static char *
gid_name(gid_t gid)
{
  struct group *gr = getgrgid(gid);
  return gr ? gr->gr_name : NULL;
}

static char *
output_mode_string(char *out, unsigned int perms)
{
  memset(out, '-', 10);

  char c = '-';
  if (S_ISDIR(perms)) {
    c = 'd';
  } else if (S_ISLNK(perms)) {
    c = 'l';
  } else if (S_ISBLK(perms)) {
    c = 'b';
  } else if (S_ISCHR(perms)) {
    c = 'c';
  } else if (S_ISFIFO(perms)) {
    c = 'p';
  } else if (S_ISSOCK(perms)) {
    c = 's';
  }
  out[0] = c;

  for (int i = 0; i < 3; i++) {
    unsigned int xperms = perms >> ((2 - i) * 3);
    if (xperms & 4) {
      out[1 + i * 3] = 'r';
    }
    if (xperms & 2) {
      out[2 + i * 3] = 'w';
    }
    c = (xperms & 1) ? 'x' : 0;
    if (perms & (04000 >> i)) {
      static char extra_bits[3] = {'S', 'S', 'T'};
      /* Rampant abuse of ASCII knowledge :-) */
      c = extra_bits[i] | (c & 0x20);
    }
    if (c != 0) {
      out[3+i*3] = c;
    }
  }

  out[10] = 0;
  return out + 10;
}


/* Commands. */

static void
do_get(int argc, char **argv)
{
  assert(argc == 1);

  struct stat st;
  if (stat(argv[0], &st) == 0 && S_ISREG(st.st_mode)) {
    FILE *fh = fopen(argv[0], "r");
    if (fh) {
      size_t size = st.st_size;
      send_success();
      send_long(size);
      while (size > 0) {
	char buf[BUFSIZ];
	int this = (size > BUFSIZ ? BUFSIZ : size);
	this = fread(buf, 1, this, fh);
	if (this == 0) {
	  x_perror("get-read");
	}
	if (write(1, buf, this) != this) {
	  x_perror("get-write");
	}
	size -= this;
      }
      fclose(fh);
    } else {
      send_errno(errno);
    }
  } else {
    send_errno(EISDIR);		/* ?? */
  }
}

static void
do_put(int argc, char **argv)
{
  assert(argc == 1);

  FILE *fh = fopen(argv[0], "w");
  if (fh) {
    intptr_t size = read_long();
    intptr_t todo = size;
    while (todo > 0) {
      char buf[BUFSIZ];
      int this = (todo > BUFSIZ ? BUFSIZ : todo);
      this = read(0, buf, this);
      if (this < 0) {
	x_perror("put-read");
      }
      if (fwrite(buf, 1, this, fh) != this) {
	x_perror("put-write");
      }
      todo -= this;
    }
    fclose(fh);
    send_success();
  } else {
    send_errno(errno);	    
  }
}

static void
do_rm(int argc, char **argv)
{
  assert(argc == 1);

  if (unlink(argv[0]) == 0) {
    send_success();
  } else {
    send_errno(errno);
  }
}

static void
do_rmdir(int argc, char **argv)
{
  assert(argc == 1);

  if (rmdir(argv[0]) == 0) {
    send_success();
  } else {
    send_errno(errno);
  }
}

static void
do_mv(int argc, char **argv)
{
  assert(argc == 2);

  if (rename(argv[0], argv[1]) == 0) {
    send_success();
  } else {
    send_errno(errno);
  }
}

static void
do_mkdir(int argc, char **argv)
{
  assert(argc == 1);

  if (mkdir(argv[0], S_IRWXU | S_IRWXG | S_IRWXO) == 0) {
    send_success();
  } else {
    send_errno(errno);
  }
}

static void
do_cp(int argc, char **argv)
{
  assert(argc == 2);

  int srcf = open(argv[0], O_RDONLY);
  if (srcf != -1) {
    int dstf = open(argv[1], O_WRONLY | O_CREAT | O_TRUNC, 0666);
    if (dstf != -1) {
      struct stat statb;
      if (fstat(srcf, &statb) == 0) {
	chmod(argv[1], statb.st_mode);
      }
      int rd;
      do {
	char buf[BUFSIZ];
	rd = read(srcf, buf, BUFSIZ);
	if (rd < 0) {
	  x_perror("copy-read");
	}
	int wr = write(dstf, buf, rd);
	if (wr != rd) {
	  x_perror("copy-write");
	}
      } while (rd != 0);
      close(dstf);
      send_success();
    } else {
      send_errno(errno);
    }
    close(srcf);
  } else {
    send_errno(errno);
  }
}

static void
do_chmod(int argc, char **argv)
{
  assert(argc == 2);

  unsigned long mode = strtol(argv[1], 0, 16);
  if (chmod(argv[0], mode) == 0) {
    send_success();
  } else {
    send_errno(errno);
  }
}

static void
do_readlink(int argc, char **argv)
{
  assert(argc == 1);

  char buf[PATH_MAX];
  int length = readlink(argv[0], buf, sizeof(buf));

  if (length != -1) {
    send_success();
    buf[length] = 0;
    send_string(buf);
  } else {
    send_errno(errno);
  }
}

static void
do_symlink(int argc, char **argv)
{
  assert(argc == 2);

  if (symlink(argv[0], argv[1]) == 0) {
    send_success();
  } else {
    send_errno(errno);
  }
}

static void
do_readdir(int argc, char **argv)
{
  assert(argc == 1);

  DIR *dir = opendir(argv[0]);
  if (dir) {
    char dirname[PATH_MAX];
    strcpy(dirname, argv[0]);
    if (dirname[strlen(dirname)-1] != '/') {
      strcat(dirname, "/");
    }
    struct dirent *de;
    while ((de = readdir(dir))) {
      /* for each entry write out the following record:

         [ NAME SIZE MODTIME TYPE MODES MODE-STRING NLINKS USER GROUP ]

	 suitable for Lisp reading. */

      char nambuf[PATH_MAX];
      strcpy(nambuf, dirname);
      strcat(nambuf, de->d_name);

      struct stat st;
      if (lstat(nambuf, &st) == 0) {
	char buf[3*PATH_MAX];
	char *ptr = buf;
	*ptr++ = '[';
	ptr = quote_string(ptr, de->d_name);
	ptr += sprintf(ptr, " %ld (%ld . %ld) %s %d \"",
		       (long)st.st_size, st.st_mtime / 86400,
		       st.st_mtime % 86400,
		       S_ISREG(st.st_mode) ? "file"
		       : S_ISDIR(st.st_mode) ? "directory"
		       : S_ISLNK(st.st_mode) ? "symlink"
		       : S_ISFIFO(st.st_mode) ? "pipe"
		       : S_ISSOCK(st.st_mode) ? "socket"
		       : S_ISCHR(st.st_mode) ? "device"
		       : S_ISBLK(st.st_mode) ? "device"
		       : "nil",
		       (int)st.st_mode & 07777);
	ptr = output_mode_string(ptr, st.st_mode);
	ptr += sprintf(ptr, "\" %d \"%s\" \"%s\"]\n",
		       (int)st.st_nlink,
		       uid_name(st.st_uid), gid_name(st.st_gid));
	send_char('\002');
	send_string(buf);
      } else {
	x_perror("readdir-stat");
      }
    }
    closedir(dir);
    send_success();
  } else {
    send_errno(errno);
  }
}


/* Entry point. */

static void
print_signature(void)
{
  char buf[256];
  sprintf(buf, "\002rep-remote; protocol %d\002\001", PROTOCOL_VERSION);
  if (write(1, buf, strlen(buf)) != strlen(buf)) {
    x_perror("print_signature");
  }
}

int
main(int argc, char **argv)
{
  fflush(0);

  /* Paranoia against braindead installation. */

  if (getuid() != geteuid() || getgid() != getegid()) {
    fputs("Don't install rep-remote setuid.\n", stderr);
    return 10;
  }

  print_signature();

  while (1) {

    int command = read_char();
    int nargs = read_char();
    if (command == EOF || nargs == EOF) {
      return 0;
    }

    char *args[nargs+1];

    for (int i = 0; i < nargs; i++) {
      args[i] = read_string();
    }

    args[nargs] = 0;
	
    switch (command) {
    case 'G':			/* get FILENAME */
      do_get(nargs, args);
      break;

    case 'P':			/* put FILENAME */
      do_put(nargs, args);
      break;

    case 'R':			/* rm FILE */
      do_rm(nargs, args);
      break;

    case 'r':			/* rmdir FILE */
      do_rmdir(nargs, args);
      break;

    case 'M':			/* mv SOURCE DEST */
      do_mv(nargs, args);
      break;

    case 'm':			/* mkdir FILE */
      do_mkdir(nargs, args);
      break;

    case 'C':			/* cp SOURCE DEST */
      do_cp(nargs, args);
      break;

    case 'c':			/* chmod FILE MODE */
      do_chmod(nargs, args);
      break;

    case 'l':			/* readlink FILE */
      do_readlink(nargs, args);
      break;

    case 'L':			/* symlink NAME1 NAME2 */
      do_symlink(nargs, args);
      break;

    case 'D':			/* readdir FILE */
      do_readdir(nargs, args);
      break;

    case 'Q':			/* quit */
      return 0;

    case '\n': case '\r':		/* ignored */
      break;
    }

    for (int i = 0; i < nargs; i++) {
      free(args[i]);
    }
  }
}
