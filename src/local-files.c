/* local-files.c -- file handler functions for Unix-like files

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
#include <sys/stat.h>

#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

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

#ifdef HAVE_STRERROR
# include <errno.h>
#else
extern int sys_nerr, errno;
extern char *sys_errlist[];
#endif


/* Support functions */

#ifndef HAVE_STRERROR
DEFSTRING(unknown_err, "Unknown system error");
#endif
repv
rep_lookup_errno(void)
{
#ifdef HAVE_STRERROR
  return rep_string_copy(strerror(errno));
#else
  if(errno >= sys_nerr) {
    return rep_string_copy(sys_errlist[errno]);
  } else {
    return rep_VAL(&unknown_err);
  }
#endif
}

void
rep_set_fd_nonblocking(int fd)
{
  int flags = fcntl(fd, F_GETFL, 0);

  if (flags != -1) {
    fcntl(fd, F_SETFL, flags | O_NONBLOCK);
  }
}

void
rep_set_fd_blocking(int fd)
{
  int flags = fcntl(fd, F_GETFL, 0);

  if (flags != -1) {
    fcntl(fd, F_SETFL, flags & ~O_NONBLOCK);
  }
}

void
rep_set_fd_cloexec(int fd)
{
  int flags = fcntl(fd, F_GETFD, 0);

  if (flags != -1) {
    fcntl(fd, F_SETFD, flags | FD_CLOEXEC);
  }
}

DEFSTRING(dot, ".");

static inline const char *
file_part(const char *name)
{
  const char *tmp = strrchr(name, '/');
  return tmp ? tmp + 1 : name;
}

static inline bool
stat_file(repv file, struct stat *st)
{
  return stat(rep_STR(file), st) == 0 ? true : false;
}

size_t
rep_file_length(repv file)
{
  struct stat st;

  if (stat_file(file, &st)) {
    return st.st_size;
  } else {
    return 0;
  }
}


/* File ops */

repv
rep_file_name_absolute_p(repv file)
{
  return rep_STR(file)[0] == '/' ? Qt : rep_nil;
}

repv
rep_expand_file_name(repv file)
{
  const char *iptr = rep_STR(file);

  char buf[PATH_MAX];
  char *optr = buf;

  while (*iptr != 0) {
    if (iptr[0] == '.') {
      if (iptr[1] == '/') {
	iptr += 1;
	goto strip;
      } else if(iptr[1] == 0) {
	if (optr == buf) {
	  /* Only character in string. Must preserve the dot. */
	  *optr++ = '.';
	}
	iptr++;
	continue;
      } else if(iptr[1] == '.' && (iptr[2] == '/' || iptr[2] == 0)) {
	/* `foo/..[/]' Try to back up over the parent directory */
	char *back = optr;
	bool all_dots = true;
	/* Step over any contiguous `/' characters */
	while (back > buf && back[-1] == '/') {
	  back--;
	}
	char *end = back;
	/* Step over any non-`/' characters */
	while (back > buf && back[-1] != '/') {
	  back--;
	  if (back[0] != '.') {
	    all_dots = false;
	  }
	}
	if (back < optr && back >= buf && *back != '/'
	    /* Don't allow `../..' -> `' */
	    && (!all_dots || end - back != 2))
	{
	  /* Reset the output ptr to the end of the parent */
	  optr = back;
	}
	/* Check for `/..' */
	else if (all_dots && end == back
		 && back == buf && optr > buf
		 && buf[0] == '/' && optr - end == 1)
	{
	  optr = back + 1;
	} else {
	  /* Can't move up; leave the .. in the file name */
	  *optr++ = '.';
	  *optr++ = '.';
	  if(iptr[2] == '/') {
	    *optr++ = '/';
	  }
	}
	iptr += (iptr[2] == 0) ? 2 : 3;
	goto strip;
      }
    }

    const char *end = strchr(iptr, '/');
    if (!end) {
      end = iptr + strlen(iptr);
    }
    memcpy(optr, iptr, end - iptr);
    optr += end - iptr;
    iptr = end;

    if (*iptr == '/') {
      *optr++ = *iptr++;
    }

  strip:
    /* merge multiple slashes into one */
    while (*iptr && *iptr == '/') {
      iptr++;
    }
  }

  /* Don't allow a fully-empty string to be returned */

  if (optr - buf == 0) {
    *optr++ = '.';
  }

  if (optr - buf != rep_STRING_LEN(file)
      || memcmp(rep_STR(file), buf, optr - buf) != 0)
  {
    return rep_string_copy_n(buf, optr - buf);
  } else {
    return file;
  }
}

repv
rep_canonical_file_name(repv file)
{
  char buf[PATH_MAX];

  if (!realpath(rep_STR(file), buf)) {
    /* realpath () failed; copy the source */
    strncpy(buf, rep_STR(file), sizeof(buf));
  }

  size_t len = strlen(buf);

  while (len > 0 && buf[len - 1] == '/') {
    buf[len - 1] = 0;
    len--;
  }

  return rep_string_copy_n(buf, len);
}

repv
rep_file_name_nondirectory(repv file)
{
  const char *tem = file_part(rep_STR(file));
  return tem == rep_STR(file) ? file : rep_string_copy(tem);
}

repv
rep_file_name_directory(repv file)
{
  ptrdiff_t len = file_part(rep_STR(file)) - rep_STR(file);
  return rep_string_copy_n(rep_STR(file), len);
}

repv
rep_file_name_as_directory(repv file)
{
  intptr_t len = rep_STRING_LEN(file);

  if (file_part(rep_STR(file)) == rep_STR(file) + len) {
    /* It's already a directory */
    return file;
  }

  repv ret = rep_string_copy_n(rep_STR(file), len + 1);

  if (ret) {
    rep_MUTABLE_STR(ret)[len] = '/';
    rep_MUTABLE_STR(ret)[len+1] = 0;
  }

  return ret;
}

repv
rep_directory_file_name(repv file)
{
  intptr_t len = rep_STRING_LEN(file);

  if (file_part(rep_STR(file)) != rep_STR(file) + len) {
    /* There's a file part. Just return the initial string? */
    return file;
  }

  if (len == 0) {
    return rep_VAL(&dot);
  } else if(len == 1) {
    return file;
  } else {
    /* Chop the trailing "/" */
    return rep_string_copy_n(rep_STR(file), len - 1);
  }
}

repv
rep_delete_file(repv file)
{
  if (unlink(rep_STR(file)) == 0) {
    return Qt;
  } else {
    return rep_signal_file_error(file);
  }
}

repv
rep_rename_file(repv old, repv new)
{
  if (rename(rep_STR(old), rep_STR(new)) != -1) {
    return Qt;
  } else {
    return rep_signal_file_error(rep_list_2(old, new));
  }
}

repv
rep_make_directory(repv dir)
{
  if (mkdir(rep_STR(dir), S_IRWXU | S_IRWXG | S_IRWXO) == 0) {
    return Qt;
  } else {
    return rep_signal_file_error(dir);
  }
}

repv
rep_delete_directory(repv dir)
{
  if (rmdir(rep_STR(dir)) == 0) {
    return Qt;
  } else {
    return rep_signal_file_error(dir);
  }
}

repv
rep_copy_file(repv src, repv dst)
{
  repv ret = Qt;

  int src_fd = open(rep_STR(src), O_RDONLY);
  if (src_fd != -1) {
    int dst_fd = open(rep_STR(dst), O_WRONLY | O_CREAT | O_TRUNC, 0666);
    if (dst_fd != -1) {
      struct stat st;
      if (fstat(src_fd, &st) == 0) {
	chmod(rep_STR(dst), st.st_mode);
      }
      ssize_t bytes;
      do {
	char buf[BUFSIZ];
	bytes = read(src_fd, buf, BUFSIZ);
	if (bytes < 0) {
	  ret = rep_signal_file_error(src);
	  break;
	}
	if (write(dst_fd, buf, bytes) != bytes) {
	  ret = rep_signal_file_error(dst);
	  break;
	}
      } while(bytes != 0);
      close(dst_fd);
    } else {
      ret = rep_signal_file_error(dst);
    }
    close(src_fd);
  } else {
    ret = rep_signal_file_error(src);
  }

  return ret;
}

repv
rep_file_readable_p(repv file)
{
  return access(rep_STR(file), R_OK) == 0 ? Qt : rep_nil;
}

repv
rep_file_writable_p(repv file)
{
  return access(rep_STR(file), W_OK) == 0 ? Qt : rep_nil;
}

repv
rep_file_exists_p(repv file)
{
  return access(rep_STR(file), F_OK) == 0 ? Qt : rep_nil;
}

repv
rep_file_regular_p(repv file)
{
  struct stat st;

  if (stat_file(file, &st)) {
    return S_ISREG(st.st_mode) ? Qt : rep_nil;
  } else {
    return rep_nil;
  }
}

repv
rep_file_directory_p(repv file)
{
  struct stat st;

  if (stat_file(file, &st)) {
    return S_ISDIR(st.st_mode) ? Qt : rep_nil;
  } else {
    return rep_nil;
  }
}

repv
rep_file_symlink_p(repv file)
{
  struct stat st;

  if (lstat(rep_STR(file), &st) == 0) {
    return S_ISLNK(st.st_mode) ? Qt : rep_nil;
  } else {
    return rep_nil;
  }
}

repv
rep_file_owner_p(repv file)
{
  struct stat st;

  if (stat_file(file, &st)) {
    return st.st_uid == geteuid() && st.st_gid == getegid() ? Qt : rep_nil;
  } else {
    return rep_nil;
  }
}

repv
rep_file_nlinks(repv file)
{
  struct stat st;

  if (stat_file(file, &st)) {
    return rep_MAKE_INT(st.st_nlink);
  } else {
    return rep_nil;
  }
}

repv
rep_file_size(repv file)
{
  struct stat st;

  if (stat_file(file, &st)) {
    return rep_make_long_uint(st.st_size);
  } else {
    return rep_nil;
  }
}

repv
rep_file_modes(repv file)
{
  struct stat st;

  if (stat_file(file, &st)) {
    return rep_MAKE_INT(st.st_mode & 07777);
  } else {
    return rep_nil;
  }
}

repv
rep_set_file_modes(repv file, repv modes)
{
  rep_DECLARE2(modes, rep_INTP);

  if (chmod(rep_STR(file), rep_INT(modes)) == 0) {
    return modes;
  } else {
    return rep_signal_file_error(file);
  }
}

repv
rep_file_modes_as_string(repv file)
{
  struct stat st;

  repv string = Fmake_string(rep_MAKE_INT(10), rep_MAKE_INT('-'));

  if (stat_file(file, &st) && string && rep_STRINGP(string)) {
    unsigned int perms = st.st_mode;
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
    rep_MUTABLE_STR(string)[0] = c;
    for(int i = 0; i < 3; i++) {
      unsigned int xperms = perms >> ((2 - i) * 3);
      if (xperms & 4) {
	rep_MUTABLE_STR(string)[1+i*3] = 'r';
      }
      if (xperms & 2) {
	rep_MUTABLE_STR(string)[2+i*3] = 'w';
      }
      c = (xperms & 1) ? 'x' : 0;
      if (perms & (04000 >> i)) {
	static char extra_bits[3] = { 'S', 'S', 'T' };
	/* Rampant abuse of ASCII knowledge :-) */
	c = extra_bits[i] | (c & 0x20);
      }
      if (c != 0) {
	rep_MUTABLE_STR(string)[3+i*3] = c;
      }
    }
  }

  return string;
}

repv
rep_file_modtime(repv file)
{
  struct stat st;

  if (stat_file(file, &st)) {
    return rep_make_long_uint(st.st_mtime);
  } else {
    /* FIXME: really this should return nil? */
    return rep_MAKE_INT(0);
  }
}

repv
rep_directory_files(repv dir_name)
{
  if (rep_STRING_LEN(dir_name) == 0) {
    dir_name = rep_VAL(&dot);
  }

  DIR *dir = opendir(rep_STR(dir_name));
  if (!dir) {
    return Fsignal(Qfile_error, rep_list_2(rep_lookup_errno(), dir_name));
  }

  repv list = rep_nil;

  struct dirent *de;
  while ((de = readdir(dir))) {
    repv name = rep_string_copy_n(de->d_name, NAMLEN(de));
    list = Fcons(name, list);
    if (!name || !list) {
      closedir(dir);
      return rep_mem_error();
    }
  }

  closedir(dir);
  return list;
}

repv
rep_read_symlink(repv file)
{
  char buf[PATH_MAX];

  int len = readlink(rep_STR(file), buf, sizeof(buf));

  if (len == -1) {
    return rep_signal_file_error(file);
  }

  return rep_string_copy_n(buf, len);
}

repv
rep_make_symlink(repv file, repv contents)
{
  if (symlink(rep_STR(contents), rep_STR(file)) == 0) {
    return Qt;
  } else {
    return rep_signal_file_error(file);
  }
}

repv
rep_getpwd(void)
{
  char buf[PATH_MAX];

#ifdef HAVE_GETCWD
  if (!getcwd(buf, PATH_MAX))
#else
  if (!getwd(buf))
#endif
  {
    return rep_signal_file_error(rep_nil);
  }

  /* Ensure that it ends with "/" */

  size_t len = strlen(buf);
  if (len < sizeof(buf) - 1 && buf[len] != '/') {
    buf[len++] = '/';
    buf[len] = 0;
  }

  return rep_string_copy_n(buf, len);
}
