/* environ.c -- environment variables

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
#include "build.h"

#include <string.h>
#include <stdlib.h>
#include <netdb.h>
#include <pwd.h>

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_SYS_UTSNAME_H
# include <sys/utsname.h>
#endif

#if defined(ENVIRON_UNDECLARED)
extern char **environ;
#endif

DEFSTRING(build_id_string,
	  BUILD_DATE " by " BUILD_USER "@" BUILD_HOST ", for " HOST_TYPE ".");
DEFSTRING(rep_version_string, REP_VERSION);

DEFSYM(rep_version, "rep-version");
DEFSYM(rep_interface_id, "rep-interface-id");
DEFSYM(rep_build_id, "rep-build-id");

/* ::doc:rep.system#rep-version::
A string defining the current version of the REP interpreter.
::end::
::doc:rep.system#rep-build-id::
A string describing when, where, and by who the running version of the
LISP interpreter was built.
::end:: */

DEFSYM(process_environment, "*process-environment*");

/* ::doc:*process-environment*::
A list of all environment variables (as strings "NAME=VALUE") passed
to the interpreter. Also used to specify the environment of subprocesses.
::end:: */

DEFUN("user-login-name", Fuser_login_name,
      Suser_login_name, (void), rep_Subr0) /*
::doc:rep.system#user-login-name::
user-login-name

Returns the login name of the user (a string).
::end:: */
{
  static repv user_login_name;

  if (!user_login_name) {
    const char *tmp = getlogin();
    
    if (tmp) {
      user_login_name = rep_string_copy(tmp);
    } else {
      struct passwd *pwd = getpwuid(geteuid());
      if (pwd && pwd->pw_name) {
	user_login_name = rep_string_copy(pwd->pw_name);
      } else {
	user_login_name = rep_nil;
      }
    }

    rep_mark_static(&user_login_name);
  }

  return user_login_name;
}

DEFUN("user-full-name", Fuser_full_name,
      Suser_full_name, (repv arg), rep_Subr1) /*
::doc:rep.system#user-full-name::
user-full-name [REAL-NAME]

Returns the real name of the user (a string). If REAL-NAME is non-nil, it's
the name to return in subsequent calls.
::end:: */
{
  static repv user_full_name;

  rep_DECLARE1_OPT(arg, rep_STRINGP);

  if (arg != rep_nil) {
    if(!user_full_name) {
      rep_mark_static(&user_full_name);
    }
    user_full_name = arg;
  }

  if (!user_full_name) {
    struct passwd *pwd = getpwuid(geteuid());
    if (pwd) {
      user_full_name = rep_string_copy(pwd->pw_gecos);
    } else {
      user_full_name = rep_nil;
    }

    rep_mark_static(&user_full_name);
  }

  return user_full_name;
}

DEFUN("user-home-directory", Fuser_home_directory,
      Suser_home_directory, (repv user), rep_Subr1) /*
::doc:rep.system#user-home-directory::
user-home-directory [USER]

Return the path to USER's home directory (a string). When USER is undefined
the directory of the user who executed Jade is found.
::end:: */
{
  rep_DECLARE1_OPT(user, rep_STRINGP);

  static repv user_home_directory;

  if (user == rep_nil && user_home_directory) {
    return user_home_directory;
  }

  char *src = 0;

  if (user == rep_nil) {
    src = getenv("HOME");
  }

  if (!src) {
    struct passwd *pwd;
    if (user == rep_nil) {
      pwd = getpwuid(geteuid());
    } else {
      pwd = getpwnam(rep_STR(user));
    }

    if (!pwd || !pwd->pw_dir) {
      DEFSTRING(no_home, "Can't find home directory");
      return Fsignal(Qerror, rep_LIST_2(rep_VAL(&no_home), user));
    }

    src = pwd->pw_dir;
  }

  size_t len = strlen(src);

  repv dir;
  if (src[len] != '/') {
    dir = rep_string_copy_n(src, len + 1);
    rep_STR(dir)[len] = '/';
    rep_STR(dir)[len+1] = 0;
  } else {
    dir = rep_string_copy(src);
  }

  if (user == rep_nil) {
    user_home_directory = dir;
    rep_mark_static(&user_home_directory);
  }

  return dir;
}

DEFUN("system-name", Fsystem_name, Ssystem_name, (void), rep_Subr0) /*
::doc:rep.system#system-name::
system-name

Returns the name of the host which the editor is running on.
::end:: */
{
  static repv system_name;

  if (system_name) {
    return system_name;
  }

  char buf[256];
#ifdef HAVE_GETHOSTNAME
  if (gethostname(buf, 256) != 0) {
    return 0;
  }
#else
  {
    struct utsname uts;
    uname(&uts);
    strncpy(buf, uts.nodename, 256);
  }
#endif

  struct hostent *h = gethostbyname(buf);
  if (h) {
    if (!strchr(h->h_name, '.')) {
      /* The official name is not fully qualified. Try looking through
         the list of alternatives. */

      char **aliases = h->h_aliases;
      while (*aliases && !strchr(*aliases, '.')) {
	aliases++;
      }
      system_name = rep_string_copy(*aliases ? *aliases : h->h_name);
    } else {
      system_name = rep_string_copy((char *)h->h_name);
    }
  } else {
    system_name = rep_string_copy(buf);
  }

  rep_mark_static(&system_name);

  return system_name;
}

DEFUN("get-command-line-option", Fget_command_line_option,
      Sget_command_line_option, (repv opt, repv arg), rep_Subr2) /*
::doc:rep.system#get-command-line-option::
get-command-line-option OPTION [REQUIRES-ARGUMENT]

Returns t if OPTION was specified on the command line (OPTION is typically
a word beginning with `--'). If REQUIRES-ARGUMENT is non-nil, this option
requires a parameter, the value of which is returned. If a parameters isn't
supplied an error is signalled.
::end:: */
{
  rep_DECLARE1(opt, rep_STRINGP);

  repv param = Qt;
  if (rep_get_option(rep_STR(opt), arg == rep_nil ? 0 : &param)) {
    return param;
  }

  return rep_nil;
}

void
rep_environ_init(void)
{
  repv tem = rep_push_structure("rep.system");

  repv lst = rep_nil;
  if (environ) {
    for (char **ptr = environ; *ptr; ptr++) {
      lst = Fcons(rep_string_copy(*ptr), lst);
    }
  }

  rep_INTERN_SPECIAL(process_environment);
  Fset(Qprocess_environment, lst);

  rep_INTERN(rep_version);
  Fset(Qrep_version, rep_VAL(&rep_version_string));
  rep_INTERN(rep_interface_id);
  Fset(Qrep_interface_id, rep_VAL(rep_MAKE_INT(rep_INTERFACE)));
  rep_INTERN(rep_build_id);
  Fset(Qrep_build_id, rep_VAL(&build_id_string));

  rep_ADD_SUBR(Sget_command_line_option);
  rep_ADD_SUBR(Suser_login_name);
  rep_ADD_SUBR(Suser_full_name);
  rep_ADD_SUBR(Suser_home_directory);
  rep_ADD_SUBR(Ssystem_name);

  rep_pop_structure(tem);
}
