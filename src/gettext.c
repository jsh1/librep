/* gettext.c -- wrap some i18n functions when available

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

#ifdef HAVE_LIBINTL_H
# include <libintl.h>
#endif

DEFUN("gettext", Fgettext, Sgettext, (repv in), rep_Subr1)
{
  rep_DECLARE1(in, rep_STRINGP);

#ifdef HAVE_LIBINTL_H
  char *out = gettext(rep_STR(in));
  if (!out || out == rep_STR(in)) {
    return in;
  } else {
    return rep_string_copy(out);
  }
#else
  return in;
#endif
}

DEFUN("bindtextdomain", Fbindtextdomain,
      Sbindtextdomain, (repv dom, repv dir), rep_Subr2)
{
#ifdef HAVE_LIBINTL_H
  const char *domainname = NULL;
  if (rep_STRINGP(dom)) {
    domainname = rep_STR(dom);
  }

  const char *dirname = NULL;
  if (rep_STRINGP(dir)) {
    dirname = rep_STR(dir);
  }

  const char *out = bindtextdomain(domainname, dirname);

  return out ? rep_string_copy(out) : rep_nil;
#else
  return rep_nil;
#endif
}

DEFUN("bindtextdomaincodeset", Fbindtextdomaincodeset,
      Sbindtextdomaincodeset, (repv dom, repv cod), rep_Subr2)
{
#ifdef HAVE_LIBINTL_H
  const char *domainname = NULL;
  if (rep_STRINGP(dom)) {
    domainname = rep_STR(dom);
  }

  const char *codeset = NULL;
  if (rep_STRINGP(cod)) {
    codeset = rep_STR(cod);
  }

  const char *out = bind_textdomain_codeset(domainname, codeset);

  return out ? rep_string_copy(out) : rep_nil;
#else
  return rep_nil;
#endif
}

DEFUN("textdomain", Ftextdomain, Stextdomain, (repv dom), rep_Subr1)
{
#ifdef HAVE_LIBINTL_H
  const char *domainname = NULL;
  if (rep_STRINGP(dom)) {
    domainname = rep_STR(dom);
  }

  const char *out = textdomain(domainname);

  return out ? rep_string_copy(out) : rep_nil;
#else
  return rep_nil;
#endif
}

repv
rep_dl_init(void)
{
  DEFSTRING(underscore, "_");

  repv tem = rep_push_structure("rep.i18n.gettext"), ret;
  rep_ADD_SUBR(Sgettext);
  rep_ADD_SUBR(Sbindtextdomain);
  rep_ADD_SUBR(Sbindtextdomaincodeset);
  rep_ADD_SUBR(Stextdomain);
  ret = rep_pop_structure(tem);

  /* Update binding of `_' in `rep' structure to point at the gettext
     function */

  tem = rep_push_structure("rep");
  Fset(Fintern(rep_VAL(&underscore), rep_nil), rep_VAL(&Sgettext));
  rep_pop_structure(tem);

  return ret;
}
