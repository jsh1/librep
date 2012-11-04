/* gettext.c -- wrap some i18n functions when available
   Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>
   $Id$

   This file is part of librep.

   librep is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   librep is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with librep; see the file COPYING.	If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#define _GNU_SOURCE

#include <config.h>
#include <rep.h>

#ifdef HAVE_LIBINTL_H
# include <libintl.h>
#endif

DEFUN("gettext", Fgettext, Sgettext, (repv in), rep_Subr1)
{
    char *out;
    rep_DECLARE1(in, rep_STRINGP);

    out = gettext (rep_STR(in));
    if (out == 0 || (char *) out == rep_STR(in))
	return in;
    else
	return rep_string_dup (out);
}

DEFUN("bindtextdomain", Fbindtextdomain,
      Sbindtextdomain, (repv dom, repv dir), rep_Subr2)
{
    char *domainname = 0, *dirname = 0, *out;

    if (rep_STRINGP(dom))
	domainname = rep_STR(dom);
    if (rep_STRINGP(dir))
	dirname = rep_STR(dir);

    out = bindtextdomain (domainname, dirname);
    return out ? rep_string_dup (out) : Qnil;
}


DEFUN("bindtextdomaincodeset", Fbindtextdomaincodeset,
      Sbindtextdomaincodeset, (repv dom, repv cod), rep_Subr2)
{
    char *domainname = 0, *codeset = 0, *out;

    if (rep_STRINGP(dom))
	domainname = rep_STR(dom);
    if (rep_STRINGP(cod))
        codeset = rep_STR(cod);

    out = bind_textdomain_codeset (domainname, codeset);

    return out ? rep_string_dup (out) : Qnil;
}


DEFUN("textdomain", Ftextdomain, Stextdomain, (repv dom), rep_Subr1)
{
    char *domainname = 0, *out;

    if (rep_STRINGP(dom))
	domainname = rep_STR(dom);

    out = textdomain (domainname);
    return out ? rep_string_dup (out) : Qnil;
}



/* DL hooks */

DEFSTRING(underscore, "_");

repv
rep_dl_init(void)
{
    repv tem = rep_push_structure ("rep.i18n.gettext"), ret;
    /* ::alias:gettext rep.i18n.gettext:: */
    rep_alias_structure ("gettext");
    rep_ADD_SUBR(Sgettext);
    rep_ADD_SUBR(Sbindtextdomain);
    rep_ADD_SUBR(Sbindtextdomaincodeset);
    rep_ADD_SUBR(Stextdomain);
    ret = rep_pop_structure (tem);

    /* Update binding of `_' in `rep' structure to point at the
       gettext function */
    tem = rep_push_structure ("rep");
    Fset (Fintern (rep_VAL (&underscore), Qnil), rep_VAL (&Sgettext));
    rep_pop_structure (tem);

    return ret;
}
