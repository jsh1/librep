/* plists.c -- symbol property lists

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

/* Plist storage */

static repv plist_structure;

DEFSYM(_plists, "%plists");

DEFUN("setplist", Fsetplist, Ssetplist, (repv sym, repv prop), rep_Subr2) /*
::doc:rep.lang.symbols#setplist::
setplist SYMBOL PROP-LIST

Sets the property list of SYMBOL to PROP-LIST, returns PROP-LIST.
::end:: */
{
  rep_DECLARE1(sym, rep_SYMBOLP);

  if (!rep_special_variable_accessible_p(sym)) {
    return Fsignal(Qvoid_value, rep_LIST_1(sym));
  }

  Fstructure_define(plist_structure, sym, prop);
  return prop;
}

DEFUN("symbol-plist", Fsymbol_plist, Ssymbol_plist, (repv sym), rep_Subr1) /*
::doc:rep.lang.symbols#symbol-plist::
symbol-plist SYMBOL

Returns the property-list of SYMBOL.
::end:: */
{
  rep_DECLARE1(sym, rep_SYMBOLP);

  if (!rep_special_variable_accessible_p(sym)) {
    return Fsignal(Qvoid_value, rep_LIST_1(sym));
  }

  repv plist = F_structure_ref(plist_structure, sym);
  return rep_VOIDP(plist) ? rep_nil : plist;
}

DEFUN("get", Fget, Sget, (repv sym, repv prop), rep_Subr2) /*
::doc:rep.lang.symbols#get::
get SYMBOL PROPERTY

Returns the value of SYMBOL's property PROPERTY. See `put'.
::end:: */
{
  rep_DECLARE1(sym, rep_SYMBOLP);

  repv plist = F_structure_ref(plist_structure, sym);
  if (rep_VOIDP(plist)) {
    return rep_nil;
  }

  while (rep_CONSP(plist) && rep_CONSP(rep_CDR(plist))) {
    if (rep_CAR(plist) == prop
	|| (!rep_SYMBOLP(prop)
	    && rep_value_cmp(rep_CAR(plist), prop) == 0))
    {
      return rep_CAR(rep_CDR(plist));
    }
    plist = rep_CDR(rep_CDR(plist));
  }

  return rep_nil;
}

DEFUN("put", Fput, Sput, (repv sym, repv prop, repv val), rep_Subr3) /*
::doc:rep.lang.symbols#put::
put SYMBOL PROPERTY VALUE

Sets the value of SYMBOL's property PROPERTY to VALUE, this value can be
retrieved with the `get' function.
::end:: */
{
  rep_DECLARE1(sym, rep_SYMBOLP);

  if (!rep_special_variable_accessible_p(sym)) {
    return Fsignal(Qvoid_value, rep_LIST_1(sym));
  }

  repv old = F_structure_ref(plist_structure, sym);
  if (rep_VOIDP(old)) {
    old = rep_nil;
  }

  repv plist = old;
  while (rep_CONSP(plist) && rep_CONSP(rep_CDR(plist))) {
    if (rep_CAR(plist) == prop
	|| (!rep_SYMBOLP(prop)
	    && rep_value_cmp(rep_CAR(plist), prop) == 0))
    {
      rep_CAR(rep_CDR(plist)) = val;
      return val;
    }
    plist = rep_CDR(rep_CDR(plist));
  }

  Fstructure_define(plist_structure, sym, Fcons(prop, Fcons(val, old)));
  return val;
}

void
rep_plists_init(void)
{
  plist_structure = Fmake_structure(rep_nil, rep_nil, rep_nil, rep_nil);
  rep_mark_static(&plist_structure);
  rep_INTERN(_plists);
  Fset_structure_name(plist_structure, Q_plists);
  
  repv tem = rep_push_structure("rep.lang.symbols");
  rep_ADD_SUBR(Ssetplist);
  rep_ADD_SUBR(Ssymbol_plist);
  rep_ADD_SUBR(Sget);
  rep_ADD_SUBR(Sput);
  rep_pop_structure(tem);
}
