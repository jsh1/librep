/* rep.c -- read-eval-print front end

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

#include "rep.h"

DEFSTRING(rep_user, "rep/user");

int
main(int argc, char **argv)
{
  char *prog_name = *argv++;
  argc--;

  rep_init(prog_name, &argc, &argv);

  rep_load_environment(rep_VAL(&rep_user));

  return rep_top_level_exit();
}
