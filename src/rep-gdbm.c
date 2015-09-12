/* rep-gdbm.c -- rep wrapper to libgdbm

   Copyright (C) 1993-2015 John Harper <jsh@unfactored.org>

   This file is part of Librep.

   Librep is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Librep is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Librep; see the file COPYING.  If not, write to the Free
   Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "rep.h"

#include <gdbm.h>
#include <fcntl.h>

typedef struct rep_dbm_struct rep_dbm;

struct rep_dbm_struct {
  repv car;
  rep_dbm *next;
  GDBM_FILE dbm;
  repv path;
  repv access;
  repv mode;
};

static repv dbm_type;
static rep_dbm *dbm_list;

#define rep_DBM(v)  ((rep_dbm *)rep_PTR(v))
#define rep_DBMP(v) (rep_CELL16_TYPEP(v, dbm_type) && rep_DBM(v)->dbm != 0)

DEFSYM(insert, "insert");
DEFSYM(replace, "replace");
DEFSYM(no_lock, "no-lock");

DEFUN("gdbm-open", Fgdbm_open, Sgdbm_open,
      (repv file, repv type, repv mode, repv flags), rep_Subr4) /*
::doc:rep.io.db.gdbm#gdbm-open::
gdbm-open PATH ACCESS-TYPE [MODE] [FLAGS]
::end:: */
{
  rep_DECLARE1(file, rep_STRINGP);
  rep_DECLARE2(type, rep_SYMBOLP);

  rep_GC_root gc_type, gc_mode;
  rep_PUSHGC(gc_type, type);
  rep_PUSHGC(gc_mode, mode);

  file = Flocal_file_name(file);

  rep_POPGC; rep_POPGC;

  if (!file) {
    return 0;
  }

  int uflags = type == Qwrite ? GDBM_NEWDB
    : type == Qappend ? GDBM_WRCREAT : GDBM_READER;

  if (rep_CONSP(flags) && rep_CAR(flags) == Qno_lock) {
#ifdef GDBM_NOLOCK
    uflags |= GDBM_NOLOCK;
#endif
  }

  int umode = rep_INTP(mode) ? rep_INT(mode) : 0666;

  rep_dbm *dbm = rep_alloc(sizeof(rep_dbm));

  dbm->car = dbm_type;
  dbm->path = file;
  dbm->access = type;
  dbm->mode = rep_MAKE_INT(umode);
  dbm->dbm = gdbm_open(rep_STR(file), 0, uflags, umode, 0);

  if (!dbm->dbm) {
    rep_free(dbm);
    return rep_signal_file_error(file);
  }

  dbm->next = dbm_list;
  dbm_list = dbm;

  rep_data_after_gc += sizeof(rep_dbm);

  return rep_VAL(dbm);
}

DEFUN("gdbm-close", Fgdbm_close, Sgdbm_close, (repv dbm), rep_Subr1) /*
::doc:rep.io.db.gdbm#gdbm-close::
gdbm-close DBM
::end:: */
{
  rep_DECLARE1(dbm, rep_DBMP);

  gdbm_close(rep_DBM(dbm)->dbm);
  rep_DBM(dbm)->dbm = NULL;

  rep_DBM(dbm)->path = rep_nil;
  rep_DBM(dbm)->access = rep_nil;
  rep_DBM(dbm)->mode = rep_nil;

  return Qt;
}

DEFUN("gdbm-fetch", Fgdbm_fetch, Sgdbm_fetch,
      (repv dbm, repv key), rep_Subr2) /*
::doc:rep.io.db.gdbm#gdbm-fetch::
gdbm-fetch DBM KEY
::end:: */
{
  rep_DECLARE1(dbm, rep_DBMP);
  rep_DECLARE2(key, rep_STRINGP);

  datum dkey;
  dkey.dptr = rep_STR(key);
  dkey.dsize = rep_STRING_LEN(key);

  datum dvalue = gdbm_fetch(rep_DBM(dbm)->dbm, dkey);

  if (!dvalue.dptr) {
    return rep_nil;
  }

  /* The string isn't always zero-terminated, so need to copy it. */

  repv out = rep_string_copy_n(dvalue.dptr, dvalue.dsize);
  free(dvalue.dptr);
  return out;
}

DEFUN("gdbm-store", Fgdbm_store, Sgdbm_store,
      (repv dbm, repv key, repv val, repv flags), rep_Subr4) /*
::doc:rep.io.db.gdbm#gdbm-store::
gdbm-store DBM KEY VALUE [FLAGS]
::end:: */
{
  rep_DECLARE1(dbm, rep_DBMP);
  rep_DECLARE2(key, rep_STRINGP);
  rep_DECLARE3(val, rep_STRINGP);

  datum dkey;
  dkey.dptr = rep_STR(key);
  dkey.dsize = rep_STRING_LEN(key);

  datum dvalue;
  dvalue.dptr = rep_STR(val);
  dvalue.dsize = rep_STRING_LEN(val);

  int dflags = flags == Qinsert ? GDBM_INSERT : GDBM_REPLACE;

  return !gdbm_store(rep_DBM(dbm)->dbm, dkey, dvalue, dflags) ? Qt : rep_nil;
}

DEFUN("gdbm-delete", Fgdbm_delete, Sgdbm_delete,
      (repv dbm, repv key), rep_Subr2) /*
::doc:rep.io.db.gdbm#gdbm-delete::
gdbm-delete DBM KEY
::end:: */
{
  rep_DECLARE1(dbm, rep_DBMP);
  rep_DECLARE2(key, rep_STRINGP);

  datum dkey;
  dkey.dptr = rep_STR(key);
  dkey.dsize = rep_STRING_LEN(key);

  return gdbm_delete(rep_DBM(dbm)->dbm, dkey) == 0 ? Qt : rep_nil;
}

DEFUN("gdbm-walk", Fgdbm_walk, Sgdbm_walk,
      (repv fun, repv dbm), rep_Subr2) /*
::doc:rep.io.db.gdbm#gdbm-walk::
gdbm-walk FUN DBM
::end:: */
{
  rep_TEST_INT_LOOP_COUNTER;

  rep_DECLARE1(dbm, rep_DBMP);

  rep_GC_root gc_dbm, gc_fun;
  rep_PUSHGC(gc_dbm, dbm);
  rep_PUSHGC(gc_fun, fun);

  repv ret = rep_nil;

  datum dkey;
  dkey = gdbm_firstkey(rep_DBM(dbm)->dbm);

  while (ret && dkey.dptr && !rep_INTERRUPTP) {
    if (!rep_call_lisp1(fun, rep_string_copy_n(dkey.dptr, dkey.dsize))) {
      ret = 0;
      free(dkey.dptr);
      break;
    }
    dkey = gdbm_nextkey(rep_DBM(dbm)->dbm, dkey);
    rep_TEST_INT;
  }

  rep_POPGC; rep_POPGC;
  return ret;
}

DEFUN("gdbm?", Fgdbmp, Sgdbmp, (repv arg), rep_Subr1) /*
::doc:rep.io.db.gdbm#gdbm?::
gdbm? ARG

Returns t if ARG is an gdbm object (created by `gdbm-open').
::end:: */
{
  return rep_DBMP(arg) ? Qt : rep_nil;
}

static void
dbm_mark(repv val)
{
  rep_MARKVAL(rep_DBM(val)->path);
  rep_MARKVAL(rep_DBM(val)->access);
  rep_MARKVAL(rep_DBM(val)->mode);
}

static void
dbm_sweep(void)
{
  rep_dbm *ptr = dbm_list;
  dbm_list = 0;

  while (ptr) {
    rep_dbm *next = ptr->next;
    if (!rep_GC_CELL_MARKEDP(rep_VAL(ptr))) {
      if (ptr->dbm) {
	gdbm_close(ptr->dbm);
      }
      rep_free(ptr);
    } else {
      rep_GC_CLR_CELL(rep_VAL(ptr));
      ptr->next = dbm_list;
      dbm_list = ptr;
    }
    ptr = next;
  }
}

static void
dbm_print(repv stream, repv dbm)
{
  rep_stream_puts(stream, "#<dbm ", -1, false);
  if (rep_STRINGP(rep_DBM(dbm)->path)) {
    rep_stream_puts(stream, rep_PTR(rep_DBM(dbm)->path), -1, true);
  } else {
    rep_stream_puts(stream, "nil", -1, false);
  }
  rep_stream_putc(stream, '>');
}

repv
rep_dl_init(void)
{
  static rep_type gdbm = {
    .name = "gdbm",
    .print = dbm_print,
    .sweep = dbm_sweep,
    .mark = dbm_mark,
  };

  dbm_type = rep_define_type(&gdbm);

  rep_INTERN(insert);
  rep_INTERN(replace);
  rep_INTERN(no_lock);

  repv tem = rep_push_structure("rep.io.db.gdbm");
  rep_ADD_SUBR(Sgdbm_open);
  rep_ADD_SUBR(Sgdbm_close);
  rep_ADD_SUBR(Sgdbm_fetch);
  rep_ADD_SUBR(Sgdbm_store);
  rep_ADD_SUBR(Sgdbm_delete);
  rep_ADD_SUBR(Sgdbm_walk);
  rep_ADD_SUBR(Sgdbmp);
  return rep_pop_structure(tem);
}

void
rep_dl_kill(void)
{
  for (rep_dbm *db = dbm_list; db != 0; db = db->next) {
    if (db->dbm) {
      Fgdbm_close(rep_VAL(db));
    }
  }
}
