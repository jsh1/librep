/* rep-sdbm.c -- rep wrapper to libsdbm

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

#include "sdbm.h"
#include <fcntl.h>

typedef struct rep_dbm_struct rep_dbm;

struct rep_dbm_struct {
  repv car;
  struct rep_dbm_struct *next;
  SDBM *dbm;
  repv path;
  repv access;
  repv mode;
};

static int dbm_type;
static rep_dbm *dbm_list;

#define rep_DBM(v)  ((rep_dbm *) rep_PTR(v))
#define rep_DBMP(v) (rep_CELL16_TYPEP(v, dbm_type) && rep_DBM(v)->dbm != 0)

DEFSYM(insert, "insert");
DEFSYM(replace, "replace");

DEFUN("sdbm-open", Fsdbm_open, Ssdbm_open, (repv file, repv flags, repv mode),
      rep_Subr3) /*
::doc:rep.io.db.sdbm#sdbm-open::
sdbm-open PATH ACCESS-TYPE [MODE]
::end:: */
{
  rep_DECLARE1(file, rep_STRINGP);
  rep_DECLARE2(flags, rep_SYMBOLP);

  rep_GC_root gc_flags, gc_mode;
  rep_PUSHGC(gc_flags, flags);
  rep_PUSHGC(gc_mode, mode);

  file = Flocal_file_name (file);

  rep_POPGC; rep_POPGC;

  if (!file) {
    return 0;
  }

  int uflags = flags == Qwrite ? O_RDWR | O_CREAT | O_TRUNC
    : flags == Qappend ? O_RDWR | O_CREAT : O_RDONLY;

  int umode = rep_INTP(mode) ? rep_INT(mode) : 0666;

  rep_dbm *dbm = rep_alloc (sizeof (rep_dbm));

  dbm->car = dbm_type;
  dbm->path = file;
  dbm->access = flags;
  dbm->mode = rep_MAKE_INT(umode);
  dbm->dbm = sdbm_open (rep_STR(file), uflags, umode);

  if (!dbm->dbm) {
    rep_free(dbm);
    return rep_signal_file_error (file);
  }

  dbm->next = dbm_list;
  dbm_list = dbm;

  rep_data_after_gc += sizeof (rep_dbm);

  return rep_VAL(dbm);
}

DEFUN("sdbm-close", Fsdbm_close, Ssdbm_close, (repv dbm), rep_Subr1) /*
::doc:rep.io.db.sdbm#sdbm-close::
sdbm-close DBM
::end:: */
{
  rep_DECLARE1(dbm, rep_DBMP);

  sdbm_close(rep_DBM(dbm)->dbm);
  rep_DBM(dbm)->dbm = 0;

  rep_DBM(dbm)->path = rep_nil;
  rep_DBM(dbm)->access = rep_nil;
  rep_DBM(dbm)->mode = rep_nil;

  return Qt;
}

DEFUN("sdbm-fetch", Fsdbm_fetch, Ssdbm_fetch,
      (repv dbm, repv key), rep_Subr2) /*
::doc:rep.io.db.sdbm#sdbm-fetch::
sdbm-fetch DBM KEY
::end:: */
{
  rep_DECLARE1(dbm, rep_DBMP);
  rep_DECLARE2(key, rep_STRINGP);

  datum dkey;
  dkey.dptr = rep_STR(key);
  dkey.dsize = rep_STRING_LEN(key);

  datum dvalue = sdbm_fetch(rep_DBM(dbm)->dbm, dkey);

  if (!dvalue.dptr) {
    return rep_nil;
  }

  return rep_string_copy_n(dvalue.dptr, dvalue.dsize);
}

DEFUN("sdbm-store", Fsdbm_store, Ssdbm_store,
      (repv dbm, repv key, repv val, repv flags), rep_Subr4) /*
::doc:rep.io.db.sdbm#sdbm-store::
sdbm-store DBM KEY VALUE [FLAGS]
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

  int dflags = flags == Qinsert ? SDBM_INSERT : SDBM_REPLACE;

  return !sdbm_store(rep_DBM(dbm)->dbm, dkey, dvalue, dflags) ? Qt : rep_nil;
}

DEFUN("sdbm-delete", Fsdbm_delete, Ssdbm_delete,
      (repv dbm, repv key), rep_Subr2) /*
::doc:rep.io.db.sdbm#sdbm-delete::
sdbm-delete DBM KEY
::end:: */
{
  rep_DECLARE1(dbm, rep_DBMP);
  rep_DECLARE2(key, rep_STRINGP);

  datum dkey;
  dkey.dptr = rep_STR(key);
  dkey.dsize = rep_STRING_LEN(key) + 1;

  return !sdbm_delete(rep_DBM(dbm)->dbm, dkey) ? Qt : rep_nil;
}

DEFUN("sdbm-firstkey", Fsdbm_firstkey,
      Ssdbm_firstkey, (repv dbm), rep_Subr1) /*
::doc:rep.io.db.sdbm#sdbm-firstkey::
sdbm-firstkey DBM
::end:: */
{
  rep_DECLARE1(dbm, rep_DBMP);

  datum dkey = sdbm_firstkey (rep_DBM(dbm)->dbm);
  if (!dkey.dptr) {
    return rep_nil;
  }

  return rep_string_copy_n(dkey.dptr, dkey.dsize);
}

DEFUN("sdbm-nextkey", Fsdbm_nextkey, Ssdbm_nextkey, (repv dbm), rep_Subr1) /*
::doc:rep.io.db.sdbm#sdbm-nextkey::
sdbm-nextkey DBM
::end:: */
{
  rep_DECLARE1(dbm, rep_DBMP);

  datum dkey = sdbm_nextkey(rep_DBM(dbm)->dbm);

  if (!dkey.dptr) {
    return rep_nil;
  }

  return rep_string_copy_n(dkey.dptr, dkey.dsize);
}

DEFUN("sdbm-rdonly", Fsdbm_rdonly, Ssdbm_rdonly, (repv dbm), rep_Subr1) /*
::doc:rep.io.db.sdbm#sdbm-rdonly::
sdbm-rdonly DBM
::end:: */
{
  rep_DECLARE1(dbm, rep_DBMP);

  return sdbm_rdonly(rep_DBM(dbm)->dbm) ? Qt : rep_nil;
}

DEFUN("sdbm-error", Fsdbm_error, Ssdbm_error, (repv dbm), rep_Subr1) /*
::doc:rep.io.db.sdbm#sdbm-error::
sdbm-error DBM
::end:: */
{
  rep_DECLARE1(dbm, rep_DBMP);

  return sdbm_error(rep_DBM(dbm)->dbm) ? Qt : rep_nil;
}

DEFUN("sdbmp", Fsdbmp, Ssdbmp, (repv arg), rep_Subr1) /*
::doc:rep.io.db.sdbm#sdbmp::
sdbmp ARG

Returns t if ARG is an sdbm object (created by `sdbm-open').
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
      if (ptr->dbm != 0) {
	sdbm_close(ptr->dbm);
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
rep_dl_init (void)
{
  static rep_type sdbm = {
    .name = "sdbm",
    .print = dbm_print,
    .sweep = dbm_sweep,
    .mark = dbm_mark,
  };

  dbm_type = rep_define_type(&sdbm);

  rep_INTERN(insert);
  rep_INTERN(replace);

  repv tem = rep_push_structure("rep.io.db.sdbm");
  rep_ADD_SUBR(Ssdbm_open);
  rep_ADD_SUBR(Ssdbm_close);
  rep_ADD_SUBR(Ssdbm_fetch);
  rep_ADD_SUBR(Ssdbm_store);
  rep_ADD_SUBR(Ssdbm_delete);
  rep_ADD_SUBR(Ssdbm_firstkey);
  rep_ADD_SUBR(Ssdbm_nextkey);
  rep_ADD_SUBR(Ssdbm_rdonly);
  rep_ADD_SUBR(Ssdbm_error);
  rep_ADD_SUBR(Ssdbmp);
  return rep_pop_structure(tem);
}

void
rep_dl_kill(void)
{
  rep_dbm *ptr;
  for (ptr = dbm_list; ptr; ptr = ptr->next) {
    if (ptr->dbm != 0) {
      Fsdbm_close(rep_VAL (ptr));
    }
  }
}
