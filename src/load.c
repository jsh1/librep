/* load.c -- file loading

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

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

DEFSTRING(default_rep_directory, REP_DIRECTORY);
DEFSTRING(dot, ".");

static repv default_suffixes;

DEFSYM(load_path, "load-path");
DEFSYM(dl_load_path, "dl-load-path");
DEFSYM(after_load_alist, "after-load-alist");
DEFSYM(rep_directory, "rep-directory");
DEFSYM(lisp_lib_directory, "lisp-lib-directory");
DEFSYM(site_lisp_directory, "site-lisp-directory");
DEFSYM(exec_directory, "exec-directory");
DEFSYM(documentation_file, "documentation-file");
DEFSYM(documentation_files, "documentation-files");
DEFSYM(_load_suffixes, "%load-suffixes");
DEFSYM(dl_load_reloc_now, "dl-load-reloc-now");
DEFSYM(load_filename, "load-filename");

/* ::doc:load-path::
A list of directory names. When `load' opens a lisp-file it searches each
directory named in this list in turn until the file is found or the list
is exhausted.
::end::
::doc:dl-load-path::
List of directories searched for dynamically loaded object files.
::end::
::doc:after-load-alist::
A list of (LIBRARY FORMS...). Whenever the `load' command reads a file
of Lisp code LIBRARY, it executes each of FORMS. Note that LIBRARY must
exactly match the FILE argument given to `load'.
::end::
::doc:rep-directory::
The directory in which all installed data files live.
::end::
::doc:lisp-lib-directory::
The name of the directory in which the standard lisp files live.
::end::
::doc:site-lisp-directory::
The name of the directory in which site-specific Lisp files are stored.
::end::
::doc:exec-directory::
The name of the directory containing architecture specific files.
::end::
::doc:documentation-file::
The name of the database containing the lisp-library's documentation strings.
::end::
::doc:documentation-files::
A list of database names containing all documentation strings.
::end::
::doc:dl-load-reloc-now::
When non-nil, dynamically loaded libraries have all symbol relocations
perfromed at load-time, not as required.
::end::
::doc:load-filename::
While using the `load' function to load a Lisp library, this variable is
set to the name of the file being loaded.
::end:: */

static repv
file_exists_p(repv name)
{
  repv tem = Ffile_readable_p(name);
  if (tem && tem != rep_nil) {
    tem = Ffile_directory_p(name);
    if (tem) {
      return tem == rep_nil ? Qt : rep_nil;
    }
  }
  return tem;
}

static repv
load_lisp_file(repv name, repv structure)
{
  rep_GC_root gc_stream, gc_frame;

  rep_PUSHGC(gc_stream, name);
  rep_PUSHGC(gc_frame, structure);

  repv stream = Fopen_file(name, Qread);

  rep_POPGC; rep_POPGC;

  if (!stream || !rep_FILEP(stream)) {
    return 0;
  }

  repv frame = rep_bind_symbol(rep_EMPTY_BINDING_FRAME, Qload_filename, name);

  rep_PUSHGC(gc_stream, stream);
  rep_PUSHGC(gc_frame, frame);

  /* Create the lexical environment for the file. */

  rep_stack_frame lc;
  lc.fun = rep_nil;
  lc.args = rep_nil;
  rep_PUSH_CALL(lc);
  rep_env = rep_nil;
  rep_structure = structure;

  repv result = rep_nil;

  int c = rep_stream_getc(stream);
  while (c != EOF) {
    repv form = rep_readl(stream, &c);
    if (!form) {
      break;
    }
    result = rep_eval(form, false);
    if (!result) {
      break;
    }
    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      result = 0;
      goto out;
    }
  }

  if (rep_throw_value
      && rep_CAR(rep_throw_value) == Qerror
      && rep_CONSP(rep_CDR(rep_throw_value))
      && rep_CAR(rep_CDR(rep_throw_value)) == Qend_of_stream)
  {
    /* lose the end-of-stream error. */
    rep_throw_value = 0;
  }

out:
  rep_POP_CALL(lc);
  rep_POPGC; rep_POPGC;

  rep_PUSHGC(gc_stream, result);

  rep_unbind_symbols(frame);
  Fclose_file(stream);

  rep_POPGC;

  return result;
}

static repv
load_dl_file(repv name, repv structure)
{
  /* Create the lexical environment for the file. */

  rep_stack_frame lc;
  lc.fun = rep_nil;
  lc.args = rep_nil;
  rep_PUSH_CALL(lc);
  rep_env = rep_nil;
  rep_structure = structure;

  repv result;
#ifdef HAVE_DYNAMIC_LOADING
  result = rep_dl_open_structure(name);
#else
  result = Fsignal(Qerror, rep_LIST_1(rep_string_copy
    ("No support for dynamic loading of shared libraries")));
#endif

  rep_POP_CALL(lc);
  return result;
}

DEFUN_INT("load", Fload, Sload,
	  (repv file, repv noerr, repv nopath, repv nosuf, repv unused),
	  rep_Subr5, "fLisp file to load:") /*
::doc:rep.io.files#load::
load FILE [NO-ERROR] [NO-PATH] [NO-SUFFIX]

Attempt to open and then read-and-eval the file of Lisp code FILE.

For each directory named in the variable `load-path' tries the value of
FILE with `.jlc' (compiled-lisp) appended to it, then with `.jl'
appended to it, finally tries FILE without modification.

If NO-ERROR is non-nil no error is signalled if FILE can't be found. If
NO-PATH is non-nil the `load-path' variable is not used, just the value
of FILE. If NO-SUFFIX is non-nil no suffixes are appended to FILE.

If the compiled version is older than it's source code, the source code
is loaded and a warning is displayed.
::end:: */
{
  /* Avoid needing to protect these args from GC. */

  bool no_path = nopath != rep_nil;
  bool no_error = noerr != rep_nil;
  bool no_suffix = nosuf != rep_nil;
  bool interp_mode = Fsymbol_value(Qinterpreted_mode, Qt) != rep_nil;

  repv name = rep_nil, path;
  repv dir = 0, try = 0;
  repv result = 0;
  repv suffixes;
  bool trying_dl = false;

  rep_GC_root gc_file, gc_name, gc_path;
  rep_GC_root gc_dir, gc_try, gc_result, gc_suffixes;

  rep_DECLARE1(file, rep_STRINGP);

  if (!no_path) {
    path = Fsymbol_value(Qload_path, rep_nil);
    if (!path) {
      return 0;
    }
  } else {
    path = Fcons(rep_null_string(), rep_nil);
  }

  suffixes = F_structure_ref(rep_structure, Q_load_suffixes);
  if (!suffixes || !rep_CONSP(suffixes)) {
    suffixes = default_suffixes;
  }

  rep_PUSHGC(gc_name, name);
  rep_PUSHGC(gc_file, file);
  rep_PUSHGC(gc_path, path);
  rep_PUSHGC(gc_dir, dir);
  rep_PUSHGC(gc_try, try);
  rep_PUSHGC(gc_suffixes, suffixes);

  /* Scan the path for the file to load. */

search_again:
  while (name == rep_nil && rep_CONSP(path)) {
    if (rep_STRINGP(rep_CAR(path))) {
      dir = Fexpand_file_name(file, rep_CAR(path));
      if (dir == 0 || !rep_STRINGP(dir)) {
	goto path_error;
      }

      if (trying_dl || !no_suffix) {
	for (int i = 1; i >= 0; i--) {
#ifdef HAVE_DYNAMIC_LOADING
	  if (trying_dl) {
	    if (i == 1) {
	      try = rep_string_concat2(rep_STR(dir), ".la");
	    } else {
	      try = Fexpand_file_name(
			rep_string_concat3("lib", rep_STR(file), ".la"),
			rep_CAR(path));
	    }
	  } else
#endif
	  {
	    if (interp_mode && i > 0) {
	      continue;
	    }
	    repv sfx = (i == 0) ? rep_CAR(suffixes) : rep_CDR(suffixes);
	    if (rep_STRINGP(sfx)) {
	      try = rep_string_concat2(rep_STR(dir), rep_STR(sfx));
	    }
	  }

	  if (try && rep_STRINGP(try)) {
	    repv tem = file_exists_p(try);
	    if (!tem) {
	      goto path_error;
	    }
	    if (tem != rep_nil) {
	      if (name != rep_nil) {
		if (rep_file_newer_than(try, name)) {
		  if (rep_message_fun != 0) {
		    (*rep_message_fun)(rep_messagef,
				       "Warning: %s newer than %s, using %s",
				       rep_STR(try), rep_STR(name),
				       rep_STR(try));
		  }
		  name = try;
		}
	      } else {
		name = try;
	      }
	    }
	  }
	}
      }

      if (!trying_dl && name == rep_nil && no_suffix) {
	/* Try without a suffix */
	repv tem = file_exists_p(dir);
	if (!tem) {
	  goto path_error;
	}
	if (tem != rep_nil) {
	  name = dir;
	}
      }
    }

    path = rep_CDR(path);

    rep_TEST_INT;
    if(rep_INTERRUPTP) {
      goto path_error;
    }
  }

#ifdef HAVE_DYNAMIC_LOADING
  if (name == rep_nil && !trying_dl) {
    if (!no_path) {
      path = Fsymbol_value(Qdl_load_path, rep_nil);
      if (!path) {
	return 0;
      }
    } else {
      path = rep_LIST_1(rep_null_string());
    }
    trying_dl = true;
    goto search_again;
  }
#endif

path_error:
  rep_POPGC; rep_POPGC; rep_POPGC; rep_POPGC; rep_POPGC; rep_POPGC;

  if (name == rep_nil) {
    if (!no_error) {
      return rep_signal_file_error(file);
    } else {
      return rep_nil;
    }
  }

  rep_PUSHGC(gc_file, file);
#ifdef HAVE_DYNAMIC_LOADING
  if (trying_dl) {
    result = load_dl_file(name, rep_structure);
  } else
#endif
  {
    result = load_lisp_file(name, rep_structure);
  }
  rep_POPGC;
  if (result == 0) {
    return 0;
  }

  /* Loading succeeded. Look for an item in the after-load-alist. */

  if (rep_STRUCTUREP(result) && rep_STRUCTURE(result)->name != rep_nil) {
    /* Use the canonical name in case of aliasing.. */
    file = rep_SYM(rep_STRUCTURE(result)->name)->name;
  }

  rep_PUSHGC(gc_result, result);
  rep_PUSHGC(gc_file, file);
  {
    repv tem;
  after_load_again:
    tem = Fsymbol_value(Qafter_load_alist, Qt);
    if(tem != 0 && rep_CONSP(tem)) {
      tem = Fassoc(file, tem);
      if (tem != 0 && rep_CONSP(tem)) {
	/* Delete this entry.. */
	Fset(Qafter_load_alist,
	     Fdelq(tem, Fsymbol_value(Qafter_load_alist, Qt)));
	/* ..then call it */
	tem = rep_CDR(tem);
	while (rep_CONSP(tem) && !rep_INTERRUPTP) {
	  rep_GC_root gc_tem;
	  rep_PUSHGC(gc_tem, tem);
	  rep_call_lisp0(rep_CAR(tem));
	  rep_POPGC;
	  tem = rep_CDR(tem);
	}
	/* Look for another entry */
	goto after_load_again;
      }
    }
  }
  rep_POPGC;
  rep_POPGC;

  return result;
}

static void
add_path(const char *env, repv var)
{
  repv list = rep_nil;

  const char *ptr = getenv(env);
  while (ptr != 0 && *ptr != 0) {
    const char *end = strchr(ptr, ':');
    list = Fcons(end ? rep_string_copy_n(ptr, end - ptr)
		 : rep_string_copy(ptr), list);
    ptr = end ? end + 1 : 0;
  }

  repv vec[2];
  vec[0] = Fnreverse(list);
  vec[1] = Fsymbol_value(var, Qt);
  Fset(var, Fnconc(2, vec));
}

void
rep_load_init(void)
{
  rep_INTERN_SPECIAL(rep_directory);
  if (getenv("REPDIR")) {
    Fset(Qrep_directory, rep_string_copy(getenv("REPDIR")));
  } else {
    Fset(Qrep_directory, rep_VAL(&default_rep_directory));
  }
  
  rep_INTERN_SPECIAL(lisp_lib_directory);
  if (getenv("REPLISPDIR")) {
    Fset(Qlisp_lib_directory, rep_string_copy(getenv("REPLISPDIR")));
  } else {
    Fset(Qlisp_lib_directory, rep_string_copy(REP_LISP_DIRECTORY));
  }
  
  rep_INTERN_SPECIAL(site_lisp_directory);
  if (getenv("REPSITELISPDIR")) {
    Fset(Qsite_lisp_directory, rep_string_copy(getenv("REPSITELISPDIR")));
  } else {
    Fset(Qsite_lisp_directory,
	 rep_string_concat2(rep_STR(Fsymbol_value(Qrep_directory, Qt)),
			     "/site-lisp"));
  }

  rep_INTERN_SPECIAL(exec_directory);
  if (getenv("REPEXECDIR")) {
    Fset(Qexec_directory, rep_string_copy(getenv("REPEXECDIR")));
  } else {
    Fset(Qexec_directory, rep_string_copy(REP_EXEC_DIRECTORY));
  }
    
  rep_INTERN_SPECIAL(documentation_file);
  if (getenv("REPDOCFILE")) {
    Fset(Qdocumentation_file, rep_string_copy(getenv("REPDOCFILE")));
  } else {
    DEFSTRING(doc_file, REP_DOC_FILE);
    Fset(Qdocumentation_file, rep_VAL(&doc_file));
  }
    
  rep_INTERN_SPECIAL(documentation_files);
  Fset(Qdocumentation_files,
       Fcons(Fsymbol_value(Qdocumentation_file, Qt), rep_nil));
    
  rep_INTERN_SPECIAL(load_path);
  Fset(Qload_path, Fcons(Fsymbol_value(Qlisp_lib_directory, Qt),
			 Fcons(Fsymbol_value(Qsite_lisp_directory, Qt),
			       Fcons(rep_VAL(&dot), rep_nil))));
  add_path("REP_LOAD_PATH", Qload_path);
    
  DEFSTRING(common_exec, REP_COMMON_EXEC_DIRECTORY);

  rep_INTERN_SPECIAL(dl_load_path);
  Fset(Qdl_load_path, Fcons(Fsymbol_value(Qexec_directory, Qt),
			    Fcons(rep_VAL(&common_exec), rep_nil)));
  add_path("REP_DL_LOAD_PATH", Qdl_load_path);
    
  rep_INTERN_SPECIAL(after_load_alist);
  Fset(Qafter_load_alist, rep_nil);
    
  rep_INTERN_SPECIAL(dl_load_reloc_now);
  Fset(Qdl_load_reloc_now, rep_nil);
    
  rep_INTERN_SPECIAL(load_filename);
    
  DEFSTRING(jl, ".jl");
  DEFSTRING(jlc, ".jlc");

  default_suffixes = Fcons(rep_VAL(&jl), rep_VAL(&jlc));
  rep_mark_static(&default_suffixes);
  rep_INTERN(_load_suffixes);

  repv tem = rep_push_structure("rep.io.files");
  rep_ADD_SUBR_INT(Sload);
  rep_pop_structure(tem);
}
