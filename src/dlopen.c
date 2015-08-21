/* dlopen.c -- Dynamic loading of C modules

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

#include <assert.h>
#include <string.h>

/* We define some extensions to the libtool .la file. As well as using
   the dlname entry to find the .so file to open, we also look for:

     rep_open_globally=[yes|no]

	whether or not to open with RTLD_GLOBAL

     rep_requires='FEATURES...'

	FEATURES is space separated list of feature symbols.
	Each of which must be provided by a dl object. */

#ifdef HAVE_DYNAMIC_LOADING

#if defined(HAVE_DLFCN_H)

# include <dlfcn.h>
# if !defined(RTLD_LAZY)
#  if defined(DL_LAZY)
#   define RTLD_LAZY DL_LAZY
#  else
    /* from gmodule-dl.c ``The Perl sources say, RTLD_LAZY needs to be
       defined as (1), at least for Solaris 1.'' */
#   define RTLD_LAZY 1
#  endif
# endif
# if !defined(RTLD_GLOBAL)
#  if defined(DL_GLOBAL)
#   define RTLD_GLOBAL DL_GLOBAL
#  else
#   define RTLD_GLOBAL 0
#  endif
# endif
# if !defined(RTLD_LOCAL)
#  if defined(DL_LOCAL)
#   define RTLD_LOCAL DL_LOCAL
#  else
#   define RTLD_LOCAL 0
#  endif
# endif
# if !defined(RTLD_NOW)
#  if defined(DL_NOW)
#   define RTLD_NOW DL_NOW
#  else
#   define RTLD_NOW 0
#  endif
# endif
# if !defined(RTLD_DEFAULT)
#  define RTLD_DEFAULT NULL
# endif
# if defined(BROKEN_RTLD_GLOBAL)
#  undef RTLD_GLOBAL
#  define RTLD_GLOBAL 0
# endif

#elif defined(HAVE_DL_H) || defined(HAVE_SYS_DL_H)

# if defined(HAVE_DL_H)
#  include <dl.h>
# else
#  include <sys/dl.h>
# endif
# if !defined(BIND_IMMEDIATE)
#  define BIND_IMMEDIATE 0
# endif
# if !defined(BIND_DEFERRED)
#  define BIND_DEFERRED 0
# endif
# if !defined(BIND_NONFATAL)
#  define BIND_NONFATAL 0
# endif
# if !defined(DYNAMIC_PATH)
#  define DYNAMIC_PATH 0
# endif

#endif

typedef struct rep_dl_module_struct rep_dl_module;

struct rep_dl_module_struct {
  repv file_name;
  repv feature_sym;
  repv structure;
  void *handle;
  bool is_rep_module;
};

static size_t dl_module_count, dl_module_capacity;
static rep_dl_module *dl_modules;

#if !defined(HAVE_DLOPEN) && defined(HAVE_SHL_LOAD)

static inline void *
dlsym(void *handle, char *sym)
{
  void *addr;
  if (shl_findsym(&handle, sym, TYPE_UNDEFINED, &addr) == 0) {
    return addr;
  } else {
    return NULL;
  }
}

static inline void
dlclose(void *handle)
{
  shl_unload(handle);
}

#endif /* !HAVE_DLOPEN && HAVE_SHL_LOAD */

#if !defined(DLSYM_NEED_USCORE)

# define x_dlsym dlsym

#else /* !DLSYM_NEED_USCORE */

static void *
x_dlsym(void *handle, char *sym)
{
  size_t buf_len = strlen(sym) + 2;
  char *buf = rep_stack_alloc(char, buf_len);

  if (!buf) {
    return NULL;
  }

  buf[0] = '_';
  strcpy(buf + 1, sym);

  void *ptr = dlsym(handle, buf);

  rep_stack_free(char, buf_len, buf);

  return ptr;
}

#endif /* DLSYM_NEED_USCORE */

static void
x_dlclose(void *handle)
{
  if (false) {
    /* Closing libraries is a _bad_ idea. There's no way of knowing if
       any pointers to their contents exist. For example, some libraries
       install an atexit() handler.. */

    dlclose(handle);
  }
}

static int
find_dl_module(repv file)
{
  assert(rep_STRINGP(file));

  for (int i = 0; i < dl_module_count; i++) {
    assert(rep_STRINGP(dl_modules[i].file_name));
    if (!strcmp(rep_STR(file), rep_STR(dl_modules[i].file_name))) {
      return i;
    }
  }

  return -1;
}

static int
find_dl_module_by_feature(repv feature)
{
  assert(rep_STRINGP(feature));

  for (int i = 0; i < dl_module_count; i++) {
    repv sym = dl_modules[i].feature_sym;
    if (rep_SYMBOLP(sym)) {
      repv name = rep_SYM(sym)->name;
      if (strcmp(rep_STR(name), rep_STR(feature)) == 0) {
	return i;
      }
    }
  }

  return -1;
}

static bool
load_requires(const char *ptr)
{
  ptr += strspn(ptr, " \t");

  while (*ptr != 0) {
    const char *end = ptr + strcspn(ptr, " \t");
    repv sym = Fintern(rep_string_copy_n(ptr, end - ptr), rep_nil);
    if (!Fintern_structure(sym)) {
      return false;
    }
    ptr = end + strspn(end, " \t");
  }

  return true;
}

static void
signal_error(const char *msg)
{
  if (Qerror != 0) {
    Fsignal(Qerror, rep_LIST_1(rep_string_copy(msg)));
  } else {
    fprintf(stderr, "error: %s\n", msg);
  }
}

int
rep_dl_intern_library(repv file_name)
{
  int idx = find_dl_module(file_name);
  if (idx >= 0) {
    return idx;
  }

  const char *dlname = 0;
  bool open_globally = false;
  bool is_rep_module = true;

  const char *tem = rep_STR(file_name);
  size_t len = strlen(tem);

  if (len >= 3 && strcmp(tem + len - 3, ".la") == 0) {
    /* We're trying to open a _libtool_ dl object. i.e it's a file
       ending in .la that contains a dlname=FOO line pointing to the
       actual DL object(in the same directory). */

    FILE *fh = fopen(rep_STR(file_name), "r");
    if (!fh) {
      rep_signal_file_error(file_name);
      return -1;
    }

    char buf[256];
    while (fgets(buf, sizeof(buf), fh)) {
      if (strncmp("dlname='", buf, sizeof("dlname='") - 1) == 0) {
	char *ptr = buf + sizeof("dlname='") - 1;
	char *end = strchr(ptr, '\'');
	if (end && end > ptr) {
	  *end = 0;
	  const char *base = strrchr(rep_STR(file_name), '/');
	  char *name;
	  if (!base) {
	    name = alloca(strlen(ptr) + 1);
	    strcpy(name, ptr);
	  } else {
	    base++;
	    name = alloca(strlen(ptr) + base - rep_STR(file_name) + 1);
	    memcpy(name, rep_STR(file_name), base - rep_STR(file_name));
	    strcpy(name + (base - rep_STR(file_name)), ptr);
	  }
	  dlname = name;
	}
      }
      else if (strncmp("rep_open_globally=", buf,
		       sizeof("rep_open_globally=") - 1) == 0)
      {
	char *ptr = buf + sizeof("rep_open_globally=") - 1;
	if (strncmp("yes", ptr, 3) == 0) {
	  open_globally = true;
	}
      }
      else if (strncmp("rep_requires='", buf,
		       sizeof("rep_requires='") - 1) == 0)
      {
	char *ptr = buf + sizeof("rep_requires='") - 1;
	char *end = strchr(ptr, '\'');
	if (end) {
	  char *string = alloca(end - ptr + 1);
	  memcpy(string, ptr, end - ptr);
	  string[end - ptr] = 0;
	  rep_GC_root gc_file_name;
	  rep_PUSHGC(gc_file_name, file_name);
	  bool success = load_requires(string);
	  rep_POPGC;
	  if (!success) {
	    return -1;
	  }
	}
      }
    }

    fclose(fh);

  } else {
    /* not .la, assume a native library name */

    dlname = rep_STR(file_name);
    is_rep_module = false;
  }

  if (!dlname) {
    char err[256];
#ifdef HAVE_SNPRINTF
    snprintf(err, sizeof(err), "Can't find dlname in %s", rep_STR(file_name));
#else
    sprintf(err, "Can't find dlname in %s", rep_STR(file_name));
#endif
    signal_error(err);
    return -1;
  }

  bool relocate_now = false;
  if (Qdl_load_reloc_now && Fsymbol_value(Qdl_load_reloc_now, Qt) != rep_nil) {
    relocate_now = true;
  }

#if defined(HAVE_DLOPEN)
  void *handle = dlopen(dlname, (relocate_now ? RTLD_NOW : RTLD_LAZY)
			| (open_globally ? RTLD_GLOBAL : RTLD_LOCAL));
#elif defined(HAVE_SHL_LOAD)
  /* XXX how do we open these locally/globally? */
  void *handle = shl_load(dlname,
			  (relocate_now ? BIND_IMMEDIATE : BIND_DEFERRED)
			  | BIND_NONFATAL | DYNAMIC_PATH, 0L);
#endif

  if (!handle) {
    const char *err;
#ifdef HAVE_DLERROR
    err = dlerror();
#else
    err = "unknown dl error";
#endif
    if (err) {
      signal_error(err);
    }
    return -1;
  }

  if (dl_module_capacity == dl_module_count) {
    int new_n = MAX(dl_module_capacity * 2, 32);
    void *ptr = rep_realloc(dl_modules, new_n * sizeof(rep_dl_module));
    if (!ptr) {
      rep_mem_error();
      dlclose(handle);
      return -1;
    }

    dl_modules = ptr;
    dl_module_capacity = new_n;
  }

  idx = dl_module_count++;

  rep_dl_module *module = &dl_modules[idx];

  module->file_name = file_name;
  module->handle = handle;
  module->feature_sym = rep_nil;
  module->structure = rep_nil;
  module->is_rep_module = is_rep_module;

  if (is_rep_module) {
    repv (*init_func)(repv) = x_dlsym(handle, "rep_dl_init");
    if (init_func) {
      repv ret = init_func(file_name);

      if (rep_nil && (!ret || ret == rep_nil)) {
	--dl_module_count;
	dlclose(handle);
	return -1;
      }

      if (ret && rep_SYMBOLP(ret) && ret != Qt) {
	module->feature_sym = ret;
      } else if (ret && rep_STRUCTUREP(ret)) {
	module->structure = ret;
	ret = rep_STRUCTURE(ret)->name;
	if (ret && rep_SYMBOLP(ret)) {
	  module->feature_sym = ret;
	}
      }
    }
  }

  return idx;
}

repv
rep_dl_open_structure(repv file_name)
{
  int idx = rep_dl_intern_library(file_name);

  if (idx < 0) {
    return 0;
  }

  if (!dl_modules[idx].is_rep_module) {
    return Qt;
  }

  if (dl_modules[idx].feature_sym != rep_nil
      && dl_modules[idx].structure == rep_nil)
  {
    /* Only `provide' the feature if there's no structure (we haven't
       actually imported it) */

    Fprovide(dl_modules[idx].feature_sym);
  }

  return dl_modules[idx].structure;
}

void *
rep_dl_lookup_symbol(int idx, const char *name)
{
  void *handle = ((idx >= 0 && idx < dl_module_count)
		  ? dl_modules[idx].handle : RTLD_DEFAULT);

  return x_dlsym(handle, name);
}

void
rep_dl_mark_data(void)
{
  for (int i = 0; i < dl_module_count; i++) {
    rep_MARKVAL(dl_modules[i].file_name);
    rep_MARKVAL(dl_modules[i].feature_sym);
    rep_MARKVAL(dl_modules[i].structure);
  }
}

void
rep_dl_kill_libraries(void)
{
  for (int i = 0; i < dl_module_count; i++) {
    if (dl_modules[i].is_rep_module) {
      void (*exit_func)(void) = x_dlsym(dl_modules[i].handle, "rep_dl_kill");
      if (exit_func) {
	(*exit_func)();
      }
    }

    x_dlclose(dl_modules[i].handle);
  }

  dl_module_count = dl_module_capacity = 0;

  rep_free(dl_modules);
  dl_modules = NULL;
}

void *
rep_find_dl_symbol(repv feature, char *symbol)
{
  assert(rep_SYMBOLP(feature));

  int idx = find_dl_module_by_feature(rep_SYM(feature)->name);

  if (idx <= 0) {
    return NULL;
  }

  return x_dlsym(dl_modules[idx].handle, symbol);
}

/* Attempt to find the name and address of the nearest symbol before or
   equal to PTR */

bool
rep_find_c_symbol(void *ptr, char **symbol_name_p, void **symbol_addr_p)
{
#if defined(HAVE_DLADDR)
  Dl_info info;
  if (dladdr(ptr, &info) != 0) {
    *symbol_name_p = (char *)info.dli_sname;
    *symbol_addr_p = info.dli_saddr;
    return true;
  }
#endif

  return false;
}
	
#else /* HAVE_DYNAMIC_LOADING */

bool
rep_find_c_symbol(void *ptr, char **name_p, void **addr_p)
{
  return false;
}

#endif /* !HAVE_DYNAMIC_LOADING */
