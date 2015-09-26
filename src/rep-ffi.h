/* rep-ffi.h -- foreigh function interface

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

#ifndef REP_FFI_H
#define REP_FFI_H

#include "repint.h"

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

#if defined (HAVE_FFI_H)
#include <ffi.h>
#elif defined (HAVE_FFI_FFI_H)
#include <ffi/ffi.h>
#endif

enum rep_ffi_types {
  rep_FFI_TYPE_VOID,
  rep_FFI_TYPE_UINT8,
  rep_FFI_TYPE_SINT8,
  rep_FFI_TYPE_UINT16,
  rep_FFI_TYPE_SINT16,
  rep_FFI_TYPE_UINT32,
  rep_FFI_TYPE_SINT32,
  rep_FFI_TYPE_UINT64,
  rep_FFI_TYPE_SINT64,
  rep_FFI_TYPE_FLOAT,
  rep_FFI_TYPE_DOUBLE,
  rep_FFI_TYPE_LONGDOUBLE,
  rep_FFI_TYPE_POINTER,
  rep_FFI_TYPE_BOOL,
  rep_FFI_TYPE_STRING,
  rep_FFI_TYPE_OBJECT,
};

#ifdef HAVE_LIBFFI

typedef struct rep_ffi_type_struct rep_ffi_type;
typedef struct rep_ffi_alias_struct rep_ffi_alias;
typedef struct rep_ffi_array_struct rep_ffi_array;
typedef struct rep_ffi_struct_struct rep_ffi_struct;
typedef struct rep_ffi_interface_struct rep_ffi_interface;

struct rep_ffi_type_struct {
  ffi_type *type;
  unsigned int subtype;
};

enum rep_ffi_subtype_enum {
  rep_FFI_PRIMITIVE = 0,
  rep_FFI_ARRAY,
  rep_FFI_STRUCT,
  rep_FFI_ALIAS,
  rep_FFI_BOOL,
  rep_FFI_STRING,
#ifdef HAVE_FFI_OBJECTS
  rep_FFI_OBJECT,
#endif
};

struct rep_ffi_alias_struct {
  rep_ffi_type super;
  repv predicate;
  repv conv_in;
  repv conv_out;
  unsigned int base;
};

struct rep_ffi_array_struct {
  rep_ffi_type super;
  ffi_type type;
  unsigned int n_elements;
  unsigned int element_id;
};

struct rep_ffi_struct_struct {
  rep_ffi_type super;
  ffi_type type;
  unsigned int n_elements;
  unsigned int *element_ids;
};

struct rep_ffi_interface_struct {
  ffi_cif cif;
  unsigned int n_args;
  unsigned int args_size;
  unsigned int ret;
  unsigned int args[1];
};

#define SIZEOF_REP_FFI_INTERFACE(n) \
  (sizeof(rep_ffi_interface) + (sizeof(int) * ((n) - 1)))

#endif /* HAVE_FFI */

#define rep_make_pointer(p) rep_make_long_uint((uintptr_t)p)
#define rep_get_pointer(x)  ((void *)rep_get_long_uint(x))
#define rep_pointerp(x)     rep_INTEGERP(x)

extern int rep_ffi_get_array_type(int n, unsigned int elt_type);
extern int rep_ffi_get_struct_type(int n, const unsigned int *elt_types);

extern int rep_ffi_get_interface(unsigned int ret_type, int argc,
  const unsigned int *arg_types);
extern const rep_ffi_interface *rep_ffi_interface_ref(unsigned int iface);

extern repv rep_ffi_apply(unsigned int iface_id, void *function_ptr,
  int argc, repv *argv);

extern repv Fffi_array(repv count, repv type);
extern repv Fffi_struct(repv fields);
extern repv Fffi_type(repv base, repv pred, repv in, repv out);
extern repv Fffi_interface(repv ret, repv args);
extern repv Fffi_apply(repv iface_id, repv ptr, repv args);

extern repv Fffi_new(repv type_id, repv count);
extern repv Fffi_delete(repv type_id, repv addr);
extern repv Fffi_address_of(repv type_id, repv addr, repv idx);
extern repv Fffi_set(repv type_id, repv addr, repv value);
extern repv Fffi_get(repv type_id, repv addr);

extern repv Fffi_load_library(repv name);
extern repv Fffi_lookup_symbol(repv name, repv handle);

#ifdef HAVE_FFI_OBJECTS
extern repv rep_ffi_make_object(void *ptr);
extern void *rep_ffi_object_pointer(repv obj);
extern void rep_ffi_objects_init(void);
#endif

#endif /* REP_FFI_H */
