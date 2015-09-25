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
  rep_FFI_TYPE_POINTER,
  rep_FFI_TYPE_STRING,
  rep_FFI_TYPE_OBJECT,
};

/* long_uint functions take/return uintptr_t. */
#define rep_make_pointer(p) rep_make_long_uint((unsigned long) p)
#define rep_get_pointer(x)  (void *) rep_get_long_uint(x)
#define rep_pointerp(x)     rep_INTEGERP(x)

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
