/* ffi.c -- foreign function interface plugin

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

/* Commentary:

   (ffi-struct [MEMBER-TYPES ...]) -> TYPE
     -- creates a new structure type

   (ffi-interface RET-TYPE (PARAM-TYPES ...)) -> INTERFACE
     -- creates a new ffi function signature

   (ffi-type BASE-TYPE [PREDICATE] [TYPE->BASE] [BASE->TYPE]) -> TYPE
     -- creates a new type alias

   (ffi-apply INTERFACE FN-POINTER ARG-LIST) -> RET-VALUE
     -- calls a function and returns its result

   Apply works by walking the list of arguments converting everything
   into native or structure types. It calls the function then converts
   any returned value back to lisp data.

   Question: how to handle `out' parameters? E.g.

	void foo (int a, int *out_b) { *out_b = a + 1 }

   one possibility is to recognize `(out TYPE)' as meaning, allocate
   space for one of TYPE on the stack, pass its address to the
   function, then return the converted value (somehow)

   but then how do you handle arrays? E.g.

	void foo (int n, int values[]) {...}

   Perhaps just provide some primitives:

	(ffi-new TYPE #!optional (COUNT 1)) -> POINTER
	(ffi-delete TYPE POINTER)

	(ffi-address-of TYPE POINTER INDEX) -> POINTER'

	(ffi-set! TYPE POINTER VALUE)
	(ffi-get TYPE POINTER) -> VALUE

   this should be enough to allow everything to be handled by higher
   level code. */

#include "rep-ffi.h"

#undef ALIGN
#define ALIGN(v, a)     (((size_t)(v) + (a) - 1) & ~((a) - 1))

#ifdef HAVE_LIBFFI

static int n_ffi_types, n_alloc_ffi_types;
static rep_ffi_type **ffi_types;

static int n_ffi_interfaces, n_alloc_ffi_interfaces;
static rep_ffi_interface **ffi_interfaces;

#define rep_VALID_TYPE_P(x) \
  (rep_INTP(x) && rep_INT(x) >= 0 && rep_INT(x) < n_ffi_types)
#define rep_VALID_INTERFACE_P(x) \
  (rep_INTP(x) && rep_INT(x) >= 0 && rep_INT(x) < n_ffi_interfaces)

static bool
ffi_types_equal_p(const rep_ffi_type *a, const rep_ffi_type *b)
{
  if (a->subtype != b->subtype) {
    return false;
  }
  if (a->type != NULL && a->type == b->type) {
    return true;
  }

  switch (a->subtype) {
  case rep_FFI_PRIMITIVE:
    return (a->type->type == b->type->type
	    && a->type->size == b->type->size
	    && a->type->alignment == b->type->alignment);

  case rep_FFI_ARRAY: {
    const rep_ffi_array *aa = (rep_ffi_array *)a;
    const rep_ffi_array *ab = (rep_ffi_array *)b;
    if (aa->n_elements != ab->n_elements) {
      return false;
    }
    if (aa->element_id != ab->element_id) {
      return false;
    }
    return true; }

  case rep_FFI_STRUCT: {
    const rep_ffi_struct *sa = (rep_ffi_struct *)a;
    const rep_ffi_struct *sb = (rep_ffi_struct *)b;
    if (sa->n_elements != sb->n_elements) {
      return false;
    }
    for (unsigned int i = 0; i < sa->n_elements; i++) {
      if (sa->element_ids[i] != sb->element_ids[i]) {
	return false;
      }
    }
    return true; }

  case rep_FFI_ALIAS: {
    const rep_ffi_alias *aa = (rep_ffi_alias *)a;
    const rep_ffi_alias *ab = (rep_ffi_alias *)b;
    return (aa->base == ab->base
	    && aa->predicate == ab->predicate
	    && aa->conv_in == ab->conv_in
	    && aa->conv_out == ab->conv_out); }

  default:
    return false;
  }
}

static unsigned int
ffi_alloc_type(rep_ffi_type *type, bool search)
{
  /* FIXME: this is O(N), it should be hashed. */

  if (search) {
    for (unsigned int i = 0; i < n_ffi_types; i++) {
      if (ffi_types_equal_p(type, ffi_types[i])) {
	rep_free(type);
	return i;
      }
    }
  }

  unsigned int idx = n_ffi_types++;

  if (idx >= n_alloc_ffi_types) {
    n_alloc_ffi_types = MAX(n_alloc_ffi_types * 2, 256);
    ffi_types = rep_realloc(ffi_types,
			    sizeof(ffi_types[0]) * n_alloc_ffi_types);
  }

  ffi_types[idx] = type;

  return idx;
}

static char *
rep_ffi_marshal(unsigned int type_id, repv value, char *ptr)
{
  rep_ffi_type *type = ffi_types[type_id];

  switch (type->subtype) {
    DEFSTRING(err, "unknown ffi type id");
    DEFSTRING(err2, "ffi struct expected a vector or list");
    DEFSTRING(err3, "ffi type expected a string");

  case rep_FFI_PRIMITIVE:
    switch (type->type->type) {
    case FFI_TYPE_VOID:
      return ptr;

    case FFI_TYPE_INT:
      *(int *)ptr = (int)rep_get_long_int(value);
      return ptr + sizeof(int);

    case FFI_TYPE_FLOAT:
      *(float *)ptr = (float)rep_get_float(value);
      return ptr + sizeof(float);

    case FFI_TYPE_DOUBLE:
      *(double *)ptr = (double)rep_get_float(value);
      return ptr + sizeof(double);

#if FFI_TYPE_LONGDOUBLE != FFI_TYPE_DOUBLE
    case FFI_TYPE_LONGDOUBLE:
      *(long double *)ptr = (long double)rep_get_float(value);
      return ptr + sizeof(long double);
#endif

    case FFI_TYPE_UINT8:
      *(uint8_t *)ptr = (uint8_t)rep_get_long_int(value);
      return ptr + sizeof(uint8_t);

    case FFI_TYPE_SINT8:
      *(int8_t *)ptr = (int8_t)rep_get_long_int(value);
      return ptr + sizeof(int8_t);

    case FFI_TYPE_UINT16:
      *(uint16_t *)ptr = (uint16_t)rep_get_long_int(value);
      return ptr + sizeof(uint16_t);

    case FFI_TYPE_SINT16:
      *(int16_t *)ptr = (int16_t)rep_get_long_int(value);
      return ptr + sizeof(int16_t);

    case FFI_TYPE_UINT32:
      *(uint32_t *)ptr = (uint32_t)rep_get_long_int(value);
      return ptr + sizeof(uint32_t);

    case FFI_TYPE_SINT32:
      *(int32_t *)ptr = (int32_t)rep_get_long_int(value);
      return ptr + sizeof(int32_t);

    case FFI_TYPE_UINT64:
      *(uint64_t *)ptr = (uint64_t)rep_get_longlong_int(value);
      return ptr + sizeof(uint64_t);

    case FFI_TYPE_SINT64:
      *(int64_t *)ptr = (int64_t)rep_get_longlong_int(value);
      return ptr + sizeof(int64_t);

    case FFI_TYPE_POINTER:
      *(void **)ptr = rep_get_pointer(value);
      return ptr + sizeof(void *);

    case FFI_TYPE_STRUCT:		/* FIXME: */
    default:
      Fsignal(Qerror, rep_list_2(rep_VAL(&err), rep_MAKE_INT(type_id)));
      return NULL;
    }
    /* not reached */

  case rep_FFI_ARRAY: {
    rep_ffi_array *a = (rep_ffi_array *)type;

    rep_GC_root gc_value;
    rep_PUSHGC (gc_value, value);

    for (int i = 0; i < a->n_elements; i++) {
      repv elt;
      if (rep_VECTORP(value)) {
	elt = rep_VECTI(value, i);
      } else if (rep_CONSP(value)) {
	elt = rep_CAR(value);
	value = rep_CDR(value);
      } else {
	rep_POPGC;
	Fsignal(Qerror, rep_list_2(rep_VAL(&err2), value));
	return NULL;
      }

      ptr = rep_ffi_marshal(a->element_id, elt, ptr);

      if (!ptr) {
	rep_POPGC;
	return NULL;
      }
    }

    rep_POPGC;
    return ptr; }

  case rep_FFI_STRUCT: {
    rep_ffi_struct *s = (rep_ffi_struct *)type;

    rep_GC_root gc_value;
    rep_PUSHGC (gc_value, value);

    for (int i = 0; i < s->n_elements; i++) {
      repv elt;
      if (rep_VECTORP(value)) {
	elt = rep_VECTI(value, i);
      } else if (rep_CONSP(value)) {
	elt = rep_CAR(value);
	value = rep_CDR(value);
      } else {
	rep_POPGC;
	Fsignal(Qerror, rep_list_2(rep_VAL(&err2), value));
	return NULL;
      }

      ptr = rep_ffi_marshal(s->element_ids[i], elt, ptr);

      if (!ptr) {
	rep_POPGC;
	return NULL;
      }
    }

    rep_POPGC;
    return ptr; }

  case rep_FFI_ALIAS: {
    rep_ffi_alias *s = (rep_ffi_alias *)type;

    if (s->conv_in) {
      value = rep_call_lisp1(s->conv_in, value);
      if (!value) {
	return NULL;
      }
    }

    return rep_ffi_marshal(s->base, value, ptr); }

  case rep_FFI_BOOL:
    *(uint8_t *)ptr = value != rep_nil;
    return ptr + sizeof(uint8_t);

  case rep_FFI_STRING:
    if (!rep_STRINGP(value)) {
      Fsignal(Qerror, rep_list_2(rep_VAL(&err3), value));
      return NULL;
    }
    *(const void **)ptr = rep_STR(value);
    return ptr + sizeof(void *);

#ifdef HAVE_FFI_OBJECTS
  case rep_FFI_OBJECT: {
    void *obj = rep_ffi_object_pointer(value);
    if (!obj) {
      return NULL;
    }
    *(void **)ptr = obj;
    return ptr + sizeof(void *); }
#endif

  default:
    Fsignal(Qerror, rep_list_2(rep_VAL(&err), rep_MAKE_INT(type_id)));
    return NULL;
  }
}

static char *
rep_ffi_demarshal(unsigned int type_id, char *ptr, repv *value)
{
  rep_ffi_type *type = ffi_types[type_id];
  
  switch (type->subtype) {
    DEFSTRING(err, "unknown ffi type id");
    
  case rep_FFI_PRIMITIVE:
    switch (type->type->type) {
    case FFI_TYPE_VOID:
      *value = rep_undefined_value;
      return ptr;
      
    case FFI_TYPE_INT:
      *value = rep_make_long_int(*(int *)ptr);
      return ptr + sizeof(int);
      
    case FFI_TYPE_FLOAT:
      *value = rep_make_float(*(float *)ptr, true);
      return ptr + sizeof(float);
      
    case FFI_TYPE_DOUBLE:
      *value = rep_make_float(*(double *)ptr, true);
      return ptr + sizeof(double);
      
#if FFI_TYPE_LONGDOUBLE != FFI_TYPE_DOUBLE
    case FFI_TYPE_LONGDOUBLE:
      *value = rep_make_float(*(long double *)ptr, true);
      return ptr + sizeof(long double);
#endif
      
    case FFI_TYPE_UINT8:
      *value = rep_MAKE_INT(*(uint8_t *)ptr);
      return ptr + sizeof(uint8_t);
      
    case FFI_TYPE_SINT8:
      *value = rep_MAKE_INT(*(int8_t *)ptr);
      return ptr + sizeof(int8_t);
      
    case FFI_TYPE_UINT16:
      *value = rep_MAKE_INT(*(uint16_t *)ptr);
      return ptr + sizeof(uint16_t);
      
    case FFI_TYPE_SINT16:
      *value = rep_MAKE_INT(*(int16_t *)ptr);
      return ptr + sizeof(int16_t);
      
    case FFI_TYPE_UINT32:
      *value = rep_make_long_int(*(uint32_t *)ptr);
      return ptr + sizeof(uint32_t);
      
    case FFI_TYPE_SINT32:
      *value = rep_make_long_int(*(int32_t *)ptr);
      return ptr + sizeof(int32_t);
      
    case FFI_TYPE_UINT64:
      *value = rep_make_longlong_int(*(uint64_t *)ptr);
      return ptr + sizeof(uint64_t);
      
    case FFI_TYPE_SINT64:
      *value = rep_make_longlong_int(*(int64_t *)ptr);
      return ptr + sizeof(int64_t);
      
    case FFI_TYPE_POINTER:
      *value = rep_make_pointer(*(void **)ptr);
      return ptr + sizeof(void *);
      
    case FFI_TYPE_STRUCT:		/* FIXME: */
    default:
      Fsignal(Qerror, rep_list_2(rep_VAL(&err), rep_MAKE_INT(type_id)));
      return NULL;
    }
    /* not reached */
    
  case rep_FFI_ARRAY: {
    rep_ffi_array *a = (rep_ffi_array *)type;
    
    *value = rep_make_vector (a->n_elements);
    
    rep_GC_n_roots gc_value;
    rep_PUSHGCN (gc_value, value, 1);
    
    for (int i = 0; i < a->n_elements; i++) {
      ptr = rep_ffi_demarshal(a->element_id, ptr, &rep_VECTI(*value, i));
      if (!ptr) {
	rep_POPGCN;
	return NULL;
      }
    }
    
    rep_POPGCN;
    return ptr; }
    
  case rep_FFI_STRUCT: {
    rep_ffi_struct *s = (rep_ffi_struct *)type;
    
    *value = rep_make_vector (s->n_elements);
    
    rep_GC_n_roots gc_value;
    rep_PUSHGCN (gc_value, value, 1);
    
    for (int i = 0; i < s->n_elements; i++) {
      ptr = rep_ffi_demarshal(s->element_ids[i], ptr, &rep_VECTI(*value, i));
      if (!ptr) {
	rep_POPGCN;
	return NULL;
      }
    }
    
    rep_POPGCN;
    return ptr; }
    
  case rep_FFI_ALIAS: {
    rep_ffi_alias *s = (rep_ffi_alias *)type;
    
    ptr = rep_ffi_marshal(s->base, *value, ptr);
    
    if (s->conv_out) {
      *value = rep_call_lisp1(s->conv_out, *value);
      if (!*value) {
	return NULL;
      }
    }
    
    return ptr; }

  case rep_FFI_BOOL:
    *value = *(uint8_t *)ptr != 0 ? Qt : rep_nil;
    return ptr + sizeof(uint8_t);

  case rep_FFI_STRING:
    *value = rep_string_copy(*(void **)ptr);
    return ptr + sizeof(void *);
    
#ifdef HAVE_FFI_OBJECTS
  case rep_FFI_OBJECT:
    *value = rep_ffi_make_object(*(void **)ptr);
    return ptr + sizeof(void *);
#endif

  default:
    Fsignal(Qerror, rep_list_2(rep_VAL(&err), rep_MAKE_INT(type_id)));
    return NULL;
  }
}

int
rep_ffi_get_array_type(int n, unsigned int elt_type)
{
  for (unsigned int i = 0; i < n_ffi_types; i++) {
    const rep_ffi_type *t = ffi_types[i];
    if (t->subtype != rep_FFI_ARRAY) {
      continue;
    }
    const rep_ffi_array *a = (const rep_ffi_array *)t;
    if (a->n_elements != n || a->element_id != elt_type) {
      continue;
    }
    return i;
  }

  rep_ffi_array *s = rep_alloc(sizeof(rep_ffi_array)
			       + sizeof(ffi_type *) * (n + 1));

  ffi_type **elts = (void *)((char *)s + sizeof(rep_ffi_array));

  s->super.type = &s->type;
  s->super.subtype = rep_FFI_ARRAY;

  s->element_id = elt_type;

  for (unsigned int i = 0; i < n; i++) {
    elts[i] = ffi_types[elt_type]->type;
  }
  elts[n] = NULL;

  s->n_elements = n;
  s->type.elements = elts;
  s->type.size = s->type.alignment = 0;

  /* We should leave size and alignment as zero and let libffi
     initialize them. But that doesn't for me, need the size known at
     all times. */

  if (n != 0) {
    s->type.alignment = elts[0]->alignment;
    for (unsigned int i = 0; i < n; i++) {
      s->type.size = ALIGN(s->type.size, elts[i]->alignment);
      s->type.size += elts[i]->size;
    }
  }

  return ffi_alloc_type(&s->super, false);
}

DEFUN("ffi-array", Fffi_array, Sffi_array, (repv count, repv field), rep_Subr2)
{
  rep_DECLARE1(count, rep_NON_NEG_INT_P);
  rep_DECLARE2(field, rep_VALID_TYPE_P);

  int idx = rep_ffi_get_array_type(rep_INT(count), rep_INT(field));

  if (idx < 0) {
    return 0;
  }

  return rep_MAKE_INT(idx);
}

int
rep_ffi_get_struct_type(int n, const unsigned int *elt_types)
{
  for (unsigned int i = 0; i < n_ffi_types; i++) {
    const rep_ffi_type *t = ffi_types[i];
    if (t->subtype != rep_FFI_STRUCT) {
      continue;
    }
    const rep_ffi_struct *s = (const rep_ffi_struct *)t;
    if (s->n_elements != n) {
      continue;
    }
    bool matches = true;
    for (unsigned int j = 0; j < s->n_elements; j++) {
      if (s->element_ids[j] != elt_types[j]) {
	matches = false;
	break;
      }
    }
    if (matches) {
      return i;
    }    
  }

  rep_ffi_struct *s = rep_alloc(sizeof(rep_ffi_struct)
				+ sizeof(ffi_type *) * (n + 1)
				+ sizeof(unsigned int) * n);

  ffi_type **elts = (void *)((char *)s + sizeof(rep_ffi_struct));
  s->element_ids = (void *)((char *)elts + sizeof(ffi_type *) * (n + 1));

  s->super.type = &s->type;
  s->super.subtype = rep_FFI_STRUCT;

  for (unsigned int i = 0; i < n; i++) {
    s->element_ids[i] = elt_types[i];
    elts[i] = ffi_types[elt_types[i]]->type;
  }
  elts[n] = NULL;

  s->n_elements = n;
  s->type.elements = elts;
  s->type.size = s->type.alignment = 0;

  /* We should leave size and alignment as zero and let libffi
     initialize them. But that doesn't for me, need the size known at
     all times. */

  for (unsigned int i = 0; i < s->n_elements; i++) {
    s->type.size = ALIGN(s->type.size, elts[i]->alignment);
    s->type.size += elts[i]->size;
    s->type.alignment = MAX(s->type.alignment, elts[i]->alignment);
  }

  return ffi_alloc_type(&s->super, false);
}

DEFUN("ffi-struct", Fffi_struct, Sffi_struct, (repv fields), rep_Subr1)
{
  unsigned int n;

  if (rep_VECTORP(fields)) {
    n = rep_VECTOR_LEN(fields);
  } else if (rep_CONSP(fields)) {
    int l = rep_list_length(fields);
    if (l < 0) {
      return 0;
    }
    n = l;
  } else {
    return rep_signal_arg_error(fields, 1);
  }

  unsigned int *elt_types = rep_stack_alloc(unsigned int, n);
  if (!elt_types) {
    return rep_mem_error();
  }

  for (unsigned int i = 0; i < n; i++) {
    repv x;
    if (rep_VECTORP(fields)) {
      x = rep_VECTI(fields, i);
    } else if (rep_CONSP(fields)) {
      x = rep_CAR(fields);
      fields = rep_CDR(fields);
    } else {
      x = 0;
    }

    if (!x || !rep_VALID_TYPE_P(x)) {
      rep_stack_free(unsigned int, n, elt_types);
      return rep_signal_arg_error(x, 1);
    }

    elt_types[i] = rep_INT(x);
  }

  int idx = rep_ffi_get_struct_type(n, elt_types);

  if (idx < 0) {
    return 0;
  }

  return rep_MAKE_INT(idx);
}

DEFUN("ffi-type", Fffi_type, Sffi_type,
      (repv base, repv pred, repv in, repv out), rep_Subr4)
{
  rep_DECLARE(1, base, rep_VALID_INTERFACE_P (base));

  rep_ffi_alias *s = rep_alloc(sizeof(rep_ffi_alias));

  s->super.subtype = rep_FFI_ALIAS;
  s->super.type = ffi_types[rep_INT(base)]->type;
  s->predicate = pred;
  s->conv_in = in;
  s->conv_out = out;
  s->base = rep_INT(base);

  /* FIXME: no type to add a mark function to, so do it statically. */

  rep_mark_static(&s->predicate);
  rep_mark_static(&s->conv_in);
  rep_mark_static(&s->conv_out);

  return rep_MAKE_INT(ffi_alloc_type(&s->super, true));
}

static repv
ffi_add_primitive_type(ffi_type *type)
{
  rep_ffi_type *s = rep_alloc(sizeof(rep_ffi_type));

  s->subtype = rep_FFI_PRIMITIVE;
  s->type = type;

  return rep_MAKE_INT(ffi_alloc_type(s, false));
}

static repv
ffi_add_custom_type(ffi_type *type, unsigned int subtype)
{
  rep_ffi_type *s = rep_alloc(sizeof(rep_ffi_type));

  s->subtype = subtype;
  s->type = type;

  return rep_MAKE_INT(ffi_alloc_type(s, false));
}

int
rep_ffi_get_interface(unsigned int ret, int n_args,
		      const unsigned int *args)
{
  unsigned int args_size = 0;
  for (unsigned int i = 0; i < n_args; i++) {
    ffi_type *arg_type = ffi_types[args[i]]->type;
    if (arg_type->alignment > 1) {
      args_size = ALIGN(args_size, arg_type->alignment);
    }
    args_size += arg_type->size;
  }

  for (unsigned int i = 0; i < n_ffi_interfaces; i++) {
    const rep_ffi_interface *p = ffi_interfaces[i];
    if (p->ret != ret) {
      continue;
    }
    if (p->n_args != n_args || p->args_size != args_size) {
      continue;
    }
    bool matches = true;
    for (unsigned int j = 0; j < n_args; j++) {
      if (p->args[i] != args[j]) {
	matches = false;
	break;
      }
    }
    if (matches) {
      return i;
    }
  }

  rep_ffi_interface *s = rep_alloc(SIZEOF_REP_FFI_INTERFACE(n_args)
				   + sizeof(ffi_type *) * n_args);
  s->n_args = n_args;
  s->args_size = args_size;
  s->ret = ret;

  ffi_type *ret_type = ffi_types[ret]->type;
  ffi_type **arg_types =
    (ffi_type **)((char *)s + SIZEOF_REP_FFI_INTERFACE(n_args));

  for (unsigned int i = 0; i < n_args; i++) {
    s->args[i] = args[i];
    arg_types[i] = ffi_types[args[i]]->type;
  }

  if (ffi_prep_cif(&s->cif, FFI_DEFAULT_ABI, n_args,
		   ret_type, arg_types) != FFI_OK)
  {
    Fsignal(Qinvalid_function, rep_nil);	/* FIXME: */
    return -1;
  }

  unsigned int idx = n_ffi_interfaces++;

  if (idx >= n_alloc_ffi_interfaces) {
    n_alloc_ffi_interfaces = MAX(n_alloc_ffi_interfaces * 2, 256);
    ffi_interfaces = rep_realloc(ffi_interfaces, sizeof (ffi_interfaces[0])
				 * n_alloc_ffi_interfaces);
  }

  ffi_interfaces[idx] = s;

  return idx;
}

const rep_ffi_interface *
rep_ffi_interface_ref(unsigned int idx)
{
  return ffi_interfaces[idx];
}

DEFUN("ffi-interface", Fffi_interface, Sffi_interface,
      (repv ret, repv args), rep_Subr2)
{
  if (ret != rep_nil) {
    rep_DECLARE(1, ret, rep_VALID_TYPE_P (ret));
  }

  unsigned int n;
  if (rep_VECTORP(args)) {
    n = rep_VECTOR_LEN(args);
  } else if (rep_LISTP(args)) {
    int l = rep_list_length(args);
    if (l < 0) {
      return 0;
    }
    n = l;
  } else {
    return rep_signal_arg_error (args, 2);
  }

  unsigned int argv[n];

  for (unsigned int i = 0; i < n; i++) {
    repv elt;
    if (rep_VECTORP(args)) {
      elt = rep_VECTI (args, i);
    } else {
      elt = rep_CAR(args);
      args = rep_CDR(args);
    }

    if (!rep_VALID_TYPE_P(elt)) {
      return rep_signal_arg_error(args, 2);
    }

    argv[i] = rep_INT(elt);
  }

  int idx = rep_ffi_get_interface(rep_INT(ret), n, argv);

  if (idx < 0) {
    return 0;
  }

  return rep_MAKE_INT(idx);
}

repv
rep_ffi_apply(unsigned int iface_id, void *function_ptr, int argc, repv *argv)
{
  const rep_ffi_interface *iface = ffi_interfaces[iface_id];

  if (iface->n_args != argc) {
    return rep_signal_missing_arg(argc + 1);
  }

  /* arrays of doubles for good alignment. */
  double _args_data[(iface->args_size >> 3) + 1];
  double _ret_data[(iface->cif.rtype->size >> 3) + 1];
  void *values[iface->n_args];
  repv ret_value = rep_undefined_value;

  char *ret_data = NULL;
  if (iface->cif.rtype->size != 0) {
    ret_data = (char *)_ret_data;
  }

  char *args_data = (char *)_args_data;
  char *args_ptr = args_data;

  rep_GC_n_roots gc_argv;
  rep_PUSHGCN(gc_argv, argv, argc);

  for (unsigned int i = 0; i < argc; i++) {
    values[i] = args_ptr;
    args_ptr = rep_ffi_marshal(iface->args[i], argv[i], args_ptr);
    if (!args_ptr) {
      rep_POPGCN;
      return 0;
    }
  }

  rep_POPGCN;

  ffi_call((ffi_cif *)&iface->cif, function_ptr, ret_data, values);

  if (ret_data) {
    if (!rep_ffi_demarshal(iface->ret, ret_data, &ret_value)) {
      return 0;
    }
  }

  return ret_value;
}

DEFUN("ffi-apply", Fffi_apply, Sffi_apply,
      (repv iface_id, repv ptr, repv args), rep_Subr3)
{
  rep_DECLARE(1, iface_id, rep_VALID_INTERFACE_P(iface_id));
  rep_DECLARE(2, ptr, rep_pointerp(ptr));

  void *function_ptr = rep_get_pointer(ptr);
  if (!function_ptr) {
    return rep_signal_arg_error(ptr, 2);
  }

  const rep_ffi_interface *iface = ffi_interfaces[rep_INT(iface_id)];

  repv argv[iface->n_args];

  for (unsigned int i = 0; i < iface->n_args; i++) {
    if (!rep_CONSP(args)) {
      return rep_signal_missing_arg(i + 1);
    }
    argv[i] = rep_CAR(args);
    args = rep_CDR(args);
  }

  return rep_ffi_apply(rep_INT(iface_id), function_ptr, iface->n_args, argv);
}

DEFUN("ffi-new", Fffi_new, Sffi_new, (repv type_id, repv count), rep_Subr2)
{
  rep_DECLARE1(type_id, rep_VALID_TYPE_P);
  if (count != rep_nil) {
    rep_DECLARE2(count, rep_INTP);
  } else {
    count = rep_MAKE_INT (1);
  }

  const rep_ffi_type *type = ffi_types[rep_INT (type_id)];
  void *ptr = rep_alloc(type->type->size * rep_INT (count));
    
  return rep_make_pointer (ptr);
}

DEFUN("ffi-delete", Fffi_delete, Sffi_delete,
      (repv type_id, repv addr), rep_Subr2)
{
  rep_DECLARE1(type_id, rep_VALID_TYPE_P);
  rep_DECLARE2(addr, rep_pointerp);

  rep_free(rep_get_pointer(addr));

  return rep_undefined_value;
}

DEFUN("ffi-address-of", Fffi_address_of, Sffi_address_of,
      (repv type_id, repv addr, repv idx), rep_Subr3)
{
  rep_DECLARE1(type_id, rep_VALID_TYPE_P);
  rep_DECLARE2(addr, rep_pointerp);
  rep_DECLARE(3, idx, rep_INTP(idx) && rep_INT(idx) >= 0);

  const rep_ffi_type *type = ffi_types[rep_INT (type_id)];
  char *ptr = rep_get_pointer(addr);

  for (intptr_t i = rep_INT(idx); i > 0; i--) {
    ptr = (void *)ALIGN(ptr, type->type->alignment);
    ptr += type->type->size;
  }

  return rep_make_pointer(ptr);
}

DEFUN("ffi-set!", Fffi_set_, Sffi_set_,
      (repv type_id, repv addr, repv value), rep_Subr4)
{
  rep_DECLARE1(type_id, rep_VALID_TYPE_P);
  rep_DECLARE2(addr, rep_pointerp);

  const rep_ffi_type *type = ffi_types[rep_INT (type_id)];

  char *ptr = rep_get_pointer (addr);
  ptr = (void *)ALIGN(ptr, type->type->alignment);

  if (!rep_ffi_marshal(rep_INT(type_id), value, ptr)) {
    return 0;
  }

  return rep_undefined_value;
}

DEFUN("ffi-get", Fffi_get, Sffi_get, (repv type_id, repv addr), rep_Subr2)
{
  rep_DECLARE1(type_id, rep_VALID_TYPE_P);
  rep_DECLARE2(addr, rep_pointerp);

  const rep_ffi_type *type = ffi_types[rep_INT (type_id)];

  char *ptr = rep_get_pointer(addr);
  ptr = (void *)ALIGN(ptr, type->type->alignment);

  repv value;
  if (!rep_ffi_demarshal(rep_INT(type_id), ptr, &value)) {
    return 0;
  }

  return value;
}

#else /* HAVE_LIBFFI */

static repv
no_libffi_error(void)
{
  DEFSTRING(err, "ffi support is not present in this installation");
  return Fsignal(Qerror, Fcons(rep_VAL(&err), rep_nil));
}

DEFUN("ffi-struct", Fffi_struct, Sffi_struct, (repv fields), rep_Subr1)
{
  return no_libffi_error();
}

DEFUN("ffi-type", Fffi_type, Sffi_type,
      (repv base, repv pred, repv in, repv out), rep_Subr4)
{
  return no_libffi_error();
}

DEFUN("ffi-interface", Fffi_interface, Sffi_interface,
      (repv ret, repv args), rep_Subr2)
{
  return no_libffi_error();
}

DEFUN("ffi-apply", Fffi_apply, Sffi_apply,
      (repv iface_id, repv ptr, repv args), rep_Subr3)
{
  return no_libffi_error();
}

DEFUN("ffi-new", Fffi_new, Sffi_new, (repv type_id, repv count), rep_Subr2)
{
  return no_libffi_error();
}

DEFUN("ffi-delete", Fffi_delete, Sffi_delete,
      (repv type_id, repv addr), rep_Subr2)
{
  return no_libffi_error();
}

DEFUN("ffi-address-of", Fffi_address_of, Sffi_address_of,
      (repv type_id, repv addr, repv idx), rep_Subr3)
{
  return no_libffi_error();
}

DEFUN("ffi-set!", Fffi_set_, Sffi_set_,
      (repv type_id, repv addr, repv value), rep_Subr4)
{
  return no_libffi_error();
}

DEFUN("ffi-get", Fffi_get, Sffi_get, (repv type_id, repv addr), rep_Subr2)
{
  return no_libffi_error();
}

#endif /* HAVE_LIBFFI */

DEFUN("ffi-load-library", Fffi_load_library,
      Sffi_load_library, (repv name), rep_Subr1)
{
  rep_DECLARE1(name, rep_STRINGP);

  int handle = rep_dl_intern_library(name);

  if (name == -1) {
    DEFSTRING(err, "Can't open shared library");
    return Fsignal(Qerror, rep_list_2(rep_VAL(&err), name));
  }

  return rep_MAKE_INT(handle);
}

DEFUN("ffi-lookup-symbol", Fffi_lookup_symbol,
      Sffi_lookup_symbol, (repv name, repv handle), rep_Subr2)
{
  rep_DECLARE1(name, rep_STRINGP);

  if (handle != rep_nil) {
    rep_DECLARE2(handle, rep_INTP);
  }

  /* anything outside the range of valid handles means RTLD_DEFAULT. */

  void *ptr = rep_dl_lookup_symbol(handle != rep_nil ? rep_INT(handle) : -1,
				   rep_STR(name));

  return ptr ? rep_make_pointer(ptr) : rep_nil;
}


/* dl hooks */

DEFSYM(ffi_type_void, "ffi-type-void");
DEFSYM(ffi_type_uint8, "ffi-type-uint8");
DEFSYM(ffi_type_sint8, "ffi-type-sint8");
DEFSYM(ffi_type_uint16, "ffi-type-uint16");
DEFSYM(ffi_type_sint16, "ffi-type-sint16");
DEFSYM(ffi_type_uint32, "ffi-type-uint32");
DEFSYM(ffi_type_sint32, "ffi-type-sint32");
DEFSYM(ffi_type_uint64, "ffi-type-uint64");
DEFSYM(ffi_type_sint64, "ffi-type-sint64");
DEFSYM(ffi_type_float, "ffi-type-float");
DEFSYM(ffi_type_double, "ffi-type-double");
DEFSYM(ffi_type_longdouble, "ffi-type-longdouble");
DEFSYM(ffi_type_pointer, "ffi-type-pointer");
DEFSYM(ffi_type_bool, "ffi-type-bool");
DEFSYM(ffi_type_string, "ffi-type-string");
DEFSYM(ffi_type_object, "ffi-type-object");

repv
rep_dl_init(void)
{
  repv tem = rep_push_structure("rep.ffi");
  
  rep_INTERN(ffi_type_void);
  rep_INTERN(ffi_type_uint8);
  rep_INTERN(ffi_type_sint8);
  rep_INTERN(ffi_type_uint16);
  rep_INTERN(ffi_type_sint16);
  rep_INTERN(ffi_type_uint32);
  rep_INTERN(ffi_type_sint32);
  rep_INTERN(ffi_type_uint64);
  rep_INTERN(ffi_type_sint64);
  rep_INTERN(ffi_type_float);
  rep_INTERN(ffi_type_double);
  rep_INTERN(ffi_type_longdouble);
  rep_INTERN(ffi_type_pointer);
  rep_INTERN(ffi_type_bool);
  rep_INTERN(ffi_type_string);
  rep_INTERN(ffi_type_object);
  
#ifdef HAVE_LIBFFI
  /* order of initialization must match rep_ffi_types enum. */
  Fset(Qffi_type_void, ffi_add_primitive_type(&ffi_type_void));
  Fset(Qffi_type_uint8, ffi_add_primitive_type(&ffi_type_uint8));
  Fset(Qffi_type_sint8, ffi_add_primitive_type(&ffi_type_sint8));
  Fset(Qffi_type_uint16, ffi_add_primitive_type(&ffi_type_uint16));
  Fset(Qffi_type_sint16, ffi_add_primitive_type(&ffi_type_sint16));
  Fset(Qffi_type_uint32, ffi_add_primitive_type(&ffi_type_uint32));
  Fset(Qffi_type_sint32, ffi_add_primitive_type(&ffi_type_sint32));
  Fset(Qffi_type_uint64, ffi_add_primitive_type(&ffi_type_uint64));
  Fset(Qffi_type_sint64, ffi_add_primitive_type(&ffi_type_sint64));
  Fset(Qffi_type_float, ffi_add_primitive_type(&ffi_type_float));
  Fset(Qffi_type_double, ffi_add_primitive_type(&ffi_type_double));
  Fset(Qffi_type_longdouble, ffi_add_primitive_type(&ffi_type_longdouble));
  Fset(Qffi_type_pointer, ffi_add_primitive_type(&ffi_type_pointer));
  rep_static_assert(sizeof(bool) == sizeof(uint8_t));
  Fset(Qffi_type_bool, ffi_add_custom_type(&ffi_type_uint8, rep_FFI_BOOL));
  Fset(Qffi_type_string, ffi_add_custom_type(&ffi_type_pointer, rep_FFI_STRING));
# ifdef HAVE_FFI_OBJECTS
  Fset(Qffi_type_object, ffi_add_custom_type(&ffi_type_pointer, rep_FFI_OBJECT));
# else
  Fset(Qffi_type_object, rep_nil);
# endif
#else
  Fset(Qffi_type_void, rep_nil);
  Fset(Qffi_type_uint8, rep_nil);
  Fset(Qffi_type_sint8, rep_nil);
  Fset(Qffi_type_uint16, rep_nil);
  Fset(Qffi_type_sint16, rep_nil);
  Fset(Qffi_type_uint32, rep_nil);
  Fset(Qffi_type_sint32, rep_nil);
  Fset(Qffi_type_uint64, rep_nil);
  Fset(Qffi_type_sint64, rep_nil);
  Fset(Qffi_type_float, rep_nil);
  Fset(Qffi_type_double, rep_nil);
  Fset(Qffi_type_longdouble, rep_nil);
  Fset(Qffi_type_pointer, rep_nil);
  Fset(Qffi_type_bool, rep_nil);
  Fset(Qffi_type_string, rep_nil);
  Fset(Qffi_type_object, rep_nil);
#endif
  
  Fexport_binding(Qffi_type_void);
  Fexport_binding(Qffi_type_uint8);
  Fexport_binding(Qffi_type_sint8);
  Fexport_binding(Qffi_type_uint16);
  Fexport_binding(Qffi_type_sint16);
  Fexport_binding(Qffi_type_uint32);
  Fexport_binding(Qffi_type_sint32);
  Fexport_binding(Qffi_type_uint64);
  Fexport_binding(Qffi_type_sint64);
  Fexport_binding(Qffi_type_float);
  Fexport_binding(Qffi_type_double);
  Fexport_binding(Qffi_type_longdouble);
  Fexport_binding(Qffi_type_pointer);
  Fexport_binding(Qffi_type_bool);
  Fexport_binding(Qffi_type_string);
  Fexport_binding(Qffi_type_object);
  
  rep_ADD_SUBR(Sffi_array);
  rep_ADD_SUBR(Sffi_struct);
  rep_ADD_SUBR(Sffi_type);
  rep_ADD_SUBR(Sffi_interface);
  rep_ADD_SUBR(Sffi_apply);
  
  rep_ADD_SUBR(Sffi_load_library);
  rep_ADD_SUBR(Sffi_lookup_symbol);
  
  rep_ADD_SUBR(Sffi_new);
  rep_ADD_SUBR(Sffi_delete);
  rep_ADD_SUBR(Sffi_address_of);
  rep_ADD_SUBR(Sffi_set_);
  rep_ADD_SUBR(Sffi_get);

#ifdef HAVE_FFI_OBJECTS
  rep_ffi_objects_init();
#endif
  
  return rep_pop_structure(tem);
}
