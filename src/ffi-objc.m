/* ffi-objc.m -- objective C ffi code

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

#include "rep-ffi.h"

#include "pointer-hash.h"

#import <CoreFoundation/CoreFoundation.h>
#import <Foundation/Foundation.h>
#import <objc/message.h>
#import <objc/runtime.h>

#define PROXIES_PER_BLOCK 127

typedef struct rep_ffi_proxy_struct rep_ffi_proxy;
typedef struct rep_ffi_class_info_struct rep_ffi_class_info;
typedef struct rep_ffi_method_info_struct rep_ffi_method_info;
typedef struct rep_ffi_proxy_block_struct rep_ffi_proxy_block;

struct rep_ffi_proxy_struct {
  repv car;
  id obj;
  rep_ffi_class_info *info;
  rep_ffi_proxy *next;
};

struct rep_ffi_class_info_struct {
  rep_ffi_class_info *next;
  Class cls;
  rep_ffi_method_info *class_methods;
  rep_ffi_method_info *instance_methods;
};

struct rep_ffi_method_info_struct {
  rep_ffi_method_info *next;
  repv name;
  SEL sel;
  int ffi_interface;
  int16_t n_args;
  int16_t ret_type;
};

struct rep_ffi_proxy_block_struct {
  rep_ffi_proxy_block *next;
  rep_ALIGN_CELL(rep_ffi_proxy data[PROXIES_PER_BLOCK]);
};

static repv proxy_type(void);

static rep_ffi_proxy_block *proxy_block_list;
static rep_ffi_proxy *proxy_free_list;

static rep_ffi_proxy **proxy_table;
static size_t proxy_table_size;

static rep_ffi_class_info *classes;

#define PROXYP(v) rep_CELL16_TYPEP(v, proxy_type())
#define PROXY(v) ((rep_ffi_proxy *)rep_PTR(v))

static rep_ffi_proxy *
refill_free_list(void)
{
  rep_ffi_proxy_block *b = rep_alloc(sizeof(rep_ffi_proxy_block));

  b->next = proxy_block_list;
  proxy_block_list = b;

  for (int i = 1; i < PROXIES_PER_BLOCK; i++) {
    b->data[i].car = 0;
    b->data[i].obj = 0;
    b->data[i].next = &b->data[i + 1];
  }

  b->data[PROXIES_PER_BLOCK - 1].next = proxy_free_list;
  proxy_free_list = &b->data[1];

  return &b->data[0];
}

static rep_ffi_class_info *
class_info_ref(Class cls)
{
  /* FIXME: hash table? */

  for (rep_ffi_class_info *c = classes; c; c = c->next) {
    if (c->cls == cls) {
      return c;
    }
  }

  rep_ffi_class_info *c = rep_alloc(sizeof(*c));

  c->cls = cls;
  c->class_methods = NULL;
  c->instance_methods = NULL;

  c->next = classes;
  classes = c;

  return c;
}

repv
rep_ffi_make_object(void *ptr)
{
  if (!ptr) {
    return rep_nil;
  }

  uintptr_t hash = pointer_hash((uintptr_t)ptr);

  rep_ffi_proxy *p;

  if (proxy_table) {
    for (p = proxy_table[hash & (proxy_table_size - 1)]; p; p = p->next) {
      if (p->obj == (id)ptr) {
	return rep_VAL(p);
      }
    }
  }

  p = proxy_free_list;
  if (p) {
    proxy_free_list = p->next;
  } else {
    p = refill_free_list();
  }

  p->car = proxy_type();
  p->obj = [(id)ptr retain];
  p->info = class_info_ref([(id)ptr class]);

  if (!proxy_table) {
    proxy_table_size = 64;
    proxy_table = calloc(proxy_table_size, sizeof(proxy_table[0]));
  }

  p->next = proxy_table[hash & (proxy_table_size - 1)];
  proxy_table[hash & (proxy_table_size - 1)] = p;

  return rep_VAL(p);
}

void *
rep_ffi_object_pointer(repv obj)
{
  if (PROXYP(obj)) {
    return (void *)PROXY(obj)->obj;
  } else if (obj == rep_nil) {
    return NULL;
  } else {
    DEFSTRING(err, "ffi expected an objc reference");
    Fsignal(Qerror, rep_list_2(rep_VAL(&err), obj));
    return NULL;
  }
}

static int parse_objc_type(const char **str_ptr);

static int
parse_objc_array_type(const char **str_ptr)
{
  /* "[NT]" where N is decimal number of values, T is type. */

  const char *str = *str_ptr + 1;
  int count = 0;

again:
  switch (*str++) {
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    count = count * 10 + (str[-1] - '0');
    goto again;

  default:
    break;
  }

  int type = parse_objc_type(&str);
  if (type < 0) {
    return -1;
  }

  type = rep_ffi_get_array_type(count, type);
  if (type < 0) {
    return -1;
  }

  *str_ptr = str + 1;
  return type;
}

static int
parse_objc_struct_type(const char **str_ptr)
{
  /* "{NAME=TT..}" */

  const char *str = *str_ptr + 1;

  while (*str != 0) {
    if (*str++ == '=') {
      break;
    }
  }

  unsigned int types[64];		/* FIXME: lazy */
  size_t count = 0;

  while (1) {
    int type = parse_objc_type(&str);
    if (type < 0) {
      break;
    }
    if (count == sizeof(types) / sizeof(types[0])) {
      return -1;
    }
    types[count++] = type;
  }

  int type = rep_ffi_get_struct_type(count, types);
  if (type < 0) {
    return -1;
  }

  *str_ptr = str + 1;
  return type;
}

static int
parse_objc_pointer_type(const char **str_ptr)
{
  /* "^T" */

  /* FIXME: need ffi.c support for typed pointers. */

  return -1;
}

static int
parse_objc_type(const char **str_ptr)
{
  const char *str = *str_ptr;
  int ret = -1;

again:
  switch (*str++) {
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    goto again;

  case 'r':				/* const */
  case 'n':				/* in */
  case 'N':				/* inout */
  case 'o':				/* out */
  case 'O':				/* bycopy */
  case 'R':				/* byref */
  case 'V':				/* oneway */
    goto again;

  case 'c':				/* char (or BOOL) */
    ret = rep_FFI_TYPE_BOOL;
    break;
  case 'i':				/* int */
    ret = rep_FFI_TYPE_SINT32;
    break;
  case 's':				/* short */
    ret = rep_FFI_TYPE_SINT16;
    break;
  case 'l':				/* long */
    return sizeof(long) == 4 ? rep_FFI_TYPE_SINT32 : rep_FFI_TYPE_SINT64;
  case 'q':				/* long long */
    ret = rep_FFI_TYPE_SINT64;
    break;
  case 'C':				/* unsigned char */
    ret = rep_FFI_TYPE_UINT8;
    break;
  case 'I':				/* unsigned int */
    ret = rep_FFI_TYPE_UINT32;
    break;
  case 'S':				/* unsigned short */
    ret = rep_FFI_TYPE_UINT16;
    break;
  case 'L':				/* unsigned long */
    return sizeof(long) == 4 ? rep_FFI_TYPE_UINT32 : rep_FFI_TYPE_UINT64;
  case 'Q':				/* unsigned long long */
    ret = rep_FFI_TYPE_UINT64;
    break;
  case 'f':				/* float */
    ret = rep_FFI_TYPE_FLOAT;
    break;
  case 'd':				/* double */
    ret = rep_FFI_TYPE_DOUBLE;
    break;
  case 'B':				/* bool */
    ret = rep_FFI_TYPE_BOOL;
    break;
  case 'v':				/* void */
    ret = rep_FFI_TYPE_VOID;
    break;
  case '*':				/* char * */
    ret = rep_FFI_TYPE_STRING;
    break;
  case '@':				/* id */
  case '#':				/* Class */
    ret = rep_FFI_TYPE_OBJECT;
    break;
  case ':':				/* SEL */
    ret = rep_FFI_TYPE_POINTER;
    break;
  case '[':				/* array */
    ret = parse_objc_array_type(&str);
    break;
  case '{':				/* struct */
    ret = parse_objc_struct_type(&str);
    break;
  case '(':				/* union */
    return -1;
  case 'b':				/* bitfield */
    return -1;
  case '^':				/* pointer */
    ret = parse_objc_pointer_type(&str);
    break;
  case '?':				/* unknown */
    return -1;
  case ']':
  case ')':
  case '}':
    return -1;
  default:
    fprintf(stderr, "rep-ffi: unexpected objc type: %s\n", str - 1);
    return -1;
  }

  *str_ptr = str;
  return ret;
}

static const rep_ffi_method_info *
proxy_method(rep_ffi_proxy *p, repv name)
{
  rep_ffi_class_info *info = p->info;
  bool class_method = p->obj == info->cls;

  /* FIXME: hash table? */

  rep_ffi_method_info *m = class_method ? info->class_methods :
    info->instance_methods;

  for (; m; m = m->next) {
    if (m->name == name) {
      return m;
    }
  }

  SEL sel = sel_registerName(rep_STR(rep_SYM(name)->name));
  Class cls = object_isClass(p->obj) ? p->obj : [p->obj class];

  Method method;
  if (cls == p->obj) {
    method = class_getClassMethod(cls, sel);
  } else {
    method = class_getInstanceMethod(cls, sel);
  }
  if (!method) {
    return NULL;
  }

  const char *type = method_getTypeEncoding(method);
  if (!type) {
    return NULL;
  }

  int ret_type = parse_objc_type(&type);
  if (ret_type < 0) {
    return NULL;
  }

  /* number returned includes self and _cmd. */

  unsigned int n_args = method_getNumberOfArguments(method);
  unsigned int arg_types[n_args];

  for (unsigned int i = 0; i < n_args; i++) {
    int ti = parse_objc_type(&type);
    if (ti < 0) {
      return NULL;
    }
    arg_types[i] = ti;
  }

  int iface_id = rep_ffi_get_interface(ret_type, n_args, arg_types);
  if (iface_id < 0) {
    return NULL;
  }

  m = rep_alloc(sizeof(rep_ffi_method_info));
  m->name = name;
  m->sel = sel;
  m->ffi_interface = iface_id;
  m->n_args = n_args;
  m->ret_type = 0;

  const rep_ffi_interface *iface = rep_ffi_interface_ref(iface_id);
  const rep_ffi_type *ret = rep_ffi_type_ref(iface->ret);

  if (ret->subtype == rep_FFI_STRUCT) {
    /* FIXME: libffi knows exactly when the return value is in memory,
       but the way it stores that in the internal fields of ffi_cif has
       changed between versions, so there's no good way we can use
       that. Instead reuse some ad hoc rules from pyobjc. */

    switch (ret->type->size) {
#if defined(__i386__)
    case 1: case 2: case 4: case 8:
      break;
    default:
      m->ret_type = 1;		/* _stret */
#elif defined(__x86_64__)
    case 1: case 2: case 4: case 8: case 16:
      break;
    default:
      m->ret_type = 1;		/* _stret */
#elif defined(__arm__)
#else
#error "Unknown _stret ABI."
#endif
    }

  } else if (ret->subtype == rep_FFI_PRIMITIVE) {
    /* FIXME: ignoring _fp2ret variant. */

#if defined(__i386__)
    if (ret->type == ffi_type_float || ffi_type_double) {
      m->ret_type = 2;		/* _fpret */
    }
#elif defined(__x86_64) || defined(__arm__)
    /* no _fpret */
#else
#error "Unknown _fpret ABI."
#endif
  }

  if (class_method) {
    m->next = info->class_methods;
    info->class_methods = m;
  } else {
    m->next = info->instance_methods;
    info->instance_methods = m;
  }

  return m;
}

static repv
proxy_apply(repv obj, int argc, repv *argv)
{
  if (argc < 1) {
    return rep_signal_missing_arg(1);
  }

  repv name = argv[0];
  
  rep_DECLARE1(name, rep_SYMBOLP);

  const rep_ffi_method_info *method = proxy_method(PROXY(obj), name);

  if (!method) {
    return Fsignal(Qinvalid_function, rep_list_2(obj, name));
  }

  void *fun;
  switch (method->ret_type) {
  case 0:
  default:
    fun = objc_msgSend;
    break;
#if !defined(__arm__)
  case 1:
    fun = objc_msgSend_stret;
    break;
  case 2:
    fun = objc_msgSend_fpret;
    break;
#if !defined(__i386__)
  case 3:
    fun = objc_msgSend_fp2ret;
    break;
#endif
#endif
  }

  int method_argc = argc + 1;
  repv *method_argv = rep_stack_alloc(repv, method_argc);
  if (!method_argv) {
    return rep_mem_error();
  }
  method_argv[0] = obj;
  method_argv[1] = rep_make_pointer((void *)method->sel);
  memcpy(method_argv + 2, argv + 1, (argc - 1) * sizeof(repv));

  return rep_ffi_apply(method->ffi_interface, fun, method_argc, method_argv);
}

static void
proxy_mark_type(void)
{
  for (rep_ffi_class_info *c = classes; c; c = c->next) {
    for (rep_ffi_method_info *m = c->class_methods; m; m = m->next) {
      rep_MARKVAL(m->name);
    }
    for (rep_ffi_method_info *m = c->instance_methods; m; m = m->next) {
      rep_MARKVAL(m->name);
    }
  }
}

static void
proxy_sweep(void)
{
  rep_ffi_proxy *free_list = NULL;

  for (rep_ffi_proxy_block *b = proxy_block_list; b; b = b->next) {
    for (int i = 0; i < PROXIES_PER_BLOCK; i++) {
      rep_ffi_proxy *p = &b->data[i];
      if (!rep_GC_CELL_MARKEDP(rep_VAL(p))) {
	if (p->obj) {
	  /* FIXME: safe to do this here? */
	  [p->obj release];
	  p->obj = NULL;
	}
	p->next = free_list;
	free_list = p;
      } else {
	rep_GC_CLR_CELL(rep_VAL(p));
      }
    }
  }

  proxy_free_list = free_list;
}

static void
proxy_print(repv stream, repv arg)
{    
  char buf[256];

  bool is_cls = object_isClass(PROXY(arg)->obj);
  rep_ffi_class_info *info = PROXY(arg)->info;
  const char *class_name = info ? class_getName(info->cls) : NULL;

  if (class_name) {
    if (is_cls) {
      snprintf(buf, sizeof(buf), "%s", class_name);
    } else {
      snprintf(buf, sizeof(buf), "%s:%p", class_name, PROXY(arg)->obj);
    }
  } else {
    snprintf(buf, sizeof(buf), "%p", PROXY(arg)->obj);
  }

  rep_stream_puts(stream, is_cls ? "#<objc-class " : "#<objc-obj ", -1, false);
  rep_stream_puts(stream, buf, -1, false);
  rep_stream_putc(stream, '>');
}

static repv
proxy_type(void)
{
  static repv type;

  if (!type) {
    static rep_type proxy = {
      .name = "objc-proxy",
      .apply = proxy_apply,
      .print = proxy_print,
      .mark_type = proxy_mark_type,
      .sweep = proxy_sweep,
    };

    type = rep_define_type(&proxy);
  }

  return type;
}

DEFUN("objc-class", Fobjc_class, Sobjc_class, (repv name), rep_Subr1) /*
::doc:rep.ffi#objc-class::
objc-class NAME

Returns a proxy object referring to the Objective class called NAME (a
symbol), or nil if no such class exists.
::end:: */
{
  rep_DECLARE1(name, rep_SYMBOLP);

  const char *name_str = rep_STR(rep_SYM(name)->name);
  Class cls = objc_lookUpClass(name_str);

  if (cls == Nil) {
    NSString *s = [[NSString alloc] initWithUTF8String:name_str];
    cls = NSClassFromString(s);
    [s release];
  }

  if (cls == Nil) {
    return rep_nil;
  } else {
    return rep_ffi_make_object(cls);
  }
}

void
rep_ffi_objects_init(void)
{
  rep_ADD_SUBR(Sobjc_class);
}
