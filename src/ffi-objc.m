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
  rep_ffi_method_info *methods;
};

struct rep_ffi_method_info_struct {
  rep_ffi_method_info *next;
  SEL sel;
  int n_args;
  int ffi_type;
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

  for (int i = 1; i < PROXIES_PER_BLOCK - 1; i++) {
    b->data[i].car = 0;
    b->data[i].next = &b->data[i + 1];
  }
  b->data[PROXIES_PER_BLOCK - 1].car = 0;
  b->data[PROXIES_PER_BLOCK - 1].next = proxy_free_list;

  proxy_free_list = &b->data[1];

  return &b->data[0];
}

static rep_ffi_class_info *
class_info_ref(Class cls)
{
  /* FIXME: use a hash table here? May not be a hot path. */

  for (rep_ffi_class_info *c = classes; c; c = c->next) {
    if (c->cls == cls) {
      return c;
    }
  }

  rep_ffi_class_info *c = rep_alloc(sizeof(*c));

  c->cls = cls;
  c->methods = NULL;			/* FIXME: implement */

  c->next = classes;
  classes = c;

  return c;
}

repv
rep_ffi_make_object(void *ptr)
{
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
  }

  DEFSTRING(err, "ffi expected an objc reference");
  Fsignal(Qerror, rep_list_2(rep_VAL(&err), obj));
  return NULL;
}

static void
proxy_sweep(void)
{
  rep_ffi_proxy *free_list = NULL;

  for (rep_ffi_proxy_block *b = proxy_block_list; b; b = b->next) {
    for (int i = 0; i < PROXIES_PER_BLOCK; i++) {
      rep_ffi_proxy *p = &b->data[i];
      if (!rep_GC_CELL_MARKEDP(rep_VAL(p))) {
	/* FIXME: safe to do this here? */
	[p->obj release];
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
      .print = proxy_print,
      .sweep = proxy_sweep,
    };

    type = rep_define_type(&proxy);
  }

  return type;
}

DEFUN("objc-class", Fobjc_class, Sobjc_class, (repv name), rep_Subr1) /*
::doc:rep.ffi#objc-class::
objc-class NAME

Returns a proxy object referring to the Objective class called NAME, or
nil if no such class exists.
::end:: */
{
  rep_DECLARE1(name, rep_STRINGP);

  Class cls = objc_lookUpClass(rep_STR(name));

  if (cls == Nil) {
    NSString *str = [[NSString alloc] initWithUTF8String:rep_STR(name)];
    cls = NSClassFromString(str);
    [str release];
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
