/* strings.c -- string functions

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

#include "utf8-utils.h"

#include <string.h>
#include <ctype.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

struct rep_string_utf32_struct {
  size_t len;
  bool dirty;
  int8_t ttl;
  uint32_t data[1];
};

#define SIZEOF_UTF32(l) (sizeof(rep_string_utf32) + ((l)-1) * sizeof(uint32_t))
#define UTF32_TTL 4

#define STRING_LEN(car) ((car) >> rep_STRING_LEN_SHIFT)

#define STRINGS_PER_BLOCK 510 /* ~4k */

typedef struct string_block_struct string_block;

struct string_block_struct {
  string_block *next;
  rep_ALIGN_CELL(rep_string data[STRINGS_PER_BLOCK]);
};

static string_block *string_block_list;
static rep_string *string_freelist;

int rep_allocated_strings, rep_used_strings;
size_t rep_allocated_string_bytes;

DEFSYM(control, "control");
DEFSYM(newlines, "newlines");

DEFSTRING(null_string_const, "");
DEFSTRING(string_overflow, "String too long");

static rep_string *
refill_free_list(void)
{
  string_block *cb = rep_alloc(sizeof(string_block));
  rep_allocated_strings += STRINGS_PER_BLOCK;

  cb->next = string_block_list;
  string_block_list = cb;

  for (int i = 1; i < STRINGS_PER_BLOCK - 1; i++) {
    cb->data[i].car = rep_VAL(&cb->data[i + 1]);
  }
  cb->data[STRINGS_PER_BLOCK - 1].car = 0;
  string_freelist = &cb->data[1];

  return &cb->data[0];
}

/* PTR should have been allocated using rep_alloc or malloc. Ownership
   of its memory passes to the lisp system. LEN _doesn't_ include the zero
   terminator */

repv
rep_box_string(char *ptr, size_t len)
{
  if (len > rep_MAX_STRING_LEN) {
    return Fsignal(Qerror, rep_LIST_1(rep_VAL(&string_overflow)));
  }

  rep_string *str = string_freelist;

  if (str) {
    string_freelist = rep_STRING(str->car);
  } else {
    str = refill_free_list();
  }

  str->car = rep_String | (len << rep_STRING_LEN_SHIFT);
  str->utf8_data = (uint8_t *)ptr;
  str->utf32_data = 0;

  rep_used_strings++;
  rep_data_after_gc += sizeof(rep_string) + len;

  return rep_VAL(str);
}

repv
rep_null_string(void)
{
  return rep_VAL(&null_string_const);
}

/* Return a string object with room for exactly LEN characters. No extra
   byte is allocated for a zero terminator; do this manually if required. */

repv
rep_allocate_string(size_t len)
{
  char *data = rep_alloc(len);
  if (data) {
    return rep_box_string(data, len - 1);
  } else {
    return 0;
  }
}

repv
rep_string_copy_n(const char *src, size_t slen)
{
  rep_string *dst = rep_STRING(rep_allocate_string(slen + 1));

  if(dst) {
    memcpy(dst->utf8_data, src, slen);
    dst->utf8_data[slen] = 0;
  }

  return rep_VAL(dst);
}

repv
rep_string_copy(const char *src)
{
  return rep_string_copy_n(src, strlen(src));
}

repv
rep_string_concat2(const char *s1, const char *s2)
{
  int len = strlen(s1) + strlen(s2);
  repv res = rep_allocate_string(len + 1);
  stpcpy(stpcpy((char *)rep_STRING(res)->utf8_data, s1), s2);
  return(res);
}

repv
rep_string_concat3(const char *s1, const char *s2, const char *s3)
{
  int len = strlen(s1) + strlen(s2) + strlen(s3);
  repv res = rep_allocate_string(len + 1);
  stpcpy(stpcpy(stpcpy((char *)rep_STRING(res)->utf8_data, s1), s2), s3);
  return(res);
}

repv
rep_string_concat4(const char *s1, const char *s2, const char *s3, const char *s4)
{
  int len = strlen(s1) + strlen(s2) + strlen(s3) + strlen(s4);
  repv res = rep_allocate_string(len + 1);
  stpcpy(stpcpy(stpcpy(stpcpy((char *)rep_STRING(res)->utf8_data, s1), s2), s3), s4);
  return(res);
}

static intptr_t
length_utf32(rep_string *s)
{
  repv v = s->utf32_data;
  if (v) {
    if (rep_INTP(v)) {
      return rep_INT(v);
    } else {
      rep_string_utf32 *u = (rep_string_utf32 *)v;
      return u->len;
    }
  }

  size_t len = utf8_string_size(s->utf8_data, STRING_LEN(s->car));
  s->utf32_data = rep_MAKE_INT(len);

  return len;
}

static bool
ascii_string_p(rep_string *s)
{
  return length_utf32(s) == STRING_LEN(s->car);
}

static bool
flatten_utf32(rep_string *s)
{
  repv v = s->utf32_data;
  if (!v || rep_INTP(v)) {
    return true;
  }

  rep_string_utf32 *u = (rep_string_utf32 *)v;
  if (!u->dirty) {
    return true;
  }

  ssize_t size = utf32_to_utf8_size(u->data, u->len);
  uint8_t *ptr = rep_realloc(s->utf8_data, size + 1);

  if (!ptr) {
    return false;
  }

  utf32_to_utf8(ptr, u->data, u->len);
  ptr[size] = 0;
  s->utf8_data = ptr;

  uintptr_t len_mask = (~((uintptr_t)0)) << rep_STRING_LEN_SHIFT;
  s->car = (s->car & ~len_mask) | (size << rep_STRING_LEN_SHIFT);

  u->dirty = false;

  return true;
}

static rep_string_utf32 *
ensure_utf32(rep_string *s, bool force)
{
  repv v = s->utf32_data;
  if (v && !rep_INTP(v)) {
    rep_string_utf32 *u = (rep_string_utf32 *)v;
    u->ttl = UTF32_TTL;
    return u;
  }

  if (v && !force && rep_INT(v) == STRING_LEN(s->car)) {
    /* size == len => ASCII. */
    return NULL;
  }

  size_t len;
  if (v) {
    len = rep_INT(v);
  } else {
    len = utf8_string_size(s->utf8_data, STRING_LEN(s->car));
    if (!force && len == STRING_LEN(s->car)) {
      /* size == len => ASCII. */
      s->utf32_data = rep_MAKE_INT(len);
      return NULL;
    }
  }

  rep_string_utf32 *u = rep_alloc(SIZEOF_UTF32(len));
  if (!u) {
    return NULL;
  }

  rep_data_after_gc += SIZEOF_UTF32(len);

  u->len = len;
  utf8_to_utf32(u->data, s->utf8_data, STRING_LEN(s->car));
  u->dirty = false;
  u->ttl = UTF32_TTL;

  s->utf32_data = rep_VAL(u);

  return u;
}

static void
free_utf32(rep_string *s)
{
  repv v = s->utf32_data;
  if (v && !rep_INTP(v)) {
    rep_string_utf32 *u = (rep_string_utf32 *)v;
    size_t len = u->len;
    rep_free(u);
    s->utf32_data = rep_MAKE_INT(len);
  }
}

static void
flatten_and_free_utf32(rep_string *s)
{
  flatten_utf32(s);
  free_utf32(s);
}

static void
collect_utf32(rep_string *s)
{
  repv v = s->utf32_data;
  if (v && !rep_INTP(v)) {
    rep_string_utf32 *u = (rep_string_utf32 *)v;
    if (--u->ttl <= 0) {
      flatten_and_free_utf32(s);
    }
  }
}

void
rep_string_set_ascii(repv s)
{
  if (!rep_STRING(s)->utf32_data) {
    rep_STRING(s)->utf32_data = rep_MAKE_INT(STRING_LEN(rep_STRING(s)->car));
  }
}

intptr_t
rep_string_ptr_size(repv s)
{
  /* flatten_utf32() may change size of UTF-8 string. */

  repv v = rep_STRING(s)->utf32_data;
  if (v && !rep_INTP(v)) {
    flatten_utf32(rep_STRING(s));
  }

  return STRING_LEN(rep_STRING(s)->car);
}

const char *
rep_string_ptr(repv s)
{
  repv v = rep_STRING(s)->utf32_data;
  if (v && !rep_INTP(v)) {
    flatten_utf32(rep_STRING(s));
  }

  return (const char *)rep_STRING(s)->utf8_data;
}

char *
rep_string_mutable_ptr(repv s)
{
  /* Have to assume that writer could modify UTF-8 sequences,
     invalidating cached number of code points. */

  if (rep_STRING(s)->utf32_data) {
    flatten_and_free_utf32(rep_STRING(s));
    rep_STRING(s)->utf32_data = 0;
  }

  return (char *)rep_STRING(s)->utf8_data;
}

int
rep_string_cmp(repv v1, repv v2)
{
  if (!rep_STRINGP(v2)) {
    return 1;
  }
  size_t len1 = rep_STRING_LEN(v1);
  size_t len2 = rep_STRING_LEN(v2);
  int tem = memcmp(rep_STR(v1), rep_STR(v2), MIN(len1, len2));
  return tem != 0 ? tem : (len1 - len2);
}

void
rep_string_princ(repv strm, repv obj)
{
  rep_stream_puts(strm, rep_PTR(obj), -1, true);
}

void
rep_string_print(repv strm, repv obj)
{
  intptr_t len = rep_STRING_LEN(obj);
  const char *s = rep_STR(obj);

  bool escape_all = false, escape_control = false, escape_newlines = false;
  repv tem = Fsymbol_value(Qprint_escape, Qt);
  if (tem == Qnewlines) {
    escape_newlines = true;
  } else if (tem == Qcontrol) {
    escape_control = escape_newlines = true;
  } else if (tem == Qt) {
    escape_all = escape_control = escape_newlines = true;
  }

  const size_t max_chars_per_iter = 4;

  char buf[256];
  size_t i = 0;

  buf[i++] = '"';

  while (len-- > 0) {
    // "+ 1" for terminating quote after loop ends
    if (i + max_chars_per_iter + 1 >= sizeof(buf)) {
      rep_stream_puts(strm, buf, i, false);
      i = 0;
    }

    uint8_t c = *s++;

    switch (c) {
    case '\t':
    case '\n':
    case '\r':
    case '\f':
      if (!escape_newlines) {
	buf[i++] = c;
      } else {
	buf[i++] = '\\';
	buf[i++] = c == '\t' ? 't' : c == '\n' ? 'n' : c == '\r' ? 'r' : 'f';
      }
      break;

    case '\\':
      buf[i++] = '\\';
      buf[i++] = '\\';
      break;

    case '"':
      buf[i++] = '\\';
      buf[i++] = '"';
      break;

    default:
      if (c < 32 || c > 126) {
	if (c < 127 ? escape_control : escape_all) {
	  buf[i++] = '\\';
	  buf[i++] = '0' + ((c >> 6) & 7);
	  buf[i++] = '0' + ((c >> 3) & 7);
	  buf[i++] = '0' + ((c >> 0) & 7);
	  break;
	}
      }
      buf[i++] = c;
      break;
    }
  }

  buf[i++] = '"';

  if (i > 0) {
    rep_stream_puts(strm, buf, i, false);
  }
}

void
rep_string_sweep(void)
{
  string_block *cb = string_block_list;

  string_block_list = NULL;
  string_freelist = NULL;

  rep_used_strings = 0;
  rep_allocated_string_bytes = 0;

  while (cb) {
    string_block *next = cb->next;

    rep_string *free_list = NULL;
    rep_string *free_tail = NULL;
    int used_strings = 0;

    for (int i = 0; i < STRINGS_PER_BLOCK; i++) {
      repv str = rep_VAL(&cb->data[i]);

      /* If on the freelist then the CELL_IS_8 bit will be unset (since
	 the pointer is long aligned). */

      if (rep_CELL_CONS_P(str) || !rep_GC_CELL_MARKEDP(str)) {
	if (!free_tail) {
	  free_tail = rep_STRING(str);
	}
	if (!rep_CELL_CONS_P(str)) {
	  rep_free(rep_STRING(str)->utf8_data);
	  free_utf32(rep_STRING(str));
	}
	rep_STRING(str)->car = rep_VAL(free_list);
	free_list = rep_STRING(str);
      } else {
	rep_GC_CLR_CELL(str);
	collect_utf32(rep_STRING(str));
	rep_allocated_string_bytes += rep_STRING_LEN(str);
	used_strings++;
      }
    }

    if (used_strings == 0) {
      rep_free(cb);
      rep_allocated_strings -= STRINGS_PER_BLOCK;
    } else {
      if (free_tail) {
	free_tail->car = rep_VAL(string_freelist);
	string_freelist = free_list;
	rep_used_strings += used_strings;
      }
      cb->next = string_block_list;
      string_block_list = cb;
    }

    cb = next;
  }
}

/* Sets the length-field of the dynamic string STR to LEN. */

bool
rep_string_set_len(repv str, size_t len)
{
  if (!rep_STRING_WRITABLE_P(str)) {
    return false;
  }

  free_utf32(rep_STRING(str));

  uintptr_t len_mask = (~((uintptr_t)0)) << rep_STRING_LEN_SHIFT;

  rep_STRING(str)->car = ((rep_STRING(str)->car & ~len_mask)
			  | (len << rep_STRING_LEN_SHIFT));
  return true;
}

intptr_t
rep_stream_put_utf8(repv stream, repv str, intptr_t count)
{
  flatten_utf32(rep_STRING(str));

  const uint8_t *ptr = rep_STRING(str)->utf8_data;
  intptr_t size = STRING_LEN(rep_STRING(str)->car);
  intptr_t bytes = size;

  if (count >= 0) {
    if (!ascii_string_p(rep_STRING(str))) {
      const uint8_t *end = utf8_skip_characters(ptr, size, count);
      if (end) {
	bytes = end - ptr;
      } else {
	count = length_utf32(rep_STRING(str));
      }
    } else if (count > size) {
      count = size;
    }
  } else {
    count = length_utf32(rep_STRING(str));
  }

  intptr_t written;
  if (bytes == size) {
    written = rep_stream_puts(stream, rep_PTR(str), bytes, true);
  } else {
    written = rep_stream_puts(stream, ptr, bytes, false);
  }

  return written > 0 ? count : written;
}

DEFUN("make-string", Fmake_string,
      Smake_string, (repv len, repv init), rep_Subr2) /*
::doc:rep.data#make-string::
make-string LENGTH [CHARACTER]

Returns a new string of length LENGTH, each character is initialised to
CHARACTER, or to space if CHARACTER is undefined.
::end:: */
{
  rep_DECLARE1(len, rep_NON_NEG_INT_P);

  intptr_t l = rep_INT(len);

  uint32_t c = rep_CHARP(init) ? rep_CHAR_VALUE(init) : ' ';

  size_t cs = utf32_to_utf8_size_1(c);
  size_t cl = cs * l;

  repv s = rep_allocate_string(cl + 1);
  if (!s) {
    return rep_mem_error();
  }

  uint8_t *ptr = rep_STRING(s)->utf8_data;
  if (c < 128) {
    memset(ptr, c, cl);
    ptr += cl;
  } else {
    uint8_t buf[8];
    utf32_to_utf8_1(buf, c);
    for (size_t i = 0; i < cl; i += cs) {
      for (size_t j = 0; j < cs; j++) {
	*ptr++ = buf[j];
      }
    }
  }
  *ptr = 0;

  rep_STRING(s)->utf32_data = rep_MAKE_INT(l);
  return s;
}

DEFUN("string?", Fstringp, Sstringp, (repv arg), rep_Subr1) /*
::doc:rep.data#string?::
string? ARG

Returns t is ARG is a string.
::end:: */
{
  return rep_STRINGP(arg) ? Qt : rep_nil;
}

DEFUN("byte-string-length", Fbyte_string_length,
      Sbyte_string_length, (repv str), rep_Subr1) /*
::doc:rep.data#byte-string-length::
byte-string-length STRING

Returns the number of bytes in STRING.
::end:: */
{
  rep_DECLARE1(str, rep_STRINGP);

  return rep_MAKE_INT(rep_STRING_LEN(str));
}

DEFUN("byte-string-ref", Fbyte_string_ref,
      Sbyte_string_ref, (repv str, repv idx), rep_Subr2) /*
::doc:rep.data#byte-string-ref::
byte-string-ref STRING INDEX

Returns the INDEX'th byte in STRING.
::end:: */
{
  rep_DECLARE1(str, rep_STRINGP);
  rep_DECLARE1(idx, rep_NON_NEG_INT_P);

  if (rep_INT(idx) >= rep_STRING_LEN(str)) { 
    return rep_signal_arg_error(idx, 2);
  }

  return rep_MAKE_INT((uint8_t)(rep_STR(str)[rep_INT(idx)]));
}

DEFUN("byte-string-set!", Fbyte_string_set,
      Sbyte_string_set, (repv str, repv idx, repv value), rep_Subr3) /*
::doc:rep.data#byte-string-set!::
byte-string-set! STRING INDEX INTEGER

Sets the INDEX'th byte in STRING to INTEGER.
::end:: */
{
  rep_DECLARE1(str, rep_STRINGP);
  rep_DECLARE2(idx, rep_NON_NEG_INT_P);
  rep_DECLARE3(value, rep_NON_NEG_INT_P);

  if (!rep_STRING_WRITABLE_P(str)) {
    return Fsignal(Qsetting_constant, rep_LIST_1(str));
  }

  if (rep_INT(idx) >= rep_STRING_LEN(str)) { 
    return rep_signal_arg_error(idx, 2);
  }

  uint32_t c = rep_INT(value);
  if (c > 255) { 
    return rep_signal_arg_error(value, 3);
  }

  rep_MUTABLE_STR(str)[rep_INT(idx)] = c;
  rep_invalidate_string(str);

  return rep_undefined_value;
}

DEFUN("string-length", Fstring_length,
      Sstring_length, (repv str), rep_Subr1) /*
::doc:rep.data#string-length::
string-length STRING

Returns the number of Unicode code points in STRING.
::doc:: */
{
  rep_DECLARE1(str, rep_STRINGP);

  return rep_MAKE_INT(length_utf32(rep_STRING(str)));
}

DEFUN("string-ref", Fstring_ref,
      Sstring_ref, (repv str, repv idx), rep_Subr2) /*
::doc:rep.data#utf8-string-ref::
string-ref STRING INDEX

Returns the INDEX'th Unicode code point in STRING.
::end:: */
{
  rep_DECLARE1(str, rep_STRINGP);
  rep_DECLARE1(idx, rep_NON_NEG_INT_P);

  /* FIXME: better to interpret UTF-8 string directly? */

  const rep_string_utf32 *u = ensure_utf32(rep_STRING(str), false);

  intptr_t i = rep_INT(idx);

  if (!u) {
    if (i >= STRING_LEN(rep_STRING(str)->car)) {
      return rep_signal_arg_error(idx, 2);
    }

    return rep_intern_char(rep_STRING(str)->utf8_data[i]);
  }

  if (i >= u->len) {
    return rep_signal_arg_error(idx, 2);
  }

  return rep_intern_char(u->data[i]);
}

DEFUN("string-set!", Fstring_set,
      Sstring_set, (repv str, repv idx, repv value), rep_Subr3) /*
::doc:rep.data#string-set!::
string-set! STRING INDEX CHARACTER

Sets the INDEX'th code point in STRING to CHARACTER.
::end:: */
{
  rep_DECLARE1(str, rep_STRINGP);
  rep_DECLARE2(idx, rep_NON_NEG_INT_P);
  rep_DECLARE3(value, rep_CHARP);

  if (!rep_STRING_WRITABLE_P(str)) {
    return Fsignal(Qsetting_constant, rep_LIST_1(str));
  }

  uint32_t c = rep_CHAR_VALUE(value);

  rep_string_utf32 *u = ensure_utf32(rep_STRING(str), c >= 128);

  intptr_t i = rep_INT(idx);

  if (!u) {
    if (i >= STRING_LEN(rep_STRING(str)->car)) { 
      return rep_signal_arg_error(idx, 2);
    }

    rep_STRING(str)->utf8_data[i] = c;
    rep_invalidate_string(str);

    return rep_undefined_value;
  }

  if (i >= u->len) { 
    return rep_signal_arg_error(idx, 2);
  }

  if (u->data[i] != c) {
    u->data[i] = c;
    u->dirty = true;
    rep_invalidate_string(str);
  }

  return rep_undefined_value;
}

DEFUN("make-string-immutable!", Fmake_string_immutable,
      Smake_string_immutable, (repv str), rep_Subr1) /*
::doc:rep.data#make-string-immutable!::
make-string-immutable! STRING

Marks that the contents of STRING may no longer be modified.
::end:: */
{
  rep_DECLARE1(str, rep_STRINGP);

  rep_STRING(str)->car |= rep_STRING_IMMUTABLE;

  return rep_undefined_value;
}

DEFUN("string->immutable-string", Fstring_to_immutable_string,
      Sstring_to_immutable_string, (repv str), rep_Subr1) /*
::doc:rep.data#string->immutable-string::
string->immutable-string STRING

Returns STRING if it's already marked as immutable, else returns a new
copy of STRING that is marked immutable.
::end:: */
{
  rep_DECLARE1(str, rep_STRINGP);

  if (!rep_STRING_WRITABLE_P(str)) {
    return str;
  }

  repv copy = rep_string_copy_n(rep_STR(str), rep_STRING_LEN(str));

  if (copy) {
    rep_STRING(copy)->car |= rep_STRING_IMMUTABLE;
  }

  return copy;
}

static repv
substring(repv str, repv start, repv end, bool character_indexes)
{
  rep_DECLARE1(str, rep_STRINGP);
  rep_DECLARE2(start, rep_NON_NEG_INT_P);
  rep_DECLARE3_OPT(end, rep_NON_NEG_INT_P);

  intptr_t i0 = rep_INT(start);
  intptr_t i1 = rep_INTP(end) ? rep_INT(end) : -1;

  if (i1 >= 0 && i1 < i0) {
    return rep_signal_arg_error(end, 3);
  }

  const uint8_t *ptr = (const uint8_t *)rep_string_ptr(str);
  intptr_t bytes = STRING_LEN(rep_STRING(str)->car);

  if (character_indexes && !ascii_string_p(rep_STRING(str))) {
    /* If not ASCII, translate code point indexes to byte indexes. */

    const uint8_t *p0 = utf8_skip_characters(ptr, bytes, i0);
    if (p0) {
      if (i1 >= 0) {
	const uint8_t *p1 =
	  utf8_skip_characters(p0, bytes - (p0 - ptr), i1 - i0);
	i1 = p1 ? p1 - ptr : INTPTR_MAX;
      }
      i0 = p0 - ptr;
    } else {
      i0 = i1 = INTPTR_MAX;
    }
  }

  if (i0 > bytes) {
    return rep_signal_arg_error(start, 2);
  }

  intptr_t len = i1 >= 0 ? i1 - i0 : bytes - i0;
  if (len < 0) {
    return rep_signal_arg_error(end, 3);
  } else if (len == 0) {
    return rep_null_string();
  } else {
    return rep_string_copy_n(rep_STR(str) + i0, len);
  }
}

DEFUN("byte-substring", Fbyte_substring, Sbyte_substring,
      (repv str, repv start, repv end), rep_Subr3) /*
::doc:rep.data#byte-substring::
byte-substring STRING START [END]

Returns the portion of STRING starting at byte index START and ending
before byte index END (or the end of the string is END is not given).
All indices start at zero.
::end:: */
{
  return substring(str, start, end, false);
}

DEFUN("substring", Fsubstring, Ssubstring,
      (repv str, repv start, repv end), rep_Subr3) /*
::doc:rep.data#substring::
substring STRING START [END]

Returns the portion of STRING starting at character index START and
ending before character index END (or the end of the string is END is
not given). All indices start at zero.
::end:: */
{
  return substring(str, start, end, true);
}

DEFUN("concat", Fconcat, Sconcat, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.data#concat::
concat ARGS...

Concatenates all ARGS... into a single string, each argument can be a string,
a character or a list or vector of characters.
::end:: */
{
  /* Pass 1. calculate the length of the new string. */

  size_t length = 0;
  for (intptr_t i = 0; i < argc; i++) {
    repv arg = argv[i];
    if (rep_CHARP(arg)) {
      length += utf32_to_utf8_size_1(rep_CHAR_VALUE(arg));
    } else if (rep_STRINGP(arg)) {
      length += rep_STRING_LEN(arg);
    } else if (rep_LISTP(arg)) {
      repv lst = arg;
      while (rep_CONSP(lst)) {
	repv c = rep_CAR(lst);
	if (rep_CHARP(c)) {
	  length += utf32_to_utf8_size_1((rep_CHAR_VALUE(c)));
	} else {
	  return rep_signal_arg_error(arg, i + 1);
	}
	lst = rep_CDR(lst);
      }
    } else if (rep_VECTORP(arg)) {
      for (intptr_t i = 0; i < rep_VECTOR_LEN(arg); i++) {
	repv c = rep_VECTI(arg, i);
	if (rep_CHARP(c)) {
	  length += utf32_to_utf8_size_1((rep_CHAR_VALUE(c)));
	} else {
	  return rep_signal_arg_error(arg, i + 1);
	}
      }
    } else {
      return rep_signal_arg_error(arg, i + 1);
    }
  }

  if (length == 0) {
    return rep_null_string();
  }

  /* Pass 2: allocate and copy in the data */

  repv string = rep_allocate_string(length + 1);
  if (!string) {
    return 0;
  }

  uint8_t *ptr = (uint8_t *)rep_MUTABLE_STR(string);

  for (intptr_t i = 0; i < argc; i++) {
    repv arg = argv[i];
    if (rep_CHARP(arg)) {
      ptr += utf32_to_utf8_1(ptr, rep_CHAR_VALUE(arg));
    } else if (rep_STRINGP(arg)) {
      memcpy(ptr, rep_STR(arg), rep_STRING_LEN(arg));
      ptr += rep_STRING_LEN(arg);
    } else if (rep_CONSP(arg)) {
      repv lst = arg;
      while (rep_CONSP(lst)) {
	repv c = rep_CAR(lst);
	if (rep_CHARP(c)) {
	  ptr += utf32_to_utf8_1(ptr, rep_CHAR_VALUE(c));
	}
	lst = rep_CDR(lst);
      }
    } else if (rep_VECTORP(arg)) {
      for (intptr_t i = 0; i < rep_VECTOR_LEN(arg); i++) {
	repv c = rep_VECTI(arg, i);
	if (rep_CHARP(c)) {
	  ptr += utf32_to_utf8_1(ptr, rep_CHAR_VALUE(c));
	}
      }
    }
  }

  size_t size = (char *)ptr - rep_STR(string);
  if (rep_STRING_LEN(string) != size) {
    rep_string_set_len(string, size);
  }

  *ptr++ = '\0';

  return string;
}

DEFUN("string-prefix?", Fstring_head_eq, Sstring_head_eq,
      (repv str1, repv str2), rep_Subr2) /*
::doc:rep.data#string-prefix?::
string-prefix? STRING PREFIX-STRING

Returns t if PREFIX-STRING matches the beginning of STRING, i.e.,

  (string-prefix? "foobar" "foo") => t
  (string-prefix? "foo" "foobar") => nil

::end:: */
{
  rep_DECLARE1(str1, rep_STRINGP);
  rep_DECLARE2(str2, rep_STRINGP);

  const char *s1 = rep_STR(str1);
  const char *s2 = rep_STR(str2);

  while (*s1 && *s2) {
    if (*s1++ != *s2++) {
      return rep_nil;
    }
  }

  return (*s1 || (*s1 == *s2)) ? Qt : rep_nil;
}

static repv
string_compare(repv str1, repv str2)
{
  rep_DECLARE1(str1, rep_STRINGP);
  rep_DECLARE2(str2, rep_STRINGP);

  const char *s1 = rep_STR(str1);
  const char *s2 = rep_STR(str2);

  while (*s1 && *s2) {
    int c1 = *s1++;
    int c2 = *s2++;
    if (c1 != c2) {
      return rep_MAKE_INT(c1 - c2);
    }
  }

  if (*s1) {
    return rep_MAKE_INT(1);
  } else if (*s2) {
    return rep_MAKE_INT(-1);
  } else {
    return rep_MAKE_INT(0);
  }
}

static repv
string_compare_ci(repv str1, repv str2)
{
  rep_DECLARE1(str1, rep_STRINGP);
  rep_DECLARE2(str2, rep_STRINGP);

  const char *s1 = rep_STR(str1);
  const char *s2 = rep_STR(str2);

  while (*s1 && *s2) {
    int c1 = rep_toupper(*s1++);
    int c2 = rep_toupper(*s2++);
    if (c1 != c2) {
      return rep_MAKE_INT(c1 - c2);
    }
  }

  if (*s1) {
    return rep_MAKE_INT(1);
  } else if (*s2) {
    return rep_MAKE_INT(-1);
  } else {
    return rep_MAKE_INT(0);
  }
}

DEFUN("string=?", Fstring_equal, Sstring_equal,
      (repv str1, repv str2), rep_Subr2) /*
::doc:rep.data#string=?::
string=? STRING1 STRING2

Returns t if STRING1 and STRING2 are the same, ignoring case.
::end:: */
{
  repv ret = string_compare(str1, str2);
  if (ret) {
    ret = rep_INT(ret) == 0 ? Qt : rep_nil;
  }
  return ret;
}

DEFUN("string<?", Fstring_less, Sstring_less,
      (repv str1, repv str2), rep_Subr2) /*
::doc:rep.data#string<?::
string<? STRING1 STRING2

Returns t if STRING1 is less than STRING2, ignoring case.
::end:: */
{
  repv ret = string_compare(str1, str2);
  if (ret) {
    ret = rep_INT(ret) < 0 ? Qt : rep_nil;
  }
  return ret;
}

DEFUN("string<=?", Fstring_less_equal, Sstring_less_equal,
      (repv str1, repv str2), rep_Subr2) /*
::doc:rep.data#string<=?::
string<=? STRING1 STRING2

Returns t if STRING1 is less than or equal to STRING2, ignoring case.
::end:: */
{
  repv ret = string_compare(str1, str2);
  if (ret) {
    ret = rep_INT(ret) <= 0 ? Qt : rep_nil;
  }
  return ret;
}

DEFUN("string>?", Fstring_greater, Sstring_greater,
      (repv str1, repv str2), rep_Subr2) /*
::doc:rep.data#string>?::
string>? STRING1 STRING2

Returns t if STRING1 is greater than STRING2, ignoring case.
::end:: */
{
  repv ret = string_compare(str1, str2);
  if (ret) {
    ret = rep_INT(ret) > 0 ? Qt : rep_nil;
  }
  return ret;
}

DEFUN("string>=?", Fstring_greater_equal, Sstring_greater_equal,
      (repv str1, repv str2), rep_Subr2) /*
::doc:rep.data#string>=?::
string>=? STRING1 STRING2

Returns t if STRING1 is greater than or equal to STRING2, ignoring
case.
::end:: */
{
  repv ret = string_compare(str1, str2);
  if (ret) {
    ret = rep_INT(ret) >= 0 ? Qt : rep_nil;
  }
  return ret;
}

DEFUN("string-ci=?", Fstring_equal_ci, Sstring_equal_ci,
      (repv str1, repv str2), rep_Subr2) /*
::doc:rep.data#string-ci=?::
string-ci=? STRING1 STRING2

Returns t if STRING1 and STRING2 are the same, ignoring case.
::end:: */
{
  repv ret = string_compare_ci(str1, str2);
  if (ret) {
    ret = rep_INT(ret) == 0 ? Qt : rep_nil;
  }
  return ret;
}

DEFUN("string-ci<?", Fstring_less_ci, Sstring_less_ci,
      (repv str1, repv str2), rep_Subr2) /*
::doc:rep.data#string-ci<?::
string-ci<? STRING1 STRING2

Returns t if STRING1 is less than STRING2, ignoring case.
::end:: */
{
  repv ret = string_compare_ci(str1, str2);
  if (ret) {
    ret = rep_INT(ret) < 0 ? Qt : rep_nil;
  }
  return ret;
}

DEFUN("string-ci<=?", Fstring_less_equal_ci, Sstring_less_equal_ci,
      (repv str1, repv str2), rep_Subr2) /*
::doc:rep.data#string-ci<=?::
string-ci<=? STRING1 STRING2

Returns t if STRING1 is less than or equal to STRING2, ignoring case.
::end:: */
{
  repv ret = string_compare_ci(str1, str2);
  if (ret) {
    ret = rep_INT(ret) <= 0 ? Qt : rep_nil;
  }
  return ret;
}

DEFUN("string-ci>?", Fstring_greater_ci, Sstring_greater_ci,
      (repv str1, repv str2), rep_Subr2) /*
::doc:rep.data#string-ci>?::
string-ci>? STRING1 STRING2

Returns t if STRING1 is greater than STRING2, ignoring case.
::end:: */
{
  repv ret = string_compare_ci(str1, str2);
  if (ret) {
    ret = rep_INT(ret) > 0 ? Qt : rep_nil;
  }
  return ret;
}

DEFUN("string-ci>=?", Fstring_greater_equal_ci, Sstring_greater_equal_ci,
      (repv str1, repv str2), rep_Subr2) /*
::doc:rep.data#string-ci>=?::
string-ci>=? STRING1 STRING2

Returns t if STRING1 is greater than or equal to STRING2, ignoring
case.
::end:: */
{
  repv ret = string_compare_ci(str1, str2);
  if (ret) {
    ret = rep_INT(ret) >= 0 ? Qt : rep_nil;
  }
  return ret;
}

DEFUN("translate-byte-string!", Ftranslate_byte_string,
      Stranslate_byte_string, (repv string, repv table), rep_Subr2) /*
::doc:rep.data#translate-byte-string!:
translate-byte-string! STRING TRANSLATION-TABLE

Applies the TRANSLATION-TABLE destructively to each byte in STRING.
TRANSLATION-TABLE is also a byte-string, each byte represents the
translation for that position in the string. If the string is less than
256 bytes long any undefined input bytes will remain unchanged.
::end:: */
{
  rep_DECLARE1(string, rep_STRINGP);
  rep_DECLARE2(table, rep_STRINGP);

  if (!rep_STRING_WRITABLE_P(string)) {
    return(rep_signal_arg_error(string, 1));
  }

  const char *tab = rep_STR(table);
  size_t tablen = rep_STRING_LEN(table);

  char *str = rep_MUTABLE_STR(string);
  size_t slen = rep_STRING_LEN(string);

  for (size_t i = 0; i < slen; i++) {
    unsigned int c = str[i];
    if (c < tablen) {
      str[i] = tab[c];
    }
  }

  rep_invalidate_string(string);
  return rep_undefined_value;
}

DEFUN("translate-string!", Ftranslate_string,
      Stranslate_string, (repv string, repv table), rep_Subr2) /*
::doc:rep.data#translate-string!:
translate-string! STRING TRANSLATION-TABLE

Applies the TRANSLATION-TABLE destructively to each character in
STRING. TRANSLATION-TABLE is also string, each character represents the
translation for that position in the string.  Any undefined input
characters will remain unchanged.
::end:: */
{
  rep_DECLARE1(string, rep_STRINGP);
  rep_DECLARE2(table, rep_STRINGP);

  if (!rep_STRING_WRITABLE_P(string)) {
    return(rep_signal_arg_error(string, 1));
  }

  const rep_string_utf32 *src = ensure_utf32(rep_STRING(table), false);

  if (src) {
    /* Table is not ASCII, so always need to assume output contains
       non-ASCII. */

    rep_string_utf32 *dst = ensure_utf32(rep_STRING(string), true);

    for (size_t i = 0; i < dst->len; i++) {
      uint32_t c = dst->data[i];
      if (c < src->len && src->data[c] != c) {
	dst->data[i] = src->data[c];
	dst->dirty = true;
      }
    }
  } else {
    /* Table is wholly ASCII. If source is also ASCII, we know that
       output can only be ASCII. */

    const uint8_t *src_data = (const uint8_t *)rep_STR(table);
    size_t src_len = STRING_LEN(rep_STRING(table)->car);

    rep_string_utf32 *dst = ensure_utf32(rep_STRING(string), false);

    if (dst) {
      for (size_t i = 0; i < dst->len; i++) {
	uint32_t c = dst->data[i];
	if (c < src_len && src_data[c] != c) {
	  dst->data[i] = src_data[c];
	  dst->dirty = true;
	}
      }
    } else {
      uint8_t *dst_data = (uint8_t *)rep_MUTABLE_STR(string);
      size_t dst_len = STRING_LEN(rep_STRING(string)->car);

      for (size_t i = 0; i < dst_len; i++) {
	uint32_t c = dst_data[i];
	if (c < src_len && src_data[c] != c) {
	  dst_data[i] = src_data[c];
	}
      }

      rep_string_set_ascii(string);
    }
  }

  rep_invalidate_string(string);
  return rep_undefined_value;
}

DEFUN("complete-string", Fcomplete_string, Scomplete_string,
      (repv existing, repv arg_list, repv fold), rep_Subr3) /*
::doc:rep.data#complete-string::
complete-string TEMPLATE LIST [FOLD-CASE]

Return a string whose beginning matches the string TEMPLATE, and is unique
in the set of all strings in LIST which also match TEMPLATE. If FOLD-CASE
is t, all matching ignores character case.
::end:: */
{
  rep_DECLARE1(existing, rep_STRINGP);
  rep_DECLARE2(arg_list, rep_LISTP);

  const char *orig = rep_STR(existing);
  size_t origlen = rep_STRING_LEN(existing);

  const char *match = NULL;
  size_t matchlen = 0;

  while (rep_CONSP(arg_list)) {
    repv arg = rep_CAR(arg_list);
    arg_list = rep_CDR(arg_list);

    if (!rep_STRINGP(arg)) {
      continue;
    }

    const char *tmp = rep_STR(arg);
    if ((fold == rep_nil
	 ? strncmp(orig, tmp, origlen)
	 : strncasecmp(orig, tmp, origlen)) == 0)
    {
      if (match) {
	const char *tmp2 = match + origlen;
	tmp += origlen;
	while (*tmp2 && *tmp) {
	  if(fold == rep_nil
	     ? (*tmp2 != *tmp)
	     : (rep_tolower(*tmp2) != rep_tolower(*tmp)))
	  {
	    break;
	  }
	  tmp2++;
	  tmp++;
	}
	if((tmp2 - match) < matchlen) {
	  matchlen = tmp2 - match;
	}
      } else {
	match = tmp;
	matchlen = strlen(tmp);
      }
    }
  }

  if (match) {
    return rep_string_copy_n(match, matchlen);
  } else {
    return rep_nil;
  }
}

void
rep_strings_init(void)
{
  repv tem = rep_push_structure("rep.data");
  rep_ADD_SUBR(Smake_string);
  rep_ADD_SUBR(Sstringp);
  rep_ADD_SUBR(Sbyte_string_length);
  rep_ADD_SUBR(Sbyte_string_ref);
  rep_ADD_SUBR(Sbyte_string_set);
  rep_ADD_SUBR(Sstring_length);
  rep_ADD_SUBR(Sstring_ref);
  rep_ADD_SUBR(Sstring_set);
  rep_ADD_SUBR(Smake_string_immutable);
  rep_ADD_SUBR(Sstring_to_immutable_string);
  rep_ADD_SUBR(Sbyte_substring);
  rep_ADD_SUBR(Ssubstring);
  rep_ADD_SUBR(Sconcat);
  rep_ADD_SUBR(Sstring_head_eq);
  rep_ADD_SUBR(Sstring_equal);
  rep_ADD_SUBR(Sstring_less);
  rep_ADD_SUBR(Sstring_less_equal);
  rep_ADD_SUBR(Sstring_greater);
  rep_ADD_SUBR(Sstring_greater_equal);
  rep_ADD_SUBR(Sstring_equal_ci);
  rep_ADD_SUBR(Sstring_less_ci);
  rep_ADD_SUBR(Sstring_less_equal_ci);
  rep_ADD_SUBR(Sstring_greater_ci);
  rep_ADD_SUBR(Sstring_greater_equal_ci);
  rep_ADD_SUBR(Stranslate_byte_string);
  rep_ADD_SUBR(Stranslate_string);
  rep_ADD_SUBR(Scomplete_string);
  rep_pop_structure(tem);

  rep_INTERN(newlines);
  rep_INTERN(control);
}

void
rep_strings_kill(void)
{
  string_block *s = string_block_list;

  string_block_list = NULL;
  rep_allocated_strings = rep_used_strings = 0;
  rep_allocated_string_bytes = 0;

  while (s) {
    int i;
    string_block *next = s->next;
    for (i = 0; i < STRINGS_PER_BLOCK; i++) {
      if (!rep_CELL_CONS_P(rep_VAL(s->data + i))) {
	rep_free(s->data[i].utf8_data);
	free_utf32(&s->data[i]);
      }
    }
    rep_free(s);
    s = next;
  }
}
