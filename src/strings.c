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

#include <string.h>
#include <ctype.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

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
  if (len > rep_MAX_STRING) {
    return Fsignal(Qerror, rep_LIST_1(rep_VAL(&string_overflow)));
  }

  rep_string *str = string_freelist;

  if (str) {
    string_freelist = rep_STRING(str->car);
  } else {
    str = refill_free_list();
  }

  str->car = rep_MAKE_STRING_CAR(len);
  str->data = ptr;

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
    memcpy(rep_STR(dst), src, slen);
    rep_STR(dst)[slen] = 0;
  }

  return rep_VAL(dst);
}

repv
rep_string_copy(const char *src)
{
  return rep_string_copy_n(src, strlen(src));
}

repv
rep_string_concat2(char *s1, char *s2)
{
  int len = strlen(s1) + strlen(s2);
  repv res = rep_allocate_string(len + 1);
  stpcpy(stpcpy(rep_STR(res), s1), s2);
  return(res);
}

repv
rep_string_concat3(char *s1, char *s2, char *s3)
{
  int len = strlen(s1) + strlen(s2) + strlen(s3);
  repv res = rep_allocate_string(len + 1);
  stpcpy(stpcpy(stpcpy(rep_STR(res), s1), s2), s3);
  return(res);
}

repv
rep_string_concat4(char *s1, char *s2, char *s3, char *s4)
{
  int len = strlen(s1) + strlen(s2) + strlen(s3) + strlen(s4);
  repv res = rep_allocate_string(len + 1);
  stpcpy(stpcpy(stpcpy(stpcpy(rep_STR(res), s1), s2), s3), s4);
  return(res);
}

int
rep_string_cmp(repv v1, repv v2)
{
  if (!rep_STRINGP(v1) || !rep_STRINGP(v2)) {
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
  char *s = rep_STR(obj);

  bool escape_all = false, escape_newlines = false;
  repv tem = Fsymbol_value(Qprint_escape, Qt);
  if (tem == Qnewlines) {
    escape_all = false, escape_newlines = true;
  } else if (tem == Qt) {
    escape_all = true, escape_newlines = true;
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

    unsigned int c = *s++;

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
      if (escape_all && (c < 32 || c > 126)) {
	buf[i++] = '\\';
	buf[i++] = '0' + ((c >> 6) & 7);
	buf[i++] = '0' + ((c >> 3) & 7);
	buf[i++] = '0' + ((c >> 0) & 7);
      } else {
	buf[i++] = c;
      }
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
	  rep_free(rep_STRING(str)->data);
	}
	rep_STRING(str)->car = rep_VAL(free_list);
	free_list = rep_STRING(str);
      } else {
	rep_GC_CLR_CELL(str);
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

  rep_STRING(str)->car = rep_MAKE_STRING_CAR(len);
  return true;
}

DEFUN("make-string", Fmake_string, Smake_string, (repv len, repv init), rep_Subr2) /*
::doc:rep.data#make-string::
make-string LENGTH [INITIAL-VALUE]

Returns a new string of length LENGTH, each character is initialised to
INITIAL-VALUE, or to space if INITIAL-VALUE is not given.
::end:: */
{
  rep_DECLARE1(len, rep_NON_NEG_INT_P);

  intptr_t l = rep_INT(len);
  repv s = rep_allocate_string(l + 1);
  if (s) {
    memset(rep_STR(s), rep_INTP(init) ? (char)rep_INT(init) : ' ', l);
    rep_STR(s)[l] = 0;
  }
  return s;
}

DEFUN("stringp", Fstringp, Sstringp, (repv arg), rep_Subr1) /*
::doc:rep.data#stringp::
stringp ARG

Returns t is ARG is a string.
::end:: */
{
  return rep_STRINGP(arg) ? Qt : rep_nil;
}

DEFUN("substring", Fsubstring, Ssubstring, (repv string, repv start, repv end), rep_Subr3) /*
::doc:rep.data#substring::
substring STRING START [END]

Returns the portion of STRING starting at character number START and ending
at the character before END (or the end of the string is END is not given).
All indices start at zero.
::end:: */
{
  rep_DECLARE1(string, rep_STRINGP);
  rep_DECLARE2(start, rep_NON_NEG_INT_P);
  rep_DECLARE3_OPT(end, rep_NON_NEG_INT_P);

  intptr_t slen = rep_STRING_LEN(string);
  if (rep_INT(start) > slen) {
    return rep_signal_arg_error(start, 2);
  }

  if (rep_INTP(end)) {
    if ((rep_INT(end) > slen) || (rep_INT(end) < rep_INT(start))) {
      return rep_signal_arg_error(end, 3);
    }
    return rep_string_copy_n(rep_STR(string) + rep_INT(start),
			   rep_INT(end) - rep_INT(start));
  } else {
    return rep_string_copy_n(rep_STR(string) + rep_INT(start),
			   slen - rep_INT(start));
  }
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
    repv elt = argv[i];
    if (rep_INTP(elt)) {
      length++;
    } else if (rep_CONSP(elt)) {
      length += rep_list_length(elt);
    } else {
      switch (rep_CELL8_TYPE(elt)) {
      case rep_String:
	length += rep_STRING_LEN(elt);
	break;
      case rep_Vector:
	length += rep_VECT_LEN(elt);
	break;
      }
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

  char *ptr = rep_STR(string);

  for (intptr_t i = 0; i < argc; i++) {
    repv elt = argv[i];
    if (rep_INTP(elt)) {
      *ptr++ = rep_INT(elt);
    } else if (rep_CONSP(elt)) {
      repv tem = elt, c;
      while (rep_CONSP(tem)) {
	c = rep_CAR(tem);
	if (rep_INTP(c)) {
	  *ptr++ = rep_INT(c);
	}
	tem = rep_CDR(tem);
      }
    } else {
      switch (rep_CELL8_TYPE(elt)) {
      case rep_String:
	memcpy(ptr, rep_STR(elt), rep_STRING_LEN(elt));
	ptr += rep_STRING_LEN(elt);
	break;
      case rep_Vector:
	for (intptr_t i = 0; i < rep_VECT_LEN(elt); i++) {
	  repv c = rep_VECTI(elt, i);
	  if (rep_INTP(c)) {
	    *ptr++ = rep_INT(c);
	  }
	}
	break;
      }
    }
  }

  if (rep_STRING_LEN(string) != ptr - rep_STR(string)) {
    rep_string_set_len(string, ptr - rep_STR(string));
  }

  *ptr++ = '\0';

  return string;
}

DEFUN("string-head-eq", Fstring_head_eq, Sstring_head_eq,
      (repv str1, repv str2), rep_Subr2) /*
::doc:rep.data#string-head-eq::
string-head-eq STRING1 STRING2

Returns t if STRING2 matches the beginning of STRING1, i.e.,

  (string-head-eq "foobar" "foo") => t
  (string-head-eq "foo" "foobar") => nil

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

DEFUN("string-equal", Fstring_equal, Sstring_equal,
      (repv str1, repv str2), rep_Subr2) /*
::doc:rep.data#string-equal::
string-equal STRING1 STRING2

Returns t if STRING1 and STRING2 are the same, ignoring case.
::end:: */
{
  rep_DECLARE1(str1, rep_STRINGP);
  rep_DECLARE2(str2, rep_STRINGP);

  const char *s1 = rep_STR(str1);
  const char *s2 = rep_STR(str2);

  while (*s1 && *s2) {
    if (rep_toupper(*s1) != rep_toupper(*s2)) {
      return rep_nil;
    }
    s1++; s2++;
  }

  return (*s1 || *s2) ? rep_nil : Qt;
}

DEFUN("string-lessp", Fstring_lessp, Sstring_lessp,
      (repv str1, repv str2), rep_Subr2) /*
::doc:rep.data#string-lessp::
string-lessp STRING1 STRING2

Returns t if STRING1 is `less' than STRING2, ignoring case.
::end:: */
{
  rep_DECLARE1(str1, rep_STRINGP);
  rep_DECLARE2(str2, rep_STRINGP);

  const char *s1 = rep_STR(str1);
  const char *s2 = rep_STR(str2);

  while (*s1 && *s2) {
    if (rep_toupper(*s1) != rep_toupper(*s2)) {
      return (rep_toupper(*s1) < rep_toupper(*s2)) ? Qt : rep_nil;
    }
    s1++; s2++;
  }

  return *s2 ? Qt : rep_nil;
}

DEFUN("translate-string", Ftranslate_string,
      Stranslate_string, (repv string, repv table), rep_Subr2) /*
::doc:rep.data#translate-string:
translate-string STRING TRANSLATION-TABLE

Applies the TRANSLATION-TABLE to each character in the string STRING.
TRANSLATION-TABLE is a string, each character represents the translation
for an ascii character of that characters position in the string. If the
string is less than 256 chars long any undefined characters will remain
unchanged.
Note that the STRING really is modified, no copy is made!
::end:: */
{
  rep_DECLARE1(string, rep_STRINGP);
  rep_DECLARE2(table, rep_STRINGP);

  if (!rep_STRING_WRITABLE_P(string)) {
    return(rep_signal_arg_error(string, 1));
  }

  char *tab = rep_STR(table);
  size_t tablen = rep_STRING_LEN(table);

  char *str = rep_STR(string);
  size_t slen = rep_STRING_LEN(string);

  for (size_t i = 0; i < slen; i++) {
    unsigned int c = str[i];
    if (c < tablen) {
      str[i] = tab[c];
    }
  }

  rep_string_modified(string);
  return string;
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

  char *orig = rep_STR(existing);
  size_t origlen = rep_STRING_LEN(existing);

  char *match = NULL;
  size_t matchlen = 0;

  while (rep_CONSP(arg_list)) {
    repv arg = rep_CAR(arg_list);
    arg_list = rep_CDR(arg_list);

    if (!rep_STRINGP(arg)) {
      continue;
    }

    char *tmp = rep_STR(arg);
    if ((fold == rep_nil
	 ? strncmp(orig, tmp, origlen)
	 : strncasecmp(orig, tmp, origlen)) == 0)
    {
      if (match) {
	char *tmp2 = match + origlen;
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
  rep_ADD_SUBR(Ssubstring);
  rep_ADD_SUBR(Sconcat);
  rep_ADD_SUBR(Sstring_head_eq);
  rep_ADD_SUBR(Sstring_equal);
  rep_ADD_SUBR(Sstring_lessp);
  rep_ADD_SUBR(Stranslate_string);
  rep_ADD_SUBR(Scomplete_string);
  rep_pop_structure(tem);
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
	rep_free(s->data[i].data);
      }
    }
    rep_free(s);
    s = next;
  }
}
