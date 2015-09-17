/* read.c -- reader

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
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <assert.h>
#include <inttypes.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

DEFSYM(backquote, "backquote");
DEFSYM(backquote_unquote, "backquote-unquote");
DEFSYM(backquote_splice, "backquote-splice");
DEFSYM(quote, "quote");
DEFSYM(function, "function");
DEFSYM(structure_ref, "structure-ref");

/* The `c' variable which keeps coming up is the lookahead character,
   since each reader function normally has to look at the next character
   to see if it's what it wants. If not, the lookahead is given to
   someone else or unread before exiting... */

static repv readl(repv, register int *, repv);

static repv
signal_reader_error(repv type, repv stream, char *message)
{
  repv data = rep_nil;
  if (message != 0) {
    data = Fcons(rep_string_copy(message), data);
  }

  if (rep_FILEP(stream)) {
    if ((rep_FILE(stream)->car & rep_LFF_BOGUS_LINE_NUMBER) == 0) {
      data = Fcons(rep_MAKE_INT(rep_FILE(stream)->line_number), data);
    }
    data = Fcons(rep_FILE(stream)->name, data);
  } else {
    data = Fcons(stream, data);
  }

  return Fsignal(type, data);
}

static int
hexdigit(int c)
{
  if (c >= '0' && c <= '9') {
    return c - '0';
  } else if (c >= 'a' && c <= 'f') {
    return 10 + (c - 'a');
  } else if (c >= 'A' && c <= 'F') {
    return 10 + (c - 'a');
  } else {
    return -1;
  }
}

static void
read_comment(repv stream, int *c_p)
{
  int terminator = *c_p;
  int depth = 1;
  int c;

  while ((c = rep_stream_getc(stream)) != EOF) {
  again:
    if (c == terminator) {
      c = rep_stream_getc(stream);
      if (c == EOF || (c == '#' && --depth == 0)) {
	break;
      } else {
	goto again;
      }
    } else if (c == '#') {
      c = rep_stream_getc(stream);
      if (c == EOF) {
	break;
      } else if (c == terminator) {
	depth++;
      } else {
	goto again;
      }
    }
  }

  if (c != EOF) {
    c = rep_stream_getc(stream);
  } else {
    signal_reader_error(Qpremature_end_of_stream,
			stream, "while reading a comment");
  }

  *c_p = c;
}

static repv
read_list(repv stream, register int *c_p)
{
  repv result = rep_nil;
  repv last = 0;

  rep_GC_root gc_result;
  rep_PUSHGC(gc_result, result);

  int start_line = -1;
  if (rep_FILEP(stream)) {
    start_line = rep_FILE(stream)->line_number;
  }

  *c_p = rep_stream_getc(stream);

  while (result != 0) {
    switch (*c_p) {
    case EOF:
      result = signal_reader_error(Qpremature_end_of_stream,
				   stream, "while reading a list");
      break;

    case ' ':
    case '\t':
    case '\n':
    case '\r':
    case '\f':
      *c_p = rep_stream_getc(stream);
      continue;

    case ';': {
      int c;
      while ((c = rep_stream_getc(stream)) != EOF
	     && c != '\n' && c != '\f' && c != '\r')
      {
      }
      *c_p = rep_stream_getc(stream);
      continue; }

    case ')':
    case ']':
      *c_p = rep_stream_getc(stream);
      goto end;

    case '.':
      *c_p = rep_stream_getc(stream);
      switch (*c_p) {
      case EOF:
	result = signal_reader_error(Qpremature_end_of_stream,
				     stream, "after `.' in list");
	goto end;

      case ' ': case '\t': case '\n': case '\f': case '\r':
	if (last) {
	  repv this = readl(stream, c_p, Qpremature_end_of_stream);
	  if (this != 0) {
	    rep_CDR(last) = this;
	  } else {
	    result = 0;
	    goto end;
	  }
	} else {
	  result = signal_reader_error(Qinvalid_read_syntax,
            stream, "nothing to dot second element of cons to");
	  goto end;
	}
	continue;

      default:
	rep_stream_ungetc(stream, *c_p);
	*c_p = '.';
      }
      goto do_default;

    case '#': {
      int c = rep_stream_getc(stream);
      if (c == EOF) {
	goto end;
      } else if (c == '|') {
	*c_p = c;
	read_comment(stream, c_p);
	if (rep_INTERRUPTP) {
	  return 0;
	}
	continue;
      }
      rep_stream_ungetc(stream, c);
      goto do_default; }

    default:
    do_default: {
      repv this = Fcons(rep_nil, rep_nil);
      if (last) {
	rep_CDR(last) = this;
      } else {
	result = this;
      }
      rep_CAR(this) = readl(stream, c_p, Qpremature_end_of_stream);
      if (!rep_CAR(this)) {
	result = 0;
      }
      last = this;
      break; }
    }
  }
end:
  rep_POPGC;

  if (result) {
    rep_record_origin(result, stream, start_line);
  }

  /* FIXME: need a way to make pairs immutable. */

  return result;
}

/* Could be a symbol or a number */

static repv
read_symbol(repv stream, int *c_p, repv obarray)
{
  /* For parsing numbers, while radix != zero, it might still be an
     integer that we're reading. */

  int radix = -1, sign = 1, nfirst = 0;
  bool exact = true, rational = false;
  bool exponent = false, had_sign = false;
  bool expecting_prefix = false;
  int force_exactness = 0;

  /* FIXME: using a static string is grotesque, but it speeds up
     the call to Ffind_symbol() below, as no allocations are required
     for the common case of parsing an existing symbol. */

  static repv buffer = 0;
  static size_t buflen = 240;

  if (!buffer) {
    buffer = rep_allocate_string(buflen + 2);
    rep_mark_static(&buffer);
  }

  char *buf = rep_MUTABLE_STR(buffer);
  int buf_i = 0;

  int c = *c_p;

  while (c != EOF) {
    if (buf_i == buflen) {
      repv new;
      buflen = buflen * 2;
      new = rep_allocate_string(buflen + 2);
      memcpy(rep_MUTABLE_STR(new), buf, buflen / 2);
      buf = rep_MUTABLE_STR(new);
    }

    switch (c) {
    case ' ':  case '\t': case '\n': case '\f': case '\r':
    case '(':  case ')':  case '[':  case ']':
    case '\'': case '"':  case ';':  case ',':
    case '`':
      goto done;

    case '#':
      if (radix == 0) {
	goto done;
      } else {
	goto number;
      }

    case '\\':
      radix = 0;
      c = rep_stream_getc(stream);
      if (c == EOF) {
	return signal_reader_error(Qpremature_end_of_stream,
				   stream, "after `\\' in identifer");
      }
      buf[buf_i++] = c;
      break;

    case '|':
      radix = 0;
      c = rep_stream_getc(stream);
      while ((c != EOF) && (c != '|') && (buf_i < buflen)) {
	buf[buf_i++] = c;
	c = rep_stream_getc(stream);
      }
      if (c == EOF) {
	return signal_reader_error(Qpremature_end_of_stream,
				   stream, "after `|' in identifier");
      }
      break;

    default:
      if (radix != 0) {
      number:
	if (expecting_prefix) {
	  switch (c) {
	  case 'b': case 'B':
	    radix = 2;
	    break;
	  case 'o': case 'O':
	    radix = 8;
	    break;
	  case 'd': case 'D':
	    radix = 10;
	    break;
	  case 'x': case 'X':
	    radix = 16;
	    break;
	  case 'e': case 'E':
	    force_exactness = +1;
	    break;
	  case 'i': case 'I':
	    force_exactness = -1;
	    break;
	  default:
	    radix = 0;
	  }
	  expecting_prefix = false;
	  nfirst = buf_i + 1;
	} else if (buf_i == nfirst && (c == '-' || c == '+' || c == '#')) {
	  /* It still may be a number that we're parsing */
	  if (c == '#') {
	    if (had_sign) {
	      radix = 0;	/* not a number? */
	    } else {
	      expecting_prefix = true;
	    }
	  } else {
	    /* A leading sign */
	    sign = (c == '-') ? -1 : 1;
	    had_sign = true;
	  }
	  nfirst = buf_i + 1;
	} else {
	  switch (radix) {
	  case -1:
	    /* Deduce the base next(or that we're not looking at a number) */
	    if (c == '.') {
	      radix = 10;
	      exact = false;
	    } else if (!(c >= '0' && c <= '9')) {
	      radix = 0;
	    } else {
	      radix = 10;
	    }
	    break;

	  default:
	    /* Now we're speculatively reading a number of base radix. */
	    switch (c) {
	    case '.':
	      if (exact && radix == 10 && !rational) {
		exact = false;
	      } else {
		radix = 0;
	      }
	      break;

	    case '/':
	      if (exact && !rational) {
		rational = true;
	      } else {
		radix = 0;
	      }
	      break;

	    case '-': case '+':
	      if (!exponent) {
		goto do_default;
	      }
	      break;

	    case 'e': case 'E':
	      if (radix == 10) {
		if (!rational && !exponent) {
		  exponent = true;
		  exact = false;
		} else {
		  radix = 0;
		}
		break;
	      }
	      /* fall through */

	    default:
	    do_default:
	      if (radix <= 10 && !(c >= '0' && c <= ('0' + radix - 1))) {
		radix = 0;
	      } else if (radix == 16 && !rep_isxdigit(c)) {
		radix = 0;
	      }
	    }
	  }
	}
      }
      buf[buf_i++] = c;
    }

    c = rep_stream_getc(stream);
  }

done:
  buf[buf_i] = 0;

  repv result = 0;

  if (buf_i == 0) {
    result = signal_reader_error(Qinvalid_read_syntax, stream,
				 "zero length identifier");
  } else if (radix > 0 && nfirst < buf_i) {
    /* It was a number of some sort */
    result = rep_parse_number(buf + nfirst, buf_i - nfirst, radix, sign,
			      !exact ? rep_NUMBER_FLOAT
			      : rational ? rep_NUMBER_RATIONAL : 0);
    if (result == 0) {
      goto intern;
    }
    if (force_exactness > 0) {
      result = Finexact_to_exact(result);
    } else if (force_exactness < 0) {
      result = Fexact_to_inexact(result);
    }
  } else {
  intern:
    rep_string_set_len(buffer, buf_i);
    result = Ffind_symbol(rep_VAL(buffer), obarray);
    if (result != 0 && result == rep_nil) {
      repv name = rep_string_copy_n(buf, buf_i);
      rep_STRING(name)->car |= rep_STRING_IMMUTABLE;
      result = Fmake_symbol(name);
      if (result != 0) {
	result = Fintern_symbol(result, obarray);
      }
    }
  }

  *c_p = c;
  return result;
}

static repv
read_vector(repv stream, int *c_p)
{
  repv list = read_list(stream, c_p);
  if (!list) {
    return 0;
  }

  size_t len = 0;
  for (repv ptr = list; rep_CONSP(ptr); ptr = rep_CDR(ptr)) {
    len++;
  }

  repv vec = rep_make_vector(len);
  if (!vec) {
    return 0;
  }

  size_t i = 0;
  while (rep_CONSP(list)) {
    rep_VECT(vec)->array[i++] = rep_CAR(list);
    repv next = rep_CDR(list);
    /* It's okay to put the cons cells back onto their freelist. No
       references to the cells can have escaped.  */
    rep_cons_free(list);
    list = next;
  }

  rep_VECT(vec)->car |= rep_VECTOR_IMMUTABLE;
  return vec;
}

static intptr_t
read_string_escape(repv stream, int *c_p, uint8_t *ptr)
{
  bool utf32 = false;
  uint32_t ret = 0;

  int c = *c_p;

  switch (c) {
  case EOF:
    return -1;

  case '\n':
    ret = rep_stream_getc(stream);
    break;

  case 'a':
    ret = 7;
    break;
  case 'b':
    ret = 8;
    break;
  case 't':
    ret = 9;
    break;
  case 'n':
    ret = 10;
    break;
  case 'v':
    ret = 11;
    break;
  case 'f':
    ret = 12;
    break;
  case 'r':
    ret = 13;
    break;
  case 'e':
    ret = 27;
    break;
  case '"':
  case '\'':
  case '\\':
    ret = c;
    break;

    /* \OOO -- three digit octal byte */

  case '0': case '1': case '2': case '3':
  case '4': case '5': case '6': case '7':
    ret = c - '0';
    c = rep_stream_getc(stream);
    if (c >= '0' && c <= '7') {
      ret = ret * 8 + (c - '0');
      c = rep_stream_getc(stream);
      if (c >= '0' && c <= '7') {
	ret = ret * 8 + (c - '0');
	break;
      } else {
	return -1;
      }
    } else {
      return -1;
    }
    break;

    /* \xXX..; -- hexadecimal UTF-32 character */

  case 'x':
    ret = 0;
    utf32 = true;
    for (size_t i = 0;; i++) {
      c = rep_stream_getc(stream);
      if (c == ';') {
	if (i == 0) {
	  return -1;
	}
	break;
      }
      int h = hexdigit(c);
      if (h < 0) {
	return -1;
      }
      ret = ret * 16 + h;
    }
    break;

  default:
    return -1;
  }

  *c_p = rep_stream_getc(stream);

  if (!utf32) {
    ptr[0] = ret;
    return 1;
  }

  return utf32_to_utf8_1(ptr, ret);
}

static repv
read_string(repv stream, int *c_p)
{
  uint8_t static_buffer[256];
  size_t buf_len = sizeof(static_buffer);
  uint8_t *buf = static_buffer;
  size_t buf_i = 0;
  bool all_ascii = true;

  int c = rep_stream_getc(stream);

  while ((c != EOF) && (c != '"')) {
    if (buf_i + 8 >= buf_len) {
      size_t new_len = buf_len * 2;
      uint8_t *newbuf = rep_alloc(new_len);
      if (newbuf) {
	memcpy(newbuf, buf, buf_len);
      }
      if (buf != static_buffer) {
	rep_free(buf);
      }
      if (!newbuf) {
	return 0;
      }
      buf = newbuf;
      buf_len = new_len;
    }

    if (c != '\\') {
      buf[buf_i++] = c;
      if (all_ascii && c > 127) {
	all_ascii = false;
      }
      c = rep_stream_getc(stream);
    } else {
      c = rep_stream_getc(stream);
      intptr_t size = read_string_escape(stream, &c, buf + buf_i);
      if (size <= 0) {
	return signal_reader_error(Qinvalid_read_syntax, stream,
				   "invalid string escape");
      }
      if (all_ascii && buf[buf_i] > 127) {
	all_ascii = false;
      }
      buf_i += size;
    }
  }

  repv ret;
  if (c == EOF) {
    ret = signal_reader_error(Qpremature_end_of_stream,
			      stream, "while reading a string");
  } else {
    *c_p = rep_stream_getc(stream);
    ret = rep_string_copy_n((char *)buf, buf_i);
    rep_STRING(ret)->car |= rep_STRING_IMMUTABLE;
    if (all_ascii) {
      rep_string_set_ascii(ret);
    }
  }

  if (buf != static_buffer) {
    rep_free(buf);
  }

  return ret;
}

static repv
skip_chars(repv stream, const char *str, repv ret, int *ptr)
{
  while (*str != 0) {
    int c = rep_stream_getc(stream);
    if (c != *str++) {
#ifdef HAVE_SNPRINTF
      char buf[256];
      snprintf(buf, sizeof(buf), "Expecting `%s'", str - 1);
      return signal_reader_error(Qinvalid_read_syntax, stream, buf);
#else
      return signal_reader_error(Qinvalid_read_syntax, stream,
				 "while reading a token");
#endif
    }
  }

  int c = rep_stream_getc(stream);

  switch (c) {
  case EOF:
  case ' ': case '\t': case '\n': case '\f': case '\r':
  case '(': case ')': case '[': case ']':
  case '\'': case '"': case ';': case ',':
  case '`':
    *ptr = c;
    return ret;

  default:
    return signal_reader_error(Qinvalid_read_syntax, stream,
			       "expected end of token");
  }
}

static bool
is_delimiter(int c)
{
  switch (c) {
  case EOF:
  case '\t':
  case '\n':
  case '\f':
  case '\r':
  case ' ':
  case 133:
  case '(':
  case ')':
  case '[':
  case ']':
  case '"':
  case ';':
  case '#':
    return true;
  default:
    return false;
  }
}

static repv
read_character(repv stream, int *c_p)
{
  static const struct {
    char *name;
    int value;
  } char_names[] = {

    /* standard character names */

    {"alarm", '\007'},
    {"backspace", '\010'},
    {"tab", '\011'},
    {"linefeed", '\012'},
    {"newline", '\012'},
    {"vtab", '\013'},
    {"page", '\014'},
    {"return", '\015'},
    {"esc", '\033'},
    {"space", ' '},
    {"delete", '\177'},
    {"rubout", '\177'},

    /* control codes */

    {"nul", '\000'},
    {"soh", '\001'},
    {"stx", '\002'},
    {"etx", '\003'},
    {"eot", '\004'},
    {"enq", '\005'},
    {"ack", '\006'},
    {"bel", '\007'},
    {"bs",  '\010'},
    {"lf",  '\012'},
    {"vt",  '\013'},
    {"ff",  '\014'},
    {"cr",  '\015'},
    {"so",  '\016'},
    {"si",  '\017'},
    {"dle", '\020'},
    {"dc1", '\021'},
    {"dc2", '\022'},
    {"dc3", '\023'},
    {"dc4", '\024'},
    {"nak", '\025'},
    {"syn", '\026'},
    {"etb", '\027'},
    {"can", '\030'},
    {"em",  '\031'},
    {"sub", '\032'},
    {"esc", '\033'},
    {"fs",  '\034'},
    {"gs",  '\035'},
    {"rs",  '\036'},
    {"us",  '\037'},
  };

  uint8_t buf[16];
  size_t size = 0;

  while (size < sizeof(buf)) {
    int c = rep_stream_getc(stream);
    if (is_delimiter(c)) {
      if (size == 0) {
	buf[size++] = (uint8_t)c;
	c = rep_stream_getc(stream);
      }
      buf[size] = 0;
      *c_p = c;
      break;
    }
    buf[size++] = (uint8_t)c;
  }

  switch (buf[0]) {
  case '0': case '1': case '2':  case '3':
  case '4': case '5': case '6':  case '7':
    if (size == 2 || size == 3) {
      uint32_t u = 0;
      for (size_t i = 0; i < size; i++) {
	unsigned int c = buf[i];
	if (!(c >= '0' && c <= '7')) {
	  return signal_reader_error(Qinvalid_read_syntax, stream,
				     "invalid octal character");
	}
	u = u * 8 + (c - '0');
      }
      return rep_intern_char(u);
    }
    break;

  case 'x':
    if (size > 1) {
      uint32_t u = 0;
      for (size_t i = 1; i < size; i++) {
	int h = hexdigit(buf[i]);
	if (h < 0) {
	  return signal_reader_error(Qinvalid_read_syntax, stream,
				     "invalid hexadecimal character");
	}
	u = u * 16 + h;
      }
      return rep_intern_char(u);
    }
    break;
  }

  if (buf[0] < 128) {
    if (size == 1) {
      /* Single byte, not an extended UTF-8 sequence. */
      return rep_intern_char(buf[0]);
    }

    for (size_t i = 0; i < size; i++) {
      if (buf[i] >= 128) {
	return signal_reader_error(Qinvalid_read_syntax, stream,
				   "invalid character name");
      }
      buf[i] = rep_tolower(buf[i]);
    }

    char c0 = buf[0], c1 = buf[1];
    for (size_t i = 0; i < sizeof(char_names) / sizeof(char_names[0]); i++) {
      if (char_names[i].name[0] == c0 && char_names[i].name[1] == c1
	  && strcmp(char_names[i].name + 2, (char *)buf + 2) == 0)
      {
	return rep_intern_char(char_names[i].value);
      }
    }

    return signal_reader_error(Qinvalid_read_syntax, stream,
			       "invalid character name");
  }

  /* Must be an extended UTF-8 character. */

  size_t len = utf8_code_point_size(buf[0]);

  if (len != size) {
    return signal_reader_error(Qinvalid_read_syntax, stream,
			       "invalid UTF-8 character");
  }

  uint32_t u;
  utf8_to_utf32(&u, buf, size);

  return rep_intern_char(u);
}

/* Using the above read_*() functions this classifies each type of
   expression and translates it into a lisp object. Returns NULL in
   case of error. */

static repv
readl(repv stream, register int *c_p, repv end_of_stream_error)
{
  while (1) {
    switch (*c_p) {
      repv form;
      rep_GC_root gc_form;

    case EOF:
      return signal_reader_error(end_of_stream_error, rep_LIST_1(stream), 0);

    case ' ':
    case '\t':
    case '\n':
    case '\f':
    case '\r':
      *c_p = rep_stream_getc(stream);
      continue;

    case ';': {
      int c;
      while ((c = rep_stream_getc(stream)) != EOF
	     && c != '\n' && c != '\f' && c != '\r')
      {
      }
      *c_p = rep_stream_getc(stream);
      continue; }

    case '(':
      return read_list(stream, c_p);

    case '\'': case '`': 
      /* 'X => (quote X)
         `X => (backquote X) */
      form = Fcons(*c_p == '\'' ? Qquote : Qbackquote,
		   Fcons(rep_nil, rep_nil));
      rep_PUSHGC(gc_form, form);
      if ((*c_p = rep_stream_getc(stream)) == EOF) {
	rep_POPGC;
	return signal_reader_error(Qpremature_end_of_stream,
				   stream, "during ` or ' syntax");
      }
      rep_CADR(form) = readl(stream, c_p, Qpremature_end_of_stream);
      rep_POPGC;
      if (!rep_CADR(form)) {
	return 0;
      }
      return form;

    case ',':
      /* ,@X => (backquote-splice X)
         ,X  => (backquote-unquote X) */
      form = Fcons(Qbackquote_unquote, Fcons(rep_nil, rep_nil));
      rep_PUSHGC(gc_form, form);
      switch ((*c_p = rep_stream_getc(stream))) {
      case EOF:
	rep_POPGC;
	return signal_reader_error(Qpremature_end_of_stream,
				   stream, "during , syntax");
      case '@':
	rep_CAR(form) = Qbackquote_splice;
	if ((*c_p = rep_stream_getc(stream)) == EOF) {
	  rep_POPGC;
	  return signal_reader_error(Qpremature_end_of_stream,
				     stream, "during ,@ syntax");
	}
      }
      rep_CADR(form) = readl(stream, c_p, Qpremature_end_of_stream);
      rep_POPGC;
      if (!rep_CADR(form)) {
	return 0;
      }
      return form;

    case '[':
      return read_vector(stream, c_p);

    case '"':
      return read_string(stream, c_p);

    case '#':
      switch (*c_p = rep_stream_getc(stream)) {
      case EOF:
	return signal_reader_error(Qpremature_end_of_stream,
				   stream, "during # syntax");

      case '[': {
	repv vec = read_vector(stream, c_p);
	if (!vec) {
	  return 0;
	}
	if (rep_VECTOR_LEN(vec) >= rep_BYTECODE_MIN_SLOTS
	    && rep_STRINGP(rep_BYTECODE_CODE(vec))
	    && rep_VECTORP(rep_BYTECODE_CONSTANTS(vec))
	    && rep_INTP(rep_BYTECODE_STACK(vec)))
	{
	  rep_BYTECODE(vec)->car = (rep_BYTECODE(vec)->car
				    & ~rep_CELL8_TYPE_MASK) | rep_Bytecode;
	  return vec;
	}
	return signal_reader_error(Qinvalid_read_syntax,
				   stream, "invalid bytecode"); }

      case '(':
	return read_vector(stream, c_p);

      case '|':
	/* comment delimited by `#| ... |#' */
	read_comment(stream, c_p);
	if (rep_INTERRUPTP) {
	  return 0;
	}
	continue;

      case '\\':
	return read_character(stream, c_p);

      case '!':
	if (rep_FILEP(stream)) {
	  repv pos = Fseek_file(stream, rep_nil, rep_nil);
	  if (pos && rep_INTP(pos) && rep_INT(pos) == 2) {
	    /* #! at the start of the file. Skip until !# */
	    read_comment(stream, c_p);
	    if (rep_INTERRUPTP) {
	      return 0;
	    }
	    continue;
	  }
	}
	switch (rep_stream_getc(stream)) {
	case 'o':
	  return skip_chars(stream, "ptional", ex_optional, c_p);
        case 'r':
	  return skip_chars(stream, "est", ex_rest, c_p);
	case 'k':
	  return skip_chars(stream, "ey", ex_key, c_p);
	default:
	  return signal_reader_error(Qinvalid_read_syntax, stream,
				     "unknown #! prefixed identifier");
	}
	// not reached

      case ':':
	rep_stream_ungetc(stream, *c_p);
	*c_p = '#';
	form = read_symbol(stream, c_p, rep_keyword_obarray);
	if (form && rep_SYMBOLP(form)) {
	  rep_SYM(form)->car |= rep_SF_KEYWORD;
	}
	return form;

      case 't': case 'T':
      case 'f': case 'F':
	form = (rep_tolower(*c_p) == 't') ? rep_scm_t : rep_scm_f;
	*c_p = rep_stream_getc(stream);
	return form;

      case 'b': case 'B': case 'o': case 'O':
      case 'd': case 'D': case 'x': case 'X':
      case 'e': case 'E': case 'i': case 'I':
	rep_stream_ungetc(stream, *c_p);
	*c_p = '#';
	goto identifier;

      case 'u':
	return skip_chars(stream, "ndefined", rep_undefined_value, c_p);

      default:
	return signal_reader_error(Qinvalid_read_syntax,
				   stream, "invalid token");
      }

    default:
    identifier:
      form = read_symbol(stream, c_p, rep_obarray);
      if (form && *c_p == '#' && rep_SYMBOLP(form)) {
	/* foo#bar expands to (structure-ref foo bar) */
	*c_p = rep_stream_getc(stream);
	repv var = read_symbol(stream, c_p, rep_obarray);
	if (var != 0) {
	  return rep_list_3(Qstructure_ref, form, var);
	} else {
	  return var;
	}
      }
      return form;
    }
  }
  /* not reached */
}

repv
rep_readl(repv stream, int *c_p)
{
  return readl(stream, c_p, Qend_of_stream);
}

void
rep_read_init(void)
{
  rep_INTERN(quote);
  rep_INTERN(backquote);
  rep_INTERN(backquote_unquote);
  rep_INTERN(backquote_splice);
  rep_INTERN(function);
  rep_INTERN(structure_ref);
}
