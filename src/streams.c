/* streams.c -- input/output streams

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

/* These are the Lisp objects which are classed as streams:

   FILE: [rw]
   MARK: [rw] advance pos attribute of mark afterwards
   BUFFER: [rw] from cursor pos
   (NUMBER . STRING): [r] from the NUMBER'th char of STRING
   (STRING . ACTUAL-LENGTH): [w] to after INDEX
   (BUFFER . POS): [rw] from BUFFER, POS is advanced
   (BUFFER . t): [w] end of BUFFER
   FUNCTION: [rw] call FUNCTION, when reading FUNCTION is expected to
  		  return the next character, when writing it is called with
  		  one arg, either character or string.
   PROCESS: [w] write to the stdin of the PROCESS if it's running
   t: [w] display in status line

   Note that when using any of the three BUFFER stream types, the buffer's
   restriction is respected. */

#include "repint.h"

#include <string.h>
#include <fcntl.h>
#include <ctype.h>
#include <stdlib.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

DEFSYM(standard_input, "*standard-input*");
DEFSYM(standard_output, "*standard-output*");
DEFSYM(format_hooks_alist, "*format-hooks-alist*");

/* ::doc:*format-hooks-alist*::
Alist of (CHAR . FUNCTION) defining extra format conversions for the
format function. FUNCTION is called as (FUNCTION VALUE), and should
return the string to be inserted.
::end:: */

int
rep_stream_getc(repv stream)
{
  if (stream == rep_nil) {
    stream = Fsymbol_value(Qstandard_input, rep_nil);
    if (!stream) {
      return EOF;
    }
  }

  int c = EOF;

  switch (rep_TYPE(stream)) {
  case rep_Cons:
    if (rep_INTP(rep_CAR(stream)) && rep_STRINGP(rep_CDR(stream))) {
      intptr_t idx = rep_INT(rep_CAR(stream));
      if (idx < rep_STRING_LEN(rep_CDR(stream))) {
	c = ((unsigned char *)rep_STR(rep_CDR(stream)))[idx];
	rep_CAR(stream) = rep_MAKE_INT(idx + 1);
      } else {
	c = EOF;
      }
      break;
    } else {
      rep_type *t = rep_get_data_type(rep_TYPE(rep_CAR(stream)));
      if (t->getc) {
	c = t->getc(stream);
      } else {
	Fsignal(Qinvalid_stream, rep_LIST_1(stream));
      }
    }
    break;

  case rep_Closure: {
    repv ret = rep_call_lisp0(stream);
    if (ret && rep_INTP(ret)) {
      c = rep_INT(ret);
    }
    break; }

  default:
    if (rep_FILEP(stream)) {
      if (rep_NILP(rep_FILE(stream)->name)) {
	c = EOF;
      } else if (rep_LOCAL_FILE_P(stream)) {
	c = getc(rep_FILE(stream)->file.fh);
      } else {
	c = rep_stream_getc(rep_FILE(stream)->file.stream);
      }
      if (c == '\n') {
	rep_FILE(stream)->line_number++;
      }
    } else {
      rep_type *t = rep_get_data_type(rep_TYPE(stream));
      if (t->getc) {
	c = t->getc(stream);
      } else {
	Fsignal(Qinvalid_stream, rep_LIST_1(stream));
      }
    }
    break;
  }

  return c;
}

/* Puts back one character, it will be returned from the next call to
   getc() on this stream. Note that some types of stream don't actually
   use `c`, they just rewind pointers. Never call this unless you
   *have* *successfully* read from the stream previously.  */

void
rep_stream_ungetc(repv stream, int c)
{
  if (stream == rep_nil) {
    stream = Fsymbol_value(Qstandard_input, rep_nil);
    if (!stream) {
      return;
    }
  }

  switch (rep_TYPE(stream)) {
  case rep_Cons:
    if (rep_INTP(rep_CAR(stream)) && rep_STRINGP(rep_CDR(stream))) {
      rep_CAR(stream) = rep_MAKE_INT(rep_INT(rep_CAR(stream)) - 1);
    } else {
      rep_type *t = rep_get_data_type(rep_TYPE(rep_CAR(stream)));
      if (t->ungetc) {
	t->ungetc(stream, c);
      } else {
	Fsignal(Qinvalid_stream, rep_LIST_1(stream));
      }
    }
    break;

  case rep_Closure:
    rep_call_lisp1(stream, rep_MAKE_INT(c));
    break;

  default:
    if (rep_FILEP(stream)) {
      if (c == '\n') {
	rep_FILE(stream)->line_number--;
      }
      if (rep_LOCAL_FILE_P(stream)) {
	ungetc(c, rep_FILE(stream)->file.fh);
      } else {
	rep_stream_ungetc(rep_FILE(stream)->file.stream, c);
      }
    } else {
      rep_type *t = rep_get_data_type(rep_TYPE(stream));
      if (t->ungetc) {
	t->ungetc(stream, c);
      } else {
	Fsignal(Qinvalid_stream, rep_LIST_1(stream));
      }
    }
    break;
  }
}

int
rep_stream_putc(repv stream, int c)
{
  int rc = -1;

  if (stream == rep_nil) {
    stream = Fsymbol_value(Qstandard_output, rep_nil);
    if (!stream) {
      return 0;
    }
  }

  switch (rep_TYPE(stream)) {
  case rep_Cons: {
    if (rep_STRINGP(rep_CAR(stream))
	&& rep_STRING_WRITABLE_P(rep_CAR(stream))
	&& rep_INTP(rep_CDR(stream)))
    {
      repv str = rep_CAR(stream);
      intptr_t capacity = rep_INT(rep_CDR(stream));
      intptr_t len = rep_STRING_LEN(str);
      if (len + 1 >= capacity) {
	intptr_t new_capacity = capacity < 16 ? 32 : capacity * 2;
	repv new = rep_allocate_string(new_capacity + 1);
	if (!new) {
	  break;
	}
	memcpy(rep_STR(new), rep_STR(str), len);
	rep_CAR(stream) = new;
	rep_CDR(stream) = rep_MAKE_INT(new_capacity);
	str = new;
      }
      rep_STR(str)[len] = c;
      rep_STR(str)[len + 1] = 0;
      rep_string_set_len(str, len + 1);
      rc = 1;
    } else {
      rep_type *t = rep_get_data_type(rep_TYPE(rep_CAR(stream)));
      if (t->putc) {
	rc = t->putc(stream, c);
      } else {
	Fsignal(Qinvalid_stream, rep_LIST_1(stream));
      }
    }
    break; }

  case rep_Symbol:
    if (stream == Qt && rep_message_fun) {
      char buf[2] = {(char)c, 0};
      (*rep_message_fun)(rep_append_message, buf, (size_t)1);
      rc = 1;
    }
    break;

  case rep_Closure: {
    repv ret = rep_call_lisp1(stream, rep_MAKE_INT(c));
    if (ret != 0) {
      rc = 1;
    }
    break; }

  default:
    if (rep_FILEP(stream)) {
      if (rep_FILE(stream)->name == rep_nil) {
	rep_unbound_file_error(stream);
	return 0;
      }
      else if (rep_LOCAL_FILE_P(stream)) {
	if (putc(c, rep_FILE(stream)->file.fh) != EOF) {
	  rc = 1;
	}
      } else {
	rc = rep_stream_putc(rep_FILE(stream)->file.stream, c);
      }
    } else {
      rep_type *t = rep_get_data_type(rep_TYPE(stream));
      if (t->putc) {
	rc = t->putc(stream, c);
      } else {
	Fsignal(Qinvalid_stream, rep_LIST_1(stream));
      }
    }
    break;
  }

  if (rc == 1) {
    return 1;
  }

  if (!rep_FILEP(stream)
      || !(rep_FILE(stream)->car & rep_LFF_SILENT_ERRORS))
  {
    Fsignal(Qend_of_stream, rep_LIST_1(stream));
  }

  return 0;
}

intptr_t
rep_stream_puts(repv stream, const void *data,
		intptr_t data_len, bool lisp_string)
{
  if (stream == rep_nil) {
    stream = Fsymbol_value(Qstandard_output, rep_nil);
    if (!stream) {
      return 0;
    }
  }

  intptr_t rc = -1;
  const char *buf = lisp_string ? rep_STR(data) : data;

  if (data_len < 0) {
    data_len = lisp_string ? rep_STRING_LEN(rep_VAL(data)) : strlen(buf);
  }

  switch (rep_TYPE(stream)) {
  case rep_Cons:
    if (rep_STRINGP(rep_CAR(stream))
	&& rep_STRING_WRITABLE_P(rep_CAR(stream))
	&& rep_INTP(rep_CDR(stream)))
    {
      repv str = rep_CAR(stream);
      intptr_t capacity = rep_INT(rep_CDR(stream));
      intptr_t len = rep_STRING_LEN(str);
      intptr_t new_len = len + data_len + 1;
      if (capacity <= new_len) {
	int new_capacity = capacity < 16 ? 32 : capacity * 2;
	while (new_capacity <= new_len) {
	  new_capacity = new_capacity * 2;
	}
	repv new = rep_allocate_string(new_capacity + 1);
	if (!new) {
	  break;
	}
	memcpy(rep_STR(new), rep_STR(str), len);
	rep_CAR(stream) = new;
	rep_CDR(stream) = rep_MAKE_INT(new_capacity);
	str = new;
      }
      memcpy(rep_STR(str) + len, buf, data_len);
      rep_STR(str)[len + data_len] = 0;
      rep_string_set_len(str, len + data_len);
      rc = data_len;
      break;
    } else {
      rep_type *t = rep_get_data_type(rep_TYPE(rep_CAR(stream)));
      if (t->puts) {
	rc = t->puts(stream, data, data_len, lisp_string);
      } else {
	Fsignal(Qinvalid_stream, rep_LIST_1(stream));
      }
    }
    break;

  case rep_Symbol:
    if (stream == Qt && rep_message_fun) {
      (*rep_message_fun)(rep_append_message, buf, (size_t)data_len);
      rc = data_len;
    }
    break;

  case rep_Closure: {
    repv arg;
    if (lisp_string)
      arg = rep_VAL(data);
    else
      arg = rep_string_copy_n(buf, data_len);
    repv ret = rep_call_lisp1(stream, arg);
    if (ret) {
      rc = data_len;
    }
    break; }

  default:
    if (rep_FILEP(stream)) {
      if (rep_NILP(rep_FILE(stream)->name)) {
	rep_unbound_file_error(stream);
	return 0;
      } else if (rep_LOCAL_FILE_P(stream)) {
	rc = fwrite(buf, 1, data_len, rep_FILE(stream)->file.fh);
      } else {
	rc = rep_stream_puts(rep_FILE(stream)->file.stream,
			     data, data_len, lisp_string);
      }
    } else {
      rep_type *t = rep_get_data_type(rep_TYPE(stream));
      if (t->puts) {
	rc = t->puts(stream, data, data_len, lisp_string);
      } else {
	Fsignal(Qinvalid_stream, rep_LIST_1(stream));
      }
    }
    break;
  }

  if (rc == data_len) {
    return rc;
  }

  if (!rep_FILEP(stream)
      || (rep_FILE(stream)->car & rep_LFF_SILENT_ERRORS) == 0)
  {
    Fsignal(Qend_of_stream, rep_LIST_1(stream));
  }

  return 0;
}

/* Read an escape sequence from STREAM. `*c_p` should contain the first
   character of the escape *not* the escape character. Supported
   sequences are,

     n   newline
     r   carriage return
     f   form feed
     t   horizontal tab
     v   vertical tab
     a   bell
     ^C  control code of C
     012 octal character code
     x12 hex character code

   Otherwise the character is returned as-is.  */

int
rep_stream_read_esc(repv stream, int *c_p)
{
  char c;

  switch (*c_p)
  {
  case 'n':
    c = '\n';
    break;

  case 'r':
    c = '\r';
    break;

  case 'f':
    c = '\f';
    break;

  case 't':
    c = '\t';
    break;

  case 'v':
    c = '\v';
    break;

  case 'a':
    c = '\a';
    break;

  case '^':
    c = rep_toupper(rep_stream_getc(stream)) ^ 0x40;
    break;

  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
    c = *c_p - '0';
    *c_p = rep_stream_getc(stream);
    if ((*c_p >= '0') && (*c_p <= '7')) {
      c = (c * 8) + (*c_p - '0');
      *c_p = rep_stream_getc(stream);
      if ((*c_p >= '0') && (*c_p <= '7')) {
	c = (c * 8) + (*c_p - '0');
	break;
      } else {
	return c;
      }
    } else {
      return c;
    }

  case 'x':
    c = 0;
    while (1) {
      *c_p = rep_stream_getc(stream);
      if (!rep_isxdigit(*c_p)) {
	return c;
      }
      if ((*c_p >= '0') && (*c_p <= '9')) {
	c = (c * 16) + (*c_p - '0');
      } else {
	c = (c * 16) + (rep_toupper(*c_p) - 'A') + 10;
      }
    }
    /* not reached */

  default:
    c = *c_p;
  }

  *c_p = rep_stream_getc(stream);
  return c;
}

DEFUN("write", Fwrite, Swrite,
      (repv stream, repv data, repv len), rep_Subr3) /*
::doc:rep.io.streams#write::
write STREAM DATA [LENGTH]

Writes DATA, which can either be a string or a character, to the stream
STREAM, returning the number of characters actually written. If DATA is
a string LENGTH can define how many characters to write.
::end:: */
{
  int written;

  switch (rep_TYPE(data)) {
    bool lisp_string;
    void *arg;

  case rep_Int:
    written = rep_stream_putc(stream, rep_INT(data));
    break;

  case rep_String:
    if (rep_INTP(len)) {
      written = rep_INT(len);
      if (written > rep_STRING_LEN(data)) {
	return rep_signal_arg_error(len, 3);
      }
      if (written == rep_STRING_LEN(data)) {
	arg = rep_PTR(data);
	lisp_string = true;
      } else {
	arg = rep_STR(data);
	lisp_string = false;
      }
    } else {
      written = rep_STRING_LEN(data);
      lisp_string = true;
      arg = rep_PTR(data);
    }
    written = rep_stream_puts(stream, arg, written, lisp_string);
    break;

  default:
    return rep_signal_arg_error(data, 2);
  }

  return !rep_INTERRUPTP ? rep_MAKE_INT(written) : 0;
}

DEFUN("read-char", Fread_char, Sread_char, (repv stream), rep_Subr1) /*
::doc:rep.io.streams#read-char::
read-char STREAM

Reads the next character from the input-stream STREAM, if no more characters
are available returns nil.
::end:: */
{
  int c = rep_stream_getc(stream);

  if (c != EOF) {
    return rep_MAKE_INT(c);
  } else {
    return rep_nil;
  }
}

DEFUN("peek-char", Fpeek_char, Speek_char, (repv stream), rep_Subr1) /*
::doc:rep.io.streams#peek-char::
peek-char STREAM

Returns the next character from the input-stream STREAM, *without*
removing that character from the head of the stream. If no more
characters are available returns nil.
::end:: */
{
  int c = rep_stream_getc(stream);

  if (c != EOF) {
    rep_stream_ungetc(stream, c);
    return rep_MAKE_INT(c);
  } else {
    return rep_nil;
  }
}

DEFUN("read-chars", Fread_chars, Sread_chars,
      (repv stream, repv count), rep_Subr2) /*
::doc:rep.io.streams#read-chars::
read-chars STREAM COUNT

Read no more than COUNT characters from the input stream STREAM,
returning a string containing the characters. If EOF is read before
reading COUNT characters, the returned string will contain the
characters read up to that point. If no characters are read, nil will
be returned.
::end:: */
{
  rep_DECLARE2(count, rep_INTP);

  char *buf = rep_stack_alloc(char, rep_INT(count));
  if (!buf) {
    return rep_mem_error();
  }

  intptr_t len = 0;

  if (rep_FILEP(stream) && rep_LOCAL_FILE_P(stream)) {
    len = fread(buf, sizeof(char), rep_INT(count), rep_FILE(stream)->file.fh);
    rep_FILE(stream)->car |= rep_LFF_BOGUS_LINE_NUMBER;
  } else {
    int c;
    while (len < rep_INT(count) && (c = rep_stream_getc(stream)) != EOF) {
      buf[len++] = c;
    }
  }

  repv ret = len > 0 ? rep_string_copy_n(buf, len) : rep_nil;

  rep_stack_free(char, rep_INT(count), buf);

  return ret;
}

DEFUN("read-line", Fread_line, Sread_line, (repv stream), rep_Subr1) /*
::doc:rep.io.streams#read-line::
read-line STREAM

Read one line of text from STREAM.
::end:: */
{
  if (rep_FILEP(stream) && rep_LOCAL_FILE_P(stream)) {
    char *line = NULL;
    size_t linecap = 0;
    intptr_t len = getline(&line, &linecap, rep_FILE(stream)->file.fh);
    if (len > 0) {
      return rep_box_string(line, len);
    } else {
      return rep_nil;
    }
  }

  char static_buf[256];
  char *buf = static_buf;
  size_t buflen = sizeof(static_buf);
  intptr_t i = 0;

  while (1) {
    int c = rep_stream_getc(stream);
    if (c == EOF) {
      break;
    }

    if (i == buflen) {
      size_t newlen = buflen * 2;
      char *newbuf = malloc(newlen);
      if (!newbuf) {
	break;
      }
      memcpy(newbuf, buf, buflen);
      buf = newbuf;
      buflen = newlen;
    }

    buf[i++] = (char) c;
    if (c == '\n') {
      break;
    }
  }

  repv ret = i != 0 ? rep_string_copy_n(buf, i) : rep_nil;

  if (buf != static_buf) {
    free(buf);
  }

  return ret;
}

DEFUN("copy-stream", Fcopy_stream, Scopy_stream, (repv source, repv dest), rep_Subr2) /*
::doc:rep.io.streams#copy-stream::
copy-stream SOURCE-STREAM DEST-STREAM

Copy all characters from SOURCE-STREAM to DEST-STREAM until an EOF is
read. Returns the number of characters copied.
::end:: */
{
  intptr_t total = 0;

  char buf[4096];
  intptr_t i = 0;

  while (1) {
    int c = rep_stream_getc(source);

    if (c == EOF || i == sizeof(buf) - 1) {
      buf[i] = 0;
      rep_stream_puts(dest, buf, i, false);
      i = 0;
    }

    if (c == EOF) {
      break;
    }

    buf[i++] = c;
    total++;

    rep_TEST_INT;
    if (rep_INTERRUPTP)
      return 0;
  }

  return rep_MAKE_INT(total);
}

DEFUN("read", Fread, Sread, (repv stream), rep_Subr1) /*
::doc:rep.io.streams#read::
read [STREAM]

Reads one Lisp expression from the input-stream STREAM (or the value of
the variable `*standard-input*' if STREAM is nil) and return it.
::end:: */
{
  if (stream == rep_nil) {
    stream = Fsymbol_value(Qstandard_input, rep_nil);
    if (!stream) {
      return rep_signal_arg_error(stream, 1);
    }
  }

  int c = rep_stream_getc(stream);
  
  if (c == EOF) {
    return Fsignal(Qend_of_stream, rep_LIST_1(stream));
  }

  repv form = rep_readl(stream, &c);

  if (form && c != EOF) {
    rep_stream_ungetc(stream, c);
  }

  return form;
}

DEFUN("print", Fprint, Sprint, (repv obj, repv stream), rep_Subr2) /*
::doc:rep.io.streams#print::
print OBJECT [STREAM]

First outputs a newline, then prints a text representation of OBJECT to
STREAM (or the contents of the variable `*standard-output*') in a form
suitable for `read'.
::end:: */
{
  if (stream == rep_nil) {
    stream = Fsymbol_value(Qstandard_output, rep_nil);
    if (!stream) {
      return rep_signal_arg_error(stream, 1);
    }
  }

  rep_stream_putc(stream, '\n');
  rep_print_val(stream, obj);

  return !rep_INTERRUPTP ? Qt : 0;
}

DEFUN("prin1", Fprin1, Sprin1, (repv obj, repv stream), rep_Subr2) /*
::doc:rep.io.streams#prin1::
prin1 OBJECT [STREAM]

Prints a text representation of OBJECT to STREAM (or the contents of the
variable `*standard-output*') in a form suitable for `read'.
::end:: */
{
  if (stream == rep_nil) {
    stream = Fsymbol_value(Qstandard_output, rep_nil);
    if (!stream) {
      return rep_signal_arg_error(stream, 1);
    }
  }

  rep_print_val(stream, obj);
  return !rep_INTERRUPTP ? Qt : 0;
}

DEFUN("princ", Fprinc, Sprinc, (repv obj, repv stream), rep_Subr2) /*
::doc:rep.io.streams#princ::
princ OBJECT [STREAM]

Prints a text representation of OBJECT to STREAM (or the contents of the
variable *standard-output*), no strange characters are quoted and no quotes
are printed around strings.
::end:: */
{
  if (stream == rep_nil) {
    stream = Fsymbol_value(Qstandard_output, rep_nil);
    if (!stream) {
      return rep_signal_arg_error(stream, 1);
    }
  }

  rep_princ_val(stream, obj);
  return !rep_INTERRUPTP ? Qt : 0;
}

DEFUN("format", Fformat, Sformat, (repv args), rep_SubrN) /*
::doc:rep.io.streams#format::
format STREAM FORMAT-STRING ARGS...

Writes a string created from the format specification FORMAT-STRING and
the argument-values ARGS to the stream, STREAM. If STREAM is nil a
string is created and returned.

FORMAT-STRING is a template for the result, any `%' characters
introduce a substitution, using the next unused ARG. The substitutions
have the following syntax,

	%[FLAGS][FIELD-WIDTH][.PRECISION]CONVERSION

FIELD-WIDTH is a positive decimal integer, defining the size in
characters of the substitution output. PRECISION is only valid when
printing floating point numbers.

CONVERSION is a character defining how to convert the corresponding ARG
to text. The default options are:

	d	Output ARG as a decimal integer
	x, X	Output ARG as a hexadecimal integer
	o	Output ARG as an octal integer
	c	Output ARG as a character
	s	Output the result of `(prin1 ARG)'
	S	Output the result of `(princ ARG)'

FLAGS is a sequence of zero or more of the following characters,

	-	Left justify substitution within field
	^	Truncate substitution at size of field
	0	Pad the field with zeros instead of spaces
	+	For d, x, and o conversions, output a leading plus
		 sign if ARG is positive
	` '	(A space) For d, x, and o conversions, if the result
		 doesn't start with a plus or minus sign, output a
		 leading space

The list of CONVERSIONS can be extended through the
*format-hooks-alist* variable; the strings created by these extra
conversions are formatted as if by the `s' conversion. 

Note that the FIELD-WIDTH and all flags currently have no effect on the
`S' conversion, (or the `s' conversion when the ARG isn't a string).
::end:: */
{
  if (!rep_CONSP(args)) {
    return rep_signal_missing_arg(1);
  }

  repv stream = rep_CAR(args);
  args = rep_CDR(args);

  bool make_string = false;

  if (stream == rep_nil) {
    stream = Fcons(rep_string_copy_n("", 0), rep_MAKE_INT(0));
    make_string = true;
  }

  if (!rep_CONSP(args)) {
    return rep_signal_missing_arg(2);
  }

  repv format = rep_CAR(args);
  args = rep_CDR(args);

  rep_DECLARE2(format, rep_STRINGP);

  repv extra_formats = 0;

  rep_GC_root gc_stream, gc_format, gc_args, gc_extra_formats;
  rep_PUSHGC(gc_stream, stream);
  rep_PUSHGC(gc_format, format);
  rep_PUSHGC(gc_args, args);
  rep_PUSHGC(gc_extra_formats, extra_formats);

  const char *fmt = rep_STR(format);
  const char *last_fmt = fmt;

  int arg_idx = 0;

  while (1) {
    char c = *fmt++;
    if (!c) {
      break;
    } else if (c != '%') {
      continue;
    }

    bool left_justify = false;
    bool truncate_field = false;
    bool pad_zeros = false;
    char leading_char = 0;
    int field_width = 0;
    int precision = 0;

    if (last_fmt != fmt - 1) {
      rep_stream_puts(stream, last_fmt, fmt - last_fmt - 1, false);
      if (rep_INTERRUPTP) {
	goto exit;
      }
    }

    const char *tem = fmt;
    while (1) {
      switch (*tem++) {
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
	break;
      case '$': {
	int arg = atoi(fmt);
	if (arg > 0) {
	  arg_idx = arg - 1;
	  fmt = tem;
	}
	goto parse_flags; }
      default:
	goto parse_flags;
      }
    }

  parse_flags:
    c = *fmt++;
    while (1) {
      switch (c) {
      case '-':
	left_justify = true;
	break;
      case '^':
	truncate_field = true;
	break;
      case '0':
	pad_zeros = true;
	break;
      case '+': case ' ':
	leading_char = c;
	break;
      default:
	goto parse_field_width;
      }
      c = *fmt++;
    }

  parse_field_width:
    while (rep_isdigit(c)) {
      field_width = field_width * 10 + (c - '0');
      c = *fmt++;
    }
    if (c == '.') {
      c = *fmt++;
      while (c && rep_isdigit(c)) {
	precision = precision * 10 + (c - '0');
	c = *fmt++;
      }
    } else {
      precision = -1;
    }

    if (c == '%') {
      rep_stream_putc(stream, '%');
    } else {
      repv fun;
      repv val = Fnth(rep_MAKE_INT(arg_idx), args);
      bool free_str = false;

      if (!val) {
	goto exit;
      }

      switch (c) {
	int radix;
	intptr_t len, actual_len;
	char buf[256], *ptr;

      case 'c':
	rep_stream_putc(stream, rep_INT(val));
	break;

      case 'x': case 'X':
	radix = 16;
	goto do_number;

      case 'o':
	radix = 8;
	goto do_number;

      case 'd':
	radix = 10;
      do_number:
	ptr = rep_print_number_to_string(val, radix, precision);
	if (!ptr) {
	  break;
	}
	free_str = true;
	len = strlen(ptr);
	goto string_out;

      case 's':
      unquoted:
	if (!rep_STRINGP(val) || (left_justify && field_width == 0)) {
	  rep_princ_val(stream, val);
	  break;
	}
	ptr = rep_STR(val);
	len = rep_STRING_LEN(val);

      string_out:
	actual_len = len;
	if (leading_char) {
	  if (*ptr != '-') {
	    actual_len++;
	  } else {
	    leading_char = 0;
	  }
	}
	if (field_width == 0 || actual_len >= field_width) {
	  if (leading_char) {
	    rep_stream_putc(stream, leading_char);
	  }
	  rep_stream_puts(stream, ptr, truncate_field
			  ? (field_width - (leading_char != 0)) : len, false);
	} else {
	  intptr_t slen = MIN(field_width - actual_len, sizeof(buf));
	  memset(buf, !pad_zeros ? ' ' : '0', slen);
	  if (left_justify) {
	    if (leading_char) {
	      rep_stream_putc(stream, leading_char);
	    }
	    rep_stream_puts(stream, ptr, len, false);
	  }
	  rep_stream_puts(stream, buf, slen, false);
	  if (!left_justify) {
	    if (leading_char) {
	      rep_stream_putc(stream, leading_char);
	    }
	    rep_stream_puts(stream, ptr, len, false);
	  }
	}
	if (free_str) {
	  free(ptr);
	}
	break;

      case 'S':
	rep_print_val(stream, val);
	break;

      default:
	if (extra_formats == 0) {
	  extra_formats = Fsymbol_value(Qformat_hooks_alist, Qt);
	}
	if (rep_CONSP(extra_formats)
	    && (fun = Fassq(rep_MAKE_INT(c), extra_formats))
	    && rep_CONSP(fun))
	{
	  val = rep_call_lisp1(rep_CDR(fun), val);
	  if (!val) {
	    goto exit;
	  }
	  if (val == rep_nil) {
	    val = rep_null_string();
	  }
	  goto unquoted;
	} else {
	  DEFSTRING(err, "Unknown format conversion");
	  Fsignal(Qerror, rep_list_2(rep_VAL(&err), rep_MAKE_INT(c)));
	  goto exit;
	}
      }
      arg_idx++;
    }
    last_fmt = fmt;

    rep_TEST_INT;
    if (rep_INTERRUPTP) {
      break;
    }
  }

  if (last_fmt != fmt - 1) {
    rep_stream_puts(stream, last_fmt, fmt - last_fmt - 1, false);
  }

  if (make_string) {
    if (rep_STRING_LEN(rep_CAR(stream)) != rep_INT(rep_CDR(stream))) {
      stream = Fcopy_sequence(rep_CAR(stream));
    } else {
      stream = rep_CAR(stream);
    }
  }

exit:
  rep_POPGC; rep_POPGC; rep_POPGC; rep_POPGC;

  return !rep_INTERRUPTP ? stream : 0;
}

DEFUN("make-string-input-stream", Fmake_string_input_stream,
      Smake_string_input_stream, (repv string, repv start), rep_Subr2) /*
::doc:rep.io.streams#make-string-input-stream::
make-string-input-stream STRING [START]

Returns a input stream, it will supply, in order, the characters in
STRING, starting from START (or the beginning of the string).
::end:: */
{
  rep_DECLARE1(string, rep_STRINGP);

  return Fcons(rep_INTP(start) ? start : rep_MAKE_INT(0), string);
}

DEFUN("make-string-output-stream", Fmake_string_output_stream,
      Smake_string_output_stream, (void), rep_Subr0) /*
::doc:rep.io.streams#make-string-output-stream::
make-string-output-stream

Returns an output stream which will accumulate the characters written
to it for the use of the `get-output-stream-string' function.
::end:: */
{
  return Fcons(rep_string_copy_n("", 0), rep_MAKE_INT(0));
}

DEFUN("get-output-stream-string", Fget_output_stream_string,
      Sget_output_stream_string, (repv strm), rep_Subr1) /*
::doc:rep.io.streams#get-output-stream-string::
get-output-stream-string STRING-OUTPUT-STREAM

Returns a string containing the characters written to the stream
STRING-OUTPUT-STREAM (created by `make-string-output-stream'). The
stream is then reset so that the next call to this function with this
stream will only return the new characters.
::end:: */
{
  if (!rep_CONSP(strm)
      || !rep_STRINGP(rep_CAR(strm))
      || !rep_INTP(rep_CDR(strm)))
  {
    return rep_signal_arg_error(strm, 1);
  }

  repv string;
  if (rep_STRING_LEN(rep_CAR(strm)) != rep_INT(rep_CDR(strm))) {
    /* Truncate to written length. */
    string = Fcopy_sequence(rep_CAR(strm));
  } else {
    string = rep_CAR(strm);
  }

  rep_CAR(strm) = rep_string_copy_n("", 0);
  rep_CDR(strm) = rep_MAKE_INT(0);

  return string;
}

DEFUN("input-stream-p", Finput_stream_p,
      Sinput_stream_p, (repv arg), rep_Subr1) /*
::doc:rep.io.streams#input-stream-p::
input-stream-p ARG

Returns t if ARG is an input stream.
::end:: */
{
  switch (rep_TYPE(arg)) {
  case rep_Closure:
    return Qt;

  case rep_Cons:
    if (rep_INTP(rep_CAR(arg)) && rep_STRINGP(rep_CDR(arg))) {
      return Qt;
    } else {
      rep_type *t = rep_get_data_type(rep_TYPE(rep_CAR(arg)));
      if (t->getc && t->ungetc) {
	return Qt;
      }
    }
    break;

  default:
    if (rep_FILEP(arg)) {
      return Qt;
    } else {
      rep_type *t = rep_get_data_type(rep_TYPE(arg));
      if (t->getc && t->ungetc) {
	return Qt;
      }
    }
    break;
  }

  return rep_nil;
}

DEFUN("output-stream-p", Foutput_stream_p,
      Soutput_stream_p, (repv arg), rep_Subr1) /*
::doc:rep.io.streams#output-stream-p::
output-stream-p ARG

Returns t if ARG is an output stream.
::end:: */
{
  switch (rep_TYPE(arg)) {
  case rep_Symbol:
    if (arg == Qt) {
      return Qt;
    }
    break;

  case rep_Closure:
    return Qt;
    break;

  case rep_Cons:
    if (rep_STRINGP(rep_CAR(arg)) && rep_INTP(rep_CDR(arg))) {
      return Qt;
    } else {
      rep_type *t = rep_get_data_type(rep_TYPE(rep_CAR(arg)));
      if (t->putc && t->puts) {
	return Qt;
      }
    }
    break;

  default:
    if (rep_FILEP(arg)) {
      return Qt;
    } else {
      rep_type *t = rep_get_data_type(rep_TYPE(arg));
      if (t->putc && t->puts) {
	return Qt;
      }
    }
    break;
  }

  return rep_nil;
}

void
rep_streams_init(void)
{
  rep_INTERN(standard_input);
  rep_INTERN(standard_output);

  repv tem = rep_push_structure("rep.io.streams");
  rep_INTERN_SPECIAL(format_hooks_alist);
  rep_ADD_SUBR(Swrite);
  rep_ADD_SUBR(Sread_char);
  rep_ADD_SUBR(Speek_char);
  rep_ADD_SUBR(Sread_chars);
  rep_ADD_SUBR(Sread_line);
  rep_ADD_SUBR(Scopy_stream);
  rep_ADD_SUBR(Sread);
  rep_ADD_SUBR(Sprint);
  rep_ADD_SUBR(Sprin1);
  rep_ADD_SUBR(Sprinc);
  rep_ADD_SUBR(Sformat);
  rep_ADD_SUBR(Smake_string_input_stream);
  rep_ADD_SUBR(Smake_string_output_stream);
  rep_ADD_SUBR(Sget_output_stream_string);
  rep_ADD_SUBR(Sinput_stream_p);
  rep_ADD_SUBR(Soutput_stream_p);
  rep_pop_structure(tem);
}
