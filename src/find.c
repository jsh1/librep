/* find.c -- Searching and replacing

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
#include <stdlib.h>
#include <assert.h>

/* Hooks for dealing with the rep_reg_obj match type. */
void (*rep_regsub_fun)(int, rep_regsubs *, const char *, char *, void *);
size_t (*rep_regsublen_fun)(int, rep_regsubs *, const char *, void *);


/* Compiling regexps. */

/* A linked list is used to store all recently-used regexps in MRU
   order. At GC the regexps at the tail of the list are freed to
   satisfy the size limit.

   It might be better to use a hash-table. But by experience it seems
   that the cache is usually quite small, and therefore searching the
   list each compilation isn't too bad (and it makes the gc easier).

   Also, the hit-ratio is very good (as I'm typing this, ~0.97) */

struct cached_regexp {
  struct cached_regexp *next;
  repv regexp;
  rep_regexp *compiled;
};

static struct cached_regexp *cached_regexps;	/* should be a hash table? */
static int regexp_hits, regexp_misses;
static int regexp_cache_limit = 1024;

DEFSYM(regexp_error, "regexp-error");

rep_regexp *
rep_compile_regexp(repv re)
{
  assert(rep_STRINGP(re));
  size_t re_len = rep_STRING_LEN(re);

  struct cached_regexp **x = &cached_regexps;
  while (*x != 0) {
    repv saved_re = (*x)->regexp;
    if (saved_re == re
	|| (rep_STRING_LEN(saved_re) == re_len
	    && memcmp(rep_STR(saved_re), rep_STR(re), re_len) == 0))
    {
      /* Found it. Move this node to the head of the list. Then return
         the compiled copy. */
      struct cached_regexp *ptr = *x;
      if (x != &cached_regexps) {
	*x = ptr->next;
	ptr->next = cached_regexps;
	cached_regexps = ptr;
      }
      regexp_hits++;
      return ptr->compiled;
    }
    x = &((*x)->next);
  }

  /* No cached copy. Compile it, then add it to the cache. */

  rep_regexp *compiled = rep_regcomp(rep_STR(re));
  if (!compiled) {
    return NULL;
  }

  struct cached_regexp *ptr = rep_alloc(sizeof(struct cached_regexp));
  rep_data_after_gc += sizeof(struct cached_regexp) + compiled->regsize;

  ptr->regexp = re;
  ptr->compiled = compiled;

  ptr->next = cached_regexps;
  cached_regexps = ptr;

  rep_STRING(re)->car |= rep_STRING_REGEXP;

  regexp_misses++;
  return compiled;
}

/* Remove any cached compilation of STRING from the regexp cache */

void
rep_invalidate_string(repv str)
{
  if (!(rep_STRING(str)->car & rep_STRING_REGEXP)) {
    return;
  }

  struct cached_regexp **x;
  for (x = &cached_regexps; *x != 0; x = &((*x)->next)) {
    if ((*x)->regexp == str) {
      struct cached_regexp *ptr = *x;
      *x = ptr->next;
      free(ptr->compiled);
      rep_free(ptr);
    }
  }
}

static void
free_regexps(struct cached_regexp *ptr)
{
  while (ptr) {
    struct cached_regexp *next = ptr->next;
    free(ptr->compiled);
    rep_free(ptr);
    ptr = next;
  }
}

static void
mark_cached_regexps(void)
{
  size_t total = 0;

  struct cached_regexp *ptr = cached_regexps;
  struct cached_regexp *last = NULL;

  while (ptr && total < regexp_cache_limit) {
    rep_MARKVAL(ptr->regexp);
    total += sizeof(struct cached_regexp) + ptr->compiled->regsize;
    last = ptr;
    ptr = ptr->next;
  }

  if (last) {
    /* Free all following regexps */

    ptr = last->next;
    last->next = 0;

    free_regexps(ptr);
  }
}

/* Free all cached regexps */

static void
release_cached_regexps(void)
{
  struct cached_regexp *ptr = cached_regexps;
  cached_regexps = 0;

  free_regexps(ptr);
}


/* Storing regexp context. */
	
/* Storage for remembering where the last match was.
   last_match_data is the string or buffer that was matched against.
   last_matches is a copy of the subexpression data of the last match.  */

static rep_regtype last_match_type;
static repv last_match_data;
static rep_regsubs last_matches;

struct rep_saved_regexp_data *rep_saved_matches;

void
rep_update_last_match(repv data, rep_regexp *prog)
{
  last_match_type = prog->lasttype;
  last_match_data = data;
  memcpy(&last_matches, &prog->matches, sizeof(last_matches));
}

/* Called by GC */

void
rep_mark_regexp_data(void)
{
  /* Don't keep too many cached REs through GC. */

  mark_cached_regexps();

  if (last_match_type == rep_reg_obj) {
    for (int i = 0; i < rep_NSUBEXP; i++) {
      rep_MARKVAL(last_matches.obj.startp[i]);
      rep_MARKVAL(last_matches.obj.endp[i]);
    }
  }

  rep_MARKVAL(last_match_data);

  struct rep_saved_regexp_data *sd;
  for (sd = rep_saved_matches; sd != 0; sd = sd->next) {
    if (sd->type == rep_reg_obj) {
      for (int i = 0; i < rep_NSUBEXP; i++) {
	rep_MARKVAL(sd->matches.obj.startp[i]);
	rep_MARKVAL(sd->matches.obj.endp[i]);
      }
    }
    rep_MARKVAL(sd->data);
  }
}

/* Fix the match buffers to reflect matching a string from START to END. */

void
rep_set_string_match(repv obj, repv start, repv end)
{
  last_match_data = obj;
  last_match_type = rep_reg_obj;

  last_matches.obj.startp[0] = start;
  last_matches.obj.endp[0] = end;

  for (int i = 1; i < rep_NSUBEXP; i++) {
    last_matches.obj.startp[i] = 0;
    last_matches.obj.endp[i] = 0;
  }
}

void
rep_push_regexp_data(struct rep_saved_regexp_data *sd)
{
  sd->type = last_match_type;
  sd->data = last_match_data;
  memcpy(&sd->matches, &last_matches, sizeof(rep_regsubs));

  sd->next = rep_saved_matches;
  rep_saved_matches = sd;
}

void
rep_pop_regexp_data(void)
{
  struct rep_saved_regexp_data *sd = rep_saved_matches;
  rep_saved_matches = sd->next;

  last_match_type = sd->type;
  last_match_data = sd->data;
  memcpy(&last_matches, &sd->matches, sizeof(rep_regsubs));
}


/* Simple string matching */

DEFUN("string-match", Fstring_match, Sstring_match, (repv re, repv str, repv start, repv nocasep), rep_Subr4) /*
::doc:rep.regexp#string-match::
string-match REGEXP STRING [START] [IGNORE-CASE-P]

Return t if REGEXP matches STRING. Updates the match data.

When defined, START is the index of the first character to start
matching at (counting from zero). When IGNORE-CASE-P is non-nil the
case of matched strings are ignored. Note that character classes are
still case-significant.
::end:: */
{
  rep_DECLARE1(re, rep_STRINGP);
  rep_DECLARE2(str, rep_STRINGP);
  rep_DECLARE3_OPT(start, rep_INTP);

  intptr_t xstart = rep_INTP(start) ? rep_INT(start) : 0;
  rep_regexp *prog = rep_compile_regexp(re);

  if (!prog) {
    return 0;
  }

  if (rep_regexec2(prog, rep_STR(str) + xstart,
		   (rep_NILP(nocasep) ? 0 : rep_REG_NOCASE)
		   | (xstart == 0 ? 0 : rep_REG_NOTBOL)))
  {
    rep_update_last_match(str, prog);
    return Qt;
  }

  return rep_nil;
}

DEFUN("string-looking-at", Fstring_looking_at, Sstring_looking_at, (repv re, repv string, repv start, repv nocasep), rep_Subr4) /*
::doc:rep.regexp#string-looking-at::
string-looking-at REGEXP STRING [START] [IGNORE-CASE-P]

Returns t if REGEXP matches the STRING (starting at character START).
Updates the match data.
::end:: */
{
  rep_DECLARE1(re, rep_STRINGP);
  rep_DECLARE2(string, rep_STRINGP);
  rep_DECLARE3_OPT(start, rep_INTP);

  intptr_t xstart = rep_INTP(start) ? rep_INT(start) : 0;
  rep_regexp *prog = rep_compile_regexp(re);

  if (!prog) {
    return 0;
  }

  if (rep_regmatch_string(prog, rep_STR(string) + xstart,
			  (rep_NILP(nocasep) ? 0 : rep_REG_NOCASE)
			  | (xstart == 0 ? 0 : rep_REG_NOTBOL)))
  {
    rep_update_last_match(string, prog);
    return Qt;
  }

  return rep_nil;
}

DEFUN("expand-last-match", Fexpand_last_match, Sexpand_last_match, (repv template), rep_Subr1) /*
::doc:rep.regexp#expand-last-match::
expand-last-match TEMPLATE-STRING

Expand the saved expressions from the most recent successfully matched
regexp according to TEMPLATE-STRING, a string that may contain any of
the following escape sequences,

  \0, \&   whole string matched by REGEXP
  \N	   N'th parenthensized expression (1 <= N <= 9)
::end:: */
{
  rep_DECLARE1(template, rep_STRINGP);

  size_t len = (*rep_regsublen_fun)(last_match_type, &last_matches,
    rep_STR(template), rep_PTR(last_match_data));

  repv str = rep_allocate_string(len);
  (*rep_regsub_fun)(last_match_type, &last_matches, rep_STR(template),
		    rep_MUTABLE_STR(str), rep_PTR(last_match_data));
  return str;
}

DEFUN("match-start", Fmatch_start, Smatch_start, (repv exp), rep_Subr1) /*
::doc:rep.regexp#match-start::
match-start [EXPRESSION-INDEX]

Return the position which the EXPRESSION-INDEX'th parenthesised expression
started at in the last successful regexp match. If EXPRESSION-INDEX is
nil or 0 the start of the whole match is returned instead.
The returned value will either be a position if the last match was in a
buffer, or an integer if the last match was in a string (i.e. regexp-match).
::end:: */
{
  rep_DECLARE1_OPT(exp, rep_INTP);

  intptr_t i = 0;
  if (rep_INTP(exp)) {
    i = rep_INT(exp);
    if (i >= rep_NSUBEXP || i < 0) {
      return rep_signal_arg_error(exp, 1);
    }
  }

  if (last_match_type == rep_reg_obj) {
    if (!last_matches.obj.startp[i]) {
      return rep_nil;
    }
    return last_matches.obj.startp[i];
  } else {
    if (!last_matches.string.startp[i]) {
      return rep_nil;
    }
    return rep_MAKE_INT(last_matches.string.startp[i]
			- rep_STR(last_match_data));
  }
}
	
DEFUN("match-end", Fmatch_end, Smatch_end, (repv exp), rep_Subr1) /*
::doc:rep.regexp#match-end::
match-end [EXPRESSION-INDEX]

Return the position which the EXPRESSION-INDEX'th parenthesised expression
ended at in the last successful regexp match. If EXPRESSION-INDEX is
nil or 0 the end of the whole match is returned instead.
The returned value will either be a position if the last match was in a
buffer, or an integer if the last match was in a string (i.e. regexp-match).
::end:: */
{
  rep_DECLARE1_OPT(exp, rep_INTP);

  intptr_t i = 0;
  if (rep_INTP(exp)) {
    i = rep_INT(exp);
    if (i >= rep_NSUBEXP || i < 0) {
      return rep_signal_arg_error(exp, 1);
    }
  }

  if (last_match_type == rep_reg_obj) {
    if (!last_matches.obj.endp[i]) {
      return rep_nil;
    }
    return last_matches.obj.endp[i];
  } else {
    if (!last_matches.string.endp[i]) {
      return rep_nil;
    }
    return rep_MAKE_INT(last_matches.string.endp[i]
			- rep_STR(last_match_data));
  }
}

DEFUN("quote-regexp", Fquote_regexp, Squote_regexp, (repv str), rep_Subr1) /*
::doc:rep.regexp#quote-regexp::
quote-regexp STRING

Returns a new version of STRING, any characters which the regexp routines
treat specially (asterisks, square brackets, etc...) is quoted by the escape
character `\'. If the STRING does not contain any regexp meta-characters
it is returned as-is (un-copied).
::end:: */
{
  rep_DECLARE1(str, rep_STRINGP);

  const char *s = rep_STR(str);
  size_t slen = rep_STRING_LEN(str);

  size_t buflen = 128;
  size_t buf_i = 0;
  char *buf = rep_alloc(buflen);
  if (!buf) {
    return 0;
  }
  
  bool quoted = false;

  while (slen-- > 0) {
    /* Ensure string is long enough, this saves doing this twice. */

    if (buf_i + 2 >= buflen) {
      size_t newlen = buflen * 2;
      char *newbuf = rep_alloc(newlen);
      if (!newbuf) {
	rep_free(buf);
	return 0;
      }
      memcpy(newbuf, buf, buf_i);
      rep_free(buf);
      buf = newbuf;
      buflen = newlen;
    }

    int c = *s++;
    switch(c) {
    case '*':
    case '+':
    case '?':
    case '.':
    case '[':
    case ']':
    case '(':
    case ')':
    case '|':
    case '^':
    case '$':
    case '\\':
      buf[buf_i++] = '\\';
      buf[buf_i++] = c;
      quoted = true;
      break;
    default:
      buf[buf_i++] = c;
      break;
    }
  }

  repv ret = !quoted ? str : rep_string_copy_n(buf, buf_i);

  rep_free(buf);

  return ret;
}

DEFUN("regexp-cache-control", Fregexp_cache_control,
      Sregexp_cache_control, (repv limit), rep_Subr1) /*
::doc:rep.regexp#regexp-cache-control::
regexp-cache-control [SOFT-LIMIT]

If SOFT-LIMIT is defined, it specifies the maximum number of bytes that
the regexp cache may occupy after garbage collection.

Returns (SOFT-LIMIT CURRENT-SIZE CURRENT-ENTRIES HITS MISSES).
::end:: */
{
  rep_DECLARE1_OPT(limit, rep_INTP);
  if (rep_INTP(limit) && rep_INT(limit) >= 0) {
    regexp_cache_limit = rep_INT(limit);
  }

  intptr_t current_size = 0, current_items = 0;

  struct cached_regexp *x = cached_regexps;
  while (x != 0) {
    current_items++;
    current_size += sizeof(struct cached_regexp) + x->compiled->regsize;
    x = x->next;
  }

  return rep_list_5(rep_MAKE_INT(regexp_cache_limit),
		    rep_MAKE_INT(current_size), rep_MAKE_INT(current_items),
		    rep_MAKE_INT(regexp_hits), rep_MAKE_INT(regexp_misses));
}	  

void
rep_regerror(char *err)
{
  Fsignal(Qregexp_error, rep_LIST_1(rep_string_copy(err)));
}

void
rep_find_init(void)
{
  repv tem = rep_push_structure("rep.regexp");
  rep_ADD_SUBR(Sstring_match);
  rep_ADD_SUBR(Sstring_looking_at);
  rep_ADD_SUBR(Sexpand_last_match);
  rep_ADD_SUBR(Smatch_start);
  rep_ADD_SUBR(Smatch_end);
  rep_ADD_SUBR(Squote_regexp);
  rep_ADD_SUBR(Sregexp_cache_control);
  rep_pop_structure(tem);
  
  rep_INTERN(regexp_error);
  
  DEFSTRING(err_regexp_error, "Regexp error");
  rep_DEFINE_ERROR(regexp_error);
  
  rep_regsub_fun = rep_default_regsub;
  rep_regsublen_fun = rep_default_regsublen;
}

void
rep_find_kill(void)
{
  release_cached_regexps();
}
