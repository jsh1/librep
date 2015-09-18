/* readline.c -- wrap some readline functions when available

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

#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_LIBREADLINE
# include <readline/readline.h>
# include <readline/history.h>
#endif

DEFSYM(rl_completion_generator, "rl-completion-generator");
DEFSYM(boundp, "variable-bound?");

static repv completion_fun;
static repv completions;

#ifdef HAVE_LIBREADLINE

static char *
completion_generator(char *word, int state)
{
  if (state == 0) {
    repv fun = completion_fun;
    if (fun == rep_nil) {
      /* backwards compatibility, ugh */
      fun = Fsymbol_value(Qrl_completion_generator, Qt);
    }
    if (Ffunctionp(fun) != rep_nil) {
      completions = Ffuncall(rep_list_2(fun, rep_string_copy (word)));
    } else {
      repv re = Fquote_regexp(rep_string_copy(word));
      repv boundp = Fsymbol_value(Qboundp, Qt);
      completions = Fapropos(rep_string_concat2("^", rep_STR(re)), boundp, rep_nil);
    }
    if (!completions) {
      completions = rep_nil;
    }
  }

  if (completions != rep_nil && rep_CONSP(completions)
      && (rep_SYMBOLP(rep_CAR(completions))
	  || rep_STRINGP(rep_CAR(completions))))
  {
    repv string = rep_CAR(completions);
    if (rep_SYMBOLP(string)) {
      string = rep_SYM(string)->name;
    }
    completions = rep_CDR(completions);
    return strdup (rep_STR(string));
  } else {
    return 0;
  }
}

/* gratuitously stolen from guile, guile-readline/readline.c */

static int match_paren(int x, int k);
static int find_matching_paren(int k);
static void init_bouncing_parens();

static void
init_bouncing_parens()
{
  if (strncmp(rl_get_keymap_name(rl_get_keymap()), "vi", 2)) {
    rl_bind_key(')', match_paren);
    rl_bind_key(']', match_paren);
    rl_bind_key('}', match_paren);
  }
}

static int
find_matching_paren(int k)
{
  int end_parens_found = 0;

  /* Choose the corresponding opening bracket.  */

  char c = 0;
  if (k == ')') {
    c = '(';
  } else if (k == ']') {
    c = '[';
  } else if (k == '}') {
    c = '{';
  }

  for (int i = rl_point - 2; i >= 0; i--) {
    /* Is the current character part of a character literal?  */

    if (i - 2 >= 0 && rl_line_buffer[i - 1] == '\\'
	&& rl_line_buffer[i - 2] == '#') {
      // nothing
    } else if (rl_line_buffer[i] == k) {
      end_parens_found++;
    } else if (rl_line_buffer[i] == '"') {
      /* Skip over a string literal.  */
      for (i--; i >= 0; i--) {
	if (rl_line_buffer[i] == '"'
	    && !(i - 1 >= 0 && rl_line_buffer[i - 1] == '\\')) {
	  break;
	}
      }
    } else if (rl_line_buffer[i] == c) {
      if (end_parens_found==0) {
	return i; 
      } else {
	--end_parens_found;
      }
    }
  }

  return -1;
}

static int
match_paren(int x, int k)
{
  rl_insert(x, k);

  /* Did we just insert a quoted paren?  If so, then don't bounce.  */

  if (rl_point - 1 >= 1 && rl_line_buffer[rl_point - 2] == '\\') {
    return 0;
  }

  /* tmp = 200000 */
  struct timeval timeout;
  timeout.tv_sec = 0 /* tmp / 1000000 */ ; 
  timeout.tv_usec = 200000 /* tmp % 1000000 */ ;
  fd_set readset;
  FD_ZERO(&readset);
  FD_SET(fileno(rl_instream), &readset);
  
  if(rl_point > 1) {
    int tmp = rl_point;
    rl_point = find_matching_paren(k);
    if(rl_point > -1) {
      rl_redisplay();
      select(1, &readset, NULL, NULL, &timeout);
    }
    rl_point = tmp;
  }

  return 0;
}

#endif /* HAVE_LIBREADLINE */

DEFUN("readline", Freadline, Sreadline,
      (repv prompt_, repv completer), rep_Subr2)
{
  char *prompt = rep_STRINGP(prompt_) ? ((char *) rep_STR(prompt_)) : "> ";

#ifdef HAVE_LIBREADLINE
  repv saved_completion = completion_fun;
  completion_fun = completer;

  rep_GC_root gc_saved_completion;
  rep_PUSHGC(gc_saved_completion, saved_completion);

  char *input = readline(prompt);

  rep_POPGC;

  completion_fun = saved_completion;

  repv ret = rep_nil;

  if (input) {
    int len = strlen(input);
    if (len > 0) {
      add_history(input);
    }
    ret = rep_allocate_string(len + 2);
    memcpy(rep_MUTABLE_STR(ret), input, len);
    rep_MUTABLE_STR(ret)[len] = '\n';
    rep_MUTABLE_STR(ret)[len+1] = 0;
    free(input);
  }

  completions = rep_nil;
  return ret;

#else

  if (isatty(0)) {
    fputs(prompt, stderr);
    fflush(stderr);
  }

  return Fread_line(Fstdin_file());

#endif
}

repv
rep_dl_init(void)
{
  rep_INTERN(rl_completion_generator);
  rep_INTERN(boundp);

  completions = rep_nil;
  rep_mark_static (&completions);

  completion_fun = rep_nil;
  rep_mark_static (&completion_fun);

#ifdef HAVE_LIBREADLINE
  rl_completion_entry_function = (void *) completion_generator;
  rl_basic_quote_characters = "\"";
  init_bouncing_parens();
#endif

  repv tem = rep_push_structure ("rep.io.readline");
  rep_ADD_SUBR(Sreadline);
  return rep_pop_structure (tem);
}
