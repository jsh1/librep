/* rep_subrs.h -- mostly LISP subr declarations

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

#ifndef REP_SUBRS_H
#define REP_SUBRS_H

#include <stdarg.h>

/* from characters.c */
extern repv rep_intern_char(uint32_t c);
extern repv Falpha_char_p(repv);
extern repv Fupper_case_p(repv);
extern repv Flower_case_p(repv);
extern repv Fdigit_char_p(repv);
extern repv Falphanumericp(repv);
extern repv Fspace_char_p(repv);
extern repv Fchar_upcase(repv);
extern repv Fchar_downcase(repv);

/* from datums.c */
extern repv Qnil;
extern repv Fmake_datum (repv, repv);
extern repv Fdefine_datum_printer (repv, repv);
extern repv Fdatum_ref (repv, repv);
extern repv Fdatum_set (repv, repv, repv);
extern repv Fhas_type_p (repv, repv);

/* from debug-buffer.c */
extern void *rep_db_alloc(char *name, int size);
extern void rep_db_free(void *db);
extern void rep_db_vprintf(void *_db, char *fmt, va_list args);
extern void rep_db_printf(void *db, char *fmt, ...);
extern void rep_db_print_backtrace(void *_db, char *fun);
extern void *rep_db_return_address(void);
extern void rep_db_spew(void *_db);
extern void rep_db_spew_all(void);
extern void rep_db_kill(void);

/* from errors.c */
extern volatile repv rep_throw_value;
extern volatile int rep_test_int_counter;
extern int rep_test_int_period;
extern void (*rep_test_int_fun)(void);
extern void rep_test_interrupt(void);
extern repv rep_int_cell, rep_term_cell;
extern void rep_handle_error(repv, repv);
extern repv rep_signal_arg_error(repv, int);
extern repv rep_signal_missing_arg(int argnum);
extern repv rep_mem_error(void);
extern repv Qerror, Qerror_message, Qinvalid_function;
extern repv Qvoid_value, Qbad_arg, Qinvalid_read_syntax;
extern repv Qpremature_end_of_stream, Qend_of_stream;
extern repv Qinvalid_lambda_list, Qmissing_arg;
extern repv Qinvalid_macro, Qno_catcher;
extern repv Qfile_error;
extern repv Qinvalid_stream, Qsetting_constant, Qprocess_error;
extern repv Qno_memory, Quser_interrupt, Qarith_error;
extern repv Qterm_interrupt;
extern repv Qstack_error;
extern repv Fsignal(repv error, repv data);

/* from files.c */
extern repv Qdefault_directory;
extern repv Qstart, Qend;
extern repv Qread, Qwrite, Qappend;
extern repv rep_fh_env;
extern int rep_op_write_buffer_contents;
extern int rep_op_read_file_contents;
extern int rep_op_insert_file_contents;
extern repv rep_signal_file_error(repv cdr);
extern repv rep_unbound_file_error(repv file);
extern repv rep_get_file_handler(repv file_name, int op);
extern repv rep_call_file_handler(repv handler, int op,
				   repv sym, int nargs, ...);
extern repv rep_get_handler_from_file_or_name(repv *filep, int op);
extern repv rep_expand_and_get_handler(repv *file_namep, int op);
extern repv rep_localise_and_get_handler(repv *file_namep, int op);
extern bool rep_file_newer_than(repv name1, repv name2);
extern repv Ffile_name_absolute_p(repv file);
extern repv Fexpand_file_name(repv, repv);
extern repv Flocal_file_name(repv);
extern repv Fcanonical_file_name(repv);
extern repv Ffile_name_nondirectory(repv);
extern repv Ffile_name_directory(repv);
extern repv Ffile_name_as_directory(repv);
extern repv Fdirectory_file_name(repv);
extern repv Ffilep(repv arg);
extern repv Ffile_binding(repv file);
extern repv Ffile_bound_stream(repv file);
extern repv Ffile_handler_data(repv);
extern repv Fset_file_handler_data(repv, repv);
extern repv Fopen_file(repv, repv);
extern repv Fmake_file_from_stream(repv, repv, repv);
extern repv Fclose_file(repv);
extern repv Fflush_file(repv file);
extern repv Fseek_file(repv file, repv offset, repv where);
extern repv Fdelete_file(repv);
extern repv Frename_file(repv, repv);
extern repv Fmake_directory(repv);
extern repv Fdelete_directory(repv);
extern repv Fcopy_file(repv, repv);
extern repv Ffile_readable_p(repv file);
extern repv Ffile_writable_p(repv file);
extern repv Ffile_exists_p(repv file);
extern repv Ffile_regular_p(repv file);
extern repv Ffile_directory_p(repv file);
extern repv Ffile_symlink_p(repv file);
extern repv Ffile_owner_p(repv file);
extern repv Ffile_nlinks(repv file);
extern repv Ffile_size(repv file);
extern repv Ffile_modes(repv file);
extern repv Fset_file_modes(repv file, repv modes);
extern repv Ffile_modes_as_string(repv file);
extern repv Ffile_modtime(repv file);
extern repv Fdirectory_files(repv dir);
extern repv Fread_symlink(repv file);
extern repv Fmake_symlink(repv file, repv contents);
extern repv Fstdin_file(void);
extern repv Fstdout_file(void);
extern repv Fstderr_file(void);
extern repv Fmake_temp_name(void);
extern repv rep_file_fdopen (int fd, char *mode);

/* from find.c */
extern rep_regexp *rep_compile_regexp(repv re);
extern void rep_push_regexp_data(struct rep_saved_regexp_data *sd);
extern void rep_pop_regexp_data(void);
extern void rep_update_last_match(repv data, rep_regexp *prog);
extern void rep_set_string_match(repv obj, repv start, repv end);
extern void (*rep_regsub_fun)(int, rep_regsubs *, const char *, char *, void *);
extern size_t (*rep_regsublen_fun)(int, rep_regsubs *, const char *, void *);
extern repv Qregexp_error;
extern repv Fstring_match(repv re, repv str, repv start, repv nocasep);
extern repv Fstring_looking_at(repv re, repv string,
				repv start, repv nocasep);
extern repv Fexpand_last_match(repv template_);
extern repv Fmatch_start(repv exp);
extern repv Fmatch_end(repv exp);
extern repv Fquote_regexp(repv str);
extern repv Fregexp_cache_control(repv limit);
extern void rep_regerror(char *err);

/* from fluids.c */
extern repv Fmake_fluid (repv);
extern repv Ffluid_ref (repv);
extern repv Ffluid_set (repv, repv);
extern repv Fwith_fluids (repv, repv, repv);

/* from autoload.c */
extern repv Qautoload, Qinvalid_autoload;
extern repv rep_load_autoload(repv);

/* from apply.c */
extern repv rep_apply(repv fun, repv args);
extern repv rep_call_lisp0(repv);
extern repv rep_call_lisp1(repv, repv);
extern repv rep_call_lisp2(repv, repv, repv);
extern repv rep_call_lisp3(repv, repv, repv, repv);
extern repv rep_call_lisp4(repv, repv, repv, repv, repv);
extern repv rep_call_lispn (repv fun, int argc, repv *argv);
extern repv Fbacktrace(repv strm);

/* from eval.c */
extern repv Fbreak(void);
extern repv Fstep(repv);

/* from lisp.c */
extern repv Qlambda, Qmacro;
extern repv rep_handle_var_int(repv, int *);
extern repv rep_handle_var_long_int(repv, long *);
extern repv Qstandard_input, Qstandard_output;
extern repv Qprint_escape, Qprint_length, Qprint_level, Qnewlines;
extern repv rep_env, rep_special_env;
extern int rep_lisp_depth, rep_max_lisp_depth;
extern repv Feval(repv);
extern repv Vmax_lisp_depth(repv val);
extern int rep_list_length(repv);
extern repv rep_concat_lists(repv args);
extern bool rep_assign_args (repv list, int required, int total, ...);

/* from lispcmds.c */
extern repv Qor, Qand;
extern repv Qload_path, Qafter_load_alist, Qlisp_lib_directory;
extern repv Qdl_load_path, Qdl_load_reloc_now;
extern repv Qsite_lisp_directory, Qdocumentation_file, Qdocumentation_files;
extern repv Fcar(repv);
extern repv Fcdr(repv);
extern repv Fmake_list(repv, repv);
extern repv Fset_car(repv, repv);
extern repv Fset_cdr(repv, repv);
extern repv Freverse(repv);
extern repv Fnreverse(repv);
extern repv Fassoc(repv, repv);
extern repv Fassq(repv, repv);
extern repv Frassoc(repv, repv);
extern repv Frassq(repv, repv);
extern repv Flist_length(repv);
extern repv Flist_ref(repv, repv);
extern repv Flist_tail(repv list, repv index);
extern repv Flast(repv);
extern repv Fmapcar(repv, repv);
extern repv Fmapc(repv, repv);
extern repv Ffilter(repv pred, repv list);
extern repv Fmember(repv, repv);
extern repv Fmemq(repv, repv);
extern repv Fmemql(repv, repv);
extern repv Fdelete(repv, repv);
extern repv Fdelq(repv, repv);
extern repv Fdelete_if(repv, repv);
extern repv Fdelete_if_not(repv, repv);
extern repv Fmake_vector(repv, repv);
extern repv Farrayp(repv);
extern repv Farray_length(repv);
extern repv Faset(repv, repv, repv);
extern repv Faref(repv, repv);
extern repv Fmake_string(repv, repv);
extern repv Fsubstring(repv string, repv start, repv end);
extern repv Flength(repv);
extern repv Fcopy_sequence(repv);
extern repv Felt(repv, repv);
extern repv Ffuncall(repv);
extern repv Fapply(repv);
extern repv Fcall_with_object(repv arg, repv thunk);
extern repv Fload(repv file, repv noerr_p, repv nopath_p,
		  repv nosuf_p, repv in_env);
extern repv Fequal(repv, repv);
extern repv Feq(repv, repv);
extern repv Fstring_head_eq(repv, repv);
extern repv Fnull(repv);
extern repv Fatom(repv);
extern repv Fconsp(repv);
extern repv Flistp(repv);
extern repv Fbytecodep(repv);
extern repv Ffunctionp(repv);
extern repv Fmacrop(repv);
extern repv Fspecial_form_p(repv);
extern repv Fsubrp(repv arg);
extern repv Fsequencep(repv arg);
extern repv FSdocumentation(repv subr, repv useVar);
extern repv FSname(repv subr, repv useVar);
extern repv Fcall_hook(repv hook, repv arg_list, repv type);

/* from lispmach.c */
extern repv Qbytecode_error;
extern repv Fvalidate_byte_code(repv bc_major, repv bc_minor);
extern repv Fmake_byte_code_subr(repv args);

/* from macros.c */
extern repv Fmacroexpand(repv, repv);

/* from main.c */
extern void rep_init(char *prog_name, int *argc, char ***argv);
extern repv rep_load_environment (repv file);
extern void rep_kill(void);
extern bool rep_get_option (const char *option, repv *argp);
extern bool rep_on_idle(int since_last_event);
extern bool rep_handle_input_exception(repv *result_p);
extern int rep_top_level_exit (void);
extern void *rep_common_db;
extern int rep_recurse_depth;
extern bool (*rep_on_idle_fun)(int since_last);
extern repv (*rep_event_loop_fun)(void);
extern repv Qidle_hook;
extern void (*rep_on_termination_fun)(void);
extern repv Qexit, Qquit, Qtop_level, Qcommand_line_args;
extern repv Qbatch_mode, Qinterpreted_mode, Qprogram_name;
extern repv Qerror_mode, Qinterrupt_mode;
extern repv Frecursive_edit(void);
extern repv rep_top_level_recursive_edit (void);
extern repv Frecursion_depth(void);
extern repv Fget_command_line_option (repv, repv);

/* from message.c */
enum rep_message {
  rep_messagen,				/* f(char *str, size_t len) */
  rep_message,				/* f(char *str) */
  rep_messagef,				/* f(char *format_str, ...) */
  rep_append_message,			/* f(char *str, size_t len) */
  rep_reset_message,			/* f() */
  rep_redisplay_message			/* f() */
};
extern void (*rep_message_fun)(enum rep_message fn, ...);

/* from misc.c */
extern char *rep_str_dupn(const char *old, int len);
extern void (*rep_beep_fun)(void);
extern repv Qprocess_environment;
extern repv Qbuild_id_string;
extern repv Qupcase_table, Qdowncase_table, Qflatten_table;
extern repv Fbeep(void);
extern repv Fcomplete_string(repv existing, repv arg_list, repv fold);
extern repv Fcurrent_time(void);
extern repv Fcurrent_time_string(repv time, repv format);
extern repv Fsleep_for(repv secs, repv msecs);
extern repv Fsit_for(repv secs, repv msecs);
extern repv Fuser_login_name(void);
extern repv Fuser_full_name(repv arg);
extern repv Fuser_home_directory(repv user);
extern repv Fsystem_name(void);
extern repv Fmessage(repv string, repv now);
extern repv Frandom(repv arg);
extern repv Ftranslate_string(repv string, repv table);

/* from numbers.c */
extern bool rep_long_int_p (repv value);
extern repv rep_make_long_uint (uintptr_t in);
extern repv rep_make_long_int (intptr_t in);
extern uintptr_t rep_get_long_uint (repv value);
extern intptr_t rep_get_long_int (repv value);
extern repv rep_make_longlong_int (long long in);
extern long long rep_get_longlong_int (repv in);
extern repv rep_make_float (double in, bool force);
extern double rep_get_float (repv in);
extern int rep_compare_numbers (repv n1, repv n2);
extern char *rep_print_number_to_string (repv obj, int radix, int prec);
extern repv rep_number_add (repv x, repv y);
extern repv rep_number_neg (repv x);
extern repv rep_number_sub (repv x, repv y);
extern repv rep_number_mul (repv x, repv y);
extern repv rep_number_div (repv x, repv y);
extern repv rep_number_lognot (repv x);
extern repv rep_number_logior (repv x, repv y);
extern repv rep_number_logxor (repv x, repv y);
extern repv rep_number_logand (repv x, repv y);
extern repv rep_number_max (repv x, repv y);
extern repv rep_number_min (repv x, repv y);
extern repv rep_integer_gcd (repv x, repv y);
extern repv Feql(repv arg1, repv arg2);
extern repv Fremainder(repv n1, repv n2);
extern repv Fmod(repv n1, repv n2);
extern repv Fquotient(repv n1, repv n2);
extern repv Flognot(repv);
extern repv Fnot(repv);
extern repv Fplus1(repv);
extern repv Fsub1(repv);
extern repv Fash(repv, repv);
extern repv Ffloor (repv);
extern repv Fceiling (repv);
extern repv Ftruncate (repv);
extern repv Fround (repv);
extern repv Fexp (repv);
extern repv Flog (repv);
extern repv Fsin (repv);
extern repv Fcos (repv);
extern repv Ftan (repv);
extern repv Fasin (repv);
extern repv Facos (repv);
extern repv Fatan (repv, repv);
extern repv Fsqrt (repv);
extern repv Fexpt (repv, repv);
extern repv Fzerop(repv);
extern repv Fnumberp(repv);
extern repv Fintegerp(repv);
extern repv Frationalp(repv);
extern repv Frealp(repv);
extern repv Fexactp(repv);
extern repv Finexactp(repv);
extern repv Fexact_to_inexact(repv);
extern repv Finexact_to_exact(repv);
extern repv Fnumerator(repv);
extern repv Fdenominator(repv);

/* from read.c */
extern repv Qquote;

/* from streams.c */
extern repv Qformat_hooks_alist;
extern int rep_stream_getc(repv);
extern void rep_stream_ungetc(repv, int);
extern int rep_stream_putc(repv, int);
extern intptr_t rep_stream_puts(repv, const void *, intptr_t, bool);
extern repv Fwrite(repv stream, repv data, repv len);
extern repv Fread_char(repv stream);
extern repv Fpeek_char(repv stream);
extern repv Fread_chars(repv stream, repv count);
extern repv Fread_line(repv stream);
extern repv Fcopy_stream(repv source, repv dest);
extern repv Fread(repv);
extern repv Fprint(repv, repv);
extern repv Fprin1(repv, repv);
extern repv Fprinc(repv, repv);
extern repv Fformat(repv);
extern repv Fmake_string_input_stream(repv string, repv start);
extern repv Fmake_string_output_stream(void);
extern repv Fget_output_stream_string(repv strm);
extern repv Finput_stream_p(repv arg);
extern repv Foutput_stream_p(repv arg);
extern intptr_t rep_stream_put_utf32(repv stream, uint32_t c);
extern int32_t rep_stream_get_utf32(repv stream);

/* from symbols.c */
extern repv rep_undefined_value;
extern repv (*rep_deref_local_symbol_fun)(repv sym);
extern repv (*rep_set_local_symbol_fun)(repv sym, repv val);
extern void rep_intern_static(repv *, repv);
extern repv rep_call_with_closure (repv closure,
				   repv (*fun)(repv arg), repv arg);
extern repv rep_bind_symbol(repv, repv, repv);
extern int rep_unbind_symbols(repv);
extern repv rep_obarray;
extern repv Qt;
extern repv Qvariable_documentation, Qpermanent_local;
extern bool rep_warn_shadowing;
extern repv Fmake_symbol(repv);
extern repv Fmake_obarray(repv);
extern repv Ffind_symbol(repv, repv);
extern repv Fintern_symbol(repv, repv);
extern repv Fintern(repv, repv);
extern repv Funintern(repv, repv);
extern repv Fmake_closure (repv function, repv name);
extern repv Fclosure_function (repv closure);
extern repv Fset_closure_function (repv closure, repv fun);
extern repv Fclosurep (repv arg);
extern repv Fsymbol_value(repv, repv);
extern repv Fset(repv, repv);
extern repv Fsetplist(repv, repv);
extern repv Fsymbol_name(repv);
extern repv Fdefault_value(repv, repv);
extern repv Fdefault_boundp(repv);
extern repv Fset_default(repv, repv);
extern repv Fboundp(repv);
extern repv Fsymbol_plist(repv);
extern repv Fgensym(void);
extern repv Fsymbolp(repv);
extern repv Fmakunbound(repv);
extern repv Fget(repv, repv);
extern repv Fput(repv, repv, repv);
extern repv Fapropos(repv, repv, repv);
extern repv Fmake_variable_special (repv sym);
extern repv Fspecial_variable_p(repv sym);
extern repv Ftrace(repv sym);
extern repv Funtrace(repv sym);
extern repv Vobarray(repv val);
extern repv Fmake_keyword (repv in);
extern repv Fkeywordp (repv arg);

/* from structures.c */
extern repv rep_structure;
extern repv Fmake_binding_immutable (repv);
extern repv Fbinding_immutable_p (repv, repv);
extern repv Fexport_bindings (repv list);
extern repv Ffeaturep(repv);
extern repv Fprovide(repv);
extern repv Frequire(repv);
extern repv rep_push_structure_name (repv name);
extern repv rep_push_structure (const char *name);
extern repv rep_pop_structure (repv old);
extern void rep_lazy_structure(const char *name_str, void (*init)(void));
extern repv rep_bootstrap_structure (const char *s);
extern repv rep_add_subr(rep_xsubr *, bool);
extern void rep_structure_exports_all (repv s, bool status);
extern void rep_structure_set_binds (repv s, bool status);

/* from tuples.c */
extern repv rep_make_tuple (repv car, repv a, repv b);
extern void rep_mark_tuple (repv t);

/* from values.c */
extern repv Qafter_gc_hook;
extern int rep_guardian_type;
extern repv rep_define_type(rep_type *t);
extern const rep_type *rep_get_type(repv type);
extern const rep_type *rep_value_type(repv value);
extern int rep_value_cmp(repv, repv);
extern void rep_princ_val(repv, repv);
extern void rep_print_val(repv, repv);
extern repv rep_null_string(void);
extern repv rep_box_string (char *ptr, size_t len);
extern repv rep_allocate_string(size_t);
extern repv rep_string_copy_n(const char *, size_t);
extern repv rep_string_copy(const char *);
extern repv rep_string_concat2(const char *, const char *);
extern repv rep_string_concat3(const char *, const char *, const char *);
extern repv rep_string_concat4(const char *s1, const char *s2, const char *s3, const char *s4);
extern bool rep_string_set_len(repv, size_t);
extern repv rep_list_1(repv);
extern repv rep_list_2(repv, repv);
extern repv rep_list_3(repv, repv, repv);
extern repv rep_list_4(repv, repv, repv, repv);
extern repv rep_list_5(repv, repv, repv, repv, repv);
extern repv rep_make_vector(int);
extern repv Fmake_primitive_guardian (void);
extern repv Fprimitive_guardian_push (repv g, repv obj);
extern repv Fprimitive_guardian_pop (repv g);
extern void rep_mark_static(repv *);
extern void rep_mark_value(repv);
extern repv Fcons(repv, repv);
extern rep_GC_root *rep_gc_root_stack;
extern rep_GC_n_roots *rep_gc_n_roots_stack;
extern repv Vgarbage_threshold(repv val);
extern repv Vidle_garbage_threshold(repv val);
extern repv Fgarbage_collect(repv noStats);
extern int rep_data_after_gc, rep_gc_threshold, rep_idle_gc_threshold;

/* from vectors.c */
extern repv Fvectorp(repv);
extern repv Fvector_length(repv);
extern repv Fvector_ref(repv, repv);
extern repv Fvector_set(repv, repv, repv);
extern repv Fmake_vector_immutable(repv);

/* from strings.c */
extern intptr_t rep_string_ptr_size(repv s);
extern const char *rep_string_ptr(repv s);
extern char *rep_string_mutable_ptr(repv s);
extern void rep_string_set_ascii(repv s);
extern intptr_t rep_stream_put_utf8(repv stream, repv str, intptr_t count);
extern repv Fstringp(repv);
extern repv Fstring_length(repv);
extern repv Fstring_ref(repv, repv);
extern repv Fstring_set(repv, repv, repv);
extern repv Fmake_string_immutable(repv);
extern repv Fstring_to_immutable_string(repv);

/* from unix_dl.c */
extern bool rep_find_c_symbol(void *, char **, void **);
extern void *rep_find_dl_symbol (repv feature, char *symbol);

/* from unix_files.c */
extern repv rep_lookup_errno(void);
extern size_t rep_file_length(repv file);

/* from unix_main.c */
extern uintptr_t rep_time(void);
extern void (*rep_register_input_fd_fun)(int fd, void (*callback)(int fd));
extern void (*rep_deregister_input_fd_fun)(int fd);
extern void rep_add_event_loop_callback (bool (*callback)(void));
extern void rep_sleep_for(int secs, int msecs);
extern void rep_register_input_fd(int fd, void (*callback)(int fd));
extern void rep_deregister_input_fd(int fd);
extern void rep_map_inputs (void (*fun)(int fd, void (*callback)(int)));
extern void rep_mark_input_pending(int fd);
extern void rep_set_fd_nonblocking(int fd);
extern void rep_set_fd_blocking(int fd);
extern void rep_set_fd_cloexec(int fd);
extern void rep_sig_restart(int sig, bool flag);
extern repv rep_event_loop(void);
extern repv rep_sit_for(int timeout_msecs);
extern repv rep_accept_input_for_callbacks (int timeout_msecs,
					    int ncallbacks,
					    void (**callbacks)(int));
extern repv rep_accept_input_for_fds (int timeout_msecs,
				      int nfds, int *fds);
extern repv rep_accept_input(int timeout_msecs, void (*callback)(int));
extern bool rep_poll_input(int fd);

#ifdef DEBUG_SYS_ALLOC
extern void *rep_alloc(size_t length);
extern void *rep_realloc(void *ptr, size_t length);
extern void rep_free(void *ptr);
extern void rep_print_allocations(void);
#else
# include <stdlib.h>
# define rep_alloc(n) malloc(n)
# define rep_realloc(p,n) realloc(p,n)
# define rep_free(p) free(p)
#endif

extern void (*rep_redisplay_fun)(void);
extern int (*rep_wait_for_input_fun)(fd_set *inputs, int timeout_msecs);
extern int rep_input_timeout_secs;
extern repv Funix_print_allocations(void);

/* from unix_processes.c */
extern repv Qpipe, Qpty;
extern void (*rep_sigchld_fun) (void);
extern bool rep_proc_periodically(void);
extern repv Fmake_process(repv stream, repv fun, repv dir,
			   repv prog, repv args);
extern repv Fstart_process(repv arg_list);
extern repv Fcall_process(repv arg_list);
extern repv Finterrupt_process(repv proc, repv grp);
extern repv Fkill_process(repv proc, repv grp);
extern repv Fstop_process(repv proc, repv grp);
extern repv Fcontinue_process(repv proc, repv grp);
extern repv Fprocess_exit_status(repv proc);
extern repv Fprocess_exit_value(repv proc);
extern repv Fprocess_id(repv proc);
extern repv Fprocess_running_p(repv proc);
extern repv Fprocess_stopped_p(repv proc);
extern repv Fprocess_in_use_p(repv proc);
extern repv Fprocessp(repv arg);
extern repv Fprocess_prog(repv proc);
extern repv Fset_process_prog(repv proc, repv prog);
extern repv Fprocess_args(repv proc);
extern repv Fset_process_args(repv proc, repv args);
extern repv Fprocess_output_stream(repv proc);
extern repv Fset_process_output_stream(repv proc, repv stream);
extern repv Fprocess_error_stream(repv proc);
extern repv Fset_process_error_stream(repv proc, repv stream);
extern repv Fprocess_function(repv proc);
extern repv Fset_process_function(repv proc, repv fn);
extern repv Fprocess_dir(repv proc);
extern repv Fset_process_dir(repv proc, repv dir);
extern repv Fprocess_connection_type(repv proc);
extern repv Fset_process_connection_type(repv proc, repv type);
extern repv Factive_processes(void);
extern repv Faccept_process_output(repv secs, repv msecs);
void rep_register_process_input_handler (void (*handler)(int));

/* in plugins */
extern repv rep_dl_init (void);
extern void rep_dl_kill (void);

#endif /* REP_SUBRS_H */
