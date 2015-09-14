/* repint_subrs.h -- library-local prototypes

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

#ifndef REPINT_SUBRS_H
#define REPINT_SUBRS_H

/* from autoload.c */
extern void rep_autoload_init(void);

/* from apply.c */
extern rep_xsubr Slambda;
extern rep_stack_frame *rep_call_stack;
extern repv rep_apply_(repv fun, repv args, bool tail_posn);
extern repv rep_apply(repv fun, repv args);
extern repv rep_current_function(void);
extern void rep_apply_init(void);

/* from arrays.c */
extern void rep_arrays_init(void);

/* from call-hook.c */
extern void rep_call_hook_init(void);

/* from characters.c */
extern void rep_characters_init(void);

/* from closures.c */
extern void rep_closures_init(void);
extern void rep_closures_kill(void);

/* from datums.c */
extern void rep_pre_datums_init (void);
extern void rep_datums_init (void);

/* from environ.c */
extern void rep_environ_init(void);

/* from errors.c */
extern void rep_errors_init(void);

/* from eval.c */
extern void rep_eval_init(void);

/* from files.c */
extern void rep_files_init(void);
extern void rep_files_kill(void);

/* from find.c */
extern struct rep_saved_regexp_data *rep_saved_matches;
extern void rep_invalidate_string(repv string);
extern void rep_mark_regexp_data(void);
extern void rep_find_init(void);
extern void rep_find_kill(void);

/* from fluids.c */
extern void rep_fluids_init(void);

/* from guardians.c */
extern void rep_run_guardians(void);
extern void rep_guardians_init(void);

/* from lambda.c */
extern repv rep_apply_lambda(repv lambda_exp, repv arg_list, bool tail_posn);
extern repv rep_tail_call_throw(repv lst);
extern void rep_lambda_init(void);

/* from lisp.c */
extern repv rep_scm_t, rep_scm_f;
extern repv rep_readl(repv, int *);
extern repv rep_eval (repv form, bool tail_posn);
extern repv Fprogn(repv args, bool tail_posn);
extern void rep_lisp_prin(repv, repv);
extern void rep_string_princ(repv, repv);
extern void rep_string_print(repv, repv);
extern repv rep_copy_list(repv);
extern bool rep_compare_error(repv error, repv handler);
extern bool rep_single_step_flag;
extern repv ex_optional, ex_rest, ex_key;
extern int rep_current_frame_index(void);

extern repv Qload_filename;
extern repv Fcall_with_exception_handler (repv, repv);
extern repv Fcond(repv, bool);
extern repv Flist_star (int argc, repv *argv);
extern repv Fnconc (int argc, repv *argv);
extern repv Fappend (int argc, repv *argv);
extern repv Fvector (int argc, repv *argv);
extern repv Fconcat (int, repv *);

/* from compare.c */
extern repv Fnum_eq (int, repv *);
extern repv Fnum_noteq (int, repv *);
extern repv Fgtthan (int, repv *);
extern repv Fgethan (int, repv *);
extern repv Fltthan (int, repv *);
extern repv Flethan (int, repv *);
extern void rep_compare_init(void);

/* from gc.c */
extern void rep_gc_init(void);

/* from lispmach.c */
extern repv Qbytecode_error;
extern repv Frun_byte_code(repv code, repv consts, repv stkreq);
extern repv rep_apply_bytecode (repv subr, int nargs, repv *args);
extern void rep_lispmach_init(void);

/* from lists.c */
extern int rep_allocated_cons, rep_used_cons;
extern void rep_cons_free(repv);
extern int rep_cons_cmp(repv v1, repv v2);
extern void rep_cons_sweep(void);
extern void rep_lists_init(void);
extern void rep_lists_kill(void);

/* from load.c */
void rep_load_init(void);

/* from main.c */
extern void rep_deprecated (bool *seen, const char *desc);

/* from macros.c */
extern void rep_macros_before_gc (void);
extern void rep_macros_clear_history (void);
extern void rep_macros_init (void);

/* from misc.c */
#ifndef HAVE_STPCPY
extern char *stpcpy(char *, const char *);
#endif
#ifndef HAVE_STRNCASECMP
extern int strncasecmp (const char *s1, const char *s2, size_t n);
#endif
extern void rep_misc_init(void);

/* from numbers.c */
extern repv rep_parse_number (const char *buf, size_t len, int radix,
			      int sign, unsigned int type);
extern repv Fplus(int, repv *);
extern repv Fminus(int, repv *);
extern repv Fproduct(int, repv *);
extern repv Fdivide(int, repv *);
extern repv Flogior(int, repv *);
extern repv Flogxor(int, repv *);
extern repv Flogand(int, repv *);
extern repv Fmax(int, repv *);
extern repv Fmin(int, repv *);
extern repv Fgcd (int, repv *);
extern void rep_numbers_init (void);
extern void rep_numbers_kill(void);

/* from origin.c */
extern bool rep_record_origins;
extern void rep_record_origin (repv form, repv stream, int start_line);
extern repv Flexical_origin (repv form);
extern void rep_mark_origins (void);
extern void rep_origin_init (void);

/* from plists.c */
extern void rep_plists_init(void);

/* from print.c */
extern void rep_print_init(void);

/* from read.c */
extern void rep_read_init(void);

/* from regsub.c */
extern void rep_default_regsub(int, rep_regsubs *, const char *, char *, void *);
extern size_t rep_default_regsublen(int, rep_regsubs *, const char *, void *);

/* from sequences.c */
extern void rep_sequences_init(void);

/* from sockets.c */
extern void rep_sockets_init(void);
extern void rep_sockets_kill(void);

/* from streams.c */
extern void rep_streams_init(void);

/* from strings.c */
extern int rep_allocated_strings, rep_used_strings;
extern size_t rep_allocated_string_bytes;
extern int rep_string_cmp(repv v1, repv v2);
extern void rep_string_sweep(void);
extern void rep_strings_init(void);
extern void rep_strings_kill(void);

/* from structures.c */
extern repv rep_default_structure, rep_specials_structure;
extern repv Q_features, Q_structures, Q_meta, Qrep, Q_specials,
    Q_user_structure, Qrep_structures, Qrep_lang_interpreter,
    Qrep_vm_interpreter, Qexternal, Qinternal;
extern rep_struct_node *rep_search_imports (rep_struct *s, repv var);
extern repv Fmake_structure (repv, repv, repv, repv);
extern repv F_structure_ref (repv, repv);
extern repv Fstructure_set (repv, repv, repv);
extern repv Fstructure_define (repv, repv, repv);
extern repv Fstructure_bound_p (repv, repv);
extern repv Fexternal_structure_ref (repv, repv);
extern repv Fintern_structure (repv);
extern repv Fget_structure (repv);
extern repv Fname_structure (repv structure, repv name);
extern repv Fexport_binding (repv var);
extern repv rep_get_initial_special_value (repv sym);
extern repv rep_documentation_property (repv structure);
extern void rep_pre_structures_init (void);
extern void rep_structures_init (void);

/* from symbols.c */
extern repv rep_keyword_obarray;
extern int rep_allocated_closures, rep_used_closures;
extern void rep_obarray_init(void);
extern void rep_symbols_init(void);

/* from tables.c */
extern repv Fstring_hash(repv arg);
extern repv Fsymbol_hash(repv arg);
extern repv Feq_hash(repv arg);
extern repv Fequal_hash(repv arg);
extern repv Fmake_table(repv hash_fun, repv cmp_fun);
extern repv Fmake_weak_table(repv hash_fun, repv cmp_fun);
extern repv Ftablep(repv arg);
extern repv Ftable_ref(repv tab, repv key);
extern repv Ftable_boundp(repv tab, repv key);
extern repv Ftable_set(repv tab, repv key, repv value);
extern repv Ftable_walk(repv fun, repv tab);
extern repv Ftable_size(repv tab);
extern repv Ftable_unset(repv tab, repv key);
extern void rep_tables_init(void);

/* from tuples.c */
extern int rep_allocated_tuples, rep_used_tuples;
extern void rep_sweep_tuples (void);
extern void rep_tuples_kill(void);

/* from time.c */
extern void rep_time_init(void);

/* from types.c */
extern int rep_type_cmp(repv, repv);
extern void rep_mark_types(void);
extern void rep_sweep_types(void);
extern void rep_types_after_gc(void);
extern void rep_types_init (void);

/* from variables.c */
extern repv rep_symbol_value(repv sym, bool no_error_if_void,
  bool allow_lexical);
extern repv Freal_set (repv var, repv value);
extern repv rep_bind_special (repv oldList, repv symbol, repv newVal);
extern bool rep_special_variable_accessible_p(repv sym);
extern repv rep_search_special_environment(repv sym);
extern void rep_variables_init(void);

/* from vectors.c */
extern int rep_used_vector_slots;
extern int rep_vector_cmp(repv v1, repv v2);
extern void rep_vector_sweep(void);
extern void rep_vectors_init(void);
extern void rep_vectors_kill(void);

/* from weak-refs.c */
extern repv Fmake_weak_ref (repv value);
extern repv Fweak_ref (repv ref);
extern repv Fweak_ref_set (repv ref, repv value);
extern void rep_scan_weak_refs (void);
extern void rep_weak_refs_init (void);

/* from unix_dl.c */
extern repv rep_dl_open_structure(repv file_name);
extern int rep_dl_intern_library (repv file_name);
extern void rep_dl_mark_data(void);
extern void *rep_dl_lookup_symbol (int idx, const char *name);
extern void rep_dl_kill_libraries(void);

/* from unix_files.c */
extern repv rep_file_name_absolute_p(repv file);
extern repv rep_expand_file_name(repv file);
extern repv rep_canonical_file_name(repv file);
extern repv rep_file_name_nondirectory(repv file);
extern repv rep_file_name_directory(repv file);
extern repv rep_file_name_as_directory(repv file);
extern repv rep_directory_file_name(repv file);
extern repv rep_delete_file(repv file);
extern repv rep_rename_file(repv old, repv new_);
extern repv rep_make_directory(repv dir);
extern repv rep_delete_directory(repv dir);
extern repv rep_copy_file(repv src, repv dst);
extern repv rep_file_readable_p(repv file);
extern repv rep_file_writable_p(repv file);
extern repv rep_file_exists_p(repv file);
extern repv rep_file_regular_p(repv file);
extern repv rep_file_directory_p(repv file);
extern repv rep_file_symlink_p(repv file);
extern repv rep_file_owner_p(repv file);
extern repv rep_file_nlinks(repv file);
extern repv rep_file_size(repv file);
extern repv rep_file_modes(repv file);
extern repv rep_set_file_modes(repv file, repv modes);
extern repv rep_file_modes_as_string(repv file);
extern repv rep_file_modtime(repv file);
extern repv rep_directory_files(repv dir_name);
extern repv rep_read_symlink (repv file);
extern repv rep_make_symlink (repv file, repv contents);
extern repv rep_getpwd(void);

/* from unix_main.c */
extern void rep_signals_init(void);
extern void rep_input_init(void);

/* from unix_processes.c */
extern repv rep_system(const char *command);
extern void rep_processes_init(void);
extern void rep_processes_kill(void);

#ifndef HAVE_REALPATH
/* from realpath.c */
extern char *realpath (const char *name, char *resolved);
#endif

#endif /* REPINT_SUBRS_H */
