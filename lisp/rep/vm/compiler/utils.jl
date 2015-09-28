#| utils.jl -- 

   $Id$

   Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

   This file is part of librep.

   librep is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   librep is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with librep; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(define-module rep.vm.compiler.utils

    (export current-stack max-stack
	    current-b-stack max-b-stack
	    const-env inline-env
	    defuns defvars defines
	    output-stream
	    silence-compiler
	    compiler-error
	    compiler-warning
	    remember-function
	    forget-function
	    remember-variable
	    remember-lexical-variable
	    check-variable-ref
	    check-variable-bind
	    check-function-call
	    increment-stack
	    decrement-stack
	    increment-b-stack
	    decrement-b-stack
	    get-lambda-vars
	    compiler-constant?
	    compiler-constant-value
	    constant-function?
	    constant-function-value
	    compile-declaration)

    (open rep
	  rep.io.files
	  rep.vm.compiler.modules
	  rep.vm.compiler.bindings
	  rep.vm.compiler.basic
	  rep.vm.bytecodes)

  (define current-stack (make-fluid 0))		;current stack requirement
  (define max-stack (make-fluid 0))		;highest possible stack
  (define current-b-stack (make-fluid 0))	;current binding stack req.
  (define max-b-stack (make-fluid 0))		;highest possible binding stack

  (define const-env (make-fluid '()))		;alist of (NAME . CONST-DEF)
  (define inline-env (make-fluid '()))		;alist of (NAME . FUN-VALUE)
  (define defuns (make-fluid '()))		;alist of (NAME REQ OPT RESTP)
					; for all functions/macros in the file
  (define defvars (make-fluid '()))		;all vars declared at top-level
  (define defines (make-fluid '()))		;all lex. vars. at top-level

  (define output-stream (make-fluid))	;stream for compiler output

  ;; also: shadowing
  (defvar *compiler-warnings* '(unused bindings parameters misc deprecated))
  (define silence-compiler (make-fluid nil))

  ;; Message output

  (define last-current-file t)
  (define last-current-fun t)

  (define (ensure-output-stream)
    (when (null? (fluid-ref output-stream))
      (if (or *batch-mode* (not (feature? 'jade)))
	  (fluid-set! output-stream (stdout-file))
	(declare (bound open-buffer))
	(fluid-set! output-stream (open-buffer "*compilation-output*"))))
    (when (and (feature? 'jade)
	       (progn
		 (declare (bound bufferp goto-buffer goto
				 end-of-buffer current-buffer))
		 (and (bufferp (fluid-ref output-stream))
		      (not (eq? (current-buffer) (fluid-ref output-stream))))))
      (goto-buffer (fluid-ref output-stream))
      (goto (end-of-buffer))))

  (define (abbreviate-file file)
    (let ((c-dd (file-name-as-directory
		 (canonical-file-name *default-directory*)))
	  (c-file (canonical-file-name file)))
      (if (string-prefix? c-file c-dd)
	  (substring c-file (string-length c-dd))
	file)))

  (define (file-prefix #!optional form)
    (unless form
      (set! form (fluid-ref current-form)))
    (let ((origin (and form (lexical-origin form))))
      (cond (origin
	     (format nil "%s:%d: "
		     (abbreviate-file (car origin)) (cdr origin)))
	    ((fluid-ref current-file)
	     (format nil "%s: " (abbreviate-file (fluid-ref current-file))))
	    (t ""))))

  (defun compiler-message (fmt #!key form #!rest args)
    (unless (fluid-ref silence-compiler)
      (ensure-output-stream)
      (unless (and (eq? last-current-fun (fluid-ref current-fun))
		   (eq? last-current-file (fluid-ref current-file)))
	(if (fluid-ref current-fun)
	    (format (fluid-ref output-stream) "%sIn function `%s':\n"
		    (file-prefix form) (fluid-ref current-fun))
	  (format (fluid-ref output-stream) "%sAt top-level:\n"
		  (file-prefix form))))
      (apply format (fluid-ref output-stream)
	     (concat "%s" fmt #\newline) (file-prefix form) args)
      (set! last-current-fun (fluid-ref current-fun))
      (set! last-current-file (fluid-ref current-file))))

  (put 'compile-error 'error-message "Compilation mishap")

  (defun compiler-error (fmt #!key form #!rest data)
    (apply compiler-message fmt #:form form data)
    (signal 'compile-error (list (apply format nil fmt data))))

  (defun compiler-warning (type fmt #!key form #!rest args)
    (when (memq type *compiler-warnings*)
      (apply compiler-message (concat "warning: " fmt) #:form form args)))

  ;; Note that there's a function or macro NAME with lambda-list ARGS
  ;; in the current file

  (defun remember-function (name args #!optional body)
    (when body
      (let ((cell (assq name (fluid-ref inline-env))))
	;; a function previously declared inline
	(when (and cell (not (cdr cell)))
	  (set-cdr! cell (list* 'lambda args body)))))
    (if (assq name (fluid-ref defuns))
	(compiler-warning
	 'misc "function or macro `%s' defined more than once" name)
      (let
	  ((count (vector 0 nil nil)) ;required, optional, rest
	   (keys '())
	   (state 0))
	;; Scan the lambda-list for the number of required and optional
	;; arguments, and whether there's a #!rest clause
	(let loop ((args args))
	  (cond ((null? args))
		((symbol? args)
		 ;; dotted #!rest arg
		 (vector-set! count 2 t))
		((memq (car args) '(#!optional #!key #!rest))
		 (case (car args)
		   ((#!optional)
		    (set! state 1)
		    (vector-set! count 1 0))
		   ((#!key)
		    (set! state 'key))
		   ((#!rest)
		    (set! args nil)
		    (vector-set! count 2 t)))
		 (loop (cdr args)))
		(t (if (number? state)
		       (vector-set! count state (1+ (vector-ref count state)))
		     (set! keys (cons (if (pair? (car args))
					  (caar args)
					(car args))
				      keys)))
		   (loop (cdr args)))))
	(fluid-set! defuns (cons (list name (vector-ref count 0)
				      (vector-ref count 1)
				      (vector-ref count 2) keys)
				(fluid-ref defuns))))))

  (defun forget-function (name)
    (let ((cell (assq name (fluid-ref defuns))))
      (fluid-set! defuns (delq! cell (fluid-ref defuns)))))

  (defun remember-variable (name)
    (cond ((memq name (fluid-ref defines))
	   (compiler-error
	    "variable `%s' was previously declared lexically" name))
	  ((memq name (fluid-ref defvars))
	   (compiler-warning
	    'misc "variable `%s' defined more than once" name))
	  (t
	   (fluid-set! defvars (cons name (fluid-ref defvars))))))

  (defun remember-lexical-variable (name)
    (cond ((memq name (fluid-ref defvars))
	   (compiler-error
	    "variable `%s' was previously declared special" name))
	  ((memq name (fluid-ref defines))
	   (compiler-warning
	    'misc "lexical variable `%s' defined more than once" name))
	  (t
	   (fluid-set! defines (cons name (fluid-ref defines))))))

  ;; Test that a reference to variable NAME appears valid

  (defun check-variable-ref (name)
    (when (and (symbol? name)
	       (not (keyword? name))
	       (null? (memq name (fluid-ref defvars)))
	       (null? (memq name (fluid-ref defines)))
	       (not (has-local-binding? name))
	       (null? (assq name (fluid-ref defuns)))
	       (not (compiler-bound? name)))
      (compiler-warning
       'bindings "referencing undeclared free variable `%s'" name)))

  ;; Test that binding to variable NAME appears valid

  (defun check-variable-bind (name)
    (cond ((assq name (fluid-ref defuns))
	   (compiler-warning
	    'shadowing "binding to `%s' shadows function" name))
	  ((has-local-binding? name)
	   (compiler-warning
	    'shadowing "binding to `%s' shadows earlier binding" name))
	  ((and (compiler-bound? name)
		(function? (compiler-variable-ref name)))
	   (compiler-warning
	    'shadowing "binding to `%s' shadows pre-defined value" name))))

  ;; Test a call to NAME with NARGS arguments

  ;; FIXME: functions in comp-fun-bindings aren't type-checked.
  ;; FIXME: this doesn't handle #!key params.

  (defun check-function-call (name nargs)
    (when (symbol? name)
      (let-escape return
	(let ((decl (assq name (fluid-ref defuns))))
	  (when (and (null? decl) (or (assq name (fluid-ref inline-env))
				      (compiler-bound? name)))
	    (let ((new-decl (or (cdr (assq name (fluid-ref inline-env)))
				(compiler-variable-ref name))))
	      (when new-decl
		(when (or (subr? new-decl)
			  (and (closure? new-decl)
			       (eq? (car (closure-function new-decl))
				    'autoload)))
		  (return))
		(when (eq? (car new-decl) 'macro)
		  (set! new-decl (cdr new-decl)))
		(when (closure? new-decl)
		  (set! new-decl (closure-function new-decl)))
		(unless (bytecode? new-decl)
		  (remember-function name (list-ref new-decl 1))))))
	  (set! decl (assq name (fluid-ref defuns)))
	  (if (null? decl)
	      (unless (or (has-local-binding? name)
			  (memq name (fluid-ref defvars))
			  (memq name (fluid-ref defines))
			  (locate-variable name))
		(compiler-warning
		 'misc "calling undeclared function `%s'" name))
	    (let
		((required (list-ref decl 1))
		 (optional (list-ref decl 2))
		 (rest (list-ref decl 3))
		 (keys (list-ref decl 4)))
	      (if (< nargs required)
		  (compiler-warning
		   'parameters "%d %s required by `%s'; %d supplied"
		   required (if (= required 1) "argument" "arguments")
		   name nargs)
		(when (and (null? rest) (null? keys)
			   (> nargs (+ required (or optional 0))))
		  (compiler-warning 'parameters
		   "too many arguments to `%s' (%d given, %d used) %S"
		   name nargs (+ required (or optional 0)) decl)))))))))

  ;; Increment the current stack size, setting the maximum stack size if
  ;; necessary

  (defun increment-stack (#!optional (n 1))
    (let ((value (+ (fluid-ref current-stack) n)))
      (fluid-set! current-stack value)
      (when (> value (fluid-ref max-stack))
	(fluid-set! max-stack value))))

  (defun decrement-stack (#!optional (n 1))
    (let ((sp (- (fluid-ref current-stack) n)))
      (fluid-set! current-stack sp)
      (unless (>= sp 0)
	(error "Invalid stack pointer"))))

  (defun increment-b-stack ()
    (fluid-set! current-b-stack (1+ (fluid-ref current-b-stack)))
    (when (> (fluid-ref current-b-stack) (fluid-ref max-b-stack))
      (fluid-set! max-b-stack (fluid-ref current-b-stack))))

  (defun decrement-b-stack ()
    (fluid-set! current-b-stack (1- (fluid-ref current-b-stack))))

  ;; Remove all keywords from a lambda list ARGS, returning the list of
  ;; variables that would be bound (in the order they would be bound)

  (defun get-lambda-vars (args)
    (let loop ((rest args)
	       (vars '()))
      (if (null? rest)
	  (reverse! vars)
	(if (symbol? rest)
	    (loop nil (cons rest vars))
	  (loop (cdr rest)
		(if (memq (car rest) '(#!optional #!key #!rest))
		    vars
		  (cons (car rest) vars)))))))

  ;; Return t if FORM is a constant

  (defun compiler-constant? (form)
    (cond ((pair? form)
	   ;; FIXME: this is wrong, but easy..!
	   (eq? (car form) 'quote))
	  ((symbol? form)
	   (or (keyword? form)
	       (assq form (fluid-ref const-env))
	       (compiler-binding-immutable? form)))
	  ;; Assume self-evaluating
	  (t t)))

  ;; If FORM is a constant, return its value

  (defun compiler-constant-value (form)
    (cond ((pair? form)
	   ;; only quote
	   (list-ref form 1))
	  ((symbol? form)
	   (cond ((keyword? form) form)
		 ((compiler-binding-immutable? form)
		  (compiler-variable-ref form))
		 (t (cdr (assq form (fluid-ref const-env))))))
	  (t form)))

  (defun constant-function? (form)
    (set! form (compiler-macroexpand form))
    (and (eq? (car form) 'lambda)
	 ;; FIXME: this is broken
	 (compiler-binding-from-rep? (car form))))

  (defun constant-function-value (form)
    (set! form (compiler-macroexpand form))
    (cond ((eq? (car form) 'lambda)
	   form)
	  ((eq? (car form) 'function)
	   (list-ref form 1))))

  ;; Declarations

  (defun compile-declaration (form)
    (for-each
     (lambda (clause)
       (let ((handler (get (or (car clause) clause) 'compiler-decl-fun)))
	 (if handler
	     (handler clause)
	   (compiler-warning 'misc "unknown declaration: `%s'" clause))))
     form))

  (defun declare-inline (form)
    (for-each (lambda (name)
		(when (symbol? name)
		  (unless (assq name (fluid-ref inline-env))
		    (fluid-set! inline-env (cons (cons name nil)
						 (fluid-ref inline-env))))))
	      (cdr form)))
  (put 'inline 'compiler-decl-fun declare-inline))
