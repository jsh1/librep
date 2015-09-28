#| rep.jl -- inliners for many rep language features

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

(define-module rep.vm.compiler.rep ()

    (open rep
	  rep.lang.doc
	  rep.vm.bytecodes
	  rep.vm.compiler.modules
	  rep.vm.compiler.utils
	  rep.vm.compiler.basic
	  rep.vm.compiler.inline
	  rep.vm.compiler.lap
	  rep.vm.compiler.bindings)

  ;; List of side-effect-free functions. They should always return the
  ;; same value when given the same inputs. Used when constant folding.
  (define constant-functions
    '(+ - * / % modulo max min 1+ 1- car cdr assoc assq rassoc rassq
      list-ref list-tail last member memq array? array-ref substring concat
      length elt lognot not logior logxor logand equal? = /= > < >= <=
      ash zero? null? atom? pair? list? number? integer? string? vector?
      bytecode? function? macro? string<=? string>? string>=? string-ci=?
      string-ci<? string-ci<=? string-ci>? string-ci>=? special-form?
      subr? sequence? string-prefix? string-match string-looking-at
      quote-regexpd complete-string char-alphabetic? char-upper-case?
      char-lower-case? char-numeric? char-alphanumeric? char-whitespace?
      char-upcase char-downcase quotient floor ceiling truncate round
      exp log sin cos tan asin acos atan sqrt expt prin1-to-string
      read-from-string assoc-regexp nop identity caar cdar cadr cddr
      caaar cdaar cadar cddar caadr cdadr caddr cdddr positive? negative?
      odd? even? abs lcm % modulo ash string-upper-case?
      string-lower-case? string-capitalized? list-length vector-length
      string-length vector-ref string-ref char->integer integer->char
      char? char<=? char>? char>=? char-ci=? char-ci<? char-ci<=?
      char-ci>? char-ci>=?))

  ;; List of symbols, when the name of the function called by a top-level
  ;; form is one of these that form is compiled.
  (define top-level-compiled
    '(if cond when unless let let* letrec catch unwind-protect condition-case
      progn prog1 prog2 while and or case define-module module))

  ;; List of symbols, when the car of a top-level form is a member of this
  ;; list, don't macroexpand the form before compiling.
  (define top-level-unexpanded
    '(defun defmacro defvar defconst defsubst %define require
      declare eval-when-compile define-module module))


;;; pass 1 support

  (defun pass-1 (forms) (add-progns (pass-1* forms)))

  (defun pass-1* (forms) (lift-progns (map do-pass-1 forms)))

  ;; flatten progn forms into their container
  (defun lift-progns (forms)
    (let loop ((rest (reverse forms))
	       (out '()))
      (cond ((null? rest) out)
	    ((eq? (caar rest) 'progn)
	     (loop (cdr rest) (append (cdar rest) out)))
	    (t (loop (cdr rest) (cons (car rest) out))))))

  ;; merge `non-top-level' forms into progn blocks. These will then
  ;; get compiled into single run-byte-code forms
  (defun add-progns (forms)
    (let loop ((rest forms))
      (cond ((null? rest) forms)
	    ((memq (caar rest) top-level-unexpanded) (loop (cdr rest)))
	    (t (unless (eq? (caar rest) 'progn)
		 (set-car! rest (list 'progn (car rest))))
	       (if (and (cadr rest)
			(not (memq (caadr rest) top-level-unexpanded)))
		   (progn
		     (set-car! rest (append! (car rest) (list (cadr rest))))
		     (set-cdr! rest (cddr rest))
		     (loop rest))
		 (loop (cdr rest)))))))

  (defun do-pass-1 (form)
    (let-fluids ((current-form form))
      (unless (or (memq (car form) top-level-unexpanded)
		  (memq (car form) top-level-compiled))
	(set! form (compiler-macroexpand
		    form (lambda (in out)
			   (or (eq? in out)
			       (memq (car out) top-level-unexpanded)
			       (memq (car out) top-level-compiled))))))
      (case (car form)
	((defun)
	 (remember-function (list-ref form 1) (list-ref form 2) (list-tail form 3)))

	((defmacro)
	 (remember-function (list-ref form 1) (list-ref form 2))
	 (note-macro-def (list-ref form 1) (cons 'lambda (list-tail form 2))))

	((defsubst)
	 (fluid-set! inline-env (cons (cons (list-ref form 1)
					   (cons 'lambda (list-tail form 2)))
				     (fluid-ref inline-env))))

	((defvar)
	 (remember-variable (list-ref form 1)))

	((defconst)
	 (remember-variable (list-ref form 1))
	 (fluid-set! const-env (cons (cons (list-ref form 1) (list-ref form 2))
				    (fluid-ref const-env))))

	((%define) (remember-lexical-variable (list-ref form 1)))

	((require)
	 (if (compiler-constant? (cadr form))
	     (note-require (compiler-constant-value (cadr form)))
	   ;; hmm..
	   (eval form)))

	((declare)
	 (compile-declaration (cdr form)))

	((eval-when-compile)
	 (if (and (eq? (car (list-ref form 1)) 'require)
		  (compiler-constant? (cadr (list-ref form 1))))
	     (note-require (compiler-constant-value (cadr (list-ref form 1))))
	   (eval (list-ref form 1))))

	((progn)
	 (set! form (cons 'progn (pass-1* (cdr form)))))

	;; put bare forms into progns so they can be merged in pass-1
	(t (unless (memq (car form) top-level-unexpanded)
	     (set! form (list 'progn form)))))

      form))


;;; pass 2 support

  (defun pass-2 (forms)
    (let loop ((rest forms)
	       (out '()))
      (if (null? rest)
	  (reverse! out)
	(loop (cdr rest) (cons (do-pass-2 (car rest)) out)))))

  (defun do-pass-2 (form)
    (let-fluids ((current-form form))
      (case (car form)
	((defun defsubst)
	 (let ((tmp (assq (list-ref form 1) (fluid-ref macro-env))))
	   (let-fluids ((current-fun (list-ref form 1)))
	     ;;(format *standard-error* "[%s]\n" (fluid-ref current-fun))
	     (when tmp
	       (set-car! tmp nil)
	       (set-cdr! tmp nil))
	     (list 'defun (list-ref form 1)
		   (compile-lambda (cons 'lambda (list-tail form 2))
				   (list-ref form 1))))))

	((defmacro)
	 (let ((code (call-with-frame
		      (lambda ()
			(create-binding '*macro-environment*)
			(compile-lambda (cons 'lambda (list-tail form 2))
					(list-ref form 1)))))
	       (tmp (assq (list-ref form 1) (fluid-ref macro-env))))
	   (let-fluids ((current-fun (list-ref form 1)))
	     (if tmp
		 (set-cdr! tmp (make-closure code))
	       (compiler-error
		"compiled macro `%s' wasn't in environment" (list-ref form 1)))
	     (list 'defmacro (list-ref form 1) code))))

	((defconst)
	 (let ((doc (list-ref form 3)))
	   (when (and *compiler-write-docs* (string? doc))
	     (add-documentation (list-ref form 1) (fluid-ref current-module) doc)
	     (set! form (delq! doc form)))
	   (unless (memq (list-ref form 1) (fluid-ref defvars))
	     (remember-variable (list-ref form 1)))
	   (unless (assq (list-ref form 1) (fluid-ref const-env))
	     (compiler-warning
	      'bindings "unknown constant `%s'" (list-ref form 1))))
	 form)

	((defvar)
	 (let ((value (list-ref form 2))
	       (doc (list-ref form 3)))
	   (when (and (list? value)
		      (not (compiler-constant? value)))
	     ;; Compile the definition. A good idea?
	     (set-car! (list-tail form 2) (compile-form (list-ref form 2))))
	   (when (and *compiler-write-docs* (string? doc))
	     (add-documentation (list-ref form 1) nil doc)
	     (set! form (delq! (list-ref form 3) form)))
	   (unless (memq (list-ref form 1) (fluid-ref defvars))
	     (remember-variable (list-ref form 1))))
	 form)

	((%define)
	 (let ((sym (list-ref form 1))
	       (value (list-ref form 2))
	       (doc (list-ref form 3)))
	   (unless (memq sym (fluid-ref defines))
	     (remember-lexical-variable (compiler-constant-value sym)))
	   (when (and *compiler-write-docs* (string? doc))
	     (add-documentation sym (fluid-ref current-module) doc)
	     (set! form (delq! doc form)))
	   (when (and (list? value) (not (compiler-constant? value)))
	     ;; Compile the definition. A good idea?
	     (set-car! (list-tail form 2) (compile-form (list-ref form 2))))
	   form))

	((define-module)
	 (compile-top-level-define-module form))

	((module)
	 (compile-top-level-module form))

	((eval-when-compile) nil)

	(t (if (memq (car form) top-level-compiled)
	       (compile-form form)
	     form)))))


;;; Source code transformations. These are basically macros that are only
;;; used at compile-time.

  ;; tells the constant-folder which functions can be removed
  (defun foldable? (name)
    (memq name constant-functions))

  (defun trans-defvar (form)
    (let
	((name (list-ref form 1))
	 (value (list-ref form 2))
	 (doc (list-ref form 3)))
      (remember-variable name)
      (when (and (compiler-constant? doc)
		 (string? (compiler-constant-value doc))
		 *compiler-write-docs*)
	(add-documentation name nil (compiler-constant-value doc))
	(set! doc nil))
      `(progn
	 ,@(and doc (list `(put ',name 'documentation ,doc)))
	 (make-variable-special ',name)
	 (unless (variable-bound? ',name)
	   (set! ,name ,value)))))
  (put 'defvar 'rep-compile-transform trans-defvar)

  (defun trans-require (form)
    (let
	((feature (list-ref form 1)))
      (when (compiler-constant? feature)
	(note-require (compiler-constant-value feature)))
      ;; Must transform to something other than (require FEATURE) to
      ;; prevent infinite regress
      `(funcall require ,feature)))
  (put 'require 'rep-compile-transform trans-require)

  (defun trans-/= (form)
    `(not (= ,@(cdr form))))
  (put '/= 'rep-compile-transform trans-/=)


;;; Functions which compile non-standard functions (ie special-forms)

  ;; module compilers from compiler-modules
  (put 'module 'rep-compile-fun compile-anonymous-module)
  (put 'define-module 'rep-compile-fun compile-define-module)
  (put 'module-ref 'rep-compile-fun compile-module-ref)

  (defun compile-declare (form)
    (compile-declaration (cdr form))
    (compile-constant nil))
  (put 'declare 'rep-compile-fun compile-declare)

  (defun compile-quote (form)
    (compile-constant (car (cdr form))))
  (put 'quote 'rep-compile-fun compile-quote)

  (defun compile-function (form)
    (compile-form-1 (cadr form)))
  (put 'function 'rep-compile-fun compile-function)

  (defun compile-lambda-form (form)
    (compile-lambda-constant form))
  (put 'lambda 'rep-compile-fun compile-lambda-form)

  (defun compile-while (form)
    (let
	((top-label (make-label))
	 (test-label (make-label)))
      (emit-insn `(jmp ,test-label))
      (fix-label top-label)
      (compile-body (list-tail form 2))
      (emit-insn '(pop))
      (decrement-stack)
      (fix-label test-label)
      (compile-form-1 (list-ref form 1))
      (emit-insn `(jpt ,top-label))))
  (put 'while 'rep-compile-fun compile-while)

  (defun compile-%define (form)
    (compile-constant (list-ref form 1))
    (compile-form-1 (list-ref form 2))
    (emit-insn '(%define))
    (decrement-stack))
  (put '%define 'rep-compile-fun compile-%define)

  ;; Compile mapc specially if we can open code the function call
  (defun compile-mapc (form)
    (let
	((fun (list-ref form 1))
	 (lst (list-ref form 2)))
      (if (constant-function? fun)
	  ;; We can open code the function
	  (let ((top-label (make-label))
		(end-label (make-label)))
	    (set! fun (constant-function-value fun))
	    (compile-form-1 lst)
	    (emit-insn `(jpn ,end-label))
	    (fix-label top-label)
	    (emit-insn '(dup))
	    (increment-stack)
	    (emit-insn '(car))
	    (compile-lambda-inline fun nil 1)
	    (emit-insn '(pop))
	    (decrement-stack)
	    (emit-insn '(cdr))
	    ;; I don't have a jump-if-t-but-never-pop instruction, so
	    ;; make one out of "jpt TOP; nil". If I ever get a peephole
	    ;; optimiser working, the nil should be fodder for it..
	    (emit-insn `(jtp ,top-label))
	    (fix-label end-label)
	    (emit-insn '(push ())))
	;; The function must be called, so just use the mapc opcode
	(compile-form-1 fun)
	(compile-form-1 lst)
	(emit-insn '(mapc))
	(decrement-stack))))
  (put 'mapc 'rep-compile-fun compile-mapc)

  ;; Compile single-list map calls to mapcar instruction
  (defun compile-map (form)
    (let ((lsts (list-tail form 2)))
      (if (null? (cdr lsts))
	  (compile-2-args form)
	;; easiest way to fall-back to map function
	(compile-funcall (cons 'funcall form)))))
  (put 'map 'rep-compile-fun compile-map)
  (put 'map 'rep-compile-opcode 'mapcar)

  ;; Compile single-list for-each calls to mapc form
  (defun compile-for-each (form)
    (let ((lists (list-tail form 2)))
      (if (null? (cdr lists))
	  (compile-mapc form)
	(compile-funcall (cons 'funcall form)))))
  (put 'for-each 'rep-compile-fun compile-for-each)

  (defun compile-progn (form #!optional return-follows)
    (compile-body (cdr form) return-follows))
  (put 'progn 'rep-compile-fun compile-progn)

  (defun compile-prog1 (form)
    (compile-form-1 (list-ref form 1))
    (compile-body (list-tail form 2))
    (emit-insn '(pop))
    (decrement-stack))
  (put 'prog1 'rep-compile-fun compile-prog1)

  (defun compile-set! (form)
    (let ((sym (list-ref form 1))
	  (val (list-ref form 2)))
      (unless (symbol? sym)
	(compiler-error "trying to set value of a non-symbol: %s" sym))
      (unless (null? (list-tail form 3))
	(compiler-error "too many parameters to set!: %S" form))
      (compile-form-1 val)
      (emit-varset sym)
      (decrement-stack)
      (emit-insn '(push #undefined))
      (increment-stack)))
  (put 'set! 'rep-compile-fun compile-set!)

  ;; compile let* specially to coalesce all bindings into a single frame
  (defun compile-let* (form #!optional return-follows)
    (let
	((lst (car (cdr form))))
      (call-with-frame
       (lambda ()
	 (emit-push-frame 'variable)
	 (while (pair? lst)
	   (cond ((pair? (car lst))
		  (let ((tmp (car lst)))
		    (compile-body (cdr tmp))
		    (check-variable-bind (car tmp))
		    (create-binding (car tmp))
		    (emit-binding (car tmp))))
		 (t (emit-insn '(push ()))
		    (increment-stack)
		    (check-variable-bind (car lst))
		    (create-binding (car lst))
		    (emit-binding (car lst))))
	   (decrement-stack)
	   (set! lst (cdr lst)))
	 (compile-body (list-tail form 2) return-follows)
	 (emit-pop-frame 'variable)))))
  (put 'let* 'rep-compile-fun compile-let*)

  ;; let can be compiled straight from its macro definition

  ;; compile letrec specially to handle tail recursion elimination
  (defun compile-letrec (form #!optional return-follows)
    (let ((bindings (car (cdr form))))
      (call-with-frame
       (lambda ()
	 (push-state)
	 (emit-push-frame 'variable)

	 ;; create the bindings, should really be to void values, but use nil..
	 (for-each (lambda (cell)
		     (let ((var (or (car cell) cell)))
		       (compile-constant nil)
		       (check-variable-bind var)
		       (create-binding var)
		       (emit-binding var)
		       (decrement-stack))) bindings)
	 ;; then set them to their values
	 (for-each (lambda (cell)
		     (let ((var (or (car cell) cell)))
		       (compile-body (cdr cell) nil var)
		       (emit-varset var)
		       (decrement-stack))) bindings)

	 ;; Test if we can inline it away.
	 ;; Look for forms like (letrec ((foo (lambda (..) body..))) (foo ..))
	 ;; where `foo' only appears in inlinable tail calls in body
	 (when (let-escape no-inline
		 (unless (= (list-length bindings) 1)
		   (no-inline t))
		 (let ((var (or (caar bindings) (car bindings)))
		       (value (cdar bindings)))
		   (unless (and (binding-tail-call-only? var)
				value (not (cdr value))
				(eq? (caar value) 'lambda))
		     (no-inline t))
		   (set! value (car value))
		   (let ((body (list-tail form 2)))
		     (unless (= (list-length body) 1)
		       (no-inline t))
		     (set! body (car body))
		     (when (and (eq? (car body) (get-language-property
						'compiler-sequencer))
				(= (list-length body) 2))
		       (set! body (cadr body)))
		     (unless (eq? (car body) var)
		       (no-inline t))

		     ;; okay, let's go
		     (let-fluids ((silence-compiler t))
		       (reload-state)
		       (create-binding var #:no-location t)
		       (remember-function var (cadr value))
		       (compile-lambda-inline value (cdr body)
					      nil return-follows var)
		       (forget-function var)
		       nil))))

	   ;; no, keep on the usual track
	   (compile-body (list-tail form 2) return-follows)
	   (emit-pop-frame 'variable))
	 (pop-state)))))
  (put 'letrec 'rep-compile-fun compile-letrec)

  (defun compile-let-fluids (form)
    (let ((bindings (cadr form))
	  (body (cddr form)))
      (call-with-frame
       (lambda ()
	 (call-with-dynamic-binding
	  (lambda ()
	    ;; compile each fluid, value pair onto the stack
	    (for-each (lambda (cell)
			(compile-form-1 (car cell))
			(compile-body (cdr cell))) bindings)
	    (emit-push-frame 'fluid)
	    (for-each (lambda (unused)
			(declare (unused unused))
			(emit-insn '(fluid-bind))
			(decrement-stack 2)) bindings)
	    (compile-body body)
	    (emit-pop-frame 'fluid)))))))
  (put 'let-fluids 'rep-compile-fun compile-let-fluids)

  (defun compile-let-escape (form #!optional return-follows)
    (let ((var (cadr form))
	  (body (cddr form)))
      (call-with-frame
       (lambda ()
	 (push-state)

	 ;; First try the general case, binding an actual closure that
	 ;; does a dynamic exit via catch/throw using the closure as
	 ;; the catch tag (need something unique to simulate lexical
	 ;; scope). But if the function binding is only referenced
	 ;; from the function slot of applications, discard all that
	 ;; code and recompile with calls to the function as jumps to
	 ;; the end of the compiled code body.

	 (emit-push-frame 'variable)
	 (emit-varref 'let-escape/tag)
	 (increment-stack)
	 (emit-insn '(call 0))
	 (check-variable-bind var)
	 (create-binding var)
	 (emit-binding var)
	 (decrement-stack)

	 (catch-helper
	  (lambda ()
	    (emit-varref var #:for-call t)	;avoids adding not-call-only
	    (increment-stack))
	  (lambda ()
	   (compile-body body)))

	 (emit-pop-frame 'variable)

	 (when (binding-call-only? var)
	   ;; no one used VAR except to call it as a function, so rewind
	   (let-fluids ((silence-compiler t))
	     (reload-state)
	     (let ((end-label (make-label)))
	       (create-binding var #:no-location t)
	       ;; lambda emitters are how we replace named function
	       ;; calls by arbitrary code
	       (call-with-lambda-emitter var '(#!optional v) +1
		(lambda (args unbind)
		  (compile-form-1 (if args (car args) #undefined))
		  (unbind)
		  (emit-insn `(jmp ,end-label)))
		(lambda ()
		  (compile-body body return-follows)
		  (fix-label end-label))))))
	 ;; done
	 (pop-state)))))
  (put 'let-escape 'rep-compile-fun compile-let-escape)

  (defun compile-defun (form)
    (remember-function (list-ref form 1) (list-ref form 2))
    (compile-constant (list-ref form 1))
    (compile-lambda-constant
     (cons 'lambda (list-tail form 2)) (list-ref form 1))
    (emit-insn '(%define))
    (decrement-stack))
  (put 'defun 'rep-compile-fun compile-defun)

  (defun compile-defmacro (form)
    (remember-function (list-ref form 1) (list-ref form 2))
    (compile-constant (list-ref form 1))
    (compile-constant 'macro)
    (compile-lambda-constant
     (cons 'lambda (list-tail form 2)) (list-ref form 1))
    (emit-insn '(cons))
    (emit-insn '(%define))
    (decrement-stack))
  (put 'defmacro 'rep-compile-fun compile-defmacro)

  (defun compile-cond (form #!optional return-follows)
    (let
	((end-label (make-label))
	 (need-trailing-nil t))
      (set! form (cdr form))
      (while (pair? form)
	(let*
	    ((subl (car form))
	     (condition (car subl))
	     (next-label (make-label)))
	  ;; See if we can squash a constant condition to t or nil
	  (when (compiler-constant? condition)
	    (set! condition (not (not (compiler-constant-value condition)))))
	  (cond
	   ((eq? condition t)
	    ;; condition t -- always taken
	    (if (pair? (cdr subl))
		;; There's something besides the condition
		(progn
		  (compile-body (cdr subl) return-follows)
		  (decrement-stack))
	      (if (eq? condition (car subl))
		  (emit-insn '(push t))
		(compile-form-1 (car subl) #:return-follows return-follows)
		(decrement-stack)))
	    (when (pair? (cdr form))
	      ;;(compiler-warning
	      ;; 'misc "unreachable conditions after t in cond statement")
	      ;; Ignore the rest of the statement
	      (set! form nil))
	    (set! need-trailing-nil nil))
	   ((eq? condition nil)
	    ;; condition nil -- never taken
	    (when (cdr subl)
	      ;;(compiler-warning
	      ;; 'misc "unreachable forms after nil in cond statement")
	      ))
	   (t
	    ;; non t-or-nil condition
	    (compile-form-1 (car subl)
			    #:return-follows (and return-follows
						  (null? (cdr subl))
						  (null? (cdr form))))
	    (decrement-stack)
	    (if (pair? (cdr subl))
		;; Something besides the condition
		(if (cdr form)
		    ;; This isn't the last condition list
		    (progn
		      (emit-insn `(jn ,next-label))
		      (compile-body (cdr subl) return-follows)
		      (decrement-stack)
		      (emit-insn `(jmp ,end-label))
		      (fix-label next-label))
		  ;; It is the last condition list, use the result
		  ;; of this condition for the return value when it's
		  ;; nil
		  (emit-insn `(jnp ,end-label))
		  (compile-body (cdr subl) return-follows)
		  (decrement-stack)
		  (set! need-trailing-nil nil))
	      ;; No action to take
	      (if (cdr form)
		  ;; This isn't the last condition list
		  (emit-insn `(jtp ,end-label))
		;; This is the last condition list, since there's no
		;; action to take, just fall out the bottom, with the
		;; condition as value.
		(set! need-trailing-nil nil))))))
	(set! form (cdr form)))
      (when need-trailing-nil
	(emit-insn '(push ())))
      (increment-stack)
      (fix-label end-label)))
  (put 'cond 'rep-compile-fun compile-cond)

  (defun compile-case (form #!optional return-follows)
    (let
	((end-label (make-label))
	 (had-default nil))
      (set! form (cdr form))
      (unless form
	(compiler-error "no key value in case statement"))
      ;; XXX if key is constant optimise case away..
      (compile-form-1 (car form))
      (set! form (cdr form))
      (while (pair? form)
	(unless (pair? form)
	  (compiler-error "badly formed clause in case statement"))
	(let
	    ((cases (caar form))
	     (forms (cdar form))
	     (next-label (make-label)))
	  (cond ((pair? cases)
		 (emit-insn '(dup))
		 (increment-stack)
		 (if (pair? (cdr cases))
		     ;; >1 possible case
		     (progn
		       (compile-constant cases)
		       (emit-insn '(memv)))
		   ;; only one case, use eql
		   (compile-constant (car cases))
		   (emit-insn '(eql)))
		 (decrement-stack)
		 (emit-insn `(jn ,next-label))
		 (decrement-stack))
		((eq? cases t) (set! had-default t))
		(t (compiler-error
		    "badly formed clause in case statement" #:form cases)))
	  (compile-body forms return-follows)
	  (decrement-stack)
	  (emit-insn `(jmp ,end-label))
	  (fix-label next-label)
	  (set! form (cdr form))))
      (unless had-default
	(emit-insn '(push ())))
      (increment-stack)
      (fix-label end-label)
      (emit-insn '(swap))
      (emit-insn '(pop))))
  (put 'case 'rep-compile-fun compile-case)

  (defun catch-helper (tag-thunk body-thunk)
    (let ((catch-label (make-label))
	  (start-label (make-label))
	  (end-label (make-label)))
      (call-with-dynamic-binding
       (lambda ()
	 ;;		jmp start
	 (emit-insn `(jmp ,start-label))

	 ;; catch:
	 ;;		TAG
	 ;;		catch
	 ;;		ejmp end
	 (increment-stack)		;enter with one arg on stack
	 (fix-label catch-label)
	 (tag-thunk)
	 (emit-insn '(catch))
	 (decrement-stack)
	 (emit-insn `(ejmp ,end-label))
	 (decrement-stack)

	 ;; start:
	 ;;		push #catch
	 ;;		binderr
	 ;;		BODY
	 ;;		pop-frame
	 ;; end:
	 (fix-label start-label)
	 (emit-push-frame 'exception #:handler catch-label)
	 (body-thunk)
	 (emit-pop-frame 'exception)
	 (fix-label end-label)))))

  (defun compile-catch (form)
    (catch-helper
     (lambda ()
       (compile-form-1 (list-ref form 1)))
     (lambda ()
       (compile-body (list-tail form 2)))))
  (put 'catch 'rep-compile-fun compile-catch)

  (defun compile-unwind-pro (form)
    (let ((cleanup-label (make-label))
	  (start-label (make-label))
	  (end-label (make-label)))
      (call-with-dynamic-binding
       (lambda ()
	 ;;		jmp start
	 (emit-insn `(jmp ,start-label))

	 ;; cleanup:
	 ;;		CLEANUP-FORMS
	 ;;		pop
	 ;;		ejmp end
	 ;; [overall, stack +1]
	 (increment-stack 2)
	 (fix-label cleanup-label)
	 (compile-body (list-tail form 2))
	 (emit-insn '(pop))
	 (emit-insn `(ejmp ,end-label))
	 (decrement-stack 2)

	 ;; start:
	 ;;		push #cleanup
	 ;;		binderr
	 ;;		FORM
	 ;;		pop-frame
	 ;;		nil
	 ;;		jmp cleanup
	 ;; [overall, stack +2]
	 (fix-label start-label)
	 (emit-push-frame 'exception #:handler cleanup-label)
	 (compile-form-1 (list-ref form 1))
	 (emit-pop-frame 'exception)
	 (emit-insn '(push ()))
	 (decrement-stack)
	 (emit-insn `(jmp ,cleanup-label))

	 ;; end:
	 (fix-label end-label)))))
  (put 'unwind-protect 'rep-compile-fun compile-unwind-pro)

  (defun compile-condition-case (form)
    (let ((cleanup-label (make-label))
	  (start-label (make-label))
	  (end-label (make-label))
	  (handlers (list-tail form 3)))
      (call-with-dynamic-binding
       (lambda ()
	 ;;		jmp start
	 ;; cleanup:
	 (emit-insn `(jmp ,start-label))
	 (fix-label cleanup-label)

	 (increment-stack)		;reach here with one item on stack
	 (if (pair? handlers)
	     (call-with-frame
	      (lambda ()
		(if (and (list-ref form 1) (not (eq? (list-ref form 1) 'nil)))
		    (let ((var (list-ref form 1)))
		      (when (spec-bound? var)
			(compiler-error
			 "condition-case can't bind to special variable `%s'" var))
		      (check-variable-bind var)
		      (create-binding var)
		      ;; XXX errorpro instruction always heap binds..
		      (set-binding-heap-allocated! var))
		  ;; something always gets bound
		  (let ((tem (gensym)))
		    (create-binding tem)
		    (set-binding-heap-allocated! tem)
		    ;; avoid `unused variable' warnings
		    (set-binding-maybe-unused! tem)))
		;; Loop over all but the last handler
		(while (pair? (cdr handlers))
		  (if (pair? (car handlers))
		      (let ((next-label (make-label)))
			;;	push CONDITIONS
			;;	errorpro
			;;	jtp next
			;;	HANDLER
			;;	jmp end
			;; next:
			(compile-constant (car (car handlers)))
			(emit-insn '(errorpro))
			(decrement-stack)
			(emit-insn `(jtp ,next-label))
			(decrement-stack)
			(compile-body (cdr (car handlers)))
			(emit-insn `(jmp ,end-label))
			(fix-label next-label))
		    (compiler-error
		     "badly formed condition-case handler: `%s'"
		     (car handlers) #:form handlers))
		  (set! handlers (cdr handlers)))
		;; The last handler
		(if (pair? (car handlers))
		    (let ((pc-label (make-label)))
		      ;;	push CONDITIONS
		      ;;	errorpro
		      ;;	ejmp pc
		      ;; pc:	HANDLER
		      ;;	jmp end
		      (compile-constant (car (car handlers)))
		      (emit-insn '(errorpro))
		      (decrement-stack)
		      (emit-insn `(ejmp ,pc-label))
		      (fix-label pc-label)
		      (decrement-stack)
		      (compile-body (cdr (car handlers)))
		      (emit-insn `(jmp ,end-label)))
		  (compiler-error
		   "badly formed condition-case handler: `%s'"
		   (car handlers) #:form (car handlers)))))
	   (compiler-error "no handlers in condition-case"))
	 (decrement-stack)

	 ;; start:
	 ;;		push cleanup
	 ;;		binderr
	 ;;		FORM
	 (fix-label start-label)
	 (emit-push-frame 'exception #:handler cleanup-label)
	 (compile-form-1 (list-ref form 2))

	 ;; end:
	 ;;		pop-frame		;pop error handler or VAR
	 (fix-label end-label)
	 (emit-pop-frame 'exception)))))
  (put 'condition-case 'rep-compile-fun compile-condition-case)

  (defun compile-list (form)
    (do ((args (cdr form) (cdr args))
	 (count 0 (1+ count)))
	((null? args)
	 ;; merge the arguments into a single list
	 (compile-constant '())
	 (do ((i 0 (1+ i)))
	     ((= i count))
	   (emit-insn '(cons))
	   (decrement-stack)))
      (compile-form-1 (car args))))
  (put 'list 'rep-compile-fun compile-list)

  (defun compile-list* (form)
    (do ((args (cdr form) (cdr args))
	 (count 0 (1+ count)))
	((null? args)
	 ;; merge the arguments into a single list
	 (do ((i 0 (1+ i)))
	     ((>= i (1- count)))
	   (emit-insn '(cons))
	   (decrement-stack)))
      (compile-form-1 (car args))))
  (put 'list* 'rep-compile-fun compile-list*)

  ;; Funcall normally translates to a single call instruction. However,
  ;; if the function being called is a constant lambda expression, open
  ;; code it.
  (defun compile-funcall (form #!optional return-follows)
    (let*
	((fun (list-ref form 1))
	 (args (list-tail form 2))
	 (arg-count 0)
	 (open-code (constant-function? fun)))
      (unless open-code
	(compile-form-1 fun))
      (while args
	(compile-form-1 (car args))
	(set! args (cdr args))
	(set! arg-count (1+ arg-count)))
      (if open-code
	  (progn
	    (compile-lambda-inline
	     (constant-function-value fun) nil arg-count return-follows)
	    ;; We push one less value than when using 'call
	    (if (zero? arg-count)
		(increment-stack)
	      (decrement-stack (1- arg-count))))
	(emit-insn `(call ,arg-count))
	(decrement-stack arg-count))))
  (put 'funcall 'rep-compile-fun compile-funcall)

  (defun compile-apply (form)
    (compile-form-1 (list-ref form 1))
    (do ((args (list-tail form 2) (cdr args))
	 (count 0 (1+ count)))
	((null? args)
	 ;; merge the arguments into a single list
	 (do ((i 0 (1+ i)))
	     ((>= i (1- count)))
	   (emit-insn '(cons))
	   (decrement-stack)))
      (compile-form-1 (car args)))
    (emit-insn '(apply))
    (decrement-stack))
  (put 'apply 'rep-compile-fun compile-apply)

  (defun compile-list-ref (form)
    (let
	((insn (cdr (assq (list-ref form 2) byte-list-ref-insns))))
      (if insn
	  (progn
	    (compile-form-1 (list-ref form 1))
	    (emit-insn (list insn)))
	(compile-2-args form))))
  (put 'list-ref 'rep-compile-fun compile-list-ref)
  (put 'list-ref 'rep-compile-opcode 'list-ref)

  (defun compile-list-tail (form)
    (let
	((insn (assq (list-ref form 2) byte-list-tail-insns)))
      (if insn
	  (progn
	    (compile-form-1 (list-ref form 1))
	    (when (cdr insn)
	      (emit-insn (list (cdr insn)))))
	(compile-2-args form))))
  (put 'list-tail 'rep-compile-fun compile-list-tail)
  (put 'list-tail 'rep-compile-opcode 'list-tail)

  (defun compile-minus (form)
    (if (/= (list-length form) 2)
	(compile-binary-op form)
      (compile-form-1 (car (cdr form)))
      (emit-insn '(neg))))
  (put '- 'rep-compile-fun compile-minus)
  (put '- 'rep-compile-opcode 'sub)

  (defun compile-make-closure (form)
    (when (list-tail form 3)
      (compiler-warning
       'parameters "more than two parameters to `%s'; rest ignored"
       (car form)))
    (compile-form-1 (list-ref form 1))
    (compile-form-1 (list-ref form 2))
    (emit-insn '(make-closure))
    (decrement-stack))
  (put 'make-closure 'rep-compile-fun compile-make-closure)

  (defun compile-log (form)
    (cond ((list-tail form 3)
	   (compiler-warning
	    'parameters "more than two parameters to `log'; rest ignored"))
	  ((list-tail form 2)
	   ;; dual argument form of log. compiles to
	   (compile-form-1 (list-ref form 1))
	   (emit-insn '(log))
	   (compile-form-1 (list-ref form 2))
	   (emit-insn '(log))
	   (emit-insn '(div))
	   (decrement-stack))
	  ((list-tail form 1)
	   ;; single argument form
	   (compile-form-1 (list-ref form 1))
	   (emit-insn '(log)))
	  (t (compiler-warning 'parameters "too few parameters to `log'"))))
  (put 'log 'rep-compile-fun compile-log)

  (defun get-form-opcode (form)
    (cond ((symbol? form) (get form 'rep-compile-opcode))
	  ;; must be a module-ref
	  ((eq? (car form) 'module-ref)
	   (get (caddr form) 'rep-compile-opcode))
	  (t (compiler-error "don't know opcode for `%s'" form))))

  ;; Instruction with no arguments
  (defun compile-0-args (form)
    (when (cdr form)
      (compiler-warning
       'parameters "all parameters to `%s' ignored" (car form)))
    (emit-insn (list (get-form-opcode (car form))))
    (increment-stack))

  ;; Instruction taking 1 arg on the stack
  (defun compile-1-args (form)
    (when (list-tail form 2)
      (compiler-warning
       'parameters "more than one parameter to `%s'; rest ignored" (car form)))
    (compile-form-1 (list-ref form 1))
    (emit-insn (list (get-form-opcode (car form)))))

  ;; Instruction taking 2 args on the stack
  (defun compile-2-args (form)
    (when (list-tail form 3)
      (compiler-warning
       'parameters "more than two parameters to `%s'; rest ignored"
       (car form)))
    (compile-form-1 (list-ref form 1))
    (compile-form-1 (list-ref form 2))
    (emit-insn (list (get-form-opcode (car form))))
    (decrement-stack))

  ;; Instruction taking 3 args on the stack
  (defun compile-3-args (form)
    (when (list-tail form 4)
      (compiler-warning
       'parameters "More than three parameters to `%s'; rest ignored"
       (car form)))
    (compile-form-1 (list-ref form 1))
    (compile-form-1 (list-ref form 2))
    (compile-form-1 (list-ref form 3))
    (emit-insn (list (get-form-opcode (car form))))
    (decrement-stack 2))

  ;; Compile a form `(OP ARG1 ARG2 ARG3 ...)' into as many two argument
  ;; instructions as needed (PUSH ARG1; PUSH ARG2; OP; PUSH ARG3; OP; ...)
  (defun compile-binary-op (form)
    (let
	((opcode (get-form-opcode (car form))))
      (set! form (cdr form))
      (unless (>= (list-length form) 2)
	(compiler-error
	 "too few arguments to binary operator `%s'" (car form)))
      (compile-form-1 (car form))
      (set! form (cdr form))
      (while (pair? form)
	(compile-form-1 (car form))
	(emit-insn (list opcode))
	(decrement-stack)
	(set! form (cdr form)))))

  ;; Used for >, >=, < and <=
  (defun compile-transitive-relation (form)
    (cond
     ((<= (list-length form) 2)
      (compiler-error "too few args to relation `%s'" (car form)))
     ((= (list-length form) 3)
      (let
	  ((opcode (get-form-opcode (car form))))
	;; Simple case, only two arguments, i.e. `(OP ARG1 ARG2)' into:
	;;  PUSH ARG1; PUSH ARG2; OP;
	(compile-form-1 (list-ref form 1))
	(compile-form-1 (list-ref form 2))
	(emit-insn (list opcode))
	(decrement-stack)))
     (t
      ;; Tricky case, >2 args,

      ;; Originally I did `(OP ARG1 ARG2 ARG3... ARGN)' as:

      ;;  PUSH ARG1; PUSH ARG2; DUP; SWAP2; OP; JNP Fail;
      ;;  PUSH ARG3; DUP; SWAP2; OP; JNP Fail;
      ;;  ...
      ;;  PUSH ARGN; OP; JMP End;
      ;; Fail:
      ;;  SWAP; POP;
      ;; End:

      ;; But that doesn't always evaluate all arguments..
      (compile-funcall (cons 'funcall form)))))


;;; Opcode properties for the generic instructions, in a progn for compiled
;;; speed

  (progn
    (put 'cons 'rep-compile-fun compile-2-args)
    (put 'cons 'rep-compile-opcode 'cons)
    (put 'car 'rep-compile-fun compile-1-args)
    (put 'car 'rep-compile-opcode 'car)
    (put 'cdr 'rep-compile-fun compile-1-args)
    (put 'cdr 'rep-compile-opcode 'cdr)
    (put 'set-car! 'rep-compile-fun compile-2-args)
    (put 'set-car! 'rep-compile-opcode 'set-car!)
    (put 'set-cdr! 'rep-compile-fun compile-2-args)
    (put 'set-cdr! 'rep-compile-opcode 'set-cdr!)
    (put 'array-length 'rep-compile-fun compile-1-args)
    (put 'array-length 'rep-compile-opcode 'array-length)
    (put 'array-set! 'rep-compile-fun compile-3-args)
    (put 'array-set! 'rep-compile-opcode 'array-set!)
    (put 'array-ref 'rep-compile-fun compile-2-args)
    (put 'array-ref 'rep-compile-opcode 'array-ref)
    (put 'length 'rep-compile-fun compile-1-args)
    (put 'length 'rep-compile-opcode 'length)
    (put 'list-length 'rep-compile-fun compile-1-args)
    (put 'list-length 'rep-compile-opcode 'list-length)
    (put '+ 'rep-compile-fun compile-binary-op)
    (put '+ 'rep-compile-opcode 'add)
    (put '* 'rep-compile-fun compile-binary-op)
    (put '* 'rep-compile-opcode 'mul)
    (put '/ 'rep-compile-fun compile-binary-op)
    (put '/ 'rep-compile-opcode 'div)
    (put 'remainder 'rep-compile-fun compile-2-args)
    (put 'remainder 'rep-compile-opcode 'rem)
    (put 'modulo 'rep-compile-fun compile-2-args)
    (put 'modulo 'rep-compile-opcode 'modulo)
    (put 'lognot 'rep-compile-fun compile-1-args)
    (put 'lognot 'rep-compile-opcode 'lnot)
    (put 'not 'rep-compile-fun compile-1-args)
    (put 'not 'rep-compile-opcode 'not)
    (put 'logior 'rep-compile-fun compile-binary-op)
    (put 'logior 'rep-compile-opcode 'lor)
    (put 'logxor 'rep-compile-fun compile-binary-op)
    (put 'logxor 'rep-compile-opcode 'lxor)
    (put 'logand 'rep-compile-fun compile-binary-op)
    (put 'logand 'rep-compile-opcode 'land)
    (put 'ash 'rep-compile-fun compile-2-args)
    (put 'ash 'rep-compile-opcode 'ash)
    (put 'equal? 'rep-compile-fun compile-2-args)
    (put 'equal? 'rep-compile-opcode 'equal)
    (put 'eq? 'rep-compile-fun compile-2-args)
    (put 'eq? 'rep-compile-opcode 'eq)
    (put '= 'rep-compile-fun compile-transitive-relation)
    (put '= 'rep-compile-opcode 'num-eq)
    (put '> 'rep-compile-fun compile-transitive-relation)
    (put '> 'rep-compile-opcode 'gt)
    (put '< 'rep-compile-fun compile-transitive-relation)
    (put '< 'rep-compile-opcode 'lt)
    (put '>= 'rep-compile-fun compile-transitive-relation)
    (put '>= 'rep-compile-opcode 'ge)
    (put '<= 'rep-compile-fun compile-transitive-relation)
    (put '<= 'rep-compile-opcode 'le)
    (put '1+ 'rep-compile-fun compile-1-args)
    (put '1+ 'rep-compile-opcode 'inc)
    (put '1- 'rep-compile-fun compile-1-args)
    (put '1- 'rep-compile-opcode 'dec)
    (put 'zero? 'rep-compile-fun compile-1-args)
    (put 'zero? 'rep-compile-opcode 'zerop)
    (put 'null? 'rep-compile-fun compile-1-args)
    (put 'null? 'rep-compile-opcode 'not)
    (put 'atom? 'rep-compile-fun compile-1-args)
    (put 'atom? 'rep-compile-opcode 'atom)
    (put 'pair? 'rep-compile-fun compile-1-args)
    (put 'pair? 'rep-compile-opcode 'consp)
    (put 'list? 'rep-compile-fun compile-1-args)
    (put 'list? 'rep-compile-opcode 'listp)
    (put 'number? 'rep-compile-fun compile-1-args)
    (put 'number? 'rep-compile-opcode 'numberp)
    (put 'string? 'rep-compile-fun compile-1-args)
    (put 'string? 'rep-compile-opcode 'stringp)
    (put 'string-length 'rep-compile-fun compile-1-args)
    (put 'string-length 'rep-compile-opcode 'string-length)
    (put 'string-ref 'rep-compile-fun compile-2-args)
    (put 'string-ref 'rep-compile-opcode 'string-ref)
    (put 'string-set! 'rep-compile-fun compile-3-args)
    (put 'string-set! 'rep-compile-opcode 'string-set!)
    (put 'vector? 'rep-compile-fun compile-1-args)
    (put 'vector? 'rep-compile-opcode 'vectorp)
    (put 'vector-length 'rep-compile-fun compile-1-args)
    (put 'vector-length 'rep-compile-opcode 'vector-length)
    (put 'vector-ref 'rep-compile-fun compile-2-args)
    (put 'vector-ref 'rep-compile-opcode 'vector-ref)
    (put 'vector-set! 'rep-compile-fun compile-3-args)
    (put 'vector-set! 'rep-compile-opcode 'vector-set!)
    (put 'throw 'rep-compile-fun compile-2-args)
    (put 'throw 'rep-compile-opcode 'throw)
    (put 'variable-set! 'rep-compile-fun compile-2-args)
    (put 'variable-set! 'rep-compile-opcode 'set)
    (put 'variable-bound? 'rep-compile-fun compile-1-args)
    (put 'variable-bound? 'rep-compile-opcode 'boundp)
    (put 'symbol? 'rep-compile-fun compile-1-args)
    (put 'symbol? 'rep-compile-opcode 'symbolp)
    (put 'get 'rep-compile-fun compile-2-args)
    (put 'get 'rep-compile-opcode 'get)
    (put 'put 'rep-compile-fun compile-3-args)
    (put 'put 'rep-compile-opcode 'put)
    (put 'signal 'rep-compile-fun compile-2-args)
    (put 'signal 'rep-compile-opcode 'signal)
    (put 'quotient 'rep-compile-fun compile-2-args)
    (put 'quotient 'rep-compile-opcode 'quotient)
    (put 'reverse 'rep-compile-fun compile-1-args) ; new 12/7/94
    (put 'reverse 'rep-compile-opcode 'reverse)
    (put 'reverse! 'rep-compile-fun compile-1-args)
    (put 'reverse! 'rep-compile-opcode 'nreverse)
    (put 'assoc 'rep-compile-fun compile-2-args)
    (put 'assoc 'rep-compile-opcode 'assoc)
    (put 'assq 'rep-compile-fun compile-2-args)
    (put 'assq 'rep-compile-opcode 'assq)
    (put 'rassoc 'rep-compile-fun compile-2-args)
    (put 'rassoc 'rep-compile-opcode 'rassoc)
    (put 'rassq 'rep-compile-fun compile-2-args)
    (put 'rassq 'rep-compile-opcode 'rassq)
    (put 'last 'rep-compile-fun compile-1-args)
    (put 'last 'rep-compile-opcode 'last)
    (put 'mapcar 'rep-compile-fun compile-2-args)
    (put 'mapcar 'rep-compile-opcode 'mapcar)
    (put 'member 'rep-compile-fun compile-2-args)
    (put 'member 'rep-compile-opcode 'member)
    (put 'memq 'rep-compile-fun compile-2-args)
    (put 'memq 'rep-compile-opcode 'memq)
    (put 'delete! 'rep-compile-fun compile-2-args)
    (put 'delete! 'rep-compile-opcode 'delete)
    (put 'delq! 'rep-compile-fun compile-2-args)
    (put 'delq! 'rep-compile-opcode 'delq)
    (put 'delete-if! 'rep-compile-fun compile-2-args)
    (put 'delete-if! 'rep-compile-opcode 'delete-if)
    (put 'delete-if-not! 'rep-compile-fun compile-2-args)
    (put 'delete-if-not! 'rep-compile-opcode 'delete-if-not)
    (put 'copy-sequence 'rep-compile-fun compile-1-args)
    (put 'copy-sequence 'rep-compile-opcode 'copy-sequence)
    (put 'sequence? 'rep-compile-fun compile-1-args)
    (put 'sequence? 'rep-compile-opcode 'sequencep)
    (put 'function? 'rep-compile-fun compile-1-args)
    (put 'function? 'rep-compile-opcode 'functionp)
    (put 'special-form? 'rep-compile-fun compile-1-args)
    (put 'special-form? 'rep-compile-opcode 'special-form-p)
    (put 'subr? 'rep-compile-fun compile-1-args)
    (put 'subr? 'rep-compile-opcode 'subrp)
    (put 'eqv? 'rep-compile-fun compile-2-args)
    (put 'eqv? 'rep-compile-opcode 'eql)
    (put 'max 'rep-compile-fun compile-binary-op)
    (put 'max 'rep-compile-opcode 'max)
    (put 'min 'rep-compile-fun compile-binary-op)
    (put 'min 'rep-compile-opcode 'min)
    (put 'filter 'rep-compile-fun compile-2-args)
    (put 'filter 'rep-compile-opcode 'filter)
    (put 'macro? 'rep-compile-fun compile-1-args)
    (put 'macro? 'rep-compile-opcode 'macrop)
    (put 'bytecode? 'rep-compile-fun compile-1-args)
    (put 'bytecode? 'rep-compile-opcode 'bytecodep)
    (put 'closure? 'rep-compile-fun compile-1-args)
    (put 'closure? 'rep-compile-opcode 'closurep)
    (put 'fluid-ref 'rep-compile-fun compile-1-args)
    (put 'fluid-ref 'rep-compile-opcode 'fluid-ref)
    (put 'fluid-set! 'rep-compile-fun compile-2-args)
    (put 'fluid-set! 'rep-compile-opcode 'fluid-set!)

    (put 'caar 'rep-compile-fun compile-1-args)
    (put 'caar 'rep-compile-opcode 'caar)
    (put 'cadr 'rep-compile-fun compile-1-args)
    (put 'cadr 'rep-compile-opcode 'cadr)
    (put 'cdar 'rep-compile-fun compile-1-args)
    (put 'cdar 'rep-compile-opcode 'cdar)
    (put 'cddr 'rep-compile-fun compile-1-args)
    (put 'cddr 'rep-compile-opcode 'cddr)
    (put 'caddr 'rep-compile-fun compile-1-args)
    (put 'caddr 'rep-compile-opcode 'caddr)
    (put 'cadddr 'rep-compile-fun compile-1-args)
    (put 'cadddr 'rep-compile-opcode 'cadddr)

    (put 'floor 'rep-compile-fun compile-1-args)
    (put 'floor 'rep-compile-opcode 'floor)
    (put 'ceiling 'rep-compile-fun compile-1-args)
    (put 'ceiling 'rep-compile-opcode 'ceiling)
    (put 'truncate 'rep-compile-fun compile-1-args)
    (put 'truncate 'rep-compile-opcode 'truncate)
    (put 'round 'rep-compile-fun compile-1-args)
    (put 'round 'rep-compile-opcode 'round)
    (put 'exp 'rep-compile-fun compile-1-args)
    (put 'exp 'rep-compile-opcode 'exp)
    (put 'sin 'rep-compile-fun compile-1-args)
    (put 'sin 'rep-compile-opcode 'sin)
    (put 'cos 'rep-compile-fun compile-1-args)
    (put 'cos 'rep-compile-opcode 'cos)
    (put 'tan 'rep-compile-fun compile-1-args)
    (put 'tan 'rep-compile-opcode 'tan)
    (put 'sqrt 'rep-compile-fun compile-1-args)
    (put 'sqrt 'rep-compile-opcode 'sqrt)
    (put 'expt 'rep-compile-fun compile-2-args)
    (put 'expt 'rep-compile-opcode 'expt))

  ;; setup properties to tell the compiler where to look for symbols
  ;; in the `rep'  package
  (unless (get 'rep 'compiler-handler-property)
    (put 'rep 'compiler-handler-property 'rep-compile-fun)
    (put 'rep 'compiler-transform-property 'rep-compile-transform)
    (put 'rep 'compiler-sequencer 'progn)
    (put 'rep 'compiler-pass-1 pass-1)
    (put 'rep 'compiler-pass-2 pass-2)
    (put 'rep 'compiler-foldable? foldable?)))
