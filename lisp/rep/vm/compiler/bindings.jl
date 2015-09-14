#| bindings.jl -- handling variable bindings

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

(define-structure rep.vm.compiler.bindings

    (export lex-bindings spec-bindings
	    lexically-pure
	    call-with-frame
	    spec-bound?
	    has-local-binding?
	    tag-binding binding-tagged?
	    note-binding
	    note-bindings
	    emit-binding emit-varset emit-varref
	    note-binding-modified
	    binding-modified?
	    binding-enclosed?
	    note-binding-referenced
	    binding-referenced?
	    binding-tail-call-only?
	    note-closure-made
	    allocate-bindings)

    (open rep
	  rep.vm.compiler.utils
	  rep.vm.compiler.lap
	  rep.vm.compiler.basic)

  (define spec-bindings (make-fluid '()))	;list of bound variables
  (define lex-bindings (make-fluid '()))	;alist of bound variables
  (define lexically-pure (make-fluid t))	;any dynamic state?

  (define (spec-bound? var)
    (or (memq var (fluid-ref defvars))
	(special-variable? var)
	(memq var (fluid-ref spec-bindings))))

  (define (lexical-binding var) (assq var (fluid-ref lex-bindings)))

  (define (lexically-bound? var)
    (let ((cell (lexical-binding var)))
      (and cell (not (cell-tagged? 'no-location cell)))))

  (define (has-local-binding? var)
    (or (memq var (fluid-ref spec-bindings))
	(lexical-binding var)))

  (define (cell-tagged? tag cell) (memq tag (cdr cell)))
  (define (tag-cell tag cell)
    (unless (cell-tagged? tag cell)
      (set-cdr! cell (cons tag (cdr cell)))))

  ;; note that the outermost binding of symbol VAR has state TAG
  (define (tag-binding var tag)
    (let ((cell (lexical-binding var)))
      (when cell
	(tag-cell tag cell))))

  ;; note that the outermost binding of symbol VAR has state TAG
  (define (untag-binding var tag)
    (let ((cell (lexical-binding var)))
      (when cell
	(when (cell-tagged? tag cell)
	  (set-cdr! cell (delq! tag (cdr cell)))))))

  ;; return t if outermost binding of symbol VAR has state TAG
  (define (binding-tagged? var tag)
    (let ((cell (lexical-binding var)))
      (and cell (cell-tagged? tag cell))))

  ;; install a new binding contour, such that THUNK can add any bindings
  ;; (lexical and special), then when THUNK exits, the bindings are removed
  (define (call-with-frame thunk)
    (let ((old-d (list-length (fluid-ref lex-bindings))))
      (let-fluids ((spec-bindings (fluid-ref spec-bindings))
		   (lexically-pure (fluid-ref lexically-pure)))
	(prog1 (thunk)
	  ;; check for unused variables
	  (do ((new-d (list-length (fluid-ref lex-bindings)) (1- new-d))
	       (new (fluid-ref lex-bindings) (cdr new)))
	      ((= new-d old-d)
	       (fluid-set! lex-bindings new))
	    (unless (or (cell-tagged? 'referenced (car new))
			(cell-tagged? 'no-location (car new))
			(cell-tagged? 'maybe-unused (car new)))
	      (compiler-warning
	       'unused "unused variable `%s'" (caar new))))))))

  ;; note that symbol VAR has been bound
  (define (note-binding var #!optional without-location)
    (if (spec-bound? var)
	(progn
	  ;; specially bound (dynamic scope)
	  (fluid-set! spec-bindings (cons var (fluid-ref spec-bindings)))
	  (fluid-set! lexically-pure nil))
      ;; assume it's lexically bound otherwise
      (fluid-set! lex-bindings (cons (list var) (fluid-ref lex-bindings)))
      (when without-location
	(tag-binding var 'no-location)))
    ;; XXX handled by `modified' tag?
;    (when (eq? var (fluid-ref lambda-name))
;      (fluid-set! lambda-name nil))
)

  (defmacro note-bindings (vars)
    (list 'for-each 'note-binding vars))

  ;; note that the outermost binding of VAR has been modified
  (define (note-binding-modified var)
    (let ((cell (lexical-binding var)))
      (when cell
	(tag-cell 'modified cell))))

  (define (binding-modified? var)
    (binding-tagged? var 'modified))

  (define (binding-enclosed? var)
    (binding-tagged? var 'enclosed))

  (define (note-binding-referenced var #!optional for-tail-call)
    (tag-binding var 'referenced)
    (unless for-tail-call
      (tag-binding var 'not-tail-call-only)))

  (define (binding-referenced? var)
    (binding-tagged? var 'referenced))

  (define (binding-tail-call-only? var)
    (not (binding-tagged? var 'not-tail-call-only)))

  ;; note that all current lexical bindings have been enclosed
  (define (note-closure-made)
    (for-each (lambda (cell)
		(tag-cell 'enclosed cell)) (fluid-ref lex-bindings)))

  (define (emit-binding var)
    (if (spec-bound? var)
	(progn
	  (emit-insn `(push ,var))
	  (increment-stack)
	  (emit-insn '(spec-bind))
	  (decrement-stack))
      (emit-insn `(lex-bind ,var ,(fluid-ref lex-bindings)))))

  (define (emit-varset sym)
    (test-variable-ref sym)
    (cond ((spec-bound? sym)
	   (emit-insn `(push ,sym))
	   (increment-stack)
	   (emit-insn '(%set))
	   (decrement-stack))
	  ((lexically-bound? sym)
	    ;; The lexical address is known. Use it to avoid scanning
	   (emit-insn `(lex-set ,sym ,(fluid-ref lex-bindings))))
	  (t
	   ;; No lexical binding, but not special either. Just
	   ;; update the global value
	   (emit-insn `(setq ,sym)))))

  (define (emit-varref form #!optional in-tail-slot)
    (cond ((spec-bound? form)
	   ;; Specially bound
	   (emit-insn `(push ,form))
	   (increment-stack)
	   (emit-insn '(ref))
	   (decrement-stack))
	  ((lexically-bound? form)
	    ;; We know the lexical address, so use it
	   (emit-insn `(lex-ref ,form ,(fluid-ref lex-bindings)))
	   (note-binding-referenced form in-tail-slot))
	  (t
	   ;; It's not bound, so just update the global value
	   (emit-insn `(refq ,form)))))


;; allocation of bindings, either on stack or in heap

  (define (heap-binding? cell)
    (or (cell-tagged? 'captured cell)
	;; used to tag bindings unconditionally on the heap
	(cell-tagged? 'heap-allocated cell)))

  ;; heap addresses count up from the _most_ recent binding
  (define (heap-address var bindings)
    (let loop ((rest bindings)
	       (i 0))
      (cond ((null? rest) (error "No heap address for %s" var))
	    ((or (not (heap-binding? (car rest)))
		 (cell-tagged? 'no-location (car rest)))
	     (loop (cdr rest) i))
	    ((eq? (caar rest) var) i)
	    (t (loop (cdr rest) (1+ i))))))

  ;; register addresses count up from the _least_ recent binding
  (define (register-address var bindings base)
    (let loop ((rest bindings))
      (cond ((eq? rest base)
	     (error "No register address for %s, %s" var bindings))
	    ((eq? (caar rest) var)
	     (let loop-2 ((rest (cdr rest))
			  (i 0))
	       (cond ((eq? rest base) i)
		     ((or (heap-binding? (car rest))
			  (cell-tagged? 'no-location (car rest)))
		      (loop-2 (cdr rest) i))
		     (t (loop-2 (cdr rest) (1+ i))))))
	    (t (loop (cdr rest))))))

  (define (identify-captured-bindings asm lex-env)
    (for-each (lambda (insn)
		(case (car insn)
		  ((lex-ref lex-set)
		   (let ((cell (assq (list-ref insn 1) lex-env)))
		     (when cell
		       (tag-cell 'captured cell))))
		  ((push-bytecode)
		   (identify-captured-bindings
		    (list-ref insn 1) (list-ref insn 2)))))
	      (assembly-code asm)))

  ;; Extra pass over the output pseudo-assembly code; converts
  ;; pseudo-instructions accessing lexical bindings into real
  ;; instructions accessing either the heap or the registers
  (define (allocate-bindings-1 asm base-env)
    (let ((max-register 0))
      (let loop ((rest (assembly-code asm)))
	(when rest
	  (case (caar rest)
	    ((lex-bind lex-ref lex-set)
	     (let* ((var (list-ref (car rest) 1))
		    (bindings (list-ref (car rest) 2))
		    (cell (assq var bindings)))
	       (if (heap-binding? cell)
		   (set-car! rest (case (caar rest)
				  ((lex-bind) (list 'bind))
				  ((lex-ref)
				   (list 'env-ref (heap-address var bindings)))
				  ((lex-set)
				   (list 'env-set (heap-address var bindings)))))
		 (let ((register (register-address var bindings base-env)))
		   (set! max-register (max max-register (1+ register)))
		   (set-car! rest (case (caar rest)
				  ((lex-bind lex-set)
				   (list 'reg-set register))
				  ((lex-ref)
				   (list 'reg-ref register))))))))
	    ((push-bytecode)
	     (let ((asm (list-ref (car rest) 1))
		   (env (list-ref (car rest) 2))
		   (doc (list-ref (car rest) 3))
		   (interactive (list-ref (car rest) 4)))
	       (allocate-bindings-1 asm env)
	       (set-car! rest (list 'push (assemble-assembly-to-subr
					 asm doc interactive))))))
	  (loop (cdr rest))))
      (assembly-registers-set asm max-register)
      asm))

  (define (allocate-bindings asm)
    (identify-captured-bindings asm (fluid-ref lex-bindings))
    (allocate-bindings-1 asm (fluid-ref lex-bindings)))


;; declarations

  ;; (declare (bound VARIABLE))

  (define (declare-bound form)
    (let loop ((vars (cdr form)))
      (when vars
	(note-binding (car vars) t)
	(loop (cdr vars)))))
  (put 'bound 'compiler-decl-fun declare-bound)

  ;; (declare (special VARIABLE))

  (define (declare-special form)
    (let loop ((vars (cdr form)))
      (when vars
	(fluid-set! spec-bindings (cons (car vars) (fluid-ref spec-bindings)))
	(loop (cdr vars)))))
  (put 'special 'compiler-decl-fun declare-special)

  ;; (declare (heap-allocated VARS...))

  (define (declare-heap-allocated form)
    (let loop ((vars (cdr form)))
      (when vars
	(tag-binding (car vars) 'heap-allocated)
	(loop (cdr vars)))))
  (put 'heap-allocated 'compiler-decl-fun declare-heap-allocated)

  (define (declare-unused form)
    (let loop ((vars (cdr form)))
      (when vars
	(tag-binding (car vars) 'maybe-unused)
	(loop (cdr vars)))))
  (put 'unused 'compiler-decl-fun declare-unused))
