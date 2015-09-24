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

    (export call-with-frame
	    save-current-frame
	    reload-current-frame
	    lexically-pure?
	    call-with-dynamic-binding
	    create-binding
	    spec-bound?
	    has-local-binding?
	    binding-modified?
	    set-binding-referenced!
	    set-binding-has-lambda-record!
	    binding-has-lambda-record?
	    set-binding-heap-allocated!
	    set-binding-maybe-unused!
	    binding-call-only?
	    binding-tail-call-only?
	    binding-captured?
	    emit-binding
	    emit-varset
	    emit-varref
	    emit-push-frame
	    emit-pop-frame
	    allocate-bindings
	    bytecode-env)

    (open rep
	  rep.data.records
	  rep.vm.compiler.utils
	  rep.vm.compiler.lap
	  rep.vm.compiler.basic)

  (define-record-type :frame
    (make-frame special-env lexical-env closure-env dynamic-binding
		#!optional variable-frames)
    frame?
    (special-env special-env set-special-env!)
    (lexical-env lexical-env set-lexical-env!)
    (closure-env closure-env)
    (dynamic-binding dynamic-binding? set-dynamic-binding!)
    (variable-frames variable-frames set-variable-frames!))

  (define (copy-frame frame)
    (make-frame (special-env frame) (lexical-env frame)
		(closure-env frame) (dynamic-binding? frame)
		(variable-frames frame)))

  (define current-frame (make-fluid nil))

  ;; Install a new binding contour, such that THUNK can add any
  ;; bindings (lexical and special), then when THUNK exits, the
  ;; bindings are removed.

  (define (call-with-frame thunk #!key captures-bindings)
    (let* ((old-frame (fluid-ref current-frame))
	   (new-frame (if old-frame
			  (make-frame (special-env old-frame)
				      (lexical-env old-frame)
				      (if captures-bindings
					  (lexical-env old-frame)
					(closure-env old-frame))
				      (if captures-bindings
					  (dynamic-binding? old-frame)
					nil))
			(make-frame '() '() '() nil))))
      (let-fluids ((current-frame new-frame))
	(prog1 (thunk)
	  ;; check for unused variables
	  (let ((old-env (and old-frame (lexical-env old-frame))))
	    (do ((new-env (lexical-env new-frame) (cdr new-env)))
		((eq? new-env old-env))
	      (unless (or (cell-tagged? (car new-env) 'referenced)
			  (cell-tagged? (car new-env) 'no-location)
			  (cell-tagged? (car new-env) 'maybe-unused))
		(compiler-warning
		 'unused "unused variable `%s'" (caar new-env)))))))))

  (define (save-current-frame)
    (let ((frame (fluid-ref current-frame)))
      ;; none of the other fields are modified permanently
      (list (special-env frame)
	    (map copy-sequence (lexical-env frame))
	    (variable-frames frame))))

  (define (reload-current-frame list)
    (let ((frame (fluid-ref current-frame)))
      (set-special-env! frame (list-ref list 0))
      ;; restore lexical environment
      (let ((frame-lex (lexical-env frame))
	    (saved-lex (list-ref list 1)))
	(set! frame-lex (list-tail frame-lex (- (list-length frame-lex)
						(list-length saved-lex))))
	(for-each (lambda (frame-cell saved-cell)
		    (unless (eq? (car frame-cell) (car saved-cell))
		      (error "Mismatched bindings: %S %S"
			     frame-cell saved-cell))
		    (set-cdr! frame-cell (cdr saved-cell)))
		  frame-lex saved-lex)
	(set-lexical-env! frame frame-lex)
	(set-variable-frames! frame (list-ref list 2)))))

  (define (lexically-pure?)
    (let ((frame (fluid-ref current-frame)))
      (and (null? (special-env frame)) (not (dynamic-binding? frame)))))

  (define (call-with-dynamic-binding thunk)
    (let* ((frame (fluid-ref current-frame))
	   (old-value (dynamic-binding? frame)))
      (set-dynamic-binding! frame t)
      (prog1 (thunk)
	(set-dynamic-binding! frame old-value))))

  (define (cell-tagged? cell tag)
    (memq tag (cdr cell)))

  (define (tag-cell cell tag)
    (unless (cell-tagged? cell tag)
      (set-cdr! cell (cons tag (cdr cell)))))

  (define (untag-cell cell tag)
    (set-cdr! cell (delq! tag (cdr cell))))

  (define (cell-has-location? cell)
    (not (cell-tagged? cell 'no-location)))

  ;; note that symbol VAR has been bound

  (define (create-binding var #!key no-location)
    (let ((frame (fluid-ref current-frame)))
      (if (spec-bound? var)
	  ;; specially bound (dynamic scope)
	  (set-special-env! frame (cons var (special-env frame)))
	;; assume it's lexically bound otherwise
	(set-lexical-env! frame (cons (list var) (lexical-env frame)))
	(when no-location
	  (tag-binding! var '(no-location))))))

  ;; returns the binding cell (VAR . (TAGS...))

  (define (lexical-binding var)
    (assq var (lexical-env (fluid-ref current-frame))))

  ;; note that the outermost binding of symbol VAR has state TAG

  (define (tag-binding! var tags)
    (let ((cell (lexical-binding var)))
      (when cell
	(for-each (lambda (tag)
		    (tag-cell cell tag)) tags))))

  (define (untag-binding! var tags)
    (let ((cell (lexical-binding var)))
      (when cell
	(for-each (lambda (tag)
		    (untag-cell cell tag)) tags))))

  ;; return t if outermost binding of symbol VAR has state TAG

  (define (binding-tagged? var tag)
    (let ((cell (lexical-binding var)))
      (and cell (cell-tagged? cell tag))))

  (define (spec-bound? var)
    (or (memq var (fluid-ref defvars))
	(special-variable? var)
	(memq var (special-env (fluid-ref current-frame)))))

  (define (has-local-binding? var)
    (or (memq var (special-env (fluid-ref current-frame)))
	(lexical-binding var)))

  ;; note that the outermost binding of VAR has been modified

  (define (binding-modified? var)
    (binding-tagged? var 'modified))

  (define (set-binding-referenced! var #!key call tail-call)
    (let ((tags '(referenced)))
      (unless call
	(set! tags (cons 'not-call-only tags)))
      (unless tail-call
	(set! tags (cons 'not-tail-call-only tags)))
      (tag-binding! var tags)))

  (defun set-binding-has-lambda-record! (var)
    ;; It's not an error for no binding to be found, e.g. inlined
    ;; bodies of defsubst forms have no actual binding.
    (tag-binding! var '(lambda)))

  (defun binding-has-lambda-record? (var)
    (binding-tagged? var 'lambda))

  (define (set-binding-heap-allocated! var)
    (tag-binding! var '(heap-allocated)))

  (define (set-binding-maybe-unused! var)
    (tag-binding! var '(maybe-unused)))

  (define (binding-call-only? var)
    (not (binding-tagged? var 'not-call-only)))

  (define (binding-tail-call-only? var)
    (not (binding-tagged? var 'not-tail-call-only)))

  (define (capture-cell-if-necessary! cell)
    (when (memq cell (closure-env (fluid-ref current-frame)))
      ;; cell is the far side of the current closure, i.e. the binding
      ;; going to be captured by the closure.
      (tag-cell cell 'captured)))

  (define (binding-captured? var)
    (binding-tagged? var 'captured))

  (define (cell-heap-allocated? cell)
    (or (cell-tagged? cell 'captured)
	;; used to tag bindings unconditionally on the heap
	(cell-tagged? cell 'heap-allocated)))

  ;; Code generation

  (define (emit-binding var)
    (if (spec-bound? var)
	(progn
	  (emit-insn `(push ,var))
	  (increment-stack)
	  (emit-insn '(spec-bind))
	  (decrement-stack))
      (emit-insn `(lex-bind ,var ,(lexical-env (fluid-ref current-frame))))))

  (define (emit-varset sym)
    (check-variable-ref sym)
    (if (spec-bound? sym)
	(progn
	  (emit-insn `(push ,sym))
	  (increment-stack)
	  (emit-insn '(%set))
	  (decrement-stack))
      (let ((cell (lexical-binding sym)))
	(if (and cell (cell-has-location? cell))
	    (progn
	      ;; The lexical address is known. Use it to avoid scanning
	      (emit-insn
	       `(lex-set ,sym ,(lexical-env (fluid-ref current-frame))))
	      (capture-cell-if-necessary! cell)
	      (tag-cell cell '(modified)))
	  ;; No lexical binding, but not special either. Just
	  ;; update the global value
	  (emit-insn `(setq ,sym))))))

  (define (emit-varref form #!key for-call tail-call)
    (if (spec-bound? form)
	(progn
	  ;; Specially bound
	  (emit-insn `(push ,form))
	  (increment-stack)
	  (emit-insn '(ref))
	  (decrement-stack))
      (let ((cell (lexical-binding form)))
	(if (and cell (cell-has-location? cell))
	    (progn
	      ;; We know the lexical address, so use it
	      (emit-insn
	       `(lex-ref ,form ,(lexical-env (fluid-ref current-frame))))
	      (capture-cell-if-necessary! cell)
	      (set-binding-referenced! form #:call for-call
				       #:tail-call tail-call))
	  ;; It's not bound, so just update the global value
	  (emit-insn `(refq ,form))))))

  (define (emit-push-frame type #!key handler)
    (case type
      ((variable)
       ;; May be able to remove the push-frame instruction later,
       ;; when we know exactly what variables were bound. To do that
       ;; we need to stamp the push/pop instructions with the current
       ;; binding depth so that we can identify them (and any extra
       ;; pop instructions added by tailcalls) later to remove them.
       ;; Save a copy of the current frame so we can work out what was
       ;; bound.
       (let ((frame (fluid-ref current-frame)))
	 (set-variable-frames! frame (cons (cons (fluid-ref intermediate-code)
						 (copy-frame frame))
					   (variable-frames frame)))
	 (emit-insn `(push-frame ,(fluid-ref current-b-stack)))))
      ((fluid)
       (emit-insn '(push-frame)))
      ((exception)
       (or handler (error "No exception handler to bind"))
       (push-label-addr handler)
       (emit-insn '(binderr))
       (decrement-stack))
      (t (error "unspecified frame type")))
    (increment-b-stack))

  (define (emit-pop-frame type)
    (decrement-b-stack)
    (if (eq? type 'variable)
	(let* ((frame (fluid-ref current-frame))
	       (saved-code (caar (variable-frames frame)))
	       (saved-frame (cdar (variable-frames frame))))
	  (set-variable-frames! frame (cdr (variable-frames frame)))
	  (if (and (eq? (special-env frame) (special-env saved-frame))
		   (let loop ((rest (lexical-env frame)))
		     (cond ((eq? rest (lexical-env saved-frame)) t)
			   ((cell-heap-allocated? (car rest)) nil)
			   (t (loop (cdr rest))))))
	      ;; only lexical bindings, don't need push/pop-frame
	      (delete-binding-insns (fluid-ref current-b-stack) saved-code)
	    (emit-insn `(pop-frame ,(fluid-ref current-b-stack)))))
      (emit-insn `(pop-frame ,(fluid-ref current-b-stack)))))

  ;; Deletes all ({push,pop}-frame ID) instructions from the current
  ;; code list, upto the point START in the list.
  
  (define (delete-binding-insns id start)
    ;; use an extra pair to make it easy to delete as we go
    (let ((header (cons nil (fluid-ref intermediate-code))))
      (let loop ((rest header))
	(if (eq? (cdr rest) start)
	    (cdr header)
	  (let ((insn (cadr rest)))
	    (if (and (memq (car insn) '(push-frame pop-frame))
		     (= (cadr insn) id))
		(progn
		  (set-cdr! rest (cddr rest))
		  (loop rest))
	      (loop (cdr rest))))))))

  ;; Allocation of bindings, either on stack or in heap

  ;; heap addresses count up from the _most_ recent binding

  (define (heap-address var bindings)
    (let loop ((rest bindings)
	       (i 0))
      (cond ((null? rest) (error "No heap address for %s" var))
	    ((or (not (cell-heap-allocated? (car rest)))
		 (not (cell-has-location? (car rest))))
	     (loop (cdr rest) i))
	    ((eq? (caar rest) var) i)
	    (t (loop (cdr rest) (1+ i))))))

  ;; register addresses count up from the _least_ recent binding

  (define (register-address var bindings base-env)
    (let loop ((rest bindings))
      (cond ((eq? rest base-env)
	     (error "No register address for %s, %s" var bindings))
	    ((eq? (caar rest) var)
	     (let loop-2 ((rest (cdr rest))
			  (i 0))
	       (cond ((eq? rest base-env) i)
		     ((or (cell-heap-allocated? (car rest))
			  (not (cell-has-location? (car rest))))
		      (loop-2 (cdr rest) i))
		     (t (loop-2 (cdr rest) (1+ i))))))
	    (t (loop (cdr rest))))))

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
	       (if (cell-heap-allocated? cell)
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
		   (base-env (list-ref (car rest) 2))
		   (doc (list-ref (car rest) 3))
		   (interactive (list-ref (car rest) 4)))
	       (allocate-bindings-1 asm base-env)
	       (set-car! rest (list 'push (assemble-assembly-to-subr
					 asm doc interactive)))))

	    ;; remove the binding ids we may have inserted
	    ((push-frame pop-frame)
	     (set-cdr! (car rest) nil)))
	  (loop (cdr rest))))
      (assembly-registers-set asm max-register)
      asm))

  (define (allocate-bindings asm)
    ;; top-level functions don't have a containing frame.
    (let ((frame (fluid-ref current-frame)))
      (allocate-bindings-1 asm (if frame (lexical-env frame) nil))))

  ;; For calls to push-bytecode. Have to record the actual environment
  ;; here, rather than just the current frame, as we need to know the
  ;; state when the closure was created.

  (define (bytecode-env)
    (lexical-env (fluid-ref current-frame)))

  ;; (declare (bound VARIABLE))

  (define (declare-bound form)
    (let loop ((vars (cdr form)))
      (when vars
	(unless (has-local-binding? (car vars))
	  (create-binding (car vars) #:no-location t))
	(loop (cdr vars)))))
  (put 'bound 'compiler-decl-fun declare-bound)

  ;; (declare (special VARIABLE))

  (define (declare-special form)
    (let ((frame (fluid-ref current-frame)))
      (let loop ((vars (cdr form)))
	(when vars
	  (set-special-env! frame (cons (car vars) (special-env frame)))
	  (loop (cdr vars))))))
  (put 'special 'compiler-decl-fun declare-special)

  ;; (declare (heap-allocated VARS...))

  (define (declare-heap-allocated form)
    (let loop ((vars (cdr form)))
      (when vars
	(set-binding-heap-allocated! (car vars))
	(loop (cdr vars)))))
  (put 'heap-allocated 'compiler-decl-fun declare-heap-allocated)

  (define (declare-unused form)
    (let loop ((vars (cdr form)))
      (when vars
	(set-binding-maybe-unused! (car vars))
	(loop (cdr vars)))))
  (put 'unused 'compiler-decl-fun declare-unused))
