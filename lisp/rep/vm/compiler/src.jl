#| src.jl -- source code program transforms

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

(define-structure rep.vm.compiler.src

    (export coalesce-constants
	    source-code-transform)

    (open rep
	  rep.vm.compiler.utils
	  rep.vm.compiler.modules
	  rep.vm.compiler.lap
	  rep.vm.compiler.bindings
	  rep.vm.bytecodes)

;;; Constant folding

  (defun foldable? (name)
    (unless (has-local-binding? name)
      (let ((fun (get-procedure-handler name 'compiler-foldable?)))
	(and fun (fun name)))))

  (defun quote-constant (value)
    (if (or (symbol? value) (pair? value))
	(list 'quote value)
      value))

  ;; This assumes that FORM is a list, and its car is one of the functions
  ;; in the comp-constant-functions list
  (defun fold-constants (form)
    (let loop ((rest (cdr form))
	       (args '()))
      (if (null? rest)
	  ;; Now we have ARGS, the constant [folded] arguments from FORM
	  (quote-constant
	   (apply (compiler-variable-ref (car form)) (reverse! args)))
	(let ((arg (car rest)))
	  (when (pair? arg)
	    (set! arg (compiler-macroexpand arg)))
	  (when (and (pair? arg) (foldable? (car arg)))
	    (set! arg (fold-constants arg)))
	  (if (compiler-constant? arg)
	      (loop (cdr rest) (cons (compiler-constant-value arg) args))
	    ;; Not a constant, exit unchanged
	    form)))))

  (defun coalesce-constants (folder forms)
    (when forms
      (let loop ((result '())
		 (first (car forms))
		 (rest (cdr forms)))
	(cond ((null? rest) (reverse! (cons first result)))
	      ((and (compiler-constant? first)
		    rest (compiler-constant? (car rest)))
	       (loop result
		     (quote-constant
		      (folder (compiler-constant-value first)
			      (compiler-constant-value (car rest))))
		     (cdr rest)))
	      (t (loop (cons first result) (car rest) (cdr rest)))))))

;;; Entry point

  (defun source-code-transform (form)
    ;; first try constant folding
    (when (and (pair? form) (foldable? (car form)))
      (set! form (fold-constants form)))
	 
    ;; then look for a specific tranformer
    (let ((transformer (and (symbol? (car form))
			    (get-procedure-handler
			     (car form) 'compiler-transform-property))))
      (when transformer
	(set! form (transformer form))))

    form))
