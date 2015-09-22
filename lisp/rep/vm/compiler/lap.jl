#| lap.jl -- intermediate code management

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

(define-structure rep.vm.compiler.lap

    (export intermediate-code
	    emit-insn
	    make-label
	    push-label-addr
	    fix-label
	    prefix-label
	    push-state
	    pop-state
	    reload-state
	    saved-state)

    (open rep
	  rep.vm.compiler.utils
	  rep.vm.compiler.bindings)

  (define saved-state (make-fluid))

  ;; list of (INSN . [ARG]), (TAG . REFS)

  (define intermediate-code (make-fluid '()))

  ;; Output one instruction (OP [ARG]) or LABEL. Returns the added value.

  (define (emit-insn insn)
    (when (pair? insn)
      ;; so the peepholer can safely modify code
      (set! insn (copy-sequence insn)))
    (fluid-set! intermediate-code (cons insn (fluid-ref intermediate-code)))
    insn)

  ;; Create a new label

  (define make-label gensym)

  ;; Arrange for the address of LABEL to be pushed onto the stack

  (define (push-label-addr label)
    (emit-insn `(push-label ,label))
    (increment-stack))

  ;; Set the address of the label LABEL to the current pc

  (define fix-label emit-insn)

  (define (prefix-label label)
    (fluid-set! intermediate-code (append! (list label)
					(fluid-ref intermediate-code))))

  (define (push-state)
    (fluid-set! saved-state
	       (cons (list (cons 'frame (save-current-frame))
			   (cons intermediate-code
				 (fluid-ref intermediate-code))
			   (cons current-stack (fluid-ref current-stack))
			   (cons max-stack (fluid-ref max-stack))
			   (cons current-b-stack (fluid-ref current-b-stack))
			   (cons max-b-stack (fluid-ref max-b-stack)))
		     (fluid-ref saved-state))))

  (define (pop-state)
    (fluid-set! saved-state (cdr (fluid-ref saved-state))))

  (define (reload-state)
    (for-each (lambda (cell)
		(if (eq? (car cell) 'frame)
		    (reload-current-frame (cdr cell))
		  (fluid-set! (car cell) (cdr cell))))
	      (car (fluid-ref saved-state)))))
