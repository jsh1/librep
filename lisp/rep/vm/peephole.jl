#| peephole.jl -- peephole optimizer for rep assembly code

   $Id$

   Copyright (C) 1999, 2000 John Harper <john@dcs.warwick.ac.uk>

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
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;; Most of the optimisation patterns in the peephole optimiser were
;; lifted from jwz's byte-optimize.el (XEmacs)

(define-module rep.vm.peephole

    (export peephole-optimizer)

    (open rep
	  rep.vm.bytecodes)

  ;; todo:

  ;; c{dd..d}r; car --> ca{dd..d}r
  ;; c{dd..d}r; cdr --> cd{dd..d}r

  ;; shift the instruction window
  (defmacro shift ()
    '(progn
       (set! point (cdr point))
       (set! insn0 insn1)
       (set! insn1 insn2)
       (set! insn2 (list-ref point 3))))

  ;; refill the window
  (defmacro refill ()
    '(progn
       (set! insn0 (list-ref point 1))
       (set! insn1 (list-ref point 2))
       (set! insn2 (list-ref point 3))))

  ;; delete the first instruction in the window
  (defmacro del-0 ()
    '(progn
       (set-cdr! point (list-tail point 2))
       (set! insn0 insn1)
       (set! insn1 insn2)
       (set! insn2 (list-ref point 3))))

  ;; delete the second instruction in the window
  (defmacro del-1 ()
    '(progn
       (set-cdr! (cdr point) (list-tail point 3))
       (set! insn1 insn2)
       (set! insn2 (list-ref point 3))))

  ;; delete the third instruction in the window
  (defmacro del-2 ()
    '(progn
       (set-cdr! (list-tail point 2) (list-tail point 4))
       (set! insn2 (list-ref point 3))))

  ;; delete the first two instructions in the window
  (defmacro del-0-1 ()
    '(progn
       (set-cdr! point (list-tail point 3))
       (set! insn0 insn2)
       (set! insn1 (list-ref point 2))
       (set! insn2 (list-ref point 3))))

  ;; delete the second two instructions in the window
  (defmacro del-1-2 ()
    '(progn
       (set-cdr! (cdr point) (list-tail point 4))
       (set! insn1 (list-ref point 2))
       (set! insn2 (list-ref point 3))))

  ;; delete all instructions in the window
  (defmacro del-0-1-2 ()
    '(progn
       (set-cdr! point (list-tail point 4))
       (refill)))

  ;; debugging
  (defmacro before ()
    `(format *standard-error* "before: [%S %S %S]\n"
	     (list-ref point 1) (list-ref point 2) (list-ref point 3)))
  (defmacro after ()
    `(format *standard-error* "after: [%S %S %S]\n"
	     (list-ref point 1) (list-ref point 2) (list-ref point 3)))

  ;; set! returns #undefined, and need the value
  (defmacro set-tem! (value)
    `(progn
       (set! tem ,value)
       tem))

  ;; run the optimiser over CODE-STRING, modifying and returning it
  ;; returns (CODE . EXTRA-STACK)
  (defun peephole-optimizer (code-string)
    (let ((keep-going t)
	  (extra-stack 0)
	  point insn0 insn1 insn2 tem)
      ;; add an extra cons cell so we can always refer to the
      ;; cdr of the intsruction _before_ insn0, this makes it
      ;; easy to delete instructions
      (set! code-string (cons 'start code-string))
      (while keep-going
	(set! keep-going nil)
	(set! point code-string)
	(refill)
	(while insn0
	  ;;(format *standard-error* "iter: %S\n\n" code-string)
	  (cond
	   ;; <side-effect-free w/ stack+1>; pop --> <deleted>
	   ;; <side-effect-free w/ stack+0>; pop --> pop
	   ;; <side-effect-free w/ stack-1>; pop --> pop; pop
	   ((and (eq? (car insn1) 'pop)
		 (memq (car insn0) byte-side-effect-free-insns))
	    (set-tem! (vector-ref byte-insn-stack-delta (bytecode-ref (car insn0))))
	    (cond ((= tem 1)
		   (del-0-1)
		   (set! keep-going t))
		  ((= tem 0)
		   (del-0)
		   (set! keep-going t))
		  ((= tem -1)
		   (set-car! insn0 'pop)
		   (set-cdr! insn0 nil)
		   (set! keep-going t))))

	   ;; {push,dup}; env-set #X; env-ref #X
	   ;;    --> {push,dup}; env-set #X; {push, dup}
	   ;; {push,dup}; bind X; env-ref #0
	   ;;    --> {push,dup}; bind X; {push, dup}
	   ;; {push,dup}; reg-set #X; reg-ref #X
	   ;;    --> {push,dup}; reg-set #X; {push, dup}
	   ((and (or (and (eq? (car insn1) 'env-set)
			  (eq? (car insn2) 'env-ref)
			  (eq? (cadr insn1) (cadr insn2)))
		     (and (eq? (car insn1) 'bind)
			  (eq? (car insn2) 'env-ref)
			  (eq? (cadr insn2) 0))
		     (and (eq? (car insn1) 'reg-set)
			  (eq? (car insn2) 'reg-ref)
			  (eq? (cadr insn1) (cadr insn2))))
		 (or (eq? (car insn0) 'dup)
		     (eq? (car insn0) 'push)))
	    (set-car! insn2 (car insn0))
	    (set-cdr! insn2 (cdr insn0))
	    (set! keep-going t))

	   ;; env-set #X; env-ref #X --> dup; env-set #X
	   ;; bind; env-ref #0 --> dup; bind
	   ;; reg-set #X; reg-ref #X --> dup; reg-set #X
	   ((or (and (eq? (car insn0) 'env-set)
		     (eq? (car insn1) 'env-ref)
		     (eq? (cadr insn0) (cadr insn1)))
		(and (eq? (car insn0) 'bind)
		     (eq? (car insn1) 'env-ref)
		     (eqv? (cadr insn1) 0))
		(and (eq? (car insn0) 'reg-set)
		     (eq? (car insn1) 'reg-ref)
		     (eq? (cadr insn0) (cadr insn1))))
	    (set-car! insn1 (car insn0))
	    (set-cdr! insn1 (cdr insn0))
	    (set-car! insn0 'dup)
	    (set-cdr! insn0 nil)
	    ;; this might require extra stack space
	    (set! extra-stack 1)
	    (set! keep-going t))

	   ;; dup; {<varset>,<varbind>} X; pop --> {<varset>,<varbind>} X
	   ((and (eq? (car insn0) 'dup)
		 (or (memq (car insn1) byte-varset-insns)
		     (memq (car insn1) byte-varbind-insns))
		 (eq? (car insn2) 'pop))
	    (set-car! insn2 (car insn1))
	    (set-cdr! insn2 (cdr insn1))
	    (del-0-1)
	    (set! keep-going t))

	   ;; <varref> X; <varref> X --> <varref> X; dup
	   ((and (memq (car insn0) byte-varref-insns)
		 (eq? (car insn1) (car insn0))
		 (eq? (cadr insn0) (cadr insn1)))
	    (set-car! insn1 'dup)
	    (set-cdr! insn1 nil)
	    (set! keep-going t))

	   ;; <varref> X; <varset> X --> deleted
	   ((or (and (eq? (car insn0) 'env-ref)
		     (eq? (car insn1) 'env-set)
		     (eqv? (cadr insn0) (cadr insn1)))
		(and (eq? (car insn0) 'refq)
		     (eq? (car insn1) 'setq)
		     (eq? (cadr insn0) (cadr insn1)))
		(and (eq? (car insn0) 'reg-ref)
		     (eq? (car insn1) 'reg-set)
		     (eq? (cadr insn0) (cadr insn1))))
	    (del-0-1)
	    (set! keep-going t))

	   ;; c?r; c?r --> c??r
	   ((and (memq (car insn0) '(car cdr))
		 (memq (car insn1) '(car cdr)))
	    (set-car! insn1 (if (eq? (car insn0) 'car)
			      (if (eq? (car insn1) 'car) 'caar 'cdar)
			    (if (eq? (car insn1) 'car) 'cadr 'cddr)))
	    (del-0)
	    (set! keep-going t))

	   ;; push 1; sub --> dec
	   ;; push -1; sub --> inc
	   ;; push 1; add --> inc
	   ;; push -1; add --> dec
	   ;; [ XXX these and more should be handled at a higher level ]
	   ((and (eq? (car insn0) 'push)
		 (memq (car insn1) '(sub add))
		 (memv (cadr insn0) '(1 -1)))
	    (let ((new (if (eqv? (cadr insn0) 1)
			   (if (eq? (car insn1) 'sub) 'dec 'inc)
			 (if (eq? (car insn1) 'sub) 'inc 'dec))))
	      (set-car! insn1 new)
	      (del-0)
	      (set! keep-going t)))

	   ;; push 0; {add,sub} --> <deleted>
	   ((and (equal? insn0 '(push 0)) (memq (car insn1) '(add sub)))
	    (del-0-1)
	    (set! keep-going t))

	   ;; push 0; num-eq --> zero?
	   ((and (equal? insn0 '(push 0)) (eq? (car insn1) 'num-eq))
	    (set-car! insn1 'zero?)
	    (del-0)
	    (set! keep-going t))

	   ;; zero?; null? --> not-zero?
	   ((and (eq? (car insn0) 'zero?) (eq? (car insn1) 'null?))
	    (set-car! insn1 'not-zero?)
	    (del-0)
	    (set! keep-going t))

	   ;; jmp X; X: --> X:
	   ((and (eq? (car insn0) 'jmp) (eq? (cadr insn0) insn1))
	    (del-0)
	    (set! keep-going t))

	   ;; {jn,jt} X; X: --> pop; X:
	   ((and (memq (car insn0) '(jn jt)) (eq? (cadr insn0) insn1))
	    (set-car! insn0 'pop)
	    (set-cdr! insn0 nil)
	    (set! keep-going t))

	   ;; {jpt,jpn} X; pop --> {jt,jn} X
	   ((and (memq (car insn0) '(jpt jpn)) (eq? (car insn1) 'pop))
	    (set-car! insn0 (if (eq? (car insn0) 'jpt) 'jt 'jn))
	    (del-1)
	    (set! keep-going t))

	   ;; null?; {jn,jt} X --> {jt,jn} X
	   ((and (eq? (car insn0) 'null?)
		 (memq (car insn1) '(jn jt)))
	    (set-car! insn1 (if (eq? (car insn1) 'jn) 'jt 'jn))
	    (del-0)
	    (set! keep-going t))

	   ;; jt X; (push ()) --> jpt X
	   ((and (eq? (car insn0) 'jt) (equal? insn1 '(push ())))
	    (set-car! insn0 'jpt)
	    (del-1)
	    (set! keep-going t))

	   ;; {jn,jt} X; jmp Y; X: --> {jt,jn} Y; X:
	   ((and (memq (car insn0) '(jn jt))
		 (eq? (car insn1) 'jmp)
		 (eq? (cadr insn0) insn2))
	    (set-car! insn1 (if (eq? (car insn0) 'jn) 'jt 'jn))
	    (del-0)
	    (set! keep-going t))

	   ;; (push X); <cond. jump> X; --> whatever
	   ((and (eq? (car insn0) 'push)
		 (memq (car insn1) byte-conditional-jmp-insns))
	    (let*
		;; only way to get a nil constant is through `(push ())'
		((is-nil (equal? insn0 '(push ())))
		 (is-t (not is-nil)))
	      (cond ((or (and is-nil (eq? (car insn1) 'jn))
			 (and is-t (eq? (car insn1) 'jt))
			 (and is-nil (eq? (car insn1) 'jpn))
			 (and is-t (eq? (car insn1) 'jpt)))
		     ;; nil; jn X --> jmp X
		     ;; t; jt X --> jmp X
		     ;; nil; jpn X --> jmp X
		     ;; t; jpt X --> jmp X
		     (set-car! insn1 'jmp)
		     (del-0))
		    ((or (and is-nil (eq? (car insn1) 'jt))
			 (and is-t (eq? (car insn1) 'jn))
			 (and is-t (eq? (car insn1) 'jnp))
			 (and is-nil (eq? (car insn1) 'jtp)))
		     ;; nil; jt X --> <deleted>
		     ;; t; jn X --> <deleted>
		     ;; t; jnp X --> <deleted>
		     ;; nil; jtp X --> <deleted>
		     (del-0-1))
		    ((or (and is-nil (eq? (car insn1) 'jnp))
			 (and is-t (eq? (car insn1) 'jtp)))
		     ;; nil; jnp X --> nil; jmp X
		     ;; t; jtp X --> t; jmp X
		     (set-car! insn1 'jmp))
		    ((or (and is-t (eq? (car insn1) 'jpn))
			 (and is-nil (eq? (car insn1) 'jpt)))
		     ;; t; jpn X --> t
		     ;; nil; jpt X --> nil
		     (del-1))
		    (t (error "Unhandled contional jump case")))
	      (set! keep-going t)))

	   ;; <varref-and-error-free-op>; pop-frame --> pop-frame; op
	   ((and (eq? (car insn1) 'pop-frame)
		 (memq (car insn0) byte-varref-free-insns))
	    (let
		((op (car insn0))
		 (arg (cdr insn0)))
	      (set-car! insn0 (car insn1))
	      (set-cdr! insn0 (cdr insn1))
	      (set-car! insn1 op)
	      (set-cdr! insn1 arg)
	      (set! keep-going t)))

	   ;; <varbind> X; pop-frame --> pop; pop-frame
	   ((and (memq (car insn0) byte-varbind-insns)
		 (eq? (car insn1) 'pop-frame))
	    (set-car! insn0 'pop)
	    (set-cdr! insn0 nil)
	    (set! keep-going t))

	   ;; push-frame; pop-frame --> deleted
	   ((and (eq? (car insn0) 'push-frame) (eq? (car insn1) 'pop-frame))
	    (del-0-1)
	    (set! keep-going t))

	   ;; push-frame; {return,pop-frames,reset-frames}
	   ;;   --> {return,pop-frames,reset-frames}
	   ((and (eq? (car insn0) 'push-frame)
		 (memq (car insn1) '(return pop-frames reset-frames)))
	    (del-0)
	    (set! keep-going t))

	   ;; {pop-frame, pop-frames, reset-frames}; return --> return
	   ((and (eq? (car insn1) 'return)
		 (memq (car insn0) '(pop-frame pop-frames reset-frames)))
	    (del-0)
	    (set! keep-going t))

	   ;; pop; pop-all -> pop-all
	   ((and (eq? (car insn1) 'pop-all) (eq? (car insn0) 'pop))
	    (del-0)
	    (set! keep-going t))

	   ;; <pushes-undefined>; pop; undefined -> <pushes-undefined>
	   ((and (eq? (car insn1) 'pop)
		 (equal? insn2 '(push #undefined))
		 (memq (car insn0) byte-pushes-undefined-insns))
	    (del-1-2)
	    (set! keep-going t))

	   ;; <varref> X; dup... ; <varref> X --> <varref> X; dup...; dup
	   ((and (memq (car insn0) byte-varref-insns)
		 (eq? (car insn1) 'dup))
	    (let
		((tem (list-tail point 2)))
	      (while (eq? (car (car tem)) 'dup)
		(set! tem (cdr tem)))
	      (when (equal? (car tem) insn0)
		(set-car! (car tem) 'dup)
		(set-cdr! (car tem) nil)
		(set! keep-going t))))

	   ;; X: Y: --> X:  [s/X/Y/]
	   ((and (symbol? insn0) (symbol? insn1))
	    (let loop ((rest (cdr code-string)))
	      (when rest
		(when (and (eq? (cadar rest) insn1)
			   (or (memq (caar rest) byte-jmp-insns)
			       (eq? (caar rest) 'push-label)))
		  (set-car! (cdar rest) insn0))
		(loop (cdr rest))))
	    (del-1)
	    (set! keep-going t))

	   ;; [unused] X: --> deleted
	   ((and (symbol? insn0)
		 (let loop ((rest (cdr code-string)))
		   (cond ((null? rest) t)
			 ((and (eq? (cadar rest) insn0)
			       (or (memq (caar rest) byte-jmp-insns)
				   (eq? (caar rest) 'push-label))) nil)
			 (t (loop (cdr rest))))))
	    (del-0)
	    (set! keep-going t))

	   ;; jmp X; ... Y: --> jmp X; Y:
	   ;; return; ... Y: --> return; Y:
	   ((and (memq (car insn0) '(jmp ejmp return))
		 insn1 (not (symbol? insn1)))
	    (set! tem (list-tail point 2))
	    (while (and tem (not (symbol? (car tem))))
	      (set! tem (cdr tem)))
	    (unless (eq? tem (list-tail point 2))
	      (set-cdr! (cdr point) tem)
	      (refill)
	      (set! keep-going t)))

	   ;; j* X; ... X: jmp Y --> j* Y; ... X: jmp Y
	   ((and (memq (car insn0) byte-jmp-insns)
		 (set-tem! (or (memq (cadr insn0) (cdr code-string))
			       (error "Can't find jump destination: %s, %s"
				      insn0 (cdr code-string))))
		 (set-tem! (car (cdr tem)))
		 (eq? (car tem) 'jmp)
		 (not (eq? (cadr insn0) (cadr tem))))
	    (set-cdr! insn0 (cdr tem))
	    (set! keep-going t))

	   ;; jmp X; ... X: return --> return; ... X: return
	   ((and (eq? (car insn0) 'jmp)
		 (set-tem! (or (memq (cadr insn0) (cdr code-string))
			       (error "Can't find jump destination: %s, %s"
				      insn0 (cdr code-string))))
		 (set-tem! (car (cdr tem)))
		 (eq? (car tem) 'return))
	    (set-car! insn0 'return)
	    (set-cdr! insn0 nil)
	    (set! keep-going t))

	   ;; {jnp,jtp} X; ... X: <cond. jmp> Y --> whatever
	   ((and (memq (car insn0) '(jnp jtp))
		 (set-tem! (cdr (or (memq (cadr insn0) (cdr code-string))
				    (error
				     "Can't find jump destination: %s, %s"
				     insn0 (cdr code-string)))))
		 (car tem)
		 (memq (car (car tem)) byte-conditional-jmp-insns))
	    (let
		((jmp (car tem))
		 need-new-label)
	      (if (eq? (car insn0) 'jtp)
		  (cond
		   ((memq (car jmp) '(jpt jt))
		    ;; jtp X; ... X: jpt Y --> jt Y; ...
		    ;; jtp X; ... X: jt Y --> jt Y; ...
		    (set-car! insn0 'jt))
		   ((eq? (car jmp) 'jpn)
		    ;; jtp X; ... X: jpn Y --> jpt Z; ... X: jpn Y; Z:
		    (set-car! insn0 'jpt)
		    (set! need-new-label t))
		   ((memq (car jmp) '(jn jnp))
		    ;; jtp X; ... X: jn Y --> jt Z; ... X: jpn Y; Z:
		    ;; jtp X; ... X: jnp Y --> jt Z; ... X: jpn Y; Z:
		    (set-car! insn0 'jt)
		    (set! need-new-label t))
		   ((eq? (car jmp) 'jtp)
		    ;; jtp X; ... X: jtp Y --> jtp Y; ...
		    (set-car! insn0 'jtp)))
		(cond
		 ((eq? (car jmp) 'jpt)
		  ;; jnp X; ... X: jpt Y --> jn Z; ... X: jpt Y; Z:
		  (set-car! insn0 'jnp)
		  (set! need-new-label t))
		 ((memq (car jmp) '(jpn jn))
		  ;; jnp X; ... X: jpn Y --> jn Y ...
		  ;; jnp X; ... X: jn Y --> jn Y ...
		  (set-car! insn0 'jn))
		 ((memq (car jmp) '(jt jtp))
		  ;; jnp X; ... X: jt Y --> jn Z; ... X: jt Y; Z:
		  ;; jnp X; ... X: jtp Y --> jn Z; ... X: jt Y; Z:
		  (set-car! insn0 'jn)
		  (set! need-new-label t))
		 ((eq? (car jmp) 'jnp)
		  ;; jnp X; ... X: jnp Y --> jnp Y ...
		  (set-car! insn0 'jnp))))
	      (if (not need-new-label)
		  (set-car! (cdr insn0) (cadr jmp))
		;; add label `Z:' following the second jump
		(let ((label (cons (gensym) (cdr tem))))
		  (set-car! (cdr insn0) (car label))
		  (set-cdr! tem label)))
	      (set! keep-going t)))

	   ;; {jpt,jpn} X; jmp Y; X: --> {jnp,jtp} Y; X:
	   ;; {jtp,jnp} X; jmp Y; X: --> {jpn,jpt} Y; X:
	   ((and (eq? (car insn1) 'jmp)
		 (memq (car insn0) '(jpt jpn jtp jnp))
		 (eq? (cadr insn0) insn2))
	    (set-car! insn1 (case (car insn0)
			    ((jpt) 'jnp)
			    ((jpn) 'jtp)
			    ((jtp) 'jpn)
			    ((jnp) 'jpt)))
	    (del-0)
	    (set! keep-going t))

	   ;; <const>; jmp X; ... X: <cond. jmp> Y --> whatever
	   ;;
	   ;; [ this should be handled already, by (1) changing the
	   ;;   first jump, then by (2) dereferencing the constant ]

	   ;; jmp X: Y: ... X: <cond. jmp> Y --> ???

	   )
	  ;; shift in the next instruction
	  (shift)))

      ;; now do one last pass, looking for simple things
      (set! point code-string)
      (refill)
      (while insn0
	(cond
	 ;; push X; {<varset>,<varbind>} Y; push X
	 ;;   --> push X; dup; {<varset>,<varbind>} Y
	 ((and (eq? (car insn0) 'push)
	       (or (memq (car insn1) byte-varset-insns)
		   (memq (car insn1) byte-varbind-insns))
	       (equal? insn0 insn2))
	  (set-car! insn2 (car insn1))
	  (set-cdr! insn2 (cdr insn1))
	  (set-car! insn1 'dup)
	  (set-cdr! insn1 nil)
	  (set! extra-stack 1)
	  (set! keep-going t))

	 ;; push X; {dup,push X}... --> push X; dup...
	 ;; <varref> X; {dup,<varref> X}... --> <varref> X; dup...
	 ((or (eq? (car insn0) 'push)
	      (memq (car insn0) byte-varref-insns))
	  (set! tem (list-tail point 2))
	  (while (or (eq? (caar tem) 'dup)
		     (equal? (car tem) insn0))
	    (set-car! (car tem) 'dup)
	    (set-cdr! (car tem) nil)
	    (set! tem (cdr tem)))))
	(shift))

      ;; drop the extra cons we added
      (cons (cdr code-string) extra-stack))))
