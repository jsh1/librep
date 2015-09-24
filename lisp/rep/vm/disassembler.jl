;; disassembler.jl -- Disassembles compiled Lisp functions

;; $Id$

;; Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of Jade.

;; Jade is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Jade is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Jade; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(define-structure rep.vm.disassembler

    (export disassemble
	    disassemble-1)

    (open rep
	  rep.vm.bytecodes)

  (define-structure-alias disassembler rep.vm.disassembler)

  ;; Lookup table of strings naming instructions
  (define disassembler-opcodes
   [ "reg-ref" nil nil nil nil nil nil nil	; #x00
     "call" nil nil nil nil nil nil nil
     "push" nil nil nil nil nil nil nil	; #x10
     "refq" nil nil nil nil nil nil nil
     "setq" nil nil nil nil nil nil nil	; #x20
     "env-set" nil nil nil nil nil nil nil
     "reg-set" nil nil nil nil nil nil nil	; #x30
     "env-ref" nil nil nil nil nil nil nil
     "ref" "%set" "fluid-ref" "enclose"
     "push-frame" "pop-frame" "dup" "swap"	; #x40
     "pop" "push ()" "push t" "cons"
     "car" "cdr" "set-car!" "set-cdr!"
     "list-ref" "list-tail" "array-set!" "array-ref"
     "length" "bind" "add" "neg" "sub"	; #x50
     "mul" "div" "rem" "lnot" "not" "lor" "land"
     "equal" "eq" "structure-ref" "list-length"
     "gt" "ge" "lt" "le"		; #x60
     "inc" "dec" "ash" "zero?" "null" "atom?" "pair?" "list?"
     "number?" "string?" "vector?" "catch"
     "throw" "binderr" "return" "pop-frames"	; #x70
     "variable-bound?" "symbol?" "get" "put"
     "errorpro" "signal" "quotient" "reverse"
     "nreverse" "assoc" "assq" "rassoc"
     "rassq" "last" "mapcar" "mapc"	; #x80
     "member" "memq" "delete!" "delq!"
     "delete-if!" "delete-if-not!" "copy-sequence" "sequence?"
     "function?" "special-form?" "subr?" "eql"
     "lxor" "max" "min" "filter"	; #x90
     "macro?" "bytecode?" "push 0" "push 1"
     "push 2" "push -1" "push -2" "push %d"
     "push %d" "push %d" "caar" "cadr"
     "cdar" "cddr" "caddr" "cadddr"	; #xa0
     "caddddr" "cadddddr" "caddddddr" "cadddddddr"
     "floor" "ceiling" "truncate" "round"
     "apply" "array-length" "vector-length" "exp"
     "log" "sin" "cos" "tan"		; #xb0
     "sqrt" "expt" "swap2" "modulo"
     "make-closure" "reset-frames" "closure?" "pop-all"
     "fluid-set!" "fluid-bind" "memv" "num-eq"
     nil nil "%define" "spec-bind"	; #xc0
     "variable-set!" "required-arg" "optional-arg" "rest-arg"
     "not-zero?" "keyword-arg" "optional-arg*" "keyword-arg*"
     "vector-ref" "vector-set!" "string-length"
     "string-ref" "string-set!" "undefined" nil nil	; #xd0
     nil nil nil nil nil nil nil nil
     nil nil nil nil nil nil nil nil	; #xe0
     nil nil nil nil nil nil nil nil
     nil nil nil nil nil nil nil nil	; #xf0
     "ejmp @%d" "jpn @%d" "jpt @%d" "jmp @%d"
     "jn @%d" "jt @%d" "jnp @%d" "jtp @%d" ])

  (define (disassemble-1 code-string consts stream #!optional depth)
    (define (const-ref i)
      (if (< i (vector-length consts))
	  (vector-ref consts i)
	'*invalid-constant*))
    (define (code-ref i)
      (byte-string-ref code-string i))
    (unless depth (set! depth 0))
    (let
	((i 0)
	 (indent (make-string depth))
	 c arg op)
      (while (< i (byte-string-length code-string))
	(set! c (code-ref i))
	(format stream "\n%s%d\t" indent i)
	(cond
	 ((< c (bytecode last-with-args))
	  (set! op (logand c #xf8))
	  (cond
	   ((< (logand c #x07) 6)
	    (set! arg (logand c #x07)))
	   ((= (logand c #x07) 6)
	    (set! i (1+ i))
	    (set! arg (code-ref i)))
	   (t
	    (set! arg (logior (ash (code-ref (1+ i)) 8)
			      (code-ref (+ i 2))))
	    (set! i (+ i 2))))
	  (cond
	   ((= op (bytecode call))
	    (format stream "call #%d" arg))
	   ((= op (bytecode push))
	    (let
		((argobj (const-ref arg)))
	      (if (or (and (pair? argobj) (eq? (car argobj) 'byte-code))
		      (bytecode? argobj))
		  (progn
		    (format stream "push [%d] bytecode...\n" arg)
		    (disassemble argobj stream (1+ depth)))
		(format stream "push [%d] %S" arg (const-ref arg)))))
	   ((= op (bytecode bind))
	    (format stream "bind [%d] %S" arg (const-ref arg)))
	   ((= op (bytecode env-ref))
	    (format stream "env-ref #%d" arg))
	   ((= op (bytecode env-set))
	    (format stream "env-set #%d" arg))
	   ((= op (bytecode reg-ref))
	    (format stream "reg-ref #%d" arg))
	   ((= op (bytecode reg-set))
	    (format stream "reg-set #%d" arg))
	   ((= op (bytecode refq))
	    (format stream "refq [%d] %S" arg (const-ref arg)))
	   ((= op (bytecode set!))
	    (format stream "set! [%d] %S" arg (const-ref arg)))))
	 ((> c (bytecode last-before-jmps))
	  (set! arg (logior (ash (code-ref (1+ i)) 8)
			    (code-ref (+ i 2))))
	  (set! op c)
	  (set! i (+ i 2))
	  (format stream (vector-ref disassembler-opcodes op) arg))
	 ((= c (bytecode pushi))
	  (set! arg (code-ref (1+ i)))
	  (set! i (1+ i))
	  (when (>= arg 128)
	    (set! arg (- (- 256 arg))))
	  (format stream (vector-ref disassembler-opcodes c) arg))
	 ((or (= c (bytecode pushi-pair-neg))
	      (= c (bytecode pushi-pair-pos)))
	  (set! arg (logior (ash (code-ref (1+ i)) 8)
			    (code-ref (+ i 2))))
	  (set! i (+ i 2))
	  (when (= c (bytecode pushi-pair-neg))
	    (set! arg (- arg)))
	  (format stream (vector-ref disassembler-opcodes c) arg))
	 (t
	  (set! op (vector-ref disassembler-opcodes c))
	  (if op
	      (write stream op)
	    (format stream "<unknown opcode %d>" c))))
	(set! i (1+ i)))
      (write stream #\newline)))

  ;;;###autoload
  (defun disassemble (arg #!optional stream depth)
    "Dissasembles ARG, with output to STREAM, or the *disassembly* buffer."
    (interactive "aFunction to disassemble:")
    (let
	(code-string consts stack
	 (*print-escape* t))
      (unless stream
	(if (feature? 'jade)
	    (progn
	      (declare (bound open-buffer clear-buffer goto-other-view
			      goto-buffer insert start-of-buffer goto))
	      (set! stream (open-buffer "*disassembly*"))
	      (clear-buffer stream)
	      (goto-other-view)
	      (goto-buffer stream)
	      (insert "\n" stream)
	      (goto (start-of-buffer))
	      (set! stream (cons stream t)))
	  (set! stream *standard-output*)))
      (unless depth
	(set! depth 0))
      (when (zero? depth)
	(if (symbol? arg)
	    (progn
	      (format stream "Disassembly of function %s:\n\n" arg)
	      (set! arg (variable-ref arg)))
	  (format stream "Disassembly of %S:\n\n" arg)))
      (when (closure? arg)
	(set! arg (closure-function arg)))
      (cond
       ((and (pair? arg) (eq? (car arg) 'run-byte-code))
	(set! code-string (list-ref arg 1))
	(set! consts (list-ref arg 2))
	(set! stack (list-ref arg 3)))
       (t
	;; bytecode vector
	(set! code-string (vector-ref arg 0))
	(set! consts (vector-ref arg 1))
	(when (zero? depth)
	  (let ((spec (and (> (vector-length arg) 4) (vector-ref arg 4)))
		(doc (and (> (vector-length arg) 3) (vector-ref arg 3))))
	    (when spec
	      (format stream "Interactive spec: %S\n" spec))
	    (when doc
	      (format stream "Doc string: %S\n" doc)))
	  (set! stack (vector-ref arg 2)))))
      (when (zero? depth)
	(format stream "%d bytes, %d constants, %d stack slots, %d binding frames and %d registers.\n"
		(byte-string-length code-string) (vector-length consts)
		(logand stack #x3ff) (logand (ash stack -10) #x3ff)
		(ash stack -20)))
      (disassemble-1 code-string consts stream depth))))
