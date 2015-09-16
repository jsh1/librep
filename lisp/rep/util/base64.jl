#| base64.jl -- base64 encoder/decoder

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

(define-structure rep.util.base64

    (export encode-base64
	    decode-base64)

    (open rep)

  ;; INPUT and OUTPUT are any type of stream

  (defconst mime-base64-alphabet
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

  (define (encode-base64 input output)
    (let ((col 0)
	  reg reg1 reg2 reg3)
      (catch 'done
	(while t
	  (set! reg1 (read-byte input))
	  (set! reg2 (read-byte input))
	  (set! reg3 (read-byte input))
	  (cond
	   ((and reg1 reg2 reg3)
	    ;; Got our 24 bits, split into four 6 bit quantities
	    (progn
	      (set! reg (logior (ash reg1 16) (ash reg2 8) reg3))
	      (write output (string-ref mime-base64-alphabet (ash reg -18)))
	      (write output (string-ref mime-base64-alphabet
					(logand (ash reg -12) #o77)))
	      (write output (string-ref mime-base64-alphabet
					(logand (ash reg -6) #o77)))
	      (write output (string-ref mime-base64-alphabet
					(logand reg #o77)))
	      (set! col (+ col 4))
	      (when (>= col 76)
		(write output #\newline)
		(set! col 0))))
	   (reg2
	    ;; 16 bits read, shift in 2 zeros
	    (set! reg (ash (logior (ash reg1 8) reg2) 2))
	    (write output (string-ref mime-base64-alphabet (ash reg -12)))
	    (write output (string-ref mime-base64-alphabet
				(logand (ash reg -6) #o77)))
	    (write output (string-ref mime-base64-alphabet (logand reg #o77)))
	    (write output #\=)
	    (throw 'done t))
	   (reg1
	    ;; eight bits read, shift in 4 zeros
	    (set! reg (ash reg1 4))
	    (write output (string-ref mime-base64-alphabet (ash reg -6)))
	    (write output (string-ref mime-base64-alphabet (logand reg #o77)))
	    (write output #\=)
	    (write output #\=)
	    (throw 'done t))
	   (t
	    ;; 0 bits read
	    (throw 'done t)))))
      (write output #\newline)))

  (define (decode-base64 input output)
    (let ((reg 0)
	  (bits 0)
	  char)
      (let loop ()
	(set! char (read-char input))
	(when char
	  (cond
	   ((and (char>=? char #\A) (char<=? char #\Z))
	    (set! char (- (char->integer char) (char->integer #\A))))
	   ((and (char>=? char #\a) (char<=? char #\z))
	    (set! char (+ 26 (- (char->integer char) (char->integer #\a)))))
	   ((and (char>=? char #\0) (char<=? char #\9))
	    (set! char (+ 52 (- (char->integer char) (char->integer #\0)))))
	   ((char=? char #\+)
	    (set! char 62))
	   ((char=? char #\/)
	    (set! char 63))
	   (t (set! char nil)))
	  (when char
	    (set! reg (logior (ash reg 6) char))
	    (set! bits (+ bits 6)))
	  (while (>= bits 8)
	    (set! char (ash reg (- 8 bits)))
	    (set! reg (logxor reg (ash char (- bits 8))))
	    (set! bits (- bits 8))
	    (write output char))
	  (loop))))))
