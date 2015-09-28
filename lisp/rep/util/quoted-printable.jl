#| quoted-printable.jl -- quoted-printable encoder/decoder

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

(define-module rep.util.quoted-printable

    (export encode-quoted-printable
	    decode-quoted-printable)

    (open rep)

  ;; INPUT and OUTPUT are any type of stream

  (define (encode-quoted-printable input output)
    (let ((col 0))
      ;; FIXME: trailing TAB and SPC chars should be encoded..
      (let loop ()
	(let ((char (read-char input)))
	  (when char
	    (when (>= col 76)
	      (write output "=\n")
	      (set! col 0))
	    (cond ((or (and (>= char 33) (<= char 60))
		       (and (>= char 62) (<= char 126))
		       (= char #\space)
		       (= char #\tab))
		   ;; null encoding
		   (set! col (1+ col))
		   (write output char))
		  ((= char #\newline)
		   (set! col 0)
		   (write output char))
		  (t
		   ;; Encode using =HH format
		   (format output "=%02X" char)
		   (set! col (+ col 3))))
	    (loop))))))

  (define (decode-char c)
    (let ((C (char-upcase c)))
      (if (and (>= C #\0) (<= C #\9))
	  (- C #\0)
	(+ (- C #\A) 10))))

  (define (decode-quoted-printable input output)
    (let loop ()
      (let ((c (read-char input)))
	(when c
	  (if (/= c #\=)
	      (write output c)
	    (let ((c1 (read-char input)))
	      (or c1 (error "Malformed quoted-printable data"))
	      (unless (= c1 #\newline)
		(let ((c2 (read-char input)))
		  (or c2 (error "Malformed quoted-printable data"))
		  (write output (logior (ash (decode-char c1) 4)
					(decode-char c2)))))))
	  (loop))))))
