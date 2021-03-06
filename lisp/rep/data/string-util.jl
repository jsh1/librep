;; string-util.jl -- some more string functions
;; $Id$

;;  Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of librep.

;; librep is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; librep is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with librep; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(declare (in-module rep.data))

;;;###autoload
(defun string-upper-case? (x)
  "Return t if string X is upper case (contains no lower case characters and
at least one upper-case character)."
  (let iter ((point 0)
	     (seen-upper nil))
    (if (>= point (string-length x))
	seen-upper
      (let ((char (string-ref x point)))
	(if (char-lower-case? char)
	    nil
	  (iter (1+ point) (or seen-upper (char-upper-case? char))))))))

;;;###autoload
(defun string-lower-case? (x)
  "Return t if string X is lower case (contains no upper case characters and
at least one lower-case character)."
  (let iter ((point 0)
	     (seen-lower nil))
    (if (>= point (string-length x))
	seen-lower
      (let ((char (string-ref x point)))
	(if (char-upper-case? char)
	    nil
	  (iter (1+ point) (or seen-lower (char-lower-case? char))))))))

;;;###autoload
(defun string-capitalized? (x)
  "Returns t if string X is capitalized (first character is upper case)."
  (and (> (string-length x) 0) (char-upper-case? (string-ref x 0))))

;;;###autoload
(defun string-upcase (x)
  "Return a new string, an upper case copy of string X."
  (let ((s (copy-sequence x)))
    (translate-byte-string! s upcase-table)
    s))

;;;###autoload
(defun string-downcase (x)
  "Return a new string, a lower case copy of string X."
  (let ((s (copy-sequence x)))
    (translate-byte-string! s downcase-table)
    s))

;;;###autoload
(defun capitalize-string (x)
  "Return a new string, a copy of X with its first character in upper case."
  (if (zero? (string-length x))
      x
    (let ((s (copy-sequence x)))
      (string-set! s 0 (char-upcase (string-ref s 0)))
      s)))

;;;###autoload
(defun mapconcat (fun sequence separator)
  "Call FUN for each member of SEQUENCE, concatenating the results. Between
each pair of results, insert SEPARATOR. Return the resulting string."
  (cond ((null? sequence) "")
	((pair? sequence)
	 ;; avoid O(n) operations on lists
	 (let loop ((rest (cdr sequence))
		    (frags (list (fun (car sequence)))))
	   (if (null? rest)
	       (apply concat (reverse! frags))
	     (loop (cdr rest)
		   (cons (fun (car rest)) (cons separator frags))))))
	(t ;; use general sequence operations
	 (let ((len (length sequence)))
	   (if (= len 0)
	       ""
	     (let loop ((i 1)
			(frags (list (fun (elt sequence 0)))))
	       (if (= i len)
		   (apply concat (reverse! frags))
		 (loop (1+ i) (cons (fun (elt sequence i))
				    (cons separator frags))))))))))
