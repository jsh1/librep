;;;; sort.jl -- Sorting functions
;;;  Copyright (C) 1998 John Harper <john@dcs.warwick.ac.uk>
;;;  $Id$

;;; This file is part of Jade.

;;; Jade is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; Jade is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with Jade; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(declare (in-module rep.data))

(open-structures '(rep.lang.math))

;;;###autoload
(defun sort! (lst #!optional (pred <))
  "Sort LST destructively, but stably, returning the sorted list.

If PRED is defined it is used to compare two objects, it should return t
when the first is `less' than the second. By default the standard less-than
function (`<') is used.

The fact that the sort is stable means that sort keys which are equal will
preserve their original position in relation to each other."
  (let ((len (list-length lst)))
  (if (< len 2)
      lst
    (let ((mid (list-tail lst (1- (quotient len 2)))))
      (set! mid (prog1
		    (cdr mid)
		  (set-cdr! mid nil)))
      ;; Now we have two separate lists, LST and MID; sort them..
      (set! lst (sort! lst pred))
      (set! mid (sort! mid pred))
      ;; ..then merge them back together
      (let ((out-head nil)		;Start of the list being built
	    (out nil)			;Cell whose cdr is next link
	    tem)
	;; While both lists have elements compare them
	(while (and lst mid)
	  (set! tem (if (pred (car mid) (car lst))
			(prog1
			    mid
			  (set! mid (cdr mid)))
		      (prog1
			  lst
			(set! lst (cdr lst)))))
	  (if out
	      (progn
		(set-cdr! out tem)
		(set! out tem))
	    (set! out-head tem)
	    (set! out tem)))
	;; If either has elements left just append them
	(when (or lst mid)
	  (if out
	      (set-cdr! out (or lst mid))
	    (set! out-head (or lst mid))))
	out-head)))))

;;;###autoload
(defun sort (lst #!optional (pred <))
  "Returns a sorted copy of LST."
  (sort! (copy-sequence lst) pred))
