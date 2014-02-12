#| heap.jl -- binary heap functions, and heap-sort

   $Id$

   Copyright (C) 2014 John Harper <jsh@unfactored.org>

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

;; FIXME: would also add heap-insert and heap-remove functions,
;; except vectors are not resizable.

(define-structure rep.data.heap

    (export heapify
	    heapsort)

    (open rep)

  (defsubst aswap (vec i j)
    (aset vec i (prog1 (aref vec j)
		  (aset vec j (aref vec i)))))

  (defun sift-down (vec less i size)
    (let loop-1 ((i i))
      (let ((child (1- (* (1+ i) 2))))
	(let loop-2 ((ci child)
		     (min-i i))
	  (cond ((and (< ci (+ child 2)) (< ci size))
		 (loop-2 (1+ ci)
			 (if (less (aref vec ci) (aref vec min-i)) ci min-i)))
		((/= min-i i)
		  (aswap vec i min-i)
		  (loop-1 min-i)))))))

  (defun sift-up (vec less i)
    (let loop ((i i))
      (let ((pi (1- (quotient (1+ i) 2))))
	(when (less (aref vec i) (aref vec pi))
	  (aswap vec i pi)
	  (loop pi)))))

  (defun heapify (vec #!key (less <))
    "Order the contents of VEC as a binary minimum heap, with (LESS a b)
defining the comparison function."
    (let loop ((i (1- (quotient (length vec) 2))))
      (when (>= i 0)
	(sift-down vec less i (length vec))
	(loop (1- i)))))

  (defun heapsort (vec #!key (less <))
    "Sort the contents of VEC using the heap-sort algorithm, using (LESS a b)
as the comparison function."
    (let ((more (lambda (x y)
		  (not (less x y)))))
      (heapify vec #:less more)
      (let loop ((i (1- (length vec))))
	(when (> i 0)
	  (aswap vec 0 i)
	  (sift-down vec more 0 i)
	  (loop (1- i)))))))
