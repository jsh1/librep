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

(define-module rep.data.heap

    (export vector-heapify!
	    vector-sort!
	    make-heap
	    heap?
	    heap-size
	    heap-data
	    heap-add
	    heap-remove)

    (open rep
	  rep.data.records
	  rep.test.framework)

  (defsubst vector-swap! (vec i j)
    (vector-set! vec i (prog1 (vector-ref vec j)
			 (vector-set! vec j (vector-ref vec i)))))

  (define (sift-down vec less i size)
    (let loop-1 ((i i))
      (let ((child (1- (* (1+ i) 2))))
	(let loop-2 ((ci child)
		     (min-i i))
	  (cond ((and (< ci (+ child 2)) (< ci size))
		 (loop-2 (1+ ci)
			 (if (less (vector-ref vec ci) (vector-ref vec min-i))
			     ci
			   min-i)))
		((/= min-i i)
		  (vector-swap! vec i min-i)
		  (loop-1 min-i)))))))

  (define (sift-up vec less i)
    (let loop ((i i))
      (let ((pi (1- (quotient (1+ i) 2))))
	(when (and (>= pi 0) (less (vector-ref vec i) (vector-ref vec pi)))
	  (vector-swap! vec i pi)
	  (loop pi)))))

  (define (vector-heapify! vec #!key (less <))
    "Order the contents of VEC as a binary minimum heap, with (LESS a b)
defining the comparison function."
    (let loop ((i (1- (quotient (vector-length vec) 2))))
      (when (>= i 0)
	(sift-down vec less i (vector-length vec))
	(loop (1- i)))))

  (define (vector-sort! vec #!key (less <))
    "Sort the contents of VEC using the heap-sort algorithm, using (LESS a b)
as the comparison function."
    (let ((more (lambda (x y)
		  (not (less x y)))))
      (vector-heapify! vec #:less more)
      (let loop ((i (1- (vector-length vec))))
	(when (> i 0)
	  (vector-swap! vec 0 i)
	  (sift-down vec more 0 i)
	  (loop (1- i))))))

  ;; resizable heap data structure, using the above functions

  (define-record-type :heap
    (heap size data less)
    heap?
    (size heap-size heap-size-set!)
    (data heap-data heap-data-set!)
    (less heap-less))

  (define (make-heap #!optional (less <))
    (heap 0 nil less))

  (define (heap-resize heap delta)
    (let* ((size (+ (heap-size heap) delta))
	   (old-vec (heap-data heap))
	   (old-size (if old-vec (vector-length old-vec) 0)))
      (when (< old-size size)
	(let* ((new-size (max (* old-size 2) 16))
	       (new-vec (make-vector new-size)))
	  (do ((i 0 (1+ i)))
	      ((= i old-size))
	    (vector-set! new-vec i (vector-ref old-vec i)))
	  (heap-data-set! heap new-vec)))))

  (define (heap-add heap value)
    (heap-resize heap 1)
    (let ((i (heap-size heap)))
      (heap-size-set! heap (1+ (heap-size heap)))
      (vector-set! (heap-data heap) i value)
      (sift-up (heap-data heap) (heap-less heap) i)))

  (define (heap-remove heap)
    (if (= (heap-size heap) 0)
	nil
      (let ((vec (heap-data heap))
	    (i (1- (heap-size heap))))
	(prog1 (vector-ref vec 0)
	  (vector-set! vec 0 (vector-ref vec i))
	  (vector-set! vec i nil)
	  (heap-size-set! heap i)
	  (sift-down (heap-data heap) (heap-less heap) 0 (heap-size heap))
	  (heap-resize heap -1)))))

;;; tests

  ;;###autoload
  (define-self-test 'rep.data.heap
    (lambda ()
      (let ((heap (make-heap))
	    (data '(10 3 1 6 9 8 51 4)))

	(test (heap? heap))
	(test (= (heap-size heap) 0))

	(for-each (lambda (x) (heap-add heap x)) data)
	(test (= (heap-size heap) (list-length data)))

	(for-each (lambda (x)
		    (test (= (heap-remove heap) x)))
		  (sort data))

	(test (= (heap-size heap) 0))))))
