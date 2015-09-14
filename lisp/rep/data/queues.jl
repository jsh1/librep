#| queues.jl -- fifo queues

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
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(define-structure rep.data.queues

    (export make-queue
	    enqueue
	    dequeue
	    queue-empty?
	    queue?
	    queue->list
	    queue-length
	    delete-from-queue)

    (open rep
	  rep.data.datums
	  rep.test.framework)

  (define-structure-alias queues rep.data.queues)

  (define type-id (cons))

  (define-datum-printer type-id (lambda (q stream)
				  (declare (unused q))
				  (write stream "#<queue>")))

  ;; Each queue is (TAIL . HEAD). HEAD is the list of items, TAIL
  ;; points to the last cell in HEAD, or the empty list.

  (define (make-queue)
    (make-datum (cons) type-id))

  (define (enqueue q x)
    (let ((cell (datum-ref q type-id))
	  (new (list x)))
      (if (null? (cdr cell))
	  ;; empty queue
	  (progn
	    (set-cdr! cell new)
	    (set-car! cell new))
	;; tail pointer is set
	(set-cdr! (car cell) new)
	(set-car! cell new))))

  (define (dequeue q)
    (let ((cell (datum-ref q type-id)))
      (if (null? (cdr cell))
	  (error "Can't dequeue from empty queue")
	(prog1 (car (cdr cell))
	  (if (not (eq? (car cell) (cdr cell)))
	      ;; at least one element left
	      (set-cdr! cell (cdr (cdr cell)))
	    ;; queue needs to be empty now
	    (set-cdr! cell '())
	    (set-car! cell '()))))))

  (define (queue-empty? q)
    (null? (cdr (datum-ref q type-id))))

  (define (queue? q)
    (datum? q type-id))

  (define (queue->list q)
    (cdr (datum-ref q type-id)))

  (define (queue-length q)
    (list-length (queue->list q)))

  (define (delete-from-queue q x)
    (let ((cell (datum-ref q type-id)))
      (let loop ((ptr cell))
	(if (null? (cdr ptr))
	    ;; avoid pointing tail to itself..
	    (if (null? (cdr cell))
		(set-car! cell '())
	      (set-car! cell ptr))
	  (if (eq? (cadr ptr) x)
	      (progn
		(set-cdr! ptr (cddr ptr))
		(loop ptr))
	    (loop (cdr ptr)))))))

;;; tests

  ;;###autoload
  (define-self-test 'rep.data.queues
    (lambda ()
      (let ((queue (make-queue)))

	(test (queue? queue))
	(test (queue-empty? queue))
	(test (null? (queue->list queue)))
	(test (= (queue-length queue) 0))

	(enqueue queue 1)
	(test (not (queue-empty? queue)))
	(test (equal? (queue->list queue) '(1)))
	(test (= (queue-length queue) 1))

	(enqueue queue 2)
	(test (equal? (queue->list queue) '(1 2)))
	(test (= (queue-length queue) 2))

	(test (= (dequeue queue) 1))
	(test (equal? (queue->list queue) '(2)))
	(test (= (queue-length queue) 1))

	(enqueue queue 3)
	(enqueue queue 4)
	(enqueue queue 5)
	(test (equal? (queue->list queue) '(2 3 4 5)))

	(delete-from-queue queue 2)
	(test (equal? (queue->list queue) '(3 4 5)))

	(delete-from-queue queue 4)
	(test (equal? (queue->list queue) '(3 5)))

	(delete-from-queue queue 5)
	(test (equal? (queue->list queue) '(3)))

	(delete-from-queue queue 3)
	(test (= (queue-length queue) 0))
	(test (queue-empty? queue))))))
