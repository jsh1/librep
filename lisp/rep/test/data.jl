#| rep.test.data -- checks for rep.data module

   $Id$

   Copyright (C) 2001 John Harper <jsh@users.sourceforge.net>

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

(define-structure rep.data.self-tests ()

    (open rep
	  rep.data.records
	  rep.test.framework)

;;; equality function tests

  ;; adapted from guile's test.scm
  (define (equality-self-test)
    (define (gen-counter)
      (let ((n 0))
	(lambda () (set! n (1+ n)) n)))

    (test (eqv? 'a 'a))
    (test (not (eqv? 'a 'b)))
    (test (eqv? 2 2))
    (test (eqv? '() '()))
    (test (eqv? '10000 '10000))
    (test (not (eqv? (cons 1 2) (cons 1 2))))
    (test (not (eqv? (lambda () 1) (lambda () 2))))

    (let ((p (lambda (x) x)))
      (test (eqv? p p)))
    (let ((g (gen-counter)))
      (test (eqv? g g)))
    (test (not (eqv? (gen-counter) (gen-counter))))
    (letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
	     (g (lambda () (if (eqv? f g) 'g 'both))))
      (test (not (eqv? f g))))

    (test (eq? 'a 'a))
    (test (not (eq? (list 'a) (list 'a))))
    (test (eq? '() '()))
    (test (eq? car car))
    (let ((x '(a)))
      (test (eq? x x)))
    (let ((x '()))
      (test (eq? x x)))
    (let ((x (lambda (x) x)))
      (test (eq? x x)))

    (test (equal? 'a 'a))
    (test (equal? '(a) '(a)))
    (test (equal? '(a (b) c) '(a (b) c)))
    (test (equal? "abc" "abc"))
    (test (equal? 2 2))
    (test (equal? (make-vector 5 'a) (make-vector 5 'a))))

;;; cons and list tests

  ;; adapted from guile's test.scm
  (define (cons-self-test)
    (test (pair? '(a . b)))
    (test (pair? '(a . 1)))
    (test (pair? '(a b c)))
    (test (not (pair? '())))
    (test (not (pair? '#(a b))))

    (test (equal? '(a) (cons 'a '())))
    (test (equal? '((a) b c d) (cons '(a) '(b c d))))
    (test (equal? '("a" b c) (cons "a" '(b c))))
    (test (equal? '(a . 3) (cons 'a 3)))
    (test (equal? '((a b) . c) (cons '(a b) 'c)))

    (test (equal? 'a (car '(a b c))))
    (test (equal? '(a) (car '((a) b c d))))
    (test (equal? 1 (car '(1 . 2))))

    (test (equal? '(b c d) (cdr '((a) b c d))))
    (test (equal? 2 (cdr '(1 . 2))))

    (test (equal? '(a 7 c) (list 'a (+ 3 4) 'c)))
    (test (equal? '() (list)))

    (test (= 3 (list-length '(a b c))))
    (test (= 3 (list-length '(a (b) (c d e)))))
    (test (= 0 (list-length '())))

    (test (equal? '(x y) (append '(x) '(y))))
    (test (equal? '(a b c d) (append '(a) '(b c d))))
    (test (equal? '(a (b) (c)) (append '(a (b)) '((c)))))
    (test (equal? '() (append)))
    (test (equal? '(a b c . d) (append '(a b) '(c . d))))
    (test (equal? 'a (append '() 'a)))

    (test (equal? '(c b a) (reverse '(a b c))))
    (test (equal? '((e (f)) d (b c) a) (reverse '(a (b c) d (e (f))))))

    (test (equal? 'c (list-ref '(a b c d) 2)))
    (test (equal? '(c d) (list-tail '(a b c d) 2)))

    (test (equal? '(a b c) (memq 'a '(a b c))))
    (test (equal? '(b c) (memq 'b '(a b c))))
    (test (null? (memq 'a '(b c d))))
    (test (null? (memq (list 'a) '(b (a) c))))
    (test (equal? '((a) c) (member (list 'a) '(b (a) c))))
    (test (equal? '(101 102) (memv 101 '(100 101 102))))

    (let ((e '((a 1) (b 2) (c 3))))
      (test (equal? '(a 1) (assq 'a e)))
      (test (equal? '(b 2) (assq 'b e)))
      (test (null? (assq 'd e))))
    (test (null? (assq (list 'a) '(((a)) ((b)) ((c))))))
    (test (equal? '((a)) (assoc (list 'a) '(((a)) ((b)) ((c))))))
    (test (equal? '(5 7) (assq 5 '((2 3) (5 7) (11 13))))))

;;; tests for rep.data.records

  (define-record-type :pare
    (kons x y)
    pare?
    (x kar set-kar!)
    (y kdr))

  (define-record-discloser :pare
    (lambda (x) (format nil "#<pare %s %s>" (kar x) (kdr x))))

  (define (record-self-test)
    (define pare (kons 1 2))
    (test pare)
    (test (pare? pare))
    (test (eqv? (kar pare) 1))
    (test (eqv? (kdr pare) 2))

    (set-kar! pare 3)
    (test (eqv? (kar pare) 3)))

;;; a few UTF-8 tests

  (define (string-encoding-test)
    ;; "hello\x2603;world", U+2603 = SNOWMAN
    (define s1 "hello☃world")
    (define s2 (make-string 5 #\x2603))

    (test (= (char->integer #\x2603) #x2603))
    (test (= (integer->char #x2603) #\x2603))
    (test (char=? (integer->char (char->integer #\x2603)) #\x2603))
    (test (= (char->integer (integer->char #x2603)) #x2603))

    (test (string=? s1 "hello\x2603;world"))
    (test (string=? s1 "hello\342\230\203world"))
    (test (string=? s1 (concat "hello" #\x2603 "world")))
    (test (string=? s2 "\x2603;\x2603;\x2603;\x2603;\x2603;"))

    (test (= (string-length s1) 11))
    (test (= (byte-string-length s1) 13))
    (test (= (string-length s2) 5))
    (test (= (byte-string-length s2) 15))

    (test (eq? (string-ref s1 0) #\h))
    (test (= (char->integer (string-ref s1 0)) 104))
    (test (= (byte-string-ref s1 0) 104))
    (test (= (char->integer (string-ref s1 5)) #x2603))
    (test (= (byte-string-ref s1 5) #o342))
    (test (= (string-ref s2 0) #\x2603))

    (string-set! s2 2 #\a)
    (test (= (string-ref s2 2) #\a))
    (test (= (string-length s2) 5))
    (test (= (byte-string-length s2) 13))
    (string-set! s2 2 #\x2603)
    (test (= (string-ref s2 2) #\x2603))
    (test (= (string-length s2) 5))
    (test (= (byte-string-length s2) 15))
    (test (string=? s2 (make-string 5 #\x2603)))

    (let ((sub (substring s1 5 6)))
      (test (string=? sub (make-string 1 #\x2603)))
      (test (char=? (string-ref sub 0) #\x2603))
      (test (= (string-length sub) 1))
      (test (= (byte-string-length sub) 3)))

    (test (string=? (substring s2 2 4) "\x2603;\x2603;")))

;;; string-util tests

  (define (string-util-self-test)
    (test (string-upper-case? "FOO"))
    (test (not (string-upper-case? "Foo")))
    (test (not (string-upper-case? "foo")))
    (test (not (string-upper-case? "123")))

    (test (string-lower-case? "foo"))
    (test (not (string-lower-case? "Foo")))
    (test (not (string-lower-case? "FOO")))
    (test (not (string-lower-case? "123")))

    (test (string-capitalized? "Foo"))
    (test (string-capitalized? "FOO"))
    (test (not (string-capitalized? "foo")))

    (test (string=? (string-upcase "foo") "FOO"))
    (test (string=? (string-upcase "FOO") "FOO"))
    (test (string=? (string-upcase "Foo") "FOO"))
    (test (string=? (string-upcase "123") "123"))

    (test (string=? (string-downcase "FOO") "foo"))
    (test (string=? (string-downcase "foo") "foo"))
    (test (string=? (string-downcase "Foo") "foo"))
    (test (string=? (string-downcase "123") "123"))

    (test (string=? (capitalize-string "FOO") "FOO"))
    (test (string=? (capitalize-string "foo") "Foo"))
    (test (string=? (capitalize-string "Foo") "Foo"))
    (test (string=? (capitalize-string "123") "123"))

    (test (string=? (mapconcat identity '("foo" "bar" "baz") " ")
		   "foo bar baz"))
    (test (string=? (mapconcat identity '("foo" "bar" "baz") #\space)
		   "foo bar baz"))
    (test (string=? (mapconcat identity '() #\space) ""))
    (test (string=? (mapconcat string-upcase '("foo" "bar" "baz") " ")
		   "FOO BAR BAZ")))

  (define (self-test)
    (equality-self-test)
    (cons-self-test)
    (record-self-test)
    (string-encoding-test)
    (string-util-self-test))

  ;;###autoload
  (define-self-test 'rep.data self-test))
