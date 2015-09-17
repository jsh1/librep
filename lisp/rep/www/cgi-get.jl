;; cgi-get.jl -- return the parameters from a CGI GET request
;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

;; $Id$

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

(define-structure rep.www.cgi-get

    (export cgi-get-params)

    (open rep
	  rep.system
	  rep.regexp
	  rep.test.framework
	  rep.www.quote-url)

  (define-structure-alias cgi-get rep.www.cgi-get)

  (define unquote-plus-map (let ((map (make-string (1+ (char->integer #\+))))
				 (i 0)
				 (top (char->integer #\+)))
			     (while (< i top)
			       (byte-string-set! map i i)
			       (set! i (1+ i)))
			     (byte-string-set! map (char->integer #\+)
					       (char->integer #\space))
			     map))

  (defun cgi-get-params (#!optional query-string)
    (unless query-string
      (set! query-string (getenv "QUERY_STRING")))
    (let ((point 0)
	  (params nil)
	  name value)
      (while (string-looking-at "([^=]+)=([^&]*)(&|$)" query-string point)
	(set! point (match-end))
	(set! name (intern
		    (unquote
		     (substring query-string (match-start 1) (match-end 1)))))
	(set! value (unquote
		     (substring query-string (match-start 2) (match-end 2))))
	(when (string=? value "")
	  (set! value nil))
	(set! params (cons (cons name value) params)))
      (reverse! params)))

  (defun unquote (string)
    (let ((s (copy-sequence string)))
      (translate-byte-string! s unquote-plus-map)
      (unquote-url s)))


;; Tests

  (define (self-test)
    (test (equal? (cgi-get-params "")
		 '()))
    (test (equal? (cgi-get-params "foo=bar")
		 '((foo . "bar"))))
    (test (equal? (cgi-get-params "foo=bar&baz=quux")
		 '((foo . "bar") (baz . "quux"))))
    (test (equal? (cgi-get-params "foo=&baz=quux")
		 '((foo . ()) (baz . "quux"))))
    (test (equal? (cgi-get-params "foo=%3A%2F%3D")
		 '((foo . ":/="))))
    (test (equal? (cgi-get-params "foo=+bar+")
		 '((foo . " bar ")))))

  ;;###autoload
  (define-self-test 'rep.www.cgi-get self-test))
