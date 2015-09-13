#| xgettext.jl -- helper functions for writing xgettext programs

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

(define-structure rep.i18n.xgettext

    (export current-file current-module
	    set-included-definers set-helper
	    register scan scan-list scan-file
	    output-c-file output-pot-file)

    (open rep
	  rep.io.files
	  rep.regexp
	  rep.system)

  (define current-file (make-fluid))
  (define current-module (make-fluid))

  (define found-strings (make-fluid))

  (define included-definers (make-fluid t))
  (define helper (make-fluid))

  (define (set-included-definers lst) (fluid-set included-definers lst))
  (define (set-helper h) (fluid-set helper h))

  (define (register string)
    (let ((cell (assoc string (fluid found-strings))))
      (if cell
	  (unless (member (fluid current-file) (cdr cell))
	    (set-cdr! cell (cons (fluid current-file) (cdr cell))))
	(fluid-set found-strings (cons (list string (fluid current-file))
				       (fluid found-strings))))))

  (define (includedp name)
    (or (eq? (fluid included-definers) t)
	(memq name (fluid included-definers))))

  (define (scan form)

    (if (and (pair? form) (eq? (car form) '_) (string? (list-ref form 1)))
	(register (list-ref form 1))

      (when (and (car form) (macro? (car form)))
	(set! form (macroexpand form)))

      (when (pair? form)
	(case (car form)
	  ((quote))

	  ((set! setq-default %define)
	   (do ((tem (cdr form) (cddr tem)))
	       ((null? (cdr tem)))
	     (scan (cadr tem))))

	  ((let let* letrec let-fluids)
	   (set! form (cdr form))
	   (when (symbol? (car form))
	     (set! form (cdr form)))
	   (let loop ((vars (car form)))
	     (when vars
	       (scan-list (cdar vars))
	       (loop (cdr vars))))
	   (scan-list (cdr form)))

	  ((function) (scan (cdr form)))

	  ((cond)
	   (for-each (lambda (f)
		       (scan-list f)) (cdr form)))

	  ((lambda) (scan-list (cddr form)))

	  ((defun defmacro defsubst defvar defconst)
	   (when (includedp (car form))
	     (let ((doc (list-ref form 3)))
	       (when (string? doc)
		 (register doc))))
	   (if (memq (car form) '(defun defmacro defsubst))
	       (scan-list (list-tail form 3))
	     (scan-list (list-tail form 2))))

	  ((define-structure)
	   (let-fluids ((current-module (list-ref form 1)))
	     (scan-list (list-tail form 4))))

	  ((structure)
	   (scan-list (list-tail form 3)))

	  (t (if (fluid helper)
		 ((fluid helper) form)
	       (scan-list form)))))))

  (define (scan-list body)
    (for-each scan body))

  (define (scan-file filename)
    (let ((file (open-file filename 'read)))
      (when file
	(unwind-protect
	    (condition-case nil
		(let-fluids ((current-file filename))
		  (while t
		    (let ((form (read file)))
		      (scan form))))
	      (end-of-stream))
	  (close-file file)))))

  (defun output-strings (c-mode)
    (for-each
     (lambda (x)
       (let ((string (car x))
	     (files (cdr x)))
	 (for-each (lambda (f)
		     (format *standard-output* "%s %s %s\n"
			     (if c-mode "  /*" "#:")
			     f (if c-mode "*/" ""))) files)
	 (let* ((*print-escape* 'newlines)
		(out (format nil "%S" string))
		(point 0))
	   (if c-mode
	       (format *standard-output* "  _(%s);\n\n" out)
	     (while (and (< point (string-length out))
			 (string-match "\\\\n" out point))
	       (set! out (concat (substring out 0 (match-start)) "\\n\"\n\""
				 (substring out (match-end))))
	       (set! point (+ (match-end) 3)))
	     (format *standard-output* "msgid %s\nmsgstr \"\"\n\n" out)))))
     (reverse! (fluid found-strings))))

  (define (output-c-file)
    (write *standard-output* "\
/* SOME DESCRIPTIVE TITLE */
/* This file is intended to be parsed by xgettext.
 * It is not intended to be compiled.
 */

#if 0
void some_function_name() {\n\n")
    (output-strings t)
    (write *standard-output* "\
}
#endif\n"))

  (define (output-pot-file)
    (format *standard-output* "\
# SOME DESCRIPTIVE TITLE.
# Copyright (C) YEAR Free Software Foundation, Inc.
# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.
#
#, fuzzy
msgid \"\"
msgstr \"\"
\"Project-Id-Version: PACKAGE VERSION\\n\"
\"POT-Creation-Date: %s\\n\"
\"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\n\"
\"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n\"
\"Language-Team: LANGUAGE <LL@li.org>\\n\"
\"MIME-Version: 1.0\\n\"
\"Content-Type: text/plain; charset=CHARSET\\n\"
\"Content-Transfer-Encoding: ENCODING\\n\"\n\n"
	  (current-time-string nil "%Y-%m-%d %H:%M%z"))
    (output-strings nil)))
