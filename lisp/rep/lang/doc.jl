;;;; lisp-doc.jl -- Accessing LISP doc strings
;;;  Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>
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

(provide 'lisp-doc)

(defun apropos-output (symbols use-function)
  (let
      ((separator (make-string 72 ?-)))
    (mapc #'(lambda (sym)
	      (write standard-output separator)
	      (if use-function
		  (describe-function-1 sym)
		(describe-variable-1 sym))
	      (format standard-output "%s\n\n"
		      (or (documentation sym (not use-function))
			  "Undocumented"))) symbols)))

;;;###autoload
(defun apropos-function (regexp &optional all-functions)
  (format standard-output "Apropos %s `%s':\n\n"
	  (if all-functions "function" "command") regexp)
  (apropos-output (apropos regexp (if all-functions
				      'fboundp
				    'commandp)) t))

;;;###autoload
(defun apropos-variable (regexp)
  (format standard-output "Apropos variable `%s':\n" regexp)
  (apropos-output (apropos regexp 'boundp) nil))

(defun describe-function-1 (fun)
  (let*
      ((fval (symbol-function fun))
       (type (cond
	      ((special-form-p fval)
	       "Special Form")
	      ((macrop fval)
	       "Macro")
	      ((subrp fval)
	       "Built-in Function")
	      (t
	       "Function"))))
    ;; Check if it's been compiled.
    (when (or (bytecodep fval)
	      (and (consp fval) (assq 'jade-byte-code fval)))
      (setq type (concat "Compiled " type)))
    (format standard-output "\n%s: %s\n\n" type fun)
    (when (fboundp fun)
      (when (or (consp fval) (bytecodep fval))
	;; A Lisp function or macro, print its argument spec.
	(let
	    ((lambda-list (if (consp fval)
			      (nth (if (eq (car fval) 'macro) 2 1) fval)
			    (aref fval 0))))
	  (prin1 fun)
	  ;; Print the arg list (one at a time)
	  (while lambda-list
	    (let
		((arg-name (symbol-name (car lambda-list))))
	      ;; Unless the argument starts with a `&' print it in capitals
	      (unless (= (aref arg-name 0) ?&)
		(setq arg-name (translate-string (copy-sequence arg-name)
						 upcase-table)))
	      (format standard-output " %s" arg-name))
	    (setq lambda-list (cdr lambda-list)))
	  (format standard-output "\n\n"))))))
  
;;;###autoload
(defun describe-function (fun)
  "Display the documentation of a function, macro or special-form."
  (let
      ((doc (documentation fun)))
    (describe-function-1 fun)
    (write standard-output (or doc "Undocumented."))
    (write standard-output "\n")))

(defun describe-variable-1 (var)
  (format standard-output
	  "\n%s: %s\nCurrent value: %S\n\n"
	  (if (const-variable-p var) "Constant" "Variable")
	  (symbol-name var) (symbol-value var t)))

;;;###autoload
(defun describe-variable (var)
  (let
      ((doc (documentation var t))
       (old-buf (current-buffer)))
    (describe-variable-1 var old-buf)
    (format standard-output "%s\n" (or doc "Undocumented."))))


;; Accessing doc strings

;;;###autoload
(defun documentation (symbol &optional is-variable)
  "Returns the documentation-string for SYMBOL. If IS-VARIABLE is t the
documentation for the variable stored in SYMBOL is returned, else
the function doc is provided."
  (catch 'exit
    (let
	(doc key dbm) 
    ;; First check for in-core documentation
    (if is-variable
	(when (setq doc (get symbol 'variable-documentation))
	  (throw 'exit doc))
      ;; a function
      (setq doc (symbol-function symbol))
      (when (consp doc)
	(if (eq 'macro (car doc))
	    (setq doc (nth 3 doc))
	  (setq doc (nth 2 doc)))
	(when (stringp doc)
	  (throw 'exit doc))))
    ;; Then for doc strings in the databases
    (require 'sdbm)
    (setq key (concat (if is-variable ?V ?S) (symbol-name symbol)))
    (mapc #'(lambda (file)
	      (setq dbm (dbm-open file 'read))
	      (when dbm
		(unwind-protect
		    (setq doc (dbm-fetch dbm key))
		  (dbm-close dbm))
		(when doc
		  (throw 'exit doc))))
	  documentation-files))))
  
;;;###autoload
(defun document-var (symbol doc-string)
  "Sets the `variable-documentation' property of SYMBOL to DOC-STRING."
  (put symbol 'variable-documentation doc-string)
  symbol)

;;;###autoload
(defun add-documentation (symbol string &optional is-variable)
  "Adds a documentation string STRING to the file of such strings."
  (require 'sdbm)
  (let
      ((dbm (dbm-open documentation-file 'append))
       (key (concat (if is-variable ?V ?S) (symbol-name symbol))))
    (dbm-store dbm key string 'replace)
    (dbm-close dbm)))