#! /bin/sh
exec rep "$0" "$@"
!#

;; rep-xgettext.jl -- extract i18n strings from lisp scripts
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

(require 'rep.i18n.xgettext)

(defvar *write-c-file* nil)


;; entry point

(when (get-command-line-option "--help")
  (write *standard-output* "\
usage: rep-xgettext [OPTIONS...] FILES...

where OPTIONS are any of:

  --include DEFINER
  --c
  --pot\n")
  (throw 'quit 0))

(when (or (get-command-line-option "-c") (get-command-line-option "--c"))
  (set! *write-c-file* t))
(when (or (get-command-line-option "-p") (get-command-line-option "--pot"))
  (set! *write-c-file* nil))

(let loop ((included '()) tem)
  (set! tem (get-command-line-option "--include" t))
  (when tem
    (set! included (cons (intern tem) included))
    (loop))
  (when included
    (set-included-definers included)))

(while *command-line-args*
  (let ((file (car *command-line-args*)))
    (set! *command-line-args* (cdr *command-line-args*))
    (scan-file file)))

(if *write-c-file*
    (output-c-file)
  (output-pot-file))

;; Local variables:
;; major-mode: lisp-mode
;; End:
