#| bootstrap for rep module

   $Id$

   Copyright (C) 1993, 1994, 2000 John Harper <john@dcs.warwick.ac.uk>

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

(declare (module-bootstrap rep))

;; The inherited binding implementation has linear performance with the
;; number of bindings, i.e. slow when exporting almost all bindings in
;; the large rep module. So be careful to only create bindings that
;; should be exported, then set the `exports-all` flag.

(structure-exports-all (current-structure) t)

(open-structures '(rep.module-system
		   rep.lang.debug
		   rep.lang.symbols
		   rep.lang.math
		   rep.data
		   rep.io.streams))

(access-structures '(rep.io.files))

(defvar *standard-output* (rep.io.files#stdout-file)
  "Stream that `prin?' writes its output to by default.")

(defvar *standard-input* (rep.io.files#stdin-file)
  "Stream that `read' takes its input from by default.")

(defvar *standard-error* (rep.io.files#stderr-file)
  "Standard stream for error output.")

(%define load rep.io.files#load)

;; null i18n function until gettext is loaded
(%define _ (lambda (x) x))

;; later changed to 'user
(set! *user-structure* 'rep)

(open-structures '(rep.lang.backquote))

(load "rep/io/file-handlers/tilde")

(defvar *debug-entry* (make-autoload '*debug-entry* "rep/lang/debugger"))
(defvar *debug-exit*)
(defvar *debug-error-entry* (make-autoload '*debug-error-entry* "rep/lang/debugger"))
