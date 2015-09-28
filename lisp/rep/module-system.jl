#| rep.module-system bootstrap

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

(declare (module-bootstrap rep.module-system))

(open-structures '(rep.lang.symbols
		   rep.structures
		   rep.data))

;; rename the bindings required by exported macros
(%define %make-module make-structure)
(%define %make-interface make-interface)
(%define %parse-interface parse-interface)
(%define %module-ref structure-access)
(%define %alias-module alias-structure)


;; module syntax

(defmacro define-interface (name sig)
  "Associate the symbol NAME with the module interface SIG (in a
separate interface-name namespace). An interface specification must be
of the form:

   INTERFACE ->  (export [ID...])
	     or  NAME
	     or  (compound-interface [INTERFACE...])
	     or  (module-interface [MODULE-NAME...])

where an ID is a symbol naming a top-level binding to export, and NAME
is the name of an interface previously defined using define-interface.

The `export' form adds top-level definitions ID... to the interface;
the `compound-interface' clauses forms the union of the given
interfaces."

  (list '%make-interface (list 'quote name)
	(list '%parse-interface (list 'quote sig))))

(defmacro module (#!optional sig config . body)
  "Create a new module whose interface is SIG, whose configuration is
defined by CONFIG (either a single clause, or a list of clauses), and
whose definitions are defined by the list of forms BODY.

See `define-interface' for the interface syntax, each configuration
clause must have the syntax:

   CLAUSE ->  (open [NAME...])
	  or  (access [NAME...])

where NAME is the name of a module. Opening a module imports all of its
exported definitions into the currently module, while accessing a
module makes the exported definitions available from the current module
using the `module-ref' form."

  (unless (list? (car config))
    (set! config (list config)))
  (list '%make-module (list '%parse-interface (list 'quote sig))
	(list* 'lambda nil (cons '(open rep.module-system) config))
	(list* 'lambda nil body)))

(defmacro define-module (name #!optional sig config . body)
  "Create a module called NAME whose interface is SIG, whose
configuration is defined by CONFIG (either a single clause, or a list
of clauses), and whose definitions are defined by the list of forms
BODY.

See the `define-interface' and `module' macros for descriptions of the
interface and configuration clause syntaxes respectively."

  (unless (list? (car config))
    (set! config (list config)))
  (list '%make-module (list '%parse-interface (list 'quote sig))
	(list* 'lambda nil (cons '(open rep.module-system) config))
	(list* 'lambda nil body)
	(list 'quote name)))

(defmacro define-modules (modules config . body)
  "Similar to `define-module' except that multiple modules are created,
each exporting a particular view of the underlying bindings.

MODULES is a list defining the names and interfaces of the created
modules, each item has the form `(NAME INTERFACE)'. CONFIG and BODY are
exactly the same as in the `define-module' syntax."
  (unless (list? (car config))
    (set! config (list config)))
  (require 'rep.lang.backquote)
  (let ((tem (gensym)))
    `(let ((,tem (list (module () ((export-all) ,@config) ,@body))))
       ,@(map (lambda (x)
		(let ((name (car x))
		      (interface (cadr x)))
		  `(%make-module
		    (%parse-interface ',interface)
		    (lambda ()
		      (open rep.module-system)
		      (%open-modules ,tem))
		    () ',name)))
		 modules))))

(defmacro define-module-alias (to from)
  "Create a secondary name TO for the module called FROM."
  (list '%alias-module (list 'quote from) (list 'quote to)))

(defmacro module-ref (module-name var-name)
  "Evaluates to the current value of the global binding of symbol
VAR-NAME in the module called MODULE-NAME. This module must previously
have been opened or accessed by the current module.

When read, the syntax `FOO#BAR' expands to `(module-ref FOO BAR)'."

  (list '%module-ref (list 'quote module-name) (list 'quote var-name)))


;; `%meta' structure used for configuring modules

;; helper definitions
(defmacro open names
  (list '%open-modules (list 'quote names)))
(defmacro access names
  (list '%access-modules (list 'quote names)))
(defmacro set-binds ()
  (list '%set-implicit-define! (list '%current-module) ''t))
(defmacro export-all ()
  (list '%set-implicit-export! (list '%current-module) ''t))

(let ((meta-struct (make-structure '(open access set-binds export-all
				     %open-modules %access-modules
				     %set-implicit-define!
				     %set-implicit-export!
				     %current-module quote)
				   nil nil '%meta)))
  (structure-define meta-struct 'open open)
  (structure-define meta-struct 'access access)
  (structure-define meta-struct 'set-binds set-binds)
  (structure-define meta-struct 'export-all export-all)
  (structure-define meta-struct '%open-modules open-structures)
  (structure-define meta-struct '%access-modules access-structures)
  (structure-define meta-struct '%set-implicit-define! set-structure-implicit-define!)
  (structure-define meta-struct '%set-implicit-export! set-structure-implicit-export!)
  (structure-define meta-struct '%current-module current-structure)
  (structure-define meta-struct 'quote quote))


;; exports

(export-bindings '(define-interface module define-module
		   define-modules define-module-alias module-ref
		   %make-module %make-interface %parse-interface
		   %module-ref %alias-module))

(export-bindings '(lambda validate-byte-code run-byte-code load))
