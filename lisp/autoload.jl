;;;; autoload.jl -- Initialise auto-load functions
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

;;; ::autoload-start::
(autoload 'add-change-log-entry "add-log" t)
(autoload 'changelog-mode "add-log" t)
(autoload 'asm-mode "asm-mode" t)
(autoload 'asm-cpp-mode "asm-mode" t)
(autoload 'buffer-summary "buffer-summary" t)
(autoload 'c-mode "c-mode" t)
(autoload 'c-backslash-area "c-mode" t)
(autoload 'c-insert-comment "c-mode" t)
(autoload 'start-compile-command "compile")
(autoload 'compile "compile" t)
(autoload 'next-error "compile" t)
(autoload 'grep "compile" t)
(autoload 'grep-buffer "compile" t)
(autoload 'compile-file "compiler" t)
(autoload 'compile-directory "compiler" t)
(autoload 'compile-lisp-lib "compiler" t)
(autoload 'compile-function "compiler" t)
(autoload 'compile-form "compiler")
(autoload 'debug-entry "debug")
(autoload 'debug-error-entry "debug")
(autoload 'disassemble "disassembler" t)
(autoload 'add-autoloads "find-autoloads" t)
(autoload 'remove-autoloads "find-autoloads" t)
(autoload 'gdb "gdb" t)
(autoload 'help "help" t)
(autoload 'describe-mode "help" t)
(autoload 'documentation "help")
(autoload 'document-var "help")
(autoload 'get-documentation "help")
(autoload 'add-documentation "help")
(autoload 'info "info" t)
(autoload 'isearch-forward "isearch" t)
(autoload 'isearch-backward "isearch" t)
(autoload 'print-keymap "keymap")
(autoload 'read-event "keymap")
(autoload 'describe-key "keymap" t)
(autoload 'latin-1-mode "latin-1" t)
(autoload 'lisp-mode "lisp-mode" t)
(autoload 'eval-sexp "lisp-mode" t)
(autoload 'eval-insert-sexp "lisp-mode" t)
(autoload 'eval-print-sexp "lisp-mode" t)
(autoload 'replace-last-match "replace")
(autoload 'replace-string "replace")
(autoload 'replace-all "replace" t)
(autoload 'query-replace "replace" t)
(autoload 'server-find-file "server")
(autoload 'server-close-file "server" t)
(autoload 'shell-mode "shell")
(autoload 'shell "shell" t)
(autoload 'shell-command "shell" t)
(autoload 'shell-command-on-buffer "shell" t)
(autoload 'shell-command-on-area "shell" t)
(autoload 'texinfo-mode "texinfo-mode" t)
(autoload 'text-mode "text-mode" t)
(autoload 'indented-text-mode "text-mode" t)
(autoload 'word-count-area "text-mode" t)
(autoload 'prompt "prompt")
(autoload 'prompt-for-file "prompt")
(autoload 'prompt-for-directory "prompt")
(autoload 'prompt-for-buffer "prompt")
(autoload 'prompt-for-symbol "prompt")
(autoload 'prompt-for-lisp "prompt")
(autoload 'prompt-for-function "prompt")
(autoload 'prompt-for-variable "prompt")
(autoload 'prompt-for-command "prompt")
(autoload 'prompt-from-list "prompt")
(autoload 'prompt-for-string "prompt")
(autoload 'prompt-for-number "prompt")
(autoload 'prompt2 "prompt")
(autoload 'yes-or-no-p "prompt")
(autoload 'y-or-n-p "prompt")
(autoload 'map-y-or-n-p "prompt")
(autoload 'read-mail "read-mail" t)
(autoload 'read-mail-folder "read-mail" t)
(autoload 'rm-reply "rm-misc" t)
(autoload 'rm-followup "rm-misc" t)
(autoload 'rm-output "rm-output" t)
(autoload 'mail-yank-original "rm-misc" t)
(autoload 'rm-forward "rm-misc" t)
(autoload 'rm-burst-message "rm-misc" t)
(autoload 'mail-setup "send-mail" t)
(autoload 'rcs-init-file "rcs")
(autoload 'rcs-register-buffer "rcs" t)
(autoload 'rcs-display-log "rcs" t)
(autoload 'tex-mode "tex-mode" t)
(autoload 'print-buffer "print" t)
(autoload 'print-buffer-to-file "print" t)
(autoload 'print-buffer-to-printer "print" t)
(autoload 'print-area "print" t)
(autoload 'print-area-to-file "print" t)
(autoload 'print-area-to-printer "print" t)
(autoload 'miranda "miranda" t)
(autoload 'insert-rectangle "rectangle")
(autoload 'copy-rectangle "rectangle")
(autoload 'delete-rectangle "rectangle")
(autoload 'cut-rectangle "rectangle")
(autoload 'load-user-mail-directory "mail-dir" t)
(autoload 'load-mail-directory "mail-dir" t)
(autoload 'prompt-for-mail-alias "mail-dir")
(autoload 'prompt-for-mail-address "mail-dir")
(autoload 'prompt-for-mail-full-name "mail-dir")
(autoload 'add-mail-alias "mail-dir" t)
(autoload 'remove-mail-alias "mail-dir" t)
(autoload 'add-mail-address "mail-dir" t)
(autoload 'remove-mail-address "mail-dir" t)
(autoload 'get-mail-alias "mail-dir" t)
(autoload 'get-mail-address "mail-dir" t)
(autoload 'get-mail-name-from-address "mail-dir" t)
(autoload 'list-mail-aliases "mail-dir-summary" t)
(autoload 'list-mail-addresses "mail-dir-summary" t)
(autoload 'set-fill-column "fill" t)
(autoload 'set-fill-prefix "fill" t)
(autoload 'fill-paragraph "fill" t)
(autoload 'fill-area "fill" t)
(autoload 'fill-mode "fill" t)
(autoload 'fill-mode-on "fill" t)
(autoload 'center-line "fill" t)
(autoload 'center-paragraph "fill" t)
(autoload 'set-bookmark "bookmarks" t)
(autoload 'goto-bookmark "bookmarks" t)
(autoload 'kill-bookmark "bookmarks" t)
(autoload 'kill-all-bookmarks "bookmarks" t)
(autoload 'list-bookmarks "bookmarks" t)
(autoload 'sort "sort")
(autoload 'visit-tag-table "tags" t)
(autoload 'find-tag "tags" t)
(autoload 'dired "dired" t)
(autoload 'getenv "environ")
(autoload 'setenv "environ")
(autoload 'file-subst "file-subst" t)
(autoload 'map-file-subst "file-subst")
(autoload 'find-url "find-url" t)
(autoload 'pwd-prompt "pwd-prompt")
(autoload 'telnet "telnet" t)
(autoload 'rlogin "telnet" t)
(autoload 'cvs-update "cvs" t)
(autoload 'cvs-update-parent "cvs" t)
(autoload 'cvs-update-pwd "cvs" t)
(autoload-variable 'cvs-keymap "cvs")
(bind-keys ctrl-x-keymap "c" '(setq next-keymap-path '(cvs-keymap)))
;;; ::autoload-end::
