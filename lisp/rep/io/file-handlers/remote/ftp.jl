;;;; remote-ftp.jl -- Remote file access via FTP
;;;  Copyright (C) 1998 John Harper <john@dcs.warwick.ac.uk>
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

;; TODO:
;;    - Allow file transfer mode (binary/ascii) to be determined by
;;	matching files against regexp(s)
;;    - Fix all the kludges marked by XXX

(define-module rep.io.file-handlers.remote.ftp

    (export remote-ftp-close-host
	    remote-ftp-close-all
	    remote-ftp-empty-cache
	    remote-ftp-add-passwd)

    (open rep
	  rep.regexp
	  rep.system
	  rep.io.files
	  rep.io.processes
	  rep.io.file-handlers
	  rep.io.file-handlers.remote.utils
	  rep.util.date
	  rep.mail.addr)

  (define-module-alias remote-ftp rep.io.file-handlers.remote.ftp)


;; Configuration:

(defvar ftp-program "ftp"
  "Program used for FTP sessions.")

(defvar remote-ftp-args '("-v" "-n" "-i" "-g")
  "List of arguments to remote FTP sessions.")

(defvar remote-ftp-show-messages t
  "When t, informational messages from FTP sessions are displayed.")

(defvar remote-ftp-max-message-lines nil
  "When true, the maximum number of FTP message lines to keep.")

(defvar remote-ftp-timeout 30
  "Number of seconds to wait for FTP output before giving up.")

(defvar remote-ftp-max-sessions 5
  "If true, the maximum number of FTP clients that may be running
concurrently.")

(defvar remote-ftp-anon-users "anonymous|ftp"
  "Regular expression matching user names of `anonymous' FTP sessions.")

(defvar remote-ftp-anon-passwd user-mail-address
  "Password sent to anonymous FTP sessions.")

;; XXX Allow this to be set by filename?
(defvar remote-ftp-transfer-type 'binary
  "Mode in which to transfer files, one of the symbols `binary' or `ascii'.")

(defvar remote-ftp-display-progress nil
  "When true, show progress of FTP transfers.")

(defvar remote-ftp-echo-output nil
  "When t, echo all output from FTP processes. Use for debugging only.")

(defvar remote-ftp-passwd-alist nil
  "Alist of (USER@HOST . PASSWD) defining all known FTP passwords.")

(defvar remote-ftp-ls-format "ls \"-la %s\""
  "FTP command format string to produce an `ls -l' format listing of the
directory substituted for the single %s format specifier.")

(defvar remote-ftp-dircache-expiry-time 360
  "Number of seconds before a dircache entry is reread.")

(defvar remote-ftp-dircache-max-dirs 5
  "Maximum number of directories whose contents may be cached at any one
time.")

(defvar remote-ftp-sessions nil
  "List of FTP structures defining all running FTP sessions.")


;; Output templates, mostly copied from ange-ftp..!

(defvar remote-ftp-prompt-regexp "([Ff]tp> *)+"
  "Regular expression matching a prompt from the FTP command (to be ignored).")

(defvar remote-ftp-multi-msgs
  "220-|230-|226|25.-|221-|200-|331-|4[25]1-|530-"
  "Regular expression matching the start of a multiline ftp reply.")

(defvar remote-ftp-good-msgs
  "220 |230 |226 |25. |221 |200 |[Hh]ash mark"
  "Regular expression matching ftp \"success\" messages.")

(defvar remote-ftp-bad-msgs 
  (concat "55. |500 |530 |\\?Invalid command"
	  "|([a-zA-Z0-9.-]+: )?[Uu]nknown host|ftp: ")
  "Regular expression matching ftp \"failure\" messages.")

(defvar remote-ftp-skip-msgs
  (concat "200 (PORT|Port) |331 |150 |350 |[0-9]+ bytes |"
	  "Connected |$|Remote system|Using| |"
	  "Data connection |"
	  "local:|Trying|125 |550-|221 .*oodbye")
  "Regular expression matching ftp messages that can be ignored.")

(defvar remote-ftp-reconnect-msgs
  (concat "Not connected|4[25]1 |rcmd: |"
	  "No control connection|"
	  "lost connection")
  "Regular expression matching ftp messages that indicate that the current
FTP process should be abandoned, and a new session started.")

(defvar remote-ftp-passwd-msgs "[Pp]assword: *"
  "Regular expression matching password prompt.")

(defvar remote-ftp-ls-l-regexp "([a-zA-Z-]+)\\s+(\\d+)\\s+(\\w+)\\s+(\\w+)\\s+(\\d+)\\s+([a-zA-Z]+\\s+\\d+\\s+[0-9:]+)\\s+([^/ \t\n]+)"
  "Regexp defining `ls -l' output syntax. Hairy.")

(defvar remote-ftp-ls-l-type-alist '((#\- . file) (#\d . directory)
				     (#\l . symlink) (#\p . pipe)
				     (#\s . socket) (#\b . device)
				     (#\c . device))
  "Alist associating characters in the first column of `ls -l' output with
file types.")


;; ftp structure

(defconst remote-ftp-host 0)
(defconst remote-ftp-user 1)
(defconst remote-ftp-process 2)
(defconst remote-ftp-status 3)	;success,failure,busy,nil,dying,timed-out
(defconst remote-ftp-callback 4)
(defconst remote-ftp-dircache 5)
(defconst remote-ftp-pending-output 6)
(defconst remote-ftp-login-data 7)	;PASSWD while logging in
(defconst remote-ftp-struct-size 8)

(defmacro remote-ftp-status-p (session stat)
  `(eq? (vector-ref ,session remote-ftp-status) ,stat))

;; Return an ftp structure for HOST and USER, with a running ftp session
(defun remote-ftp-open-host (host #!optional user)
  (unless user
    (set! user (remote-get-user host)))
  (catch 'foo
    (for-each (lambda (s)
		(when (and (string=? (vector-ref s remote-ftp-host) host)
			   (string=? (vector-ref s remote-ftp-user) user))
		  ;; Move S to the head of the list
		  (set! remote-ftp-sessions
			(cons s (delq! s remote-ftp-sessions)))
		  (throw 'foo s)))
	      remote-ftp-sessions)
    ;; Create a new session
    (let*
	((session (make-vector remote-ftp-struct-size)))
      (vector-set! session remote-ftp-host host)
      (vector-set! session remote-ftp-user user)
      (remote-ftp-open-session session))))

(defun remote-ftp-open-session (session)
  (let
      ((process (make-process (lambda (data)
				(remote-ftp-output-filter session data))
			      remote-ftp-sentinel
			      nil ftp-program
			      (append remote-ftp-args
				      (list (vector-ref session remote-ftp-host))))))
    (when (and remote-ftp-max-sessions
	       (> (list-length remote-ftp-sessions) remote-ftp-max-sessions))
      ;; Kill the session last used the earliest
      (remote-ftp-close-session (last remote-ftp-sessions)))
    (set-process-connection-type! process 'pty)
    (vector-set! session remote-ftp-process process)
    (vector-set! session remote-ftp-status 'busy)
    (or (start-process process)
	(error "Can't start FTP session"))
    (set! remote-ftp-sessions (cons session remote-ftp-sessions))
    (condition-case data
	(progn
	  (remote-ftp-connect session)
	  (remote-ftp-login session))
      (error
       (remote-ftp-close-session session)
       (signal (car data) (cdr data))))
    session))

(defun remote-ftp-close-session (session)
  (when (and (vector-ref session remote-ftp-process)
	     (process-in-use? (vector-ref session remote-ftp-process)))
    (vector-set! session remote-ftp-status 'dying)
    (set-process-output-stream! (vector-ref session remote-ftp-process) nil)
    (set-process-error-stream! (vector-ref session remote-ftp-process) nil)
    (kill-process (vector-ref session remote-ftp-process))))

(defun remote-ftp-close-host (host #!optional user)
  "Close the FTP subprocess connect to `USER@HOST'."
  (interactive "sHost:\nsUser:")
  (when (or (null? user) (string=? user ""))
    (set! user (remote-get-user host)))
  (catch 'foo
    (for-each (lambda (s)
		(when (and (string=? (vector-ref s remote-ftp-host) host)
			   (string=? (vector-ref s remote-ftp-user) user))
		  (remote-ftp-close-session s)
		  (throw 'foo t)))
	      remote-ftp-sessions)))

(defun remote-ftp-close-all ()
  "Close all running FTP subprocesses."
  (interactive)
  (for-each remote-ftp-close-session remote-ftp-sessions))

(defun remote-ftp-get-session-by-process (process)
  (catch 'return
    (for-each (lambda (s)
		(and (eq? (vector-ref s remote-ftp-process) process)
		     (throw 'return s)))
	      remote-ftp-sessions)))


;; Communicating with ftp sessions

(defun remote-ftp-write (session fmt arg-list)
  (when (remote-ftp-status-p session 'dying)
    (error "FTP session is dying"))
  (apply format (vector-ref session remote-ftp-process) fmt arg-list)
  (write (vector-ref session remote-ftp-process) #\newline)
  (vector-set! session remote-ftp-status 'busy))

(defun remote-ftp-while (session status #!optional type)
  (when (and (not (eq? status 'dying))
	     (remote-ftp-status-p session 'dying))
    (error "FTP session is dying"))
  (while (remote-ftp-status-p session status)
    (when (and (process-running? (vector-ref session remote-ftp-process))
	       (accept-process-output remote-ftp-timeout))
      (vector-set! session remote-ftp-status 'timed-out)
      (error "FTP process timed out (%s)" (or type "unknown")))))

(defun remote-ftp-command (session type fmt #!rest args)
  (let
      ((retry t))
    (while retry
      (set! retry nil)
      (condition-case data
	  (progn
	    (when remote-ftp-display-progress
	      (message (format nil "FTP %s: " type) t))
	    (remote-ftp-while session 'busy type)
	    (remote-ftp-write session fmt args)
	    (remote-ftp-while session 'busy type)
	    (when remote-ftp-display-progress
	      (format t " %s" (vector-ref session remote-ftp-status))))
	(error
	 (when (and (string? (list-ref data 1))
		    (string-match "transient error" (list-ref data 1))
		    (not (eq? type 'login)))
	   ;; The session has been killed. Wait for it to die
	   ;; totally then try to reconnect
	   (message (format nil "FTP: reconnecting to `%s'...\n"
			    (vector-ref session remote-ftp-host)))
	   (remote-ftp-while session 'dying type)
	   (remote-ftp-open-session session)
	   (set! retry t)))))
    (remote-ftp-error-if-unsuccessful session fmt args)))

;; Return t if successful, else signal a file-error
(defun remote-ftp-error-if-unsuccessful (session #!optional fmt args)
  (or (eq? (vector-ref session remote-ftp-status) 'success)
      (signal 'file-error
	      (list 'ftp
		    (format nil "%s@%s"
			    (vector-ref session remote-ftp-user)
			    (vector-ref session remote-ftp-host))
		    (and fmt (apply format nil fmt args))))))

(defun remote-ftp-output-filter (session output)
  (when remote-ftp-echo-output
    (let
	((*print-escape* t))
      (format (stderr-file) "FTP output: %S\n" output)))
  (when (vector-ref session remote-ftp-pending-output)
    (set! output (concat (vector-ref session remote-ftp-pending-output) output))
    (vector-set! session remote-ftp-pending-output nil))
  (let
      ((point 0)
       line-end)
    (while (< point (string-length output))
      ;; Skip any prompts
      (when (string-looking-at remote-ftp-prompt-regexp output point)
	(set! point (match-end)))
      ;; Look for `#' progress characters
      (when (string-looking-at "#+" output point)
	(set! point (match-end))
	(when remote-ftp-display-progress
	  (write t (substring output (match-start) (match-end)))
	  (when (feature? 'jade)
	    (declare (bound redisplay))
	    (redisplay))))
      (if (string-looking-at remote-ftp-passwd-msgs output point)
	  ;; Send password
	  (progn
	    (remote-ftp-write
	     session "%s\n"
	     (list (if (string-match remote-ftp-anon-users
				     (vector-ref session remote-ftp-user))
		       remote-ftp-anon-passwd
		     (let
			 ((pass (remote-ftp-get-passwd
				 (vector-ref session remote-ftp-user)
				 (vector-ref session remote-ftp-host)
				 (vector-ref session remote-ftp-login-data))))
		       (unless pass
			 (remote-ftp-close-session session)
			 (error "No valid password"))
		       (vector-set! session remote-ftp-login-data pass)
		       pass))))
	    ;; Can't be anything more?
	    (set! point (string-length output)))
	(if (string-match "\n" output point)
	    (progn
	      ;; At least one whole line
	      (set! line-end (match-end))
	      (cond
	       ((string-looking-at remote-ftp-skip-msgs output point)
		;; Ignore this line of output
		(set! point line-end))
	       ((string-looking-at remote-ftp-good-msgs output point)
		;; Success!
		(vector-set! session remote-ftp-status 'success)
		(set! point line-end))
	       ((string-looking-at remote-ftp-bad-msgs output point)
		;; Failure!
		(vector-set! session remote-ftp-status 'failure)
		(set! point line-end))
	       ((string-looking-at remote-ftp-multi-msgs output point)
		;; One line of a multi-line message
		(remote-ftp-show-multi output point line-end)
		(set! point line-end))
	       ((string-looking-at remote-ftp-reconnect-msgs output point)
		;; Transient error. Kill the session, then try to reopen it
		(remote-ftp-close-session session)
		(error "FTP process had transient error"))
	       (t
		;; Hmm. something else. If one exists invoke the callback
		(when (vector-ref session remote-ftp-callback)
		  ((vector-ref session remote-ftp-callback)
		   session output point line-end))
		(set! point line-end))))
	  ;; A partial line. Store it as pending
	  (vector-set! session remote-ftp-pending-output (substring output point))
	  (set! point (string-length output)))))))

(defun remote-ftp-sentinel (process)
  (let
      ((session (remote-ftp-get-session-by-process process)))
    (unless (process-in-use? process)
      (vector-set! session remote-ftp-process nil)
      (vector-set! session remote-ftp-dircache nil)
      (vector-set! session remote-ftp-status nil)
      (vector-set! session remote-ftp-pending-output nil)
      (vector-set! session remote-ftp-callback nil)
      (set! remote-ftp-sessions (delq! session remote-ftp-sessions)))))

(defun remote-ftp-show-multi (string start end)
  (if (feature? 'jade)
      (progn
	(declare (bound open-buffer goto end-of-buffer insert
			pos-line delete-area start-of-buffer
			backward-line goto-buffer other-view
			shrink-view-if-larger-than-buffer))
	(let
	    ((buffer (open-buffer "*ftp messages*")))
	  (with-object buffer
	    (goto (end-of-buffer))
	    (insert (substring string start end))
	    (when (and remote-ftp-max-message-lines
		       (> (pos-line (end-of-buffer))
			  remote-ftp-max-message-lines))
	      (delete-area (start-of-buffer)
			   (backward-line remote-ftp-max-message-lines
					  (end-of-buffer)))))
	  (when remote-ftp-show-messages
	    (with-object (other-view)
	      (goto-buffer buffer)
	      (shrink-view-if-larger-than-buffer)))))
    (when remote-ftp-show-messages
      (write *standard-output* (substring string start end)))))


;; FTP commands

;; SESSION has been started, wait for the connection to
;; succeed or fail
(defun remote-ftp-connect (session)
  (remote-ftp-while session 'busy 'connect)
  (remote-ftp-error-if-unsuccessful session "connect"))

;; Starts the process structure already defined in SESSION, then
;; logs in as the named user
(defun remote-ftp-login (session)
  (and (remote-ftp-command session 'login "user %s"
			   (vector-ref session remote-ftp-user))
       (when (string? (vector-ref session remote-ftp-login-data))
	 ;; The password for this session. It seemed successful
	 ;; so store for later use
	 (remote-ftp-add-passwd (vector-ref session remote-ftp-user)
				(vector-ref session remote-ftp-host)
				(vector-ref session remote-ftp-login-data))
	 (vector-set! session remote-ftp-login-data nil))
       (remote-ftp-command session 'login "type %s" remote-ftp-transfer-type)
       ;; For testing the reconnection-on-idle
       ;(set! remote-ftp-echo-output t)
       ;(remote-ftp-command session 'login "quote site idle 30")
       (and remote-ftp-display-progress
	    (remote-ftp-command session 'login "hash"))))

(defun remote-ftp-get (session remote-file local-file)
  (remote-ftp-command session 'get "get %s %s" remote-file local-file))

(defun remote-ftp-put (session local-file remote-file)
  (unwind-protect
      (remote-ftp-command session 'put "put %s %s" local-file remote-file)
    (remote-ftp-invalidate-directory
     session (file-name-directory remote-file))))

(defun remote-ftp-rm (session remote-file)
  (unwind-protect
      (remote-ftp-command session 'rm "delete %s" remote-file)
    (remote-ftp-invalidate-directory
     session (file-name-directory remote-file))))

(defun remote-ftp-mv (session old-name new-name)
  (unwind-protect
      (remote-ftp-command session 'mv "rename %s %s" old-name new-name)
    (remote-ftp-invalidate-directory
     session (file-name-directory old-name))
    (remote-ftp-invalidate-directory
     session (file-name-directory new-name))))

(defun remote-ftp-rmdir (session remote-dir)
  (unwind-protect
      (remote-ftp-command session 'rmdir "rmdir %s" remote-dir)
    (remote-ftp-invalidate-directory
     session (file-name-directory remote-dir))))

(defun remote-ftp-mkdir (session remote-dir)
  (unwind-protect
      (remote-ftp-command session 'mkdir "mkdir %s" remote-dir)
    (remote-ftp-invalidate-directory
     session (file-name-directory remote-dir))))

(defun remote-ftp-chmod (session mode file)
  ;; XXX Some FTP clients (i.e. Solaris 2.6) don't have the
  ;; XXX chmod command. Perhaps we could use "quote site chmod .."
  ;; XXX but the Solaris ftpd doesn't support this either..
  (unwind-protect
      (condition-case nil
	  (remote-ftp-command
	   session 'chmod "quote site chmod %o %s" mode file)
	(file-error
	 ;; Assume the chmod failed
	 (message (format nil "Warning: `chmod %o %s@%s:file' failed"
			  mode (vector-ref session remote-ftp-user)
			  (vector-ref session remote-ftp-host) file) t)
	 nil))
    (remote-ftp-invalidate-directory
     session (file-name-directory file))))


;; Directory handling/caching

(defconst remote-ftp-file-name 0)
(defconst remote-ftp-file-size 1)
(defconst remote-ftp-file-modtime 2)	;may be an unparsed string
(defconst remote-ftp-file-type 3)
(defconst remote-ftp-file-modes 4)	;nil if mode-string needs parsing
(defconst remote-ftp-file-mode-string 5)
(defconst remote-ftp-file-nlinks 6)
(defconst remote-ftp-file-user 7)
(defconst remote-ftp-file-group 8)
(defconst remote-ftp-file-symlink 9)
(defconst remote-ftp-file-struct-size 10)

(defconst remote-ftp-cache-dir 0)
(defconst remote-ftp-cache-expiry 1)
(defconst remote-ftp-cache-entries 2)
(defconst remote-ftp-cache-struct-size 3)

(defun remote-ftp-parse-ls-l (string point)
  (when (string-looking-at remote-ftp-ls-l-regexp string point)
    (let
	((mode-string (substring string (match-start 1) (match-end 1)))
	 (nlinks (string->number (substring
				  string (match-start 2) (match-end 2))))
	 (user (substring string (match-start 3) (match-end 3)))
	 (group (substring string (match-start 4) (match-end 4)))
	 (size (string->number (substring
				string (match-start 5) (match-end 5))))
	 (modtime (substring string (match-start 6) (match-end 6)))
	 (name (substring string (match-start 7) (match-end 7)))
	 symlink)
      (when (string-match ".*\\s+->\\s+(\\S+)" string point)
	(set! symlink (substring string (match-start 1) (match-end 1))))
      (vector name size modtime (cdr (assq (string-ref mode-string 0)
					   remote-ftp-ls-l-type-alist))
	      nil mode-string nlinks user group symlink))))

(defun remote-ftp-file-get-modtime (file-struct)
  (when (string? (vector-ref file-struct remote-ftp-file-modtime))
    (let
	((date (parse-date (vector-ref file-struct remote-ftp-file-modtime))))
      (when date
	(vector-set! file-struct remote-ftp-file-modtime
	      (vector-ref date date-vec-epoch-time)))))
  (vector-ref file-struct remote-ftp-file-modtime))

(defun remote-ftp-file-get-modes (file-struct)
  (unless (vector-ref file-struct remote-ftp-file-modes)
    (let*
	((string (vector-ref file-struct remote-ftp-file-mode-string))
	 (tuple-function
	  (lambda (point tuple)
	    (+ (ash (+ (if (/= (string-ref string point) #\-) 4 0)
		       (if (/= (string-ref string (1+ point)) #\-) 2 0)
		       (if (char-lower-case? (string-ref string (+ point 2))) 1 0))
		    (* tuple 3))
	       (if (memq (string-ref string (+ point 2)) '(#\s #\S #\t #\T))
		   (ash #o1000 tuple)
		 0)))))
      (vector-set! file-struct remote-ftp-file-modes
	    (+ (tuple-function 1 2)
	       (tuple-function 4 1)
	       (tuple-function 7 0)))))
  (vector-ref file-struct remote-ftp-file-modes))

(defun remote-ftp-file-owner-p (session file)
  (string=? (vector-ref session remote-ftp-user)
	   (vector-ref file remote-ftp-file-user)))

(defun remote-ftp-dir-cached-p (session dir)
  (set! dir (directory-file-name dir))
  (catch 'exit
    (for-each (lambda (dir-entry)
		(when (string=? (vector-ref dir-entry remote-ftp-cache-dir) dir)
		  (throw 'exit dir-entry)))
	      (vector-ref session remote-ftp-dircache))))

(defun remote-ftp-get-file (session filename)
  (let
      ((dir (file-name-directory filename))
       (base (file-name-nondirectory filename))
       entry)
    (when (string=? base "")
      ;; hack, hack
      (set! base (file-name-nondirectory dir))
      (set! dir (file-name-directory dir))
      (when (string=? base "")
	(set! base ".")))
    (set! dir (directory-file-name dir))
    (set! entry (remote-ftp-dir-cached-p session dir))
    (if (not (and entry (> (vector-ref entry remote-ftp-cache-expiry)
			   (current-time))))
	(progn
	  ;; Cache directory DIR
	  (when entry
	    (vector-set! session remote-ftp-dircache
		  (delq! entry (vector-ref session remote-ftp-dircache)))
	    (set! entry nil))
	  (remote-ftp-while session 'busy 'dircache)
	  (when (>= (list-length (vector-ref session remote-ftp-dircache))
		    remote-ftp-dircache-max-dirs)
	    ;; delete the least-recently-used entry
	    (set-cdr! (list-tail (vector-ref session remote-ftp-dircache)
			    (1- (list-length (vector-ref session remote-ftp-dircache)))) nil))
	  ;; add the new (empty) entry for the directory to be read.
	  (set! entry (vector dir (+ (current-time)
				     remote-ftp-dircache-expiry-time) nil))
	  (vector-set! session remote-ftp-dircache
		(cons entry (vector-ref session remote-ftp-dircache)))
	  ;; construct the callback function to have the new cache entry
	  ;; as the first argument
	  (vector-set! session remote-ftp-callback
		(lambda (#!rest args)
		  (apply
		   (lambda (cache-entry session output point line-end)
		     (declare (unused session line-end))
		     (let
			 ((file-struct (remote-ftp-parse-ls-l output point)))
		       (when file-struct
			 (vector-set! cache-entry remote-ftp-cache-entries
			       (cons file-struct
				     (vector-ref cache-entry
					   remote-ftp-cache-entries))))))
		   entry args)))
	  ;; my ftp server (wu-2.6.0(1)) doesn't like being told to
	  ;; `ls .' -- it recursively lists the top-level directories
	  (if (string=? dir ".")
	      (remote-ftp-command session 'dircache "ls -la")
	    (remote-ftp-command session 'dircache remote-ftp-ls-format dir))
	  (vector-set! session remote-ftp-callback nil))
      ;; entry is still valid, move it to the front of the list
      (vector-set! session remote-ftp-dircache
	    (cons entry (delq! entry (vector-ref session remote-ftp-dircache)))))
    ;; ENTRY now has the valid dircache directory structure
    (catch 'return
      (for-each (lambda (f)
		  (when (string=? (vector-ref f remote-ftp-file-name) base)
		    (throw 'return f)))
		(vector-ref entry remote-ftp-cache-entries)))))

;; similar to remote-ftp-get-file, but symbolic links are followed
(defun remote-ftp-lookup-file (session file)
  (let
      ((file-struct (remote-ftp-get-file session file)))
    (while (and file-struct
		(eq? (vector-ref file-struct remote-ftp-file-type) 'symlink))
      (let
	  ((link (vector-ref file-struct remote-ftp-file-symlink)))
	(set! file (expand-file-name link (file-name-directory file)))
	(set! file-struct (remote-ftp-get-file session file))))
    file-struct))

(defun remote-ftp-invalidate-directory (session directory)
  (set! directory (directory-file-name directory))
  (let
      ((entry (remote-ftp-dir-cached-p session directory)))
    (when entry
      (vector-set! session remote-ftp-dircache
	    (delq! entry (vector-ref session remote-ftp-dircache))))))

(defun remote-ftp-empty-cache ()
  "Discard all cached FTP directory entries."
  (interactive)
  (for-each (lambda (ses)
	      (vector-set! ses remote-ftp-dircache nil)) remote-ftp-sessions))


;; Password caching

(defun remote-ftp-get-passwd (user host #!optional retrying)
  (let*
      ((joined (concat user #\@ host))
       (cell (assoc joined remote-ftp-passwd-alist)))
    (if cell
	(cdr cell)
      (pwd-prompt (concat (if retrying "Try again; p" #\P)
			  "assword for " joined #\:)))))

(defun remote-ftp-add-passwd (user host passwd)
  "Add the string PASSWD as the password for FTP session of USER@HOST."
  (interactive "sUsername:\nsHost:\nPassword for %s@%s:")
  (let
      ((joined (concat user #\@ host)))
    (catch 'foo
      (for-each (lambda (cell)
		  (when (string=? (car cell) joined)
		    (set-cdr! cell passwd)
		    (throw 'foo)))
		remote-ftp-passwd-alist)
      (set! remote-ftp-passwd-alist (cons (cons joined passwd)
					  remote-ftp-passwd-alist)))))


;; Backend handler

(defun remote-ftp-handler (split-name op args)
  (cond
   ((eq? op 'canonical-file-name)
    ;; XXX implement this by resolving symlinks
    (car args))
   ((file? (car args))
    ;; Operations on file handles
    (cond
     ((memq op '(seek-file flush-file write-buffer-contents
		 read-file-contents insert-file-contents))
      ;; Just pass these through to the underlying file
      (apply (variable-ref op) (file-bound-stream (car args)) (cdr args)))
     ((eq? op 'close-file)
      ;; Close the file, synchronise with the remote file if required
      (let*
	  ((file (car args))
	   (data (file-handler-data file))
	   (session (remote-ftp-open-host (list-ref split-name 1)
					  (car split-name))))
	(close-file (file-bound-stream file))
	(when (memq (vector-ref data 1) '(write append))
	  ;; Copy the local version back to the remote fs
	  (remote-ftp-put session (vector-ref data 3) (vector-ref data 2)))
	(delete-file (vector-ref data 3))))
     (t
      (error "Unsupported FTP op on file-handler: %s %s" op args))))
   ((memq op '(read-file-contents insert-file-contents copy-file-to-local-fs))
    ;; Need to get the file to the local fs
    (let
	((local-name (if (eq? op 'copy-file-to-local-fs)
			 (list-ref args 1)
		       (make-temp-name)))
	 (session (remote-ftp-open-host
		   (list-ref split-name 1) (car split-name))))
      (remote-ftp-get session (list-ref split-name 2) local-name)
      (unless (eq? op 'copy-file-to-local-fs)
	(unwind-protect
	    ((variable-ref op) local-name)
	  (delete-file local-name)))
      t))
   ((memq op '(write-buffer-contents copy-file-from-local-fs))
    ;; Need to get the file off the local fs
    (let
	((local-name (if (eq? op 'copy-file-from-local-fs)
			 (car args)
		       (make-temp-name)))
	 (session (remote-ftp-open-host
		   (list-ref split-name 1) (car split-name))))
      (unless (eq? op 'copy-file-from-local-fs)
	(apply (variable-ref op) local-name (cdr args)))
      (unwind-protect
	  (remote-ftp-put session local-name (list-ref split-name 2))
	(unless (eq? op 'copy-file-from-local-fs)
	  (delete-file local-name)))
      t))
   ((eq? op 'copy-file)
    ;; Copying on the remote fs.
    ;; XXX Is there a way to avoid the double transfer?
    ;; XXX Not for inter-session copies, anyway.
    (let
	((local-file (make-temp-name))
	 (dest-split (remote-split-filename (list-ref args 1))))
      (unwind-protect
	  (and (remote-ftp-handler split-name 'copy-file-to-local-fs
				   (list (car args) local-file))
	       (remote-ftp-handler dest-split 'copy-file-from-local-fs
				   (list local-file (list-ref args 1))))
	(and (file-exists? local-file)
	     (delete-file local-file)))))
   ((eq? op 'rename-file)
    (let
	((session (remote-ftp-open-host
		   (list-ref split-name 1) (car split-name)))
	 (dest-split (remote-split-filename (list-ref args 1))))
      (or (and (string=? (car dest-split) (car split-name))
	       (string=? (list-ref dest-split 1) (list-ref split-name 1)))
	  (error "Can't rename files across FTP sessions"))
      (remote-ftp-mv session (list-ref split-name 2) (list-ref dest-split 2))))
   (t
    ;; All functions taking a single argument
    (let
	((session (remote-ftp-open-host
		   (list-ref split-name 1) (car split-name)))
	 (file-name (list-ref split-name 2)))
      (cond
       ((eq? op 'directory-files)
	(let
	    ;; XXX this assumes local/remote have same naming structure!
	    ((dir (file-name-as-directory file-name)))
	  (remote-ftp-lookup-file session dir)
	  (map (lambda (f)
		 (vector-ref f remote-ftp-file-name))
	       (vector-ref (remote-ftp-dir-cached-p session dir)
		     remote-ftp-cache-entries))))
       ((eq? op 'delete-file)
	(remote-ftp-rm session file-name))
       ((eq? op 'delete-directory)
	(remote-ftp-rmdir session file-name))
       ((eq? op 'make-directory)
	(remote-ftp-mkdir session file-name))
       ((eq? op 'set-file-modes)
	(remote-ftp-chmod session (list-ref args 1) file-name))
       ((eq? op 'open-file)
	(let
	    ((type (list-ref args 1))
	     (local-file (make-temp-name))
	     local-fh)
	  (when (memq type '(read append))
	    ;; Need to transfer the file initially
	    (remote-ftp-get session file-name local-file))
	  ;; Open the local file
	  (set! local-fh (make-file-from-stream (car args)
						(open-file local-file type)
						'remote-file-handler))
	  (set-file-handler-data local-fh
				 (vector remote-ftp-handler
					 type		;access type
					 file-name	;remote name
					 local-file))	;local copy
	  (remote-register-file-handle local-fh)
	  local-fh))
       (t
	(let
	    ((file (if (eq? op 'file-symlink?)
		       (remote-ftp-get-file session file-name)
		     (remote-ftp-lookup-file session file-name))))
	  (cond
	   ((eq? op 'file-exists?)
	    file)
	   ((eq? op 'file-regular?)
	    (and file (eq? (vector-ref file remote-ftp-file-type) 'file)))
	   ((eq? op 'file-directory?)
	    (and file (eq? (vector-ref file remote-ftp-file-type) 'directory)))
	   ((eq? op 'file-symlink?)
	    (and file (eq? (vector-ref file remote-ftp-file-type) 'symlink)))
	   ((eq? op 'file-size)
	    (and file (vector-ref file remote-ftp-file-size)))
	   ((eq? op 'file-modes)
	    (and file (remote-ftp-file-get-modes file)))
	   ((eq? op 'file-modes-as-string)
	    (and file (vector-ref file remote-ftp-file-mode-string)))
	   ((eq? op 'file-nlinks)
	    (and file (vector-ref file remote-ftp-file-nlinks)))
	   ((eq? op 'file-modtime)
	    (if file (remote-ftp-file-get-modtime file) (cons 0 0)))
	   ((eq? op 'file-owner?)
	    (and file (remote-ftp-file-owner-p session file)))
	   ((eq? op 'file-readable?)
	    (and file (/= (logand (remote-ftp-file-get-modes file)
				  (if (remote-ftp-file-owner-p session file)
				      #o400 #o004)) 0)))
	   ((eq? op 'file-writable?)
	    (and file (/= (logand (remote-ftp-file-get-modes file)
				  (if (remote-ftp-file-owner-p session file)
				      #o200 #o002)) 0)))
	   ((eq? op 'read-symlink)
	    (and file (or (vector-ref file remote-ftp-file-symlink)
			  (signal 'file-error
				  (list "File isn't a symlink:" split-name)))))
	   (t
	    (error "Unsupported FTP op: %s %s" op args))))))))))

;;;###autoload (put 'ftp 'remote-backend 'remote-ftp-handler)

;;;###autoload (autoload-file-handler 'remote-ftp-handler 'rep.io.file-handlers.remote.ftp)

(define-file-handler 'remote-ftp-handler remote-ftp-handler))
