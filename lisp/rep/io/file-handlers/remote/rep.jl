;;;; remote-rep.jl -- Remote file access via the rep-remote program
;;;  Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>
;;;  $Id$

;;; This file is part of librep.

;;; librep is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; librep is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with librep; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(define-structure rep.io.file-handlers.remote.rep

    (export remote-rep-add-passwd
	    remote-rep-close-host
	    remote-rep-close-all
	    remote-rep-empty-cache)

    (open rep
	  rep.regexp
	  rep.system
	  rep.io.processes
	  rep.io.files
	  rep.io.file-handlers
	  rep.io.file-handlers.remote.utils)

  (define-structure-alias remote-rep rep.io.file-handlers.remote.rep)


;; Configuration

;; rsh doesn't ask for passwords when changing uids,
;; better to use ssh which does
(defvar remote-rep-rsh-program "rsh")

(defvar remote-rep-program "rep-remote")

(defvar remote-rep-timeout 30)

(defvar remote-rep-display-progress nil)

(defvar remote-rep-max-sessions 5)

(defvar remote-rep-echo-output nil)

(define remote-rep-passwd-alist nil)

(defvar remote-rep-dircache-expiry-time 60)

(defvar remote-rep-dircache-max-dirs 5)

(define remote-rep-sessions nil)

(defconst remote-rep-signature "\002rep-remote; protocol (\\d+)\002")

(defvar remote-rep-passwd-msgs "[Pp]assword: *"
  "Regular expression matching password prompt.")

(defconst remote-rep-required-protocol 1)

(define remote-rep-hex-map (let
			       ((map (make-string 128 0))
				i)
			     (setq i #\0)
			     (while (<= i #\9)
			       (string-set! map i (- i #\0))
			       (setq i (1+ i)))
			     (setq i #\a)
			     (while (<= i #\f)
			       (string-set! map i (+ (- i #\a) 10))
			       (string-set! map (+ i (- #\A #\a))
					    (string-ref map i))
			       (setq i (1+ i)))
			     map))

(defconst remote-rep-success #\001)
(defconst remote-rep-failure #\177)



;; session structure

(defconst remote-rep-host 0)
(defconst remote-rep-user 1)
(defconst remote-rep-process 2)
(defconst remote-rep-status 3)	;success,failure,busy,nil,dying,timed-out
(defconst remote-rep-callback 4)
(defconst remote-rep-dircache 5)
(defconst remote-rep-pending-output 6)
(defconst remote-rep-login-data 7)	;PASSWD while logging in
(defconst remote-rep-error 8)
(defconst remote-rep-protocol 9)
(defconst remote-rep-struct-size 10)

(defmacro remote-rep-status-p (session stat)
  `(eq? (vector-ref ,session remote-rep-status) ,stat))

;; Return an rep structure for HOST and USER, with a running rep session
(defun remote-rep-open-host (host #!optional user)
  (unless user
    (setq user (remote-get-user host)))
  (catch 'foo
    (for-each (lambda (s)
		(when (and (string=? (vector-ref s remote-rep-host) host)
			   (string=? (vector-ref s remote-rep-user) user))
		  ;; Move S to the head of the list
		  (setq remote-rep-sessions
			(cons s (delq! s remote-rep-sessions)))
		  (throw 'foo s)))
	      remote-rep-sessions)
    ;; Create a new session
    (let*
	((session (make-vector remote-rep-struct-size)))
      (vector-set! session remote-rep-host host)
      (vector-set! session remote-rep-user user)
      (remote-rep-open-session session))))

(defun remote-rep-open-session (session)
  (let
      ((process (make-process (lambda (data)
				(remote-rep-output-filter session data))
			      remote-rep-sentinel
			      nil remote-rep-rsh-program
			      (list "-l" (vector-ref session remote-rep-user)
				    (vector-ref session remote-rep-host)
				    remote-rep-program))))
    (when (and remote-rep-max-sessions
	       (> (list-length remote-rep-sessions) remote-rep-max-sessions))
      ;; Kill the session last used the earliest
      (remote-rep-close-session (last remote-rep-sessions)))
    (set-process-connection-type! process 'pipe)
    (vector-set! session remote-rep-process process)
    (vector-set! session remote-rep-status 'busy)
    (or (start-process process)
	(error "Can't start rep-remote session"))
    (setq remote-rep-sessions (cons session remote-rep-sessions))
    (condition-case data
	(remote-rep-connect session)
      (error
       (remote-rep-close-session session)
       (signal (car data) (cdr data))))
    session))

(defun remote-rep-close-session (session)
  (when (and (vector-ref session remote-rep-process)
	     (process-in-use? (vector-ref session remote-rep-process)))
    (vector-set! session remote-rep-status 'dying)
    (set-process-output-stream! (vector-ref session remote-rep-process) nil)
    (set-process-error-stream! (vector-ref session remote-rep-process) nil)
    (kill-process (vector-ref session remote-rep-process))))

(defun remote-rep-close-host (host #!optional user)
  "Close the rep-remote subprocess connected to `USER@HOST'."
  (interactive "sHost:\nsUser:")
  (when (or (null? user) (string=? user ""))
    (setq user (remote-get-user host)))
  (catch 'foo
    (for-each (lambda (s)
		(when (and (string=? (vector-ref s remote-rep-host) host)
			   (string=? (vector-ref s remote-rep-user) user))
		  (remote-rep-close-session s)
		  (throw 'foo t)))
	      remote-rep-sessions)))

(defun remote-rep-close-all ()
  "Close all running rep-remote subprocesses."
  (interactive)
  (for-each remote-rep-close-session remote-rep-sessions))

(defun remote-rep-get-session-by-process (process)
  (catch 'return
    (for-each (lambda (s)
		(and (eq? (vector-ref s remote-rep-process) process)
		     (throw 'return s)))
	      remote-rep-sessions)))


;; Communicating with the remote process

(defun remote-rep-write (session fmt #!rest args)
  (when (remote-rep-status-p session 'dying)
    (error "rep-remote session is dying"))
  (apply format (vector-ref session remote-rep-process) fmt args)
  (vector-set! session remote-rep-status 'busy))

(defun remote-rep-send-int (session int)
  (remote-rep-write session "%08x" int))

(defun remote-rep-send-string (session string)
  (remote-rep-send-int session (string-length string))
  (remote-rep-write session "%s" string))

(defun remote-rep-while (session status #!optional type)
  (when (and (not (eq? status 'dying))
	     (remote-rep-status-p session 'dying))
    (error "rep-remote session is dying"))
  (while (remote-rep-status-p session status)
    (when (and (process-running? (vector-ref session remote-rep-process))
	       (accept-process-output-1 (vector-ref session remote-rep-process)
					remote-rep-timeout))
      (vector-set! session remote-rep-status 'timed-out)
      (error "rep-remote process timed out (%s)" (or type "unknown")))))

(defun remote-rep-command (session type #!optional output-fun #!rest args)
  (when remote-rep-display-progress
    (message (format nil "rep %c %s: " type args) t))
  (remote-rep-while session 'busy type)
  (remote-rep-write session "%c%c" type (list-length args))
  (for-each (lambda (a)
	      (remote-rep-send-string session a)) args)
  (when output-fun
    (output-fun session))
  (remote-rep-while session 'busy type)
  (when remote-rep-display-progress
    (format t " %s" (vector-ref session remote-rep-status)))
  (remote-rep-error-if-unsuccessful session type args))

;; Return t if successful, else signal a file-error
(defun remote-rep-error-if-unsuccessful (session #!optional type args)
  (or (eq? (vector-ref session remote-rep-status) 'success)
      (signal 'file-error
	      (list (vector-ref session remote-rep-error)
		    type
		    (format nil "%s@%s %s"
			    (vector-ref session remote-rep-user)
			    (vector-ref session remote-rep-host) args)))))

(defun remote-rep-read-length (string point)
  (when (>= (string-length string) (+ point 8))
    ;; unrolled eight-digit hex decoder
    (+ (string-ref remote-rep-hex-map (string-ref string (+ point 7)))
       (ash (string-ref remote-rep-hex-map (string-ref string (+ point 6))) 4)
       (ash (string-ref remote-rep-hex-map (string-ref string (+ point 5))) 8)
       (ash (string-ref remote-rep-hex-map (string-ref string (+ point 4))) 12)
       (ash (string-ref remote-rep-hex-map (string-ref string (+ point 3))) 16)
       (ash (string-ref remote-rep-hex-map (string-ref string (+ point 2))) 20)
       (ash (string-ref remote-rep-hex-map (string-ref string (+ point 1))) 24)
       (ash (string-ref remote-rep-hex-map (string-ref string point)) 28))))

;; returns nil or STRING
(defun remote-rep-read-string (string point)
  (let
      ((len (remote-rep-read-length string point)))
    (when (and len (>= (string-length string) (+ point 8 len)))
      (substring string (+ point 8) (+ point 8 len)))))
    
(defun remote-rep-output-filter (session output)
  (when (vector-ref session remote-rep-pending-output)
    (setq output (concat (vector-ref session remote-rep-pending-output) output))
    (vector-set! session remote-rep-pending-output nil))
  (when remote-rep-echo-output
    (let
	((*print-escape* t))
      (format (stderr-file) "rep output: %S\n" output)))
  (if (vector-ref session remote-rep-callback)
      ((vector-ref session remote-rep-callback) session output 0)
    (let
	((point 0))
      (while (< point (string-length output))
	(cond ((and (null? (vector-ref session remote-rep-protocol))
		    (string-match remote-rep-passwd-msgs output point))
	       ;; Send password
	       (remote-rep-write
		session "%s\n"
		(let
		    ((pass (remote-rep-get-passwd
			    (vector-ref session remote-rep-user)
			    (vector-ref session remote-rep-host))))
		  (unless pass
		    (remote-rep-close-session session)
		    (error "No valid password"))
		  (vector-set! session remote-rep-login-data pass)
		  pass))
	       (setq point (string-length output)))
	      ((string-match remote-rep-signature output point)
	       (vector-set! session remote-rep-protocol
		     (string->number (expand-last-match "\\1")))
	       (setq point (match-end)))
	      ((= (string-ref output point) remote-rep-success)
	       ;; success
	       (vector-set! session remote-rep-status 'success)
	       (setq point (1+ point)))
	      ((= (string-ref output point) remote-rep-failure)
	       ;; failure, look for error message
	       (let
		   ((msg (remote-rep-read-string output (1+ point))))
		 (if msg
		     (progn
		       (vector-set! session remote-rep-error msg)
		       (vector-set! session remote-rep-status 'failure)
		       (setq point (+ point 9 (string-length msg))))
		   (vector-set! session remote-rep-pending-output
				(substring output point))
		   (setq point (string-length output)))))
	      (t
;	       (unless (string-looking-at "\\s*$" output point)
;		 (format *standard-error* "remote-rep: unhandled output %S\n"
;			 (substring output point)))
	       (setq point (string-length output))))))))

(defun remote-rep-sentinel (process)
  (let
      ((session (remote-rep-get-session-by-process process)))
    (unless (process-in-use? process)
      (vector-set! session remote-rep-process nil)
      (vector-set! session remote-rep-dircache nil)
      (vector-set! session remote-rep-status nil)
      (vector-set! session remote-rep-pending-output nil)
      (vector-set! session remote-rep-callback nil)
      (setq remote-rep-sessions (delq! session remote-rep-sessions)))))


;; Commands

;; SESSION has been started, wait for the connection to
;; succeed or fail
(defun remote-rep-connect (session)
  (remote-rep-while session 'busy 'connect)
  (remote-rep-error-if-unsuccessful session "connect")
  (unless (>= (vector-ref session remote-rep-protocol) remote-rep-required-protocol)
    (error "rep-remote program on %s is too old"
	   (vector-ref session remote-rep-host))))

(defun remote-rep-get (session remote-file local-file)
  (let
      ((remote-rep-get-fh (open-file local-file 'write))
       (remote-rep-len nil))
    (when remote-rep-get-fh
      (unwind-protect
	  (progn
	    (vector-set! session remote-rep-callback
		  (lambda (session output point)
		    (unless remote-rep-len
		      (cond ((= (string-ref output point) remote-rep-success)
			     ;; success
			     (let
				 ((len (remote-rep-read-length
					output (1+ point))))
			       (if len
				   (progn
				     (setq remote-rep-len len)
				     (setq point (+ point 9)))
				 ;; wait for next output
				 (vector-set! session remote-rep-pending-output
				       (substring output point)))))
			    ((= (string-ref output point) remote-rep-failure)
			     ;; failure
			     (let
				 ((msg (remote-rep-read-string
					output (1+ point))))
			       (if msg
				   (progn
				     (vector-set! session
					   remote-rep-status 'failure)
				     (vector-set! session remote-rep-error msg))
				 (vector-set! remote-rep-pending-output
				       (substring output point)))))))
		    (when remote-rep-len
		      (let
			  ((this (min remote-rep-len
				      (- (string-length output) point))))
			(write remote-rep-get-fh
			       (substring output point (+ point this)))
			(setq remote-rep-len (- remote-rep-len this))
			(setq point (+ point this)))
		      (when (zero? remote-rep-len)
			(vector-set! session remote-rep-status 'success)))))
	    (unwind-protect
		(remote-rep-command session #\G nil remote-file)
	      (vector-set! session remote-rep-callback nil)))
	(close-file remote-rep-get-fh)))))

(defun remote-rep-put (session local-file remote-file)
  (unwind-protect
      (remote-rep-command session #\P
			  (lambda (session)
			    (let
				((len (file-size local-file))
				 (fh (open-file local-file 'read)))
			      (when fh
				(unwind-protect
				    (progn
				      (remote-rep-send-int session len)
				      (copy-stream
				       fh (vector-ref session remote-rep-process)))
				  (close-file fh)))))
			  remote-file)
    (remote-rep-invalidate-directory
     session (file-name-directory remote-file))))

(defun remote-rep-rm (session remote-file)
  (unwind-protect
      (remote-rep-command session #\R nil remote-file)
    (remote-rep-invalidate-directory
     session (file-name-directory remote-file))))

(defun remote-rep-mv (session old-name new-name)
  (unwind-protect
      (remote-rep-command session #\M nil old-name new-name)
    (remote-rep-invalidate-directory
     session (file-name-directory old-name))
    (remote-rep-invalidate-directory
     session (file-name-directory new-name))))

(defun remote-rep-rmdir (session remote-dir)
  (unwind-protect
      (remote-rep-command session #\r nil remote-dir)
    (remote-rep-invalidate-directory
     session (file-name-directory remote-dir))))

(defun remote-rep-mkdir (session remote-dir)
  (unwind-protect
      (remote-rep-command session #\m nil remote-dir)
    (remote-rep-invalidate-directory
     session (file-name-directory remote-dir))))

(defun remote-rep-chmod (session mode file)
  (unwind-protect
      (remote-rep-command session #\c nil file (format nil "%x" mode))
    (remote-rep-invalidate-directory
     session (file-name-directory file))))

(defun remote-rep-make-symlink (session file contents)
  (unwind-protect
      (remote-rep-command session #\L nil contents file)
    (remote-rep-invalidate-directory
     session (file-name-directory file))))

(defun remote-rep-read-symlink (session file)
  (let
      (remote-rep-link)
    (vector-set! session remote-rep-callback
	  (lambda (session output point)
	    (cond ((= (string-ref output point) remote-rep-success)
		   ;; success
		   (setq remote-rep-link
			 (remote-rep-read-string
			  output (1+ point)))
		   (if remote-rep-link
		       (vector-set! session remote-rep-status 'success)
		     (vector-set! session remote-rep-pending-output
			   (substring output point))))
		  ((= (string-ref output point) remote-rep-failure)
		   (let
		       ((msg (remote-rep-read-string
			      output (1+ point))))
		     (if msg
			 (progn
			   (vector-set! session remote-rep-status 'failure)
			   (vector-set! session remote-rep-error msg))
		       (vector-set! session remote-rep-pending-output
			     (substring output point))))))))
    (unwind-protect
	(remote-rep-command session #\l nil file)
      (vector-set! session remote-rep-callback nil))
    remote-rep-link))


;; Directory handling/caching

(defconst remote-rep-file-name 0)
(defconst remote-rep-file-size 1)
(defconst remote-rep-file-modtime 2)
(defconst remote-rep-file-type 3)
(defconst remote-rep-file-modes 4)
(defconst remote-rep-file-mode-string 5)
(defconst remote-rep-file-nlinks 6)
(defconst remote-rep-file-user 7)
(defconst remote-rep-file-group 8)
(defconst remote-rep-file-struct-size 9)

(defconst remote-rep-cache-dir 0)
(defconst remote-rep-cache-expiry 1)
(defconst remote-rep-cache-entries 2)
(defconst remote-rep-cache-struct-size 3)

(defun remote-rep-file-owner-p (session file)
  (string=? (vector-ref session remote-rep-user)
	   (vector-ref file remote-rep-file-user)))

(defun remote-rep-dir-cached-p (session dir)
  (setq dir (directory-file-name dir))
  (catch 'exit
    (for-each (lambda (dir-entry)
		(when (string=? (vector-ref dir-entry remote-rep-cache-dir) dir)
		  (throw 'exit dir-entry)))
	      (vector-ref session remote-rep-dircache))))

(defun remote-rep-get-file (session filename)
  (let
      ((dir (file-name-directory filename))
       (base (file-name-nondirectory filename))
       entry)
    (when (string=? base "")
      ;; hack, hack
      (setq base (file-name-nondirectory dir)
	    dir (file-name-directory dir))
      (when (string=? base "")
	(setq base ".")))
    (setq dir (directory-file-name dir))
    (setq entry (remote-rep-dir-cached-p session dir))
    (if (not (and entry (> (vector-ref entry remote-rep-cache-expiry)
			   (current-time))))
	(progn
	  ;; Cache directory DIR
	  (when entry
	    (vector-set! session remote-rep-dircache
		  (delq! entry (vector-ref session remote-rep-dircache)))
	    (setq entry nil))
	  (remote-rep-while session 'busy 'dircache)
	  (when (>= (list-length (vector-ref session remote-rep-dircache))
		    remote-rep-dircache-max-dirs)
	    ;; delete the least-recently-used entry
	    (set-cdr! (list-tail (vector-ref session remote-rep-dircache)
			    (1- (list-length (vector-ref session remote-rep-dircache)))) nil))
	  ;; add the new (empty) entry for the directory to be read.
	  (setq entry (vector dir (+ (current-time)
				     remote-rep-dircache-expiry-time) nil))
	  (vector-set! session remote-rep-dircache
		(cons entry (vector-ref session remote-rep-dircache)))
	  ;; construct the callback function to have the new cache entry
	  ;; as the first argument
	  (vector-set! session remote-rep-callback
		(lambda (#!rest args)
		  (apply remote-rep-dircache-callback entry args)))
	  (unwind-protect
	      (condition-case nil
		  (remote-rep-command session #\D nil dir)
		(file-error))
	    (vector-set! session remote-rep-callback nil)))
      ;; entry is still valid, move it to the front of the list
      (vector-set! session remote-rep-dircache
	    (cons entry (delq! entry (vector-ref session remote-rep-dircache)))))
    ;; ENTRY now has the valid dircache directory structure
    (catch 'return
      (for-each (lambda (f)
		  (when (string=? (vector-ref f remote-rep-file-name) base)
		    (throw 'return f)))
		(vector-ref entry remote-rep-cache-entries))
      nil)))

;; similar to remote-rep-get-file, but symbolic links are followed
(defun remote-rep-lookup-file (session file)
  (let
      ((file-struct (remote-rep-get-file session file)))
    (while (and file-struct
		(eq? (vector-ref file-struct remote-rep-file-type) 'symlink))
      (let
	  ((link (remote-rep-read-symlink session file)))
	(setq file (expand-file-name link (file-name-directory file)))
	(setq file-struct (remote-rep-get-file session file))))
    file-struct))

(defun remote-rep-dircache-callback (cache-entry session output point)
  (catch 'done
    (while (string-match "\002" output point)
      (let*
	  ((start (match-end))
	   (text (remote-rep-read-string output start))
	   file-struct)
	(if text
	    (progn
	      (setq file-struct (read-from-string text))
	      (unless (vector? file-struct)
		(error "file-struct isn't a vector!: %S" file-struct))
	      (vector-set! cache-entry remote-rep-cache-entries
		    (cons file-struct
			  (vector-ref cache-entry remote-rep-cache-entries)))
	      (setq point (+ start 8 (string-length text))))
	  (throw 'done t)))))
  (when (< point (string-length output))
    (cond ((= (string-ref output point) remote-rep-success)
	   ;; success marker
	   (vector-set! session remote-rep-status 'success))
	  ((= (string-ref output point) remote-rep-failure)
	   ;; failure
	   (let
	       ((msg (remote-rep-read-string output (1+ point))))
	     (if msg
		 (progn
		   (vector-set! session remote-rep-status 'failure)
		   (vector-set! session remote-rep-error msg))
	       ;; save output for next time
	       (vector-set! session
		     remote-rep-pending-output (substring output point)))))
	  (t
	   ;; some output to save for next time
	   (vector-set! session
		 remote-rep-pending-output (substring output point))))))

(defun remote-rep-invalidate-directory (session directory)
  (setq directory (directory-file-name directory))
  (let
      ((entry (remote-rep-dir-cached-p session directory)))
    (when entry
      (vector-set! session remote-rep-dircache
	    (delq! entry (vector-ref session remote-rep-dircache))))))

(defun remote-rep-empty-cache ()
  "Discard all cached rep-remote directory entries."
  (interactive)
  (for-each (lambda (ses)
	      (vector-set! ses remote-rep-dircache nil)) remote-rep-sessions))


;; Password caching

(defun remote-rep-get-passwd (user host)
  (let*
      ((joined (concat user #\@ host))
       (cell (assoc joined remote-rep-passwd-alist)))
    (if cell
	(cdr cell)
      (pwd-prompt (concat "Password for " joined #\:)))))

(defun remote-rep-add-passwd (user host passwd)
  "Add the string PASSWD as the password for rep-remote session of USER@HOST."
  (interactive "sUsername:\nsHost:\nPassword for %s@%s:")
  (let
      ((joined (concat user #\@ host)))
    (catch 'foo
      (for-each (lambda (cell)
		  (when (string=? (car cell) joined)
		    (set-cdr! cell passwd)
		    (throw 'foo)))
		remote-rep-passwd-alist)
      (setq remote-rep-passwd-alist (cons (cons joined passwd)
					  remote-rep-passwd-alist)))))


;; Backend handler

(defun remote-rep-handler (split-name op args)
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
      (apply (symbol-value op) (file-bound-stream (car args)) (cdr args)))
     ((eq? op 'close-file)
      ;; Close the file, synchronise with the remote file if required
      (let*
	  ((file (car args))
	   (data (file-handler-data file))
	   (session (remote-rep-open-host (list-ref split-name 1)
					  (car split-name))))
	(close-file (file-bound-stream file))
	(when (memq (vector-ref data 1) '(write append))
	  ;; Copy the local version back to the remote fs
	  (remote-rep-put session (vector-ref data 3) (vector-ref data 2)))
	(delete-file (vector-ref data 3))))
     (t
      (error "Unsupported rep-remote op on file-handler: %s %s" op args))))
   ((memq op '(read-file-contents insert-file-contents copy-file-to-local-fs))
    ;; Need to get the file to the local fs
    (let
	((local-name (if (eq? op 'copy-file-to-local-fs)
			 (list-ref args 1)
		       (make-temp-name)))
	 (session (remote-rep-open-host (list-ref split-name 1) (car split-name))))
      (remote-rep-get session (list-ref split-name 2) local-name)
      (if (eq? op 'copy-file-to-local-fs)
	  (set-file-modes local-name (remote-rep-handler split-name
							 'file-modes
							 (list (car args))))
	(unwind-protect
	    ((symbol-value op) local-name)
	  (delete-file local-name)))
      t))
   ((memq op '(write-buffer-contents copy-file-from-local-fs))
    ;; Need to get the file off the local fs
    (let
	((local-name (if (eq? op 'copy-file-from-local-fs)
			 (car args)
		       (make-temp-name)))
	 (session (remote-rep-open-host (list-ref split-name 1) (car split-name))))
      (unless (eq? op 'copy-file-from-local-fs)
	(apply (symbol-value op) local-name (cdr args)))
      (unwind-protect
	  (remote-rep-put session local-name (list-ref split-name 2))
	(if (eq? op 'copy-file-from-local-fs)
	    (remote-rep-chmod
	     session (file-modes local-name) (list-ref split-name 2))
	  (delete-file local-name)))
      t))
   ((eq? op 'copy-file)
    ;; Copying on the remote fs.
    ;; XXX For intra-session remote copies use the rep-remote copy command
    (let
	((local-file (make-temp-name))
	 (dest-split (remote-split-filename (list-ref args 1))))
      (unwind-protect
	  (and (remote-rep-handler split-name 'copy-file-to-local-fs
				   (list (car args) local-file))
	       (remote-rep-handler dest-split 'copy-file-from-local-fs
				   (list local-file (list-ref args 1))))
	(and (file-exists? local-file)
	     (delete-file local-file)))))
   ((eq? op 'rename-file)
    (let
	((session (remote-rep-open-host (list-ref split-name 1) (car split-name)))
	 (dest-split (remote-split-filename (list-ref args 1))))
      (or (and (string=? (car dest-split) (car split-name))
	       (string=? (list-ref dest-split 1) (list-ref split-name 1)))
	  (error "Can't rename files across rep sessions"))
      (remote-rep-mv session (list-ref split-name 2) (list-ref dest-split 2))))
   (t
    ;; All functions taking a single argument
    (let
	((session (remote-rep-open-host (list-ref split-name 1) (car split-name)))
	 (file-name (list-ref split-name 2)))
      (cond
       ((eq? op 'directory-files)
	(let
	    ;; XXX this assumes local/remote have same naming structure!
	    ((dir (file-name-as-directory file-name)))
	  (remote-rep-lookup-file session dir)
	  (map (lambda (f)
		 (vector-ref f remote-rep-file-name))
	       (vector-ref (remote-rep-dir-cached-p session dir)
		     remote-rep-cache-entries))))
       ((eq? op 'delete-file)
	(remote-rep-rm session file-name))
       ((eq? op 'delete-directory)
	(remote-rep-rmdir session file-name))
       ((eq? op 'make-directory)
	(remote-rep-mkdir session file-name))
       ((eq? op 'set-file-modes)
	(remote-rep-chmod session (list-ref args 1) file-name))
       ((eq? op 'make-symlink)
	(remote-rep-make-symlink session file-name (list-ref args 1)))
       ((eq? op 'read-symlink)
	(remote-rep-read-symlink session file-name))
       ((eq? op 'open-file)
	(let
	    ((type (list-ref args 1))
	     (local-file (make-temp-name))
	     local-fh)
	  (when (memq type '(read append))
	    ;; Need to transfer the file initially
	    (remote-rep-get session file-name local-file))
	  ;; Open the local file
	  (setq local-fh (make-file-from-stream (car args)
						(open-file local-file type)
						'remote-file-handler))
	  (set-file-handler-data local-fh
				 (vector remote-rep-handler
					 type		;access type
					 file-name	;remote name
					 local-file))	;local copy
	  (remote-register-file-handle local-fh)
	  local-fh))
       (t
	(let
	    ((file (if (eq? op 'file-symlink?)
		       (remote-rep-get-file session file-name)
		     (remote-rep-lookup-file session file-name))))
	  (cond
	   ((eq? op 'file-exists?)
	    file)
	   ((eq? op 'file-regular?)
	    (and file (eq? (vector-ref file remote-rep-file-type) 'file)))
	   ((eq? op 'file-directory?)
	    (and file (eq? (vector-ref file remote-rep-file-type) 'directory)))
	   ((eq? op 'file-symlink?)
	    (and file (eq? (vector-ref file remote-rep-file-type) 'symlink)))
	   ((eq? op 'file-size)
	    (and file (vector-ref file remote-rep-file-size)))
	   ((eq? op 'file-modes)
	    (and file (vector-ref file remote-rep-file-modes)))
	   ((eq? op 'file-modes-as-string)
	    (and file (vector-ref file remote-rep-file-mode-string)))
	   ((eq? op 'file-nlinks)
	    (and file (vector-ref file remote-rep-file-nlinks)))
	   ((eq? op 'file-modtime)
	    (if file (vector-ref file remote-rep-file-modtime) (cons 0 0)))
	   ((eq? op 'file-owner?)
	    (and file (remote-rep-file-owner-p session file)))
	   ((eq? op 'file-readable?)
	    (and file (/= (logand (vector-ref file remote-rep-file-modes)
				  (if (remote-rep-file-owner-p session file)
				      #o400 #o004)) 0)))
	   ((eq? op 'file-writable?)
	    (and file (/= (logand (vector-ref file remote-rep-file-modes)
				  (if (remote-rep-file-owner-p session file)
				      #o200 #o002)) 0)))
	   (t
	    (error "Unsupported rep-remote op: %s %s" op args))))))))))

;;;###autoload (put 'rep 'remote-backend 'remote-rep-handler)

;;;###autoload (autoload-file-handler 'remote-rep-handler 'rep.io.file-handlers.remote.rep)

(define-file-handler 'remote-rep-handler remote-rep-handler))
