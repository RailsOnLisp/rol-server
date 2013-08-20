;;
;;  LowH Triangle Server
;;
;;  Copyright 2012 Thomas de Grivel <billitch@gmail.com>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :lowh.triangle.server)

;;  Session

(defstruct session
  id ctime atime key remote-addr user-agent)

(defun session-is-valid (session)
  (with-slots (atime) session
    (and atime
	 (< (get-universal-time)
	    (+ *session-timeout* atime)))))

(defun session-is-secure (session)
  (with-slots (remote-addr user-agent) session
    (and (string= remote-addr (request-remote-addr))
	 (string= user-agent (request-header :user_agent)))))

;;  Session ID

(defpackage :lowh.triangle.server.sessions
  (:nicknames :L>server.sessions))

(defun make-sid ()
  (do ((sid #1=(make-random-string 171) #1#))
      ((not (session-find sid))
       (intern sid :L>server.sessions))))

(defun session-delete (s)
  (etypecase s
    (session (session-delete (session-id s)))
    (symbol (makunbound s)
	    (unintern s :L>server.sessions))))

(defun session-gc ()
  (do-symbols (sid :L>server.sessions)
    (unless (session-is-valid (symbol-value sid))
      (session-delete sid))))

(defun session-find (sid)
  (when sid
    (let ((sym (find-symbol sid :L>server.sessions)))
      (when sym
	(symbol-value sym)))))

;;

(defun session-create ()
  "Return a new session"
  (session-gc)
  (facts:with-transaction
    (let* ((sid (make-sid))
	   (time (get-universal-time))
	   (session (make-session :id sid
				  :ctime time
				  :atime time
				  :key (make-random-string 64)
				  :remote-addr (request-remote-addr)
				  :user-agent (request-header :user_agent))))
      (set sid session)
      (set-cookie *session-cookie* sid (+ *session-timeout*
					  (get-universal-time)))
      (setf *session* session))))

(defun session-touch (session)
  (with-slots (atime) session
    (setf atime (get-universal-time))))

(defun session-attach ()
  (when-let ((session (session-find (cookie-value *session-cookie*))))
    (if (and (session-is-valid session)
	     (session-is-secure session))
	(progn
	  (session-touch session)
	  (setf *session* session)
	  session)
	(session-delete session))))

(defun session-attach-or-create ()
  (or (session-attach) (session-create)))

(defun session-hmac (&rest parts)
  (apply #'hmac-string (session-key *session*) parts))
