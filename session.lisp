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

(define-resource session
  (random-id :length 32)
  (has-one ctime)
  (has-one atime)
  (has-one key)
  (has-one remote-addr)
  (has-one user-agent))

(defun session-is-valid (session)
  (let ((atime (session.atime session)))
    (and atime
	 (< (get-universal-time)
	    (+ *session-timeout* atime)))))

(defun session-is-secure (session)
  (and (string= (session.remote-addr session) (request-remote-addr))
       (string= (session.user-agent session) (request-header :user_agent))))

(defun session-touch (session)
  (setf (session.atime session) (get-universal-time)))

(defun session-delete (s)
  (when (facts:bound-p ((s :is-a 'session)))
    (facts:rm ((s ?p ?o)))
    t))

(defun session-gc ()
  (facts:with ((?s :is-a 'session))
    (unless (session-is-valid ?s)
      (session-delete ?s))))

;;  Cookie API

(defun session-create ()
  "Return a new session"
  (session-gc)
  (facts:with-transaction
    (let* ((time (get-universal-time))
	   (session (add-session 'session.ctime time
				 'session.atime time
				 'session.key (make-random-string 32)
				 'session.remote-addr (request-remote-addr)
				 'session.user-agent (request-header :user_agent))))
      (set-cookie *session-cookie* (session.id session)
		  (+ *session-timeout* (get-universal-time)))
      (setq *session* session))))

(defun session-end ()
  (setq *session* nil)
  (delete-cookie *session-cookie*))

(defun session-attach ()
  (or *session*
      (when-let ((session (find-session (cookie-value *session-cookie*))))
	(if (and (session-is-valid session)
		 (session-is-secure session))
	    (progn
	      (session-touch session)
	      (setf *session* session)
	      session)
	    (session-end)))))

(defun session ()
  (or (session-attach) (session-create)))

(defun session-reset ()
  (when *session*
    (let ((old-sid (session.id *session*))
	  (new-sid (make-sid)))
      (setf (session.id *session*) new-sid)
      (set-cookie *session-cookie* (symbol-name new-sid)
		  (+ *session-timeout* (get-universal-time)))
      (session-delete old-sid))))

(defun session-hmac (&rest parts)
  (apply #'hmac-string (session.key (session)) parts))
