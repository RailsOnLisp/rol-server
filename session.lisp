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

(in-package :RoL-server)

;;  TODO: clean split between facts API and external API

;;  Session

(defvar *session-db*)
(defvar *session-db-path* #P"data/session")

(defun init-session-db ()
  (setq *session-db* (make-instance 'facts:db))
  (facts:transaction-var *session-db* '*session-db*))

(defmacro with-session-db (&body body)
  `(let ((facts:*db* *session-db*)
         (facts:*db-path* *session-db-path*))
     ,@body))

(define-resource session
  (random-id :length 32)
  (has-one ctime)
  (has-one atime)
  (has-one key)
  (has-one remote-addr)
  (has-one user-agent))

(defun session-is-valid (session)
  (with-session-db
    (let ((atime (session.atime session)))
      (and atime
           (< (get-universal-time)
              (+ *session-timeout* atime))))))

(defun session-is-secure (session)
  (with-session-db
    (and (string= (session.remote-addr session) (request-remote-addr))
         (string= (session.user-agent session) (request-header :user_agent)))))

(defun session-touch (session)
  (with-session-db
    (setf (session.atime session) (get-universal-time))))

(defun session-delete (s)
  (with-session-db
    (when (facts:bound-p ((s :is-a 'session)))
      (facts:rm ((s ?p ?o)))
      t)))

(defun session-gc ()
  (with-session-db
    (facts:with ((?s :is-a 'session))
      (unless (session-is-valid ?s)
        (session-delete ?s)))))

;;  Cookie API

(defun session-create ()
  "Return a new session"
  (with-session-db
    (session-gc)
    (facts:with-transaction
      (let* ((time (get-universal-time))
             (session (add-session 'session.ctime time
                                   'session.atime time
                                   'session.key (make-random-string 32)
                                   'session.remote-addr (request-remote-addr)
                                   'session.user-agent (request-header :user_agent))))
        (delete-cookie *session-cookie*)
        (set-cookie *session-cookie* (session.id session)
                    (+ *session-timeout* (get-universal-time)))
        (when (debug-p (or :app :session))
          (log-msg :INFO "New session ~S" (session.id session)))
        (setq *session* session)))))

;;  External API

(defun session-end ()
  (setq *session* nil)
  (delete-cookie *session-cookie*))

(defun session-attach ()
  (or *session*
      (with-session-db
        (when-let ((session (find-session (cookie-value *session-cookie*))))
          (if (and (session-is-valid session)
                   (session-is-secure session))
              (progn
                (session-touch session)
                (setf *session* session)
                (when (debug-p (or :app :session))
                  (log-msg :INFO "Session ~S" (session.id session)))
                session)
              (session-end))))))

(defun session ()
  (or (session-attach) (session-create)))

(defun session-reset ()
  (when *session*
    (with-session-db
      (let ((new-sid (make-session-id)))
        (setf (session.id *session*) new-sid)
        (when (debug-p (or :app :session))
          (log-msg :INFO "Session reset ~S" new-sid))
        (delete-cookie *session-cookie*)
        (set-cookie *session-cookie* new-sid
                    (+ *session-timeout* (get-universal-time)))))))

(defun session-hmac (&rest parts)
  (with-session-db
    (apply #'hmac-string (session.key (session)) parts)))
