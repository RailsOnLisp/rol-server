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

(defun session-is-valid (session)
  (facts:let-with ((atime (session :atime ?)))
    (and atime
	 (< (get-universal-time)
	    (+ *session-timeout* atime)))))

(defun session-is-secure (session)
  (facts:bound-p ((session :client-address (cgi-env "REMOTE_ADDR"))
		  (session :user-agent (cgi-env "HTTP_USER_AGENT")))))
		 
(defun session-gc ()
  (facts:with-transaction
    (facts:with ((?session :is-a :session))
      (unless (session-is-valid ?session)
	(facts:rm ((?session ?p ?o)))))))

(defun session-create ()
  "Return a new session id"
  (session-gc)
  (facts:with-transaction
    (let ((session (do ((sess #1=(make-random-string 171) #1#))
		       ((null (or (facts:bound-p ((sess ?p ?o))))
			s)))
	  (time (get-universal-time)))
      (facts:add (session :is-a :session)
		 (session :client-address (cgi-env "REMOTE_ADDR"))
		 (session :user-agent (cgi-env "HTTP_USER_AGENT"))
		 (session :ctime time)
		 (session :atime time))
      (set-cookie *session-cookie* session (+ *session-timeout*
					      (get-universal-time)))
      (setf *session* session))))

(defun session-attach ()
  (when-let ((session (cookie-value *session-cookie*)))
    (facts:bound-p ((session :is-a :session))
		   