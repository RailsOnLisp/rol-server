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

(defun reply-send ()
  (unless *reply-sent*
    (setq *reply-sent* t)
    (let ((content (flexi-streams:get-output-stream-sequence
		    (flexi-streams:flexi-stream-stream
		     *standard-output*))))
      (content-length (length content))
      (let ((headers (backend-send-headers)))
	(backend-send-body content)
	(when (find :reply *debug*)
	  (log-msg :debug "REPLY: ~A~&~A" headers *reply*))))
    *reply*))

(defmacro with-reply-handlers (&body body)
  `(handler-bind ((warning
		   (lambda (w)
		     (log-msg :warn "~A" w)
		     (muffle-warning w)))
		  (condition
		   (lambda (c)
		     (unless *reply-sent*
		       (let ((status (http-error-status c)))
			 (log-msg (if (char= #\5 (char status 0)) :error :info)
				  "~A" c)
			 (render-error status (http-error-message c)))
		       (reply-send)))))
     ,@body))

(defmacro with-reply (&body body)
  `(let* ((*reply* nil)
	  (*reply-sent* nil)
	  #-hunchentoot
	  (*headers-output* (make-string-output-stream :element-type 'base-char))
	  (*standard-output* (flexi-streams:make-flexi-stream
			      (flexi-streams:make-in-memory-output-stream)
			      :external-format :utf-8)))
     #+hunchentoot
     (setf hunchentoot:*catch-errors-p* (not (find :reply *debug*)))
     (with-reply-handlers ,@body)
     (reply-send)
     *reply*))
