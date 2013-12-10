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

(define-condition http-error (error)
  ((status :type string :reader http-error-status :initarg :message)
   (message :type string :reader http-error-message :initarg :message)))

(defmethod http-error-status (condition)
  "500 Internal server error")

(defmethod http-error-message (condition)
  (format nil "~A: ~A" (type-of condition) condition))

(defun http-error (status msg-fmt &rest args)
  (signal 'http-error
	  :status status
	  :message (if args
		       (apply 'format nil msg-fmt args)
		       msg-fmt)))

(defmethod print-object ((o http-error) stream)
  (if (and *print-pretty* (not *print-readably*))
      (format stream "~A~%~A"
	      (http-error-status o) (http-error-message o))
      (format stream "(http-error ~S ~S)"
	      (http-error-status o) (http-error-message o))))
