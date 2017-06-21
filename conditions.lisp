;;
;;  RoL-server  -  Rails on Lisp application server
;;
;;  Copyright 2012-2015 Thomas de Grivel <thomas@lowh.net>
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

(defgeneric http-error-status (condition))
(defgeneric http-error-message (condition))

(defmethod http-error-status (condition)
  "500 Internal server error")

(defmethod http-error-message (condition)
  (format nil "~A: ~A" (type-of condition) condition))

(define-condition http-error (error)
  ((status :type string :initarg :status)
   (message :type string :initarg :message)))

(defmethod http-error-status ((e http-error))
  (slot-value e 'status))

(defmethod http-error-message ((e http-error))
  (slot-value e 'message))

(defun http-error (status msg-fmt &rest args)
  (error 'http-error
         :status status
         :message (if args
                      (apply 'format nil msg-fmt args)
                      msg-fmt)))

(defmethod print-object ((e http-error) stream)
  (if (and *print-pretty* (not *print-readably*))
      (format stream "~A~%~A"
              (http-error-status e) (http-error-message e))
      (print `(http-error ,(http-error-status e)
                          ,(http-error-message e))
             stream)))
