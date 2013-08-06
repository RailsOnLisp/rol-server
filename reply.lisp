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

(defun transform-http-content (byte)
  byte)

(trace transform-http-content)

(defclass reply ()
  ((headers-stream :type stream
		   :initarg :headers-stream
		   :initform (make-string-output-stream :element-type 'base-char)
		   :reader reply-headers-stream)
   (content-stream :type stream
		   :initarg :content-stream
		   :initform (flexi-streams:make-flexi-stream
			      (flexi-streams:make-in-memory-output-stream)
			      :external-format :utf-8)
		   :reader reply-content-stream)))

(defmacro with-reply ((reply) &body body)
  `(let* ((,reply (make-instance 'reply))
	  (*headers-output* (reply-headers-stream ,reply))
	  (*standard-output* (reply-content-stream ,reply)))
     ,@body))

(defun reply-get-headers (reply)
  (get-output-stream-string
    (reply-headers-stream reply)))

(defun reply-get-output (reply)
  (flexi-streams:get-output-stream-sequence
   (flexi-streams:flexi-stream-stream
    (reply-content-stream reply))))
