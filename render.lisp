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

(in-package :lowh-triangle-server)

(defun render-headers (&optional (headers *headers*))
  (with-output-to-string (str)
    (dolist (h headers)
      (format str "~A: ~A~%"
	      (string-capitalize (car h))
	      (cdr h)))
    (format str "~%")))

(defun render-text (text)
  (setf (reply-header :content-type) "text/plain")
  text)

(defun render-error (status &optional (msg ""))
  (setf (reply-header :status) status)
  (let ((body (when *debug*
		(format nil "~A~%~%~A" msg
			(sb-fastcgi:fcgx-getenv *req*)))))
    (render-text (format nil "~A~%~@[~%~A~%~]"
			 status body))))
