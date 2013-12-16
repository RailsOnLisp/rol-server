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

;;  Config

(setf hunchentoot:*hunchentoot-default-external-format* :utf8)
(setf hunchentoot:*log-lisp-errors-p* t)
(setf hunchentoot:*log-lisp-backtraces-p* t)
(setf hunchentoot:*log-lisp-warnings-p* t)
(setf hunchentoot:*show-lisp-errors-p* t)
(defvar hunchentoot:*show-lisp-backtraces-p* t)

;;  Request

(defun backend-request-env ()
  (list :headers (hunchentoot:headers-in*)
	:get-parameters (hunchentoot:get-parameters*)
	:post-parameters (hunchentoot:post-parameters*)
	:cookies (hunchentoot:cookies-in*)))

(defun backend-request-method ()
  (hunchentoot:request-method*))

(defun backend-request-header (name)
  (hunchentoot:header-in* name))

(defun backend-request-uri ()
  (hunchentoot:request-uri*))

(defun backend-request-remote-addr ()
  (hunchentoot:remote-addr*))

;;  Forms

(defun backend-read-request-data ()
  (hunchentoot:raw-post-data))

(defun backend-read-form-data ()
  (hunchentoot:post-parameters*))

;;  Reply

(defun status (status-string)
  (setf (hunchentoot:return-code*) (parse-integer status-string
						  :junk-allowed t)))

(defun backend-header (name &rest parts)
  (setf (hunchentoot:header-out name)
	(apply #'str parts)))

(defun backend-send-headers ()
  (with-output-to-string (out)
    (mapc (lambda (h)
	    (format out "~A: ~A~A" (car h) (cdr h) +crlf+))
	  (hunchentoot:headers-out*))))

(defun backend-send (content)
  (trivial-utf-8:utf-8-bytes-to-string content))

;;  Running

(defclass triangle-acceptor (hunchentoot:acceptor)
  ())

(defmethod hunchentoot:acceptor-request-dispatcher ((hunchentoot:*acceptor*
						     triangle-acceptor))
  (lambda (hunchentoot:*request*)
    (let ((hunchentoot:*catch-errors-p* (not (find :reply *debug*))))
      (route-request))))

(defun backend-run ()
  (log-msg :info "starting hunchentoot at 127.0.0.1:~A" *port*)
  (hunchentoot:start (make-instance 'triangle-acceptor
				    :address "127.0.0.1"
				    :port *port*))
  (error "hunchentoot server exited"))