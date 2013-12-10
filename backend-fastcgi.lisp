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

#+openbsd
(handler-case
    (sb-fastcgi:load-libfcgi "/usr/local/lib/libfcgi.so.0.0")
  (error (e)
    (format t "~%~%~A~%~%Could not load libfcgi, 'pkg_add fcgi' maybe ?~%~%" e)
    (sb-ext:exit :code 1)))

;;  Request

(defvar *req* nil)

(defun backend-request-env ()
  (sb-fastcgi:fcgx-getenv *req*))

(defun backend-request-method ()
  (sb-fastcgi:fcgx-getparam *req* "REQUEST_METHOD"))

(defun backend-request-header (name)
  (sb-fastcgi:fcgx-getparam *req* (str "HTTP_" (string-upcase name))))

(defun backend-request-uri ()
  (sb-fastcgi:fcgx-getparam *req* "DOCUMENT_URI"))

(defun backend-request-remote-addr ()
  (sb-fastcgi:fcgx-getparam *req* "REMOTE_ADDR"))

;;  Forms

(defun backend-read-request-data ()
  (let ((data (sb-fastcgi:fcgx-read-all *req*)))
    (trivial-utf-8:utf-8-bytes-to-string
     (apply #'concatenate data))))

(defun backend-read-form-data ()
  (let ((content-type (sb-fastcgi:fcgx-getparam *req* "CONTENT_TYPE")))
    (cond ((string-equal "application/x-www-form-urlencoded" content-type)
	   (parse-www-form-url-encoded (backend-read-request-data))))))

;;  Reply

(defgeneric backend-send (data))

(defmethod backend-send ((data string))
  (sb-fastcgi:fcgx-puts *req* data)
  (values))

(defmethod backend-send ((data array))
  (sb-fastcgi:fcgx-putchars *req* data)
  (values))

;;  Reply headers

(defun status (&rest msg)
  (apply #'header 'status msg))

(defun backend-header (name &rest parts)
  (backend-send (string-capitalize name))
  (backend-send ": ")
  (walk-str #'backend-send parts)
  (backend-send +crlf+))

(defun backend-send-headers ()
  (backend-send +crlf+))

;;  Running

(defvar *headers-output*)

(defun backend-run ()
  (flet ((fastcgi-request (req)
	   (let ((*req* req))
	     (route-request))))
    (log-msg :info "starting fastcgi at 127.0.0.1:~A" *port*)
    (sb-fastcgi:socket-server #'fastcgi-request
			      :inet-addr "127.0.0.1"
			      :port *port*)
    (error "fastcgi socket server exited")))
