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

(defun backend-request-getenv (name)
  (sb-fastcgi:fcgx-getparam *req* (string name)))

(defun backend-request-method ()
  (sb-fastcgi:fcgx-getparam *req* :request_method))

(defun backend-request-header (name)
  (sb-fastcgi:fcgx-getparam *req* (str "HTTP_" (string-upcase name))))

(defun backend-request-uri ()
  (sb-fastcgi:fcgx-getparam *req* :document_uri))

(defun backend-request-remote-addr ()
  (sb-fastcgi:fcgx-getparam *req* :remote_addr))

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

(defun backend-write-headers (headers)
  (sb-fastcgi:fcgx-puts *req* headers))

(defun backen-write-content (content)
  (sb-fastcgi::fcgx-putchars *req* content))

;;  Running

(defun backend-run ()
  (log-msg :info "starting fastcgi at 127.0.0.1:~A" *port*)
  (sb-fastcgi:socket-server (lambda (req)
			      (let ((*req* req))
				(route-request)))
			    :inet-addr "127.0.0.1"
			    :port *port*)
  (error "fastcgi socket server exited"))

;;  I/O

(defclass fcgi-stream (trivial-gray-stream-mixin)
  ((req :initarg :req)
   (pos :initform 0)))

(defmethod stream-write-sequence ((stream fcgi-stream)
				  (sequence sequence)
				  start
				  end
				  &key)
  (stream-write-sequence stream
			 (subseq sequence
				 (or start 0)
				 end)
			 0 nil))

(defmethod stream-write-sequence ((stream fcgi-stream)
				  (sequence simple-base-string)
				  (start (eql 0))
				  (end (eql nil))
				  &key)
  (with-slots (req pos) stream
    (sb-fastcgi:fcgx-puts req sequence)
    (incf pos (length sequence))))

(defmethod stream-write-sequence ((stream fcgi-stream)
				  (sequence string)
				  (start (eql 0))
				  (end (eql nil))
				  &key)
  (with-slots (req pos) stream
    (sb-fastcgi:fcgx-puts-utf-8 req sequence)
    (incf pos (length sequence))))
