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

;;  Config

#+openbsd
(handler-case
    (cl-fastcgi:load-libfcgi "/usr/local/lib/libfcgi.so.0.0")
  (error (e)
    (format t "~%~%~A~%~%Could not load libfcgi, 'pkg_add fcgi' maybe ?~%~%" e)
    (sb-ext:exit :code 1)))

;;  Request

(defvar *req*)

(defun backend-request-env ()
  (cl-fastcgi:fcgx-getenv *req*))

(defun backend-request-method ()
  (cl-fastcgi:fcgx-getparam *req* "REQUEST_METHOD"))

(defun backend-request-header (name)
  (let ((name (str "HTTP_" (nsubstitute #\_ #\- (string-upcase name)
					:test #'char=))))
    (cl-fastcgi:fcgx-getparam *req* name)))

(defun backend-request-uri ()
  (cl-fastcgi:fcgx-getparam *req* "DOCUMENT_URI"))

(defun backend-request-remote-addr ()
  (cl-fastcgi:fcgx-getparam *req* "REMOTE_ADDR"))

;;  Forms

(defun backend-read-request-data ()
  (let ((data (cl-fastcgi:fcgx-read-all *req*)))
    (babel:octets-to-string
     (apply #'concatenate 'array data))))

(defun backend-read-form-data ()
  (let ((content-type (string-downcase
		       (cl-fastcgi:fcgx-getparam *req* "CONTENT_TYPE"))))
    (cond ((cl-ppcre:scan "^application/x-www-form-urlencoded\\b" content-type)
	   (parse-www-form-url-encoded (backend-read-request-data)))
	  ((cl-ppcre:scan "^application/json\\b" content-type)
	   (parse-www-form-json-encoded (backend-read-request-data))))))

;;  Reply

(defgeneric backend-send (data))

(defmethod backend-send ((data string))
  (when (debug-p :reply)
    (log-msg :debug "SEND ~S" (if (eq +crlf+ data)
				  '+crlf+
				  data)))
  (cl-fastcgi:fcgx-puts *req* data)
  (values))

(defmethod backend-send ((data simple-array))
  (when (debug-p :reply)
    (log-msg :debug "SEND ~D bytes" (length data)))
  (cl-fastcgi:fcgx-puts *req* data)
  (values))

(defmethod backend-send ((data vector))
  (when (debug-p :reply)
    (log-msg :debug "SEND ~D bytes" (length data)))
  (cl-fastcgi:fcgx-puts *req* (make-array (length data)
					  :element-type '(unsigned-byte 8)
					  :initial-contents data))
  (values))

;;  Reply headers

(defun backend-status (&rest parts)
  (apply #'backend-header 'status parts))

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
    (cl-fastcgi:socket-server #'fastcgi-request
			      :inet-addr "127.0.0.1"
			      :port *port*)
    (error "fastcgi socket server exited")))
