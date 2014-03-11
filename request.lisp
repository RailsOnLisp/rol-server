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

(define-constant +http-verbs+ #(:GET :POST :PUT :DELETE)
  :test 'equalp)

(defun http-verb (object)
  (find (string-upcase object)
	+http-verbs+
	:key #'symbol-name
	:test #'string=))

(defun request-method ()
  (let ((method (http-verb (backend-request-method))))
    (or (and (eq :POST method)
	     (http-verb (with-form-data (_method) _method)))
	method)))

(defun request-header (name)
  (backend-request-header name))

(defun request-remote-addr ()
  (backend-request-remote-addr))

(defun accept-p (&rest types)
  (cl-ppcre:scan `(:sequence :word-boundary
			     ,(if (cdr types)
				  `(:alternation ,@(mapcar #'string-upcase types))
				  (string-upcase (car types)))
			     :word-boundary)
		 (string-upcase (request-header :accept))))

(defun cookie-value (name)
  (when-let ((cookie (request-header :cookie)))
    (cl-ppcre:register-groups-bind (n value) ("([^=]+)=([^;]+)" cookie)
      (when (string= (str name) n)
	value))))

(defmacro with-request (&body body)
  `(let* ((*form-data* nil)
	  (*method* (request-method))
	  (*host* (request-header :host))
	  (*uri* (canonical-document-uri (backend-request-uri)))
	  (*session*))
     ,@body))
