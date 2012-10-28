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

(defun url-decode (string)
  (let ((out (make-array (length string) :element-type '(unsigned-byte 8)))
	(pos 0))
    (cl-ppcre:do-register-groups (unreserved plus percent hex)
	("([^%+]+)?(\\+)?(%([0123456789ABCDEFabcdef][0123456789ABCDEFabcdef])?)?"
	 string nil :sharedp t)
      (let ((percent (when (= 1 (length percent)) percent)))
	(when unreserved
	  (dotimes (i (length unreserved))
	    (setf (aref out pos) (char-code (char unreserved i)))
	    (incf pos)))
	(when plus
	  (setf (aref out pos) (char-code #\Space))
	  (incf pos))
	(when percent
	  (setf (aref out pos) (char-code #\%))
	  (incf pos))
	(when hex
	  (setf (aref out pos) (parse-integer hex :radix 16))
	  (incf pos))))
    (trivial-utf-8:utf-8-bytes-to-string out :end pos)))

(defun parse-www-form-url-encoded (string)
  (let (form-data)
    (cl-ppcre:do-register-groups (var value)
	("([^&=]+)(?:=([^&=]*))?" string nil :sharedp t)
      (push (cons (url-decode var) (url-decode value)) form-data))
    (nreverse form-data)))

(defun read-request-data ()
  (let ((data (sb-fastcgi:fcgx-read-all *req*)))
    (trivial-utf-8:utf-8-bytes-to-string
     (apply #'concatenate data))))

(defun read-form-data ()
  (let ((content-type (sb-fastcgi:fcgx-getparam *req* "CONTENT_TYPE")))
    (cond ((string-equal "application/x-www-form-urlencoded" content-type)
	   (parse-www-form-url-encoded (read-request-data))))))

(defun form-data ()
  (or *form-data* (setf *form-data* (read-form-data))))

(defmacro with-form-data (vars &body body)
  (let ((form-data (gensym "FORM-DATA-")))
    `(let ((,form-data (form-data)))
       (let ,(mapcar (lambda (var)
		       `(,var (cdr (assoc ,(symbol-name var) ,form-data
					  :test #'string-equal))))
		     vars)
	 ,@body))))
