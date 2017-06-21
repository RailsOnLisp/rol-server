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
    (babel:octets-to-string (subseq out 0 pos))))

(defun parse-www-form-url-encoded (string)
  (let (form-data)
    (cl-ppcre:do-register-groups (var value)
        ("([^&=]+)(?:=([^&=]*))?" string nil :sharedp t)
      (push (cons (url-decode var) (url-decode (or value ""))) form-data))
    (nreverse form-data)))

(defun parse-www-form-json-encoded (string)
  (json:decode-json-from-string string))

(defun form-data ()
  (or *form-data*
      (setf *form-data* (backend-read-form-data))))

(defgeneric form-data-get (form-data key))

(defmethod form-data-get ((form-data null) key)
  nil)

(defmethod form-data-get ((form-data cons) key)
  (cdr (assoc key form-data :test #'string-equal)))

(defmethod form-data-get ((form-data json:fluid-object) key)
  (json-slot form-data key))

(defmacro with-form-data (vars &body body)
  (let ((form-data (gensym "FORM-DATA-")))
    `(let ((,form-data (form-data)))
       (when (debug-p :request)
         (log-msg :debug "FORM ~S" ,form-data))
       (let ,(mapcar (lambda (var)
                       `(,var (form-data-get
                               ,form-data
                               ',(intern (symbol-name var) :keyword))))
                     vars)
         ,@body))))
