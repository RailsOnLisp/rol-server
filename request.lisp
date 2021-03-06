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

(defun cookie-value (name &optional (cookies (request-header :cookie)))
  (when cookies
    (re-bind (str "(?:^|; )\\s*" (re-quote (str name)) "=([^;]+)")
        (value) cookies
      value)))

(defmacro with-request (&body body)
  `(let* ((*session*)
          (*form-data* nil)
          (*method* (request-method))
          (*host* (request-header :host))
          (*uri* (canonical-document-uri (backend-request-uri))))
     ,@body))
