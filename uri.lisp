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

;;  Canonical URI

(defun canonical-document-uri (uri)
  (or (when (string= "/" uri)
	uri)
      (cl-ppcre:regex-replace
       "/$" (cl-ppcre:regex-replace "//" uri "/")
       "")))

;;  URI Templates

(defun parse-uri-template/token (token)
  (cond
    ((char= #\{ (char token 0))
     (intern (string-upcase (subseq token 1 (- (length token) 1)))))
    (t token)))

(defun uri-template (template)
  (let (tokens chars)
    (flet ((push-token ()
	     (when chars
	       (push (parse-uri-template/token (coerce (nreverse chars) 'string))
		     tokens)
	       (setf chars nil))))
      (dotimes (i (length template))
	(let ((c (char template i)))
	  (cond
	    ((char= #\{ c) (push-token) (push c chars))
	    ((char= #\} c) (push c chars) (push-token))
	    (t (push c chars)))))
      (push-token))
    (nreverse tokens)))

(defun uri-template-regex (template)
  (let ((template (etypecase template
		    (string (uri-template template))
		    (cons template)))
	(registers nil))
    (values
     `(:sequence
       ,@(mapcar (lambda (token)
		   (etypecase token
		     (string token)
		     (symbol (push token registers)
			     '(:register (:greedy-repetition
					  1 nil (:inverted-char-class #\/))))))
		 template)
       (:greedy-repetition 0 1 "/")
       :end-anchor)
     (nreverse registers))))

(defmacro uri-template-bind ((template target) &body body)
  (multiple-value-bind (regex vars) (uri-template-regex template)
    `(cl-ppcre:register-groups-bind
	 ,vars
	 (',regex ,target)
       ,@body)))

#+nil(
(assert (string= "42" (uri-template-bind ("/module/{id}/edit" "/module/42/edit")
			id)))

(assert (null (uri-template-bind ("/module/{id}/edit" "/module//edit")
		(declare (ignore id))
		42)))

(assert (null (uri-template-bind ("/module/{id}/edit" "/module/plop")
		(declare (ignore id))
		42)))
)
