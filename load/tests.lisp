;;
;;  LowH Triangle Server
;;
;;  Copyright 2014 Thomas de Grivel <billitch@gmail.com>
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

(require :fiveam)
(import '(5am:test
	  5am:is))

(require :priest)
(import '(priest:*content-type*
	  priest:*doc*
	  priest:request))
(defvar *base-url-for-tests* "http://localhost/")

(defun run-tests (&key (suite :RoL-tests))
  (5am:def-suite :RoL-tests :description "The main RoL test suite.")
  (5am:in-suite :RoL-tests)
  (dolist (path (directory "tests/**.lisp"))
    (unless (char= #\. (char (pathname-name path) 0))
      (load path)))
  (priest:with-session
    (setq priest:*base-uri* *base-url-for-tests*)
    (5am:run! suite))
  (terpri))

(defun an-instance-of (type value)
  (typep value type))

(defmacro is-a (type expr)
  `(if (typep ,expr ',type)
       (5am::add-result '5am::test-passed
			:test-expr '(is-a ,type ,expr))
       (5am::process-failure :reason (format nil "~S did not return a ~S"
					     ',expr ',type)
			     :test-expr '(is-a ,type ,expr))))

(defmacro test-request ((method url &rest args) &body body)
  (let ((request `(priest:request ,method ,url . ,args)))
    `(let ((log (with-output-to-string (priest:*log-output*)
		  ,request)))
       (cond ((null priest:*doc*)
	      (5am::process-failure :reason log
				    :test-expr ',request))
	     (t
	      (5am::add-result '5am::test-passed :test-expr ',request)
	      ,@body)))))

(defmacro has-dom-element (selector)
  `(if (priest:select ,selector)
       (5am::add-result '5am::test-passed
			:test-expr '(priest:select ,selector))
       (5am::process-failure :reason (format nil "~S~%has no dom element matching~%~S"
					     priest:*uri* ,selector)
			     :test-expr '(priest:select ,selector))))
