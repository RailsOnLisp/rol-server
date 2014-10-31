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

(defun render-text (text)
  (content-type :text/plain)
  (write-string text *reply-stream*))

(defun render-error.txt (status-string &optional (msg "") condition backtrace)
  (flexi-streams:get-output-stream-sequence *reply-stream*)
  (content-type :text/plain)
  (write-string status-string *reply-stream*)
  (when (debug-p :app)
    (fresh-line *reply-stream*)
    (write-string msg *reply-stream*)
    (fresh-line *reply-stream*)
    (terpri *reply-stream*)
    (prin1 (backend-request-env) *reply-stream*)
    (fresh-line *reply-stream*)
    (mapc #'print backtrace)))

(defun find-error-template (status)
  (flet ((type-match (type)
	   (flet ((try-name (name)
		    (let ((template (find-template type name "_errors")))
		      (when (probe-file template)
			(list template type)))))
	     (or (try-name (subseq status 0 3))
		 (try-name (str (char status 0) (char status 1) "x"))
		 (try-name (str (char status 0) "xx"))
		 (when (debug-p :app)
		   (try-name "debug"))
		 (try-name "500")))))
    (or (type-match '.html)
	(type-match '.txt)
	(type-match '.json))))

(defun render-error (status &optional (message "") condition backtrace)
  (status status)
  (handler-case
      (destructuring-bind (&optional template type) (find-error-template status)
	(cond (template (when type
			  (content-type (mime-type type) "; charset=utf-8"))
			(template-let (status message condition backtrace)
			  (print-template template)))
	      (:otherwise (render-error.txt status message condition backtrace))))
    (error ()
      (render-error.txt status message condition backtrace))))

(defun render-json (thing)
  (content-type :application/json)
  (json:encode-json thing *reply-stream*))
