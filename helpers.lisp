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

(defun odd/even (list)
  (let (odd)
    (mapcar (lambda (x)
	      (setf odd (not odd))
	      (list* :odd/even (if odd "odd" "even") x))
	    list)))

;;  To URL

(defgeneric uri-for (thing)
  (:documentation "Returns a URI used to access THING."))

(defmethod uri-for ((thing cons))
  (route-reverse thing))

;; To HTML

(defgeneric h (thing)
  (:documentation "Returns the HTML code for THING."))

(defmethod h ((thing null))
  "")

(defmethod h ((thing string))
  (quote-html thing))

(defmethod h ((thing symbol))
  (h (string-downcase thing)))

;;  Markdown

(defgeneric markdown (destination input))

(defmethod markdown ((destination stream) (input stream))
  (sb-ext:run-program "markdown" '("-xcodehilite(force_linenos=True)" "/dev/stdin")
		      :search t
		      :input input
		      :output destination))

(defmethod markdown ((destination null) (input t))
  (with-output-to-string (s)
    (markdown s input)))

(defmethod markdown ((destination t) (input string))
  (with-input-from-string (s input)
    (markdown destination s)))

(defun print-markdown (input)
  (markdown *reply-stream* input))

;;  Alert boxes

(defvar L>template.vars::alerts)

(defmacro alert (level &rest message-parts)
  `(push (list ,level (str ,@message-parts))
	 L>template.vars::alerts))
