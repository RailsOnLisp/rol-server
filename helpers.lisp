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

(in-package :RoL-server)

(defun odd/even (list)    ;; Rails cycle
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

(defmethod h ((thing number))
  (h (atom-str thing)))

(defmethod h ((object json:fluid-object))
  (h (j object)))

;;  Markdown

(defgeneric markdown (destination input))

(defmethod markdown ((destination t) (input null))
  nil)

(defmethod markdown ((destination stream) (input stream))
  (exec-js:from-file #P"lib/triangle/server/markdown.js"
		     :safely nil :in input :out destination))

#+nil(defmethod markdown ((destination stream) (input stream))
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

;;  Convert URLs to links

(defun urls-to-links (text)
  (cl-ppcre:regex-replace-all
   "(^|\\s)(https?://\\S+)"
   text
   (lambda (s s1 s2 m1 m2 r1 r2)
     (declare (ignore s1 s2 m1 m2))
     (let ((space (subseq s (svref r1 0) (svref r2 0)))
	   (url   (subseq s (svref r1 1) (svref r2 1))))
       (str space "<a href=\"" (h url) "\">" (h url) "</a>")))))

;;  Alert boxes

(define-template-var alerts)

(defmacro alert (level &rest message-parts)
  `(push (list ,level (str ,@message-parts))
	 (template-var alerts)))
