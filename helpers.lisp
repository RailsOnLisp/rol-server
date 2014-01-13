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

(defun unaccent (c)
  (or (cl-ppcre:register-groups-bind (name)
	  ("(.*) WITH .*" (cl-unicode:unicode-name c))
	(cl-unicode:character-named name))
      c))

(defun to-url (str)
  (string-trim
   "-"
   (with-output-to-string (out)
     (let ((len (length str)))
       (labels ((out (c)
		  (write-char (unaccent (char-downcase c)) out))
		(nohyphen (i)
		  (when (< i len)
		    (let ((c (char str i)))
		      (if (alphanumericp c)
			  (progn (out c)
				 (hyphen (1+ i)))
			  (nohyphen (1+ i))))))
		(hyphen (i)
		  (when (< i len)
		    (let ((c (char str i)))
		      (if (alphanumericp c)
			  (progn (write-char (unaccent (char-downcase c)) out)
				 (hyphen (1+ i)))
			  (progn (write-char #\- out)
				 (nohyphen (1+ i))))))))
	 (nohyphen 0))))))

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
