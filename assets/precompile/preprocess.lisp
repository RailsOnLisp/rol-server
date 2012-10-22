;;
;;  Assets  -  Asset pipeline
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

(in-package :assets)

;;  Preprocess

(defun preprocess/comment (type comment)
  (debug-msg "Comment ~S" comment)
  (with-msg-indent ((if *debug* 3 0))
    (let (command
	  args)
      (cl-ppcre:do-register-groups (?command ?args)
	  ("^\\W*=\\s*(\\w+)(?:\\s+(\\S+))*\\s*$" comment)
	(setf command ?command
	      args ?args)
	(return))
      (when command
	(cond
	  ((string= "require" command)
	   (preprocess/name type args)))))))

(defun match-comment-start-and-end (start end)
  (when (find-if (lambda (match)
		   (and (string= start (car match))
			(string= end (cdr match))))
		 '(("/*" . "*/")))
    t))

(defun preprocess/stream (type stream &optional stack)
  (let ((line (read-line stream nil))
	start
	comment
	end)
    (when line
      (cl-ppcre:do-register-groups (?start ?comment ?end)
	  ("^\\s*(/\\*|//)?(.*?)(?:(\\*/).*)?$" line)
	(setf start ?start
	      comment ?comment
	      end ?end)
	(return))
      (unless (empty-p start)
	(debug-msg "Start ~S" start)
	(incf *msg-indent* 3)
	(push start stack))
      (unless (empty-p comment)
	(if stack
	    (preprocess/comment type comment)
	    (setf line nil)))
      (when (string= "//" (first stack))
	(pop stack))
      (when (and stack end (match-comment-start-and-end (first stack) end))
	(decf *msg-indent* 3)
	(debug-msg "End ~S" end)
	(pop stack)
	(unless stack
	  (setf line nil)))
      (when line
	(preprocess/stream type stream stack)))))

(defun preprocess/path (type path)
  (msg "Preprocessing ~A ~A" type path)
  (with-msg-indent (3)
    (with-input-from-file/utf-8 (input path)
      (preprocess/stream type input))
    (msg "Processing ~A ~A" type path)
    (with-msg-indent (3)
      (with-input-from-file/utf-8 (input path)
	(process type input)))))

(defun preprocess/name (type name)
  (preprocess/path type (asset-path name type)))
