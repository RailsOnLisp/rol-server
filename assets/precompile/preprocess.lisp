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

;;  Preprocess : asset -> list of assets to compile

(defun preprocess/require (asset specs assets)
  (reduce (lambda (assets asset)
	    (preprocess/asset asset assets))
	  (find-assets-from-specs specs (class-of asset))
	  :initial-value assets))

(defun preprocess/comment (asset comment assets)
  (debug-msg "Comment ~S" comment)
  (or (cl-ppcre:register-groups-bind (command arguments)
	  ("^\\W*=\\s*(\\w+)(\\s+\\S+)*\\s*$" comment)
	(let ((arg-list (rest (cl-ppcre:split "\\s+" arguments))))
	  (cond ((string= "require" command)
		 (preprocess/require asset arg-list assets))
		(t
		 (warn "Unknown preprocessor command : ~A ~S"
		       command arg-list)
		 assets))))
      assets))

(defun match-comment-start-and-end (start end)
  (when (find-if (lambda (match)
		   (and (string= start (car match))
			(string= end (cdr match))))
		 '(("/*" . "*/")))
    t))

(defun preprocess/stream (asset stream assets &optional stack)
  (let ((assets assets)
	(line (read-line stream nil))
	start comment end)
    (or (when line
	  (cl-ppcre:register-groups-bind (s c e)
	      ("^\\s*(/\\*|//)?(.*?)(?:(\\*/).*)?$" line)
	    (setf start s comment c end e))
	  (unless (empty-p start)
	    (push start stack))
	  (unless (empty-p comment)
	    (if stack
		(setf assets (preprocess/comment asset comment assets))
		(setf line nil)))
	  (when (string= "//" (first stack))
	    (pop stack))
	  (when (and stack end (match-comment-start-and-end
				(first stack) end))
	    (pop stack)
	    (unless stack
	      (setf line nil)))
	  (when line
	    (preprocess/stream asset stream assets stack)))
	(cons asset assets))))

(defun preprocess/asset (asset assets)
  (let ((path (asset-source-path asset)))
    (msg "~A" path)
    (with-msg-indent (1)
      (with-input-from-file/utf-8 (input path)
	(preprocess/stream asset input assets)))))

(defun preprocess-asset (asset &optional assets)
  (preprocess/asset asset assets))
