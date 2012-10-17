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

(in-package :cl-user)

(defpackage :triangle.files
  (:use :cl :alexandria)
  (:import-from :cl-ppcre #:nsubseq)
  (:export
   #:with-output-to-file/utf-8
   #:with-input-from-file/utf-8
   #:open-temporary-file
   #:with-temporary-file
   #:file-more-recent-p
   #:copy-files
   #:regex-stream-lines
   #:regex-lines))

(in-package :triangle.files)

;;  With files

(defmacro with-output-to-file/utf-8 ((stream filespec) &body body)
  `(with-open-file (,stream ,filespec
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type 'character
			    :external-format :utf-8)
     ,@body))

(defmacro with-input-from-file/utf-8 ((stream filespec) &body body)
  `(with-open-file (,stream ,filespec
			    :direction :input
			    :if-does-not-exist :error
			    :element-type 'character
			    :external-format :utf-8)
     ,@body))

;;  Temporary files

(defun open-temporary-file (&optional (prefix "/tmp/.tmp"))
  (let ((template (merge-pathnames
		   (pathname (format nil "~A.XXXXXXXXXXXX" prefix)))))
    (multiple-value-bind (fd path) (sb-posix:mkstemp template)
      (let* ((path (pathname path))
	     (file (truename path))
	     stream)
	(unwind-protect
	     (setf stream
		   (sb-sys:make-fd-stream fd
					  :auto-close t
					  :output t
					  :name (format nil "temporary ~D ~A"
							fd file)
					  :pathname (pathname path)
					  :file file
					  :element-type 'character 
					  :external-format :utf-8))
	  (unless stream
	    (sb-posix:close fd)))
	stream))))

(defmacro with-temporary-file ((stream-var &optional (prefix "/tmp/.tmp"))
			       &body body)
  (let ((g!stream (gensym "STREAM-")))
    `(let ((,g!stream (open-temporary-file ,prefix)))
       (unwind-protect
	    (let ((,stream-var ,g!stream))
	      ,@body)
	 (close ,g!stream)
	 (sb-posix:unlink (pathname ,g!stream))))))

;;  Stat

(defun file-more-recent-p (a b)
  "Return T if A is more recent than B according to FILE-WRITE-DATE.
Return NIL otherwise."
  (when (cl-fad:file-exists-p a)
    (> (file-write-date a) (file-write-date b))))

;;  Copying

(defun copy-files (from to &key replace update exclude rename)
  (ensure-directories-exist to)
  (mapcan
   (lambda (src)
     (let* ((src (enough-namestring src))
	    (name (make-pathname :name (pathname-name src)
				 :type (pathname-type src))))
       (unless (etypecase exclude
		 (sequence (member name exclude :test #'pathname-match-p))
		 (function (funcall exclude name))
		 (null nil))
	 (let* ((renamed (etypecase rename
			   (list (cdr (assoc name rename
					     :test #'pathname-match-p)))
			   (function (funcall rename (format nil "~A" name)))
			   (null nil)))
		(dest (merge-pathnames (or renamed name) to)))
	   (unless (and update (file-more-recent-p dest src))
	     (cl-fad:copy-file src dest :overwrite replace))
	   `(,dest)))))
   (directory from)))

;;  Regex

(defun regex-do-matches (regex string fn)
  (cl-ppcre:do-scans (match-start match-end reg-starts reg-ends regex string)
    (let ((match (subseq string match-start match-end))
	  (regs (loop for i below (length reg-starts)
		   for start = (aref reg-starts i)
		   for end = (aref reg-ends i)
		   collect (subseq string start end))))
      (apply fn match regs))))

(defun regex% (regex string replace match-fun output)
  (cond
    (replace (multiple-value-bind (replaced matched)
		 (cl-ppcre:regex-replace regex string replace)
	       (when (and match-fun matched)
		 (funcall match-fun replaced))
	       (when output
		 (write-string replaced output)
		 (terpri output))
	       replaced))
    (match-fun (regex-do-matches regex string match-fun))
    (t (warn "Neither replace or match were given to regex."))))

(defun regex-stream-lines (regex input replace match output)
  (do ((line #1=(read-line input nil) #1#))
      ((null line))
    (regex% regex line replace match output)))

(defun regex-lines (regex input &key replace match output)
  (labels ((resolve (input match output)
	     (cond ((typep input 'pathname)
		    (with-input-from-file/utf-8 (stream input)
		      (resolve stream match output)))
		   ((typep output 'pathname)
		    (with-output-to-file/utf-8 (stream output)
		      (resolve input match stream)))
		   (t
		    (regex-stream-lines regex input replace match output)))))
    (resolve input match output)))

;;  JSON

(defun pathname-string (pathname)
  (format nil "~A" (pathname pathname)))

(defmethod json:encode-json ((object pathname) &optional stream)
  (json:encode-json (pathname-string object) stream))
