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

;;  Log messages

(defvar *msg-indent* 0)

(defmacro with-msg-indent ((level) &body body)
  `(let ((*msg-indent* (+ ,level *msg-indent*)))
     ,@body))

(defun msg (fmt &rest args)
  (let ((*standard-output* *error-output*))
    (fresh-line)
    (dotimes (i *msg-indent*)
      (write-char #\Space))
    (apply #'format t fmt args)
    (fresh-line)
    (force-output)))

(defun debug-msg (fmt &rest args)
  (when *debug*
    (apply #'msg fmt args)))

;;  Misc

(defun empty-p (string)
  (or (null string)
      (not (cl-ppcre:scan "\\S" string))))


;;  Process assets

(defun process-js (input)
  (write-string
   (cl-uglify-js:ast-gen-code (cl-uglify-js:ast-mangle
			       (cl-uglify-js:ast-squeeze
				(parse-js:parse-js input)))
			      :beautify nil))
  (values))

#+nil(defun recess (path* &rest options)
  (let* ((path* (mapcar (lambda (p) (format nil "~A" p))
		       (if (listp path*) path* `(,path*))))
	 (fmt "~
var recess = require('recess')
  , src = ~A
  , opt = ~A
recess(src, opt, function (err, obj) {
  if (err) throw err
  process.stdout.write(JSON.stringify(obj))
})
")
	 (js (format nil fmt
		     (json:encode-json-to-string path*)
		     (json:encode-json-plist-to-string options))))
    (exec-js:from-string js :safely nil)))

(defun less (src parser-options css-options)
  (let* ((fmt "~
var path = require('path'),
    fs = require('fs'),
    sys = require('util'),
    os = require('os');
var less = require('less');
var src = path.resolve(process.cwd(), ~A);
var parser_opts = ~A;
var css_opts = ~A;

var print_error = function (e) {
  less.writeError(e);
  process.exit(2);
}
var print_tree = function (e, tree) {
  if (e)
    print_error(e);
  try {
    var css = tree.toCSS(css_opts);
    process.stdout.write(css);
  } catch (e) {
    print_error(e);
  }
}
var parse_data = function (e, data) {
  if (e)
    print_error(e);
  try {
    new(less.Parser)(parser_opts).parse(data, print_tree)
  } catch (e) {
    print_error(e);
  }
}
try {
  fs.readFile(src, 'utf8', parse_data);
} catch (e) {
  print_error(e);
}
")
	 (js (format nil fmt
		     (json:encode-json-to-string src)
		     (json:encode-json-plist-to-string parser-options)
		     (json:encode-json-plist-to-string css-options))))
    #+nil(format *error-output* "~%~A~%" js)
    (exec-js:from-string js :safely nil)))

(defun process-css (input)
  (with-temporary-file (tmp (format nil "tmp/~A.~A"
				    (pathname-name input)
				    (pathname-type input)))
    (cl-fad:copy-stream input tmp)
    (force-output tmp)
    (close tmp)
    (write-string (less (pathname tmp)
			(list :paths (mapcar #'truename (assets-dirs))
			      :filename (enough-namestring (pathname input)))
			(list :yuicompress t))))
  (values))

(defun process (type input)
  (ecase type
    (:js  (process-js  input))
    (:css (process-css input))))

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

;;  Pipelines

(defun copy (output-path asset-path type)
  (declare (ignore type))
  (msg "Copying from ~A" asset-path)
  (cl-fad:copy-file asset-path output-path :overwrite t))

(defun preprocess (output-path asset-path type)
  (with-output-to-file/utf-8 (*standard-output* output-path)
    (preprocess/path type asset-path)))

;;  Precompile

(defun precompile-asset (output-path asset-path)
  (ensure-directories-exist output-path)
  (let ((type (asset-type asset-path)))
    (dolist (pipeline-op (asset-type-pipeline type))
      (funcall pipeline-op output-path asset-path type))))

(defun locate-precompiled-assets ()
  (let (assets)
    (dolist (asset-spec (reverse *precompiled-assets*))
      (dolist (assets-dir (assets-dirs))
	(let ((wild (merge-pathnames asset-spec assets-dir)))
	  (msg "Search ~A" wild)
	  (dolist (path (directory wild :resolve-symlinks nil))
	    (msg "Found ~A" (enough-namestring path))
	    (pushnew (enough-namestring path (truename assets-dir))
		     assets
		     :test #'equal)))))
    assets))

(defun precompile ()
  (dolist (asset (locate-precompiled-assets))
    (let ((output-path (merge-pathnames asset #P"public/assets/")))
      (msg "Precompiling ~A" asset)
      (with-msg-indent (3)
	(precompile-asset output-path (asset-path asset)))
      (msg "Done precompiling ~A" asset))))

;;  Asset generators

(defvar *generator* nil)

(defun generator (name)
  (declare (type symbol name))
  (setf *generator* name))

(defun generate/file (path)
  (let (*generator*
	(dir (enough-namestring
	      (truename
	       (merge-pathnames "../" (make-pathname :name nil :type nil
						     :defaults path))))))
    (debug-msg "Loading ~A" (enough-namestring path))
    (load path)
    (funcall *generator* dir)))

(defun generate ()
  (msg "Generating assets...")
  (mapc #'generate/file (directory "lib/*/triangle/assets.lisp")))
