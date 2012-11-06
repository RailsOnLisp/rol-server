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

;;  Finding assets

;;  FIXME: This could be improved by listing all files in assets
;;  FIXME: directories to work the paths in memory.

(defun find-in-assets (type dir name ext assets)
  (find-if (lambda (asset)
	     (declare (type asset asset))
	     (and (typep asset type)
		  (string= name (asset-name asset))
		  (or (not (string= dir (asset-source-dir asset)))
		      (string= ext (asset-source-ext asset)))))
	   assets))

#+nil
(fmakunbound 'find-assets)

(defgeneric find-assets (type dir name ext assets))

;;  Resolve name, possibly wild

(defmethod find-assets ((type class)
			(dir string)
			(name string)
			(ext symbol)
			assets)
  (let ((absolute-dir (merge-pathnames dir))
	(assets assets))
    (dolist (path (directory (str dir name (when ext ".") ext)
			     :resolve-symlinks nil))
      (let ((name (enough-namestring (make-pathname :type nil :defaults path)
				     absolute-dir)))
	(unless (find-in-assets type dir name ext assets)
	  (push (make-instance type
			       :name name
			       :source-dir dir
			       :source-ext ext)
		assets))))
    assets))

;;    Loop through extensions

(defmethod find-assets ((type class)
			(dir string)
			(name string)
			(extensions cons)
			assets)
  (reduce (lambda (assets ext)
	    (declare (type symbol ext))
	    (find-assets type dir name ext assets))
	  extensions
	  :initial-value assets))

(defmethod find-assets ((type class)
			(dir string)
			(name string)
			(ext null)
			assets)
  (find-assets type dir name (asset-class-extensions type) assets))

;;    Loop through dirs

(defmethod find-assets ((type class)
			(directories cons)
			(name string)
			ext
			assets)
  (reduce (lambda (assets dir)
	    (declare (type string dir))
	    (find-assets type dir name ext assets))
	  directories
	  :initial-value assets))

(defmethod find-assets (type (dir null) name ext assets)
  (find-assets type (assets-dirs) name ext assets))

;;    Resolve class

(defmethod find-assets ((type symbol) dir name ext assets)
  (let ((class (if (keywordp type)
		   (find-class (find-symbol (str type "-ASSET")))
		   (find-class type))))
    (unless class
      (error "Unknown asset type : ~S" type))
    (find-assets (the class class) dir name ext assets)))

;;  Asset spec

(defmacro with-asset-spec (spec (name ext) &body body)
  `(let (,name ,ext)
     (cl-ppcre:register-groups-bind (n e)
	 ("^\\s*(.*?)(?:\\.([^./]+))?\\s*$" ,spec)
       (setf ,name n ,ext (intern-extension e)))
     (let ((,name ,name) (,ext ,ext))
       ,@body)))

(defun find-assets-from-spec (spec &optional class assets)
  (with-asset-spec spec (name ext)
    (find-assets (or class (extension-asset-class ext))
		 nil name nil assets)))

(defun find-assets-from-specs (specs &optional class assets)
  (reduce (lambda (assets spec)
	    (find-assets-from-spec spec class assets))
	  specs
	  :initial-value assets))