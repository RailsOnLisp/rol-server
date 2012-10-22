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

;;  Config

(defvar *debug* nil)

(defvar *assets-dirs*
  '("lib/*/triangle/assets/*/"
    "app/assets/*/"))

(defvar *precompiled-assets*
  '("all.css"
    "all.js"
    "**/*.jpeg"
    "**/*.jpg"
    "**/*.png"
    "**/*.svg"
    "**/*.eot"
    "**/*.ttf"
    "**/*.woff"))

(defun assets-dir (pathspec)
  (pushnew (pathname pathspec) *assets-dirs* :test #'equal))

(defun precompiled-asset (asset-name)
  (pushnew asset-name *precompiled-assets* :test #'string=))

;;  Asset types

(defvar *asset-types*
  '((:css   (preprocess) (:css :less))
    (:js    (preprocess) (:js))
    (:image (copy) (:gif :ico :jpeg :jpg :png :svg :svgz))
    (:font  (copy) (:eot :ttf :woff))
    (:other (copy) nil)))

(defun asset-type-name (asset-type)
  (first asset-type))

(defun asset-type/name (name)
  (find name *asset-types* :key #'asset-type-name))

(defun asset-type-pipeline (asset-type)
  (second (if (symbolp asset-type)
	      (asset-type/name asset-type)
	      asset-type)))

(defun asset-type-extensions (asset-type)
  (third (if (symbolp asset-type)
	     (asset-type/name asset-type)
	     asset-type)))

(defun asset-type (asset)
  (let ((path-type (string-upcase (pathname-type (pathname asset)))))
    (asset-type-name (find-if (lambda (types)
				(or (null types)
				    (find path-type types :test #'string=)))
			      *asset-types*
			      :key #'asset-type-extensions))))

;;  Misc

(defun empty-p (string)
  (or (null string)
      (not (cl-ppcre:scan "\\S" string))))

(defun cache-fn (fn args &key clear)
  (let ((cache (load-time-value (make-hash-table :test 'equal))))
    (or (gethash args cache)
	(when clear
	  (clrhash cache)
	  (format t "clear ~S~%" args)
	  nil)
	(setf (gethash args cache) (apply fn args)))))

(defun directories (list)
  (declare (type list list))
  (mapcar #'enough-namestring (mapcan #'directory list)))

(defun file-exists-p (path)
  (when (cl-fad:file-exists-p path)
    (enough-namestring path)))

;;  Finding assets

(defun assets-dirs ()
  (cache-fn (compose #'directories #'reverse)
	    `(,*assets-dirs*) :clear t))

(defun asset-path (name &optional type (error-p t))
  (labels ((search-types (dir types)
	     (unless (endp types)
	       (or (file-exists-p (merge-pathnames
				   (make-pathname :type (string-downcase
							 (first types)))
				   (merge-pathnames name dir)))
		   (search-types dir (rest types)))))
	   (search-dirs (dirs)
	     (unless (endp dirs)
	       (or (if type
		       (search-types (first dirs)
				     (asset-type-extensions type))
		       (file-exists-p (merge-pathnames name (first dirs))))
		   (search-dirs (rest dirs))))))
    (or (search-dirs (assets-dirs))
	(when error-p
	  (error "Asset not found ~S ~A" type name)))))
