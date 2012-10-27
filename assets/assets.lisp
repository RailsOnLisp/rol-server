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

;;  Misc

(defun empty-p (string)
  (or (null string)
      (not (cl-ppcre:scan "\\S" string))))

(eval-when (:compile-toplevel :load-toplevel)
  (let ((cache-nil (gensym "CACHE-NIL-")))

    (defmacro cache-1 ((test key) &body body)
      "Cache one value of BODY. TEST identifies KEY is cached."
      (let ((cache (gensym "CACHE-")))
	`(let ((,cache (load-time-value (cons ',cache-nil nil))))
	   (if (,test (car ,cache) ,key)
	       (cdr ,cache)
	       (setf (car ,cache) ,key
		     (cdr ,cache) (progn ,@body))))))))

(defun str (&rest objects)
  (cond ((endp objects)
	 (str ""))
	((= 1 (length objects))
	 (let ((obj (first objects)))
	   (typecase obj
	     (null "")
	     (symbol (str (string-downcase (symbol-name obj))))
	     (string obj)
	     (t (string obj)))))
	(t
	 (apply #'concatenate 'string
		(mapcar 'str objects)))))

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
  (let* ((namestring (enough-namestring pathspec))
	 (path (if (char= #\/ (last-elt namestring))
		   namestring
		   (str namestring "/"))))
    (pushnew path *assets-dirs* :test #'string=)))

(defun precompiled-asset (asset-name)
  (pushnew asset-name *precompiled-assets* :test #'string=))

(defun assets-dirs ()
  (cache-1 (eq *assets-dirs*)
    (directories (reverse *assets-dirs*))))

;;  Asset class

(defclass asset ()
  ((name :initarg :name
	 :reader asset-name
	 :type string)
   (source-dir :initarg :source-dir
	       :accessor asset-source-dir
	       :type string)
   (source-ext :initarg :source-ext
	       :reader asset-source-ext
	       :type keyword)))

(defgeneric asset-ext (asset))

(defmethod asset-ext ((asset asset))
  (asset-source-ext asset))

(defun asset-url (asset)
  (declare (type asset asset))
  (let ((name (asset-name asset))
	(ext (asset-ext asset)))
    (str "/assets/" name (when ext ".") ext)))

(defun asset-path (asset)
  (declare (type asset asset))
  (let ((name (asset-name asset))
	(ext (asset-ext asset)))
    (str "public/assets/" name (when ext ".") ext)))

(defun asset-source-path (asset)
  (declare (type asset asset))
  (with-slots (name source-dir source-ext) asset
    (str source-dir name (when source-ext ".") source-ext)))

(defmethod print-object ((asset asset) stream)
  (print-unreadable-object (asset stream :type t)
    (ignore-errors (format stream "~S" (asset-path asset)))
    (ignore-errors (format stream " ~S" (asset-source-path asset)))))

;;  Asset class -> extensions

#+nil
(fmakunbound 'asset-class-extensions)

(defgeneric asset-class-extensions (asset-class))

(defmethod asset-class-extensions ((any symbol))
  nil)

(defmethod asset-class-extensions ((class class))
  (asset-class-extensions (class-name class)))

(defmethod asset-class-extensions ((asset asset))
  (asset-class-extensions (class-of asset)))

;;  Asset classes

;;    Image

(defclass image-asset (asset) ())

(defmethod asset-class-extensions ((class (eql 'image-asset)))
  (extensions #:gif #:ico #:jpeg #:jpg #:png #:svg #:svgz))

;;    Font

(defclass font-asset (asset) ())

(defmethod asset-class-extensions ((class (eql 'font-asset)))
  (extensions #:eot #:ttf #:woff))

;;    Preprocessed assets

(defclass preprocessed-asset (asset) ())

;;    CSS

(defclass css-asset (preprocessed-asset) ())

(defmethod asset-ext ((asset css-asset))
  (extension #:css))

(defmethod asset-class-extensions ((class (eql 'css-asset)))
  (extensions #:css #:less))

;;    JS

(defclass js-asset (preprocessed-asset) ())

(defmethod asset-ext ((asset js-asset))
  (extension #:js))

(defmethod asset-class-extensions ((class (eql 'js-asset)))
  (extensions #:js))
