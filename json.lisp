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

(cl-json:set-decoder-simple-clos-semantics)

(defgeneric j (thing)
  (:documentation "Returns the JSON code for THING."))

(defmethod j (thing)
  (json:encode-json-to-string thing))

(defmethod print-object ((object json:fluid-object) stream)
  (write-string (j object) stream))

(defun read-json-object (stream char)
  (with-input-from-string (char-stream (make-string 1 :initial-element char))
    (json:decode-json (make-concatenated-stream char-stream stream))))

(defun enable-json-object-syntax ()
  (set-macro-character #\{ #'read-json-object t))

(enable-json-object-syntax)

(defmethod lessp:lessp ((a json:fluid-object) (b json:fluid-object))
  (lessp:lessp (j a) (j b)))

(defmethod make-load-form ((obj json:fluid-object) &optional env)
  (declare (ignore env))
  `(json:decode-json ,(j obj)))

(defgeneric bound-slots (object))

(defmethod bound-slots (object)
  (loop :for def :in (closer-mop:class-slots (class-of object))
     :for name = (closer-mop:slot-definition-name def)
     :when (slot-boundp object name)
     :collect name))

(defun json-slot (object key)
  (let ((slot (json:json-intern (string-upcase key))))
    (when (slot-boundp object slot)
      (slot-value object slot))))

(defsetf json-slot (object key) (value)
  `(setf (slot-value ,object
		     (json:json-intern (string-upcase ,key)))
	 ,value))

(defun json-dot (object &rest keys)
  (reduce #'json-slot keys :initial-value object))

(defmacro with-json-accessors (accessors object &body body)
  (let ((g!object (gensym "OBJECT-")))
  `(let ((,g!object ,object))
     (symbol-macrolet ,(mapcar (lambda (a)
				 `(,a (json-slot ,g!object ',a)))
			       accessors)
       ,@body))))

(defmacro define-json-accessors (class &body accessors)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(mapcar (lambda (slot)
		 (let ((name (sym class #\. slot)))
		   `(defmacro ,name (,class)
		      `(json-slot ,,class ',',slot))))
	       accessors)))

(defun set-json-attributes (obj &rest attributes)
  (if attributes
      (destructuring-bind (key value &rest rest) attributes
	(setf (json-slot obj key) value)
	(apply #'set-json-attributes obj rest))
      obj))

(defgeneric to-json (x))

(defmethod to-json ((x t))
  x)

(defmethod to-json ((x cons))
  (if (every (lambda (i)
	       (and (consp i)
		    (or (symbolp (car i))
			(stringp (car i)))))
	     x)
      (json:make-object (mapcar (lambda (i)
				  (cons (car i) (to-json (cdr i))))
				x)
			nil)
      x))
