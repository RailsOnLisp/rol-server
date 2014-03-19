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

(defun resource-action-method (action)
  (case action
    ((index get)
     :get)
    ((create post)
     :post)
    ((update put)
     :put)
    ((delete)
     :delete)))

;;  Resource definition macros

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *resource-macros*
    nil
    "Alist of macros available during DEFINE-RESOURCE."))

(defun resource-macro (name)
  (cdr (assoc name *resource-macros*)))

(defsetf resource-macro (name) (value)
  `(let ((cell (assoc ,name *resource-macros*)))
     (if cell
	 (setf (cdr cell) ,value)
	 (progn (push (cons ,name ,value) *resource-macros*)
		,value))))

(defmacro define-resource-macro (name args &body body)
  (let ((macro-name (sym 'define-resource/ name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defmacro ,macro-name ,args
	 ,@body)
       (setf (resource-macro ',name) ',macro-name))))

(defmacro with-resource-macros (resource-name &body body)
  (labels ((walk (x)
	     (if (consp x)
		 (let ((macro-name (resource-macro (car x)))
		       (rest (mapcar #'walk (rest x))))
		   (if macro-name
		       (list* macro-name resource-name rest)
		       (cons (car x) rest)))
		 x)))
    `(progn ,@(mapcar #'walk body))))

;;  Relations

(defmacro resource-relation (slot-name)
  `(sym resource-name "." ,slot-name))

;;  Relation with multiple objects

(define-resource-macro has-many (resource-name collection-name)
  (let ((accessor (resource-relation collection-name)))
    `(progn (defun ,accessor (,resource-name)
	      (facts:collect ((,resource-name ',accessor ?x))
		?x))
	    (defmacro ,(sym 'do- accessor) ((var ,resource-name) &body body)
	      `(facts:with ((,,resource-name ',',accessor ?x))
		 (let ((,var ?x))
		   ,@body))))))

;;  Relation to one object

(define-resource-macro has-one (resource-name slot-name &key read-only having many)
  (let ((accessor (resource-relation slot-name)))
    `(progn (defun ,accessor (,resource-name)
	      (facts:first-bound ((,resource-name ',accessor ?))))
	    ,@(unless read-only
		`((defsetf ,accessor (,resource-name) (,slot-name)
		    `(facts:with-transaction
		       (facts:rm ((,,resource-name ',',accessor ?)))
		       (facts:add (,,resource-name ',',accessor ,,slot-name))
		       ,,slot-name))))
	    ,@(when having
		`((defun ,having (,slot-name)
		    (facts:collect ((?x ',accessor ,slot-name)) ?x))
		  (defmacro ,(sym 'do- having)
		      ((var ,slot-name) &body body)
		    `(facts:with ((?x ',',accessor ,,slot-name))
		       (let ((,var ?x))
			 ,@body)))))
	    ,@(when many
		(warn "(HAS-ONE .. :MANY ..) is deprecated. Please use :HAVING instead.")
		(let ((many-accessor (resource-relation many)))
		  `((defun ,many-accessor (,resource-name)
		      (facts:collect ((?x ',accessor ,resource-name))
			?x))
		    (defmacro ,(sym 'do- many-accessor)
			((var ,resource-name) &body body)
		      `(facts:with ((?x ',',accessor ,,resource-name))
			 (let ((,var ?x))
			   ,@body)))))))))

;;  ID

(define-resource-macro random-id (resource-name &key (length 6))
  (let ((find-resource (sym 'find- resource-name))
	(resource-id (sym resource-name '.id)))
    `(progn
       (define-resource/has-one ,resource-name id :read-only t)
       (defun ,find-resource (id)
	 (facts:first-bound ((?c :is-a ',resource-name)
			     (?c ',resource-id id))))
       (defmacro ,(sym 'add- resource-name) (&body properties)
	 `(facts:with-transaction
	    (let ((id (loop for i = (make-resource-id ,,length)
			 while (,',find-resource i)
			 finally (return i))))
	      (facts:with-anon (,',resource-name)
		(facts:add (,',resource-name :is-a ',',resource-name
					     ',',resource-id id
					     ,@properties))
		,',resource-name)))))))

;;  The actual macro

;;  Some use cases for MAKE-RESOURCE-ID require strong random for security purposes.

(defun make-resource-id (length)
  (make-random-string length))

(defmacro define-resource (resource-name &body body)
  `(with-resource-macros ,resource-name
     ,@(unless (find 'random-id body :key #'car)
	 '((random-id)))
     ,@body))
