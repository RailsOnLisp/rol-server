;;
;;  RoL-server  -  Rails on Lisp application server
;;
;;  Copyright 2012-2015 Thomas de Grivel <thomas@lowh.net>
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

;;  Static routes

(defvar *static-routes*)
(defvar *static-routes/reverse*)

(defun clear-static-routes ()
  (setq *static-routes*         (make-hash-table :test 'equal)
	*static-routes/reverse* (make-hash-table :test 'equal)))

(unless (and (boundp '*static-routes*)
             (boundp '*static-routes/reverse*))
  (clear-static-routes))

(defsetf static-route-controller (uri) (value)
  `(setf (gethash ,uri *static-routes*) ,value))

(defun static-route-controller (uri)
  (gethash uri *static-routes*))

(defmacro static-route-reverse (controller)
  `(gethash ,controller *static-routes/reverse*))

(defun define-static-route (uri controller)
  (setf (static-route-controller uri) controller
        (static-route-reverse controller) uri))

(defun sstring< (a b)
  (declare (type simple-string a b))
  (string< a b))

(defun list-static-routes ()
  (let (routes)
    (maphash (lambda (uri form)
	       (push (list uri form) routes))
	     *static-routes*)
    (sort routes #'sstring< :key #'car)))

;;  Template routes

(defstruct templated-route
  (uri-template ())
  (controller-form () :type cons)
  (function () :type function))

(defvar *templated-routes* nil)

(defun clear-templated-routes ()
  (setq *templated-routes* nil))

(defun find-templated-route (uri-template)
  (find uri-template *templated-routes*
	:key #'templated-route-uri-template
	:test #'string=))

(defun remove-templated-route (uri-template)
  (setf *templated-routes*
	(remove uri-template *templated-routes*
		:key #'templated-route-uri-template
		:test #'string=)))

(defun update-templated-route (templated-route)
  (if (null *templated-routes*)
      (push templated-route *templated-routes*)
      (let ((uri-template (templated-route-uri-template templated-route)))
	(labels ((iter (cell)
		   (cond ((string= uri-template
				   (templated-route-uri-template (car cell)))
			  (setf (car cell) templated-route))
			 ((endp (cdr cell))
			  (setf (cdr cell) (cons templated-route nil)))
			 (t (iter (cdr cell))))))
	  (iter *templated-routes*))))
  templated-route)

(defun list-unquote-if (test list)
  (labels ((walk (x)
	     (if (funcall test x)
		 x
		 (cond ((consp x) (loop for v = x then (cdr v)
				     while (consp v)
				     collect (walk (car v)) into cars
				     finally (return
					       (if (null v)
						   `(list ,@cars)
						   `(list* ,@cars
							   ,(walk v))))))
		       ((or (null x)
			    (eq t x)
			    (keywordp x)) x)
		       ((symbolp x) `(quote ,x))
		       (t x)))))
    (walk list)))

(defun define-templated-route (uri controller)
  (update-templated-route
   (make-templated-route :uri-template (uri-template-string uri)
                         :controller-form controller
                         :function (compile-uri-template-matcher
                                    uri (list controller)))))

(defun templated-route-controller (uri)
  (some (lambda (route)
	  (funcall (templated-route-function route) uri))
	*templated-routes*))

(defun templated-route-reverse (controller-form)
  (labels ((unify (r v acc &optional (tail acc))
	     (cond ((and (endp r) (endp v)) acc)
		   ((or (endp r) (endp v)) nil)
		   ((uri-var-p (car r))
		    (unify (cdr r) (cdr v) acc
			   (cdr (setf (cdr tail)
				      (list (intern (symbol-name (car r))
						    :keyword)
					    (car v))))))
		   ((equal (car r) (car v))
		    (unify (cdr r) (cdr v) acc tail))
		   (t nil)))
	   (match-routes (routes)
	     (unless (endp routes)
	       (or (unify (templated-route-controller-form (car routes))
			  controller-form
			  (cons (car routes) nil))
		   (match-routes (cdr routes))))))
    (when-let ((match (match-routes *templated-routes*)))
      (apply #'expand-uri nil (templated-route-uri-template (car match))
	     (cdr match)))))

(defun list-templated-routes ()
  (mapcar (lambda (route)
	    (list (templated-route-uri-template route)
		  (templated-route-controller-form route)))
	  *templated-routes*))

;;  Abstract routes functions

(defmacro define-route (uri controller)
  (if (uri-template-p uri)
      `(define-templated-route ,uri ',controller)
      `(define-static-route ,uri ,controller)))

(defun find-route (uri)
  (or (static-route-controller uri)
      (templated-route-controller uri)
      `(render-error "404 Not found"
		     ,(format nil "No route configured for ~S." uri))))

(defun route-reverse (controller)
  (or (static-route-reverse controller)
      (templated-route-reverse controller)
      (http-error "500 Route not found"
		  "No route matches ~S." controller)))

(defun list-routes ()
  (append (list-static-routes)
	  (list-templated-routes)))

(defun clear-routes ()
  (clear-static-routes)
  (clear-templated-routes))

;;  Rendering

(defun render-route (route)
  (apply (the symbol (car route))
	 (the list (cdr route))))

(defun route-request ()
  (time
   (with-request
     (with-reply
       (when (debug-p :app)
	 (load-app))
       (let ((route (the cons (find-route *uri*))))
	 (log-msg :info "~A ~S -> ~S" *method* *uri* route)
	 (when (debug-p :request)
	   (log-msg :debug "ENV ~S" (backend-request-env)))
	 (render-route route)))
     (force-output *trace-output*))))
