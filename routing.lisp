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

;;  Static routes

(defsetf static-route-controller (uri) (value)
  `(setf (gethash ,uri *static-routes*) ,value))

(defun static-route-controller (uri)
  (gethash uri *static-routes*))

(defmacro static-route-reverse (controller)
  `(gethash ,controller *static-routes/reverse*))

(defun define-static-route (uri controller-form)
  (setf (static-route-controller uri) controller-form
	(static-route-reverse controller-form) uri))

(defun list-static-routes ()
  (let (routes)
    (maphash (lambda (uri form)
	       (push (list uri form) routes))
	     *static-routes*)
    (sort routes #'string< :key #'car)))

;;  Template routes

(defstruct templated-route
  (uri-template ())
  (controller-form () :type cons)
  (function () :type function))

(defun find-templated-route (uri-template)
  (find uri-template *templated-routes*
	:key #'templated-route-uri-template
	:test #'string=))

(defmacro define-templated-route (uri-template controller-form)
  (let ((g!uri (gensym "URI-")))
    `(let ((old (find-templated-route ,uri-template)))
       (when old
	 (remove old *templated-routes*))
       (push (make-templated-route :uri-template ,uri-template
				   :controller-form ',controller-form
				   :function
				   (lambda (,g!uri)
				     (uri-template-bind (,uri-template ,g!uri)
				       (list ,@controller-form))))
	     *templated-routes*))))

(defun templated-route-controller (uri)
  (when *templated-routes*
    (loop
       for route in *templated-routes*
       for fun = (funcall (templated-route-function route) uri)
       until fun
       return fun)))

(defun templated-route-reverse (controller)
  (let ((route (find controller *templated-routes*
		     :key #'templated-route-controller
		     :test (lambda (c template)
			     (declare (ignorable c template))
			     (error "FIXME: reverse templated routes")))))
    (when route
      (templated-route-uri-template route))))

(defun list-templated-routes ()
  (mapcar (lambda (route)
	    (list (templated-route-uri-template route)
		  (templated-route-controller-form route)))
	  *templated-routes*))

;;  Abstract routes functions

(defun uri-template-is-static-p (uri-template)
  (not (find #\{ uri-template)))

(defmacro define-route (uri-template controller-form)
  (if (uri-template-is-static-p uri-template)
      `(define-static-route ,uri-template ',controller-form)
      `(define-templated-route ,uri-template ,controller-form)))

(defun find-route (uri)
  (or (static-route-controller uri)
      (templated-route-controller uri)
      '(render-error "404 Not found" "no route")))

(defun route-reverse (controller)
  (or (static-route-reverse controller)
      (templated-route-reverse controller)
      '(render-error "500 Route not found" "no route")))

(defun list-routes ()
  (append (list-static-routes)
	  (list-templated-routes)))

;;  Rendering

(defun render-route (route)
  (apply (the symbol (car route))
	 (the list (cdr route))))

(defun route-request ()
  (with-request
    (let ((route (the cons (find-route *uri*))))
      (log-msg :info "~A ~S -> ~S" *method* *uri* route)
      (with-reply
	(time (render-route route))
	(force-output *trace-output*)))))
