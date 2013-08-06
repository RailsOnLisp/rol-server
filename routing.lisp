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

(defun define-static-route (uri controller-form)
  (setf (gethash uri *static-routes*) (lambda ()
					(apply (first controller-form)
					       (rest controller-form)))
	(gethash controller-form *static-routes/reverse*) uri))

(defun static-route-controller (uri)
  (gethash uri *static-routes*))

(defun static-route-reverse (controller)
  (gethash controller *static-routes/reverse*))

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
				       (lambda () ,controller-form))))
	     *templated-routes*))))

(defun templated-route-controller (uri)
  (when *templated-routes*
    (do ((routes *templated-routes* (rest routes))
	 (fun (funcall (templated-route-function (car *templated-routes*)) uri)
	      (when routes
		(funcall (templated-route-function (car routes)) uri))))
	(fun fun))))

(defun templated-route-reverse (controller)
  (let ((route (find controller *templated-routes*
		     :key #'templated-route-controller
		     :test (lambda (c template)
			     (declare (ignorable c template))
			     (error "FIXME: reverse templated routes")))))
    (when route
      (templated-route-uri-template route))))

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
      (lambda () (render-error "404 Not found" "no route"))))

(defun route-reverse (controller)
  (or (static-route-reverse controller)
      (templated-route-reverse controller)
      (lambda () (render-error "500 Route not found" "no route"))))

;;  Rendering

(defun route-request (req)
  (time
   (with-request req
     (let ((route (find-route *uri*)))
       (with-reply (reply)
	 (log-msg :info "~A ~S -> ~S" (cgi-env :request_method) *uri* route)
	 (funcall route)
	 (let ((content (reply-get-output reply)))
	   (content-length (length content))
	   (crlf *headers-output*)
	   (let ((headers (reply-get-headers reply)))
	     (when (find :reply *debug*)
	       (log-msg :debug "REPLY: ~S~%~S" headers
			(trivial-utf-8:utf-8-bytes-to-string content)))
	     (sb-fastcgi:fcgx-puts req headers)
	     (sb-fastcgi::fcgx-putchars req content))))))))
