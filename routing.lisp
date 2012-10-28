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

(in-package :lowh-triangle-server)

(defun cgi-env (name)
  (sb-fastcgi:fcgx-getparam *req* (string name)))

(defun render (route)
  (let ((controller (first route)))
    (if (fboundp controller)
	(apply controller (rest route))
	(render-error "404 Not found" "controller function not found"))))

(defun routed-by (uri)
  (or (facts:first-bound ((uri :routed-by ?)))
      '(render-error "404 Not found" "no route")))

(define-constant +http-verbs+ #(:GET :POST :PUT :DELETE)
  :test 'equalp)

(defun http-verb (object)
  (find (string-upcase object)
	+http-verbs+
	:key #'symbol-name
	:test #'string=))

(defun request-method ()
  (let ((method (http-verb (cgi-env :request_method))))
    (or (and (eq :POST method)
	     (http-verb (with-form-data (_method) _method)))
	method)))

(defun route (req)
  (time
   (let* ((*req* req)
	  (*form-data* nil)
	  (*method* (request-method))
	  (*host* (cgi-env :host))
	  (*uri* (cgi-env :document_uri))
	  (*headers-output* (make-string-output-stream
			     :element-type 'base-char))
	  (*standard-output* (make-string-output-stream
			     :element-type 'character))
	  (route (routed-by *uri*)))
     (log-msg :info "~A ~S -> ~S" (cgi-env :request_method) *uri* route)
     (render route)
     (let ((content (get-output-stream-string *standard-output*)))
       (content-length (trivial-utf-8:utf-8-byte-length content))
       (crlf *headers-output*)
       (let ((headers (get-output-stream-string *headers-output*)))
	 (when *debug*
	   (log-msg :debug "FULL REPLY: ~S~%~S" headers content))
	 (sb-fastcgi:fcgx-puts req headers)
	 (sb-fastcgi:fcgx-puts-utf-8 req content))))))
