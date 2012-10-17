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

(defun find-template (type name &rest directories)
  (let ((path (make-pathname :directory (cons :relative
					      (mapcar #'string-downcase
						      directories))
			     :name (string-downcase name)
			     :type (string-downcase type))))
    (html-template:create-template-printer path)))

(defun render-template (template params)
  (html-template:fill-and-print-template template
					 params
					 :stream *standard-output*))

(defun type-mime (type)
  (case type
    ((:txt :text) "text/plain")
    ((:html) "text/html")
    (:otherwise (string-downcase type))))

(defun render-view (controller action type params)
  (let* ((template (find-template type action controller))
	 (layout (find-template type *layout* "_layouts")))
    (content-type (type-mime type))
    (render-template layout
		     `(:controller ,(string-downcase controller)
				   :action ,(string-downcase action)
				   :parts ((,template ,@params))
				   ,@params))))
