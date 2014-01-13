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

(defun find-template (type name &rest directories)
  (make-pathname :directory (list* :relative "app" "views"
				   (mapcar #'string-downcase
					   directories))
		 :name (string-downcase name)
		 :type (subseq (string-downcase type) 1)))

(defvar *render-view-nested* nil)

(defun render-view (controller action type)
  (declare (type extension type))
  (let ((template (find-template type action controller)))
    (if *render-view-nested*
	(print-template template)
	(let ((layout (find-template type *layout* "_layouts"))
	      (*render-view-nested* t))
	  (content-type (mime-type type))
	  (template-let (template controller action)
	    (print-template layout))))))

(setq *template-output* (make-synonym-stream '*reply-stream*))
