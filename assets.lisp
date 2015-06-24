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

;;  Render assets on demand

(defun debug-asset-p (asset)
  (and (debug-p :assets)
       (typep asset 'preprocessed-asset)))

(defun asset-controller (name ext &optional
                                    (*assets-url-template* *assets-url-template*)
                                    (*assets-path-template* *assets-path-template*))
  (let* ((asset-spec (str name (when ext ".") ext))
	 (asset (find-asset asset-spec)))
    (unless asset
      (http-error "404 not found" "asset not found: ~S" asset-spec))
    (let ((write-date (asset-write-date asset))
	  (if-modified-since (request-header :if-modified-since)))
      (cond ((and if-modified-since
		  (= (parse-rfc1123-date-time if-modified-since)
		     write-date))
	     (status "304 not modified"))
	    (t (header :last-modified (rfc1123-date-time write-date))
	       (header :content-type (mime-type asset) "; charset=utf-8")
	       (if (debug-asset-p asset)
		   (process-asset asset *reply-stream*)
		   (compile-asset asset *reply-stream*)))))))

(defun define-assets-route (url-template path-template)
  (define-templated-route url-template
    ``(asset-controller ,,(uri-var 'name) ,,(uri-var 'ext)
                        ,,url-template ,,path-template)))

(defun print-asset-tag (spec &rest args)
  (let ((asset (find-asset spec)))
    (cond ((null asset)
           (http-error "404 Not found" "Asset not found"))
          ((debug-asset-p asset)
           (dolist (source (asset-sources asset))
             (apply #'asset-include *reply-stream* :html source args)))
          (t (apply #'asset-include *reply-stream* :html asset args)))))

(defun cache-assets ()
  (clear-asset-cache)
  (mapcar (lambda (asset)
            (let* ((id (str (asset-name asset) (asset-ext asset)))
                   (found (find-asset id)))
              (assert (equal (asset-path asset) (asset-path found)))
              id))
          (locate-precompiled-assets)))
