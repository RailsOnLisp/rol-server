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

;;  Pipelines

(defgeneric compile-asset (asset output))

(defmethod compile-asset ((asset asset) (output pathname))
  (ensure-directories-exist output)
  (let ((path (asset-source-path asset)))
    (msg "~A" path)
    (copy-files path output :replace t :update t)
    nil))

(defmethod compile-asset ((asset preprocessed-asset) (output pathname))
  (ensure-directories-exist output)
  (let ((assets (preprocess-asset asset)))
    (when (or (not (file-exists-p output))
	      (some (lambda (asset)
		      (file-more-recent-p (asset-source-path asset)
					  output))
		    assets))
      (with-output-to-file/utf-8 (out output)
	(dolist (a assets)
	  (process-asset a out (not (eq asset a))))))))

;;  Precompile

(defun locate-precompiled-assets ()
  (find-assets-from-specs *precompiled-assets*))

(defun precompile ()
  (msg "Precompile")
  (with-msg-indent (1)
    (dolist (asset (locate-precompiled-assets))
      (let ((output-path (asset-path asset)))
	(msg "~A" output-path)
	(with-msg-indent (1)
	  (compile-asset asset (pathname output-path)))))))
