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

(defun copy (output-path asset-path type)
  (declare (ignore type))
  (msg "Copying from ~A" asset-path)
  (cl-fad:copy-file asset-path output-path :overwrite t))

(defun preprocess (output-path asset-path type)
  (with-output-to-file/utf-8 (*standard-output* output-path)
    (preprocess/path type asset-path)))

;;  Precompile

(defun precompile-asset (output-path asset-path)
  (ensure-directories-exist output-path)
  (let ((type (asset-type asset-path)))
    (dolist (pipeline-op (asset-type-pipeline type))
      (funcall pipeline-op output-path asset-path type))))

(defun locate-precompiled-assets ()
  (let (assets)
    (dolist (asset-spec (reverse *precompiled-assets*))
      (dolist (assets-dir (assets-dirs))
	(let ((wild (merge-pathnames asset-spec assets-dir)))
	  (msg "Search ~A" wild)
	  (dolist (path (directory wild :resolve-symlinks nil))
	    (msg "Found ~A" (enough-namestring path))
	    (pushnew (enough-namestring path (truename assets-dir))
		     assets
		     :test #'equal)))))
    assets))

(defun precompile ()
  (dolist (asset (locate-precompiled-assets))
    (let ((output-path (merge-pathnames asset #P"public/assets/")))
      (msg "Precompiling ~A" asset)
      (with-msg-indent (3)
	(precompile-asset output-path (asset-path asset)))
      (msg "Done precompiling ~A" asset))))
