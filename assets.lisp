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

;;  Render assets on demand

(defun asset-route (url)
  (unless (string= url *asset-url-prefix*
		   :end1 (length *asset-url-prefix*))
    (http-error "404 not found" "asset prefix not found"))
  (let* ((spec (subseq url (length *asset-url-prefix*)))
	 (asset (first (find-assets-from-spec spec))))
    (unless asset
      (http-error "404 not found" "asset not found: ~S" spec))
    (let ((path (pathname (asset-path asset))))
      (compile-asset asset path)
      (with-open-file (in path :element-type '(unsigned-byte 8))
	(copy-stream in *standard-output*)))))

(defun route-precompiled-assets (&optional (enable t))
  (when enable
    (msg "Precompile asset routes")
    (with-msg-indent (1)
      (dolist (asset (locate-precompiled-assets))
	(let ((url (asset-url asset)))
	  (msg "~A" url)
	  (define-static-route url `(asset-route ,url)))))))
