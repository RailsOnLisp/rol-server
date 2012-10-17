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

(require :lowh-triangle-server)

(use-package :alexandria)
(use-package :lowh-triangle-server)

(load-secret)

(dolist (dir '("app/models/*.lisp"
	       "app/controllers/*.lisp"
	       "config/*.lisp"))
  (dolist (file (directory dir))
    (when (alphanumericp (char (pathname-name file) 0))
      (load (enough-namestring file)))))

(defun reload ()
  (load "lib/lowh-triangle-server/load/app"))

(defun build (core)
  (sb-ext:save-lisp-and-die core
			    :toplevel #'run
			    #+sb-core-compression :compression
			    #+sb-core-compression t))
